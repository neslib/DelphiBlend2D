unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ListBox,
  FMX.Layouts,
  {$IFDEF USE_SKIA}
  System.Skia,
  {$ENDIF}
  Blend2D,
  FBase;

type
  TBubbleF = record
  public
    P: TPointF;
    R: Single;
    X: Single;
    A: Single;
    B: Single;
    C: Single;
    Color: TAlphaColor;
  end;
  PBubbleF = ^TBubbleF;

type
  TBubble = record
  public
    P: TBLPoint;
    R: Double;
    X: Double;
    A: Double;
    B: Double;
    C: Double;
    Color: TAlphaColor;
  public
    function ToBubbleF: TBubbleF;
  end;
  PBubble = ^TBubble;

type
  TFormMain = class(TFormBase)
    ToolBar1: TToolBar;
    TrackBarCount: TTrackBar;
    LabelCountHeader: TLabel;
    ToolBar2: TToolBar;
    TrackBarParam: TTrackBar;
    LabelParamHeader: TLabel;
    LabelCount: TLabel;
    LabelParam: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarCountChange(Sender: TObject);
    procedure TrackBarParamChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRandom: TBLRandom;
    FBubbles: TList<TBubble>;
    FBubblesF: TList<TBubbleF>;
  private
    function RandomRgba32: TAlphaColor; inline;
  protected
    procedure BeforeRender; override;
    procedure RenderFireMonkey(const ACanvas: TCanvas); override;
    procedure RenderBlend2D(const AContext: TBLContext); override;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISkCanvas); override;
    {$ENDIF}
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math;

{ TBubble }

function TBubble.ToBubbleF: TBubbleF;
begin
  Result.P := PointF(P.X, P.Y);
  Result.R := R;
  Result.X := X;
  Result.A := A;
  Result.B := B;
  Result.C := C;
  Result.Color := Color;
end;

{ TFormMain }

procedure TFormMain.BeforeRender;
const
  ADD_LIMIT = 10;

  function NewBubble(const AW, AH, AParam: Double): TBubble;
  begin
    var R := (FRandom.NextDouble * 20) + 5;
    Result.P.Reset(FRandom.NextDouble * AW, AH + R);
    Result.R := R;
    Result.X := Result.P.X;
    Result.A := (FRandom.NextDouble * 3) + 1;
    Result.B := 0;
    Result.C := (FRandom.NextDouble * (AParam * 0.4 + 0.001)) + (AParam * 0.02);
    Result.Color := RandomRgba32;
  end;

begin
  inherited;
  var Count := Trunc(TrackBarCount.Value);
  var W: Double := PaintBox.Width;
  var H: Double := PaintBox.Height;

  if (FBubbles.Count > Count) then
  begin
    FBubbles.Count := Count;
    FBubblesF.Count := Count;
  end;

  var Param: Double := TrackBarParam.Value * 0.0001;
  var Added := 0;
  while (FBubbles.Count < Count) and (Added < ADD_LIMIT) do
  begin
    var B := NewBubble(W, H, Param);
    FBubbles.Add(B);
    FBubblesF.Add(B.ToBubbleF);
    Inc(Added);
  end;

  var Bubble := PBubble(FBubbles.List);
  var BubbleF := PBubbleF(FBubblesF.List);
  for var I := 0 to FBubbles.Count - 1 do
  begin
    Bubble.B := Bubble.B + Bubble.C;

    if (Bubble.B > 2) then
      Bubble.B := Bubble.B - 2;

    Bubble.P.X := Bubble.X + (Sin(Bubble.B * Pi) * 20);
    Bubble.P.Y := Bubble.P.Y - Bubble.A;

    if (Bubble.P.Y < -Bubble.R) then
    begin
      Bubble^ := NewBubble(W, H, Param);
      BubbleF^ := Bubble.ToBubbleF;
    end
    else
    begin
      BubbleF.P.X := Bubble.P.X;
      BubbleF.P.Y := Bubble.P.Y;
      BubbleF.B := Bubble.B;
    end;

    Inc(Bubble);
    Inc(BubbleF);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FBubbles := TList<TBubble>.Create;
  FBubblesF := TList<TBubbleF>.Create;
  FRandom.Reset($123456789ABCDEF);
  {$IFDEF USE_SKIA}
  FSkiaFill.Blender := TSkBlender.MakeMode(TSkBlendMode.Plus);
  {$ENDIF}
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FBubblesF.Free;
  FBubbles.Free;
end;

function TFormMain.RandomRgba32: TAlphaColor;
begin
  Result := FRandom.NextUInt32 or $55000000;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);
  AContext.CompOp := TBLCompOp.Plus;

  var G: TBLGradient;
  G.Kind := TBLGradientKind.Radial;

  var InvH: Double := 1 / PaintBox.Height;

  var Stops: array [0..1] of TBLGradientStop;
  Stops[0].Offset := 0;
  Stops[1].Offset := 1;
  Stops[1].Rgba := 0;

  var Bubble := PBubble(FBubbles.List);
  for var I := 0 to FBubbles.Count - 1 do
  begin
    var R: Double := Bubble.R;
    var F: Double := (-R * 0.5) + (R * (Bubble.P.Y * InvH));

    Stops[0].Rgba.Reset(Bubble.Color);

    G.SetValues(BLRadialGradientValues(Bubble.P.X, Bubble.P.Y, Bubble.P.X,
      Bubble.P.Y + F, R));
    G.AssignStops(@Stops, 2);

    AContext.FillRect(BLRectI(
      Trunc(Bubble.P.X - R - 1), Trunc(Bubble.P.Y - R - 1),
      Trunc(R * 2) + 2, Trunc(R * 2) + 2), G);

    Inc(Bubble);
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  RenderNotSupported;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var InvH: Single := 1 / PaintBox.Height;

  var Bubble := PBubbleF(FBubblesF.List);
  for var I := 0 to FBubblesF.Count - 1 do
  begin
    var R: Single := Bubble.R;
    var F: Single := (-R * 0.5) + (R * (Bubble.P.Y * InvH));

    var Shader: ISkShader := TSkShader.MakeGradientRadial(
      PointF(Bubble.P.X, Bubble.P.Y + F), R,
      Bubble.Color, 0);
    FSkiaFill.Shader := Shader;

    var Rect: TRectF;
    Rect.Left := Trunc(Bubble.P.X - R - 1);
    Rect.Top := Trunc(Bubble.P.Y - R - 1);
    Rect.Width := Trunc(R * 2) + 2;
    Rect.Height := Trunc(R * 2) + 2;
    ACanvas.DrawRect(Rect, FSkiaFill);

    Inc(Bubble);
  end;
end;
{$ENDIF}

procedure TFormMain.TrackBarCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarCount.Value);
  LabelCount.Text := Count.ToString;
end;

procedure TFormMain.TrackBarParamChange(Sender: TObject);
begin
  inherited;
  var Param := Trunc(TrackBarParam.Value);
  LabelParam.Text := Param.ToString;
end;

end.
