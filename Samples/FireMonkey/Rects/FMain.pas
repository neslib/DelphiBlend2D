unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
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
  Neslib.Skia,
  {$ENDIF}
  Blend4D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    LabelCompOp: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxCompOp: TComboBox;
    TrackBarRectCount: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxCompOpChange(Sender: TObject);
    procedure TrackBarRectCountChange(Sender: TObject);
  private const
    RECT_SIZE = 64;
    HALF_SIZE = 0.5 * RECT_SIZE;
  private
    FCoords: TArray<TPointF>;
    FSteps: TArray<TPointF>;
    FColors: TArray<TAlphaColor>;
    FBlend2DCompOp: TBLCompOp;
    {$IFDEF USE_SKIA}
    FSkiaBlendMode: TSKBlendMode;
    {$ENDIF}
    procedure SetRectCount(const AValue: Integer);
  protected
    procedure BeforeRender; override;
    procedure RenderFireMonkey(const ACanvas: TCanvas); override;
    procedure RenderBlend2D(const AContext: IBLContext); override;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISKCanvas); override;
    {$ENDIF}
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math;

{ TFormMain }

procedure TFormMain.BeforeRender;
var
  W, H: Single;
  Vertex, Step: PPointF;
  I: Integer;
begin
  inherited;
  W := PaintBox.Width;
  H := PaintBox.Height;
  for I := 0 to Length(FCoords) - 1 do
  begin
    Vertex := @FCoords[I];
    Step := @FSteps[I];

    Vertex^ := Vertex^ + Step^;

    if (Vertex.X <= 0) or (Vertex.X >= W) then
    begin
      Step.X := -Step.X;
      Vertex.X := Min(Vertex.X + Step.X, W);
    end;

    if (Vertex.Y <= 0) or (Vertex.Y >= H) then
    begin
      Step.Y := -Step.Y;
      Vertex.Y := Min(Vertex.Y + Step.Y, H);
    end;
  end;
end;

procedure TFormMain.ComboBoxCompOpChange(Sender: TObject);
const
  BLEND2D_COMP_OPS: array [0..8] of TBLCompOp = (TBLCompOp.SrcOver, TBLCompOp.SrcCopy,
    TBLCompOp.DstAtop, TBLCompOp.ExclusiveOr, TBLCompOp.Plus, TBLCompOp.Screen,
    TBLCompOp.Lighten, TBLCompOp.HardLight, TBLCompOp.Difference);

  {$IFDEF USE_SKIA}
  SKIA_BLEND_MODES: array [0..8] of TSKBlendMode = (TSKBlendMode.SrcOver,
    TSKBlendMode.Src, TSKBlendMode.DstAtop, TSKBlendMode.ExclusiveOr,
    TSKBlendMode.Plus, TSKBlendMode.Screen, TSKBlendMode.Lighten,
    TSKBlendMode.HardLight, TSKBlendMode.Difference);
  {$ENDIF}
var
  I: Integer;
begin
  inherited;
  I := ComboBoxCompOp.ItemIndex;
  if (I >= 0) and (I < Length(BLEND2D_COMP_OPS)) then
  begin
    FBlend2DCompOp := BLEND2D_COMP_OPS[I];
    {$IFDEF USE_SKIA}
    FSkiaBlendMode := SKIA_BLEND_MODES[I];
    {$ENDIF}
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FBlend2DCompOp := TBLCompOp.SrcOver;
  {$IFDEF USE_SKIA}
  FSkiaBlendMode := TSKBlendMode.SrcOver;
  {$ENDIF}
  SetRectCount(Trunc(TrackBarRectCount.Value));
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  I: Integer;
  P: TPointF;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  AContext.CompOp := FBlend2DCompOp;

  for I := 0 to Length(FCoords) - 1 do
  begin
    P := FCoords[I];
    AContext.FillColor := FColors[I];
    AContext.FillBox(P.X - HALF_SIZE, P.Y - HALF_SIZE, P.X + HALF_SIZE, P.Y + HALF_SIZE);
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
var
  I: Integer;
  P: TPointF;
begin
  ACanvas.Clear(TAlphaColors.Black);

  if (FBlend2DCompOp <> TBLCompOp.SrcOver) then
  begin
    RenderNotSupported;
    Exit;
  end;

  for I := 0 to Length(FCoords) - 1 do
  begin
    P := FCoords[I];
    ACanvas.Fill.Color := FColors[I];
    ACanvas.FillRect(
      RectF(P.X - HALF_SIZE, P.Y - HALF_SIZE, P.X + HALF_SIZE, P.Y + HALF_SIZE),
      0, 0, [], 1);
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  I: Integer;
  P: TPointF;
begin
  ACanvas.Clear(TAlphaColors.Black);
  FSkiaFill.BlendMode := FSkiaBlendMode;
  for I := 0 to Length(FCoords) - 1 do
  begin
    P := FCoords[I];
    FSkiaFill.Color := FColors[I];
    ACanvas.DrawRect(P.X - HALF_SIZE, P.Y - HALF_SIZE, RECT_SIZE, RECT_SIZE, FSkiaFill);
  end;
end;
{$ENDIF}

procedure TFormMain.SetRectCount(const AValue: Integer);
var
  W, H: Single;
  I: Integer;
begin
  W := PaintBox.Width;
  H := PaintBox.Height;
  I := Length(FCoords);

  SetLength(FCoords, AValue);
  SetLength(FSteps, AValue);
  SetLength(FColors, AValue);

  while (I < AValue) do
  begin
    FCoords[I] := PointF(Random * W, Random * H);
    FSteps[I] := PointF(((Random * 0.5) + 0.05) * RandomSign,
                        ((Random * 0.5) + 0.05) * RandomSign);
    FColors[I] := RandomColor;
    Inc(I);
  end;
end;

procedure TFormMain.TrackBarRectCountChange(Sender: TObject);
begin
  inherited;
  SetRectCount(Trunc(TrackBarRectCount.Value));
end;

end.
