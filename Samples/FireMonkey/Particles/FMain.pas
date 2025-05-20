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
  TParticle = record
  public
    P: TBLPoint;
    V: TBLPoint;
    Age: Integer;
    Category: Integer;
  end;
  PParticle = ^TParticle;

type
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    TrackBarCount: TTrackBar;
    LabelCountHeader: TLabel;
    ToolBar1: TToolBar;
    TrackBarRotation: TTrackBar;
    LabelRotationHeader: TLabel;
    CheckBoxColors: TCheckBox;
    LabelCount: TLabel;
    LabelRotation: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarCountChange(Sender: TObject);
    procedure TrackBarRotationChange(Sender: TObject);
  private const
    MAX_AGE        = 650;
    RADIUS_SCALE   = 6;
    CATEGORY_COUNT = 8;
    COLORS: array [0..CATEGORY_COUNT - 1] of TAlphaColor = (
      $FF4F00FF, $FFFF004F, $FFFF7F00, $FFFF3F9F,
      $FF7F4FFF, $FFFF9F3F, $FFFFFF00, $FFAF3F00);
  private
    FParticles: TList<TParticle>;
    FRandom: TBLRandom;
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

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  var Count := FParticles.Count;
  var Rot: Double := TrackBarRotation.Value * 0.02 / 1000;
  var M := TBLMatrix2D.MakeRotation(Rot);

  var P: TParticle;
  var J := 0;

  for var I := 0 to Count - 1 do
  begin
    P := FParticles[I];
    P.P := P.P + P.V;
    P.V := M.MapPoint(P.V);
    Inc(P.Age);
    if (P.Age < MAX_AGE) then
    begin
      FParticles[J] := P;
      Inc(J);
    end;
  end;
  FParticles.Count := J;

  var MaxParticles := Trunc(TrackBarCount.Value);
  var N := Trunc((FRandom.NextDouble * MaxParticles / 60) + 0.95);

  for var I := 0 to N - 1 do
  begin
    if (FParticles.Count >= MaxParticles) then
      Break;

    var Angle: Double := FRandom.NextDouble * Pi * 2;
    var Speed: Double := Max(FRandom.NextDouble * 2, 0.05);
    var ASin, ACos: Double;
    SinCos(Angle, ASin, ACos);

    P.P.Reset;
    P.V.Reset(ACos * Speed, ASin * Speed);
    P.Age := Trunc(Min(FRandom.NextDouble, 0.5) * MAX_AGE);
    P.Category := Trunc(FRandom.NextDouble * CATEGORY_COUNT);
    FParticles.Add(P);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FParticles := TList<TParticle>.Create;
  FRandom.Reset(1234);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FParticles.Free;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);

  var CX: Double := PaintBox.Width / 2;
  var CY: Double := PaintBox.Height / 2;
  var I: Integer;
  var P: TParticle;

  if (CheckBoxColors.IsChecked) then
  begin
    var Paths: array [0..CATEGORY_COUNT - 1] of TBLPath;
    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Paths[P.Category].AddCircle(BLCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE));
    end;

    AContext.CompOp := TBLCompOp.Plus;

    for I := 0 to CATEGORY_COUNT - 1 do
      AContext.FillPath(Paths[I], COLORS[I]);
  end
  else
  begin
    var Path: TBLPath;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Path.AddCircle(BLCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE));
    end;

    AContext.CompOp := TBLCompOp.SrcOver;
    AContext.FillPath(Path, TAlphaColors.White);
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
{ When using FireMonkey, it is (much) faster to draw ellipsis directly instead
  of using a path. }
begin
  ACanvas.Clear(TAlphaColors.Black);

  var CX: Single := PaintBox.Width / 2;
  var CY: Single := PaintBox.Height / 2;
  var X, Y, R: Single;
  var I: Integer;
  var P: TParticle;

  if (CheckBoxColors.IsChecked) then
  begin
    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      X := CX + P.P.X;
      Y := CY + P.P.Y;
      R := (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE;

      { Note: FireMonkey does not support additive blending }
      ACanvas.Fill.Color := COLORS[P.Category];
      ACanvas.FillEllipse(RectF(X - R, Y - R, X + R, Y + R), 1);
    end;
  end
  else
  begin
    ACanvas.Fill.Color := TAlphaColors.White;
    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      X := CX + P.P.X;
      Y := CY + P.P.Y;
      R := (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE;
      ACanvas.FillEllipse(RectF(X - R, Y - R, X + R, Y + R), 1);
    end;
  end;
end;

procedure TFormMain.TrackBarCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarCount.Value);
  LabelCount.Text := Count.ToString;
end;

procedure TFormMain.TrackBarRotationChange(Sender: TObject);
begin
  inherited;
  var Rotation := Trunc(TrackBarRotation.Value);
  LabelRotation.Text := Rotation.ToString;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var CX: Single := PaintBox.Width / 2;
  var CY: Single := PaintBox.Height / 2;
  var I: Integer;
  var P: TParticle;

  if (CheckBoxColors.IsChecked) then
  begin
    var PathBuilders: array [0..CATEGORY_COUNT - 1] of ISkPathBuilder;
    for I := 0 to CATEGORY_COUNT - 1 do
      PathBuilders[I] := TSkPathBuilder.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      PathBuilders[P.Category].AddCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE);
    end;

    FSkiaFill.Blender := TSkBlender.MakeMode(TSkBlendMode.Plus);

    for I := 0 to CATEGORY_COUNT - 1 do
    begin
      FSkiaFill.Color := COLORS[I];
      ACanvas.DrawPath(PathBuilders[I].Detach, FSkiaFill);
    end;
  end
  else
  begin
    var PathBuilder: ISkPathBuilder := TSkPathBuilder.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      PathBuilder.AddCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE);
    end;

    FSkiaFill.Blender := nil;
    FSkiaFill.Color := TAlphaColors.White;
    ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
  end;
end;
{$ENDIF}

end.
