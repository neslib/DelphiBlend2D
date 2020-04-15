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
  Neslib.Skia,
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
    CheckBoxColors: TCheckBox;
    LabelCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private const
    MAX_AGE        = 650;
    RADIUS_SCALE   = 6;
    CATEGORY_COUNT = 3;
    COLORS: array [0..CATEGORY_COUNT - 1] of TAlphaColor = (
      $FFFF7F00, $FFFF3F9F, $FF7F4FFF);
  private
    FParticles: TList<TParticle>;
    FRandom: TBLRandom;
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
  I, J, N, Count, MaxParticles: Integer;
  Angle, Speed, ASin, ACos: Double;
  P: TParticle;
  M: TBLMatrix2D;
begin
  inherited;
  Count := FParticles.Count;
  M.ResetToRotation(0.01);

  J := 0;
  for I := 0 to Count - 1 do
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

  MaxParticles := Trunc(TrackBarCount.Value);
  N := Trunc((FRandom.NextDouble * MaxParticles / 60) + 0.95);

  for I := 0 to N - 1 do
  begin
    if (FParticles.Count >= MaxParticles) then
      Break;

    Angle := FRandom.NextDouble * Pi * 2;
    Speed := Max(FRandom.NextDouble, 0.1);
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

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  CX, CY: Double;
  Paths: array [0..CATEGORY_COUNT - 1] of IBLPath;
  Path: IBLPath;
  P: TParticle;
  I: Integer;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;

  if (CheckBoxColors.IsChecked) then
  begin
    for I := 0 to CATEGORY_COUNT - 1 do
      Paths[I] := TBLPath.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Paths[P.Category].AddCircle(BLCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE));
    end;

    AContext.CompOp := TBLCompOp.Plus;

    for I := 0 to CATEGORY_COUNT - 1 do
    begin
      AContext.FillColor := COLORS[I];
      AContext.FillPath(Paths[I]);
    end;
  end
  else
  begin
    Path := TBLPath.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Path.AddCircle(BLCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE));
    end;

    AContext.CompOp := TBLCompOp.SrcOver;
    AContext.FillColor := TAlphaColors.White;
    AContext.FillPath(Path);
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
{ When using FireMonkey, it is (much) faster to draw ellipsis directly instead
  of using a path. }
var
  CX, CY, X, Y, R: Single;
  P: TParticle;
  I: Integer;
begin
  ACanvas.Clear(TAlphaColors.Black);

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;

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

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  CX, CY: Single;
  Paths: array [0..CATEGORY_COUNT - 1] of ISKPath;
  Path: ISKPath;
  P: TParticle;
  I: Integer;
begin
  ACanvas.Clear(TAlphaColors.Black);

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;

  if (CheckBoxColors.IsChecked) then
  begin
    for I := 0 to CATEGORY_COUNT - 1 do
      Paths[I] := TSKPath.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Paths[P.Category].AddCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE);
    end;

    FSkiaFill.BlendMode := TSKBlendMode.Plus;

    for I := 0 to CATEGORY_COUNT - 1 do
    begin
      FSkiaFill.Color := COLORS[I];
      ACanvas.DrawPath(Paths[I], FSkiaFill);
    end;
  end
  else
  begin
    Path := TSKPath.Create;

    for I := 0 to FParticles.Count - 1 do
    begin
      P := FParticles[I];
      Path.AddCircle(CX + P.P.X, CY + P.P.Y,
        (MAX_AGE - P.Age) / MAX_AGE * RADIUS_SCALE);
    end;

    FSkiaFill.BlendMode := TSKBlendMode.SrcOver;
    FSkiaFill.Color := TAlphaColors.White;
    ACanvas.DrawPath(Path, FSkiaFill);
  end;
end;
{$ENDIF}

end.
