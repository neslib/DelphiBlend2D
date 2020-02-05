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
    procedure FormCreate(Sender: TObject);
  private const
    MARGIN_SIZE = 7;
    SQUARE_SIZE = 45;
    FULL_SIZE   = SQUARE_SIZE + (MARGIN_SIZE * 2);
    HALF_SIZE   = 0.5 * FULL_SIZE;
  private
    FTime: Double;
    FBlend2DGradient: IBLGradient;
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
  System.Math,
  System.Math.Vectors;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  FTime := FTime + 2;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FBlend2DGradient := TBLGradient.Create;
  FBlend2DGradient.GradientType := TBLGradientType.Linear;
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  I, W, H, Count: Integer;
  IX, IY, Start, Now, X, Y, Dur, Pos, BouncePos, R, B, Rotation, Radius: Double;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  Count := W * H;

  IX := 0;
  IY := 0;
  Start := 0;
  Now := FTime;

  for I := 0 to Count - 1 do
  begin
    X := IX * FULL_SIZE;
    Y := IY * FULL_SIZE;

    Dur := (Now - Start) + (I * 50);
    Pos := FMod(Dur, 3000) / 3000;
    BouncePos := Abs((Pos * 2) - 1);
    R := ((BouncePos * 50) + 50) / 100;
    B := ((1 - BouncePos) * 50) / 100;

    Rotation := Pos * (Pi * 2);
    Radius := BouncePos * 25;

    AContext.Save;
    AContext.Rotate(Rotation, X + HALF_SIZE, Y + HALF_SIZE);
    AContext.Translate(X, Y);

    FBlend2DGradient.ResetStops;
    FBlend2DGradient.AddStop(0.0, $FFFF7F00);
    FBlend2DGradient.AddStop(1.0, BLRgba32(Trunc(R * 255), 0, Trunc(B * 255)));
    FBlend2DGradient.SetValues(BLLinearGradientValues(0, MARGIN_SIZE, 0, MARGIN_SIZE + SQUARE_SIZE));

    AContext.SetFillStyle(FBlend2DGradient);
    AContext.FillRoundRect(MARGIN_SIZE, MARGIN_SIZE, SQUARE_SIZE, SQUARE_SIZE, Radius, Radius);
    AContext.Restore;

    IX := IX + 1;
    if (IX >= W) then
    begin
      IX := 0;
      IY := IY + 1;
    end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
var
  I, W, H, Count: Integer;
  IX, IY, Start, Now, X, Y, Dur, Pos, BouncePos, R, B, Rotation, Radius: Double;
  OrigMatrix, Matrix: TMatrix;
  C: TAlphaColorRec;
begin
  ACanvas.Clear(TAlphaColors.Black);

  W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  Count := W * H;

  IX := 0;
  IY := 0;
  Start := 0;
  Now := FTime;
  C.G := 0;
  C.A := 255;

  for I := 0 to Count - 1 do
  begin
    X := IX * FULL_SIZE;
    Y := IY * FULL_SIZE;

    Dur := (Now - Start) + (I * 50);
    Pos := FMod(Dur, 3000) / 3000;
    BouncePos := Abs((Pos * 2) - 1);
    R := ((BouncePos * 50) + 50) / 100;
    B := ((1 - BouncePos) * 50) / 100;

    Rotation := Pos * (Pi * 2);
    Radius := BouncePos * 25;

    OrigMatrix := ACanvas.Matrix;
    Matrix := TMatrix.CreateTranslation(X + HALF_SIZE, Y + HALF_SIZE) * OrigMatrix;
    Matrix := TMatrix.CreateRotation(Rotation) * Matrix;
    Matrix := TMatrix.CreateTranslation(-HALF_SIZE, -HALF_SIZE) * Matrix;
    ACanvas.SetMatrix(Matrix);

    C.R := Trunc(R * 255);
    C.B := Trunc(B * 255);
    ACanvas.Fill.Kind := TBrushKind.Gradient;
    ACanvas.Fill.Gradient.Style := TGradientStyle.Linear;
    ACanvas.Fill.Gradient.Color := $FFFF7F00;
    ACanvas.Fill.Gradient.Color1 := C.Color;

    ACanvas.FillRect(RectF(MARGIN_SIZE, MARGIN_SIZE, MARGIN_SIZE + SQUARE_SIZE,
      MARGIN_SIZE + SQUARE_SIZE), Radius, Radius, AllCorners, 1);
    ACanvas.SetMatrix(OrigMatrix);

    IX := IX + 1;
    if (IX >= W) then
    begin
      IX := 0;
      IY := IY + 1;
    end;
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  I, W, H, Count: Integer;
  IX, IY, Start, Now, X, Y, Dur, Pos, BouncePos, R, B, Rotation, Radius: Double;
  Shader: ISKShader;
begin
  ACanvas.Clear(TAlphaColors.Black);

  W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  Count := W * H;

  IX := 0;
  IY := 0;
  Start := 0;
  Now := FTime;

  for I := 0 to Count - 1 do
  begin
    X := IX * FULL_SIZE;
    Y := IY * FULL_SIZE;

    Dur := (Now - Start) + (I * 50);
    Pos := FMod(Dur, 3000) / 3000;
    BouncePos := Abs((Pos * 2) - 1);
    R := ((BouncePos * 50) + 50) / 100;
    B := ((1 - BouncePos) * 50) / 100;

    Rotation := Pos * (Pi * 2);
    Radius := BouncePos * 25;

    ACanvas.Save;
    ACanvas.RotateRadians(Rotation, X + HALF_SIZE, Y + HALF_SIZE);
    ACanvas.Translate(X, Y);

    Shader := TSKShader.CreateLinearGradient(
      TSKPoint.Create(0, MARGIN_SIZE),
      TSKPoint.Create(0, MARGIN_SIZE + SQUARE_SIZE),
      [$FFFF7F00, TSKColor.Create(Trunc(R * 255), 0, Trunc(B * 255))],
      [0, 1], TSKShaderTileMode.Clamp);

    FSkiaFill.Shader := Shader;
    ACanvas.DrawRoundRect(MARGIN_SIZE, MARGIN_SIZE, SQUARE_SIZE, SQUARE_SIZE,
      Radius, Radius, FSkiaFill);
    ACanvas.Restore;

    IX := IX + 1;
    if (IX >= W) then
    begin
      IX := 0;
      IY := IY + 1;
    end;
  end;
end;
{$ENDIF}

end.
