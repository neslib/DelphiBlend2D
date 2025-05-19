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
  System.Skia,
  {$ENDIF}
  Blend2D,
  FBase;

type
  TStyleId = (Solid, Linear, Radial, Conic);

type
  TFormMain = class(TFormBase)
    ToolBar1: TToolBar;
    LabelStyle: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxStyle: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxStyleChange(Sender: TObject);
  private const
    MARGIN_SIZE = 7;
    SQUARE_SIZE = 45;
    FULL_SIZE   = SQUARE_SIZE + (MARGIN_SIZE * 2);
    HALF_SIZE   = 0.5 * FULL_SIZE;
  private
    FTime: Double;
    FStyleId: TStyleId;
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
  System.Math,
  System.Math.Vectors;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  FTime := FTime + 2;
end;

procedure TFormMain.ComboBoxStyleChange(Sender: TObject);
begin
  inherited;
  FStyleId := TStyleId(ComboBoxStyle.ItemIndex);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FStyleId := TStyleId.Linear;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);

  var W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  var H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  var Count := W * H;

  var IX: Double := 0;
  var IY: Double := 0;
  var Start: Double := 0;
  var Now: Double := FTime;

  var Gradient: TBLGradient;

  case FStyleId of
    TStyleId.Linear:
      begin
        Gradient.Kind := TBLGradientKind.Linear;
        Gradient.SetValues(
          BLLinearGradientValues(0, MARGIN_SIZE, 0, MARGIN_SIZE + SQUARE_SIZE));
      end;

    TStyleId.Radial:
      begin
        Gradient.Kind := TBLGradientKind.Radial;
        Gradient.SetValues(
          BLRadialGradientValues(HALF_SIZE, HALF_SIZE, HALF_SIZE, HALF_SIZE - 15, HALF_SIZE));
      end;

    TStyleId.Conic:
      begin
        Gradient.Kind := TBLGradientKind.Conic;
        Gradient.SetValues(
          BLConicGradientValues(HALF_SIZE, HALF_SIZE, Pi / -2, 1));
      end;
  end;
  for var I := 0 to Count - 1 do
  begin
    var X: Double := IX * FULL_SIZE;
    var Y: Double := IY * FULL_SIZE;

    var Dur: Double := (Now - Start) + (I * 50);
    var Pos: Double := FMod(Dur, 3000) / 3000;
    var BouncePos: Double := Abs((Pos * 2) - 1);
    var R: Double := ((BouncePos * 50) + 50) / 100;
    var B: Double := ((1 - BouncePos) * 50) / 100;

    var Rotation: Double := Pos * (Pi * 2);
    var Radius: Double := BouncePos * 25;

    AContext.Save;
    AContext.Rotate(Rotation, X + HALF_SIZE, Y + HALF_SIZE);
    AContext.Translate(X, Y);

    var RoundRect := BLRoundRect(MARGIN_SIZE, MARGIN_SIZE, SQUARE_SIZE,
      SQUARE_SIZE, Radius, Radius);

    case FStyleId of
      TStyleId.Solid:
        AContext.FillRoundRect(RoundRect, BLRgba32(Trunc(R * 255), 0, Trunc(B * 255)));

      TStyleId.Linear,
      TStyleId.Radial:
        begin
          Gradient.ResetStops;
          Gradient.AddStop(0.0, $FFFF7F00);
          Gradient.AddStop(1.0, BLRgba32(Trunc(R * 255), 0, Trunc(B * 255)));
          AContext.FillRoundRect(RoundRect, Gradient);
        end;

      TStyleId.Conic:
        begin
          Gradient.ResetStops;
          Gradient.AddStop(0.0, $FFFF7F00);
          Gradient.AddStop(0.5, BLRgba32(Trunc(R * 255), 0, Trunc(B * 255)));
          Gradient.AddStop(1.0, $FFFF7F00);
          AContext.FillRoundRect(RoundRect, Gradient);
        end;
    end;
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
begin
  ACanvas.Clear(TAlphaColors.Black);

  if (FStyleId = TStyleId.Conic) then
  begin
    RenderNotSupported;
    Exit;
  end;

  var W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  var H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  var Count := W * H;

  var IX: Double := 0;
  var IY: Double := 0;
  var Start: Double := 0;
  var Now: Double := FTime;

  var C: TAlphaColorRec;
  C.G := 0;
  C.A := 255;

  for var I := 0 to Count - 1 do
  begin
    var X: Double := IX * FULL_SIZE;
    var Y: Double := IY * FULL_SIZE;

    var Dur: Double := (Now - Start) + (I * 50);
    var Pos: Double := FMod(Dur, 3000) / 3000;
    var BouncePos: Double := Abs((Pos * 2) - 1);
    var R: Double := ((BouncePos * 50) + 50) / 100;
    var B: Double := ((1 - BouncePos) * 50) / 100;

    var Rotation: Double := Pos * (Pi * 2);
    var Radius: Double := BouncePos * 25;

    var OrigMatrix := ACanvas.Matrix;
    var Matrix := TMatrix.CreateTranslation(X + HALF_SIZE, Y + HALF_SIZE) * OrigMatrix;
    Matrix := TMatrix.CreateRotation(Rotation) * Matrix;
    Matrix := TMatrix.CreateTranslation(-HALF_SIZE, -HALF_SIZE) * Matrix;
    ACanvas.SetMatrix(Matrix);

    C.R := Trunc(R * 255);
    C.B := Trunc(B * 255);

    case FStyleId of
      TStyleId.Solid:
        begin
          ACanvas.Fill.Kind := TBrushKind.Solid;
          ACanvas.Fill.Color := C.Color;
        end;

      TStyleId.Linear:
        begin
          ACanvas.Fill.Kind := TBrushKind.Gradient;
          ACanvas.Fill.Gradient.Style := TGradientStyle.Linear;
          ACanvas.Fill.Gradient.Color := $FFFF7F00;
          ACanvas.Fill.Gradient.Color1 := C.Color;
        end;

      TStyleId.Radial:
        begin
          ACanvas.Fill.Kind := TBrushKind.Gradient;
          ACanvas.Fill.Gradient.Style := TGradientStyle.Radial;
          ACanvas.Fill.Gradient.Color := C.Color;
          ACanvas.Fill.Gradient.Color1 := $FFFF7F00;
        end;
    end;

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
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var W := Trunc(PaintBox.Width + FULL_SIZE - 1) div FULL_SIZE;
  var H := Trunc(PaintBox.Height + FULL_SIZE - 1) div FULL_SIZE;
  var Count := W * H;

  var IX: Double := 0;
  var IY: Double := 0;
  var Start: Double := 0;
  var Now: Double := FTime;

  for var I := 0 to Count - 1 do
  begin
    var X: Double := IX * FULL_SIZE;
    var Y: Double := IY * FULL_SIZE;

    var Dur: Double := (Now - Start) + (I * 50);
    var Pos: Double := FMod(Dur, 3000) / 3000;
    var BouncePos: Double := Abs((Pos * 2) - 1);
    var R: Double := ((BouncePos * 50) + 50) / 100;
    var B: Double := ((1 - BouncePos) * 50) / 100;

    var Rotation: Double := Pos * (Pi * 2);
    var Radius: Double := BouncePos * 25;

    ACanvas.Save;
    ACanvas.Rotate(RadToDeg(Rotation), X + HALF_SIZE, Y + HALF_SIZE);
    ACanvas.Translate(X, Y);

    var C: TAlphaColorRec;
    C.R := Trunc(R * 255);
    C.G := 0;
    C.B := Trunc(B * 255);
    C.A := 255;

    var Shader: ISkShader := nil;
    case FStyleId of
      TStyleId.Solid:
        FSkiaFill.Color := C.Color;

      TStyleId.Linear:
        Shader := TSkShader.MakeGradientLinear(
          PointF(0, MARGIN_SIZE), PointF(0, MARGIN_SIZE + SQUARE_SIZE),
          $FFFF7F00, C.Color, TSkTileMode.Clamp);

      TStyleId.Radial:
        Shader := TSkShader.MakeGradientRadial(
          PointF(HALF_SIZE, HALF_SIZE - 15), HALF_SIZE,
          $FFFF7F00, C.Color, TSkTileMode.Clamp);

      TStyleId.Conic:
        Shader := TSkShader.MakeGradientSweep(
          PointF(HALF_SIZE, HALF_SIZE), [$FFFF7F00, C.Color, $FFFF7F00],
          [0.0, 0.5, 1.0], TSkTileMode.Clamp);
    end;

    FSkiaFill.Shader := Shader;
    ACanvas.DrawRoundRect(
      RectF(MARGIN_SIZE, MARGIN_SIZE, MARGIN_SIZE + SQUARE_SIZE, MARGIN_SIZE + SQUARE_SIZE),
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
