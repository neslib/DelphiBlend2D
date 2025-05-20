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
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    TrackBarCount: TTrackBar;
    LabelCountHeader: TLabel;
    LabelCount: TLabel;
    procedure TrackBarCountChange(Sender: TObject);
  private
    FAngle: Single;
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

{ The idea is based on:
    https://github.com/fogleman/gg/blob/master/examples/spiral.go }

uses
  System.Math;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  FAngle := FAngle + 0.05;
  if (FAngle >= 360) then
    FAngle := FAngle - 360;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);;

  var Path: TBLPath;
  var Count := Trunc(TrackBarCount.Value);

  var CX: Double := PaintBox.Width / 2;
  var CY: Double := PaintBox.Height / 2;
  var BaseAngle: Double := FAngle / 180 * Pi;

  for var I := 0 to Count - 1 do
  begin
    var T: Double := I * 1.01 / 1000;
    var D: Double := (T * 1000 * 0.4) + 10;
    var A: Double := BaseAngle + (T * Pi * 2 * 20);
    var S, C: Double;
    SinCos(A, S, C);
    var X: Double := CX + (C * D);
    var Y: Double := CY + (S * D);
    var R: Double := Min((T * 8) + 0.5, 10);
    Path.AddCircle(BLCircle(X, Y, R));
  end;

  AContext.FillPath(Path, TAlphaColors.White);
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
{ When using FireMonkey, it is (much) faster to draw ellipsis directly instead
  of using a path. }
begin
  ACanvas.Clear(TAlphaColors.Black);

  var Count := Trunc(TrackBarCount.Value);

  var CX: Single := PaintBox.Width / 2;
  var CY: Single := PaintBox.Height / 2;
  var BaseAngle: Single := FAngle / 180 * Pi;

  ACanvas.Fill.Color := TAlphaColors.White;
  for var I := 0 to Count - 1 do
  begin
    var T: Single := I * 1.01 / 1000;
    var D: Single := (T * 1000 * 0.4) + 10;
    var A: Single := BaseAngle + (T * Pi * 2 * 20);
    var S, C: Single;
    SinCos(A, S, C);
    var X: Single := CX + (C * D);
    var Y: Single := CY + (S * D);
    var R: Single := Min((T * 8) + 0.5, 10);

    T := I * 1.01 / 1000;
    D := (T * 1000 * 0.4) + 10;
    A := BaseAngle + (T * Pi * 2 * 20);

    ACanvas.FillEllipse(RectF(X - R, Y - R, X + R, Y + R), 1);
  end;
end;

procedure TFormMain.TrackBarCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarCount.Value);
  LabelCount.Text := Count.ToString;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var PathBuilder: ISkPathBuilder := TSkPathBuilder.Create;
  var Count := Trunc(TrackBarCount.Value);

  var CX: Single := PaintBox.Width / 2;
  var CY: Single := PaintBox.Height / 2;
  var BaseAngle: Single := FAngle / 180 * Pi;

  for var I := 0 to Count - 1 do
  begin
    var T: Single := I * 1.01 / 1000;
    var D: Single := (T * 1000 * 0.4) + 10;
    var A: Single := BaseAngle + (T * Pi * 2 * 20);
    var S, C: Single;
    SinCos(A, S, C);
    var X: Single := CX + (C * D);
    var Y: Single := CY + (S * D);
    var R: Single := Min((T * 8) + 0.5, 10);

    T := I * 1.01 / 1000;
    D := (T * 1000 * 0.4) + 10;
    A := BaseAngle + (T * Pi * 2 * 20);

    PathBuilder.AddCircle(X, Y, R);
  end;

  FSkiaFill.Color := TAlphaColors.White;
  ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
end;
{$ENDIF}

end.
