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
  Blend2D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    TrackBarCount: TTrackBar;
  private
    FAngle: Single;
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

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  Path: IBLPath;
  I, Count: Integer;
  CX, CY, BaseAngle, T, D, A, X, Y, R, S, C: Double;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  Path := TBLPath.Create;
  Count := Trunc(TrackBarCount.Value);

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;
  BaseAngle := FAngle / 180 * Pi;

  for I := 0 to Count - 1 do
  begin
    T := I * 1.01 / 1000;
    D := (T * 1000 * 0.4) + 10;
    A := BaseAngle + (T * Pi * 2 * 20);
    SinCos(A, S, C);
    X := CX + (C * D);
    Y := CY + (S * D);
    R := Min((T * 8) + 0.5, 10);
    Path.AddCircle(BLCircle(X, Y, R));
  end;

  AContext.FillColor := TAlphaColors.White;
  AContext.FillPath(Path);
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
{ When using FireMonkey, it is (much) faster to draw ellipsis directly instead
  of using a path. }
var
  I, Count: Integer;
  CX, CY, BaseAngle, T, D, A, X, Y, R, S, C: Double;
begin
  ACanvas.Clear(TAlphaColors.Black);

  Count := Trunc(TrackBarCount.Value);

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;
  BaseAngle := FAngle / 180 * Pi;

  ACanvas.Fill.Color := TAlphaColors.White;
  for I := 0 to Count - 1 do
  begin
    T := I * 1.01 / 1000;
    D := (T * 1000 * 0.4) + 10;
    A := BaseAngle + (T * Pi * 2 * 20);
    SinCos(A, S, C);
    X := CX + (C * D);
    Y := CY + (S * D);
    R := Min((T * 8) + 0.5, 10);
    ACanvas.FillEllipse(RectF(X - R, Y - R, X + R, Y + R), 1);
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  Path: ISKPath;
  I, Count: Integer;
  CX, CY, BaseAngle, T, D, A, X, Y, R, S, C: Double;
begin
  ACanvas.Clear(TAlphaColors.Black);

  Path := TSKPath.Create;
  Count := Trunc(TrackBarCount.Value);

  CX := PaintBox.Width / 2;
  CY := PaintBox.Height / 2;
  BaseAngle := FAngle / 180 * Pi;

  for I := 0 to Count - 1 do
  begin
    T := I * 1.01 / 1000;
    D := (T * 1000 * 0.4) + 10;
    A := BaseAngle + (T * Pi * 2 * 20);
    SinCos(A, S, C);
    X := CX + (C * D);
    Y := CY + (S * D);
    R := Min((T * 8) + 0.5, 10);
    Path.AddCircle(X, Y, R);
  end;

  FSkiaFill.Color := TAlphaColors.White;
  ACanvas.DrawPath(Path, FSkiaFill);
end;
{$ENDIF}

end.
