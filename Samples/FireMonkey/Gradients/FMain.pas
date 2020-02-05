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
  Blend4D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBarRadius: TToolBar;
    TrackBarRadius: TTrackBar;
    ToolBarGradient: TToolBar;
    LabelGradient: TLabel;
    LayoutGradient: TLayout;
    ComboBoxGradient: TComboBox;
    LabelExtend: TLabel;
    LayoutExtend: TLayout;
    ComboBoxExtend: TComboBox;
    LabelRadius: TLabel;
    ButtonRandom: TButton;
    ButtonColors: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxGradientChange(Sender: TObject);
    procedure ComboBoxExtendChange(Sender: TObject);
    procedure TrackBarRadiusChange(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ButtonColorsClick(Sender: TObject);
    procedure ButtonRandomClick(Sender: TObject);
  private
    FGradient: IBLGradient;
    FPts: array [0..1] of TBLPoint;
    FGradientType: TBLGradientType;
    FGradientExtendMode: TBLExtendMode;
    FNumPoints: Integer;
    FClosestVertex: Integer;
    FGrabbedVertex: Integer;
  private
    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double): Integer;
  protected
    procedure RenderBlend2D(const AContext: IBLContext); override;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math;

{ TFormMain }

procedure TFormMain.ButtonColorsClick(Sender: TObject);
begin
  FGradient.ResetStops;
  FGradient.AddStop(0.0, RandomColor or $FF000000);
  FGradient.AddStop(0.5, RandomColor or $FF000000);
  FGradient.AddStop(1.0, RandomColor or $FF000000);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonRandomClick(Sender: TObject);
begin
  inherited;
  FPts[0].X := Random(Trunc(PaintBox.Width)) + 0.5;
  FPts[0].Y := Random(Trunc(PaintBox.Height)) + 0.5;
  FPts[1].X := Random(Trunc(PaintBox.Width)) + 0.5;
  FPts[1].Y := Random(Trunc(PaintBox.Height)) + 0.5;
  PaintBox.Repaint;
end;

procedure TFormMain.ComboBoxExtendChange(Sender: TObject);
begin
  inherited;
  FGradientExtendMode := TBLExtendMode(ComboBoxExtend.ItemIndex);
  PaintBox.Repaint;
end;

procedure TFormMain.ComboBoxGradientChange(Sender: TObject);
begin
  inherited;
  FGradientType := TBLGradientType(ComboBoxGradient.ItemIndex);
  if (FGradientType = TBLGradientType.Conical) then
    FNumPoints := 1
  else
    FNumPoints := 2;

  PaintBox.Repaint;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  { Disable animation and FPS testing }
  Application.OnIdle := nil;

  FPts[0].Reset(100, 80);
  FPts[1].Reset(350, 150);
  FNumPoints := 2;
  FClosestVertex := -1;
  FGrabbedVertex := -1;

  FGradientType := TBLGradientType.Linear;
  FGradientExtendMode := TBLExtendMode.Pad;

  FGradient := TBLGradient.Create;
  FGradient.AddStop(0.0, TAlphaColors.Black);
  FGradient.AddStop(1.0, TAlphaColors.White);
end;

function TFormMain.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): Integer;
var
  ClosestDistance, D, DX, DY: Double;
  I: Integer;
begin
  Result := -1;
  ClosestDistance := Double.MaxValue;
  for I := 0 to FNumPoints - 1 do
  begin
    DX := AP.X - FPts[I].X;
    DY := AP.Y - FPts[I].Y;
    D := Sqrt((DX * DX) + (DY * DY));
    if (D < ClosestDistance) and (D < AMaxDistance) then
    begin
      Result := I;
      ClosestDistance := D;
    end;
  end;
end;

procedure TFormMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    if (FClosestVertex >= 0) then
    begin
      FGrabbedVertex := FClosestVertex;
      PaintBox.Repaint;
    end;
  end;
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  P: TBLPoint;
begin
  inherited;
  P.Reset(X, Y);
  if (FGrabbedVertex < 0) then
    FClosestVertex := GetClosestVertex(P, 5)
  else
    FPts[FGrabbedVertex] := P;
  PaintBox.Repaint;
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (Button = TMouseButton.mbLeft) then
  begin
    if (FGrabbedVertex >= 0) then
    begin
      FGrabbedVertex := -1;
      PaintBox.Repaint;
    end;
  end;
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  I: Integer;
begin
  FGradient.GradientType := FGradientType;
  FGradient.ExtendMode := FGradientExtendMode;

  case FGradientType of
    TBLGradientType.Linear:
      FGradient.SetValues(BLLinearGradientValues(FPts[0].X, FPts[0].Y, FPts[1].X, FPts[1].Y));

    TBLGradientType.Radial:
      FGradient.SetValues(BLRadialGradientValues(FPts[0].X, FPts[0].Y, FPts[1].X, FPts[1].Y, TrackBarRadius.Value));

    TBLGradientType.Conical:
      FGradient.SetValues(BLConicalGradientValues(FPts[0].X, FPts[0].Y, TrackBarRadius.Value));
  end;

  AContext.FillGradient := FGradient;
  AContext.FillAll;

  for I := 0 to FNumPoints - 1 do
  begin
    if (I = FClosestVertex) then
      AContext.FillColor := $FF00FFFF
    else
      AContext.FillColor := $FF007FFF;

    AContext.FillCircle(FPts[I].X, FPts[I].Y, 2);
  end;
end;

procedure TFormMain.TrackBarRadiusChange(Sender: TObject);
begin
  inherited;
  PaintBox.Repaint;
end;

end.
