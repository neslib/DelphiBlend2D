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
  FMX.Colors,
  Blend2D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBarParam1: TToolBar;
    TrackBarParam1: TTrackBar;
    ToolBarGradient: TToolBar;
    LabelParam1Header: TLabel;
    LabelExtend: TLabel;
    LabelGradient: TLabel;
    LayoutExtend: TLayout;
    ComboBoxExtend: TComboBox;
    LayoutGradient: TLayout;
    ComboBoxGradient: TComboBox;
    ButtonRandom: TButton;
    LabelParam1: TLabel;
    ToolBarParam2: TToolBar;
    TrackBarParam2: TTrackBar;
    LabelParam2Header: TLabel;
    LabelParam2: TLabel;
    CheckBoxControl: TCheckBox;
    CheckBoxDither: TCheckBox;
    ToolBarColors: TToolBar;
    LabelColors: TLabel;
    ComboColorBox1: TComboColorBox;
    LayoutColor1: TLayout;
    LayoutColor2: TLayout;
    ComboColorBox2: TComboColorBox;
    LayoutColor3: TLayout;
    ComboColorBox3: TComboColorBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxGradientChange(Sender: TObject);
    procedure ComboBoxExtendChange(Sender: TObject);
    procedure TrackBarParam1Change(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ButtonRandomClick(Sender: TObject);
    procedure ComboColorBoxChange(Sender: TObject);
  private
    FPts: array [0..1] of TBLPoint;
    FGradientKind: TBLGradientKind;
    FGradientExtendMode: TBLExtendMode;
    FNumPoints: Integer;
    FClosestVertex: Integer;
    FGrabbedVertex: Integer;
  private
    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double): Integer;
    procedure UpdateLabels;
    function SliderAngle(const AScale: Double): Double;
  protected
    procedure RenderBlend2D(const AContext: TBLContext); override;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math;

{ TFormMain }

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
  FGradientKind := TBLGradientKind(ComboBoxGradient.ItemIndex);
  if (FGradientKind = TBLGradientKind.Conic) then
    FNumPoints := 1
  else
    FNumPoints := 2;

  UpdateLabels;
  PaintBox.Repaint;
end;

procedure TFormMain.ComboColorBoxChange(Sender: TObject);
begin
  inherited;
  PaintBox.Repaint;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  DisableAnimation;

  FPts[0].Reset(350, 300);
  FPts[1].Reset(200, 150);
  FNumPoints := 2;
  FClosestVertex := -1;
  FGrabbedVertex := -1;

  FGradientKind := TBLGradientKind.Linear;
  FGradientExtendMode := TBLExtendMode.Pad;

  UpdateLabels;
end;

function TFormMain.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): Integer;
begin
  Result := -1;
  var ClosestDistance: Double := Double.MaxValue;
  for var I := 0 to FNumPoints - 1 do
  begin
    var DX: Double := FPts[I].X - AP.X;
    var DY: Double := FPts[I].Y - AP.Y;
    var D: Double := Sqrt((DX * DX) + (DY * DY));
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

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  if (CheckBoxDither.IsChecked) then
    AContext.GradientQuality := TBLGradientQuality.Dither
  else
    AContext.GradientQuality := TBLGradientQuality.Nearest;

  var Gradient: TBLGradient;
  Gradient.Kind := FGradientKind;
  Gradient.ExtendMode := FGradientExtendMode;
  Gradient.ResetStops;

  Gradient.AddStop(0.0, ComboColorBox1.Color);
  Gradient.AddStop(0.5, ComboColorBox2.Color);
  Gradient.AddStop(1.0, ComboColorBox3.Color);

  case FGradientKind of
    TBLGradientKind.Linear:
      begin
        Gradient.SetValues(BLLinearGradientValues(
          FPts[0].X, FPts[0].Y, FPts[1].X, FPts[1].Y));
      end;

    TBLGradientKind.Radial:
      begin
        Gradient.SetValues(BLRadialGradientValues(
          FPts[0].X, FPts[0].Y, FPts[1].X, FPts[1].Y, TrackBarParam1.Value, TrackBarParam2.Value));
      end;

    TBLGradientKind.Conic:
      Gradient.SetValues(BLConicGradientValues(
        FPts[0].X, FPts[0].Y, SliderAngle(Pi * 2), TrackBarParam2.Value));
  end;

  AContext.FillAll(Gradient);

  if (CheckBoxControl.IsChecked) then
  begin
    for var I := 0 to FNumPoints - 1 do
    begin
      var Color: TAlphaColor := $FF007FFF;
      if (I = FClosestVertex) then
        Color := $FF00FFFF;

      AContext.StrokeCircle(FPts[I].X, FPts[I].Y, 3, Color);
    end;
  end;
end;

function TFormMain.SliderAngle(const AScale: Double): Double;
begin
  Result := TrackBarParam1.Value / 720.0 * AScale;
end;

procedure TFormMain.TrackBarParam1Change(Sender: TObject);
begin
  inherited;
  PaintBox.Repaint;
end;

procedure TFormMain.UpdateLabels;
begin
  case FGradientKind of
    TBLGradientKind.Linear:
      begin
        LabelParam1Header.Text := '(Unused)';
        LabelParam2Header.Text := '(Unused)';
      end;

    TBLGradientKind.Radial:
      begin
        LabelParam1Header.Text := 'Center Rad:';
        LabelParam2Header.Text := 'Focal Rad:';
      end;

    TBLGradientKind.Conic:
      begin
        LabelParam1Header.Text := 'Angle:';
        LabelParam2Header.Text := 'Repeat:';
      end;
  end;
end;

end.
