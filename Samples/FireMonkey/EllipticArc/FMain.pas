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
  Blend2D,
  FBase;

type
  TFormMain = class(TFormBase)
    ToolBarXRadius: TToolBar;
    TrackBarXRadius: TTrackBar;
    LabelXRadius: TLabel;
    CheckBoxLargeArc: TCheckBox;
    ToolBarYRadius: TToolBar;
    TrackBarYRadius: TTrackBar;
    LabelYRadius: TLabel;
    CheckBoxSweepArc: TCheckBox;
    ToolBarAngle: TToolBar;
    TrackBarAngle: TTrackBar;
    LabelAngle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
  private
    FPts: array [0..1] of TBLPoint;
    FClosestVertex: Integer;
    FGrabbedVertex: Integer;
    procedure RenderPathPoints(const AContext: IBLContext; const APath: IBLPath;
      const AColor: TBLRgba32);
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FPts[0].Reset(124, 180);
  FPts[1].Reset(296, 284);
  FClosestVertex := -1;
  FGrabbedVertex := -1;
end;

function TFormMain.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): Integer;
var
  ClosestDistance, D, DX, DY: Double;
  I: Integer;
begin
  Result := -1;
  ClosestDistance := Double.MaxValue;
  for I := 0 to Length(FPts) - 1 do
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
  Radius, Start, Stop: TBLPoint;
  Angle: Double;
  P: IBLPath;
  I: Integer;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  Radius.Reset(TrackBarXRadius.Value, TrackBarYRadius.Value);
  Start := FPts[0];
  Stop := FPts[1];
  Angle := TrackBarAngle.Value / 180 * Pi;

  { Render all arcs before rendering the one that is selected. }
  P := TBLPath.Create;

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, False, False, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, False, True, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, True, False, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, True, True, Stop);

  AContext.StrokeColor := $40FFFFFF;
  AContext.StrokePath(P);

  { Render elliptic arc based on the given parameters. }
  P.Clear;
  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, CheckBoxLargeArc.IsChecked,
    CheckBoxSweepArc.IsChecked, Stop);

  AContext.StrokeColor := $FFFFFFFF;
  AContext.StrokePath(P);

  { Render all points of the path (as the arc was split into segments). }
  RenderPathPoints(AContext, P, $FF808080);

  { Render the rest of the UI (draggable points). }
  for I := 0 to Length(FPts) - 1 do
  begin
    if (I = FClosestVertex) then
      AContext.FillColor := $FF00FFFF
    else
      AContext.FillColor := $FF007FFF;

    AContext.FillCircle(FPts[I].X, FPts[I].Y, 2.5);
  end;
end;

procedure TFormMain.RenderPathPoints(const AContext: IBLContext;
  const APath: IBLPath; const AColor: TBLRgba32);
var
  I, Count: Integer;
  Vtx: PBLPoint;
begin
  Count := APath.Count;
  Vtx := APath.VertexData;

  AContext.FillColor := AColor;
  for I := 0 to Count - 1 do
  begin
    if (not Vtx.X.IsInfinity) then
      AContext.FillCircle(Vtx.X, Vtx.Y, 2);

    Inc(Vtx);
  end;
end;

end.
