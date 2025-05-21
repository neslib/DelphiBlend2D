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
    StatusBar: TStatusBar;
    LabelStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ControlChange(Sender: TObject);
  private
    FPts: array [0..1] of TBLPoint;
    FClosestVertex: Integer;
    FGrabbedVertex: Integer;
    FStatus: String;
    procedure RenderPathPoints(const AContext: TBLContext; const APath: TBLPath;
      const AColor: TBLRgba32);
    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double): Integer;
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

procedure TFormMain.ControlChange(Sender: TObject);
begin
  inherited;
  PaintBox.Repaint;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  DisableAnimation;
  FPts[0].Reset(124, 180);
  FPts[1].Reset(296, 284);
  FClosestVertex := -1;
  FGrabbedVertex := -1;
end;

function TFormMain.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): Integer;
begin
  Result := -1;
  var ClosestDistance: Double := Double.MaxValue;
  for var I := 0 to Length(FPts) - 1 do
  begin
    var DX: Double := AP.X - FPts[I].X;
    var DY: Double := AP.Y - FPts[I].Y;
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
begin
  inherited;
  var P := BLPoint(X, Y);
  if (FGrabbedVertex < 0) then
    FClosestVertex := GetClosestVertex(P, {$IFDEF MOBILE}20{$ELSE}5{$ENDIF})
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
  AContext.FillAll(TAlphaColors.Black);

  var Radius := BLPoint(TrackBarXRadius.Value, TrackBarYRadius.Value);
  var Start := FPts[0];
  var Stop := FPts[1];
  var Angle: Double := TrackBarAngle.Value / 180 * Pi;

  { Render all arcs before rendering the one that is selected. }
  var P: TBLPath;

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, False, False, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, False, True, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, True, False, Stop);

  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, True, True, Stop);

  AContext.StrokePath(P, $40FFFFFF);

  { Render elliptic arc based on the given parameters. }
  P.Clear;
  P.MoveTo(Start);
  P.EllipticArcTo(Radius, Angle, CheckBoxLargeArc.IsChecked,
    CheckBoxSweepArc.IsChecked, Stop);

  AContext.StrokePath(P, $FFFFFFFF);

  { Render all points of the path (as the arc was split into segments). }
  RenderPathPoints(AContext, P, $FF808080);

  { Render the rest of the UI (draggable points). }
  for var I := 0 to Length(FPts) - 1 do
  begin
    var Color: TAlphaColor := $FF007FFF;
    if (I = FClosestVertex) then
      Color := $FF00FFFF;

    AContext.FillCircle(FPts[I].X, FPts[I].Y, 2.5, Color);
  end;

  FStatus := Format('<path d="M%.1f %.1f A%.1f %.1f %.3f %d %d %.1f %.1f" />',
    [Start.X, Start.Y, Radius.X, Radius.Y, Angle * Pi / 180,
     Ord(CheckBoxLargeArc.IsChecked), Ord(CheckBoxSweepArc.IsChecked),
     Stop.X, Stop.Y]);

  TThread.ForceQueue(nil,
    procedure
    begin
      LabelStatus.Text := FStatus;
    end);
end;

procedure TFormMain.RenderPathPoints(const AContext: TBLContext;
  const APath: TBLPath; const AColor: TBLRgba32);
begin
  var Count := APath.Size;
  var Vtx := APath.VertexData;

  AContext.SetFillStyle(AColor);
  for var I := 0 to Count - 1 do
  begin
    if (not Vtx.X.IsInfinity) then
      AContext.FillCircle(Vtx.X, Vtx.Y, 2);

    Inc(Vtx);
  end;
end;

end.
