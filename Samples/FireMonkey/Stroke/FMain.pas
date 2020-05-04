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
    ToolBar1: TToolBar;
    LabelStrokeCaps: TLabel;
    ToolBar2: TToolBar;
    LabelStrokeJoin: TLabel;
    ToolBar3: TToolBar;
    TrackBarWidth: TTrackBar;
    LabelWidth: TLabel;
    LayoutStrokeCaps: TLayout;
    ComboBoxStrokeCaps: TComboBox;
    LayoutStrokeJoin: TLayout;
    ComboBoxStrokeJoin: TComboBox;
    ButtonA: TButton;
    ButtonB: TButton;
    ButtonC: TButton;
    ButtonD: TButton;
    ButtonE: TButton;
    ButtonF: TButton;
    ButtonRandom: TButton;
    ButtonX: TButton;
    ButtonY: TButton;
    ButtonZ: TButton;
    LayoutWidth: TLayout;
    LayoutMiterLimit: TLayout;
    LabelMiterLimit: TLabel;
    TrackBarMiterLimit: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure ButtonAClick(Sender: TObject);
    procedure ButtonBClick(Sender: TObject);
    procedure ButtonCClick(Sender: TObject);
    procedure ButtonDClick(Sender: TObject);
    procedure ButtonEClick(Sender: TObject);
    procedure ButtonFClick(Sender: TObject);
    procedure ButtonXClick(Sender: TObject);
    procedure ButtonYClick(Sender: TObject);
    procedure ButtonZClick(Sender: TObject);
    procedure ComboBoxStrokeCapsChange(Sender: TObject);
    procedure ComboBoxStrokeJoinChange(Sender: TObject);
    procedure TrackBarMiterLimitChange(Sender: TObject);
    procedure TrackBarWidthChange(Sender: TObject);
    procedure ButtonRandomClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FPath: IBLPath;
    FPrng: TBLRandom;
    FStrokeOptions: TBLStrokeOptions;
    FClosestVertex: Integer;
    FGrabbedVertex: Integer;
    FShowControl: Boolean;
    procedure RenderPathPoints(const AContext: IBLContext; const APath: IBLPath;
      const AHighlight: Integer; const ANormalColor, AHighlightColor: TBLRgba32);
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

procedure TFormMain.ButtonAClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(345, 333);
  FPath.CubicTo(308, 3, 33, 352, 512, 244);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonBClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(60, 177);
  FPath.QuadTo(144, 354, 396, 116);
  FPath.QuadTo(106, 184, 43.4567, 43.3091);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonCClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(488, 45);
  FPath.CubicTo(22, 331, 26, 27, 493, 338);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonDClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(276, 152);
  FPath.LineTo(194.576, 54.1927);
  FPath.LineTo(114, 239);
  FPath.LineTo(526.311, 134.453);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonEClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(161, 308);
  FPath.CubicTo(237.333, 152.509, 146.849, 108.62, 467.225, 59.9782);
  FPath.Close;
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonFClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.AddCircle(BLCircle(280, 190, 140));
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonRandomClick(Sender: TObject);
var
  MinX, MinY, MaxX, MaxY, Cmd: Double;

  function RandomX: Double;
  begin
    Result := (FPrng.NextDouble * (MaxX - MinX)) + MinX;
  end;

  function RandomY: Double;
  begin
    Result := (FPrng.NextDouble * (MaxY - MinY)) + MinY;
  end;

begin
  inherited;
  MinX := 25;
  MinY := 25;
  MaxX := PaintBox.Width - MinX;
  MaxY := PaintBox.Height - MinY;

  FPath.Clear;
  FPath.MoveTo(RandomX, RandomY);

  Cmd := FPrng.NextDouble;
  if (Cmd < 0.33) then
  begin
    FPath.LineTo(RandomX, RandomY);
    FPath.LineTo(RandomX, RandomY);
    FPath.LineTo(RandomX, RandomY);
  end
  else if (Cmd < 0.66) then
  begin
    FPath.QuadTo(RandomX, RandomY, RandomX, RandomY);
    FPath.QuadTo(RandomX, RandomY, RandomX, RandomY);
  end
  else
    FPath.CubicTo(RandomX, RandomY, RandomX, RandomY, RandomX, RandomY);

  if (FPrng.NextDouble < 0.5) then
    FPath.Close;

  PaintBox.Repaint;
end;

procedure TFormMain.ButtonXClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(300, 200);
  FPath.QuadTo(50, 200, 500, 200);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonYClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(300, 200);
  FPath.CubicTo(50, 200, 500, 200, 350, 200);
  PaintBox.Repaint;
end;

procedure TFormMain.ButtonZClick(Sender: TObject);
begin
  inherited;
  FPath.Clear;
  FPath.MoveTo(300, 200);
  FPath.LineTo(50, 200);
  FPath.LineTo(500, 200);
  FPath.LineTo(350, 200);
  PaintBox.Repaint;
end;

procedure TFormMain.ComboBoxStrokeCapsChange(Sender: TObject);
begin
  inherited;
  FStrokeOptions.SetCaps(TBLStrokeCap(ComboBoxStrokeCaps.ItemIndex));
  PaintBox.Repaint;
end;

procedure TFormMain.ComboBoxStrokeJoinChange(Sender: TObject);
begin
  inherited;
  FStrokeOptions.Join := TBLStrokeJoin(ComboBoxStrokeJoin.ItemIndex);
  PaintBox.Repaint;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FClosestVertex := -1;
  FGrabbedVertex := -1;
  FShowControl := True;
  FPath := TBLPath.Create;
  FPrng.Reset(TThread.GetTickCount);
  FStrokeOptions.Reset;
  FStrokeOptions.Width := TrackBarWidth.Value;
  FStrokeOptions.MiterLimit := TrackBarMiterLimit.Value;
  ButtonAClick(nil);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if (KeyChar = 'z') then
  begin
    FShowControl := not FShowControl;
    PaintBox.Repaint;
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
    FClosestVertex := FPath.GetClosestVertex(P, 5)
  else
    FPath.SetVertexAt(FGrabbedVertex, TBLPathCmd.Move, P, True);
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
  S: IBLPath;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  S := TBLPath.Create;
  S.AddStrokedPath(FPath, FStrokeOptions, TBLApproximationOptions.Default);

  AContext.FillColor := $8F003FAA;
  AContext.FillPath(S);

  if (FShowControl) then
  begin
    AContext.StrokeColor := $FF0066AA;
    AContext.StrokePath(S);
    RenderPathPoints(AContext, S, Integer.MaxValue, $7F007FFF, $FFFFFFFF);
  end;

  AContext.StrokeColor := TAlphaColors.White;
  AContext.StrokePath(FPath);

  RenderPathPoints(AContext, FPath, FClosestVertex, $FFFFFFFF, $FF00FFFF);
end;

procedure TFormMain.RenderPathPoints(const AContext: IBLContext;
  const APath: IBLPath; const AHighlight: Integer; const ANormalColor,
  AHighlightColor: TBLRgba32);
var
  I, Count: Integer;
  Vtx: PBLPoint;
begin
  Count := APath.Count;
  Vtx := APath.VertexData;

  for I := 0 to Count - 1 do
  begin
    if (not Vtx.X.IsInfinity) then
    begin
      if (I = AHighlight) then
        AContext.FillColor := AHighlightColor
      else
        AContext.FillColor := ANormalColor;

      AContext.FillCircle(Vtx.X, Vtx.Y, 2.5);
    end;
    Inc(Vtx);
  end;
end;

procedure TFormMain.TrackBarMiterLimitChange(Sender: TObject);
begin
  inherited;
  FStrokeOptions.MiterLimit := TrackBarMiterLimit.Value;
  PaintBox.Repaint;
end;

procedure TFormMain.TrackBarWidthChange(Sender: TObject);
begin
  inherited;
  FStrokeOptions.Width := TrackBarWidth.Value;
  PaintBox.Repaint;
end;

end.
