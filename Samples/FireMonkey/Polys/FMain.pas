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
    LabelOperation: TLabel;
    LayoutOperation: TLayout;
    ComboBoxOperation: TComboBox;
    TrackBarPointCount: TTrackBar;
    LabelCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarPointCountChange(Sender: TObject);
  private
    FPolyF: TArray<TPointF>;
    FPolyD: TArray<TBLPoint>;
    FStep: TArray<TPointF>;
    procedure SetPolyCount(const AValue: Integer);
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

const
  OP_FILL_POLY        = 0;
  OP_STROKE_POLY_W1   = 1;
  OP_STROKE_POLY_W3   = 2;
  OP_STROKE_POLY_W5   = 3;
  OP_FILL_CUBICS      = 4;
  OP_STROKE_CUBICS_W1 = 5;
  OP_STROKE_CUBICS_W3 = 6;
  OP_STROKE_CUBICS_W5 = 7;

{ TFormMain }

procedure TFormMain.BeforeRender;
var
  W, H: Single;
  I, Count: Integer;
  Vertex, Step: PPointF;
begin
  inherited;
  W := PaintBox.Width;
  H := PaintBox.Height;
  Count := Length(FPolyF);
  for I := 0 to Count - 1 do
  begin
    Vertex := @FPolyF[I];
    Step := @FStep[I];

    Vertex^ := Vertex^ + Step^;

    if (Vertex.X <= 0) or (Vertex.X >= W) then
    begin
      Step.X := -Step.X;
      Vertex.X := Min(Vertex.X + Step.X, W);
    end;

    if (Vertex.Y <= 0) or (Vertex.Y >= H) then
    begin
      Step.Y := -Step.Y;
      Vertex.Y := Min(Vertex.Y + Step.Y, H);
    end;

    FPolyD[I].Reset(Vertex.X, Vertex.Y);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  SetPolyCount(Trunc(TrackBarPointCount.Value));
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  Poly: TArray<TBLPoint>;
  Path: IBLPath;
  I, N, Op: Integer;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  AContext.FillRule := TBLFillRule.EvenOdd;
  AContext.FillColor := TAlphaColors.White;
  AContext.StrokeColor := TAlphaColors.White;

  Poly := FPolyD;
  Op := ComboBoxOperation.ItemIndex;
  case Op of
    OP_FILL_POLY:
      AContext.FillPolygon(Poly);

    OP_STROKE_POLY_W1..OP_STROKE_POLY_W5:
      begin
        AContext.StrokeWidth := ((Op - OP_STROKE_POLY_W1) * 2) + 1;
        AContext.StrokePolygon(Poly);
      end;
  else
    N := Length(Poly);
    Path := TBLPath.Create;
    Path.MoveTo(Poly[0]);
    I := 4;
    while (I < N) do
    begin
      Path.CubicTo(Poly[I - 3], Poly[I - 2], Poly[I - 3]);
      Inc(I, 3);
    end;

    if (Op = OP_FILL_CUBICS) then
      AContext.FillPath(Path)
    else
    begin
      AContext.StrokeWidth := ((Op - OP_STROKE_CUBICS_W1) * 2) + 1;
      AContext.StrokePath(Path)
    end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
var
  Poly: TPolygon;
  Path: TPathData;
  I, N, Op: Integer;
begin
  ACanvas.Clear(TAlphaColors.Black);

  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.Stroke.Color := TAlphaColors.White;

  Poly := TPolygon(FPolyF);
  Op := ComboBoxOperation.ItemIndex;
  case Op of
    OP_FILL_POLY:
      ACanvas.FillPolygon(Poly, 1);

    OP_STROKE_POLY_W1..OP_STROKE_POLY_W5:
      begin
        ACanvas.Stroke.Thickness := ((Op - OP_STROKE_POLY_W1) * 2) + 1;
        ACanvas.DrawPolygon(Poly, 1);
      end;
  else
    N := Length(Poly);
    Path := TPathData.Create;
    try
      Path.MoveTo(Poly[0]);
      I := 4;
      while (I < N) do
      begin
        Path.CurveTo(Poly[I - 3], Poly[I - 2], Poly[I - 3]);
        Inc(I, 3);
      end;

      if (Op = OP_FILL_CUBICS) then
        ACanvas.FillPath(Path, 1)
      else
      begin
        ACanvas.Stroke.Thickness := ((Op - OP_STROKE_CUBICS_W1) * 2) + 1;
        ACanvas.DrawPath(Path, 1);
      end;
    finally
      Path.Free;
    end;
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  Poly: TArray<TSKPoint>;
  Path: ISKPath;
  I, N, Op: Integer;
begin
  ACanvas.Clear(TAlphaColors.Black);

  FSkiaFill.Color := TAlphaColors.White;
  FSkiaStroke.Color := TAlphaColors.White;

  Poly := TArray<TSKPoint>(FPolyF);
  Op := ComboBoxOperation.ItemIndex;
  case Op of
    OP_FILL_POLY:
      begin
        Path := TSKPath.Create;
        Path.FillType := TSKPathFillType.EvenOdd;
        Path.AddPoly(Poly);
        ACanvas.DrawPath(Path, FSkiaFill);
      end;

    OP_STROKE_POLY_W1..OP_STROKE_POLY_W5:
      begin
        FSkiaStroke.StrokeWidth := ((Op - OP_STROKE_POLY_W1) * 2) + 1;
        ACanvas.DrawPoints(TSKPointMode.Polygon, Poly, FSkiaStroke);
      end;
  else
    N := Length(Poly);
    Path := TSKPath.Create;
    Path.FillType := TSKPathFillType.EvenOdd;
    Path.MoveTo(Poly[0]);
    I := 4;
    while (I < N) do
    begin
      Path.CubicTo(Poly[I - 3], Poly[I - 2], Poly[I - 3]);
      Inc(I, 3);
    end;

    if (Op = OP_FILL_CUBICS) then
      ACanvas.DrawPath(Path, FSkiaFill)
    else
    begin
      FSkiaStroke.StrokeWidth := ((Op - OP_STROKE_CUBICS_W1) * 2) + 1;
      ACanvas.DrawPath(Path, FSkiaStroke);
    end;
  end;
end;
{$ENDIF}

procedure TFormMain.SetPolyCount(const AValue: Integer);
var
  W, H: Double;
  P: TPointF;
  Prev: Integer;
begin
  W := PaintBox.Width;
  H := PaintBox.Height;
  Prev := Length(FPolyF);
  SetLength(FPolyF, AValue);
  SetLength(FPolyD, AValue);
  SetLength(FStep, AValue);

  while (Prev < AValue) do
  begin
    P := PointF(Random * W, Random * H);
    FPolyF[Prev] := P;
    FPolyD[Prev].Reset(P.X, P.Y);
    FStep[Prev] := PointF((Random * 0.5 + 0.05) * RandomSign, (Random * 0.5 + 0.05) * RandomSign);
    Inc(Prev);
  end;
end;

procedure TFormMain.TrackBarPointCountChange(Sender: TObject);
begin
  inherited;
  SetPolyCount(Trunc(TrackBarPointCount.Value));
end;

end.
