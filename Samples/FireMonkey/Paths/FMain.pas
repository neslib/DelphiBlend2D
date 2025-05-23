unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
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
    ToolBar1: TToolBar;
    LabelOp: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxOp: TComboBox;
    TrackBarRectCount: TTrackBar;
    LabelCountHeader: TLabel;
    ToolBar2: TToolBar;
    LabelStyle: TLabel;
    Layout1: TLayout;
    ComboBoxStyle: TComboBox;
    LabelCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarRectCountChange(Sender: TObject);
  private
    FRandom: TBLRandom;
    FPoly: TArray<TBLPoint>;
    FStep: TArray<TBLPoint>;
    FPolygon: TPolygon;
    FPolyF: TArray<TPointF>;
    FStepF: TArray<TPointF>;
    procedure SetPolyCount(const ACount: Integer);
    function RandomSign: Double;
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
  System.Math;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  var W: Double := PaintBox.Width;
  var H: Double := PaintBox.Height;

  var Count := Length(FPoly);
  var Vertex := PBLPoint(FPoly);
  var Step := PBLPoint(FStep);
  var VertexPoly := PPointF(FPolygon);
  var VertexF := PPointF(FPolyF);
  var StepF := PPointF(FStepF);

  for var I := 0 to Count - 1 do
  begin
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

    VertexPoly.X := Vertex.X;
    VertexPoly.Y := Vertex.Y;
    VertexF.X := Vertex.X;
    VertexF.Y := Vertex.Y;
    StepF.X := Step.X;
    StepF.Y := Step.Y;

    Inc(Vertex);
    Inc(Step);
    Inc(VertexPoly);
    Inc(VertexF);
    Inc(StepF);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FRandom.Reset($1234);
  SetPolyCount(50);
end;

function TFormMain.RandomSign: Double;
begin
  if (FRandom.NextDouble < 0.5) then
    Result := 1
  else
    Result := -1;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll(TAlphaColors.Black);
  AContext.FillRule := TBLFillRule.EvenOdd;

  var Poly := FPoly;
  var W: Double := PaintBox.Width;
  var H: Double := PaintBox.Height;
  var G: TBLGradient;

  case ComboBoxStyle.ItemIndex of
    1: begin
         G.Make(BLLinearGradientValues(0, 0, W, H));
         G.AddStop(0.0, $FFFF0000);
         G.AddStop(0.5, $FFAF00AF);
         G.AddStop(1.0, $FF0000FF);

         AContext.SetFillStyle(G);
         AContext.SetStrokeStyle(G);
       end;
    2: begin
         var R: Double := Min(W, H);
         G.Make(BLRadialGradientValues(W * 0.5, H * 0.5, W * 0.5, H * 0.5, R * 0.5));
         G.AddStop(0.0, $FF0000FF);
         G.AddStop(0.5, $FFAF00AF);
         G.AddStop(1.0, $FFFF0000);

         AContext.SetFillStyle(G);
         AContext.SetStrokeStyle(G);
       end;
    3: begin
         G.Make(BLConicGradientValues(W * 0.5, H * 0.5, 0));
         G.AddStop(0.00, $FFFF0000);
         G.AddStop(0.33, $FFAF00AF);
         G.AddStop(0.66, $FF0000FF);
         G.AddStop(1.00, $FFFF0000);

         AContext.SetFillStyle(G);
         AContext.SetStrokeStyle(G);
       end;
  else
    AContext.SetFillStyle(TAlphaColors.White);
    AContext.SetStrokeStyle(TAlphaColors.White);
  end;

  var I, N: Integer;
  var Op := ComboBoxOp.ItemIndex;
  case Op of
    0: AContext.FillPolygon(Poly);

    1, 2, 3:
      begin
        AContext.StrokeWidth := ((Op - 1) * 2) + 1;
        AContext.StrokePolygon(Poly);
      end;

    4, 5, 6, 7:
      begin
        N := Length(Poly);
        var Path: TBLPath;
        Path.MoveTo(Poly[0]);
        I := 3;
        while (I <= N) do
        begin
          Path.QuadTo(Poly[I - 2], Poly[I - 1]);
          Inc(I, 2);
        end;

        if (Op = 4) then
          AContext.FillPath(Path)
        else
        begin
          AContext.StrokeWidth := ((Op - 5) * 2) + 1;
          AContext.StrokePath(Path);
        end;
      end;

    8, 9, 10, 11:
      begin
        N := Length(Poly);
        var Path: TBLPath;
        Path.MoveTo(Poly[0]);
        I := 4;
        while (I <= N) do
        begin
          Path.CubicTo(Poly[I - 3], Poly[I - 2], Poly[I - 1]);
          Inc(I, 3);
        end;

        if (Op = 8) then
          AContext.FillPath(Path)
        else
        begin
          AContext.StrokeWidth := ((Op - 9) * 2) + 1;
          AContext.StrokePath(Path);
        end;
      end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  if (ComboBoxStyle.ItemIndex = 3) then
  begin
    RenderNotSupported;
    Exit;
  end;

  var Poly := FPolygon;
  var Gradient := ACanvas.Fill.Gradient;

  case ComboBoxStyle.ItemIndex of
    1,
    2: begin
         ACanvas.Fill.Kind := TBrushKind.Gradient;

         if (ComboBoxStyle.ItemIndex = 1) then
           Gradient.Style := TGradientStyle.Linear
         else
           Gradient.Style := TGradientStyle.Radial;

         while (Gradient.Points.Count < 3) do
           Gradient.Points.Add;

         Gradient.Points[0].Offset := 0;
         Gradient.Points[0].Color := $FFFF0000;
         Gradient.Points[1].Offset := 0.5;
         Gradient.Points[1].Color := $FFAF00AF;
         Gradient.Points[2].Offset := 1;
         Gradient.Points[2].Color := $FF0000FF;

         ACanvas.Stroke.Gradient.Assign(Gradient);
       end;
  else
    ACanvas.Fill.Kind := TBrushKind.Solid;
    ACanvas.Fill.Color := TAlphaColors.White;
    ACanvas.Stroke.Kind := TBrushKind.Solid;
    ACanvas.Stroke.Color := TAlphaColors.White;
  end;

  var I, N: Integer;
  var Path: TPathData;

  var Op := ComboBoxOp.ItemIndex;
  case Op of
    0: ACanvas.FillPolygon(Poly, 1);

    1, 2, 3:
      begin
        ACanvas.Stroke.Thickness := ((Op - 1) * 2) + 1;
        ACanvas.DrawPolygon(Poly, 1);
      end;

    4, 5, 6, 7:
      begin
        N := Length(Poly);
        Path := TPathData.Create;
        try
          Path.MoveTo(Poly[0]);
          I := 3;
          while (I <= N) do
          begin
            Path.SmoothCurveTo(Poly[I - 2], Poly[I - 1]);
            Inc(I, 2);
          end;

          if (Op = 4) then
            ACanvas.FillPath(Path, 1)
          else
          begin
            ACanvas.Stroke.Thickness := ((Op - 5) * 2) + 1;
            ACanvas.DrawPath(Path, 1);
          end;
        finally
          Path.Free;
        end;
      end;

    8, 9, 10, 11:
      begin
        N := Length(Poly);
        Path := TPathData.Create;
        try
          Path.MoveTo(Poly[0]);
          I := 4;
          while (I <= N) do
          begin
            Path.CurveTo(Poly[I - 3], Poly[I - 2], Poly[I - 1]);
            Inc(I, 3);
          end;

          if (Op = 8) then
            ACanvas.FillPath(Path, 1)
          else
          begin
            ACanvas.Stroke.Thickness := ((Op - 9) * 2) + 1;
            ACanvas.DrawPath(Path, 1);
          end;
        finally
          Path.Free;
        end;
      end;
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  var Poly := FPolyF;
  var W: Single := PaintBox.Width;
  var H: Single := PaintBox.Height;
  var Shader: ISkShader := nil;

  case ComboBoxStyle.ItemIndex of
    1: begin
         Shader := TSkShader.MakeGradientLinear(PointF(0, 0), PointF(W, H),
           [$FFFF0000, $FFAF00AF, $FF0000FF], [0.0, 0.5, 1.0]);
       end;

    2: begin
         var R: Single := Min(W, H);
         Shader := TSkShader.MakeGradientRadial(PointF(W * 0.5, H * 0.5), R * 0.5,
           [$FF0000FF, $FFAF00AF, $FFFF0000], [0.0, 0.5, 1.0]);
       end;

    3: begin
         Shader := TSkShader.MakeGradientSweep(PointF(W * 0.5, H * 0.5),
           [$FFFF0000, $FFAF00AF, $FF0000FF, $FFFF0000], [0.00, 0.33, 0.66, 1.00]);
       end;
  else
    FSkiaFill.Color := TAlphaColors.White;
    FSkiaStroke.Color := TAlphaColors.White;
  end;

  FSkiaFill.Shader := Shader;
  FSkiaStroke.Shader := Shader;

  var I, N: Integer;
  var Path: ISkPathBuilder;

  var Op := ComboBoxOp.ItemIndex;
  case Op of
    0: ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, Poly, FSkiaFill);

    1, 2, 3:
      begin
        FSkiaStroke.StrokeWidth := ((Op - 1) * 2) + 1;
        ACanvas.DrawPoints(TSkDrawPointsMode.Polygon, Poly, FSkiaStroke);
      end;

    4, 5, 6, 7:
      begin
        N := Length(Poly);
        Path := TSkPathBuilder.Create;
        Path.FillType := TSkPathFillType.EvenOdd;
        Path.MoveTo(Poly[0]);
        I := 3;
        while (I <= N) do
        begin
          Path.QuadTo(Poly[I - 2], Poly[I - 1]);
          Inc(I, 2);
        end;

        if (Op = 4) then
          ACanvas.DrawPath(Path.Detach, FSkiaFill)
        else
        begin
          FSkiaStroke.StrokeWidth := ((Op - 5) * 2) + 1;
          ACanvas.DrawPath(Path.Detach, FSkiaStroke);
        end;
      end;

    8, 9, 10, 11:
      begin
        N := Length(Poly);
        Path := TSkPathBuilder.Create;
        Path.FillType := TSkPathFillType.EvenOdd;
        Path.MoveTo(Poly[0]);
        I := 4;
        while (I <= N) do
        begin
          Path.CubicTo(Poly[I - 3], Poly[I - 2], Poly[I - 1]);
          Inc(I, 3);
        end;

        if (Op = 8) then
          ACanvas.DrawPath(Path.Detach, FSkiaFill)
        else
        begin
          FSkiaStroke.StrokeWidth := ((Op - 9) * 2) + 1;
          ACanvas.DrawPath(Path.Detach, FSkiaStroke);
        end;
      end;
  end;
end;
{$ENDIF}

procedure TFormMain.SetPolyCount(const ACount: Integer);
begin
  var W: Double := PaintBox.Width;
  var H: Double := PaintBox.Height;

  var Prev := Length(FPoly);
  SetLength(FPoly, ACount);
  SetLength(FStep, ACount);
  SetLength(FPolygon, ACount);
  SetLength(FPolyF, ACount);
  SetLength(FStepF, ACount);

  while (Prev < ACount) do
  begin
    FPoly[Prev].Reset(FRandom.NextDouble * W, FRandom.NextDouble * H);
    FStep[Prev].Reset(
      (FRandom.NextDouble * 0.5 + 0.5) * RandomSign,
      (FRandom.NextDouble * 0.5 + 0.5) * RandomSign);

    FPolygon[Prev] := PointF(FPoly[Prev].X, FPoly[Prev].Y);
    FPolyF[Prev] := PointF(FPoly[Prev].X, FPoly[Prev].Y);
    FStepF[Prev] := PointF(FStep[Prev].X, FStep[Prev].Y);

    Inc(Prev);
  end;
end;

procedure TFormMain.TrackBarRectCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarRectCount.Value);
  LabelCount.Text := Count.ToString;
  SetPolyCount(Count);
end;

end.
