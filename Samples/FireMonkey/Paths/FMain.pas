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

    Inc(Vertex);
    Inc(Step);
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
         G.AddStop(0.0, $FFFF0000);
         G.AddStop(0.5, $FFAF00AF);
         G.AddStop(1.0, $FF0000FF);

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
//  ACanvas.Clear(TAlphaColors.Black);
//
//  if (FBlend2DCompOp <> TBLCompOp.SrcOver) then
//  begin
//    RenderNotSupported;
//    Exit;
//  end;
//
//  var RectSize: Single := FRectSize;
//  var HalfSize: Single := 0.5 * RectSize;
//  var I: Integer;
//  var P: TPointF;
//  var Path: TPathData;
//
//  case FShapeType of
//    TShapeType.RectA,
//    TShapeType.RectU:
//      for I := 0 to Length(FCoords) - 1 do
//      begin
//        P := FCoords[I];
//        ACanvas.Fill.Color := FColors[I];
//        ACanvas.FillRect(
//          RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),
//          0, 0, [], 1);
//      end;
//
//    TShapeType.RectPath:
//      begin
//        Path := TPathData.Create;
//        try
//          for I := 0 to Length(FCoords) - 1 do
//          begin
//            P := FCoords[I];
//
//            Path.Clear;
//            Path.AddRectangle(RectF(P.X - HalfSize, P.Y - HalfSize,
//              P.X + HalfSize, P.Y + HalfSize), 0, 0, []);
//
//            ACanvas.Fill.Color := FColors[I];
//            ACanvas.FillPath(Path, 1);
//          end;
//        finally
//          Path.Free;
//        end;
//      end;
//
//    TShapeType.Polygon:
//      begin
//        Path := TPathData.Create;
//        try
//          for I := 0 to Length(FCoords) - 1 do
//          begin
//            P := FCoords[I];
//            var X: Single := P.X - HalfSize;
//            var Y: Single := P.Y - HalfSize;
//
//            Path.Clear;
//            Path.MoveTo(PointF(X + HalfSize, Y));
//            Path.LineTo(PointF(X + RectSize, Y + RectSize * (1 / 3)));
//            Path.LineTo(PointF(X + RectSize * (2 / 3), Y + RectSize));
//            Path.LineTo(PointF(X + RectSize * (1 / 3), Y + RectSize));
//            Path.LineTo(PointF(X, Y + RectSize * (1 / 3)));
//
//            ACanvas.Fill.Color := FColors[I];
//            ACanvas.FillPath(Path, 1);
//          end;
//        finally
//          Path.Free;
//        end;
//      end;
//
//    TShapeType.RoundRect:
//      for I := 0 to Length(FCoords) - 1 do
//      begin
//        P := FCoords[I];
//        ACanvas.Fill.Color := FColors[I];
//        ACanvas.FillRect(
//          RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),
//          10, 10, AllCorners, 1);
//      end;
//  end;
end;

procedure TFormMain.SetPolyCount(const ACount: Integer);
begin
  var W: Double := PaintBox.Width;
  var H: Double := PaintBox.Height;

  var Prev := Length(FPoly);
  SetLength(FPoly, ACount);
  SetLength(FStep, ACount);

  while (Prev < ACount) do
  begin
    FPoly[Prev].Reset(FRandom.NextDouble * W, FRandom.NextDouble * H);
    FStep[Prev].Reset(
      (FRandom.NextDouble * 0.5 + 0.5) * RandomSign,
      (FRandom.NextDouble * 0.5 + 0.5) * RandomSign);

    Inc(Prev);
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
//  ACanvas.Clear(BackgroundForCompOp(FBlend2DCompOp));
//  FSkiaFill.Blender := FSkiaBlender;
//
//  var RectSize: Single := FRectSize;
//  var HalfSize: Single := 0.5 * RectSize;
//  var I: Integer;
//  var P: TPointF;
//  var PathBuilder: ISkPathBuilder;
//
//  case FShapeType of
//    TShapeType.RectA,
//    TShapeType.RectU:
//      for I := 0 to Length(FCoords) - 1 do
//      begin
//        P := FCoords[I];
//        FSkiaFill.Color := FColors[I];
//        ACanvas.DrawRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize), FSkiaFill);
//      end;
//
//    TShapeType.RectPath:
//      begin
//        PathBuilder := TSkPathBuilder.Create;
//        for I := 0 to Length(FCoords) - 1 do
//        begin
//          P := FCoords[I];
//
//          PathBuilder.Reset;
//          PathBuilder.AddRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize));
//
//          FSkiaFill.Color := FColors[I];
//          ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
//        end;
//      end;
//
//    TShapeType.Polygon:
//      begin
//        PathBuilder := TSkPathBuilder.Create;
//        for I := 0 to Length(FCoords) - 1 do
//        begin
//          P := FCoords[I];
//          var X: Single := P.X - HalfSize;
//          var Y: Single := P.Y - HalfSize;
//
//          PathBuilder.Reset;
//          PathBuilder.MoveTo(X + HalfSize, Y);
//          PathBuilder.LineTo(X + RectSize, Y + RectSize * (1 / 3));
//          PathBuilder.LineTo(X + RectSize * (2 / 3), Y + RectSize);
//          PathBuilder.LineTo(X + RectSize * (1 / 3), Y + RectSize);
//          PathBuilder.LineTo(X, Y + RectSize * (1 / 3));
//
//          FSkiaFill.Color := FColors[I];
//          ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
//        end;
//      end;
//
//    TShapeType.RoundRect:
//      for I := 0 to Length(FCoords) - 1 do
//      begin
//        P := FCoords[I];
//        FSkiaFill.Color := FColors[I];
//        ACanvas.DrawRoundRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),
//
//          10, 10, FSkiaFill);
//      end;
//  end;
end;
{$ENDIF}

procedure TFormMain.TrackBarRectCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarRectCount.Value);
  LabelCount.Text := Count.ToString;
  SetPolyCount(Count);
end;

end.
