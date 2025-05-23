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
  TShapeType = (RectA, RectU, RectPath, RoundRect, Polygon);

type
  TFormMain = class(TFormBase)
    ToolBar1: TToolBar;
    LabelCompOp: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxCompOp: TComboBox;
    TrackBarRectCount: TTrackBar;
    LabelCountHeader: TLabel;
    ToolBar2: TToolBar;
    LabelShape: TLabel;
    Layout1: TLayout;
    ComboBoxShape: TComboBox;
    TrackBarSize: TTrackBar;
    LabelSizeHeader: TLabel;
    LabelCount: TLabel;
    LabelSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBoxCompOpChange(Sender: TObject);
    procedure TrackBarRectCountChange(Sender: TObject);
    procedure ComboBoxShapeChange(Sender: TObject);
    procedure TrackBarSizeChange(Sender: TObject);
  private
    FCoords: TArray<TPointF>;
    FSteps: TArray<TPointF>;
    FColors: TArray<TAlphaColor>;
    FBlend2DCompOp: TBLCompOp;
    FShapeType: TShapeType;
    FRectSize: Double;
    FRandom: TBLRandom;
    {$IFDEF USE_SKIA}
    FSkiaBlender: ISkBlender;
    {$ENDIF}
    procedure SetRectCount(const AValue: Integer);
    function RandomSign: Double;
    function RandomColor: TAlphaColor;
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
  var W: Single := PaintBox.Width;
  var H: Single := PaintBox.Height;
  for var I := 0 to Length(FCoords) - 1 do
  begin
    var Vertex := PPointF(@FCoords[I]);
    var Step := PPointF(@FSteps[I]);

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
  end;
end;

procedure TFormMain.ComboBoxCompOpChange(Sender: TObject);
const
  BLEND2D_COMP_OPS: array [0..16] of TBLCompOp = (TBLCompOp.SrcOver,
    TBLCompOp.SrcCopy, TBLCompOp.SrcAtop, TBLCompOp.DstAtop,
    TBLCompOp.ExclusiveOr, TBLCompOp.Plus, TBLCompOp.Multiply, TBLCompOp.Screen,
    TBLCompOp.Overlay, TBLCompOp.Darken, TBLCompOp.Lighten,
    TBLCompOp.ColorDodge, TBLCompOp.ColorBurn, TBLCompOp.HardLight,
    TBLCompOp.SoftLight, TBLCompOp.Difference, TBLCompOp.Exclusion);

  {$IFDEF USE_SKIA}
  SKIA_BLEND_MODES: array [0..16] of TSkBlendMode = (TSkBlendMode.SrcOver,
    TSkBlendMode.Src, TSkBlendMode.SrcATop, TSkBlendMode.DestATop,
    TSkBlendMode.Xor, TSkBlendMode.Plus, TSkBlendMode.Multiply,
    TSkBlendMode.Screen, TSkBlendMode.Overlay, TSkBlendMode.Darken,
    TSkBlendMode.Lighten, TSkBlendMode.ColorDodge, TSkBlendMode.ColorBurn,
    TSkBlendMode.HardLight, TSkBlendMode.SoftLight, TSkBlendMode.Difference,
    TSkBlendMode.Exclusion);
  {$ENDIF}
begin
  inherited;
  var I := ComboBoxCompOp.ItemIndex;
  if (I >= 0) and (I < Length(BLEND2D_COMP_OPS)) then
  begin
    FBlend2DCompOp := BLEND2D_COMP_OPS[I];
    {$IFDEF USE_SKIA}
    FSkiaBlender := TSkBlender.MakeMode(SKIA_BLEND_MODES[I]);
    {$ENDIF}
  end;
end;

procedure TFormMain.ComboBoxShapeChange(Sender: TObject);
begin
  inherited;
  FShapeType := TShapeType(ComboBoxShape.ItemIndex);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FRandom.Reset($123456789ABCDEF);
  FBlend2DCompOp := TBLCompOp.SrcOver;
  {$IFDEF USE_SKIA}
  FSkiaBlender := TSkBlender.MakeMode(TSkBlendMode.SrcOver);
  {$ENDIF}
  SetRectCount(Trunc(TrackBarRectCount.Value));
  FRectSize := 64;
end;

function TFormMain.RandomColor: TAlphaColor;
begin
  Result := FRandom.NextUInt32;
end;

function TFormMain.RandomSign: Double;
begin
  if (FRandom.NextDouble < 0.5) then
    Result := 1.0
  else
    Result := -1.0;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.CompOp := TBLCompOp.SrcCopy;
  AContext.FillAll(BackgroundForCompOp(FBlend2DCompOp));
  AContext.CompOp := FBlend2DCompOp;

  var RectSize: Double := FRectSize;
  var HalfSize: Double := 0.5 * RectSize;

  var I: Integer;
  var P: TPointF;
  var Path: TBLPath;
  case FShapeType of
    TShapeType.RectA:
      begin
        var RectSizeI := Trunc(FRectSize);
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];
          var X := Trunc(P.X - HalfSize);
          var Y := Trunc(P.Y - HalfSize);
          AContext.FillRect(BLRectI(X, Y, RectSizeI, RectSizeI), FColors[I]);
        end;
      end;

    TShapeType.RectU:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        AContext.FillRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize, FColors[I]);
      end;

    TShapeType.RectPath:
      begin
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];

          Path.Reset;
          Path.AddRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize);

          AContext.FillPath(Path, FColors[I]);
        end;
      end;

    TShapeType.Polygon:
      begin
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];
          var X: Double := P.X - HalfSize;
          var Y: Double := P.Y - HalfSize;

          Path.Reset;
          Path.MoveTo(X + HalfSize, Y);
          Path.LineTo(X + RectSize, Y + RectSize * (1 / 3));
          Path.LineTo(X + RectSize * (2 / 3), Y + RectSize);
          Path.LineTo(X + RectSize * (1 / 3), Y + RectSize);
          Path.LineTo(X, Y + RectSize * (1 / 3));

          AContext.FillPath(Path, FColors[I]);
        end;
      end;

    TShapeType.RoundRect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        AContext.FillRoundRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize, 10, FColors[I]);
      end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);

  if (FBlend2DCompOp <> TBLCompOp.SrcOver) then
  begin
    RenderNotSupported;
    Exit;
  end;

  var RectSize: Single := FRectSize;
  var HalfSize: Single := 0.5 * RectSize;
  var I: Integer;
  var P: TPointF;
  var Path: TPathData;

  case FShapeType of
    TShapeType.RectA,
    TShapeType.RectU:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        ACanvas.Fill.Color := FColors[I];
        ACanvas.FillRect(
          RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),
          0, 0, [], 1);
      end;

    TShapeType.RectPath:
      begin
        Path := TPathData.Create;
        try
          for I := 0 to Length(FCoords) - 1 do
          begin
            P := FCoords[I];

            Path.Clear;
            Path.AddRectangle(RectF(P.X - HalfSize, P.Y - HalfSize,
              P.X + HalfSize, P.Y + HalfSize), 0, 0, []);

            ACanvas.Fill.Color := FColors[I];
            ACanvas.FillPath(Path, 1);
          end;
        finally
          Path.Free;
        end;
      end;

    TShapeType.Polygon:
      begin
        Path := TPathData.Create;
        try
          for I := 0 to Length(FCoords) - 1 do
          begin
            P := FCoords[I];
            var X: Single := P.X - HalfSize;
            var Y: Single := P.Y - HalfSize;

            Path.Clear;
            Path.MoveTo(PointF(X + HalfSize, Y));
            Path.LineTo(PointF(X + RectSize, Y + RectSize * (1 / 3)));
            Path.LineTo(PointF(X + RectSize * (2 / 3), Y + RectSize));
            Path.LineTo(PointF(X + RectSize * (1 / 3), Y + RectSize));
            Path.LineTo(PointF(X, Y + RectSize * (1 / 3)));

            ACanvas.Fill.Color := FColors[I];
            ACanvas.FillPath(Path, 1);
          end;
        finally
          Path.Free;
        end;
      end;

    TShapeType.RoundRect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        ACanvas.Fill.Color := FColors[I];
        ACanvas.FillRect(
          RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),
          10, 10, AllCorners, 1);
      end;
  end;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISkCanvas);
begin
  ACanvas.Clear(BackgroundForCompOp(FBlend2DCompOp));
  FSkiaFill.Blender := FSkiaBlender;

  var RectSize: Single := FRectSize;
  var HalfSize: Single := 0.5 * RectSize;
  var I: Integer;
  var P: TPointF;
  var PathBuilder: ISkPathBuilder;

  case FShapeType of
    TShapeType.RectA,
    TShapeType.RectU:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        FSkiaFill.Color := FColors[I];
        ACanvas.DrawRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize), FSkiaFill);
      end;

    TShapeType.RectPath:
      begin
        PathBuilder := TSkPathBuilder.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];

          PathBuilder.Reset;
          PathBuilder.AddRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize));

          FSkiaFill.Color := FColors[I];
          ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
        end;
      end;

    TShapeType.Polygon:
      begin
        PathBuilder := TSkPathBuilder.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];
          var X: Single := P.X - HalfSize;
          var Y: Single := P.Y - HalfSize;

          PathBuilder.Reset;
          PathBuilder.MoveTo(X + HalfSize, Y);
          PathBuilder.LineTo(X + RectSize, Y + RectSize * (1 / 3));
          PathBuilder.LineTo(X + RectSize * (2 / 3), Y + RectSize);
          PathBuilder.LineTo(X + RectSize * (1 / 3), Y + RectSize);
          PathBuilder.LineTo(X, Y + RectSize * (1 / 3));

          FSkiaFill.Color := FColors[I];
          ACanvas.DrawPath(PathBuilder.Detach, FSkiaFill);
        end;
      end;

    TShapeType.RoundRect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        FSkiaFill.Color := FColors[I];
        ACanvas.DrawRoundRect(RectF(P.X - HalfSize, P.Y - HalfSize, P.X + HalfSize, P.Y + HalfSize),

          10, 10, FSkiaFill);
      end;
  end;
end;
{$ENDIF}

procedure TFormMain.SetRectCount(const AValue: Integer);
begin
  var W: Single := PaintBox.Width;
  var H: Single := PaintBox.Height;
  var I := Length(FCoords);

  SetLength(FCoords, AValue);
  SetLength(FSteps, AValue);
  SetLength(FColors, AValue);

  while (I < AValue) do
  begin
    FCoords[I] := PointF(Random * W, Random * H);
    FSteps[I] := PointF(((Random * 0.5) + 0.04) * RandomSign,
                        ((Random * 0.5) + 0.04) * RandomSign);
    FColors[I] := RandomColor;
    Inc(I);
  end;
end;

procedure TFormMain.TrackBarRectCountChange(Sender: TObject);
begin
  inherited;
  var Count := Trunc(TrackBarRectCount.Value);
  LabelCount.Text := Count.ToString;
  SetRectCount(Count);
end;

procedure TFormMain.TrackBarSizeChange(Sender: TObject);
begin
  inherited;
  var Size := Trunc(TrackBarSize.Value);
  LabelSize.Text := Size.ToString;
  FRectSize := Size;
end;

end.
