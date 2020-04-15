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
  TShapeType = (Rect, RectPath, RoundRect, Polygon);

type
  TFormMain = class(TFormBase)
    ToolBar1: TToolBar;
    LabelCompOp: TLabel;
    LayoutCompOp: TLayout;
    ComboBoxCompOp: TComboBox;
    TrackBarRectCount: TTrackBar;
    LabelCount: TLabel;
    ToolBar2: TToolBar;
    LabelShape: TLabel;
    Layout1: TLayout;
    ComboBoxShape: TComboBox;
    TrackBarSize: TTrackBar;
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
    {$IFDEF USE_SKIA}
    FSkiaBlendMode: TSKBlendMode;
    {$ENDIF}
    procedure SetRectCount(const AValue: Integer);
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
  System.Math;

{ TFormMain }

procedure TFormMain.BeforeRender;
var
  W, H: Single;
  Vertex, Step: PPointF;
  I: Integer;
begin
  inherited;
  W := PaintBox.Width;
  H := PaintBox.Height;
  for I := 0 to Length(FCoords) - 1 do
  begin
    Vertex := @FCoords[I];
    Step := @FSteps[I];

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
  BLEND2D_COMP_OPS: array [0..8] of TBLCompOp = (TBLCompOp.SrcOver, TBLCompOp.SrcCopy,
    TBLCompOp.DstAtop, TBLCompOp.ExclusiveOr, TBLCompOp.Plus, TBLCompOp.Screen,
    TBLCompOp.Lighten, TBLCompOp.HardLight, TBLCompOp.Difference);

  {$IFDEF USE_SKIA}
  SKIA_BLEND_MODES: array [0..8] of TSKBlendMode = (TSKBlendMode.SrcOver,
    TSKBlendMode.Src, TSKBlendMode.DstAtop, TSKBlendMode.ExclusiveOr,
    TSKBlendMode.Plus, TSKBlendMode.Screen, TSKBlendMode.Lighten,
    TSKBlendMode.HardLight, TSKBlendMode.Difference);
  {$ENDIF}
var
  I: Integer;
begin
  inherited;
  I := ComboBoxCompOp.ItemIndex;
  if (I >= 0) and (I < Length(BLEND2D_COMP_OPS)) then
  begin
    FBlend2DCompOp := BLEND2D_COMP_OPS[I];
    {$IFDEF USE_SKIA}
    FSkiaBlendMode := SKIA_BLEND_MODES[I];
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
  FBlend2DCompOp := TBLCompOp.SrcOver;
  {$IFDEF USE_SKIA}
  FSkiaBlendMode := TSKBlendMode.SrcOver;
  {$ENDIF}
  SetRectCount(Trunc(TrackBarRectCount.Value));
  FRectSize := 64;
end;

procedure TFormMain.RenderBlend2D(const AContext: IBLContext);
var
  I: Integer;
  P: TPointF;
  X, Y, RectSize, HalfSize: Double;
  Path: IBLPath;
begin
  AContext.FillColor := TAlphaColors.Black;
  AContext.FillAll;

  AContext.CompOp := FBlend2DCompOp;

  RectSize := FRectSize;
  HalfSize := 0.5 * RectSize;

  case FShapeType of
    TShapeType.Rect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        AContext.FillColor := FColors[I];
        AContext.FillRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize);
      end;

    TShapeType.RectPath:
      begin
        Path := TBLPath.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];

          Path.Reset;
          Path.AddRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize);

          AContext.FillColor := FColors[I];
          AContext.FillPath(Path);
        end;
      end;

    TShapeType.Polygon:
      begin
        Path := TBLPath.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];
          X := P.X - HalfSize;
          Y := P.Y - HalfSize;

          Path.Reset;
          Path.MoveTo(X + HalfSize, Y);
          Path.LineTo(X + RectSize, Y + RectSize * (1 / 3));
          Path.LineTo(X + RectSize * (2 / 3), Y + RectSize);
          Path.LineTo(X + RectSize * (1 / 3), Y + RectSize);
          Path.LineTo(X, Y + RectSize * (1 / 3));

          AContext.FillColor := FColors[I];
          AContext.FillPath(Path);
        end;
      end;

    TShapeType.RoundRect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        AContext.FillColor := FColors[I];
        AContext.FillRoundRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize, 10);
      end;
  end;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
var
  I: Integer;
  P: TPointF;
  X, Y, RectSize, HalfSize: Single;
  Path: TPathData;
begin
  ACanvas.Clear(TAlphaColors.Black);

  if (FBlend2DCompOp <> TBLCompOp.SrcOver) then
  begin
    RenderNotSupported;
    Exit;
  end;

  RectSize := FRectSize;
  HalfSize := 0.5 * RectSize;

  case FShapeType of
    TShapeType.Rect:
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
            X := P.X - HalfSize;
            Y := P.Y - HalfSize;

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
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
var
  I: Integer;
  P: TPointF;
  X, Y, RectSize, HalfSize: Single;
  Path: ISKPath;
begin
  ACanvas.Clear(TAlphaColors.Black);
  FSkiaFill.BlendMode := FSkiaBlendMode;

  RectSize := FRectSize;
  HalfSize := 0.5 * RectSize;

  case FShapeType of
    TShapeType.Rect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        FSkiaFill.Color := FColors[I];
        ACanvas.DrawRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize, FSkiaFill);
      end;

    TShapeType.RectPath:
      begin
        Path := TSKPath.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];

          Path.Reset;
          Path.AddRect(TSKRect.CreateXYWH(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize));

          FSkiaFill.Color := FColors[I];
          ACanvas.DrawPath(Path, FSkiaFill);
        end;
      end;

    TShapeType.Polygon:
      begin
        Path := TSKPath.Create;
        for I := 0 to Length(FCoords) - 1 do
        begin
          P := FCoords[I];
          X := P.X - HalfSize;
          Y := P.Y - HalfSize;

          Path.Reset;
          Path.MoveTo(X + HalfSize, Y);
          Path.LineTo(X + RectSize, Y + RectSize * (1 / 3));
          Path.LineTo(X + RectSize * (2 / 3), Y + RectSize);
          Path.LineTo(X + RectSize * (1 / 3), Y + RectSize);
          Path.LineTo(X, Y + RectSize * (1 / 3));

          FSkiaFill.Color := FColors[I];
          ACanvas.DrawPath(Path, FSkiaFill);
        end;
      end;

    TShapeType.RoundRect:
      for I := 0 to Length(FCoords) - 1 do
      begin
        P := FCoords[I];
        FSkiaFill.Color := FColors[I];
        ACanvas.DrawRoundRect(P.X - HalfSize, P.Y - HalfSize, RectSize, RectSize,
          10, 10, FSkiaFill);
      end;
  end;
end;
{$ENDIF}

procedure TFormMain.SetRectCount(const AValue: Integer);
var
  W, H: Single;
  I: Integer;
begin
  W := PaintBox.Width;
  H := PaintBox.Height;
  I := Length(FCoords);

  SetLength(FCoords, AValue);
  SetLength(FSteps, AValue);
  SetLength(FColors, AValue);

  while (I < AValue) do
  begin
    FCoords[I] := PointF(Random * W, Random * H);
    FSteps[I] := PointF(((Random * 0.5) + 0.05) * RandomSign,
                        ((Random * 0.5) + 0.05) * RandomSign);
    FColors[I] := RandomColor;
    Inc(I);
  end;
end;

procedure TFormMain.TrackBarRectCountChange(Sender: TObject);
begin
  inherited;
  SetRectCount(Trunc(TrackBarRectCount.Value));
end;

procedure TFormMain.TrackBarSizeChange(Sender: TObject);
begin
  inherited;
  FRectSize := TrackBarSize.Value;
end;

end.
