unit FMain;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
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
  TTigerPath = class
  public
    BLPath: TBLPath;
    BLStrokedPath: TBLPath;
    BLStrokeOptions: TBLStrokeOptions;
    BLFillRule: TBLFillRule;

    {$IFDEF USE_SKIA}
    SKPath: ISkPath;
    SKStrokeCap: TSkStrokeCap;
    SKStrokeJoin: TSkStrokeJoin;
    {$ENDIF}

    FillColor: TAlphaColor;
    StrokeColor: TAlphaColor;

    Fill: Boolean;
    Stroke: Boolean;
  public
    constructor Create;
  end;

type
  TTiger = class
  public
    FPaths: TObjectList<TTigerPath>;
  public
    constructor Create;
    destructor Destroy; override;

    property Paths: TObjectList<TTigerPath> read FPaths;
  end;

type
  TFormMain = class(TFormBase)
    ToolBar: TToolBar;
    TrackBarScale: TTrackBar;
    LabelCaching: TLabel;
    LayoutCaching: TLayout;
    ComboBoxCaching: TComboBox;
    LabelZoomHeader: TLabel;
    LabelZoom: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarScaleChange(Sender: TObject);
  private const
    MIN_X = 17;
    MIN_Y = 53;
    MAX_X = 562;
    MAX_Y = 613;
  private
    FTiger: TTiger;
    FRot: Double;
    FScale: Double;
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
{$POINTERMATH ON}

uses
  System.Math,
  System.Math.Vectors,
  Tiger;

{ TFormMain }

procedure TFormMain.BeforeRender;
begin
  inherited;
  FRot := FRot + 0.25;
  if (FRot >= 360) then
    FRot := FRot - 360;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  FTiger := TTiger.Create;
  FScale := 1;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  inherited;
  FTiger.Free;
end;

procedure TFormMain.RenderBlend2D(const AContext: TBLContext);
begin
  AContext.FillAll($FF00007F);

  var CacheStroke := (ComboBoxCaching.ItemIndex = 1);

  var S: Double := Min(PaintBox.Width / (MAX_X - MIN_X), PaintBox.Height / (MAX_Y - MIN_Y)) * FScale;

  var Transform: TBLMatrix2D;
  Transform.Reset;
  Transform.Rotate((FRot / 180) * Pi, MIN_X + (MAX_X / 2), MIN_Y + (MAX_Y / 2));
  Transform.PostTranslate(-MAX_X / 2, -MAX_Y / 2);

  AContext.Save;
  AContext.Translate(PaintBox.Width / 2, PaintBox.Height / 2);
  AContext.Scale(S);
  AContext.ApplyTransform(Transform);

  for var I := 0 to FTiger.Paths.Count - 1 do
  begin
    var TP := FTiger.Paths[I];

    if (TP.Fill) then
    begin
      AContext.FillRule := TP.BLFillRule;
      AContext.FillPath(TP.BLPath, TP.FillColor);
    end;

    if (TP.Stroke) then
    begin
      if CacheStroke then
        AContext.FillPath(TP.BLStrokedPath, TP.StrokeColor)
      else
      begin
        AContext.StrokeOptions := TP.BLStrokeOptions;
        AContext.StrokePath(TP.BLPath, TP.StrokeColor);
      end;
    end;
  end;

  AContext.Restore;
end;

procedure TFormMain.RenderFireMonkey(const ACanvas: TCanvas);
begin
  ACanvas.Clear(TAlphaColors.Black);
  RenderNotSupported;
end;

{$IFDEF USE_SKIA}
procedure TFormMain.RenderSkia(const ACanvas: ISKCanvas);
begin
  ACanvas.Clear($FF00007F);

  var S: Single := Min(PaintBox.Width / (MAX_X - MIN_X), PaintBox.Height / (MAX_Y - MIN_Y)) * FScale;

  var Transform := TMatrix.CreateRotation((FRot / 180) * Pi);
  Transform := TMatrix.CreateTranslation(-MAX_X / 2, -MAX_Y / 2) * Transform;

  ACanvas.Save;
  ACanvas.Translate(PaintBox.Width / 2, PaintBox.Height / 2);
  ACanvas.Scale(S, S);
  ACanvas.Concat(Transform);

  for var I := 0 to FTiger.Paths.Count - 1 do
  begin
    var TP := FTiger.Paths[I];

    if (TP.Fill) then
    begin
      FSkiaFill.Color := TP.FillColor;
      ACanvas.DrawPath(TP.SKPath, FSkiaFill);
    end;

    if (TP.Stroke) then
    begin
      FSkiaStroke.StrokeCap := TP.SKStrokeCap;
      FSkiaStroke.StrokeJoin := TP.SKStrokeJoin;
      FSkiaStroke.StrokeMiter := TP.BLStrokeOptions.MiterLimit;
      FSkiaStroke.StrokeWidth := TP.BLStrokeOptions.Width;
      FSkiaStroke.Color := TP.StrokeColor;
      ACanvas.DrawPath(TP.SKPath, FSkiaStroke);
    end;
  end;
end;
{$ENDIF}

procedure TFormMain.TrackBarScaleChange(Sender: TObject);
begin
  inherited;
  FScale := TrackBarScale.Value;
  LabelZoom.Text := Format('%.3f', [FScale]);
end;

{ TTigerPath }

constructor TTigerPath.Create;
begin
  inherited;
  BLStrokeOptions.Reset;
end;

{ TTiger }

constructor TTiger.Create;
begin
  inherited;
  FPaths := TObjectList<TTigerPath>.Create;
  var C := PUTF8Char(@TTigerData.COMMANDS[0]);
  var CEnd := C;
  Inc(CEnd, Length(TTigerData.COMMANDS));
  var P := PSingle(@TTigerData.POINTS[0]);
  var H: Single := TTigerData.HEIGHT;
  var Color: TAlphaColorRec;
  Color.A := 255;

  while (C < CEnd) do
  begin
    var TP := TTigerPath.Create;
    FPaths.Add(TP);

    {$IFDEF USE_SKIA}
    var PathBuilder: ISkPathBuilder := TSkPathBuilder.Create;
    {$ENDIF}
    { Fill params }
    case C^ of
      'F': begin
             TP.Fill := True;
             TP.BLFillRule := TBLFillRule.NonZero;
             {$IFDEF USE_SKIA}
             PathBuilder.FillType := TSKPathFillType.Winding;
             {$ENDIF}
           end;

      'E': begin
             TP.Fill := True;
             TP.BLFillRule := TBLFillRule.EvenOdd;
             {$IFDEF USE_SKIA}
             PathBuilder.FillType := TSKPathFillType.EvenOdd;
             {$ENDIF}
           end;
    end;
    Inc(C);

    { Stroke params }
    TP.Stroke := (C^ = 'S');
    Inc(C);

    case C^ of
      'B': begin
             TP.BLStrokeOptions.SetCaps(TBLStrokeCap.Butt);
             {$IFDEF USE_SKIA}
             TP.SKStrokeCap := TSKStrokeCap.Butt;
             {$ENDIF}
           end;
      'R': begin
             TP.BLStrokeOptions.SetCaps(TBLStrokeCap.Round);
             {$IFDEF USE_SKIA}
             TP.SKStrokeCap := TSKStrokeCap.Round;
             {$ENDIF}
           end;
      'S': begin
             TP.BLStrokeOptions.SetCaps(TBLStrokeCap.Square);
             {$IFDEF USE_SKIA}
             TP.SKStrokeCap := TSKStrokeCap.Square;
             {$ENDIF}
           end;
    end;
    Inc(C);

    case C^ of
      'M': begin
             TP.BLStrokeOptions.Join := TBLStrokeJoin.MiterBevel;
             {$IFDEF USE_SKIA}
             TP.SKStrokeJoin := TSKStrokeJoin.Miter;
             {$ENDIF}
           end;
      'R': begin
             TP.BLStrokeOptions.Join := TBLStrokeJoin.Round;
             {$IFDEF USE_SKIA}
             TP.SKStrokeJoin := TSKStrokeJoin.Round;
             {$ENDIF}
           end;
      'B': begin
             TP.BLStrokeOptions.Join := TBLStrokeJoin.Bevel;
             {$IFDEF USE_SKIA}
             TP.SKStrokeJoin := TSKStrokeJoin.Bevel;
             {$ENDIF}
           end;
    end;
    Inc(C);

    TP.BLStrokeOptions.MiterLimit := P[0];
    TP.BLStrokeOptions.Width := P[1];
    Inc(P, 2);

    { Stroke & Fill style }
    Color.R := Trunc(P[0] * 255);
    Color.G := Trunc(P[1] * 255);
    Color.B := Trunc(P[2] * 255);
    Inc(P, 3);
    TP.StrokeColor := Color.Color;

    Color.R := Trunc(P[0] * 255);
    Color.G := Trunc(P[1] * 255);
    Color.B := Trunc(P[2] * 255);
    Inc(P, 3);
    TP.FillColor := Color.Color;

    { Path }
    var Count := Trunc(P^);
    Inc(P);
    for var I := 0 to Count - 1 do
    begin
      case C^ of
        'M': begin
               TP.BLPath.MoveTo(P[0], H - P[1]);
               {$IFDEF USE_SKIA}
               PathBuilder.MoveTo(P[0], H - P[1]);
               {$ENDIF}
               Inc(P, 2);
             end;
        'L': begin
               TP.BLPath.LineTo(P[0], H - P[1]);
               {$IFDEF USE_SKIA}
               PathBuilder.LineTo(P[0], H - P[1]);
               {$ENDIF}
               Inc(P, 2);
             end;
        'C': begin
               TP.BLPath.CubicTo(P[0], H - P[1], P[2], H - P[3], P[4], H - P[5]);
               {$IFDEF USE_SKIA}
               PathBuilder.CubicTo(P[0], H - P[1], P[2], H - P[3], P[4], H - P[5]);
               {$ENDIF}
               Inc(P, 6);
             end;
        'E': begin
               TP.BLPath.Close;
             end;
      end;
      Inc(C);
    end;

    TP.BLPath.Shrink;

    if (TP.Stroke) then
    begin
      TP.BLStrokedPath.AddStrokedPath(TP.BLPath, TP.BLStrokeOptions,
        TBLApproximationOptions.Default);
      TP.BLStrokedPath.Shrink;
    end;

    {$IFDEF USE_SKIA}
    TP.SKPath := PathBuilder.Detach;
    {$ENDIF}
  end;
end;

destructor TTiger.Destroy;
begin
  FPaths.Free;
  inherited;
end;

end.
