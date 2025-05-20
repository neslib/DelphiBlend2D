unit FBase;

{$INCLUDE 'blFmxConfig.inc'}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Objects,
  {$IFDEF USE_SKIA}
  System.Skia,
  {$ENDIF}
  Blend2D;

type
  TFormBase = class(TForm)
    ToolBarMain: TToolBar;
    LabelRenderer: TLabel;
    ComboBoxRenderer: TComboBox;
    LayoutRenderer: TLayout;
    CheckBoxLimitFPS: TCheckBox;
    PaintBox: TPaintBox;
    LabelFPS: TLabel;
    TimerRepaint: TTimer;
    StyleBook: TStyleBook;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
    procedure CheckBoxLimitFPSChange(Sender: TObject);
    procedure TimerRepaintTimer(Sender: TObject);
  private
    { Private declarations }
    FBitmap: TBitmap;
    FDstSize: TSize;
    FSrcRect: TRectF;
    FDstRect: TRectF;
    FPixelScale: Single;
    FFrameCount: Integer;
    FFPS: Integer;
    FTicksPerFrame: Int64;
    FTicks: Int64;
    FNextFrame: Int64;
    FNextSecond: Int64;
  protected
    FBlend2DImage: TBLImage;
    FBlend2DBitmapData: TBitmapData;
    FBlend2DContext: TBLContext;
    {$IFNDEF MSWINDOWS}
    FBlend2DConverter: TBLPixelConverter;
    {$ENDIF}
  protected
    {$IFDEF USE_SKIA}
    FSkiaSurface: ISkSurface;
    FSkiaCanvas: ISkCanvas;
    FSkiaFill: ISkPaint;
    FSkiaStroke: ISkPaint;
    FSkiaBitmapData: TBitmapData;
    {$ENDIF}
  private
    procedure RenderFireMonkeyInternal;
    procedure RenderBlend2DInternal(const AThreadCount: Integer);
    {$IFDEF USE_SKIA}
    procedure RenderSkiaInternal;
    {$ENDIF}
    procedure CheckRepaint;
    procedure UpdateStats;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
  protected
    procedure BeforeRender; virtual;
    procedure AfterRender; virtual;
    procedure RenderNotSupported;
    procedure RenderFireMonkey(const ACanvas: TCanvas); virtual; abstract;
    procedure RenderBlend2D(const AContext: TBLContext); virtual; abstract;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISkCanvas); virtual; abstract;
    {$ENDIF}
  public
    { Public declarations }
    class function BackgroundForCompOp(const ACompOp: TBLCompOp): TAlphaColor; static;
  end;

implementation

{$R *.fmx}

uses
  System.Math.Vectors;

const
  TAG_BLEND2D_DEF = 0;
  TAG_BLEND2D_1T  = 1;
  TAG_BLEND2D_2T  = 2;
  TAG_BLEND2D_4T  = 4;
  TAG_BLEND2D_8T  = 8;
  TAG_BLEND2D_12T = 12;
  TAG_BLEND2D_16T = 16;

  TAG_FIREMONKEY  = -1;
  TAG_SKIA        = -2;

procedure TFormBase.AfterRender;
begin
  { No default implementation }
end;

procedure TFormBase.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  CheckRepaint;
end;

class function TFormBase.BackgroundForCompOp(
  const ACompOp: TBLCompOp): TAlphaColor;
const
  COLORS: array [TBLCompOp] of TAlphaColor = (
    $FF000000, // SrcOver
    $FF000000, // SrcCopy
    $FFFFFFFF, // SrcIn
    $00000000, // SrcOut
    $FFFFFFFF, // SrcAtop
    $FFFFFFFF, // DstOver
    $FF000000, // DstCopy
    $FF000000, // DstIn
    $FF000000, // DstOut
    $FF000000, // DstAtop
    $FF000000, // ExclusiveOr
    $FF000000, // Clear
    $FF000000, // Plus
    $FF000000, // Minus
    $FF000000, // Modulate
    $FFFFFFFF, // Multiply
    $FF000000, // Screen
    $00000000, // Overlay
    $FFFFFFFF, // Darken
    $FF000000, // Lighten
    $00000000, // ColorDodge
    $00000000, // ColorBurn
    $FF000000, // LinearBurn
    $FF000000, // LinearLight
    $FF000000, // PinLight
    $FF000000, // HardLight
    $00000000, // SoftLight
    $FF000000, // Difference
    $FFFFFFFF);// Exclusion
begin
  Result := COLORS[ACompOp];
end;

procedure TFormBase.BeforeRender;
begin
  { No default implementation }
end;

procedure TFormBase.CheckBoxLimitFPSChange(Sender: TObject);
begin
  if (CheckBoxLimitFPS.IsChecked) then
    FNextFrame := TStopwatch.GetTimeStamp + FTicksPerFrame;
end;

procedure TFormBase.CheckRepaint;
begin
  FTicks := TStopwatch.GetTimeStamp;
  if (CheckBoxLimitFPS.IsChecked) then
  begin
    if (FTicks < FNextFrame) then
      Exit;

    Inc(FNextFrame, FTicksPerFrame);
  end;

  PaintBox.Repaint;
end;

procedure TFormBase.FormCreate(Sender: TObject);

  procedure AddItem(const AText: String; const ATag: Integer);
  begin
    var Item := TListBoxItem.Create(Self);
    Item.Text := AText;
    Item.Tag := ATag;
    ComboBoxRenderer.AddObject(Item);
  end;

begin
  ReportMemoryLeaksOnShutdown := True;
  ComboBoxRenderer.BeginUpdate;
  try
    var CanvasName := Canvas.ClassName;
    if (CanvasName.StartsWith('TCanvas')) then
      CanvasName := CanvasName.Substring(7);
    AddItem('FMX (' + CanvasName + ')', TAG_FIREMONKEY);

    AddItem('Blend2D', TAG_BLEND2D_DEF);
    AddItem('Blend2D 1T', TAG_BLEND2D_1T);
    AddItem('Blend2D 2T', TAG_BLEND2D_2T);
    AddItem('Blend2D 4T', TAG_BLEND2D_4T);
    AddItem('Blend2D 8T', TAG_BLEND2D_8T);
    AddItem('Blend2D 12T', TAG_BLEND2D_12T);
    AddItem('Blend2D 16T', TAG_BLEND2D_16T);
    {$IFDEF USE_SKIA}
    AddItem('Skia', TAG_SKIA);
    {$ENDIF}
    ComboBoxRenderer.ItemIndex := 1;
  finally
    ComboBoxRenderer.EndUpdate;
  end;

  FPixelScale := Handle.Scale;

  FBitmap := TBitmap.Create;

  {$IFNDEF MSWINDOWS}
  { We need to convert ARGB to ABGR on non-Windows platforms. }
  FBlend2DConverter.MakePlatformConverter(TBLFormat.Prgb32);
  {$ENDIF}

  {$IFDEF USE_SKIA}
  FSkiaFill := TSkPaint.Create;
  FSkiaFill.Style := TSkPaintStyle.Fill;
  FSkiaFill.AntiAlias := True;

  FSkiaStroke := TSkPaint.Create;
  FSkiaStroke.Style := TSkPaintStyle.Stroke;
  FSkiaStroke.Antialias := True;
  {$ENDIF}

  Application.OnIdle := ApplicationIdle;

  TStopwatch.StartNew; { Initialize high-res timer }
  FTicksPerFrame := TStopwatch.Frequency div 60; { 60 fps when limited }
  FNextFrame := TStopwatch.GetTimeStamp + FTicksPerFrame;
  FNextSecond := TStopwatch.GetTimeStamp + TStopwatch.Frequency;
end;

procedure TFormBase.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

procedure TFormBase.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
begin
  if (FBitmap = nil) or (ComboBoxRenderer.Selected = nil) then
    Exit;

  var SrcSize, DstSize: TSize;
  DstSize.Width := Trunc(PaintBox.Width);
  DstSize.Height := Trunc(PaintBox.Height);
  if (DstSize <> FDstSize) then
  begin
    FDstSize := DstSize;
    FPixelScale := Handle.Scale;
    SrcSize.Width := Trunc(FDstSize.Width * FPixelScale);
    SrcSize.Height := Trunc(FDstSize.Height * FPixelScale);
    FBitmap.SetSize(SrcSize);

    FSrcRect := RectF(0, 0, SrcSize.Width, SrcSize.Height);
    FDstRect := RectF(0, 0, FDstSize.Width, FDstSize.Height);
  end;

  BeforeRender;
  case ComboBoxRenderer.Selected.Tag of
    TAG_FIREMONKEY:
      RenderFireMonkeyInternal;

    {$IFDEF USE_SKIA}
    TAG_SKIA:
      RenderSkiaInternal;
    {$ENDIF}
  else
    RenderBlend2DInternal(ComboBoxRenderer.Selected.Tag);
  end;
  AfterRender;

  Canvas.DrawBitmap(FBitmap, FSrcRect, FDstRect, 1, True);

  Inc(FFrameCount);
  if (FTicks >= FNextSecond) then
  begin
    Inc(FNextSecond, TStopwatch.Frequency);
    FFPS := FFrameCount;
    FFrameCount := 0;
    UpdateStats;
  end;
end;

procedure TFormBase.RenderBlend2DInternal(const AThreadCount: Integer);
begin
  var Data: TBitmapData;
  if (FBitmap.Map(TMapAccess.Write, Data)) then
  try
    if (Data.Data <> FBlend2DBitmapData.Data) or (Data.Pitch <> FBlend2DBitmapData.Pitch) then
    begin
      FBlend2DImage.MakeFromData(Data.Width, Data.Height,
        TBLFormat.PRGB32, Data.Data, Data.Pitch);
      FBlend2DBitmapData.Data := Data.Data;
      FBlend2DBitmapData.Pitch := Data.Pitch;
    end;

    var CreateInfo: TBLContextCreateInfo;
    CreateInfo.Reset;
    CreateInfo.ThreadCount := AThreadCount;

    FBlend2DContext.Start(FBlend2DImage, CreateInfo);
    try
      FBlend2DContext.Scale(FPixelScale);
      RenderBlend2D(FBlend2DContext);
    finally
      FBlend2DContext.Finish;
    end;

    {$IFNDEF MSWINDOWS}
    FBlend2DConverter.ConvertRect(Data.Data, Data.Pitch, Data.Data, Data.Pitch,
      Data.Width, Data.Height);
    {$ENDIF}
  finally
    FBitmap.Unmap(Data);
  end;
end;

procedure TFormBase.RenderFireMonkeyInternal;
begin
  var Canvas := FBitmap.Canvas;
  Canvas.BeginScene;
  try
    Canvas.SetMatrix(TMatrix.CreateScaling(FPixelScale, FPixelScale));
    RenderFireMonkey(Canvas);
  finally
    Canvas.EndScene;
  end;
end;

procedure TFormBase.RenderNotSupported;
begin
  if (ComboBoxRenderer.Selected <> nil) then
  begin
    case ComboBoxRenderer.Selected.Tag of
      TAG_FIREMONKEY:
        begin
          FBitmap.Canvas.Font.Size := 16;
          FBitmap.Canvas.Fill.Kind := TBrushKind.Solid;
          FBitmap.Canvas.Fill.Color := TAlphaColors.White;
          FBitmap.Canvas.FillText(RectF(0, 0, PaintBox.Width, PaintBox.Height),
            'Not supported with FireMonkey', True, 1, [], TTextAlign.Center);
        end;
    else
      Assert(False);
    end;
  end;
end;

procedure TFormBase.TimerRepaintTimer(Sender: TObject);
begin
  CheckRepaint;
end;

{$IFDEF USE_SKIA}
procedure TFormBase.RenderSkiaInternal;
begin
  var Data: TBitmapData;
  if (FBitmap.Map(TMapAccess.Write, Data)) then
  try
    if (Data.Data <> FSkiaBitmapData.Data) or (Data.Pitch <> FSkiaBitmapData.Pitch) then
    begin
      FSkiaCanvas := nil;
      FSkiaSurface := nil;

      var Info := TSkImageInfo.Create(Data.Width, Data.Height,
        SkNative32ColorType, TSkAlphaType.Premul);
      FSkiaSurface := TSkSurface.MakeRasterDirect(Info, Data.Data, Data.Pitch);
      FSkiaCanvas := FSkiaSurface.Canvas;

      FSkiaBitmapData.Data := Data.Data;
      FSkiaBitmapData.Pitch := Data.Pitch;
    end;

    FSkiaCanvas.Scale(FPixelScale, FPixelScale);
    RenderSkia(FSkiaCanvas);
    FSkiaCanvas.ResetMatrix;
  finally
    FBitmap.Unmap(Data);
  end;
end;
{$ENDIF}

procedure TFormBase.UpdateStats;
begin
  LabelFPS.Text := Format('%d fps', [FFPS]);
  Invalidate;
end;

end.
