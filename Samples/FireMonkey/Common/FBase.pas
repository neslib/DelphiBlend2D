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
  Neslib.Skia,
  {$ENDIF}
  Blend2D.Api,
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
    FBlend2DImage: IBLImage;
    FBlend2DBitmapData: TBitmapData;
    FBlend2DContext: IBLContext;
    {$IFNDEF MSWINDOWS}
    FBlend2DConverter: IBLPixelConverter;
    {$ENDIF}
  protected
    {$IFDEF USE_SKIA}
    FSkiaSurface: ISKSurface;
    FSkiaCanvas: ISKCanvas;
    FSkiaFill: ISKPaint;
    FSkiaStroke: ISKPaint;
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
    procedure RenderBlend2D(const AContext: IBLContext); virtual; abstract;
    {$IFDEF USE_SKIA}
    procedure RenderSkia(const ACanvas: ISKCanvas); virtual; abstract;
    {$ENDIF}
  public
    { Public declarations }
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
  var
    Item: TListBoxItem;
  begin
    Item := TListBoxItem.Create(Self);
    Item.Text := AText;
    Item.Tag := ATag;
    ComboBoxRenderer.AddObject(Item);
  end;

begin
  ReportMemoryLeaksOnShutdown := True;
  ComboBoxRenderer.BeginUpdate;
  try
    AddItem('FireMonkey', TAG_FIREMONKEY);
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
  FBlend2DImage := TBLImage.Create;
  FBlend2DContext := TBLContext.Create;

  {$IFNDEF MSWINDOWS}
  { We need to convert ARGB to ABGR on non-Windows platforms. }
  FBlend2DConverter := TBLPixelConverter.CreatePlatformConverter;
  {$ENDIF}

  {$IFDEF USE_SKIA}
  FSkiaFill := TSKPaint.Create;
  FSkiaFill.Style := TSKPaintStyle.Fill;
  FSkiaFill.IsAntialias := True;

  FSkiaStroke := TSKPaint.Create;
  FSkiaStroke.Style := TSKPaintStyle.Stroke;
  FSkiaStroke.IsAntialias := True;
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
var
  SrcSize, DstSize: TSize;
begin
  if (FBitmap = nil) or (ComboBoxRenderer.Selected = nil) then
    Exit;

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
var
  Data: TBitmapData;
  CreateInfo: TBLContextCreateInfo;
begin
  if (FBitmap.Map(TMapAccess.Write, Data)) then
  try
    if (Data.Data <> FBlend2DBitmapData.Data) or (Data.Pitch <> FBlend2DBitmapData.Pitch) then
    begin
      FBlend2DImage.InitializeFromData(Data.Width, Data.Height,
        TBLFormat.PRGB32, Data.Data, Data.Pitch);
      FBlend2DBitmapData.Data := Data.Data;
      FBlend2DBitmapData.Pitch := Data.Pitch;
    end;

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
var
  Canvas: TCanvas;
begin
  Canvas := FBitmap.Canvas;
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
var
  Data: TBitmapData;
  Info: TSKImageInfo;
begin
  if (FBitmap.Map(TMapAccess.Write, Data)) then
  try
    if (Data.Data <> FSkiaBitmapData.Data) or (Data.Pitch <> FSkiaBitmapData.Pitch) then
    begin
      FSkiaCanvas := nil;
      FSkiaSurface := nil;

      Info := TSKImageInfo.Create(Data.Width, Data.Height,
        TSKImageInfo.PlatformColorType, TSKAlphaType.Premul);
      FSkiaSurface := TSKSurface.Create(Info, Data.Data, Data.Pitch);
      FSkiaCanvas := FSkiaSurface.Canvas;

      FSkiaBitmapData.Data := Data.Data;
      FSkiaBitmapData.Pitch := Data.Pitch;
    end;

    FSkiaCanvas.Scale(FPixelScale);
    RenderSkia(FSkiaCanvas);
    FSkiaCanvas.ResetMatrix;
    FSkiaCanvas.Flush;
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
