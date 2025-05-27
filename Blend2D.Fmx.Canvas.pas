unit Blend2D.Fmx.Canvas;

interface

uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  System.Types,
  System.UITypes,
  System.Classes,
  System.SysUtils,
  System.Messaging,
  System.Math.Vectors,
  FMX.Types,
  FMX.Graphics,
  FMX.TextLayout,
  FMX.Platform,
  Blend2D;

type
  EBLCanvas = class(Exception);

type
  TBLBitmapHandle = class
  {$REGION 'Internal Declarations'}
  private
    FImage: TBLImage;
    FPixels: Pointer;
    FWidth: Integer;
    FHeight: Integer;
    FPixelFormat: TPixelFormat;
  private
    function Map(var ABitmapData: TBitmapData): Boolean;
    procedure NeedPixels;
    procedure NeedImage;
    procedure Draw(const AContext: TBLContext; const ASrcRect, ADestRect: TRectF);
  public
    constructor Create(const AWidth, AHeight: Integer; const APixelFormat: TPixelFormat);
    destructor Destroy; override;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  TBLCanvas = class(TCanvas)
  {$REGION 'Internal Declarations'}
  private type
    TSaveState = class(TCanvasSaveState)
    protected
      procedure AssignTo(ADest: TPersistent); override;
    public
      procedure Assign(ASource: TPersistent); override;
    end;
//  private class var
//    FFontManager: TBLFontManager;
  private
    FContext: TBLContext;
    FImage: TBLImage;
    FContextHandle: THandle;
    {$IFDEF MSWINDOWS}
    FBitmap: HBITMAP;
    {$ENDIF}
  private
    procedure Resized;
    procedure BeginContext(const AContextHandle: THandle);
    procedure EndContext;
    procedure GetImageFromWindow(const AContextHandle: THandle);
    procedure SetStyle(const ABrush: TBrush; const ARect: TRectF;
      const AForFill: Boolean);
    procedure SetGradientStyle(const AGradient: TGradient; const ARect: TRectF;
      const AForFill: Boolean);
    procedure SetPatternStyle(const ABrushBitmap: TBrushBitmap;
      const ARect: TRectF; const AForFill: Boolean);
    procedure DrawNonBLBitmap(const ABitmap: TBitmap; const ASrcRect,
      ADestRect: TRectF);
    procedure MakePatternFromNonBLBitmap(const ABitmap: TBitmap;
      const APattern: TBLPattern; const AExtendMode: TBLExtendMode);
    procedure ReleaseBitmap;
  private
    class procedure ConvertPath(const ASrc: TPathData; const ADst: TBLPath); static;
  protected
    { TCanvas }
    constructor CreateFromPrinter(const APrinter: TAbstractPrinter); override;
    {$IFDEF MSWINDOWS}
    constructor CreateFromWindow(const AParent: TWindowHandle; const AWidth,
      AHeight: Integer; const AQuality: TCanvasQuality = TCanvasQuality.SystemDefault); override;
    {$ENDIF}
    function CreateSaveState: TCanvasSaveState; override;
    function DoBeginScene(AClipRects: PClipRects = nil; AContextHandle: THandle = 0): Boolean; override; final;
    procedure DoClear(const AColor: TAlphaColor); override;
    procedure DoClearRect(const ARect: TRectF; const AColor: TAlphaColor = 0); override;
    procedure DoDrawBitmap(const ABitmap: TBitmap; const ASrcRect, ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean); override;
    procedure DoDrawEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawLine(const APoint1, APoint2: TPointF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawPath(const APath: TPathData; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoDrawRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TStrokeBrush); override;
    procedure DoEndScene; override; final;
    procedure DoFillEllipse(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillPath(const APath: TPathData; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoFillRect(const ARect: TRectF; const AOpacity: Single; const ABrush: TBrush); override;
    procedure DoSetMatrix(const AMatrix: TMatrix); override;
    function GetCanvasScale: Single; override;

    class procedure DoFinalizeBitmap(var ABitmapHandle: THandle); override;
    class function DoInitializeBitmap(const AWidth, AHeight: Integer; const AScale: Single; var APixelFormat: TPixelFormat): THandle; override;
    class function DoMapBitmap(const ABitmapHandle: THandle; const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean; override;
    class procedure DoUnmapBitmap(const ABitmapHandle: THandle; var ABitmapData: TBitmapData); override;
  public
    destructor Destroy; override;

    { TCanvas }
    procedure ExcludeClipRect(const ARect: TRectF); override;
    procedure IntersectClipRect(const ARect: TRectF); override;
    function LoadFontFromStream(const AStream: TStream): Boolean; override;
    function PtInPath(const APoint: TPointF; const APath: TPathData): Boolean; override;
    procedure SetSize(const AWidth, AHeight: Integer); override; final;

    class function GetCanvasStyle: TCanvasStyles; override;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  TBLTextLayout = class(TTextLayout)
  {$REGION 'Internal Declarations'}
  protected
    { TTextLayout }
    procedure DoDrawLayout(const ACanvas: TCanvas); override;
    function DoPositionAtPoint(const APoint: TPointF): Integer; override;
    function DoRegionForRange(const ARange: TTextRange): TRegion; override;
    procedure DoRenderLayout; override;
    function GetTextHeight: Single; override;
    function GetTextRect: TRectF; override;
    function GetTextWidth: Single; override;
  public
    { TTextLayout }
    constructor Create(const ACanvas: TCanvas = nil); override;
    destructor Destroy; override;
    procedure ConvertToPath(const APath: TPathData); override;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  TBLCanvasService = class(TInterfacedObject, IFMXCanvasService)
  {$REGION 'Internal Declarations'}
  private
    FOriginal: IFMXCanvasService;
  private
    {$IFDEF DEBUG}
    FUseBlend2D: Boolean;
    procedure HandleFormBeforeShown(const ASender: TObject;
      const AMessage: TMessage);
    {$ENDIF}
  protected
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  FMX.Platform.Win,
  {$ENDIF}
  FMX.Forms,
  Blend2D.Fmx;

var
  GCanvasService: IFMXCanvasService = nil;
//  GCanvasClass: TCanvasClass = nil;

{ TBLBitmapHandle }

constructor TBLBitmapHandle.Create(const AWidth, AHeight: Integer;
  const APixelFormat: TPixelFormat);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
end;

destructor TBLBitmapHandle.Destroy;
begin
  FreeMem(FPixels);
  inherited;
end;

procedure TBLBitmapHandle.Draw(const AContext: TBLContext; const ASrcRect,
  ADestRect: TRectF);
begin
  NeedImage;

  var SrcRect: TBLRectI;
  var DstRect: TBLRect;
  SrcRect.Reset(ASrcRect.Round);
  DstRect.Reset(ADestRect);

  AContext.BlitImage(DstRect, FImage, SrcRect);
end;

function TBLBitmapHandle.Map(var ABitmapData: TBitmapData): Boolean;
begin
  NeedPixels;
  ABitmapData.Data := FPixels;
  ABitmapData.Pitch := FWidth * PixelFormatBytes[FPixelFormat];
  Result := True;
end;

procedure TBLBitmapHandle.NeedImage;
begin
  if (FImage = nil) then
  begin
    NeedPixels;
    FImage.MakeFromData(FWidth, FHeight, PIXEL_FORMAT_TO_BL_FORMAT[FPixelFormat],
      FPixels, FWidth * PixelFormatBytes[FPixelFormat], TBLDataAccessFlags.Read);
  end;
end;

procedure TBLBitmapHandle.NeedPixels;
begin
  if (FPixels = nil) then
    GetMem(FPixels, FWidth * FHeight * PixelFormatBytes[FPixelFormat]);
end;

{ TBLCanvas }

procedure TBLCanvas.BeginContext(const AContextHandle: THandle);
begin
  if (Parent <> nil) then
  begin
    if (FImage = nil) then
    begin
      GetImageFromWindow(AContextHandle);
      if (FImage <> nil) then
      begin
        FContext := TBLContext.Create(FImage);
        FContext.FillRule := TBLFillRule.EvenOdd;
      end;
    end;
  end
  else if (Bitmap <> nil) then
  begin
    Assert(Bitmap.Canvas.InheritsFrom(TBLCanvas));
    var BitmapHandle := TBLBitmapHandle(Bitmap.Handle);
    BitmapHandle.NeedImage;
    if (BitmapHandle.FImage <> nil) then
    begin
      FContext := TBLContext.Create(BitmapHandle.FImage);
      FContext.FillRule := TBLFillRule.EvenOdd;
    end;
  end;
end;

class procedure TBLCanvas.ConvertPath(const ASrc: TPathData;
  const ADst: TBLPath);
begin
  var PointCount := ASrc.Count;
  var I := 0;
  while (I < PointCount) do
  begin
    var Point := ASrc[I];
    case Point.Kind of
      TPathPointKind.MoveTo:
        ADst.MoveTo(Point.Point.X, Point.Point.Y);

      TPathPointKind.LineTo:
        ADst.LineTo(Point.Point.X, Point.Point.Y);

      TPathPointKind.CurveTo:
        begin
          Inc(I);
          var CP2 := ASrc[I].Point;
          Inc(I);
          var Stop := ASrc[I].Point;
          ADst.CubicTo(Point.Point.X, Point.Point.Y, CP2.X, CP2.Y, Stop.X, Stop.Y);
        end;

      TPathPointKind.Close:
        ADst.Close;
    end;
    Inc(I);
  end;
end;

constructor TBLCanvas.CreateFromPrinter(const APrinter: TAbstractPrinter);
begin
  inherited;
  { Not used }
end;

{$IFDEF MSWINDOWS}
constructor TBLCanvas.CreateFromWindow(const AParent: TWindowHandle;
  const AWidth, AHeight: Integer; const AQuality: TCanvasQuality);
begin
  inherited;
  var ParentHandle := WindowHandleToPlatform(Parent);
  if (ParentHandle.Transparency) then
    ParentHandle.CreateBuffer(ParentHandle.WndClientSize.Width, ParentHandle.WndClientSize.Height);
end;
{$ENDIF}

function TBLCanvas.CreateSaveState: TCanvasSaveState;
begin
  Result := TSaveState.Create;
end;

destructor TBLCanvas.Destroy;
begin
  if (Parent <> nil) then
    ReleaseBitmap;
  inherited;
end;

function TBLCanvas.DoBeginScene(AClipRects: PClipRects;
  AContextHandle: THandle): Boolean;
begin
  Result := inherited;
  if (Result) then
  begin
    BeginContext(AContextHandle);
    Result := (FContext <> nil);
    if (Result) then
    begin
      FContextHandle := AContextHandle;
      FContext.Save;

      var M := Matrix;
      if (Scale <> 1) then
        M := M * TMatrix.CreateScaling(Scale, Scale);

      var BM: TBLMatrix2D;
      BM.Reset(M);
      FContext.SetTransform(BM);

      if (AClipRects <> nil) then
      begin
        var Rects := AClipRects^;
        var Count := Length(Rects);
        if (Count > 0) then
        begin
          { NOTE: Multiple cliprects not supported by Blend2D }
          var R := Rects[0];
          FContext.ClipToRect(R.Left, R.Top, R.Width, R.Height);
        end;
      end;
    end;
  end;
end;

procedure TBLCanvas.DoClear(const AColor: TAlphaColor);
begin
  var OrigCompOp := FContext.CompOp;
  FContext.CompOp := TBLCompOp.SrcCopy;
  FContext.FillAll(AColor);
  FContext.CompOp := OrigCompOp;
end;

procedure TBLCanvas.DoClearRect(const ARect: TRectF; const AColor: TAlphaColor);
begin
  var OrigCompOp := FContext.CompOp;
  FContext.CompOp := TBLCompOp.SrcCopy;
  FContext.FillRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height, AColor);
  FContext.CompOp := OrigCompOp;
end;

procedure TBLCanvas.DoDrawBitmap(const ABitmap: TBitmap; const ASrcRect,
  ADestRect: TRectF; const AOpacity: Single; const AHighSpeed: Boolean);
begin
  FContext.GlobalAlpha := AOpacity;

  var SrcRect := ASrcRect * TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height);
  if (ABitmap.HandleAllocated) and (not ASrcRect.IsEmpty) and (not ADestRect.IsEmpty) then
  begin
    if (ABitmap.CanvasClass.InheritsFrom(TBLCanvas)) then
    begin
      Assert(TObject(ABitmap.Handle) is TBLBitmapHandle);
      TBLBitmapHandle(ABitmap.Handle).Draw(FContext, SrcRect, ADestRect);
    end
    else
      DrawNonBLBitmap(ABitmap, ASrcRect, ADestRect);
  end;
end;

procedure TBLCanvas.DoDrawEllipse(const ARect: TRectF; const AOpacity: Single;
  const ABrush: TStrokeBrush);
begin
  var W: Double := ARect.Width;
  var H: Double := ARect.Height;
  if (W <= 0) or (H <= 0) then
    Exit;

  SetStyle(ABrush, ARect, False);
  FContext.GlobalAlpha := AOpacity;

  var RX: Single := ARect.Width * 0.5;
  var RY: Single := ARect.Height * 0.5;
  FContext.StrokeEllipse(ARect.Left + RX, ARect.Top + RY, RX, RY);
end;

procedure TBLCanvas.DoDrawLine(const APoint1, APoint2: TPointF;
  const AOpacity: Single; const ABrush: TStrokeBrush);
begin
  FContext.GlobalAlpha := AOpacity;
  var Rect := TRectF.Create(APoint1, APoint2, True);
  SetStyle(ABrush, Rect, False);
  FContext.StrokeLine(APoint1.X, APoint1.Y, APoint2.X, APoint2.Y);
end;

procedure TBLCanvas.DoDrawPath(const APath: TPathData; const AOpacity: Single;
  const ABrush: TStrokeBrush);
begin
  var Bounds := APath.GetBounds;
  if (Bounds.Width <= 0) or (Bounds.Height <= 0) or (APath.Count = 0) then
    Exit;

  SetStyle(ABrush, Bounds, False);
  FContext.GlobalAlpha := AOpacity;

  var Path: TBLPath;
  ConvertPath(APath, Path);
  FContext.StrokePath(Path);
end;

procedure TBLCanvas.DoDrawRect(const ARect: TRectF; const AOpacity: Single;
  const ABrush: TStrokeBrush);
begin
  var W: Double := ARect.Width;
  var H: Double := ARect.Height;
  if (W <= 0) or (H <= 0) then
    Exit;

  SetStyle(ABrush, ARect, False);
  FContext.GlobalAlpha := AOpacity;
  FContext.StrokeRect(ARect.Left, ARect.Top, W, H);
end;

procedure TBLCanvas.DoEndScene;
begin
  FContext.Restore;
  EndContext;
  inherited;
end;

procedure TBLCanvas.DoFillEllipse(const ARect: TRectF; const AOpacity: Single;
  const ABrush: TBrush);
begin
  var W: Double := ARect.Width;
  var H: Double := ARect.Height;
  if (W <= 0) or (H <= 0) then
    Exit;

  SetStyle(ABrush, ARect, True);
  FContext.GlobalAlpha := AOpacity;

  var RX: Single := ARect.Width * 0.5;
  var RY: Single := ARect.Height * 0.5;
  FContext.FillEllipse(ARect.Left + RX, ARect.Top + RY, RX, RY);
end;

procedure TBLCanvas.DoFillPath(const APath: TPathData; const AOpacity: Single;
  const ABrush: TBrush);
begin
  var Bounds := APath.GetBounds;
  if (Bounds.Width <= 0) or (Bounds.Height <= 0) or (APath.Count = 0) then
    Exit;

  SetStyle(ABrush, Bounds, True);
  FContext.GlobalAlpha := AOpacity;

  var Path: TBLPath;
  ConvertPath(APath, Path);
  FContext.FillPath(Path);
end;

procedure TBLCanvas.DoFillRect(const ARect: TRectF; const AOpacity: Single;
  const ABrush: TBrush);
begin
  var W: Double := ARect.Width;
  var H: Double := ARect.Height;
  if (W <= 0) or (H <= 0) then
    Exit;

  SetStyle(ABrush, ARect, True);
  FContext.GlobalAlpha := AOpacity;
  FContext.FillRect(ARect.Left, ARect.Top, W, H);
end;

class procedure TBLCanvas.DoFinalizeBitmap(var ABitmapHandle: THandle);
begin
  TBLBitmapHandle(ABitmapHandle).Free;
end;

class function TBLCanvas.DoInitializeBitmap(const AWidth, AHeight: Integer;
  const AScale: Single; var APixelFormat: TPixelFormat): THandle;
begin
  if (APixelFormat = TPixelFormat.None)
    or (PIXEL_FORMAT_TO_BL_FORMAT[APixelFormat] = TBLFormat.None)
  then
    APixelFormat := TPixelFormat.BGRA;

  var Bitmap := TBLBitmapHandle.Create(AWidth, AHeight, APixelFormat);
  Result := THandle(Bitmap);
end;

class function TBLCanvas.DoMapBitmap(const ABitmapHandle: THandle;
  const AAccess: TMapAccess; var ABitmapData: TBitmapData): Boolean;
begin
  Result := TBLBitmapHandle(ABitmapHandle).Map(ABitmapData);
end;

procedure TBLCanvas.DoSetMatrix(const AMatrix: TMatrix);
begin
  if (BeginSceneCount > 0) then
  begin
    var M := AMatrix;
    if (Scale <> 1) then
      M := M * TMatrix.CreateScaling(Scale, Scale);

    var BM: TBLMatrix2D;
    BM.Reset(M);
    FContext.SetTransform(BM);
  end;
end;

class procedure TBLCanvas.DoUnmapBitmap(const ABitmapHandle: THandle;
  var ABitmapData: TBitmapData);
begin
  { Not needed }
end;

procedure TBLCanvas.DrawNonBLBitmap(const ABitmap: TBitmap; const ASrcRect,
  ADestRect: TRectF);
begin
  var Data: TBitmapData;
  if (ABitmap.Map(TMapAccess.Read, Data)) then
  try
    var Image: TBLImage;
    Image.MakeFromData(Data.Width, Data.Height,
      PIXEL_FORMAT_TO_BL_FORMAT[Data.PixelFormat], Data.Data, Data.Pitch,
      TBLDataAccessFlags.Read);

    var SrcRect: TBLRectI;
    var DstRect: TBLRect;
    SrcRect.Reset(ASrcRect.Round);
    DstRect.Reset(ADestRect);

    FContext.BlitImage(DstRect, Image, SrcRect);
  finally
    ABitmap.Unmap(Data);
  end;
end;

procedure TBLCanvas.EndContext;
begin
  if (Parent <> nil) then
  begin
    FContext.Finish;

    {$IFDEF MSWINDOWS}
    var ParentHandle := WindowHandleToPlatform(Parent);
    if (ParentHandle.Transparency) then
      Assert(False, 'TODO');
    {$ENDIF}

    if (FContextHandle <> 0) then
    begin
      {$IF Defined(MSWINDOWS)}
      var DC := CreateCompatibleDC(0);
      if (DC <> 0) then
      try
        var OldObj := SelectObject(DC, FBitmap);
        try
          BitBlt(HDC(FContextHandle), 0, 0, Round(Width * Scale),
            Round(Height * Scale), DC, 0, 0, SRCCOPY);
        finally
          if (OldObj <> 0) then
            SelectObject(DC, OldObj);
        end;
      finally
        DeleteDC(DC);
      end;
      {$ELSE}
      Assert(False, 'TODO');
      {$ENDIF}
    end;
  end
  else if (Bitmap <> nil) then
  begin
    FContext.Finish;
  end;
end;

procedure TBLCanvas.ExcludeClipRect(const ARect: TRectF);
begin
  { TODO : Blend2D currently does not support complex clip regions }
end;

function TBLCanvas.GetCanvasScale: Single;
begin
  Result := inherited;
end;

class function TBLCanvas.GetCanvasStyle: TCanvasStyles;
begin
  Result := [];
end;

procedure TBLCanvas.GetImageFromWindow(const AContextHandle: THandle);
begin
  Assert(FImage = nil);
  {$IF Defined(MSWINDOWS)}
  if (FBitmap = 0) then
  begin
    var W := Round(Width * Scale);
    var H := Round(Height * Scale);
    var BitmapInfo: TBitmapInfo;
    var Bits: Pointer;

    FillChar(BitmapInfo, SizeOf(TBitmapInfo), 0);
    BitmapInfo.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    BitmapInfo.bmiHeader.biWidth := W;
    BitmapInfo.bmiHeader.biHeight := -H;
    BitmapInfo.bmiHeader.biPlanes := 1;
    BitmapInfo.bmiHeader.biBitCount := 32;
    BitmapInfo.bmiHeader.biCompression := BI_RGB;
    BitmapInfo.bmiHeader.biSizeImage := W * H * 4;
    FBitmap := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, Bits, 0, 0);
    if (FBitmap = 0) then
      Exit;
  end;

  var DibSection: TDIBSection;
  if GetObject(FBitmap, SizeOf(TDIBSection), @DibSection) = 0 then
    Exit;

  FImage.MakeFromData(DibSection.dsBm.bmWidth, DibSection.dsBm.bmHeight,
    TBLFormat.Prgb32, DibSection.dsBm.bmBits, DibSection.dsBm.bmWidthBytes);
  {$ELSE}
  Assert(False, 'TODO');
  {$ENDIF}
end;

procedure TBLCanvas.IntersectClipRect(const ARect: TRectF);
begin
  { TODO : Blend2D currently does not support complex clip regions }
end;

function TBLCanvas.LoadFontFromStream(const AStream: TStream): Boolean;
begin
{  try
    if (not FFontManager.IsValid) then
      FFontManager.Make;

    var Size := AStream.Size - AStream.Position;
    var Bytes: TBytes;
    SetLength(Bytes, Size);
    AStream.ReadBuffer(Bytes, Size);

    var Data: TBLFontData;
    Data.MakeFromData(Bytes);

    var Face: TBLFontFace;
    Face.MakeFromData(Data, 0);

    FFontManager.AddFace(Face);
    Result := True;
  except
    Result := False;
  end;}
  Result := True;
end;

procedure TBLCanvas.MakePatternFromNonBLBitmap(const ABitmap: TBitmap;
  const APattern: TBLPattern; const AExtendMode: TBLExtendMode);
begin
  var Data: TBitmapData;
  if (ABitmap.Map(TMapAccess.Read, Data)) then
  try
    var Image: TBLImage;
    Image.MakeFromData(Data.Width, Data.Height,
      PIXEL_FORMAT_TO_BL_FORMAT[Data.PixelFormat], Data.Data, Data.Pitch,
      TBLDataAccessFlags.Read);

    APattern.Make(Image, AExtendMode);
  finally
    ABitmap.Unmap(Data);
  end;
end;

function TBLCanvas.PtInPath(const APoint: TPointF;
  const APath: TPathData): Boolean;
begin
  var Bounds := APath.GetBounds;
  if (Bounds.Width <= 0) or (Bounds.Height <= 0) or (APath.Count = 0) then
    Exit(False);

  if (not Bounds.Contains(APoint)) then
    Exit(False);

  var Path: TBLPath;
  ConvertPath(APath, Path);
  Result := (Path.HitTest(BLPoint(APoint), TBLFillRule.EvenOdd) = TBLHitTest.FullyIn);
end;

procedure TBLCanvas.ReleaseBitmap;
begin
  FImage.Reset;
  {$IF DEFINED(MSWINDOWS)}
  if (FBitmap <> 0) then
  begin
    DeleteObject(FBitmap);
    FBitmap := 0;
  end;
  {$ELSE}
  Assert(False, 'TODO');
  {$ENDIF}
end;

procedure TBLCanvas.Resized;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if (Parent <> nil) then
  begin
    var ParentHandle := WindowHandleToPlatform(Parent);
    if (ParentHandle.Transparency) then
      ParentHandle.ResizeBuffer(ParentHandle.WndClientSize.Width, ParentHandle.WndClientSize.Height);
  end;
  {$ENDIF}

  if (Parent <> nil) then
    ReleaseBitmap;
end;

procedure TBLCanvas.SetGradientStyle(const AGradient: TGradient;
  const ARect: TRectF; const AForFill: Boolean);
begin
  var Points := AGradient.Points;
  if (Points.Count < 2) then
    Exit;

  var W: Single := ARect.Width;
  var H: Single := ARect.Height;

  var G: TBLGradient;

  if (AGradient.Style = TGradientStyle.Radial) then
  begin
    var P := AGradient.RadialTransform.RotationCenter.Point;
    G.Make(BLRadialGradientValues(P.X, P.Y, P.X, P.Y, 0.5));

    for var I := 0 to Points.Count - 1 do
    begin
      var Point := Points[I];
      G.AddStop(1 - Point.Offset, Point.Color);
    end;

    G.Scale(W, H);
  end
  else
  begin
    G.Make(BLLinearGradientValues(
      ARect.Left + (AGradient.StartPosition.X * W),
      ARect.Top + (AGradient.StartPosition.Y * H),
      ARect.Left + (AGradient.StopPosition.X * W),
      ARect.Top + (AGradient.StopPosition.Y * H)));

    for var I := 0 to Points.Count - 1 do
    begin
      var Point := Points[I];
      G.AddStop(Point.Offset, Point.Color);
    end;
  end;

  if (AForFill) then
    FContext.SetFillStyle(G)
  else
    FContext.SetStrokeStyle(G);
end;

procedure TBLCanvas.SetPatternStyle(const ABrushBitmap: TBrushBitmap;
  const ARect: TRectF; const AForFill: Boolean);
const
  EXTEND_MODES: array [TWrapMode] of TBLExtendMode = (
    TBLExtendMode.Repeating, TBLExtendMode.Pad, TBLExtendMode.Repeating);
begin
  var Pattern: TBLPattern;
  var Bitmap := ABrushBitmap.Bitmap;
  if (Bitmap.CanvasClass.InheritsFrom(TBLCanvas)) then
  begin
    Assert(TObject(Bitmap.Handle) is TBLBitmapHandle);
    var BitmapHandle := TBLBitmapHandle(Bitmap.Handle);
    BitmapHandle.NeedImage;
    Pattern.Make(BitmapHandle.FImage, EXTEND_MODES[ABrushBitmap.WrapMode]);
  end
  else
    MakePatternFromNonBLBitmap(Bitmap, Pattern, EXTEND_MODES[ABrushBitmap.WrapMode]);

  if (ABrushBitmap.WrapMode = TWrapMode.TileStretch) then
  begin
    if (Stroke.Kind = TBrushKind.None) then
      Pattern.Scale(ARect.Width / Bitmap.Width, ARect.Height / Bitmap.Height)
    else
    begin
      Pattern.Translate(ARect.Left, ARect.Top);
      Pattern.Scale((ARect.Width + (0.5 * Stroke.Thickness)) / Bitmap.Width,
                    (ARect.Height + (0.5 * Stroke.Thickness)) / Bitmap.Height);
    end;
  end;

  if (AForFill) then
    FContext.SetFillStyle(Pattern)
  else
    FContext.SetStrokeStyle(Pattern);
end;

procedure TBLCanvas.SetSize(const AWidth, AHeight: Integer);
begin
  if (Width <> AWidth) or (Height <> AHeight) then
  begin
    inherited;
    Resized;
  end;
end;

procedure TBLCanvas.SetStyle(const ABrush: TBrush; const ARect: TRectF;
  const AForFill: Boolean);
const
  JOINS: array [TStrokeJoin] of TBLStrokeJoin = (
    TBLStrokeJoin.MiterClip, TBLStrokeJoin.Round, TBLStrokeJoin.Bevel);
  CAPS: array [TStrokeCap] of TBLStrokeCap = (TBLStrokeCap.Square,
    TBLStrokeCap.Round);
begin
  var Brush := ABrush;
  while (Brush <> nil) and (Brush.Kind = TBrushKind.Resource) do
    Brush := Brush.Resource.Brush;
  if (Brush = nil) then
    Exit;

  case Brush.Kind of
    TBrushKind.Solid:
      if (AForFill) then
        FContext.SetFillStyle(Brush.Color)
      else
        FContext.SetStrokeStyle(Brush.Color);

    TBrushKind.Gradient:
      SetGradientStyle(Brush.Gradient, ARect, AForFill);

    TBrushKind.Bitmap:
      SetPatternStyle(Brush.Bitmap, ARect, AForFill);
  else
    Exit;
  end;

  if (not AForFill) then
  begin
    Assert(ABrush is TStrokeBrush);
    var Stroke := TStrokeBrush(ABrush);
    if (Stroke.Kind = TBrushKind.Resource) then
    begin
      Brush := Stroke;
      while (Brush <> nil) and (Brush.Kind = TBrushKind.Resource) do
        Brush := Brush.Resource.Brush;

      if (Brush is TStrokeBrush) then
        Stroke := TStrokeBrush(Brush)
      else
        Exit;
    end;

    if (Stroke = nil) then
      Exit;

    FContext.StrokeWidth := Stroke.Thickness;
    FContext.StrokeJoin := JOINS[Stroke.Join];
    FContext.SetStrokeCaps(CAPS[Stroke.Cap]);

    { Blend2D does not support dashed lines yet
      (https://github.com/blend2d/blend2d/issues/48)
    var Dash: TBLArray<Double>;
    if (Stroke.Dash <> TStrokeDash.Solid) then
    begin
      FContext.StrokeDashOffset := Stroke.DashOffset;

      var Dashes := Stroke.DashArray;
      Dash.Reserve(Length(Dashes));
      for var I := 0 to Length(Dashes) - 1 do
        Dash.Append(Dashes[I]);
      Dash.Append(0.1);
      Dash.Append(0.5);
    end;
    FContext.StrokeDashArray := Dash;}
  end;
end;

{ TBLCanvas.TSaveState }

procedure TBLCanvas.TSaveState.Assign(ASource: TPersistent);
begin
  inherited;
  if (ASource is TBLCanvas) then
    TBLCanvas(ASource).FContext.Save;
end;

procedure TBLCanvas.TSaveState.AssignTo(ADest: TPersistent);
begin
  if (ADest is TBLCanvas) then
    TBLCanvas(ADest).FContext.Restore;
  inherited;
end;

{ TBLTextLayout }

procedure TBLTextLayout.ConvertToPath(const APath: TPathData);
begin
  inherited;
  Assert(False, 'TODO');
end;

constructor TBLTextLayout.Create(const ACanvas: TCanvas);
begin
  inherited;
end;

destructor TBLTextLayout.Destroy;
begin
  inherited;
end;

procedure TBLTextLayout.DoDrawLayout(const ACanvas: TCanvas);
begin
  inherited;
  Assert(False, 'TODO');
end;

function TBLTextLayout.DoPositionAtPoint(const APoint: TPointF): Integer;
begin
  Assert(False, 'TODO');
  Result := 0;
end;

function TBLTextLayout.DoRegionForRange(const ARange: TTextRange): TRegion;
begin
  Assert(False, 'TODO');
end;

procedure TBLTextLayout.DoRenderLayout;
begin
  if (Text = '') then
    Exit;

  { TODO : Implement }
end;

function TBLTextLayout.GetTextHeight: Single;
begin
  Assert(False, 'TODO');
  Result := 0;
end;

function TBLTextLayout.GetTextRect: TRectF;
begin
  Assert(False, 'TODO');
end;

function TBLTextLayout.GetTextWidth: Single;
begin
  Assert(False, 'TODO');
  Result := 0;
end;

{ TBLCanvasService }

constructor TBLCanvasService.Create;
begin
  inherited;
  FOriginal := IFMXCanvasService(TPlatformServices.Current.GetPlatformService(IFMXCanvasService));
  {$IFDEF DEBUG}
  TMessageManager.DefaultManager.SubscribeToMessage(TFormBeforeShownMessage, HandleFormBeforeShown);
  {$ENDIF}
end;

destructor TBLCanvasService.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, HandleFormBeforeShown);
  inherited;
end;

{$IFDEF DEBUG}
procedure TBLCanvasService.HandleFormBeforeShown(const ASender: TObject;
  const AMessage: TMessage);
const
  MESSAGE_TEXT = '''
    Your declaration of GlobalUseBlend2D has no effect because the canvas
    service has already been started. In this case, just create a unit in the
    project like "Project.Startup.pas", place the GlobalUseBlend2D declaration
    in the initialization of this new unit, and declare this new unit before any
    other unit of yours in the .dpr, that is, right after FMX.Forms.
    ''';
begin
  TMessageManager.DefaultManager.Unsubscribe(TFormBeforeShownMessage, HandleFormBeforeShown);
  if (FUseBlend2D <> GlobalUseBlend2D) then
    raise EBLCanvas.Create(MESSAGE_TEXT);
end;
{$ENDIF}

procedure TBLCanvasService.RegisterCanvasClasses;
begin
  if (FOriginal <> nil) then
  begin
    if (GlobalUseBlend2D) then
    begin
      var CanvasClass: TCanvasClass := TBLCanvas;
  //    AtomicExchange(PPointer(@GCanvasClass)^, PPointer(@CanvasClass)^);
      TCanvasManager.EnableSoftwareCanvas(True);
      TCanvasManager.RegisterCanvas(CanvasClass, True, False);
      TTextLayoutManager.RegisterTextLayout(TBLTextLayout, CanvasClass);
    end;
    FOriginal.RegisterCanvasClasses;
  end;
  {$IFDEF DEBUG}
  FUseBlend2D := GlobalUseBlend2D;
  {$ENDIF}
end;

procedure TBLCanvasService.UnregisterCanvasClasses;
begin
  if (FOriginal <> nil) then
    FOriginal.UnregisterCanvasClasses;
end;

initialization
  GCanvasService := TBLCanvasService.Create;
  TPlatformServices.Current.RemovePlatformService(IFMXCanvasService);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, GCanvasService);

end.
