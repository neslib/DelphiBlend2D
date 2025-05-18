unit Blend2D.Fmx.PaintBox;

interface

uses
  System.Types,
  System.Classes,
  System.UITypes,
  FMX.Controls,
  FMX.Graphics,
  Blend2D;

type
  /// <summary>
  ///  Event handler type for TBlend2DPaintBox.OnPaint.
  /// </summary>
  TBlend2DPaintEvent = procedure(const ASender: TObject; const AContext: TBLContext) of object;

type
  /// <summary>
  ///  A paint box that uses Blend2D for drawing.
  ///  To be able to install a package with this control, you *must* copy
  ///  "blend2d_win32.dll" to your package output directory (usually
  ///  c:\Users\Public\Documents\Embarcadero\Studio\<version>\Bpl\)
  /// </summary>
  TBlend2DPaintBox = class(TControl)
  {$REGION 'Internal Declarations'}
  private
    FBitmap: TBitmap;
    FBitmapData: TBitmapData;
    FImage: TBLImage;
    FContext: TBLContext;
    {$IFNDEF MSWINDOWS}
    FConverter: TBLPixelConverter;
    {$ENDIF}
    FPixelScale: Single;
    FDstSize: TSize;
    FSrcRect: TRectF;
    FDstRect: TRectF;
    FLowResolution: Boolean;
    FForceResize: Boolean;
    FOnPaint: TBlend2DPaintEvent;
    procedure SetLowResolution(const AValue: Boolean);
  protected
    procedure Paint; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;

    /// <summary>
    ///  Whether to use a low-resolution buffer on high-DPI (eg. Retina) displays.
    ///
    ///  When set to True, the drawing canvas will be in logical units instead
    ///  of physical units. This will increase performance but result in lower
    ///  quality (less sharp) output.
    ///
    ///  When set to False (the default), the drawing canvas will match the units
    ///  of the display, resulting a sharp high-quality output at the cost of a
    ///  decrease in performance.
    ///
    ///  On non-high-DPI displays, this property has no effect.
    /// </summary>
    property LowResolution: Boolean read FLowResolution write SetLowResolution default False;

    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property Visible default True;
    property Width;

    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint: TBlend2DPaintEvent read FOnPaint write FOnPaint;
    property OnResize;
    property OnResized;
  end;

procedure Register;

implementation

uses
  FMX.Types;

procedure Register;
begin
  RegisterComponents('Shapes', [TBlend2DPaintBox]);
end;

{ TBlend2DPaintBox }

constructor TBlend2DPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create;

  {$IFNDEF MSWINDOWS}
  FConverter := TBLPixelConverter.CreatePlatformConverter;
  {$ENDIF}
end;

destructor TBlend2DPaintBox.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TBlend2DPaintBox.Paint;
begin
  if (csDesigning in ComponentState) and (not FLocked) and (not FInPaintTo) then
  begin
    var R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
    Exit;
  end;

  if (FBitmap = nil) then
    Exit;

  var SrcSize, DstSize: TSize;
  DstSize.Width := Trunc(Width);
  DstSize.Height := Trunc(Height);
  if (DstSize <> FDstSize) or (FForceResize) then
  begin
    FDstSize := DstSize;

    if (FLowResolution) or (Scene = nil) then
      FPixelScale := 1
    else
      FPixelScale := Scene.GetSceneScale;

    SrcSize.Width := Trunc(FDstSize.Width * FPixelScale);
    SrcSize.Height := Trunc(FDstSize.Height * FPixelScale);
    FBitmap.SetSize(SrcSize);

    FSrcRect := RectF(0, 0, SrcSize.Width, SrcSize.Height);
    FDstRect := RectF(0, 0, FDstSize.Width, FDstSize.Height);
    FForceResize := False;
  end;

  var Data: TBitmapData;
  if (FBitmap.Map(TMapAccess.Write, Data)) then
  try
    if (Data.Data <> FBitmapData.Data) or (Data.Pitch <> FBitmapData.Pitch) then
    begin
      FImage.MakeFromData(Data.Width, Data.Height, TBLFormat.PRGB32,
        Data.Data, Data.Pitch);
      FBitmapData.Data := Data.Data;
      FBitmapData.Pitch := Data.Pitch;
    end;

    if Assigned(FOnPaint) then
    begin
      FContext.Start(FImage);
      try
        if (FPixelScale <> 1) then
          FContext.Scale(FPixelScale);

        FOnPaint(Self, FContext);
      finally
        FContext.Finish;
      end;

      {$IFNDEF MSWINDOWS}
      FConverter.ConvertRect(Data.Data, Data.Pitch, Data.Data, Data.Pitch,
        Data.Width, Data.Height);
      {$ENDIF}
    end;
  finally
    FBitmap.Unmap(Data);
  end;

  Canvas.DrawBitmap(FBitmap, FSrcRect, FDstRect, 1, True);
end;

procedure TBlend2DPaintBox.SetLowResolution(const AValue: Boolean);
begin
  if (AValue <> FLowResolution) then
  begin
    FLowResolution := AValue;
    FForceResize := True;
    Repaint;
  end;
end;

end.
