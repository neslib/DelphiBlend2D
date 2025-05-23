unit Blend2D.Vcl.PaintBox;

interface

uses
  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Blend2D;

type
  /// <summary>
  ///  Event handler type for TBlend2DPaintBox.OnPaint.
  /// </summary>
  TBlend2DPaintEvent = procedure(const ASender: TObject; const AContext: TBLContext) of object;

type
  /// <summary>
  ///  A paint box that uses Blend2D for drawing.
  /// </summary>
  /// <remarks>
  ///  To be able to install a package with this control, you *must* copy
  ///  "blend2d_win32.dll" to your package output directory (usually
  ///  c:\Users\Public\Documents\Embarcadero\Studio\<version>\Bpl\)
  /// </remarks>
  TBlend2DPaintBox = class(TGraphicControl)
  {$REGION 'Internal Declarations'}
  private
    FBitmap: TBitmap;
    FImage: TBLImage;
    FContext: TBLContext;
    FDstSize: TSize;
    FPixelScale: Single;
    FLowResolution: Boolean;
    FForceResize: Boolean;
    FOnPaint: TBlend2DPaintEvent;
    procedure SetLowResolution(const AValue: Boolean);
  protected
    procedure Paint; override;
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;

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

    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TBlend2DPaintEvent read FOnPaint write FOnPaint;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('System', [TBlend2DPaintBox]);
end;

{ TBlend2DPaintBox }

procedure TBlend2DPaintBox.ChangeScale(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  if (isDpiChange) then
  begin
    FPixelScale := M / GetDesignDpi;
    FForceResize := True;
    Repaint;
  end;
end;

constructor TBlend2DPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
  Width := 105;
  Height := 105;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := TPixelFormat.pf32bit;
  FPixelScale := 1;
end;

destructor TBlend2DPaintBox.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TBlend2DPaintBox.Paint;
begin
  if (csDesigning in ComponentState) then
  begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
    Exit;
  end;

  if (FBitmap = nil) then
    Exit;

  { DstSize is in physical coordinates }
  var SrcSize, DstSize: TSize;
  DstSize.Width := Trunc(Width);
  DstSize.Height := Trunc(Height);
  if (DstSize <> FDstSize) or (FForceResize) then
  begin
    FDstSize := DstSize;

    if (FLowResolution) then
    begin
      SrcSize.Width := Trunc(FDstSize.Width / FPixelScale);
      SrcSize.Height := Trunc(FDstSize.Height / FPixelScale);
    end
    else
      SrcSize := DstSize;

    FBitmap.SetSize(SrcSize.Width, SrcSize.Height);

    if (SrcSize.Height > 1) then
      FImage.MakeFromData(SrcSize.Width, SrcSize.Height, TBLFormat.PRGB32,
        FBitmap.ScanLine[0], NativeInt(FBitmap.ScanLine[1]) - NativeInt(FBitmap.ScanLine[0]));
  end;

  if Assigned(FOnPaint) then
  begin
    FContext.Start(FImage);
    try
      if (not FLowResolution) and (FPixelScale <> 1) then
        FContext.Scale(FPixelScale);

      FOnPaint(Self, FContext);
    finally
      FContext.Finish;
    end;
  end;

  if (FBitmap.Width = DstSize.Width) and (FBitmap.Height = DstSize.Height) then
    Canvas.Draw(0, 0, FBitmap)
  else
    Canvas.StretchDraw(Rect(0, 0, DstSize.Width, DstSize.Height), FBitmap);
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
