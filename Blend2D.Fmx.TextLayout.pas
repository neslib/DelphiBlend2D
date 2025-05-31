unit Blend2D.Fmx.TextLayout;

interface

uses
  System.Types,
  System.UITypes,
  System.Generics.Collections,
  FMX.Graphics,
  FMX.TextLayout,
  Blend2D;

type
  TBLTextLayout = class(TTextLayout)
  {$REGION 'Internal Declarations'}
  private class var
    FFontManager: TBLFontManager;
  private type
    TAttribute = record
    public
      Font: TFont;     // Reference
      Offset: Integer; // In paragraph, not in overall text
      Length: Integer;
      Color: TAlphaColor;
    public
      procedure Init(const AOffset, ALength: Integer; const AFont: TFont;
        const AColor: TAlphaColor);
    end;
  private type
    TRun = record
    private
      FGlyphBuffer: TBLGlyphBuffer; // Must stay alive because FRun references it
      FRun: TBLGlyphRun;
      FFont: TBLFont;
      FXPosition: Double;
      FYPosition: Double;
      FColor: TAlphaColor;
//      Leading: Double;
//      Width: Double;
//      Trailing: Double;
    private
      class function GetFontFace(const AFont: TFont): TBLFontFace; static;
    public
      procedure Init(var AXPosition: Double; const AText: PChar;
        const AAttr: TAttribute);
      procedure Draw(const AContext: TBLContext; const AX, AY: Double);
    end;
  private type
    TParagraph = record
    private
      FRuns: TArray<TRun>;

//      FText: PChar;
//      FLength: Integer;
//      FOffsetInText: Integer;
    public
      procedure Update(const ALayout: TBLTextLayout; const AText: PChar;
        const ALength, AOffsetInText: Integer);
      procedure Draw(const AContext: TBLContext; const AX, AY: Double);
    end;
  private
    FParagraphs: TArray<TParagraph>;
    FTempAttributes: TList<TAttribute>;
  private
    procedure DrawLayout(const AContext: TBLContext);
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
  public
    class constructor Create;

    class property FontManager: TBLFontManager read FFontManager;
  {$ENDREGION 'Internal Declarations'}
  end;

implementation

uses
  Blend2D.FontProvider,
  Blend2D.Fmx.Canvas;

type
  TBLCanvasOpener = class(TBLCanvas);

{ TBLTextLayout }

procedure TBLTextLayout.ConvertToPath(const APath: TPathData);
begin
  inherited;
  Assert(False, 'TODO');
end;

constructor TBLTextLayout.Create(const ACanvas: TCanvas);
begin
  inherited;
  FTempAttributes := TList<TAttribute>.Create;
end;

class constructor TBLTextLayout.Create;
begin
  FFontManager.Make;
end;

destructor TBLTextLayout.Destroy;
begin
  FTempAttributes.Free;
  inherited;
end;

procedure TBLTextLayout.DoDrawLayout(const ACanvas: TCanvas);
begin
  if (ACanvas is TBLCanvas) then
  begin
    if (ACanvas.BeginSceneCount > 0) then
      DrawLayout(TBLCanvasOpener(ACanvas).FContext);
  end
  else
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
  var Text := Self.Text;
  if (Text = '') then
    Exit;

  { Count number of paragraphs/lines in the text }
  var ParaCount := 1;
  var TextStart: PChar := Pointer(Text);
  var P := TextStart;
  while (P^ <> #0) do
  begin
    if (P^ = #10) then
      Inc(ParaCount);

    Inc(P);
  end;

  { Initialize and update each paragraph with the corresponding text segment }
  SetLength(FParagraphs, ParaCount);
  P := Pointer(Text);
  var ParaStart := P;
  for var I := 0 to ParaCount - 1 do
  begin
    while (P^ <> #0) and (P^ <> #10) do
      Inc(P);

    var ParaLen := P - ParaStart;
    if (P > TextStart) and (P[-1] = #13) then
      Dec(ParaLen);

    { NOTE: This assumes that Text stays alive, which it should since it is a
      field of TTextLayout. }
    FParagraphs[I].Update(Self, ParaStart, ParaLen, ParaStart - TextStart);
    ParaStart := P + 1;
  end;
end;

procedure TBLTextLayout.DrawLayout(const AContext: TBLContext);
begin
  if (FParagraphs = nil) then
    Exit;

  AContext.Save;
  try
    var X: Double := TopLeft.X;
    var Y: Double := TopLeft.Y;
    AContext.ClipToRect(X, Y, MaxSize.X, MaxSize.Y);
    for var I := 0 to Length(FParagraphs) - 1 do
      FParagraphs[I].Draw(AContext, X, Y);
  finally
    AContext.Restore;
  end;
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

{ TBLTextLayout.TAttribute }

procedure TBLTextLayout.TAttribute.Init(const AOffset, ALength: Integer;
  const AFont: TFont; const AColor: TAlphaColor);
begin
  Font := AFont;
  Offset := AOffset;
  Length := ALength;
  Color := AColor;
end;

{ TBLTextLayout.TRun }

procedure TBLTextLayout.TRun.Draw(const AContext: TBLContext; const AX,
  AY: Double);
begin
  var Origin: TBLPoint;
  Origin.X := AX + FXPosition;
  Origin.Y := AY + FYPosition;
  AContext.FillGlyphRun(Origin, FFont, FRun, FColor);
end;

class function TBLTextLayout.TRun.GetFontFace(const AFont: TFont): TBLFontFace;
const
  FONT_SLANT_TO_STYLE: array [TFontSlant] of TBLFontStyle = (
    TBLFontStyle.Normal, TBLFontStyle.Italic, TBLFontStyle.Oblique);
const
  FONT_WEIGHT_TO_WEIGHT: array [TFontWeight] of TBLFontWeight = (
    TBLFontWeight.Thin, TBLFontWeight.ExtraLight, TBLFontWeight.Light,
    TBLFontWeight.SemiLight, TBLFontWeight.Normal, TBLFontWeight.Medium,
    TBLFontWeight.SemiBold, TBLFontWeight.Bold, TBLFontWeight.ExtraBold,
    TBLFontWeight.Black, TBLFontWeight.ExtraBlack);
const
  FONT_STRETCH_TO_STRETCH: array [TFontStretch] of TBLFontStretch = (
    TBLFontStretch.UltraCondensed, TBLFontStretch.ExtraCondensed,
    TBLFontStretch.Condensed, TBLFontStretch.SemiCondensed,
    TBLFontStretch.Normal, TBLFontStretch.SemiExpanded, TBLFontStretch.Expanded,
    TBLFontStretch.ExtraExpanded, TBLFontStretch.UltraExpanded);

begin
  var Props: TBLFontQueryProperties;
  var Style := AFont.StyleExt;

  Props.Reset;
  Props.Style := FONT_SLANT_TO_STYLE[Style.Slant];
  Props.Weight := FONT_WEIGHT_TO_WEIGHT[Style.Weight];
  Props.Stretch := FONT_STRETCH_TO_STRETCH[Style.Stretch];
  Result := FFontManager.QueryFace(AFont.Family, Props);

  if (Result = nil) then
  begin
    var FaceIndex: Integer;
    var FontFaceData := TBLFontProvider.Instance.GetTypefaceData(
      AFont.Family, Props, FaceIndex);

    if (FontFaceData = nil) then
      { Should not happen. }
      Exit;

    var Data: TBLFontData;
    Data.MakeFromData(FontFaceData);

    Result.MakeFromData(Data, FaceIndex);

    { TBLFontManager doesn't seem to release the font faces, so the font data
      is never destroyed. Register as expected leaks for now until this gets
      fixed. }
    {$WARN SYMBOL_PLATFORM OFF}
    RegisterExpectedMemoryLeak(PByte(FontFaceData) - 4 - SizeOf(Pointer));
    {$WARN SYMBOL_PLATFORM ON}

    FFontManager.AddFace(Result);
  end;
end;

procedure TBLTextLayout.TRun.Init(var AXPosition: Double; const AText: PChar;
  const AAttr: TAttribute);
begin
  var Face := GetFontFace(AAttr.Font);
  FFont.MakeFromFace(Face, AAttr.Font.Size);
  var FontMetrics := FFont.Metrics;
  FYPosition := FontMetrics.Ascent;

  FGlyphBuffer.SetUtf16Text(AText + AAttr.Offset, AAttr.Length);
  FFont.Shape(FGlyphBuffer);

  FRun := FGlyphBuffer.GlyphRun;
  FXPosition := AXPosition;
  FColor := AAttr.Color;

  Assert(FRun.PlacementType = TBLGlyphPlacementType.AdvanceOffset);

  var Metrics: TBLTextMetrics;
  FFont.GetTextMetrics(FGlyphBuffer, Metrics);
  AXPosition := AXPosition + Metrics.Advance.X;
//  Leading := Metrics.LeadingBearing.X;
//  Width := Metrics.BoundingBox.X1 - Metrics.BoundingBox.X0;
//  Trailing := Metrics.TrailingBearing.X;
end;

{ TBLTextLayout.TParagraph }

procedure TBLTextLayout.TParagraph.Draw(const AContext: TBLContext; const AX,
  AY: Double);
begin
  for var I := 0 to Length(FRuns) - 1 do
    FRuns[I].Draw(AContext, AX, AY);
end;

procedure TBLTextLayout.TParagraph.Update(const ALayout: TBLTextLayout;
  const AText: PChar; const ALength, AOffsetInText: Integer);
begin
  { Find the attributes for this text segment }
  var TempAttribs := ALayout.FTempAttributes;
  TempAttribs.Clear;

  var Attr: TAttribute;
  if (ALayout.AttributesCount > 0) then
    Assert(False, 'TODO')
  else
  begin
    { No attributes. Create a default one. }
    Attr.Init(0, ALength, ALayout.Font, ALayout.Color);
    TempAttribs.Add(Attr);
  end;

  { Process each part of the paragraph that has the same attributes }
  var XPosition: Double;
  SetLength(FRuns, TempAttribs.Count);
  for var I := 0 to TempAttribs.Count - 1 do
    FRuns[I].Init(XPosition, AText, TempAttribs[I]);
//  begin
//    Attr := TempAttribs[I];
//    var Face := GetFontFace(Attr.Font);
//    var Font: TBLFont;
//    Font.MakeFromFace(Face, Attr.Font.Size);
//    GlyphBuffer.SetUtf16Text(AText + Attr.Offset, Attr.Length);
//    Font.Shape(GlyphBuffer);
//
//    FRuns[I].Init(XPosition, Font, Attr.Color, GlyphBuffer);

//    var Metrics: TBLTextMetrics;
//    Font.GetTextMetrics(GlyphBuffer, Metrics);
//    var FontMetrics := Font.DesignMetrics;
//
//    var Run := GlyphBuffer.GlyphRun;
//    Assert(Run.PlacementType =
//    var P := Run.PlacementDataAsGlyphPlacements;
//    for var J := 0 to Run.Count - 1 do
//      Inc(P);
//  end;
//  FText := AText;
//  FLength := ALength;
//  FOffsetInText := AOffsetInText;
end;

end.
