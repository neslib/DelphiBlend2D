unit Blend2D.TypefaceManager.Windows;

interface

uses
  Winapi.D2D1,
  System.SysUtils,
  Blend2D,
  Blend2D.TypefaceManager;

type
  TBLTypefaceManagerWindows = class(TBLTypefaceManager)
  {$REGION 'Internal Declarations'}
  private
    FD2DFactory: ID2D1Factory;
    FDWriteFactory: IDWriteFactory;
    FFontCollection: IDWriteFontCollection;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;

    function GetTypefaceData(const AFamilyName: String;
      const AProps: TBLFontQueryProperties;
      out AFaceIndex: Integer): TBytes; override;
  end;

implementation

uses
  Winapi.Windows,
  System.Win.ComObj;

{ TBLTypefaceManagerWindows }

constructor TBLTypefaceManagerWindows.Create;
begin
  inherited;
  OleCheck(D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, ID2D1Factory,
    nil, FD2DFactory));

  OleCheck(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory,
    IUnknown(FDWriteFactory)));

  OleCheck(FDWriteFactory.GetSystemFontCollection(FFontCollection));
end;

function TBLTypefaceManagerWindows.GetTypefaceData(const AFamilyName: String;
  const AProps: TBLFontQueryProperties; out AFaceIndex: Integer): TBytes;
const
  BUFFER_SIZE = 64 * 1024;
begin
  AFaceIndex := 0;
  Result := nil;

  var Exists: LongBool;
  var Index: Cardinal;
  if Failed(FFontCollection.FindFamilyName(PChar(AFamilyName), Index, Exists)) then
    Exists := False;

  if (not Exists) then
    Index := 0;

  var Family: IDWriteFontFamily;
  if Failed(FFontCollection.GetFontFamily(Index, Family)) then
    Exit;

  var Font: IDWriteFont;
  if Failed(Family.GetFirstMatchingFont(
    DWRITE_FONT_WEIGHT(AProps.Weight),
    DWRITE_FONT_STRETCH(AProps.Stretch),
    DWRITE_FONT_STYLE(AProps.Style), Font))
  then
    Exit;

  var FontFace: IDWriteFontFace;
  if Failed(Font.CreateFontFace(FontFace)) then
    Exit;

  AFaceIndex := FontFace.GetIndex;

  var FileCount: Cardinal := 1;
  var FontFile: IDWriteFontFile;
  if Failed(FontFace.GetFiles(FileCount, FontFile)) then
    Exit;

  if (FileCount = 0) then
    Exit;

  var Key: Pointer;
  var KeySize: Cardinal;
  if Failed(FontFile.GetReferenceKey(Key, KeySize)) then
    Exit;

  var FontFileLoader: IDWriteFontFileLoader;
  if Failed(FontFile.GetLoader(FontFileLoader)) then
    Exit;

  var FontFileStream: IDWriteFontFileStream;
  if Failed(FontFileLoader.CreateStreamFromKey(Key, KeySize, FontFileStream)) then
    Exit;

  var FileSize: UInt64;
  if Failed(FontFileStream.GetFileSize(FileSize)) then
    Exit;

  { Limit file to 50MB. Even the largest (CJK) font are usually below 20MB }
  if (FileSize = 0) or (FileSize > (50 * 1024 * 1024)) then
    Exit;

  SetLength(Result, FileSize);
  var FragmentOffset: UInt64 := 0;
  while (FileSize > 0) do
  begin
    var FragmentSize := FileSize;
    if (FragmentSize > BUFFER_SIZE) then
      FragmentSize := BUFFER_SIZE;

    var FragmentStart, FragmentContext: Pointer;
    if Failed(FontFileStream.ReadFileFragment(FragmentStart, FragmentOffset,
      FragmentSize, FragmentContext))
    then
      Exit(nil);

    Move(FragmentStart^, Result[FragmentOffset], FragmentSize);
    FontFileStream.ReleaseFileFragment(FragmentContext);

    Inc(FragmentOffset, FragmentSize);
    Dec(FileSize, FragmentSize);
  end;
end;

end.
