unit Blend2D;
{ OPP wrappers for Blend2D C API.

  Follows the C++ API where possible. The following C++ classes are not
  converted because Delphi provides built-in alternatives for these:
  * TBLBitSet: will be deprecated according to https://blend2d.com/doc/group__bl__containers.html


  * BLArray<T>: uses TArray<T> instead
  * BLFile: uses TFileStream instead
  * BLString: uses String instead
  * BLVariant: not needed
  * BLStyle: style properties are used in another way. We may introduce a
    TBLStyle type once Delphi has support for managed records (or use Scope
    solution like TBLStrokeOptions). }

{$MINENUMSIZE 4}
{$SCOPEDENUMS ON}
{$EXCESSPRECISION OFF}

interface

uses
  System.SysUtils;

{$REGION 'Common Types'}
type
  {$IF Defined(MACOS)}
  Size_T = LongWord;
  {$ELSE}
  Size_T = NativeUInt;
  {$ENDIF}
  PSize_T = ^Size_T;
{$ENDREGION 'Common Types'}

{$REGION 'Error Handling'}

{ ============================================================================
   [Error Handling - Enums]
  ============================================================================ }

type
  /// <summary>
  ///  Blend2D result code.
  /// </summary>
  TBLResult = (
    /// <summary>Successful result code.</summary>
    Success = 0,

    /// <summary>Out of memory [ENOMEM].</summary>
    OutOfMemory = $00010000,

    /// <summary>Invalid value/argument [EINVAL].</summary>
    InvalidValue,

    /// <summary>Invalid state [EFAULT].</summary>
    InvalidState,

    /// <summary>Invalid handle or file [EBADF].</summary>
    InvalidHandle,

    /// <summary>Invalid conversion.</summary>
    InvalidConversion,

    /// <summary>Value too large [EOVERFLOW].</summary>
    Overflow,

    /// <summary>Object not initialized.</summary>
    NotInitialized,

    /// <summary>Not implemented [ENOSYS].</summary>
    NotImplemented,

    /// <summary>Operation not permitted [EPERM].</summary>
    NotPermitted,

    /// <summary>IO error [EIO].</summary>
    IOError,

    /// <summary>Device or resource busy [EBUSY].</summary>
    Busy,

    /// <summary>Operation interrupted [EINTR].</summary>
    Interrupted,

    /// <summary>Try again [EAGAIN].</summary>
    TryAgain,

    /// <summary>Timed out [ETIMEDOUT].</summary>
    TimedOut,

    /// <summary>Broken pipe [EPIPE].</summary>
    BrokenPipe,

    /// <summary>File is not seekable [ESPIPE].</summary>
    InvalidSeek,

    /// <summary>Too many levels of symlinks [ELOOP].</summary>
    SymlinkLoop,

    /// <summary>File is too large [EFBIG].</summary>
    FileTooLarge,

    /// <summary>File/directory already exists [EEXIST].</summary>
    AlreadyExists,

    /// <summary>Access denied [EACCES].</summary>
    AccessDenied,

    /// <summary>Media changed [Windows::ERROR_MEDIA_CHANGED].</summary>
    MediaChanged,

    /// <summary>The file/FS is read-only [EROFS].</summary>
    ReadOnlyFS,

    /// <summary>Device doesn't exist [ENXIO].</summary>
    NoDevice,

    /// <summary>Not found, no entry (fs) [ENOENT].</summary>
    NoEntry,

    /// <summary>No media in drive/device [ENOMEDIUM].</summary>
    NoMedia,

    /// <summary>No more data / end of file [ENODATA].</summary>
    NoMoreData,

    /// <summary>No more files [ENMFILE].</summary>
    NoMoreFiles,

    /// <summary>No space left on device [ENOSPC].</summary>
    NoSpaceLeft,

    /// <summary>Directory is not empty [ENOTEMPTY].</summary>
    NotEmpty,

    /// <summary>Not a file [EISDIR].</summary>
    NotFile,

    /// <summary>Not a directory [ENOTDIR].</summary>
    NotDirectory,

    /// <summary>Not same device [EXDEV].</summary>
    NotSameDevice,

    /// <summary>Not a block device [ENOTBLK].</summary>
    NotBlockDevice,

    /// <summary>File/path name is invalid.</summary>
    InvalidFilename,

    /// <summary>File/path name is too long [ENAMETOOLONG].</summary>
    FilenameTooLong,

    /// <summary>Too many open files [EMFILE].</summary>
    TooManyOpenFiles,

    /// <summary>Too many open files by OS [ENFILE].</summary>
    TooManyOpenFilesByOS,

    /// <summary>Too many symbolic links on FS [EMLINK].</summary>
    TooManyLinks,

    /// <summary>Too many threads [EAGAIN].</summary>
    TooManyThreads,

    /// <summary>Thread pool is exhausted and couldn't acquire the requested thread count.</summary>
    ThreadPoolExhausted,

    /// <summary>File is empty (not specific to any OS error).</summary>
    FileEmpty,

    /// <summary>File open failed [Windows::ERROR_OPEN_FAILED].</summary>
    OpenFailed,

    /// <summary>Not a root device/directory [Windows::ERROR_DIR_NOT_ROOT].</summary>
    NotRootDevice,

    /// <summary>Unknown system error that failed to translate to Blend2D result code.</summary>
    UnknownSystemError,

    /// <summary>Invalid data alignment.</summary>
    InvalidArgument,

    /// <summary>Invalid data signature or header.</summary>
    InvalidSignature,

    /// <summary>Invalid or corrupted data.</summary>
    InvalidData,

    /// <summary>Invalid string (invalid data of either UTF8, UTF16, or UTF32).</summary>
    InvalidString,

    /// <summary>Invalid key or property.</summary>
    InvalidKey,

    /// <summary>Truncated data (more data required than memory/stream provides).</summary>
    DataTruncated,

    /// <summary>Input data too large to be processed.</summary>
    DataTooLarge,

    /// <summary>Decompression failed due to invalid data (RLE, Huffman, etc).</summary>
    DecompressionFailed,

    /// <summary>Invalid geometry (invalid path data or shape).</summary>
    InvalidGeometry,

    /// <summary>Returned when there is no matching vertex in path data.</summary>
    NoMatchingVertex,

    /// <summary>Invalid create flags (TBLContext).</summary>
    InvalidCreateFlags,

    /// <summary>No matching cookie (TBLContext).</summary>
    NoMatchingCookie,

    /// <summary>No states to restore (TBLContext).</summary>
    NoStatesToRestore,

    /// <summary>Cannot save state as the number of saved states reached the limit (TBLContext).</summary>
    TooManySavedStates,

    /// <summary>The size of the image is too large.</summary>
    ImageTooBig,

    /// <summary>Image codec for a required format doesn't exist.</summary>
    ImageNoMatchingCodec,

    /// <summary>Unknown or invalid file format that cannot be read.</summary>
    ImageUnknownFileFormat,

    /// <summary>Image codec doesn't support reading the file format.</summary>
    ImageDecoderNotProvided,

    /// <summary>Image codec doesn't support writing the file format.</summary>
    ImageEncoderNotProvided,

    /// <summary>Multiple IHDR chunks are not allowed (PNG).</summary>
    PngMultipleIHDR,

    /// <summary>Invalid IDAT chunk (PNG).</summary>
    PngInvalidIDAT,

    /// <summary>Invalid IEND chunk (PNG).</summary>
    PngInvalidIEND,

    /// <summary>Invalid PLTE chunk (PNG).</summary>
    PngInvalidPLTE,

    /// <summary>Invalid tRNS chunk (PNG).</summary>
    PngInvalidTRNS,

    /// <summary>Invalid filter type (PNG).</summary>
    PngInvalidFilter,

    /// <summary>Unsupported feature (JPEG).</summary>
    JpegUnsupportedFeature,

    /// <summary>Invalid SOS marker or header (JPEG).</summary>
    JpegInvalidSOS,

    /// <summary>Invalid SOF marker (JPEG).</summary>
    JpegInvalidSOF,

    /// <summary>Multiple SOF markers (JPEG).</summary>
    JpegMultipleSOF,

    /// <summary>Unsupported SOF marker (JPEG).</summary>
    JpegUnsupportedSOF,

    /// <summary>Font doesn't have any data as it's not initialized.</summary>
    FontNotInitialized,

    /// <summary>Font or font-face was not matched (TBLFontManager).</summary>
    FontNoMatch,

    /// <summary>Font has no character to glyph mapping data.</summary>
    FontNoCharacterMapping,

    /// <summary>Font has missing an important table.</summary>
    FontMissingImportantTable,

    /// <summary>Font feature is not available.</summary>
    FontFeatureNotAvailable,

    /// <summary>Font has an invalid CFF data.</summary>
    FontCFFInvalidData,

    /// <summary>Font program terminated because the execution reached the limit.</summary>
    FontProgramTerminated,

    /// <summary>Glyph substitution requires too much space and was terminated.</summary>
    GlyphSubstitutionTooLarge,

    /// <summary>Invalid glyph identifier.</summary>
    InvalidGlyph);

type
  /// <summary>
  ///  Adds functionality to `TBLResult`.
  /// </summary>
  /// <seealso cref="TBLResult"/>
  _TBLResultHelper = record helper for TBLResult
  public
    /// <summary>
    ///  Converts the result code to a string.
    /// </summary>
    function ToString: String;
  end;

{ ============================================================================
   [Error Handling - Handlers]
  ============================================================================ }

type
  /// <summary>
  ///  Type of exception that is raised for Blend2D errors.
  ///  Exceptions are enabled by default, but can be disabled using
  ///  `BLSetErrorHandler`.
  /// </summary>
  /// <seealso cref="BLSetErrorHandler"/>
  EBlend2DError = class(Exception)
  {$REGION 'Internal Declarations'}
  private
    FResult: TBLResult;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates an exception object.
    /// </summary>
    constructor Create(const AResult: TBLResult);

    /// <summary>
    ///  The Blend2D result code.
    /// </summary>
    property ResultCode: TBLResult read FResult;
  end;

type
  /// <summary>
  ///  Type of procedure that is called when a Blend2D error occurs.
  /// </summary>
  /// <param name="AResult">The Blend2D result code.</param>
  /// <param name="AUserData">Any user data passed to `BLSetErrorHandler`.</param>
  /// <seealso cref="BLSetErrorHandler"/>
  TBLErrorHandler = procedure(const AResultCode: TBLResult; const AUserData: Pointer);

/// <summary>
///  Sets a Blend2D error handler.
/// </summary>
/// <param name="AHandler">The error handler that is called when a Blend2D error occurs.</param>
/// <param name="AUserData">Any data you want to pass to the handler.</param>
/// <remarks>
///  The default error handler raises an exception of type `EBlend2DError`.
///  You can disable error handling completely by setting AHandler to nil. In
///  that case, there is no way to know if and when an error occured.
///
///  The following procedures can be used to set some default error handlers:
///   * `BLSetExceptionErrorHandler`: sets the error handler to a procedure that
///     raises an exception when a Blend2D error occurs. This is the default
///     behavior.
///   * `BLSetGetLastErrorHandler`: sets the error handler to a procedure that
///     sets a global error variable when a Blend2D error occurs. You can then
///     use `BLGetLastError` to retrieve this error code.
/// </remarks>
/// <seealso cref="EBlend2DError"/>
/// <seealso cref="BLSetExceptionErrorHandler"/>
/// <seealso cref="BLSetGetLastErrorHandler"/>
/// <seealso cref="BLGetLastError"/>
procedure BLSetErrorHandler(const AHandler: TBLErrorHandler;
  const AUserData: Pointer);

/// <summary>
///  Sets the error handler to a procedure that raises an exception when a
///  Blend2D error occurs. This is the default behavior.
/// </summary>
procedure BLSetExceptionErrorHandler;

/// <summary>
///  Sets the error handler to a procedure that sets a global error variable
///  when a Blend2D error occurs. You can then use `BLGetLastError` to retrieve
///  this error code.
/// </summary>
/// <seealso cref="BLGetLastError"/>
procedure BLSetGetLastErrorHandler;

/// <summary>
///  Retrieves the last Blend2D error, or `TBLResult.Success` if there was none.
/// </summary>
/// <remarks>
///  After this call, the last error is reset to `TBLResult.Success`.
///
///  This function should only be used when `BLSetGetLastErrorHandler` has been
///  called. Otherwise, it always returns `TBLResult.Success`.
///
///  This function is not thread-safe. If Blend2D is used from multiple threads,
///  the returned value can be from any thread.
/// </remarks>
/// <seealso cref="TBLResult"/>
/// <seealso cref="BLSetGetLastErrorHandler"/>
function BLGetLastError: TBLResult;

{ ============================================================================
   [Error Handling - Internal]
  ============================================================================ }

procedure _BLCheck(const AResult: Integer); overload;
procedure _BLCheck(const AResult: TBLResult); overload;
{$ENDREGION 'Error Handling'}

{$REGION 'Globals'}

{ ============================================================================
   [Globals - Types]
  ============================================================================ }

type
  /// <summary>
  ///  Tag is a 32-bit integer consisting of 4 bytes (or ASCII characters) in
  ///  the following format:
  ///
  ///  ```
  ///  Tag := (A shl 24) or (B shl 16) or (C shl 8) or D;
  ///  ```
  ///
  ///  Tags are used extensively by OpenType fonts and other binary formats like
  ///  PNG. In most cases TAGs should only contain ASCII letters, digits, and
  ///  spaces.
  ///
  ///  Blend2D uses TBLTag in public and internal APIs to distinguish between a
  ///  regular `UInt32` and tag.
  /// </summary>
  TBLTag = type UInt32;

type
  ///  Unique identifier that can be used for caching purposes.
  ///
  ///  Some objects such as TBLImage and TBLFontFace have assigned an unique
  ///  identifier that can be used to identify such objects for caching
  ///  purposes. This identifier is never zero, so zero can be safely used as
  ///  "uncached".
  /// </summary>
  /// <remarks>
  ///  Unique identifier is per-process. It's implemented as an increasing
  ///  global or thread-local counter in a way that identifiers would not
  ///  collide.
  /// </remarks>
  /// <seealso cref="TBLImage"/>
  /// <seealso cref="TBLFontFace"/>
  TBLUniqueId = UInt64;

{ ============================================================================
   [Globals - Enums and Sets]
  ============================================================================ }

type
  /// <summary>
  ///  Byte order.
  /// </summary>
  TBLByteOrder = (
    /// <summary>
    ///  Little endian byte-order.
    /// </summary>
    LE,

    /// <summary>
    ///  Big endian byte-order.
    /// </summary>
    BE,

    /// <summary>
    ///  Native (host) byte-order.
    /// </summary>
    Native  = LE,

    /// <summary>
    ///  Swapped byte-order (BE if host is LE and vice versa).
    /// </summary>
    Swapped = BE);

type
  /// <summary>
  ///  Data access flags.
  /// </summary>
  TBLDataAccessFlags = (
    /// <summary>
    ///  No data access flags.
    /// </summary>
    None  = $00,

    /// <summary>
    ///  Read access.
    /// </summary>
    Read  = $01,

    /// <summary>
    ///  Write access.
    /// </summary>
    Write = $02,

    /// <summary>
    /// Read and write access.
    /// </summary>
    RW    = $03);

type
  /// <summary>
  ///  Data source type.
  /// </summary>
  TBLDataSourceType = (
    /// <summary>
    ///  No data source.
    /// </summary>
    None,

    /// <summary>
    ///  Memory data source.
    /// </summary>
    Memory,

    /// <summary>
    ///  File data source.
    /// </summary>
    &File,

    /// <summary>
    ///  Custom data source.
    /// </summary>
    Custom);

type
  /// <summary>
  ///  Modification operation applied to Blend2D containers.
  /// </summary>
  TBLModifyOp = (
    /// <summary>
    ///  Assign operation, which reserves space only to fit the requested input.
    /// </summary>
    AssignFit,

    /// <summary>
    ///  Assign operation, which takes into consideration successive appends.
    /// </summary>
    AssignGrow,

    /// <summary>
    ///  Append operation, which reserves space only to fit the current and appended content.
    /// </summary>
    AppendFit,

    /// <summary>
    ///  Append operation, which takes into consideration successive appends.
    /// </summary>
    AppendGrow);

type
  /// <summary>
  ///  Boolean operator (between A and B).
  /// </summary>
  TBLBooleanOp = (
    /// <summary>
    ///  Result = B.
    /// </summary>
    Copy,

    /// <summary>
    ///  Result = A and B.
    /// </summary>
    &And,

    /// <summary>
    ///  Result = A or B.
    /// </summary>
    &Or,

    /// <summary>
    ///  Result = A xor B.
    /// </summary>
    &Xor,

    /// <summary>
    ///  Result = A and (not B).
    /// </summary>
    AndNot,

    /// <summary>
    ///  Result = (not A) and B.
    /// </summary>
    NotAnd);

{$ENDREGION 'Globals'}

{$REGION 'Objects'}

type
  /// <summary>
  ///  Object type identifier.
  /// </summary>
  TBLObjectType = (
    /// <summary>
    ///  Object represents a TBLRgba value stored as four 32-bit floating point
    ///  components (can be used as Style).
    /// </summary>
    Rgba = 0,

    /// <summary>
    ///  Object represents a TBLRgba32 value stored as 32-bit integer in
    ///  `$AARRGGBB` form.
    /// </summary>
    Rgba32 = 1,

    /// <summary>
    ///  Object represents a TBLRgba64 value stored as 64-bit integer in
    ///  `$AAAARRRRGGGGBBBB` form.
    /// </summary>
    Rgba64 = 2,

    /// <summary>
    ///  Object is `Null` (can be used as Style).
    /// </summary>
    Null = 3,

    /// <summary>
    ///  Object is TBLPattern (can be used as Style).
    /// </summary>
    Pattern = 4,

    /// <summary>
    ///  Object is TBLGradient (can be used as Style).
    /// </summary>
    Gradient = 5,

    /// <summary>
    ///  Object is TBLImage.
    /// </summary>
    Image = 9,

    /// <summary>
    ///  Object is TBLPath.
    /// </summary>
    Path = 10,

    /// <summary>
    ///  Object is TBLFont.
    /// </summary>
    Font = 16,

    /// <summary>
    ///  Object is TBLFontFeatureSettings.
    /// </summary>
    FontFeatureSettings = 17,

    /// <summary>
    ///  Object is TBLFontVariationSettings.
    /// </summary>
    FontVariationSettings = 18,

    /// <summary>
    ///  Object is TBLBitArray.
    /// </summary>
    BitArray = 25,

    /// <summary>
    ///  Object is TBLBitSet.
    /// </summary>
    BitSet = 26,

    /// <summary>
    ///  Object represents a Boolean value.
    /// </summary>
    Bool = 28,

    /// <summary>
    ///  Object represents a 64-bit signed integer value.
    /// </summary>
    Int64 = 29,

    /// <summary>
    ///  Object represents a 64-bit unsigned integer value.
    /// </summary>
    UInt64 = 30,

    /// <summary>
    ///  Object represents a 64-bit floating point value.
    /// </summary>
    Double = 31,

    /// <summary>
    ///  Object is TBLString.
    /// </summary>
    &String = 32,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a `TBLObject` compatible type.
    /// </summary>
    ArrayObject = 33,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 8-bit signed integral type.
    /// </summary>
    ArrayInt8 = 34,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 8-bit unsigned integral type.
    /// </summary>
    ArrayUInt8 = 35,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 16-bit signed integral type.
    /// </summary>
    ArrayInt16 = 36,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 16-bit unsigned integral type.
    /// </summary>
    ArrayUInt16 = 37,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 32-bit signed integral type.
    /// </summary>
    ArrayInt32 = 38,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 32-bit unsigned integral type.
    /// </summary>
    ArrayUInt32 = 39,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 64-bit signed integral type.
    /// </summary>
    ArrayInt64 = 40,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 64-bit unsigned integral type.
    /// </summary>
    ArrayUInt64 = 41,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 32-bit floating point type.
    /// </summary>
    ArrayFloat32 = 42,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` matches 64-bit floating point type.
    /// </summary>
    ArrayFloat64 = 43,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 1.
    /// </summary>
    ArrayStruct1 = 44,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 2.
    /// </summary>
    ArrayStruct2 = 45,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 3.
    /// </summary>
    ArrayStruct3 = 46,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 4.
    /// </summary>
    ArrayStruct4 = 47,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 6.
    /// </summary>
    ArrayStruct6 = 48,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 8.
    /// </summary>
    ArrayStruct8 = 49,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 10.
    /// </summary>
    ArrayStruct10 = 50,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 12.
    /// </summary>
    ArrayStruct12 = 51,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 16.
    /// </summary>
    ArrayStruct16 = 52,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 20.
    /// </summary>
    ArrayStruct20 = 53,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 24.
    /// </summary>
    ArrayStruct24 = 54,

    /// <summary>
    ///  Object is TBLArray&lt;T> where `T` is a record of size 32.
    /// </summary>
    ArrayStruct32 = 55,

    /// <summary>
    ///  Object is TBLContext.
    /// </summary>
    Context = 100,

    /// <summary>
    ///  Object is TBLImageCodec.
    /// </summary>
    ImageCodec = 101,

    /// <summary>
    ///  Object is TBLImageDecoder.
    /// </summary>
    ImageDecoder = 102,

    /// <summary>
    ///  Object is TBLImageEncoder.
    /// </summary>
    ImageEncoder = 103,

    /// <summary>
    ///  Object is TBLFontFace.
    /// </summary>
    FontFace = 104,

    /// <summary>
    ///  Object is TBLFontData.
    /// </summary>
    FontData = 105,

    /// <summary>
    ///  Object is TBLFontManager.
    /// </summary>
    FontManager = 106,

    /// <summary>
    ///  Minimum object type of an array object.
    /// </summary>
    MinArray = 33,

    /// <summary>
    ///  Maximum object type of an array object.
    /// </summary>
    MaxArray = 55,

    /// <summary>
    ///  Minimum object type identifier that can be used as a style.
    /// </summary>
    MinStyle = 0,

    /// <summary>
    ///  Maximum object type identifier that can be used as a style.
    /// </summary>
    MaxStyle = 5,

    /// <summary>
    ///  Minimum object type of an object with virtual function table.
    /// </summary>
    MinVirtual = 100,

    /// <summary>
    ///  Maximum object type of an object with virtual function table.
    /// </summary>
    MaxVirtual = 127);

type
  /// <summary>
  /// Base "class" used by all Blend2D objects.
  /// </summary>
  TBLObjectCore = record
  {$REGION 'Internal Declarations'}
  private const
    P_SHIFT    = 0;
    Q_SHIFT    = 8;
    C_SHIFT    = 8;
    B_SHIFT    = 12;
    A_SHIFT    = 16;
    TYPE_SHIFT = 22;
    R_SHIFT    = 29;
    D_SHIFT    = 30;
    M_SHIFT    = 31;
  private const
    P_MASK = $FF shl P_SHIFT;           // [........|........|........|pppppppp]
    Q_MASK = $FF shl Q_SHIFT;           // [........|........|qqqqqqqq|........]
    C_MASK = $0F shl C_SHIFT;           // [........|........|....cccc|........]
    B_MASK = $0F shl B_SHIFT;           // [........|........|bbbb....|........]
    A_MASK = $3F shl A_SHIFT;           // [........|..aaaaaa|........|........]
    FIELDS_MASK = $003FFFF;
    TYPE_MASK = $7F shl TYPE_SHIFT;     // [...ttttt|tt......|........|........]
    R_FLAG = $01 shl R_SHIFT;           // [..R.....|........|........|........]
    D_FLAG = $01 shl D_SHIFT;           // [.D......|........|........|........]
    M_FLAG = Cardinal($01) shl M_SHIFT; // [M.......|........|........|........]
    MD_FLAGS = M_FLAG or D_FLAG;
    MDR_FLAGS = M_FLAG or D_FLAG or R_FLAG;
  private const
    /// <summary>
    /// Size of object static storage not considering FInfo field.
    /// </summary>
    STATIC_DATA_SIZE = 12;
  private
    FImpl: Pointer;
    {$IFDEF CPU32BITS}
    FData: UInt64;
    {$ELSE}
    FData: UInt32;
    {$ENDIF}
    FInfo: UInt32;
//  private
//    class function PackTypeWithMarker(const AType: TBLObjectType): UInt32; inline; static;
//    class function PackAbcp(const AA: UInt32; const AB: UInt32 = 0;
//      const AC: UInt32 = 0; const AP: UInt32 = 0): UInt32; inline; static;
  private
//    procedure InitStatic(const AInfo: UInt32); inline;
    function NeedsCleanup: Boolean; inline;
    procedure Swap(var AOther: TBLObjectCore); inline;
  {$ENDREGION 'Internal Declarations'}
  end;

type
  /// <summary>
  ///  A function callback that is called when an object that holds external data
  ///  is going to be destroyed. It's often used as a notification that a data
  ///  passed to a certain object is no longer in use by Blend2D.
  /// </summary>
  TBLDestroyExternalDataFunc = procedure(AImpl, AExternalData, AUserData: Pointer); cdecl;

{$ENDREGION 'Objects'}

{$REGION 'Containers'}

{ ============================================================================
   [Containers - Views & Common Types]
  ============================================================================ }

type
  /// <summary>
  ///  Array view of `T`.
  /// </summary>
  /// <remarks>
  ///  `T` must be an unmanaged type (eg. cannot be a String or interface or
  ///  dynamic array) and cannot contain [weak] references. This is checked with
  ///  an assertion.
  /// </remarks>
  TBLArrayView<T> = record
  public type
    {$POINTERMATH ON}
    /// <summary>
    ///  A pointer to type `T`.
    /// </summary>
    P = ^T;
    {$POINTERMATH OFF}
  {$REGION 'Internal Declarations'}
  private
    FData: P;
    FSize: NativeInt;
    function GetItem(const AIndex: NativeInt): T; inline;
    function GetRef(const AIndex: NativeInt): P; inline;
  public
    class constructor Create;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default empty array view
    /// </summary>
    class operator Initialize(out ADest: TBLArrayView<T>);

    procedure Reset; overload; inline;
    procedure Reset(const ADataIn: P; const ASizeIn: NativeInt); overload; inline;

    function First: P; inline;
    function Last: P; inline;

    property Size: NativeInt read FSize;
    property Items[const AIndex: NativeInt]: T read GetItem; default;
    property Refs[const AIndex: NativeInt]: P read GetRef;
  end;

type
  /// <summary>
  ///  View of `UTF8Char[]` data used by TBLString.
  /// </summary>
  TBLStringView = TBLArrayView<UTF8Char>;

type
  /// <summary>
  ///  View of untyped data.
  /// </summary>
  TBLDataView = TBLArrayView<Byte>;

type
  /// <summary>
  ///  Provides start and end indexes. It has the same semantics as Slices in
  ///  other programming languages - range is always within [Start, Stop)
  ///  internal (Start is inclusive, Stop is exclusive). It's used to specify a
  ///  range of an operation of indexed containers like TBLString, TBLArray,
  ///  TBLGradient, TBLPath, etc...
  /// </summary>
  TBLRange = record
  public
    Start: NativeInt;
    Stop: NativeInt;
  public
    class operator Equal(const ALeft, ARight: TBLRange): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRange): Boolean; inline; static;

    /// <summary>
    ///  Reset the range to [0, 0).
    /// </summary>
    procedure Reset; overload; inline;

    /// <summary>
    ///  Reset the range to [AStart, AStop).
    /// </summary>
    procedure Reset(const AStart, AStop: NativeInt); overload; inline;

    function Equals(const AOther: TBLRange): Boolean; inline;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLRange.
  /// </summary>
  _TBLRangeHelper = record helper for TBLRange
  public const
    Everything: TBLRange = (Start: 0; Stop: NativeInt.MaxValue);
  end;

{ ============================================================================
   [Containers - Sequential Containers]
  ============================================================================ }

type
  /// <summary>
  ///  Generic array container.
  /// </summary>
  /// <remarks>
  ///  'T' must be of one of the following types:
  ///    * A signed or unsigned 8-, 16-, 32- or 64-bit integer.
  ///    * Enumerated types.
  ///    * A 32- or 64-bit floating-point value (Single or Double).
  ///    * An unmanaged record that does not contain any [weak] references.
  ///      So the record cannot contain reference-counted values like Strings,
  ///      Interfaces or dynamic arrays. Also, only small records of certain
  ///      sizes <= 32 bytes are supported.
  ///
  ///  These conditions are checked with an assertion/
  /// </remarks>
  TBLArray<T> = record
  public type
    {$POINTERMATH ON}
    /// <summary>
    ///  A pointer to type `T`.
    /// </summary>
    P = ^T;
    {$POINTERMATH OFF}
  {$REGION 'Internal Declarations'}
  private class var
    FArrayType: TBLObjectType;
//    FSsoEmptySignature: UInt32;
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
    function GetSize: NativeInt; inline;
    function GetCapacity: NativeInt; inline;
    function GetItem(const AIndex: NativeInt): T; inline;
    procedure SetItem(const AIndex: NativeInt; const AValue: T); inline;
    function GetRef(const AIndex: NativeInt): P; inline;
    function GetData: P; inline;
  public
    class constructor Create;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLArray<T>);

    /// <summary>
    ///  Destroys the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLArray<T>);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Creates a weak-copy of the `ASrc` array by increasing it's internal
    ///  reference counter. This array and `ASrc` would point to the same data
    ///  and would be otherwise identical. Any change to `ASrc` would also
    ///  affect this array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLArray<T>; const [ref] ASrc: TBLArray<T>); inline;

    /// <summary>
    ///  Used to compare against `nil` (empty array).
    /// </summary>
    class operator Equal(const ALeft: TBLArray<T>; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two arrays are equal (have the same contents).
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLArray<T>): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil` (empty array).
    /// </summary>
    class operator NotEqual(const ALeft: TBLArray<T>; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two arrays are not equal (do not have the same contents).
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLArray<T>): Boolean; inline; static;

    /// <summary>
    ///  Returns whether the content of this array and `AOther` matches.
    /// </summary>
    function Equals(const AOther: TBLArray<T>): Boolean; inline;

    /// <summary>
    ///  Resets the array into a default constructed state by clearing its
    ///  content and releasing its memory.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reset; inline;

    /// <summary>
    ///  Swaps the content of this array with the `AOther` array.
    /// </summary>
    procedure Swap(var AOther: TBLArray<T>); inline;

    /// <summary>
    ///  Returns a reference to the first item.
    /// </summary>
    /// <remarks>
    ///  The array must have at least one item otherwise calling `First` would
    ///  point to the end of the array, which is not initialized, and such
    ///  reference would be invalid. Debug builds would catch this condition
    ///  with an assertion.
    /// </remarks>
    function First: P; inline;

    /// <summary>
    ///  Returns a reference to the last item.
    /// </summary>
    /// <remarks>
    ///  The array must have at least one item otherwise calling `Last` would
    ///  point to the end of the array, which is not initialized, and such
    ///  reference would be invalid. Debug builds would catch this condition
    ///  with an assertion.
    /// </remarks>
    function Last: P; inline;

    /// <summary>
    ///  Returns the array data as `TBLArrayView<T>`.
    /// </summary>
    function View: TBLArrayView<T>; inline;

    /// <summary>
    ///  Returns the array item at the given `AIndex`.
    /// </summary>
    /// <remarks>
    ///  The index must be valid, which means it has to be less than the array
    ///  length. Accessing items out of range is undefined behavior that would
    ///  be caught by assertions in debug builds.
    /// </remarks>
    function At(const AIndex: NativeInt): T; inline;

    /// <summary>
    ///  Returns a reference to the array item at the given `AIndex`.
    /// </summary>
    /// <remarks>
    ///  The index must be valid, which means it has to be less than the array
    ///  length. Accessing items out of range is undefined behavior that would
    ///  be caught by assertions in debug builds.
    /// </remarks>
    function RefAt(const AIndex: NativeInt): P; inline;

    /// <summary>
    ///  Clears the content of the array.
    /// </summary>
    /// <remarks>
    ///  If the array uses a dynamically allocated memory and the instance is
    ///  mutable the memory won't be released, it will be reused instead.
    ///  Consider using `Reset` if you want to release the memory in such case
    ///  instead.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Clear; inline;

    /// <summary>
    ///  Shrinks the capacity of the array to fit its length.
    ///
    ///  Some array operations like `Append` may grow the array more than
    ///  necessary to make it faster when such manipulation operations are
    ///  called consecutively. When you are done with modifications and you know
    ///  the lifetime of the array won't be short you can use `Shrink` to fit
    ///  its memory requirements to the number of items it stores, which could
    ///  optimize the application's memory requirements.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Shrink; inline;

    /// <summary>
    ///  Reserves the array capacity to hold at least `AMinCapacity` items.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reserve(const AMinCapacity: NativeInt); inline;

    /// <summary>
    ///  Truncates the length of the array to maximum `AMaxSize` items.
    ///
    ///  If the length of the array is less than `AMaxSize` then truncation
    ///  does nothing.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Truncate(const AMaxSize: NativeInt); inline;

    /// <summary>
    ///  Resizes the array to `ASize` items.
    ///
    ///  If `ASize` is greater than the array length then all new items will be
    ///  initialized by `AFill` item.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Resize(const ASize: NativeInt; const AFill: T); inline;

    /// <summary>
    ///  Makes the array mutable by possibly creating a deep copy of the data if
    ///  it's either read-only or shared with another array. Returns a pointer
    ///  to the beginning of mutable data in `dataOut`.
    ///
    ///  ```Delphi
    ///  var A: TBLArray<Byte>;
    ///  A.Append([0, 1, 2, 3, 4, 5, 6, 7]);
    ///
    ///  var Data := A.MakeMutable;
    ///
    ///  // `Data` is a mutable pointer to array content of 8 items.
    ///  Data[0] = 100;
    ///
    ///  // Calling array member functions could invalidate `Data`.
    ///  A.Append(9); // You shouldn't use `Data` afterwards.
    ///  ```
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    function MakeMutable: P; inline;

    /// <summary>
    ///  Modify operation is similar to `MakeMutable`, however, the `AOp`
    ///  argument specifies the desired array operation, and `ASize` specified
    ///  the number of items to assign or append. Returns a pointer to the first
    ///  item to be either assigned or appended and it points to an
    ///  uninitialized memory.
    ///
    ///  Please note that assignments mean to wipe out the whole array content
    ///  and to set the length of the array to `ASize`. The caller is
    ///  responsible for initializing the returned data.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="MakeMutable"/>
    function ModifyOp(const AOp: TBLModifyOp; const ASize: NativeInt): P; inline;

    /// <summary>
    ///  Insert operation, the semantics is similar to `ModifyOp`, however,
    ///  ASize items are inserted at the given `AIndex` instead of assigned or
    ///  appended.
    ///
    ///  The caller is responsible for initializing the returned data.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="ModifyOp"/>
    function InsertOp(const AIndex, ASize: NativeInt): P; inline;

    /// <summary>
    /// Similar to `ModifyOp`, but the items to assign/append to the array are
    /// given after the `AOp` argument.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="ModifyOp"/>
    procedure Modify(const AOp: TBLModifyOp; const AItems: array of T);

    /// <summary>
    ///  Copy assignment, but creates a deep copy of the `AOther` array instead
    ///  of weak copy.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignDeep(const AOther: TBLArray<T>); inline;

    /// <summary>
    ///  Replaces the content of the array with the items passed in `AItems`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Assign(const AItems: array of T);

    /// <summary>
    ///  Replaces the content of the array by items in the passed array `AView`.
    /// </summary>
    /// <remarks>
    ///  The implementation can handle `AView` pointing to the array's data as
    ///  well, so it's possible to create a slice of the array if required.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignData(const AView: TBLArrayView<T>); overload; inline;

    /// <summary>
    ///  Replaces the content of the array `AItems` of length `ASize`.
    /// </summary>
    /// <remarks>
    ///  The implementation can handle items pointing to the array's data as
    ///  well, so it's possible to create a slice of the array if required.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignData(const AItems: P; const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Assign an external buffer to the array, which would replace the
    ///  existing content.
    ///
    /// <param name="AData">External data buffer to use (cannot be nil).</param>
    /// <param name="ASize">Size of the data buffer in items.</param>
    /// <param name="ACapacity">Capacity of the buffer, cannot be zero or
    ///  smaller than `ASize`.</param>
    /// <param name="AAccessFlags">Flags that describe whether the data is
    ///  read-only or read-write.</param>
    /// <param name="ADestroyFunc">(Optional) function that would be called when
    ///  the array is destroyed (can be nil if you don't need it).</param>
    /// <param name="AUserData">(Optional) user data passed to `ADestroyFunc`.</param>
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignExternalData(const AData: P; const ASize,
      ACapacity: NativeInt; const AAccessFlags: TBLDataAccessFlags;
      const ADestroyFunc: TBLDestroyExternalDataFunc = nil;
      const AUserData: Pointer = nil); inline;

    /// <summary>
    ///  Appends `AItem` to the array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Append(const AItem: T); overload; inline;

    /// <summary>
    ///  Appends a number of items items passed in `AItems` to the array.
    /// </summary>
    /// <remarks>
    ///  The data in `AItems` cannot point to the same data that the array holds
    ///  as the function that prepares the append operation has no way to know
    ///  about the source (it only makes space for new data). It's an undefined
    ///  behavior in such case.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Append(const AItems: array of T); overload;

    /// <summary>
    ///  Appends items to the array of the given array `AView`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AView` pointing to the array data
    ///  itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendData(const AView: TBLArrayView<T>); overload; inline;

    /// <summary>
    ///  Appends `AItems` to the array of length `ASize`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AItems` pointing to the array
    ///  data itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendData(const AItems: P; const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Prepends `AItem` to the array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Prepend(const AItem: T); overload; inline;

    /// <summary>
    ///  Prepends a number of items passed in `AItems` to the array.
    /// </summary>
    /// </remarks>
    ///  The data in `AItems` cannot point to the same data that the array holds
    ///  as the function that prepares the prepend operation has no way to know
    ///  about the source (it only makes space for new data). It's an undefined
    ///  behavior in such case.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Prepend(const AItems: array of T); overload;

    /// <summary>
    ///  Prepends items to the array of the given array `AView`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AView` pointing to the array data
    ///  itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure PrependData(const AView: TBLArrayView<T>); overload; inline;

    /// <summary>
    ///  Prepends `AItems` to the array of length `ASize`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AItems` pointing to the array
    ///  data itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure PrependData(const AItems: P; const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Inserts `AItem` at the given `AIndex`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Insert(const AIndex: NativeInt; const AItem: T); overload;

    /// <summary>
    ///  Inserts a number of items passed in `AItems` at the given `AIndex`.
    /// </summary>
    /// <remarks>
    ///  The data in `AItems` cannot point to the same data that the array holds
    ///  as the function that prepares the insert operation has no way to know
    ///  about the source (it only makes space for new data). It's an undefined
    ///  behavior in such case.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Insert(const AIndex: NativeInt; const AItems: array of T); overload;

    /// <summary>
    ///  Inserts items to the array of the given array `AView` at the given
    ///  `AIndex`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AView` pointing to the array data
    ///  itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure InsertData(const AIndex: NativeInt; const AView: TBLArrayView<T>); overload; inline;

    /// <summary>
    ///  Prepends `AItems` to the array of length `ASize` at the given `AIndex`.
    /// </summary>
    /// <remarks>
    ///  The implementation guarantees that a `AItems` pointing to the array
    ///  data itself would work.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure InsertData(const AIndex: NativeInt; const AItems: P;
      const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Replaces an item at the given `AIndex` by `AItem`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Replace(const AIndex: NativeInt; const AItem: T); inline;

    /// <summary>
    ///  Replaces the given `ARange` of items by the given array `AView`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceData(const ARange: TBLRange; const AView: TBLArrayView<T>); overload; inline;

    /// <summary>
    ///  Replaces the given `ARange` of items by `AItems` of length `ASize`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceData(const ARange: TBLRange; const AItems: P;
      const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Removes an item at the given `Andex`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Remove(const AIndex: NativeInt); overload; inline;

    /// <summary>
    ///  Removes `ARange` of items.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Remove(const ARange: TBLRange); overload; inline;

    /// <summary>
    ///  Returns the first index at which a given `AItem` can be found in the
    ///  array, or -1 if not found.
    /// </summary>
    function IndexOf(const AItem: T): NativeInt; overload; inline;

    /// <summary>
    ///  Returns the index at which a given `AItem` can be found in the array
    ///  starting from `AFromIndex`, or -1 if not found.
    /// </summary>
    function IndexOf(const AItem: T; const AFromIndex: NativeInt): NativeInt; overload;

    /// <summary>
    ///  Returns the last index at which a given `AItem` can be found in the
    ///  array, or -1 if not present.
    /// </summary>
    function LastIndexOf(const AItem: T): NativeInt; overload; inline;

    /// <summary>
    ///  Returns the index at which a given `AItem` can be found in the array
    ///  starting from `AFromIndex` and ending at 0, or -1 if not present.
    /// </summary>
    function LastIndexOf(const AItem: T; const AFromIndex: NativeInt): NativeInt; overload;

    /// <summary>
    ///  Whether the array is empty.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///  The size of the array (number of items).
    /// </summary>
    property Size: NativeInt read GetSize;

    /// <summary>
    ///  The capacity of the array (number of items).
    /// </summary>
    property Capacity: NativeInt read GetCapacity;

    /// <summary>
    ///  The items in the array.
    /// </summary>
    /// <remarks>
    ///  This is the same as calling `At(AIndex)`.
    ///  The index must be valid, which means it has to be less than the array
    ///  length. Accessing items out of range is undefined behavior that would
    ///  be caught by assertions in debug builds.
    /// </remarks>
    property Items[const AIndex: NativeInt]: T read GetItem write SetItem; default;

    /// <summary>
    ///  References to the items in the array.
    /// </summary>
    /// <remarks>
    ///  This is the same as calling `RefAt(AIndex)`.
    /// </remarks>
    property Refs[const AIndex: NativeInt]: P read GetRef;

    /// <summary>
    ///  Pointer to the array data.
    /// </summary>
    property Data: P read GetData;
  end;

type
  /// <summary>
  ///  Byte string.
  ///
  ///  Blend2D always uses UTF-8 encoding in public APIs so all strings are
  ///  assumed UTF-8 by default. However, `TBLString` doesn't guarantee any
  ///  assumptions about the encoding of the data it holds. It can hold
  ///  arbitrary byte sequence and act as a raw byte-string when this
  ///  functionality is desired.
  /// </summary>
  /// <remarks>
  ///  Most Blend2D API's use `TBLString` instead of Delphi's built-in String
  ///  (UnicodeString) type since `TBLString` can be more efficient for short
  ///  strings. If you use the same string multiple times, it's more efficient
  ///  to keep a `TBLString` value around instead recreating them or using
  ///  regular strings, to avoid Unicode-to-UTF8 conversions.
  /// </remarks>
  TBLString = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
    function GetSize: NativeInt; inline;
    function GetCapacity: NativeInt; inline;
    function GetData: PUTF8Char; inline;
    function GetChar(const AIndex: NativeInt): UTF8Char; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates an empty string.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLString);

    /// <summary>
    ///  Destroys the string.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLString);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Performs weak copy of the data held by the `ASrc` string.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLString; const [ref] ASrc: TBLString); inline;

    /// <summary>
    ///  Constructor that creates a string from the given string `AView`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const AView: TBLStringView); overload;

    /// <summary>
    ///  Constructor that creates a string from the given UTF8 data specified by
    ///  `AStr` and `ASize`. If `ASize` is -1 the string is assumed to be null
    ///  terminated.
    ///
    ///  This is a convenience function that doesn't provide error handling. If
    ///  size exceeds small string capacity and dynamic allocation failed then a
    ///  default empty string would be constructed.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const AStr: PUTF8Char; const ASize: NativeInt = -1); overload;

    /// <summary>
    ///  Constructor that creates a string from a Delphi (Unicode) String.
    /// </summary>
    /// <remarks>
    ///  This involves a Unicode-to-UTF8 conversion.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const AStr: String); overload;

    /// <summary>
    ///  Constructor that creates a string from a Delphi UTF8String.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const AStr: UTF8String); overload;

    /// <summary>
    ///  Implicitly convers a Delphi (Unicode) String to a TBLString.
    /// </summary>
    /// <remarks>
    ///  This involves a Unicode-to-UTF8 conversion.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Implicit(const AStr: String): TBLString; inline; static;

    /// <summary>
    ///  Implicitly convers a Delphi UTF8String to a TBLString.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Implicit(const AStr: UTF8String): TBLString; inline; static;

    /// <summary>
    ///  Used to compare against `nil` (empty string).
    /// </summary>
    class operator Equal(const ALeft: TBLString; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two strings are equal (have the same contents).
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLString): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil` (empty string).
    /// </summary>
    class operator NotEqual(const ALeft: TBLString; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two strings are not equal (do not have the same contents).
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLString): Boolean; inline; static;

    class operator LessThan(const ALeft, ARight: TBLString): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft, ARight: TBLString): Boolean; inline; static;
    class operator GreaterThan(const ALeft, ARight: TBLString): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft, ARight: TBLString): Boolean; inline; static;

    class operator Equal(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;
    class operator LessThan(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;
    class operator GreaterThan(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft: TBLString; const ARight: TBLStringView): Boolean; inline; static;

    class operator Equal(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;
    class operator LessThan(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;
    class operator GreaterThan(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft: TBLString; const ARight: PUTF8Char): Boolean; inline; static;

    class operator Equal(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;
    class operator LessThan(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;
    class operator GreaterThan(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft: TBLString; const ARight: UTF8String): Boolean; inline; static;

    class operator Equal(const ALeft: TBLString; const ARight: String): Boolean; inline; static;
    class operator NotEqual(const ALeft: TBLString; const ARight: String): Boolean; inline; static;
    class operator LessThan(const ALeft: TBLString; const ARight: String): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft: TBLString; const ARight: String): Boolean; inline; static;
    class operator GreaterThan(const ALeft: TBLString; const ARight: String): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft: TBLString; const ARight: String): Boolean; inline; static;

    /// <summary>
    ///  Returns whether this string and `AOther` are equal (i.e. their contents
    ///  match).
    /// </summary>
    function Equals(const AOther: TBLString): Boolean; overload; inline;

    /// <summary>
    ///  Returns whether this string and other string `AView` are equal.
    /// </summary>
    function Equals(const AOther: TBLStringView): Boolean; overload; inline;

    /// <summary>
    ///  Returns whether this string and the given string data `AStr` of length
    ///  `ASize` are equal.
    /// </summary>
    function Equals(const AOther: PUTF8Char; const ASize: NativeInt = -1): Boolean; overload; inline;

    /// <summary>
    ///  Returns whether this string and the given UTF8 string data `AStr` are
    ///  equal.
    /// </summary>
    function Equals(const AOther: UTF8String): Boolean; overload; inline;

    /// <summary>
    ///  Returns whether this string and the given Unicode string data `AStr`
    ///  are equal.
    /// </summary>
    /// <remarks>
    ///  This involves a Unicode-to-UTF8 conversion of AOther.
    /// </remarks>
    function Equals(const AOther: String): Boolean; overload; inline;

    /// <summary>
    ///  Compares this string with `AOther` and returns either `-1`, `0`, or `1`.
    /// </summary>
    function Compare(const AOther: TBLString): Integer; overload; inline;

    /// <summary>
    ///  Compares this string with other string `AView` and returns either
    ///  `-1`, `0`, or `1`.
    /// </summary>
    function Compare(const AOther: TBLStringView): Integer; overload; inline;

    /// <summary>
    ///  Compares this string with other string data and returns either
    ///  `-1`, `0`, or `1`.
    /// </summary>
    function Compare(const AOther: PUTF8Char; const ASize: NativeInt = -1): Integer; overload; inline;

    /// <summary>
    ///  Compares this string with other string data and returns either
    ///  `-1`, `0`, or `1`.
    /// </summary>
    function Compare(const AOther: UTF8String): Integer; overload; inline;

    /// <summary>
    ///  Compares this string with other string data and returns either
    ///  `-1`, `0`, or `1`.
    /// </summary>
    /// <remarks>
    ///  This involves a Unicode-to-UTF8 conversion of AOther.
    /// </remarks>
    function Compare(const AOther: String): Integer; overload; inline;

    /// <summary>
    ///  Returns a character at the given `AIndex`.
    /// </summary>
    /// <remarks>
    ///  Index must be valid and cannot be out of bounds - there is an assertion.
    /// </remarks>
    function At(const AIndex: NativeInt): UTF8Char; inline;

    /// <summary>
    ///  Clears the content of the string and releases its data.
    ///
    ///  After reset the string content matches a default constructed string.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reset; inline;

    /// <summary>
    ///  Swaps the content of this string with the `AOther` string.
    /// </summary>
    procedure Swap(var AOther: TBLString); inline;

    /// <summary>
    ///  Returns the content of the string as `TBLStringView`.
    /// </summary>
    function View: TBLStringView; inline;

    /// <summary>
    ///  Clears the content of the string without releasing its dynamically
    ///  allocated data, if possible.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Clear; inline;

    /// <summary>
    ///  Shrinks the capacity of the string to match the actual content.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Shrink; inline;

    /// <summary>
    ///  Reserves at least `AMinSize` bytes (UTF8 characters) in the string for
    ///  further manipulation (most probably appending).
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reserve(const AMinSize: NativeInt); inline;

    /// <summary>
    ///  Resizes the string to `ASize` and fills the additional data by
    ///  `AFill` pattern.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Resize(const ASize: NativeInt; const AFill: UTF8Char = #0); inline;

    /// <summary>
    ///  Makes the string mutable.
    ///
    ///  This operation checks whether the string is mutable and if not it makes
    ///  a deep copy of its content so it can be modified. Please note that you
    ///  can only modify the content that is defined by its length property.
    ///  Even if the string had higher capacity before `MakeMutable` it's not
    ///  guaranteed that the possible new data would match that capacity.
    ///
    ///  If you want to make the string mutable for the purpose of appending or
    ///  making other modifications please consider using `ModifyOp` and
    ///  `InsertOp` instead.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="ModifyOp"/>
    /// <seealso cref="InsertOp"/>
    function MakeMutable: PUTF8Char; inline;

    function ModifyOp(const AOp: TBLModifyOp; const ASize: NativeInt): PUTF8Char; inline;
    function InsertOp(const AIndex, ASize: NativeInt): PUTF8Char; inline;

    /// <summary>
    ///  Replaces the content of the string by `AChar` character or multiple
    ///  characters if `ACount` is greater than one.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Assign(const AChar: UTF8Char; const ACount: NativeInt = 1); overload; inline;

    /// <summary>
    ///  Replaces the string by the content described by the given string `AView`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Assign(const AView: TBLStringView); overload; inline;

    /// <summary>
    ///  Replaces the string by `AStr` data of the given length `ASize`.
    /// </summary>
    /// <remarks>
    ///  The implementation assumes null terminated string if `ASize` equals to
    ///  `-1`.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Assign(const AStr: PUTF8Char; const ASize: NativeInt = -1); overload; inline;

    /// <summary>
    ///  Copy assignment, but creates a deep copy of the `AOther` string instead
    ///  of weak copy.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignDeep(const AOther: TBLString); inline;

    /// <summary>
    ///  Replaces the content of the string by a result of calling
    ///  `Format(AFmt, AArgs)`.
    /// </summary>
    /// <remarks>
    ///  This involves a Unicode-to-UTF8 conversion.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignFormat(const AFmt: String; const AArgs: array of const);

    /// <summary>
    ///  Truncates the string length to `ASize`.
    ///
    ///  It does nothing if the the string length is less than `ASize`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Truncate(const ASize: NativeInt); inline;

    procedure Append(const AChar: UTF8Char; const ACount: NativeInt = 1); overload; inline;
    procedure Append(const AOther: TBLString); overload; inline;
    procedure Append(const AView: TBLStringView); overload; inline;
    procedure Append(const AStr: PUTF8Char; const ASize: NativeInt = -1); overload; inline;
    procedure Append(const AStr: UTF8String); overload; inline;
    procedure Append(const AStr: String); overload; inline;

    procedure AppendFormat(const AFmt: String; const AArgs: array of const);

    procedure Prepend(const AChar: UTF8Char; const ACount: NativeInt = 1); overload; inline;
    procedure Prepend(const AOther: TBLString); overload; inline;
    procedure Prepend(const AView: TBLStringView); overload; inline;
    procedure Prepend(const AStr: PUTF8Char; const ASize: NativeInt = -1); overload; inline;
    procedure Prepend(const AStr: UTF8String); overload; inline;
    procedure Prepend(const AStr: String); overload; inline;

    procedure Insert(const AIndex: NativeInt; const AChar: UTF8Char;
      const ACount: NativeInt = 1); overload; inline;
    procedure Insert(const AIndex: NativeInt; const AOther: TBLString); overload; inline;
    procedure Insert(const AIndex: NativeInt; const AView: TBLStringView); overload; inline;
    procedure Insert(const AIndex: NativeInt; const AStr: PUTF8Char;
      const ASize: NativeInt = -1); overload; inline;
    procedure Insert(const AIndex: NativeInt; const AStr: UTF8String); overload; inline;
    procedure Insert(const AIndex: NativeInt; const AStr: String); overload; inline;

    procedure Remove(const AIndex: NativeInt); overload; inline;
    procedure Remove(const ARange: TBLRange); overload; inline;

    /// <summary>
    ///  Returns the first index at which a given character `AChar` can be found
    ///  in the string, or -1 if not present.
    /// </summary>
    function IndexOf(const AChar: UTF8Char): NativeInt; overload; inline;

    /// <summary>
    ///  Returns the index at which a given character `AChar` can be found in
    ///  the string starting from `AFromIndex`, or -1 if not present.
    /// </summary>
    function IndexOf(const AChar: UTF8Char; const AFromIndex: NativeInt): NativeInt; overload; inline;

    /// <summary>
    ///  Returns the last index at which a given character `AChar` can be found
    ///  in the string, or -1 if not present.
    /// </summary>
    function LastIndexOf(const AChar: UTF8Char): NativeInt; overload; inline;

    /// <summary>
    ///  Returns the index at which a given character `AChar` can be found in
    ///  the string starting from `AFromIndex` and ending at `0`, or -1 if not
    ///  present.
    /// </summary>
    function LastIndexOf(const AChar: UTF8Char; const AFromIndex: NativeInt): NativeInt; overload; inline;

    /// <summary>
    ///  Whether the string is empty.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///  The size of the string [in bytes or number of UTF8 characters].
    /// </summary>
    property Size: NativeInt read GetSize;

    /// <summary>
    ///  The capacity of the string [in bytes or number of UTF8 characters].
    /// </summary>
    property Capacity: NativeInt read GetCapacity;

    /// <summary>
    ///  Pointer to the data of the string.
    /// </summary>
    property Data: PUTF8Char read GetData;

    /// <summary>
    ///  The characters at the given `AIndex` (0-based).
    /// </summary>
    /// <remarks>
    ///  This is the same as calling `At(AIndex)`.
    ///  Index must be valid and cannot be out of bounds - there is an assertion.
    /// </remarks>
    property Chars[const AIndex: NativeInt]: UTF8Char read GetChar; default;
  end;

{ ============================================================================
   [Containers - Bit Containers]
  ============================================================================ }

type
  /// <summary>
  ///  BitArray container.
  /// </summary>
  TBLBitArray = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
    function GetSize: Integer; inline;
    function GetWordCount: Integer; inline;
    function GetCapacity: Integer; inline;
    function GetCardinality: Integer; inline;
    function GetData: PUInt32; inline;
    function GetBit(const AIndex: Integer): Boolean; inline;
    procedure SetBit(const AIndex: Integer; const AValue: Boolean); overload; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates an empty bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLBitArray);

    /// <summary>
    ///  Destroys the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLBitArray);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Performs weak copy of the data held by the `ASrc` bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLBitArray; const [ref] ASrc: TBLBitArray); inline;

    /// <summary>
    ///  Used to compare against `nil` (empty bit array).
    /// </summary>
    class operator Equal(const ALeft: TBLBitArray; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two bit arrays are equal (have the same contents).
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLBitArray): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil` (empty bit array).
    /// </summary>
    class operator NotEqual(const ALeft: TBLBitArray; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two bit arrays are not equal (do not have the same contents).
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLBitArray): Boolean; inline; static;

    class operator LessThan(const ALeft, ARight: TBLBitArray): Boolean; inline; static;
    class operator LessThanOrEqual(const ALeft, ARight: TBLBitArray): Boolean; inline; static;
    class operator GreaterThan(const ALeft, ARight: TBLBitArray): Boolean; inline; static;
    class operator GreaterThanOrEqual(const ALeft, ARight: TBLBitArray): Boolean; inline; static;

    /// <summary>
    ///  Clears the content of the bit array and releases its data.
    ///
    ///  After reset the bit array content matches a default constructed instance.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reset; inline;

    /// <summary>
    ///  Swaps the content of this bit array with the `AOther` bit array.
    /// </summary>
    procedure Swap(var AOther: TBLBitArray); inline;

    /// <summary>
    ///  Returns whether this bit array and `AOther` are bitwise equal.
    /// </summary>
    function Equals(const AOther: TBLBitArray): Boolean; inline;

    /// <summary>
    ///  Compares this bit array with `AOther` and returns either `-1`, `0`,
    ///  or `1`.
    /// </summary>
    function Compare(const AOther: TBLBitArray): Integer; inline;

    /// <summary>
    ///  Returns the number of bits set in the given `[AStartBit, AEndBit)` range.
    /// </summary>
    function CardinalityInRange(const AStartBit, AEndBit: Integer): Integer; inline;

    /// <summary>
    ///  Returns a bit-value at the given `ABitIndex`.
    /// </summary>
    function HasBit(const ABitIndex: Integer): Boolean; inline;

    /// <summary>
    ///  Sets a bit to True at the given `ABitIndex`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SetBit(const ABitIndex: Integer); overload; inline;

    /// <summary>
    ///  Sets a bit to False at the given `ABitIndex`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ClearBit(const ABitIndex: Integer); overload; inline;

    /// <summary>
    ///  Replaces a bit in the bit array at the given `ABitIndex` to match
    ///  `ABitValue`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceBit(const ABitIndex: Integer; const ABitValue: Boolean); inline;

    /// <summary>
    ///  Returns whether the bit-set has at least on bit in the given
    ///  `[AStartBit, AEndbit)` range.
    /// </summary>
    function HasBitsInRange(const AStartBit, AEndBit: Integer): Boolean; inline;

    /// <summary>
    ///  Returns whether this bit array subsumes `AOther`.
    /// </summary>
    function Subsumes(const AOther: TBLBitArray): Boolean; inline;

    /// <summary>
    ///  Returns whether this bit array intersects with `AOther`.
    /// </summary>
    function Intersects(const AOther: TBLBitArray): Boolean; inline;

    /// <summary>
    ///  Replaces the content of the bit array by bits specified by `AWordData`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignWords(const AWordData: TArray<UInt32>); overload; inline;

    /// <summary>
    ///  Replaces the content of the bit array by bits specified by `AWordData`
    ///  of size `AWordCount` [the size is in UInt32 units].
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignWords(const AWordData: PUInt32; const AWordCount: Integer); overload; inline;

    /// <summary>
    ///  Clears the content of the bit array without releasing its dynamically
    ///  allocated data, if possible.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Clear; inline;

    /// <summary>
    ///  Resizes the bit array so its size matches `ANumBits`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Resize(const ANumBits: Integer); inline;

    /// <summary>
    ///  Reserves `ANumBits` in the bit array (capacity would match `ANumBits`)
    ///  without changing its size.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reserve(const ANumBits: Integer); inline;

    /// <summary>
    ///  Shrinks the capacity of the bit array to match the actual content with
    ///  the intention to save memory.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Shrink; inline;

    /// <summary>
    ///  Fills bits in `[AStartBit, AEndBit)` range to True.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure FillRange(const AStartBit, AEndBit: Integer); inline;

    /// <summary>
    ///  Fills bits starting from `ABitIndex` specified by `AWordData` to True
    ///  (zeros in AWordData are ignored).
    /// </summary>
    /// <remarks>
    ///  This operation uses an `OR` operator - bits in `AWordData` are combined
    ///  with OR operator with existing bits in bit array.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure FillWords(const ABitIndex: Integer; const AWordData: TArray<UInt32>); overload; inline;

    /// <summary>
    ///  Fills bits starting from `ABitIndex` specified by `AWordData` and
    ///  `AWordCount` to True (zeros in AWordData are ignored).
    /// </summary>
    /// <remarks>
    ///  This operation uses an `OR` operator - bits in `AWordData` are combined
    ///  with OR operator with existing bits in bit array.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure FillWords(const ABitIndex: Integer; const AWordData: PUInt32;
      const AWordCount: UInt32); overload; inline;

    /// <summary>
    ///  Sets bits in `[AStartBit, EndBit)` range to False.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ClearRange(const AStartBit, AEndBit: Integer); inline;

    /// <summary>
    ///  Sets bits starting from `ABitIndex` specified by `AWordValue` to False
    ///  (zeros in ASordValue are ignored).
    /// </summary>
    /// <remarks>
    ///  This operation uses an `AND_NOT` operator - bits in `AWordData` are
    ///  negated and then combined with AND operator with existing bits in
    ///  bit array.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ClearWord(const ABitIndex: Integer; const AWordValue: UInt32); inline;

    /// <summary>
    ///  Sets bits starting from `ABitIndex` specified by `AWordData` to False
    ///  (zeros in AWordData are ignored).
    /// </summary>
    /// <remarks>
    ///  This operation uses an `AND_NOT` operator - bits in `AWordData` are
    ///  negated and then combined with AND operator with existing bits in
    ///  bit array.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ClearWords(const ABitIndex: Integer; const AWordData: TArray<UInt32>); overload; inline;

    /// <summary>
    ///  Sets bits starting from `ABitIndex` specified by `AWordData` and
    ///  `AWordCount` to False (zeros in AWordData are ignored).
    /// </summary>
    /// <remarks>
    ///  This operation uses an `AND_NOT` operator - bits in `AWordData` are
    ///  negated and then combined with AND operator with existing bits in
    ///  bit array.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ClearWords(const ABitIndex: Integer; const AWordData: PUInt32;
      const AWordCount: UInt32); overload; inline;

    /// <summary>
    ///  Makes the bit array mutable with the intention to replace all bits of it.
    /// </summary>
    /// <remarks>
    ///  All bits in the bit array will be set to zero.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    function ReplaceOp(const ANumBits: Integer): PUInt32; inline;

    /// <summary>
    ///  Replaces bits starting from `ABitIndex` to match the bits specified by
    ///  `AWordValue`.
    /// </summary>
    /// <remarks>
    ///  Replaced bits from bit array are not combined by using any operator,
    ///  `AWordValue` is copied as is, thus replaces fully the existing bits.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceWord(const ABitIndex: Integer; const AWordValue: UInt32); inline;

    /// <summary>
    ///  Replaces bits starting from `ABitIndex` to match the bits specified by
    ///  `AWordData`.
    /// </summary>
    /// <remarks>
    ///  Replaced bits from bit array are not combined by using any operator,
    ///  `AWordData` is copied as is, thus replaces fully the existing bits.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceWords(const ABitIndex: Integer; const AWordData: TArray<UInt32>); overload; inline;

    /// <summary>
    ///  Replaces bits starting from `ABitIndex` to match the bits specified by
    ///  `AWordData` and `AWordCount`.
    /// </summary>
    /// <remarks>
    ///  Replaced bits from bit array are not combined by using any operator,
    ///  `AWordData` is copied as is, thus replaces fully the existing bits.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReplaceWords(const ABitIndex: Integer; const AWordData: PUInt32;
      const AWordCount: UInt32); overload; inline;

    /// <summary>
    ///  Appends a bit `ABitValue` to the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendBit(const ABitValue: Boolean); inline;

    /// <summary>
    ///  Appends a single word `AWordValue` to the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendWord(const AWordValue: UInt32); inline;

    /// <summary>
    ///  Appends whole words to the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendWords(const AWordData: TArray<UInt32>); overload; inline;

    /// <summary>
    ///  Appends whole words to the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AppendWords(const AWordData: PUInt32; const AWordCount: Integer); overload; inline;

    /// <summary>
    ///  Whether the bit array is empty (has no content).
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///  The size of the bit array in bits.
    /// </summary>
    property Size: Integer read GetSize;

    /// <summary>
    ///  The bits in the bit array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    property Bits[const AIndex: Integer]: Boolean read GetBit write SetBit; default;

    /// <summary>
    ///  The number of bit words this bit array uses.
    /// </summary>
    property WordCount: Integer read GetWordCount;

    /// <summary>
    ///  The capacity of the bit array in bits.
    /// </summary>
    property Capacity: Integer read GetCapacity;

    /// <summary>
    ///  The number of bits set in the bit array.
    /// </summary>
    property Cardinality: Integer read GetCardinality;

    /// <summary>
    ///  The bit data.
    /// </summary>
    property Data: PUInt32 read GetData;
  end;

{$ENDREGION 'Containers'}

{$REGION 'Geometries'}

{ ============================================================================
   [Geometries - Geometry Enums]
  ============================================================================ }

type
  /// <summary>
  ///  Direction of a geometry used by geometric primitives and paths.
  /// </summary>
  TBLGeometryDirection = (
    /// <summary>
    ///  No direction specified.
    /// </summary>
    None,

    /// <summary>
    ///  Clockwise direction.
    /// </summary>
    CW,

    /// <summary>
    ///  Counter-clockwise direction.
    /// </summary>
    CCW);

type
  /// <summary>
  ///  Geometry type.
  ///
  ///  Geometry describes a shape or path that can be either rendered or added
  ///  to `TBLPath` container. Both `TBLPath` and `TBLContext` provide
  ///  functionality to work with all geometry types. Please note that each type
  ///  provided here requires to pass a matching record to the function that
  ///  consumes `AGeometryType` and `AGeometryData` arguments.
  /// </summary>
  /// <seealso cref="TBLPath"/>
  /// <seealso cref="TBLContext"/>
  TBLGeometryType = (
    /// <summary>
    ///  No geometry provided.
    /// </summary>
    None,

    /// <summary>
    ///  TBLBoxI record.
    /// </summary>
    BoxI,

    /// <summary>
    ///  TBLBox record.
    /// </summary>
    BoxD,

    /// <summary>
    ///  TBLRectI record.
    /// </summary>
    RectI,

    /// <summary>
    ///  TBLRect record.
    /// </summary>
    RectD,

    /// <summary>
    ///  TBLCircle record.
    /// </summary>
    Circle,

    /// <summary>
    ///  TBLEllipse record.
    /// </summary>
    Ellipse,

    /// <summary>
    ///  TBLRoundRect record.
    /// </summary>
    RoundRect,

    /// <summary>
    ///  TBLArc record.
    /// </summary>
    Arc,

    /// <summary>
    ///  TBLArc record representing chord.
    /// </summary>
    Chord,

    /// <summary>
    ///  TBLArc record representing pie.
    /// </summary>
    Pie,

    /// <summary>
    ///  TBLLine record.
    /// </summary>
    Line,

    /// <summary>
    ///  TBLTriangle record.
    /// </summary>
    Triangle,

    /// <summary>
    ///  TBLArrayView<TBLPointI> representing a polyline.
    /// </summary>
    PolylineI,

    /// <summary>
    ///  TBLArrayView<TBLPoint> representing a polyline.
    /// </summary>
    PolylineD,

    /// <summary>
    ///  TBLArrayView<BLPointI> representing a polygon.
    /// </summary>
    PolygonI,

    /// <summary>
    ///  TBLArrayView<BLPoint> representing a polygon.
    /// </summary>
    PolygonD,

    /// <summary>
    ///  TBLArrayView<BLBoxI> record.
    /// </summary>
    ArrayViewBoxI,

    /// <summary>
    ///  TBLArrayView<BLBox> record.
    /// </summary>
    ArrayViewBoxD,

    /// <summary>
    ///  TBLArrayView<BLRectI> record.
    /// </summary>
    ArrayViewRectI,

    /// <summary>
    ///  TBLArrayView<BLRect> record.
    /// </summary>
    ArrayViewRectD,

    /// <summary>
    ///  TBLPath.
    /// </summary>
    Path);

type
  /// <summary>
  ///  Fill rule.
  /// </summary>
  TBLFillRule = (
    /// <summary>
    ///  Non-zero fill-rule.
    /// </summary>
    NonZero,

    /// <summary>
    ///  Even-odd fill-rule.
    /// </summary>
    EvenOdd);

type
  /// <summary>
  ///  Hit-test result.
  /// </summary>
  TBLHitTest = (
    /// <summary>
    ///  Fully in.
    /// </summary>
    FullyIn,

    /// <summary>
    ///  Partially in/out.
    /// </summary>
    Partial,

    /// <summary>
    ///  Fully out.
    /// </summary>
    FullyOut,

    /// <summary>
    ///  Hit test failed (invalid argument, NaNs, etc).
    /// </summary>
    Invalid = $FFFFFFFF);

{ ============================================================================
   [Geometries - Lightweight Geometries and Structs]
  ============================================================================ }

type
  /// <summary>
  ///  Point specified as [X, Y] using `Double` as a storage type.
  /// </summary>
  TBLPoint = record
  public
    X: Double;
    Y: Double;
  public
    constructor Create(const AX, AY: Double);

    class operator Equal(const ALeft, ARight: TBLPoint): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLPoint): Boolean; inline; static;
    class operator Negative(const AValue: TBLPoint): TBLPoint; inline; static;
    class operator Add(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Subtract(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Multiply(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Divide(const ALeft: TBLPoint; const ARight: Double): TBLPoint; inline; static;
    class operator Add(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Subtract(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Multiply(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Divide(const ALeft: Double; const ARight: TBLPoint): TBLPoint; inline; static;
    class operator Add(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;
    class operator Subtract(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;
    class operator Multiply(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;
    class operator Divide(const ALeft, ARight: TBLPoint): TBLPoint; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX, AY: Double); overload; inline;
    procedure Reset(const AOther: TBLPoint); overload; inline;

    function Equals(const AOther: TBLPoint): Boolean; inline;
  end;
  PBLPoint = ^TBLPoint;

type
  /// <summary>
  ///  Adds functionality to TBLPoint
  /// </summary>
  _TBLPointHelper = record helper for TBLPoint
  public const
    Empty: TBLPoint = (X: 0; Y: 0);
  end;

function BLPoint(const AX, AY: Double): TBLPoint; inline;
function BLAbs(const AValue: TBLPoint): TBLPoint; overload; inline;
function BLMin(const AA, AB: TBLPoint): TBLPoint; overload; inline;
function BLMin(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
function BLMin(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
function BLMax(const AA, AB: TBLPoint): TBLPoint; overload; inline;
function BLMax(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
function BLMax(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
function BLClamp(const AA: TBLPoint; const AB, AC: Double): TBLPoint; inline;

type
  /// <summary>
  ///  Point specified as [X, Y] using `Integer` as a storage type.
  /// </summary>
  TBLPointI = record
  public
    X: Integer;
    Y: Integer;
  public
    constructor Create(const AX, AY: Integer);

    class operator Equal(const ALeft, ARight: TBLPointI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLPointI): Boolean; inline; static;
    class operator Negative(const AValue: TBLPointI): TBLPointI; inline; static;
    class operator Add(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Subtract(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Multiply(const ALeft: TBLPointI; const ARight: Integer): TBLPointI; inline; static;
    class operator Add(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Subtract(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Multiply(const ALeft: Integer; const ARight: TBLPointI): TBLPointI; inline; static;
    class operator Add(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;
    class operator Subtract(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;
    class operator Multiply(const ALeft, ARight: TBLPointI): TBLPointI; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX, AY: Integer); overload; inline;
    procedure Reset(const AOther: TBLPointI); overload; inline;

    function Equals(const AOther: TBLPointI): Boolean; inline;
  end;
  PBLPointI = ^TBLPointI;

type
  /// <summary>
  ///  Adds functionality to TBLPointI
  /// </summary>
  _TBLPointIHelper = record helper for TBLPointI
  public const
    Empty: TBLPointI = (X: 0; Y: 0);
  end;

function BLPointI(const AX, AY: Integer): TBLPointI; inline;

type
  /// <summary>
  ///  Size specified as [W, H] using `Double` as a storage type.
  /// </summary>
  TBLSize = record
  public
    W: Double;
    H: Double;
  public
    constructor Create(const AW, AH: Double);

    class operator Equal(const ALeft, ARight: TBLSize): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLSize): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AW, AH: Double); overload; inline;
    procedure Reset(const AOther: TBLSize); overload; inline;

    function Equals(const AOther: TBLSize): Boolean; inline;
  end;
  PBLSize = ^TBLSize;

type
  /// <summary>
  ///  Adds functionality to TBLSize
  /// </summary>
  _TBLSizeHelper = record helper for TBLSize
  public const
    Empty: TBLSize = (W: 0; H: 0);
  end;

function BLSize(const AW, AH: Double): TBLSize; inline;
function BLAbs(const AValue: TBLSize): TBLSize; overload; inline;
function BLMin(const AA, AB: TBLSize): TBLSize; overload; inline;
function BLMax(const AA, AB: TBLSize): TBLSize; overload; inline;

type
  /// <summary>
  ///  Size specified as [W, H] using `Integer` as a storage type.
  /// </summary>
  TBLSizeI = record
  public
    W: Integer;
    H: Integer;
  public
    constructor Create(const AW, AH: Integer);

    class operator Equal(const ALeft, ARight: TBLSizeI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLSizeI): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AW, AH: Integer); overload; inline;
    procedure Reset(const AOther: TBLSizeI); overload; inline;

    function Equals(const AOther: TBLSizeI): Boolean; inline;
  end;
  PBLSizeI = ^TBLSizeI;

type
  /// <summary>
  ///  Adds functionality to TBLSizeI
  /// </summary>
  _TBLSizeIHelper = record helper for TBLSizeI
  public const
    Empty: TBLSizeI = (W: 0; H: 0);
  end;

function BLSizeI(const AW, AH: Integer): TBLSizeI; inline;

type
  /// <summary>
  ///  Box specified as [X0, Y0, X1, Y1] using `Double` as a storage type.
  /// </summary>
  TBLBox = record
  public
    X0: Double;
    Y0: Double;
    X1: Double;
    Y1: Double;
  public
    constructor Create(const AX0, AY0, AX1, AY1: Double);

    class operator Equal(const ALeft, ARight: TBLBox): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLBox): Boolean; inline; static;
    class operator Add(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Subtract(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Multiply(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Divide(const ALeft: TBLBox; const ARight: Double): TBLBox; inline; static;
    class operator Add(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Subtract(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Multiply(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Divide(const ALeft: Double; const ARight: TBLBox): TBLBox; inline; static;
    class operator Add(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Subtract(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Multiply(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Divide(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox; inline; static;
    class operator Add(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;
    class operator Subtract(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;
    class operator Multiply(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;
    class operator Divide(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;
    procedure Reset(const AOther: TBLBox); overload; inline;

    function Equals(const AOther: TBLBox): Boolean; inline;
    function Contains(const AX, AY: Double): Boolean; overload; inline;
    function Contains(const APoint: TBLPoint): Boolean; overload; inline;
  end;
  PBLBox = ^TBLBox;

type
  /// <summary>
  ///  Adds functionality to TBLBox
  /// </summary>
  _TBLBoxHelper = record helper for TBLBox
  public const
    Empty: TBLBox = (X0: 0; Y0: 0; X1: 0; Y1: 0);
  end;

function BLBox(const AX0, AY0, AX1, AY1: Double): TBLBox; inline;

type
  /// <summary>
  ///  Box specified as [X0, Y0, X1, Y1] using `Integer` as a storage type.
  /// </summary>
  TBLBoxI = record
  public
    X0: Integer;
    Y0: Integer;
    X1: Integer;
    Y1: Integer;
  public
    constructor Create(const AX0, AY0, AX1, AY1: Integer);

    class operator Equal(const ALeft, ARight: TBLBoxI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLBoxI): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Integer); overload; inline;
    procedure Reset(const AOther: TBLBoxI); overload; inline;

    function Equals(const AOther: TBLBoxI): Boolean; inline;
    function Contains(const AX, AY: Integer): Boolean; overload; inline;
    function Contains(const APoint: TBLPointI): Boolean; overload; inline;
  end;
  PBLBoxI = ^TBLBoxI;

type
  /// <summary>
  ///  Adds functionality to TBLBoxI
  /// </summary>
  _TBLBoxIHelper = record helper for TBLBoxI
  public const
    Empty: TBLBoxI = (X0: 0; Y0: 0; X1: 0; Y1: 0);
  end;

function BLBoxI(const AX0, AY0, AX1, AY1: Integer): TBLBoxI; inline;

type
  /// <summary>
  ///  Rectangle specified as [X, Y, W, H] using `Double` as a storage type.
  /// </summary>
  TBLRect = record
  public
    X: Double;
    Y: Double;
    W: Double;
    H: Double;
  public
    constructor Create(const AX, AY, AW, AH: Double);

    class operator Equal(const ALeft, ARight: TBLRect): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRect): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX, AY, AW, AH: Double); overload; inline;
    procedure Reset(const AOther: TBLRect); overload; inline;

    function Equals(const AOther: TBLRect): Boolean; inline;
  end;
  PBLRect = ^TBLRect;

type
  /// <summary>
  ///  Adds functionality to TBLRect
  /// </summary>
  _TBLRectHelper = record helper for TBLRect
  public const
    Empty: TBLRect = (X: 0; Y: 0; W: 0; H: 0);
  end;

function BLRect(const AX, AY, AW, AH: Double): TBLRect; inline;

type
  /// <summary>
  ///  Rectangle specified as [X, Y, W, H] using `Integer` as a storage type.
  /// </summary>
  TBLRectI = record
  public
    X: Integer;
    Y: Integer;
    W: Integer;
    H: Integer;
  public
    constructor Create(const AX, AY, AW, AH: Integer);

    class operator Equal(const ALeft, ARight: TBLRectI): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRectI): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX, AY, AW, AH: Integer); overload; inline;
    procedure Reset(const AOther: TBLRectI); overload; inline;

    function Equals(const AOther: TBLRectI): Boolean; inline;
  end;
  PBLRectI = ^TBLRectI;

type
  /// <summary>
  ///  Adds functionality to TBLRectI
  /// </summary>
  _TBLRectIHelper = record helper for TBLRectI
  public const
    Empty: TBLRectI = (X: 0; Y: 0; W: 0; H: 0);
  end;

function BLRectI(const AX, AY, AW, AH: Integer): TBLRectI; inline;

type
  /// <summary>
  ///  Rounded rectangle specified as [X, Y, W, H, RX, RY] using `Double` as a
  ///  storage type.
  /// </summary>
  TBLRoundRect = record
  public
    X: Double;
    Y: Double;
    W: Double;
    H: Double;
    RX: Double;
    RY: Double;
  public
    constructor Create(const ARect: TBLRect; const AR: Double); overload;
    constructor Create(const ARect: TBLRect; const ARX, ARY: Double); overload;
    constructor Create(const AX, AY, AW, AH, AR: Double); overload;
    constructor Create(const AX, AY, AW, AH, ARX, ARY: Double); overload;

    class operator Equal(const ALeft, ARight: TBLRoundRect): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRoundRect): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ARect: TBLRect; const AR: Double); overload; inline;
    procedure Reset(const ARect: TBLRect; const ARX, ARY: Double); overload; inline;
    procedure Reset(const AX, AY, AW, AH, AR: Double); overload; inline;
    procedure Reset(const AX, AY, AW, AH, ARX, ARY: Double); overload; inline;
    procedure Reset(const AOther: TBLRoundRect); overload; inline;

    function Equals(const AOther: TBLRoundRect): Boolean; inline;
  end;
  PBLRoundRect = ^TBLRoundRect;

type
  /// <summary>
  ///  Adds functionality to TBLRoundRect
  /// </summary>
  _TBLRoundRectHelper = record helper for TBLRoundRect
  public const
    Empty: TBLRoundRect = (X: 0; Y: 0; W: 0; H: 0; RX: 0; RY: 0);
  end;

function BLRoundRect(const ARect: TBLRect; const AR: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const ARect: TBLRect; const ARX, ARY: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const AX, AY, AW, AH, AR: Double): TBLRoundRect; overload; inline;
function BLRoundRect(const AX, AY, AW, AH, ARX, ARY: Double): TBLRoundRect; overload; inline;

type
  /// <summary>
  ///  Circle specified as [CX, CY, R] using `Double` as a storage type.
  /// </summary>
  TBLCircle = record
  public
    CX: Double;
    CY: Double;
    R: Double;
  public
    constructor Create(const ACX, ACY, AR: Double);

    class operator Equal(const ALeft, ARight: TBLCircle): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLCircle): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, AR: Double); overload; inline;
    procedure Reset(const AOther: TBLCircle); overload; inline;

    function Equals(const AOther: TBLCircle): Boolean; inline;
  end;
  PBLCircle = ^TBLCircle;

type
  /// <summary>
  ///  Adds functionality to TBLCircle
  /// </summary>
  _TBLCircleHelper = record helper for TBLCircle
  public const
    Empty: TBLCircle = (CX: 0; CY: 0; R: 0);
  end;

function BLCircle(const ACX, ACY, AR: Double): TBLCircle; inline;

type
  /// <summary>
  ///  Ellipse specified as [CX, CY, RX, RY] using `Double` as a storage type.
  /// </summary>
  TBLEllipse = record
  public
    CX: Double;
    CY: Double;
    RX: Double;
    RY: Double;
  public
    constructor Create(const ACX, ACY, AR: Double); overload;
    constructor Create(const ACX, ACY, ARX, ARY: Double); overload;

    class operator Equal(const ALeft, ARight: TBLEllipse): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLEllipse): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, AR: Double); overload; inline;
    procedure Reset(const ACX, ACY, ARX, ARY: Double); overload; inline;
    procedure Reset(const AOther: TBLEllipse); overload; inline;

    function Equals(const AOther: TBLEllipse): Boolean; inline;
  end;
  PBLEllipse = ^TBLEllipse;

type
  /// <summary>
  ///  Adds functionality to TBLEllipse
  /// </summary>
  _TBLEllipseHelper = record helper for TBLEllipse
  public const
    Empty: TBLEllipse = (CX: 0; CY: 0; RX: 0; RY: 0);
  end;

function BLEllipse(const ACX, ACY, AR: Double): TBLEllipse; overload; inline;
function BLEllipse(const ACX, ACY, ARX, ARY: Double): TBLEllipse; overload; inline;

type
  /// <summary>
  ///  Arc specified as [CX, CY, RX, RY, Start, Sweep] using `Double` as a
  ///  storage type.
  /// </summary>
  TBLArc = record
  public
    CX: Double;
    CY: Double;
    RX: Double;
    RY: Double;
    Start: Double;
    Sweep: Double;
  public
    constructor Create(const ACX, ACY, ARX, ARY, AStart, ASweep: Double);

    class operator Equal(const ALeft, ARight: TBLArc): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLArc): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ACX, ACY, ARX, ARY, AStart, ASweep: Double); overload; inline;
    procedure Reset(const AOther: TBLArc); overload; inline;

    function Equals(const AOther: TBLArc): Boolean; inline;
  end;
  PBLArc = ^TBLArc;

type
  /// <summary>
  ///  Adds functionality to TBLArc
  /// </summary>
  _TBLArcHelper = record helper for TBLArc
  public const
    Empty: TBLArc = (CX: 0; CY: 0; RX: 0; RY: 0; Start: 0; Sweep: 0);
  end;

function BLArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double): TBLArc; inline;

type
  /// <summary>
  ///  Line specified as [X0, Y0, X1, Y1] using `Double` as a storage type.
  /// </summary>
  TBLLine = record
  public
    X0: Double;
    Y0: Double;
    X1: Double;
    Y1: Double;
  public
    constructor Create(const AX0, AY0, AX1, AY1: Double);

    class operator Equal(const ALeft, ARight: TBLLine): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLLine): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;
    procedure Reset(const AOther: TBLLine); overload; inline;

    function Equals(const AOther: TBLLine): Boolean; inline;
  end;
  PBLLine = ^TBLLine;

type
  /// <summary>
  ///  Adds functionality to TBLLine
  /// </summary>
  _TBLLineHelper = record helper for TBLLine
  public const
    Empty: TBLLine = (X0: 0; Y0: 0; X1: 0; Y1: 0);
  end;

function BLLine(const AX0, AY0, AX1, AY1: Double): TBLLine; inline;

type
  /// <summary>
  ///  Triangle data specified as [X0, Y0, X1, Y1, X2, Y2] using `Double` as a
  ///  storage type.
  /// </summary>
  TBLTriangle = record
  public
    X0: Double;
    Y0: Double;
    X1: Double;
    Y1: Double;
    X2: Double;
    Y2: Double;
  public
    constructor Create(const AX0, AY0, AX1, AY1, AX2, AY2: Double);

    class operator Equal(const ALeft, ARight: TBLTriangle): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLTriangle): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1, AX2, AY2: Double); overload; inline;
    procedure Reset(const AOther: TBLTriangle); overload; inline;

    function Equals(const AOther: TBLTriangle): Boolean; inline;
  end;
  PBLTriangle = ^TBLTriangle;

type
  /// <summary>
  ///  Adds functionality to TBLTriangle
  /// </summary>
  _TBLTriangleHelper = record helper for TBLTriangle
  public const
    Empty: TBLTriangle = (X0: 0; Y0: 0; X1: 0; Y1: 0; X2: 0; Y2: 0);
  end;

function BLTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double): TBLTriangle; inline;

{ ============================================================================
   [Geometries - Transformations]
  ============================================================================ }

type
  /// <summary>
  ///  Transformation matrix operation type.
  /// </summary>
  TBLTransformOp = (
    /// <summary>
    ///  Reset matrix to identity (argument ignored, should be nil).
    /// </summary>
    Reset,

    /// <summary>
    ///  Assign (copy) the other matrix.
    /// </summary>
    Assign,

    /// <summary>
    ///  Translate the matrix by [X, Y].
    /// </summary>
    Translate,

    /// <summary>
    ///  Scale the matrix by [X, Y].
    /// </summary>
    Scale,

    /// <summary>
    ///  Skew the matrix by [X, Y].
    /// </summary>
    Skew,

    /// <summary>
    ///  Rotate the matrix by the given angle about [0, 0].
    /// </summary>
    Rotate,

    /// <summary>
    ///  Rotate the matrix by the given angle about [X, Y].
    /// </summary>
    RotatePoint,

    /// <summary>
    ///  Transform this matrix by other `TBLMatrix2D`.
    /// </summary>
    /// <seealso cref="TBLMatrix2D"/>
    Transform,

    /// <summary>
    ///  Post-translate the matrix by [X, Y].
    /// </summary>
    PostTranslate,

    /// <summary>
    ///  Post-scale the matrix by [X, Y].
    /// </summary>
    PostScale,

    /// <summary>
    ///  Post-skew the matrix by [X, Y].
    /// </summary>
    PostSkew,

    /// <summary>
    ///  Post-rotate the matrix about [0, 0].
    /// </summary>
    PostRotate,

    /// <summary>
    ///  Post-rotate the matrix about a reference `TBLPoint`.
    /// </summary>
    /// <seealso cref="TBLPoint"/>
    PostRotatePoint,

    /// <summary>
    ///  Post-transform this matrix by other `TBLMatrix2D`.
    /// </summary>
    /// <seealso cref="TBLMatrix2D"/>
    PostTransform);

type
  /// <summary>
  ///  Transformation matrix type that can be obtained by calling
  ///  `TBLMatrix2D.Kind`.
  ///
  ///  ```
  ///   Identity  Transl.  Scale     Swap    Affine
  ///    [1  0]   [1  0]   [.  0]   [0  .]   [.  .]
  ///    [0  1]   [0  1]   [0  .]   [.  0]   [.  .]
  ///    [0  0]   [.  .]   [.  .]   [.  .]   [.  .]
  ///  ```
  /// </summary>
  TBLTransformKind = (
    /// <summary>
    ///  Identity matrix.
    /// </summary>
    Identity,

    /// <summary>
    ///  Has translation part (the rest is like identity).
    /// </summary>
    Translate,

    /// <summary>
    ///  Has translation and scaling parts.
    /// </summary>
    Scale,

    /// <summary>
    ///  Has translation and scaling parts, however scaling swaps X/Y.
    /// </summary>
    Swap,

    /// <summary>
    ///  Generic affine matrix.
    /// </summary>
    Affine,

    /// <summary>
    ///  Invalid/degenerate matrix not useful for transformations.
    /// </summary>
    Invalid);

type
  /// <summary>
  ///  2D matrix represents an affine transformation matrix that can be used to
  ///  transform geometry and images.
  /// </summary>
  TBLMatrix2D = record
  {$REGION 'Internal Declarations'}
  private
    function GetKind: TBLTransformKind; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    M: array [0..5] of Double;
  public
    constructor Create(const AM00, AM01, AM10, AM11, AM20, AM21: Double);

    class operator Equal(const ALeft, ARight: TBLMatrix2D): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLMatrix2D): Boolean; inline; static;

    function Equals(const AOther: TBLMatrix2D): Boolean; inline;

    /// <summary>
    ///  Creates a new matrix initialized to identity.
    /// </summary>
    class function MakeIdentity: TBLMatrix2D; inline; static;

    /// <summary>
    ///  Creates a new matrix initialized to translation.
    /// </summary>
    class function MakeTranslation(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeTranslation(const AP: TBLPoint): TBLMatrix2D; overload; inline; static;
    class function MakeTranslation(const AP: TBLPointI): TBLMatrix2D; overload; inline; static;

    /// <summary>
    ///  Creates a new matrix initialized to scaling.
    /// </summary>
    class function MakeScaling(const AXY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AP: TBLPoint): TBLMatrix2D; overload; inline; static;
    class function MakeScaling(const AP: TBLPointI): TBLMatrix2D; overload; inline; static;

    /// <summary>
    ///  Creates a new matrix initialized to rotation.
    /// </summary>
    class function MakeRotation(const AAngle: Double): TBLMatrix2D; overload; inline; static;
    class function MakeRotation(const AAngle, AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeRotation(const AAngle: Double; const AOrigin: TBLPoint): TBLMatrix2D; overload; inline; static;

    /// <summary>
    ///  Create a new skewing matrix.
    /// </summary>
    class function MakeSkewing(const AX, AY: Double): TBLMatrix2D; overload; inline; static;
    class function MakeSkewing(const AOrigin: TBLPoint): TBLMatrix2D; overload; inline; static;

    class function MakeSinCos(const ASin, ACos: Double; const ATX: Double = 0;
      const ATY: Double = 0): TBLMatrix2D; overload; inline; static;
    class function MakeSinCos(const ASin, ACos: Double; const AP: TBLPoint): TBLMatrix2D; overload; inline; static;

    /// <summary>
    ///  Resets matrix to identity.
    /// </summary>
    procedure Reset; overload; inline;

    /// <summary>
    ///  Resets matrix to `AOther` (copy its content to this matrix).
    /// </summary>
    procedure Reset(const AOther: TBLMatrix2D); overload; inline;

    /// <summary>
    ///  Resets matrix to `[AM00, AM01, AM10, AM11, AM20, AM21]`.
    /// </summary>
    procedure Reset(const AM00, AM01, AM10, AM11, AM20, AM21: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to translation.
    /// </summary>
    procedure ResetToTranslation(const AX, AY: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to translation.
    /// </summary>
    procedure ResetToTranslation(const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Resets matrix to translation.
    /// </summary>
    procedure ResetToTranslation(const AP: TBLPointI); overload; inline;

    /// <summary>
    ///  Resets matrix to scaling.
    /// </summary>
    procedure ResetToScaling(const AXY: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to scaling.
    /// </summary>
    procedure ResetToScaling(const AX, AY: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to scaling.
    /// </summary>
    procedure ResetToScaling(const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Resets matrix to scaling.
    /// </summary>
    procedure ResetToScaling(const AP: TBLPointI); overload; inline;

    /// <summary>
    ///  Resets matrix to skewing.
    /// </summary>
    procedure ResetToSkewing(const AX, AY: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to skewing.
    /// </summary>
    procedure ResetToSkewing(const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Resets matrix to rotation specified by `ASin` and `ACos` and optional
    ///  translation `ATX` and `ATY`.
    /// </summary>
    procedure ResetToSinCos(const ASin, ACos: Double; const ATX: Double = 0;
      const ATY: Double = 0); overload; inline;

    /// <summary>
    ///  Resets matrix to rotation specified by `ASin` and `ACos` and
    ///  translation `AP`.
    /// </summary>
    procedure ResetToSinCos(const ASin, ACos: Double; const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Resets matrix to rotation.
    /// </summary>
    procedure ResetToRotation(const AAngle: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to rotation around a point `[X, Y]`.
    /// </summary>
    procedure ResetToRotation(const AAngle, AX, AY: Double); overload; inline;

    /// <summary>
    ///  Resets matrix to rotation around a point `p`.
    /// </summary>
    procedure ResetToRotation(const AAngle: Double; const AOrigin: TBLPoint); overload; inline;

    /// <summary>
    ///  Calculates the matrix determinant.
    /// </summary>
    function Determinant: Double; inline;

    procedure Translate(const AX, AY: Double); overload; inline;
    procedure Translate(const AP: TBLPoint); overload; inline;
    procedure Translate(const AP: TBLPointI); overload; inline;

    procedure Scale(const AXY: Double); overload; inline;
    procedure Scale(const AX, AY: Double); overload; inline;
    procedure Scale(const AP: TBLPoint); overload; inline;
    procedure Scale(const AP: TBLPointI); overload; inline;

    procedure Skew(const AX, AY: Double); overload; inline;
    procedure Skew(const AP: TBLPoint); overload; inline;

    procedure Rotate(const AAngle: Double); overload; inline;
    procedure Rotate(const AAngle, AX, AY: Double); overload; inline;
    procedure Rotate(const AAngle: Double; const AP: TBLPoint); overload; inline;
    procedure Rotate(const AAngle: Double; const AP: TBLPointI); overload; inline;

    procedure Transform(const AM: TBLMatrix2D); inline;

    procedure PostTranslate(const AX, AY: Double); overload; inline;
    procedure PostTranslate(const AP: TBLPoint); overload; inline;
    procedure PostTranslate(const AP: TBLPointI); overload; inline;

    procedure PostScale(const AXY: Double); overload; inline;
    procedure PostScale(const AX, AY: Double); overload; inline;
    procedure PostScale(const AP: TBLPoint); overload; inline;
    procedure PostScale(const AP: TBLPointI); overload; inline;

    procedure PostSkew(const AX, AY: Double); overload; inline;
    procedure PostSkew(const AP: TBLPoint); overload; inline;

    procedure PostRotate(const AAngle: Double); overload; inline;
    procedure PostRotate(const AAngle, AX, AY: Double); overload; inline;
    procedure PostRotate(const AAngle: Double; const AP: TBLPoint); overload; inline;
    procedure PostRotate(const AAngle: Double; const AP: TBLPointI); overload; inline;

    procedure PostTransform(const AM: TBLMatrix2D); inline;

    /// <summary>
    ///  Inverts the matrix.
    ///  Returns True if the matrix has been inverted successfully.
    /// </summary>
    function Invert: Boolean; overload; inline;

    /// <summary>
    ///  Inverts `ASrc` matrix and stores the result in `ADst`.
    ///  Returns True if the matrix has been inverted successfully.
    /// </summary>
    class function Invert(const ASrc: TBLMatrix2D; out ADst: TBLMatrix2D): Boolean; overload; inline; static;

    function MapPoint(const AX, AY: Double): TBLPoint; overload; inline;
    function MapPoint(const AP: TBLPoint): TBLPoint; overload; inline;

    function MapVector(const AX, AY: Double): TBLPoint; overload; inline;
    function MapVector(const AP: TBLPoint): TBLPoint; overload; inline;

    /// <summary>
    ///  Element [0, 0]. Contains X scaling.
    /// </summary>
    property M00: Double read M[0] write M[0];

    /// <summary>
    ///  Element [0, 1]. Contains rotation and skewing.
    /// </summary>
    property M01: Double read M[1] write M[1];

    /// <summary>
    ///  Element [1, 0]. Contains rotation and skewing.
    /// </summary>
    property M10: Double read M[2] write M[2];

    /// <summary>
    ///  Element [1, 1]. Contains Y scaling.
    /// </summary>
    property M11: Double read M[3] write M[3];

    /// <summary>
    ///  Element [2, 0]. Contains X translation.
    /// </summary>
    property M20: Double read M[4] write M[4];

    /// <summary>
    ///  Element [2, 1]. Contains Y translation.
    /// </summary>
    property M21: Double read M[5] write M[5];

    /// <summary>
    ///  The matrix kind.
    /// </summary>
    property Kind: TBLTransformKind read GetKind;
  end;
  PBLMatrix2D = ^TBLMatrix2D;

type
  /// <summary>
  ///  Adds functionality to TBLMatrix2D
  /// </summary>
  _TBLMatrix2DHelper = record helper for TBLMatrix2D
  public const
    Identity: TBLMatrix2D = (M: (1, 0, 0, 1, 0, 0));
  end;

{ ============================================================================
   [Geometries - Path Operations]
  ============================================================================ }

type
  /// <summary>
  ///  Mode that specifies how to construct offset curves.
  /// </summary>
  TBLOffsetMode = (
    /// <summary>
    ///  Use default mode (decided by Blend2D).
    /// </summary>
    Default,

    /// <summary>
    ///  Iterative offset construction.
    /// </summary>
    Iterative);

type
  /// <summary>
  ///  Mode that specifies how curves are approximated to line segments.
  /// </summary>
  TBLFlattenMode = (
    /// <summary>
    ///  Use default mode (decided by Blend2D).
    /// </summary>
    Default,

    /// <summary>
    ///  Recursive subdivision flattening.
    /// </summary>
    Recursive);

type
  /// <summary>
  ///  A presentation attribute defining the shape to be used at the end of open
  ///  sub-paths.
  /// </summary>
  TBLStrokeCap = (
    /// <summary>
    ///  Butt cap [default].
    /// </summary>
    Butt,

    /// <summary>
    ///  Square cap.
    /// </summary>
    Square,

    /// <summary>
    ///  Round cap.
    /// </summary>
    Round,

    /// <summary>
    ///  Round cap reversed.
    /// </summary>
    RoundRev,

    /// <summary>
    ///  Triangle cap.
    /// </summary>
    Triangle,

    /// <summary>
    ///  Triangle cap reversed.
    /// </summary>
    TriangleRev);

type
  /// <summary>
  ///  Position of a stroke-cap.
  /// </summary>
  TBLStrokeCapPosition = (
    /// <summary>
    ///  Start of the path.
    /// </summary>
    StartOfPath,

    /// <summary>
    ///  End of the path.
    /// </summary>
    EndOfPath);

type
  /// <summary>
  ///  Stroke join type.
  /// </summary>
  TBLStrokeJoin = (
    /// <summary>
    ///  Miter-join possibly clipped at `MiterLimit` [default].
    /// </summary>
    MiterClip,

    /// <summary>
    ///  Miter-join or bevel-join depending on miterLimit condition.
    /// </summary>
    MiterBevel,

    /// <summary>
    ///  Miter-join or round-join depending on miterLimit condition.
    /// </summary>
    MiterRound,

    /// <summary>
    ///  Bevel-join.
    /// </summary>
    Bevel,

    /// <summary>
    ///  Round-join.
    /// </summary>
    Round);

type
  /// <summary>
  ///  Stroke transform order.
  /// </summary>
  TBLStrokeTransformOrder = (
    /// <summary>
    ///  Transform after stroke  => `Transform(Stroke(Input))` [default].
    /// </summary>
    After,

    /// <summary>
    ///  Transform before stroke => `Stroke(Transform(Input))`.
    /// </summary>
    Before);

type
  /// <summary>
  ///  Stroke options.
  /// </summary>
  TBLStrokeOptions = record
  {$REGION 'Internal Declarations'}
  private type
    TValues = packed record
    case Byte of
      0: (StartCap: UInt8;
          EndCap: UInt8;
          Join: UInt8;
          TransformOrder: UInt8;
          Reserved: array [0..3] of UInt8);
      1: (Caps: array [TBLStrokeCapPosition] of UInt8);
      2: (Hints: UInt64);
    end;
  private
    FValues: TValues;
    FWidth: Double;
    FMiterLimit: Double;
    FDashOffset: Double;
    FDashArray: TBLArray<Double>;
    function GetStartCap: TBLStrokeCap; inline;
    procedure SetStartCap(const AValue: TBLStrokeCap); inline;
    function GetEndCap: TBLStrokeCap; inline;
    procedure SetEndCap(const AValue: TBLStrokeCap); inline;
    function GetCap(const AIndex: TBLStrokeCapPosition): TBLStrokeCap; inline;
    procedure SetCap(const AIndex: TBLStrokeCapPosition;
      const AValue: TBLStrokeCap); inline;
    function GetJoin: TBLStrokeJoin; inline;
    procedure SetJoin(const AValue: TBLStrokeJoin); inline;
    function GetTransformOrder: TBLStrokeTransformOrder; inline;
    procedure SetTransformOrder(const AValue: TBLStrokeTransformOrder); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed stroke options object.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLStrokeOptions);

    /// <summary>
    ///  Destroys the stroke options object.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLStrokeOptions);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Creates a weak-copy of the `ASrc`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLStrokeOptions; const [ref] ASrc: TBLStrokeOptions); inline;

    class operator Equal(const ALeft, ARight: TBLStrokeOptions): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLStrokeOptions): Boolean; inline; static;

    function Equals(const AOther: TBLStrokeOptions): Boolean; inline;

    procedure Reset; inline;
    procedure SetCaps(const ACap: TBLStrokeCap); inline;

    property StartCap: TBLStrokeCap read GetStartCap write SetStartCap;
    property EndCap: TBLStrokeCap read GetEndCap write SetEndCap;
    property Caps[const AIndex: TBLStrokeCapPosition]: TBLStrokeCap read GetCap write SetCap;
    property Join: TBLStrokeJoin read GetJoin write SetJoin;
    property TransformOrder: TBLStrokeTransformOrder read GetTransformOrder write SetTransformOrder;
    property Width: Double read FWidth write FWidth;
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property DashOffset: Double read FDashOffset write FDashOffset;
    property DashArray: TBLArray<Double> read FDashArray write FDashArray;
  end;

{ ============================================================================
   [Geometries - Paths]
  ============================================================================ }

type
  /// <summary>
  ///  Path command.
  /// </summary>
  TBLPathCmd = (
    /// <summary>
    ///  Move-to command (starts a new figure).
    /// </summary>
    Move,

    /// <summary>
    ///  On-path command (interpreted as line-to or the end of a curve).
    /// </summary>
    OnPath,

    /// <summary>
    ///  Quad-to control point.
    /// </summary>
    Quad,

    /// <summary>
    ///  Conic-to control point
    /// </summary>
    Conic,

    /// <summary>
    ///  Cubic-to control point (always used as a pair of commands).
    /// </summary>
    Cubic,

    /// <summary>
    ///  Close path.
    /// </summary>
    Close,

    /// <summary>
    ///  Conic weight.
    /// </summary>
    /// <remarks>
    ///  This is not a point. This is a pair of values from which only the first
    ///  (X) is used to represent weight as used by conic curve. The other value
    ///  (Y) is always set to NaN by Blend2D, but can be arbitrary as it has
    ///  no meaning.
    /// </remarks>
    Weight,

    /// <summary>
    ///  Used by `TBLPath.SetVertexAt` to preserve the current command value.
    /// </summary>
    /// <remarks>
    ///  This command is never stored in the path.
    /// </remarks>
    Preserve = $FFFFFFFF);

type
  /// <summary>
  ///  Path flags.
  /// </summary>
  TBLPathFlag = (
    /// <summary>
    ///  Path is empty (no commands or close commands only).
    /// </summary>
    Empty = 0,

    /// <summary>
    ///  Path contains multiple figures.
    /// </summary>
    Multiple = 1,

    /// <summary>
    ///  Path contains one or more quad curves.
    /// </summary>
    Quads = 2,

    /// <summary>
    ///  Path contains one or more conic curves.
    /// </summary>
    Conics = 3,

    /// <summary>
    ///  Path contains one or more cubic curves.
    /// </summary>
    Cubics = 4,

    /// <summary>
    ///  Path is invalid.
    /// </summary>
    Invalid = 30,

    /// <summary>
    ///  Flags are dirty (not reflecting the current status).
    /// </summary>
    Dirty = 31);

type
  /// <summary>
  ///  Path flags.
  /// </summary>
  TBLPathFlags = set of TBLPathFlag;

type
  /// <summary>
  ///  Adds functionality to TBLPathFlags.
  /// </summary>
  _TBLPathFlagsHelper = record helper for TBLPathFlags
  public const
    None = [];
  end;

type
  /// <summary>
  ///  Path reversal mode.
  /// </summary>
  TBLPathReverseMode = (
    /// <summary>
    ///  Reverse each figure and their order as well (default).
    /// </summary>
    Complete,

    /// <summary>
    ///  Reverse each figure separately (keeps their order).
    /// </summary>
    Separate);

type
  /// <summary>
  ///  Options used to describe how geometry is approximated.
  ///
  ///  This record cannot be simply zeroed and then passed to functions that
  ///  accept approximation options. Use `TBLApproximationOptions.Default` to
  ///  setup defaults and then alter values you want to change.
  ///
  ///  Example of using `TBLApproximationOptions`:
  ///
  ///  ```
  ///  // Initialize with defaults first.
  ///  var Approx := TBLApproximationOptions.Default;
  ///
  ///  // Override values you want to change.
  ///  Ppprox.SimplifyTolerance = 0.02;
  ///
  ///  // ... now safely use approximation options in your code ...
  ///  ```
  /// </summary>
  TBLApproximationOptions = packed record
  {$REGION 'Internal Declarations'}
  private
    FFlattenMode: UInt8;
    FOffsetMode: UInt8;
    FReservedFlags: array [0..5] of UInt8;
    FFlattenTolerance: Double;
    FSimplifyTolerance: Double;
    FOffsetParameter: Double;
    function GetFlattenMode: TBLFlattenMode; inline;
    procedure SetFlattenMode(const AValue: TBLFlattenMode); inline;
    function GetOffsetMode: TBLOffsetMode; inline;
    procedure SetOffsetMode(const AValue: TBLOffsetMode); inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Specifies how curves are flattened.
    /// </summary>
    property FlattenMode: TBLFlattenMode read GetFlattenMode write SetFlattenMode;

    /// <summary>
    ///  Specifies how curves are offsetted (used by stroking).
    /// </summary>
    property OffsetMode: TBLOffsetMode read GetOffsetMode write SetOffsetMode;

    /// <summary>
    ///  Tolerance used to flatten curves.
    /// </summary>
    property FlattenTolerance: Double read FFlattenTolerance write FFlattenTolerance;

    /// <summary>
    ///  Tolerance used to approximate cubic curves with quadratic curves.
    /// </summary>
    property SimplifyTolerance: Double read FSimplifyTolerance write FSimplifyTolerance;

    /// <summary>
    ///  Curve offsetting parameter, exact meaning depends on `offsetMode`.
    /// </summary>
    property OffsetParameter: Double read FOffsetParameter write FOffsetParameter;
  end;

type
  /// <summary>
  ///  Adds functionality to 2D vector path.
  /// </summary>
  _TBLApproximationOptionsHelper = record helper for TBLApproximationOptions
  public const
    /// <summary>
    ///  Default approximation options used by Blend2D.
    /// </summary>
    Default: TBLApproximationOptions = (
      FFlattenMode: 0;
      FOffsetMode: 0;
      FFlattenTolerance: 0.2;
      FSimplifyTolerance: 0.05;
      FOffsetParameter: 0.414213562);
  end;

type
  /// <summary>
  ///  2D vector path view provides pointers to vertex and command data along
  ///  with their size.
  /// </summary>
  TBLPathView = record
  {$REGION 'Internal Declarations'}
  private
    FCommandData: PByte;
    FVertexData: PBLPoint;
    FSize: NativeInt;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure Reset; overload; inline;
    procedure Reset(const ACommandDataIn: PByte; const AVertexDataIn: PBLPoint;
      const ASizeIn: NativeInt); overload; inline;

    property CommandData: PByte read FCommandData;
    property VertexData: PBLPoint read FVertexData;
    property Size: NativeInt read FSize;
  end;

type
  /// <summary>
  ///  2D vector path.
  /// </summary>
  TBLPath = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
    function GetSize: NativeInt; inline;
    function GetCapacity: NativeInt; inline;
    function GetVertexData: PBLPoint; inline;
    function GetVertexDataEnd: PBLPoint; inline;
    function GetCommandData: PByte; inline;
    function GetCommandDataEnd: PByte; inline;
    function GetInfoFlags: TBLPathFlags; inline;
    function GetControlBox: TBLBox; inline;
    function GetBoundingBox: TBLBox; inline;
    function GetFigureRange(const AIndex: Integer): TBLRange; inline;
    function GetLastVertex: TBLPoint; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLPath);

    /// <summary>
    ///  Destroys the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLPath);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Creates a weak-copy of the `ASrc` path by increasing it's internal
    ///  reference counter. This path and `ASrc` would point to the same data
    ///  and would be otherwise identical. Any change to `ASrc` would also
    ///  affect this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLPath; const [ref] ASrc: TBLPath); inline;

    /// <summary>
    ///  Used to compare against `nil` (empty path).
    /// </summary>
    class operator Equal(const ALeft: TBLPath; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two paths are equal (have the same contents).
    /// </summary>
    /// <seealso cref="Equals"/>
    class operator Equal(const ALeft, ARight: TBLPath): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil` (empty path).
    /// </summary>
    class operator NotEqual(const ALeft: TBLPath; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two paths are not equal (do not have the same contents).
    /// </summary>
    /// <seealso cref="Equals"/>
    class operator NotEqual(const ALeft, ARight: TBLPath): Boolean; inline; static;

    procedure Reset; inline;
    procedure Swap(var AOther: TBLPath); inline;

    /// <summary>
    ///  Returns a read-only path data as `TBLPathView`.
    /// </summary>
    function View: TBLPathView; inline;

    /// <summary>
    ///  Clears the content of the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Clear; inline;

    /// <summary>
    ///  Shrinks the capacity of the path to fit the current usage.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Shrink; inline;

    /// <summary>
    ///  Reserves the capacity of the path for at least `AMinCapacity` vertices
    ///  and commands.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reserve(const AMinCapacity: NativeInt); inline;

    procedure ModifyOp(const AOp: TBLModifyOp; const ASize: NativeInt;
      out ACmdDataOut: PByte; out AVertexDataOut: PBLPoint); inline;

    procedure AssignDeep(const AOther: TBLPath); inline;

    /// <summary>
    ///  Sets vertex at `AIndex` to `ACmd` and `APt`.
    ///
    ///  Pass `TBLPathCmd.Preserve` in `ACmd` to preserve the current command.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SetVertexAt(const AIndex: NativeInt; const ACmd: TBLPathCmd;
      const APt: TBLPoint); overload; inline;

    /// <summary>
    ///  Sets vertex at `AIndex` to `ACmd` and `[AX, AY]`.
    ///
    ///  Pass `TBLPathCmd.Preserve` in `ACmd` to preserve the current command.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SetVertexAt(const AIndex: NativeInt; const ACmd: TBLPathCmd;
      const AX, AY: Double); overload; inline;

    /// <summary>
    ///  Moves to `AP0`.
    ///
    ///  Appends `TBLPathCmd.Move[AP0]` command to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure MoveTo(const AP0: TBLPoint); overload; inline;

    /// <summary>
    ///  Moves to `[AX0, AY0]`.
    ///
    ///  Appends `TBLPathCmd.Move[AX0, AY0]` command to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure MoveTo(const AX0, AY0: Double); overload; inline;

    /// <summary>
    ///  Adds line to `AP1`.
    ///
    ///  Appends `TBLPathCmd.OnPath[AP1]` command to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure LineTo(const AP1: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds line to `[AX1, AY1]`.
    ///
    ///  Appends `TBLPathCmd.OnPath[AX1, AY1]` command to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure LineTo(const AX1, AY1: Double); overload; inline;

    /// <summary>
    ///  Adds a polyline (LineTo) of the given `APoly` array of size `ACount`.
    ///
    ///  Appends multiple `TBLPathCmd.OnPath[AX[I], AY[I]]` commands to the path
    ///  depending on `ACount` parameter.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure PolyTo(const APoly: PBLPoint; const ACount: NativeInt); overload; inline;

    /// <summary>
    ///  Adds a polyline (LineTo) of the given `APoly` array.
    ///
    ///  Appends multiple `TBLPathCmd.OnPath[AX[I], AY[I]]` commands to the path
    ///  depending on the length of the `APoly` array.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure PolyTo(const APoly: TArray<TBLPoint>); overload; inline;

    /// <summary>
    ///  Adds a quadratic curve to `AP1` and `AP2`.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Quad[AP1]`
    ///    - `TBLPathCmd.OnPath[AP2]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands">SVG 'Q' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure QuadTo(const AP1, AP2: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds a quadratic curve to `[AX1, AY1]` and `[AX2, AY2]`.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Quad[AX1, AY1]`
    ///    - `TBLPathCmd.OnPath[AX2, AY2]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands">SVG 'Q' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure QuadTo(const AX1, AY1, AX2, AY2: Double); overload; inline;

    procedure ConicTo(const AP1, AP2: TBLPoint; const AWeight: Double); overload; inline;
    procedure ConicTo(const AX1, AY1, AX2, AY2, AWeight: Double); overload; inline;

    /// <summary>
    ///  Adds a cubic curve to `AP1`, `AP2`, and `AP3`.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Cubic[AP1]`
    ///    - `TBLPathCmd.Cubic[AP2]`
    ///    - `TBLPathCmd.On[AP3]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands">SVG 'C' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure CubicTo(const AP1, AP2, AP3: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds a cubic curve to `[AX1, AY1]`, `[AX2, AY2]`, and `[AX3, AY3]`.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Cubic[AX1, AY1]`
    ///    - `TBLPathCmd.Cubic[AX2, AY2]`
    ///    - `TBLPathCmd.On[AX3, AY3]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands">SVG 'C' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double); overload; inline;

    /// <summary>
    ///  Adds a smooth quadratic curve to `AP2`, calculating `AP1` from last
    ///  points.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Quad[Calculated]`
    ///    - `TBLPathCmd.OnPath[AP2]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands">SVG 'T' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SmoothQuadTo(const AP2: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds a smooth quadratic curve to `[AX2, AY2]`, calculating `[AX1, AY1]`
    ///  from last points.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Quad[Calculated]`
    ///    - `TBLPathCmd.OnPath[AX2, AY2]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataQuadraticBezierCommands">SVG 'T' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SmoothQuadTo(const AX2, AY2: Double); overload; inline;

    /// <summary>
    ///  Adds a smooth cubic curve to `AP2` and `AP3`, calculating `AP1` from
    ///  last points.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Cubic[Calculated]`
    ///    - `TBLPathCmd.Cubic[AP2]`
    ///    - `TBLPathCmd.OnPath[AP3]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands">SVG 'S' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SmoothCubicTo(const AP2, AP3: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds a smooth cubic curve to `[AX2, AY2]` and `[AX3, AY3]`, calculating
    ///  `[AX1, AY1]` from last points.
    ///
    ///  Appends the following commands to the path:
    ///    - `TBLPathCmd.Cubic[Calculated]`
    ///    - `TBLPathCmd.Cubic[AX2, AY2]`
    ///    - `TBLPathCmd.OnPath[AX3, AY3]`
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataCubicBezierCommands">SVG 'S' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure SmoothCubicTo(const AX2, AY2, AX3, AY3: Double); overload; inline;

    /// <summary>
    ///  Adds an arc to the path.
    ///
    ///  The center of the arc is specified by `AC` and radius by `AR`. Both
    ///  `AStart` and `ASweep` angles are in radians.
    ///  If the last vertex doesn't match the start of the arc then a `LineTo`
    ///  would be emitted before adding the arc.
    ///  Pass `True` in `AForceMoveTo` to always emit `MoveTo` at the beginning
    ///  of the arc, which starts a new figure.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ArcTo(const AC, AR: TBLPoint; const AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload; inline;

    /// <summary>
    ///  Adds an arc to the path.
    ///
    ///  The center of the arc is specified by `[ACX, ACY]` and radius by
    ///  `[ARX, ARY]`. Both `AStart` and `ASweep` angles are in radians.
    ///  If the last vertex doesn't match the start of the arc then a `LineTo`
    ///  would be emitted before adding the arc.
    ///  Pass `True` in `AForceMoveTo` to always emit `MoveTo` at the beginning
    ///  of the arc, which starts a new figure.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ArcTo(const ACX, ACY, ARX, ARY, AStart, ASweep: Double;
      const AForceMoveTo: Boolean = False); overload; inline;

    /// <summary>
    ///  Adds an arc quadrant (90deg) to the path. The first point `AP1`
    ///  specifies the quadrant corner and the last point `AP2` specifies the
    ///  end point.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ArcQuadrantTo(const AP1, AP2: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds an arc quadrant (90deg) to the path. The first point `[AX1, AY1]`
    ///  specifies the quadrant corner and the last point `[AX2, AY2]` specifies
    ///  the end point.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ArcQuadrantTo(const AX1, AY1, AX2, AY2: Double); overload; inline;

    /// <summary>
    ///  Adds an elliptic arc to the path that follows the SVG specification.
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands">SVG 'A' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure EllipticArcTo(const ARP: TBLPoint; const AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AP1: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds an elliptic arc to the path that follows the SVG specification.
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands">SVG 'A' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure EllipticArcTo(const ARX, ARY, AXAxisRotation: Double;
      const ALargeArcFlag, ASweepFlag: Boolean; const AX1, AY1: Double); overload; inline;

    /// <summary>
    ///  Closes the current figure.
    ///
    ///  Appends `TBLPathCmd.Close` to the path.
    ///
    ///  Matches <see href="https://www.w3.org/TR/SVG/paths.html#PathDataClosePathCommand">SVG 'Z' path command</see>.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Close; inline;

    /// <summary>
    ///  Adds multiple LineTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddLineToSegments(const APoints: TArray<TBLPoint>); overload; inline;

    /// <summary>
    ///  Adds multiple LineTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddLineToSegments(const APoints: PBLPoint;
      const ACount: NativeInt); overload; inline;

    /// <summary>
    ///  Adds multiple QuadTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <remarks>
    ///  The APoints array contains pairs of points (P0, P1). Thus the length
    ///  of the array must be even.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddQuadToSegments(const APoints: TArray<TBLPoint>); overload; inline;

    /// <summary>
    ///  Adds multiple QuadTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <remarks>
    ///  The APoints array contains pairs of points (P0, P1).
    ///  ASegmentCount contains the number of segments (pairs), *not* the number
    ///  of points.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddQuadToSegments(const APoints: PBLPoint;
      const ASegmentCount: NativeInt); overload; inline;

    /// <summary>
    ///  Adds multiple CubicTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <remarks>
    ///  The APoints array contains triples of points (P0, P1, P2). Thus the
    ///  length of the array must be a multiple of 3.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddCubicToSegments(const APoints: TArray<TBLPoint>); overload; inline;

    /// <summary>
    ///  Adds multiple CubicTo segments to the path.
    ///  Provides high-performance path building in case that the user knows
    ///  the segments that will be added to the path in advance.
    /// </summary>
    /// <remarks>
    ///  The APoints array contains triples of points (P0, P1, P2).
    ///  ASegmentCount contains the number of segments (triples), *not* the
    ///  number of points.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddCubicToSegments(const APoints: PBLPoint;
      const ASegmentCount: NativeInt); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `ABox`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBox(const ABox: TBLBoxI;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `ABox`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBox(const ABox: TBLBox;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `[AX0, AY0, AX1, AY1]`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBox(const AX0, AY0, AX1, AY1: Double;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `ARect`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRect(const ARect: TBLRectI;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `ARect`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRect(const ARect: TBLRect;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rectangle to the path specified by `[AX, AY, AW, AH]`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRect(const AX, AY, AW, AH: Double;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a geometry to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddGeometry(const AGeometryType: TBLGeometryType;
      const AGeometryData: Pointer;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a geometry to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddGeometry(const AGeometryType: TBLGeometryType;
      const AGeometryData: Pointer; const AMatrix: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed circle to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddCircle(const ACircle: TBLCircle;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed circle to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddCircle(const ACircle: TBLCircle; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed ellipse to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddEllipse(const AEllipse: TBLEllipse;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed ellipse to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddEllipse(const AEllipse: TBLEllipse; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rounded rectangle to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRoundRect(const ARR: TBLRoundRect;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed rounded rectangle to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRoundRect(const ARR: TBLRoundRect; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an unclosed arc to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddArc(const AArc: TBLArc;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an unclosed arc to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddArc(const AArc: TBLArc; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed chord to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddChord(const AChord: TBLArc;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed chord to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddChord(const AChord: TBLArc; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed pie to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPie(const APie: TBLArc;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed pie to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPie(const APie: TBLArc; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an unclosed line to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddLine(const ALine: TBLLine;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an unclosed line to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddLine(const ALine: TBLLine; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed triangle.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddTriangle(const ATriangle: TBLTriangle;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a closed triangle.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddTriangle(const ATriangle: TBLTriangle; const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TBLArrayView<TBLPointI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TBLArrayView<TBLPointI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TArray<TBLPointI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TArray<TBLPointI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: PBLPointI; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: PBLPointI; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TBLArrayView<TBLPoint>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TBLArrayView<TBLPoint>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TArray<TBLPoint>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: TArray<TBLPoint>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: PBLPoint; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polyline.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolyline(const APoly: PBLPoint; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TBLArrayView<TBLPointI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TBLArrayView<TBLPointI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TArray<TBLPointI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TArray<TBLPointI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: PBLPointI; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: PBLPointI; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TBLArrayView<TBLPoint>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TBLArrayView<TBLPoint>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TArray<TBLPoint>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: TArray<TBLPoint>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: PBLPoint; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds a polygon.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPolygon(const APoly: PBLPoint; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TBLArrayView<TBLBoxI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TBLArrayView<TBLBoxI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TArray<TBLBoxI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TArray<TBLBoxI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: PBLBoxI; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: PBLBoxI; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TBLArrayView<TBLBox>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TBLArrayView<TBLBox>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TArray<TBLBox>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: TArray<TBLBox>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: PBLBox; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed boxes.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddBoxArray(const AArray: PBLBox; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TBLArrayView<TBLRectI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TBLArrayView<TBLRectI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TArray<TBLRectI>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TArray<TBLRectI>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: PBLRectI; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: PBLRectI; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TBLArrayView<TBLRect>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TBLArrayView<TBLRect>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TArray<TBLRect>;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: TArray<TBLRect>;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: PBLRect; const ACount: NativeInt;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds an array of closed rectangles.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddRectArray(const AArray: PBLRect; const ACount: NativeInt;
      const ATransform: TBLMatrix2D;
      const ADir: TBLGeometryDirection = TBLGeometryDirection.CW); overload; inline;

    /// <summary>
    ///  Adds other `APath` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath); overload; inline;

    /// <summary>
    ///  Adds other `APath` sliced by the given `ARange` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath; const ARange: TBLRange); overload; inline;

    /// <summary>
    ///  Adds other `APath` translated by `AP` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath; const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds other `APath` translated by `AP` and sliced by the given `ARange`
    ///  to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath; const ARange: TBLRange;
      const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Adds other `APath` transformed by `ATransform` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath; const ATransform: TBLMatrix2D); overload; inline;

    /// <summary>
    ///  Adds other `APath` transformed by `ATransform` and sliced by the given
    ///  `ARange` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddPath(const APath: TBLPath; const ARange: TBLRange;
      const ATransform: TBLMatrix2D); overload; inline;

    /// <summary>
    ///  Adds other `APath`, but reversed.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddReversedPath(const APath: TBLPath;
      const AReverseMode: TBLPathReverseMode); overload; inline;

    /// <summary>
    ///  Adds other `APath`, but reversed.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddReversedPath(const APath: TBLPath; const ARange: TBLRange;
      const AReverseMode: TBLPathReverseMode); overload; inline;

    /// <summary>
    ///  Adds a stroke of `APath` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddStrokedPath(const APath: TBLPath;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload; inline;

    /// <summary>
    ///  Adds a stroke of `APath` to this path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AddStrokedPath(const APath: TBLPath; const ARange: TBLRange;
      const AStrokeOptions: TBLStrokeOptions;
      const AApproximationOptions: TBLApproximationOptions); overload; inline;

    procedure RemoveRange(const ARange: TBLRange); inline;

    /// <summary>
    ///  Translates the whole path by `AP`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Translate(const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Translates a part of the path specified by the given `ARange` by `AP`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Translate(const ARange: TBLRange;
      const AP: TBLPoint); overload; inline;

    /// <summary>
    ///  Transforms the whole path by matrix `AM`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Transform(const AM: TBLMatrix2D); overload; inline;

    /// <summary>
    ///  Transforms a part of the path specified by the given `ARange` by
    ///  matrix `AM`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Transform(const ARange: TBLRange;
      const AM: TBLMatrix2D); overload; inline;

    /// <summary>
    ///  Fits the whole path into the given `ARect`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure FitTo(const ARect: TBLRect); overload; inline;

    /// <summary>
    ///  Fits a part of the path specified by the given `ARange` into the given
    ///  `ARect`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure FitTo(const ARange: TBLRange;
      const ARect: TBLRect); overload; inline;

    /// <summary>
    ///  Tests whether this path and the `AOther` path are equal.
    ///
    ///  The equality check is deep. The data of both paths is examined and
    ///  binary compared (thus a slight difference like -0 and +0 would make the
    ///  equality check to fail).
    /// </summary>
    function Equals(const AOther: TBLPath): Boolean; inline;

    function GetClosestVertex(const AP: TBLPoint;
      const AMaxDistance: Double): NativeInt; overload; inline;

    function GetClosestVertex(const AP: TBLPoint; const AMaxDistance: Double;
      out ADistanceOut: Double): NativeInt; overload; inline;

    /// <summary>
    ///  Hit tests the given point `AP` by respecting the given `AFillRule`.
    /// </summary>
    function HitTest(const AP: TBLPoint; const AFillRule: TBLFillRule): TBLHitTest; inline;

    /// <summary>
    ///  Whether the path is empty, which means its size equals to zero.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///  Path size (number of vertices used).
    /// </summary>
    property Size: NativeInt read GetSize;

    /// <summary>
    ///  Path capacity (number of allocated vertices).
    /// </summary>
    property Capacity: NativeInt read GetCapacity;

    /// <summary>
    ///  Path's vertex data (read-only).
    /// </summary>
    property VertexData: PBLPoint read GetVertexData;

    /// <summary>
    ///  The end of path's vertex data (read-only).
    /// </summary>
    property VertexDataEnd: PBLPoint read GetVertexDataEnd;

    /// <summary>
    ///  Path's command data (read-only).
    /// </summary>
    property CommandData: PByte read GetCommandData;

    /// <summary>
    ///  The end of path's command data (read-only).
    /// </summary>
    property CommandDataEnd: PByte read GetCommandDataEnd;

    /// <summary>
    ///  Update a path information if necessary.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    property InfoFlags: TBLPathFlags read GetInfoFlags;

    /// <summary>
    ///  Bounding box of all vertices and control points to.
    ///
    ///  Control box is simply bounds of all vertices the path has without
    ///  further processing. It contains both on-path and off-path points.
    ///  Consider using `BoundingBox` if you need a visual bounding box.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="BoundingBox"/>
    property ControlBox: TBLBox read GetControlBox;

    /// <summary>
    ///  Bounding box of all on-path vertices and curve extrema.
    ///
    ///  The bounding box could be smaller than a bounding box obtained by
    ///  `ControlBox` as it's calculated by merging only start/end points and
    ///  curves at their extrema (not control points). The resulting bounding
    ///  box represents a visual bounds of the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="ControlBox"/>
    property BoundingBox: TBLBox read GetBoundingBox;

    /// <summary>
    ///  The ranges describing a figure at the given `AIndex`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    property FigureRanges[const AIndex: Integer]: TBLRange read GetFigureRange;

    /// <summary>
    ///  The last vertex of the path. If the very last command of the path is
    ///  `TBLPathCmd.Close` then the path will be iterated in reverse order to
    ///  match the initial vertex of the last figure.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    property LastVertex: TBLPoint read GetLastVertex;
  end;

{$ENDREGION 'Geometries'}

{$REGION 'Styling'}

{ ============================================================================
   [Styling - Colors]
  ============================================================================ }

type
  /// <summary>
  ///  32-bit RGBA color (8-bit per component) stored as `$AARRGGBB`.
  /// </summary>
  TBLRgba32 = record
  {$REGION 'Internal Declarations'}
  private
    function GetR: Byte; inline;
    procedure SetR(const AValue: Byte); inline;
    function GetG: Byte; inline;
    procedure SetG(const AValue: Byte); inline;
    function GetB: Byte; inline;
    procedure SetB(const AValue: Byte); inline;
    function GetA: Byte; inline;
    procedure SetA(const AValue: Byte); inline;
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Packed 32-bit RGBA value.
    /// </summary>
    Value: UInt32;
  public
    class function Create: TBLRgba32; overload; inline; static;
    constructor Create(const ARgba32: UInt32); overload;
    constructor Create(const AR, AG, AB: Byte; const AA: Byte = $FF); overload;

    /// <summary>
    ///  Implicitly converts from a packed 32-bit RGBA value to a TBLRgba32.
    /// </summary>
    class operator Implicit(const AValue: UInt32): TBLRgba32; inline; static;

    /// <summary>
    ///  Implicitly converts from a TBLRgba32 to a packed 32-bit RGBA value.
    /// </summary>
    class operator Implicit(const AValue: TBLRgba32): UInt32; inline; static;

    class operator Equal(const ALeft, ARight: TBLRgba32): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba32): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ARgba32: UInt32); overload; inline;
    procedure Reset(const AR, AG, AB: Byte; const AA: Byte = $FF); overload; inline;
    function Equals(const AOther: TBLRgba32): Boolean; inline;

    property R: Byte read GetR write SetR;
    property G: Byte read GetG write SetG;
    property B: Byte read GetB write SetB;
    property A: Byte read GetA write SetA;

    /// <summary>
    ///  Whether the color is fully opaque (alpha equals $FF).
    /// </summary>
    property IsOpaque: Boolean read GetIsOpaque;

    /// <summary>
    ///  Whether the color is fully transparent (alpha equals $00).
    /// </summary>
    property IsTransparent: Boolean read GetIsTransparent;
  end;
  PBLRgba32 = ^TBLRgba32;

type
  /// <summary>
  ///  64-bit RGBA color (8-bit per component) stored as `$AAAARRRRGGGGBBBB`.
  /// </summary>
  TBLRgba64 = record
  {$REGION 'Internal Declarations'}
  private
    function GetR: Word; inline;
    procedure SetR(const AValue: Word); inline;
    function GetG: Word; inline;
    procedure SetG(const AValue: Word); inline;
    function GetB: Word; inline;
    procedure SetB(const AValue: Word); inline;
    function GetA: Word; inline;
    procedure SetA(const AValue: Word); inline;
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Packed 64-bit RGBA value.
    /// </summary>
    Value: UInt64;
  public
    class function Create: TBLRgba64; overload; inline; static;
    constructor Create(const ARgba64: UInt64); overload;
    constructor Create(const ARgba32: TBLRgba32); overload;
    constructor Create(const AR, AG, AB: Word; const AA: Word = $FFFF); overload;

    /// <summary>
    ///  Implicitly converts from a packed 64-bit RGBA value to a TBLRgba64.
    /// </summary>
    class operator Implicit(const AValue: UInt64): TBLRgba64; inline; static;

    /// <summary>
    ///  Implicitly converts from a TBLRgba64 to a packed 64-bit RGBA value.
    /// </summary>
    class operator Implicit(const AValue: TBLRgba64): UInt64; inline; static;

    class operator Equal(const ALeft, ARight: TBLRgba64): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba64): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ARgba64: UInt64); overload; inline;
    procedure Reset(const ARgba32: TBLRgba32); overload; inline;
    procedure Reset(const AR, AG, AB: Word; const AA: Word = $FFFF); overload; inline;
    function Equals(const AOther: TBLRgba64): Boolean; inline;

    property R: Word read GetR write SetR;
    property G: Word read GetG write SetG;
    property B: Word read GetB write SetB;
    property A: Word read GetA write SetA;

    /// <summary>
    ///  Whether the color is fully opaque (alpha equals $FFFF).
    /// </summary>
    property IsOpaque: Boolean read GetIsOpaque;

    /// <summary>
    ///  Whether the color is fully transparent (alpha equals $0000).
    /// </summary>
    property IsTransparent: Boolean read GetIsTransparent;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLRgba32
  /// </summary>
  _TBLRgba32Helper = record helper for TBLRgba32
  public
    constructor Create(const ARgba64: TBLRgba64); overload;
    procedure Reset(const ARgba64: TBLRgba64); overload; inline;
  end;

type
  /// <summary>
  ///  128-bit RGBA color stored as 4 32-bit floating point values in [RGBA] order.
  /// </summary>
  TBLRgba = record
  {$REGION 'Internal Declarations'}
  private
    function GetIsOpaque: Boolean; inline;
    function GetIsTransparent: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Red component.
    /// </summary>
    R: Single;

    /// <summary>
    ///  Green component.
    /// </summary>
    G: Single;

    /// <summary>
    ///  Blur component.
    /// </summary>
    B: Single;

    /// <summary>
    ///  Alpha component.
    /// </summary>
    A: Single;
  public
    class function Create: TBLRgba; overload; inline; static;
    constructor Create(const AR, AG, AB: Single; const AA: Single = 1); overload;
    constructor Create(const ARgba32: TBLRgba32); overload;
    constructor Create(const ARgba64: TBLRgba64); overload;

    class operator Equal(const ALeft, ARight: TBLRgba): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLRgba): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const ARgba32: TBLRgba32); overload; inline;
    procedure Reset(const ARgba64: TBLRgba64); overload; inline;
    procedure Reset(const AR, AG, AB: Single; const AA: Single = 1); overload; inline;

    function Equals(const AOther: TBLRgba): Boolean; overload; inline;
    function Equals(const AOther: TBLRgba32): Boolean; overload; inline;
    function Equals(const AOther: TBLRgba64): Boolean; overload; inline;
    function Equals(const AR, AG, AB: Single; const AA: Single = 1): Boolean; overload; inline;

    function ToRgba32: TBLRgba32; inline;
    function ToRgba64: TBLRgba64; inline;

    /// <summary>
    ///  Whether the color is fully opaque (alpha equals 1.0).
    /// </summary>
    property IsOpaque: Boolean read GetIsOpaque;

    /// <summary>
    ///  Whether the color is fully transparent (alpha equals 0.0).
    /// </summary>
    property IsTransparent: Boolean read GetIsTransparent;
  end;

function BLRgba32: TBLRgba32; overload; inline;
function BLRgba32(const ARgba32: UInt32): TBLRgba32; overload; inline;
function BLRgba32(const ARgba64: TBLRgba64): TBLRgba32; overload; inline;
function BLRgba32(const AR, AG, AB: Byte; const AA: Byte = $FF): TBLRgba32; overload; inline;

function BLRgba64: TBLRgba64; overload; inline;
function BLRgba64(const ARgba64: UInt64): TBLRgba64; overload; inline;
function BLRgba64(const ARgba32: TBLRgba32): TBLRgba64; overload; inline;
function BLRgba64(const AR, AG, AB: Word; const AA: Word = $FFFF): TBLRgba64; overload; inline;

function BLRgba: TBLRgba; overload; inline;
function BLRgba(const ARgba32: TBLRgba32): TBLRgba; overload; inline;
function BLRgba(const ARgba64: TBLRgba64): TBLRgba; overload; inline;
function BLRgba(const AR, AG, AB: Single; const AA: Single = 1): TBLRgba; overload; inline;

function BLMin(const AA, AB: TBLRgba32): TBLRgba32; overload; inline;
function BLMax(const AA, AB: TBLRgba32): TBLRgba32; overload; inline;
function BLMin(const AA, AB: TBLRgba64): TBLRgba64; overload; inline;
function BLMax(const AA, AB: TBLRgba64): TBLRgba64; overload; inline;
function BLMin(const AA, AB: TBLRgba): TBLRgba; overload; inline;
function BLMax(const AA, AB: TBLRgba): TBLRgba; overload; inline;

{ ============================================================================
   [Styling - Gradients]
  ============================================================================ }

type
  /// <summary>
  ///  Gradient type.
  /// </summary>
  TBLGradientType = (
    /// <summary>
    ///  Linear gradient type.
    /// </summary>
    Linear,

    /// <summary>
    ///  Radial gradient type.
    /// </summary>
    Radial,

    /// <summary>
    ///  Conic gradient type.
    /// </summary>
    Conic);

type
  /// <summary>
  ///  Gradient data index.
  /// </summary>
  TBLGradientValue = (
    /// <summary>
    ///  X0 - start 'X' for a Linear gradient and `X` center for both Radial and
    ///  Conic gradients.
    /// </summary>
    CommonX0,

    /// <summary>
    ///  Y0 - start 'Y' for a Linear gradient and `Y` center for both Radial and
    ///  Conic gradients.
    /// </summary>
    CommonY0,

    /// <summary>
    ///  X1 - end 'X' for a Linear gradient and focal point `X` for a Radial
    ///  gradient.
    /// </summary>
    CommonX1,

    /// <summary>
    ///  Y1 - end 'Y' for a Linear/gradient and focal point `Y` for a Radial
    ///  gradient.
    /// </summary>
    CommonY1,

    /// <summary>
    ///  Radial gradient center radius.
    /// </summary>
    RadialR0,

    /// <summary>
    ///  Radial gradient focal radius.
    /// </summary>
    RadialR1,

    /// <summary>
    ///  Conic gradient angle.
    /// </summary>
    ConicAngle = 2,

    /// <summary>
    ///  Conic gradient angle.
    /// </summary>
    ConicRepeat = 3);

type
  /// <summary>
  ///  Gradient rendering quality.
  /// </summary>
  TBLGradientQuality = (
    /// <summary>
    ///  Nearest neighbor.
    /// </summary>
    Nearest,

    /// <summary>
    ///  Use smoothing, if available (currently never available).
    /// </summary>
    Smooth,

    /// <summary>
    ///  The renderer will use an implementation-specific dithering algorithm to
    ///  prevent banding.
    /// </summary>
    Dither);

type
  /// <summary>
  ///  Defines an `Offset` and `Rgba` color that is used by `TBLGradient` to
  ///  define a linear transition between colors.
  /// </summary>
  /// <seealso cref="TBLGradient"/>
  TBLGradientStop = record
  public
    Offset: Double;
    Rgba: TBLRgba64;
  public
    class function Create: TBLGradientStop; overload; inline; static;
    constructor Create(const AOffset: Double; const ARgba32: TBLRgba32); overload;
    constructor Create(const AOffset: Double; const ARgba64: TBLRgba64); overload;

    class operator Equal(const ALeft, ARight: TBLGradientStop): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLGradientStop): Boolean; inline; static;

    procedure Reset; overload; inline;
    procedure Reset(const AOffset: Double; const ARgba32: TBLRgba32); overload; inline;
    procedure Reset(const AOffset: Double; const ARgba64: TBLRgba64); overload; inline;

    function Equals(const AOther: TBLGradientStop): Boolean; inline;
  end;
  PBLGradientStop = ^TBLGradientStop;

function BLGradientStop: TBLGradientStop; overload; inline;
function BLGradientStop(const AOffset: Double; const ARgba32: TBLRgba32): TBLGradientStop; overload; inline;
function BLGradientStop(const AOffset: Double; const ARgba64: TBLRgba64): TBLGradientStop; overload; inline;

type
  /// <summary>
  ///  Linear gradient values packed into a structure.
  /// </summary>
  TBLLinearGradientValues = record
  public
    X0: Double;
    Y0: Double;
    X1: Double;
    Y1: Double;
  public
    class function Create: TBLLinearGradientValues; overload; inline; static;
    constructor Create(const AX0, AY0, AX1, AY1: Double); overload;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1: Double); overload; inline;
  end;

function BLLinearGradientValues: TBLLinearGradientValues; overload; inline;
function BLLinearGradientValues(const AX0, AY0, AX1, AY1: Double): TBLLinearGradientValues; overload; inline;

type
  /// <summary>
  ///  Radial gradient values packed into a structure.
  /// </summary>
  TBLRadialGradientValues = record
  public
    X0: Double;
    Y0: Double;
    X1: Double;
    Y1: Double;
    R0: Double;
    R1: Double;
  public
    class function Create: TBLRadialGradientValues; overload; inline; static;
    constructor Create(const AX0, AY0, AX1, AY1, AR0: Double;
      const AR1: Double = 0); overload;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AX1, AY1, AR0: Double;
      const AR1: Double = 0); overload; inline;
  end;

function BLRadialGradientValues: TBLRadialGradientValues; overload; inline;
function BLRadialGradientValues(const AX0, AY0, AX1, AY1, AR0: Double;
  const AR1: Double = 0): TBLRadialGradientValues; overload; inline;

type
  /// <summary>
  ///  Conic gradient values packed into a structure.
  /// </summary>
  TBLConicGradientValues = record
  public
    X0: Double;
    Y0: Double;
    Angle: Double;
    Repetition: Double;
  public
    class function Create: TBLConicGradientValues; overload; inline; static;
    constructor Create(const AX0, AY0, AAngle: Double;
      const ARepeat: Double = 1); overload;

    procedure Reset; overload; inline;
    procedure Reset(const AX0, AY0, AAngle: Double;
      const ARepeat: Double = 1); overload; inline;
  end;

function BLConicGradientValues: TBLConicGradientValues; overload; inline;
function BLConicGradientValues(const AX0, AY0, AAngle: Double;
  const ARepeat: Double = 1): TBLConicGradientValues; overload; inline;

type
  /// <summary>
  ///  Gradient.
  /// </summary>
  TBLGradient = record
  {$REGION 'Internal Declarations'}
  private type
    TImpl = record
    public
      Stops: PBLGradientStop;
      Size: Size_T;
      Capacity: Size_T;
      Transform: TBLMatrix2D;
      case Byte of
        0: (Values: array [TBLGradientValue] of Double);
        1: (Linear: TBLLinearGradientValues);
        2: (Radial: TBLRadialGradientValues);
        3: (Conic: TBLConicGradientValues);
    end;
    PImpl = ^TImpl;
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed gradient.
    ///
    ///  A default constructed gradient has `TBLGradientType.Linear` type, all
    ///  values set to zero, and has no color stops.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLGradient);

    /// <summary>
    ///  Destroys the gradient.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLGradient);

    /// <summary>
    ///  Copy constructor creates a weak copy of `other`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLGradient; const [ref] ASrc: TBLGradient); inline;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator Equal(const ALeft: TBLGradient; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Equality operator, performs the same operation as `ALeft.Equals(ARight)`.
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLGradient): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator NotEqual(const ALeft: TBLGradient; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Equality operator, performs the same operation as `not ALeft.Equals(ARight)`.
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLGradient): Boolean; inline; static;

    /// <summary>
    ///  Whether the gradient is empty.
    ///
    ///  Empty gradient is considered any gradient that has no stops.
    /// </summary>
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{$ENDREGION 'Styling'}

{$REGION 'Imaging'}

{ ============================================================================
   [Imaging - Pixel Format]
  ============================================================================ }

type
  /// <summary>
  ///  Pixel format.
  /// </summary>
  TBLFormat = (
    /// <summary>
    /// None or invalid pixel format.
    /// </summary>
    None,

    /// <summary>
    /// 32-bit premultiplied ARGB pixel format (8-bit components).
    /// </summary>
    Prgb32,

    /// <summary>
    /// 32-bit (X)RGB pixel format (8-bit components, alpha ignored).
    /// </summary>
    Xrgb32,

    /// <summary>
    /// 8-bit alpha-only pixel format.
    /// </summary>
    A8);

type
  /// <summary>
  ///  Pixel format flags.
  /// </summary>
  TBLFormatFlag = (
    /// <summary>
    ///  Pixel format provides RGB components.
    /// </summary>
    Rgb = 0,

    /// <summary>
    ///  Pixel format provides only alpha component.
    /// </summary>
    Alpha = 1,

    /// <summary>
    ///  Pixel format provides LUM component (and not RGB components).
    /// </summary>
    Lum = 2,

    /// <summary>
    ///  Indexed pixel format the requires a palette (I/O only).
    /// </summary>
    Indexed = 4,

    /// <summary>
    ///  RGB components are premultiplied by alpha component.
    /// </summary>
    Premultiplied = 8,

    /// <summary>
    ///  Pixel format doesn't use native byte-order (I/O only).
    /// </summary>
    ByteSwap = 9,

    // The following flags are only informative. They are part of `TBLFormatInfo`,
    // but don't have to be passed to `TBLPixelConverter` as they will always be
    // calculated automatically.

    /// <summary>
    ///  Pixel components are byte aligned (all 8bpp).
    /// </summary>
    ByteAligned = 16,

    /// <summary>
    ///  Pixel has some undefined bits that represent no information.
    ///
    ///  For example a 32-bit XRGB pixel has 8 undefined bits that are usually
    ///  set to all ones so the format can be interpreted as premultiplied RGB
    ///  as well. There are other formats like 16_0555 where the bit has no
    ///  information and is usually set to zero. Blend2D doesn't rely on the
    ///  content of such bits.
    /// </summary>
    UndefinedBits = 17);

type
  /// <summary>
  ///  Pixel format flags.
  /// </summary>
  TBLFormatFlags = set of TBLFormatFlag;

type
  /// <summary>
  ///  Adds functionality to TBLFormatFlags.
  /// </summary>
  _TBLFormatFlagsHelper = record helper for TBLFormatFlags
  public const
    /// <summary>
    ///  No flags
    /// </summary>
    None = [];

    /// <summary>
    ///  A combination of `Rgb` and `Alpha`.
    /// </summary>
    Rgba = [TBLFormatFlag.Rgb, TBLFormatFlag.Alpha];

    /// <summary>
    ///  A combination of `Lum` and `Alpha`.
    /// </summary>
    Luma = [TBLFormatFlag.Lum, TBLFormatFlag.Alpha];
  end;

type
  TBLFourBytes = array [0..3] of Byte;

type
  /// <summary>
  ///  Provides a detailed information about a pixel format.
  ///  Use `Query` for information of Blend2D native pixel formats.
  /// </summary>
  TBLFormatInfo = packed record
  {$REGION 'Internal Declarations'}
  private
    FDepth: Int32;
    FFlags: TBLFormatFlags;
    FSizes: TBLFourBytes;
    FShifts: TBLFourBytes;
    function GetShift(const AIndex: Integer): Byte; inline;
    function GetSize(const AIndex: Integer): Byte; inline;
    function GetPalette: PBLRgba32; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Equal(const ALeft, ARight: TBLFormatInfo): Boolean; inline; static;
    class operator NotEqual(const ALeft, ARight: TBLFormatInfo): Boolean; inline; static;

    procedure Reset; inline;

    procedure Init(const ADepth: Integer; const AFlags: TBLFormatFlags;
      const ASizes, AShifts: TBLFourBytes); inline;
    procedure SetSizes(const AR, AG, AB, AA: Byte); inline;
    procedure SetShifts(const AR, AG, AB, AA: Byte); inline;

    function HasFlag(const AFlag: TBLFormatFlag): Boolean; inline;
    procedure AddFlags(const AFlags: TBLFormatFlags); inline;
    procedure ClearFlags(const AFlags: TBLFormatFlags); inline;

    /// <summary>
    ///  Query Blend2D `AFormat` and copy it to this format info.
    /// </summary>
    /// <remarks>
    ///  `TBLFormat.None` is considered invalid format, thus if it's passed to
    ///  `Query` it will raise an error.
    /// <.remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Query(const AFormat: TBLFormat); inline;

    /// <summary>
    ///  Sanitize this `TBLFormatInfo`.
    ///
    ///  Sanitizer verifies whether the format is valid and updates the format
    ///  information about flags to values that Blend2D expects. For example
    ///  format flags are properly examined and simplified if possible,
    ///  byte-swap is implicitly performed for formats where a single component
    ///  matches one byte, etc...
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Sanitize; inline;

    property Depth: Integer read FDepth;
    property Flags: TBLFormatFlags read FFlags;
    property Sizes[const AIndex: Integer]: Byte read GetSize;
    property Shifts[const AIndex: Integer]: Byte read GetShift;
    property RSize: Byte read FSizes[0];
    property GSize: Byte read FSizes[1];
    property BSize: Byte read FSizes[2];
    property ASize: Byte read FSizes[3];
    property RShift: Byte read FShifts[0];
    property GShift: Byte read FShifts[1];
    property BShift: Byte read FShifts[2];
    property AShift: Byte read FShifts[3];
    property Palette: PBLRgba32 read GetPalette;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLFormat.
  /// </summary>
  _TBLFormatHelper = record helper for TBLFormat
  public
    /// <summary>
    ///  Get information about this format.
    /// </summary>
    /// <remarks>
    ///  `TBLFormat.None` is considered invalid format, thus if it's passed to
    ///  `Query` it will raise an error.
    /// <.remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    function Info: TBLFormatInfo; inline;
  end;

{ ============================================================================
   [Imaging - Pixel Conversion]
  ============================================================================ }

type
  /// <summary>
  ///  Flags used by `TBLPixelConverter.Make` function.
  /// </summary>
  /// <seealso cref="TBLPixelConverter"/>
  TBLPixelConverterCreateFlag = (
    /// <summary>
    ///  Specifies that the source palette in `TBLFormatInfo` doesn't have to by
    ///  copied by `TBLPixelConverter`. The caller must ensure that the palette
    ///  would stay valid until the pixel converter is destroyed.
    /// </summary>
    /// <seealso cref="TBLFormatInfo"/>
    /// <seealso cref="TBLPixelConverter"/>
    DontCopyPalette = 0,

    /// <summary>
    ///  Specifies that the source palette in `TBLFormatInfo` is alterable and
    ///  the pixel converter can modify it when preparing the conversion. The
    ///  modification can be irreversible so only use this flag when you are
    ///  sure that the palette passed to `TBLPixelConverter.Make` won't be
    ///  needed outside of pixel conversion.
    /// </summary>
    /// <remarks>
    ///  The flag `DontCopyPalette` must be set as well, otherwise this flag
    ///  would be ignored.
    /// </remarks>
    /// <seealso cref="TBLFormatInfo"/>
    /// <seealso cref="TBLPixelConverter"/>
    AlterablePalette = 1,

    /// <summary>
    ///  When there is no built-in conversion between the given pixel formats
    ///  it's possible to use an intermediate format that is used during
    ///  conversion. In such case the base pixel converter creates two more
    ///  converters that are then used internally.
    ///
    ///  This option disables such feature - creating a pixel converter would
    ///  fail if direct conversion is not possible.
    /// </summary>
    NoMultiStep = 2);

type
  /// <summary>
  ///  Flags used by `TBLPixelConverter.Make` function.
  /// </summary>
  TBLPixelConverterCreateFlags = set of TBLPixelConverterCreateFlag;

type
  /// <summary>
  ///  Adds functionality to TBLPixelConverterCreateFlags.
  /// </summary>
  _TBLPixelConverterCreateFlagsHelper = record helper for TBLPixelConverterCreateFlags
  public const
    None = [];
  end;

type
  /// <summary>
  ///  Pixel conversion options.
  /// </summary>
  TBLPixelConverterOptions = record
  public
    Origin: TBLPointI;
    Gap: NativeInt;
  end;

type
  /// <summary>
  ///  Pixel converter.
  ///
  ///  Provides an interface to convert pixels between various pixel formats.
  ///  The primary purpose of this record is to allow efficient conversion
  ///  between pixel formats used natively by Blend2D and pixel formats used
  ///  elsewhere, for example image codecs or native framebuffers.
  /// </summary>
  /// <remarks>
  ///  A default-initialized converter has a valid conversion function that
  ///  would return fail if invoked. Use `IsInitialized` to test whether the
  ///  pixel converter was properly initialized.
  /// </remarks>
  TBLPixelConverter = record
  {$REGION 'Internal Declarations'}
  private type
    TCore = record
    case Byte of
      0: (ConvertFunc: Pointer;
          InternalFlags: Byte);
      1: (Data: array [0..79] of Byte);
    end;
  private
    FCore: TCore;
    function GetIsInitialized: Boolean; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a new default-initialized pixel converter.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLPixelConverter);

    /// <summary>
    ///  Destroys the pixel-converter and releases all resources allocated by it.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLPixelConverter);

    /// <summary>
    ///  Creates a copy of the `AOther` converter.
    ///
    ///  If the `AOther` converter has dynamically allocated resources they will
    ///  be properly managed (reference counting). Only very specific converters
    ///  require such resources so this operation should be considered very
    ///  cheap.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLPixelConverter; const [ref] ASrc: TBLPixelConverter); inline;

    /// <summary>
    ///  Reset the pixel converter.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Reset; inline;

    /// <summary>
    ///  Creates a new pixel converter that will convert pixels described by
    //   `ASrcInfo` into pixels described by `ADstInfo`.
    ///
    ///  Use `ACreateFlags` to further specify the parameters of the conversion.
    /// </summary>
    /// <remarks>
    ///  Destination and source format informattion must be valid, otherwise
    ///  this method will fail.
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Make(const ADstInfo, ASrcInfo: TBLFormatInfo;
      const ACreateFlags: TBLPixelConverterCreateFlags = []); inline;

    /// <summary>
    ///  Converts a single span of pixels of `AWidth`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ConvertSpan(const ADstData, ASrcData: Pointer;
      const AWidth: Integer); overload; inline;

    /// <summary>
    ///  Converts a single span of pixels of `AWidth`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ConvertSpan(const ADstData, ASrcData: Pointer;
      const AWidth: Integer; const AOptions: TBLPixelConverterOptions); overload; inline;

    /// <summary>
    ///  Converts a rectangular area of pixels from source format to destination.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ConvertRect(const ADstData: Pointer; const ADstStride: NativeInt;
      const ASrcData: Pointer; const ASrcStride: NativeInt;
      const AWidth, AHeight: Integer); overload; inline;

    /// <summary>
    ///  Converts a rectangular area of pixels from source format to destination.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ConvertRect(const ADstData: Pointer; const ADstStride: NativeInt;
      const ASrcData: Pointer; const ASrcStride: NativeInt; const AWidth,
      AHeight: Integer; const AOptions: TBLPixelConverterOptions); overload; inline;

    /// <summary>
    ///  Whether if the converter is initialized.
    /// </summary>
    property IsInitialized: Boolean read GetIsInitialized;
  end;

{ ============================================================================
   [Imaging - Image Codecs]
  ============================================================================ }

type
  /// <summary>
  ///  Flags used by `TBLImageInfo`.
  /// </summary>
  TBLImageInfoFlag = (
    /// <summary>
    ///  Progressive mode.
    /// </summary>
    Progressive = 0);

type
  /// <summary>
  ///  Flags used by `TBLImageInfo`.
  /// </summary>
  TBLImageInfoFlags = set of TBLImageInfoFlag;

type
  /// <summary>
  ///  Adds functionality to TBLImageInfoFlags
  /// </summary>
  _TBLImageInfoFlagsHelper = record helper for TBLImageInfoFlags
  public const
    None = [];
  end;

type
  /// <summary>
  ///  Image information provided by image codecs.
  /// </summary>
  TBLImageInfo = record
  {$REGION 'Internal Declarations'}
  private
    FSize: TBLSizeI;
    FDensity: TBLSize;
    FFlags: UInt32;
    FDepth: Int16;
    FPlaneCount: Int16;
    FFrameCount: Int64;
    FRepeatCount: Int32;
    FReserved: array [0..2] of UInt32;
    FFormat: array [0..15] of UTF8Char;
    FCompression: array [0..15] of UTF8Char;
    function GetFlags: TBLImageInfoFlags; inline;
    function GetFormat: String; inline;
    function GetCompression: String; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Image size.
    /// </summary>
    property Size: TBLSizeI read FSize;

    /// <summary>
    ///  Pixel density per one meter, can contain fractions.
    /// </summary>
    property Density: TBLSize read FDensity;

    /// <summary>
    ///  Image flags.
    /// </summary>
    property Flags: TBLImageInfoFlags read GetFlags;

    /// <summary>
    ///  Image depth.
    /// </summary>
    property Depth: Smallint read FDepth;

    /// <summary>
    ///  Number of planes.
    /// </summary>
    property PlaneCount: Smallint read FPlaneCount;

    /// <summary>
    ///  Number of frames (0 = unknown/unspecified).
    /// </summary>
    property FrameCount: Int64 read FFrameCount;

    /// <summary>
    ///  Number of animation repeats (0 = infinite).
    /// </summary>
    property RepeatCount: Integer read FRepeatCount;

    /// <summary>
    ///  Image format (as understood by codec).
    /// </summary>
    property Format: String read GetFormat;

    /// <summary>
    ///  Image compression (as understood by codec).
    /// </summary>
    property Compression: String read GetCompression;
  public
    procedure Reset; inline;
  end;

type
  /// <summary>
  ///  Image codec feature bits.
  /// </summary>
  TBLImageCodecFeature = (
    /// <summary>
    ///  Image codec supports reading images (can create TBLImageDecoder).
    /// </summary>
    Read = 0,

    /// <summary>
    ///  Image codec supports writing images (can create TBLImageEncoder).
    /// </summary>
    Write = 1,

    /// <summary>
    ///  Image codec supports lossless compression.
    /// </summary>
    Lossless = 2,

    /// <summary>
    ///  Image codec supports lossy compression.
    /// </summary>
    Lossy = 3,

    /// <summary>
    ///  Image codec supports writing multiple frames (GIF).
    /// </summary>
    MultiFrame = 4,

    /// <summary>
    ///  Image codec supports IPTC metadata.
    /// </summary>
    Iptc = 28,

    /// <summary>
    ///  Image codec supports EXIF metadata.
    /// </summary>
    Exif = 29,

    /// <summary>
    ///  Image codec supports XMP metadata.
    /// </summary>
    Xmp = 30);

type
  /// <summary>
  ///  Image codec feature bits.
  /// </summary>
  TBLImageCodecFeatures = set of TBLImageCodecFeature;

type
  /// <summary>
  ///  Adds functionality to TBLImageCodecFeatures.
  /// </summary>
  _TBLImageCodecFeaturesHelper = record helper for TBLImageCodecFeatures
  public const
    None = [];
  end;

type
  /// <summary>
  ///  Image decoder.
  /// </summary>
  TBLImageDecoder = record
  {$REGION 'Internal Declarations'}
  private type
    TImpl = record
    public
      Virt: Pointer;
      Codec: TBLObjectCore;
      LastResult: Integer;
      Handle: Pointer;
      FrameIndex: Int64;
      BufferIndex: NativeInt;
    end;
    PImpl = ^TImpl;
  private
    FBase: TBLObjectCore;
    function GetIsValid: Boolean; inline;
    function GetLastResult: TBLResult; inline;
    function GetFrameIndex: Int64; inline;
    function GetBufferIndex: NativeInt; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a new default-initialized image decoder.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLImageDecoder);

    /// <summary>
    ///  Destroys the image decoder and releases all resources allocated by it.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLImageDecoder);

    /// <summary>
    ///  Creates a copy of the `AOther` image decoder.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLImageDecoder; const [ref] ASrc: TBLImageDecoder); inline;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator Equal(const ALeft: TBLImageDecoder; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image decoders are equal.
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLImageDecoder): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator NotEqual(const ALeft: TBLImageDecoder; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image decoders are not equal.
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLImageDecoder): Boolean; inline; static;

    function Equals(const AOther: TBLImageDecoder): Boolean; inline;
    procedure Reset; inline;
    procedure Swap(var AOther: TBLImageDecoder); inline;

    procedure Restart; inline;

    procedure ReadInfo(out ADst: TBLImageInfo; const ABuffer: TBLArray<Byte>); overload; inline;
    procedure ReadInfo(out ADst: TBLImageInfo; const ABuffer: TBytes); overload; inline;
    procedure ReadInfo(out ADst: TBLImageInfo; const AView: TBLArrayView<Byte>); overload; inline;
    procedure ReadInfo(out ADst: TBLImageInfo; const AData: Pointer;
      const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Tests whether the image decoder is not a built-in null instance.
    /// </summary>
    property IsValid: Boolean read GetIsValid;

    /// <summary>
    ///  The last decoding result.
    /// </summary>
    property LastResult: TBLResult read GetLastResult;

    /// <summary>
    ///  The current frame index (to be decoded).
    /// </summary>
    property FrameIndex: Int64 read GetFrameIndex;

    /// <summary>
    ///  The position in source buffer.
    /// </summary>
    property BufferIndex: NativeInt read GetBufferIndex;
  end;

type
  /// <summary>
  ///  Image encoder.
  /// </summary>
  TBLImageEncoder = record
  {$REGION 'Internal Declarations'}
  private type
    TImpl = record
    public
      Virt: Pointer;
      Codec: TBLObjectCore;
      LastResult: Integer;
      Handle: Pointer;
      FrameIndex: Int64;
      BufferIndex: NativeInt;
    end;
    PImpl = ^TImpl;
  private
    FBase: TBLObjectCore;
    function GetIsValid: Boolean; inline;
    function GetLastResult: TBLResult; inline;
    function GetFrameIndex: Int64; inline;
    function GetBufferIndex: NativeInt; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a new default-initialized image encoder.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLImageEncoder);

    /// <summary>
    ///  Destroys the image encoder and releases all resources allocated by it.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLImageEncoder);

    /// <summary>
    ///  Creates a copy of the `AOther` image encoder.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLImageEncoder; const [ref] ASrc: TBLImageEncoder); inline;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator Equal(const ALeft: TBLImageEncoder; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image encoders are equal.
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLImageEncoder): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator NotEqual(const ALeft: TBLImageEncoder; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image encoders are not equal.
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLImageEncoder): Boolean; inline; static;

    function Equals(const AOther: TBLImageEncoder): Boolean; inline;
    procedure Reset; inline;
    procedure Swap(var AOther: TBLImageEncoder); inline;

    procedure Restart; inline;

    /// <summary>
    ///  Tests whether the image encoder is not a built-in null instance.
    /// </summary>
    property IsValid: Boolean read GetIsValid;

    /// <summary>
    ///  The last encoding result.
    /// </summary>
    property LastResult: TBLResult read GetLastResult;

    /// <summary>
    ///  The current frame index (yet to be written).
    /// </summary>
    property FrameIndex: Int64 read GetFrameIndex;

    /// <summary>
    ///  The position in destination buffer.
    /// </summary>
    property BufferIndex: NativeInt read GetBufferIndex;
  end;

type
  /// <summary>
  ///  Image codec.
  ///
  ///  Provides a unified interface for inspecting image data and creating image
  ///  decoders & encoders.
  /// </summary>
  TBLImageCodec = record
  {$REGION 'Internal Declarations'}
  private type
    TImpl = record
    public
      Virt: Pointer;
      Name: TBLObjectCore;
      Vendor: TBLObjectCore;
      MimeType: TBLObjectCore;
      Extensions: TBLObjectCore;
      Features: UInt32;
    end;
    PImpl = ^TImpl;
  private
    FBase: TBLObjectCore;
    function GetIsValid: Boolean; inline;
    function GetName: TBLString; inline;
    function GetVendor: TBLString; inline;
    function GetMimeType: TBLString; inline;
    function GetExtensions: TBLString; inline;
    function GetFeatures: TBLImageCodecFeatures; inline;
    class function GetBuiltInCodecs: TBLArray<TBLImageCodec>; inline; static;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a new default-initialized image codec.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLImageCodec);

    /// <summary>
    ///  Destroys the image codec and releases all resources allocated by it.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLImageCodec);

    /// <summary>
    ///  Creates a copy of the `AOther` image codec.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLImageCodec; const [ref] ASrc: TBLImageCodec); inline;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator Equal(const ALeft: TBLImageCodec; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image codecs are equal.
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLImageCodec): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator NotEqual(const ALeft: TBLImageCodec; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Returns True if two image codecs are not equal.
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLImageCodec): Boolean; inline; static;

    function Equals(const AOther: TBLImageCodec): Boolean; inline;
    procedure Reset; inline;
    procedure Swap(var AOther: TBLImageCodec); inline;

    /// <summary>
    ///  Tests whether the image codec has the given feature.
    /// </summary>
    function HasFeature(const AFeature: TBLImageCodecFeature): Boolean; inline;

    procedure FindByName(const AName: String); overload; inline;
    procedure FindByName(const AName: String;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;
    procedure FindByName(const AName: TBLStringView); overload; inline;
    procedure FindByName(const AName: TBLStringView;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    procedure FindByExtension(const AExt: String); overload; inline;
    procedure FindByExtension(const AExt: String;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;
    procedure FindByExtension(const AExt: TBLStringView); overload; inline;
    procedure FindByExtension(const AExt: TBLStringView;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    procedure FindByData(const AData: Pointer; const ASize: NativeInt); overload; inline;
    procedure FindByData(const AData: Pointer; const ASize: NativeInt;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;
    procedure FindByData(const AView: TBLArrayView<Byte>); overload; inline;
    procedure FindByData(const AView: TBLArrayView<Byte>;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;
    procedure FindByData(const ABuffer: TBLArray<Byte>); overload; inline;
    procedure FindByData(const ABuffer: TBLArray<Byte>;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;
    procedure FindByData(const ABuffer: TBytes); overload; inline;
    procedure FindByData(const ABuffer: TBytes;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    function InspectData(const ABuffer: TBLArray<Byte>): Cardinal; overload; inline;
    function InspectData(const ABuffer: TBytes): Cardinal; overload; inline;
    function InspectData(const AView: TBLArrayView<Byte>): Cardinal; overload; inline;
    function InspectData(const AData: Pointer; const ASize: NativeInt): Cardinal; overload; inline;

    function CreateDecoder: TBLImageDecoder; inline;
    function CreateEncoder: TBLImageEncoder; inline;

    /// <summary>
    ///  Adds a codec to a global built-in codecs registry.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class procedure AddToBuiltIn(const ACodec: TBLImageCodec); inline; static;

    /// <summary>
    ///  Removes a codec from a global built-in codecs registry.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class procedure RemoveFromBuiltIn(const ACodec: TBLImageCodec); inline; static;

    /// <summary>
    ///  Tests whether the image codec is not a built-in null instance.
    /// </summary>
    property IsValid: Boolean read GetIsValid;

    /// <summary>
    ///  Image codec name (i.e, "PNG", "JPEG", etc...).
    /// </summary>
    property Name: TBLString read GetName;

    /// <summary>
    ///  The image codec vendor (i.e. "Blend2D" for all built-in codecs).
    /// </summary>
    property Vendor: TBLString read GetVendor;

    /// <summary>
    ///  A mime-type associated with the image codec's format.
    /// </summary>
    property MimeType: TBLString read GetMimeType;

    /// <summary>
    ///  A list of file extensions used to store image of this codec,
    ///  separated by '|' character.
    /// </summary>
    property Extensions: TBLString read GetExtensions;

    /// <summary>
    ///  Image codec flags.
    /// </summary>
    property Features: TBLImageCodecFeatures read GetFeatures;

    /// <summary>
    ///  Built-in codecs, which are present in a global registry.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class property BuiltInCodecs: TBLArray<TBLImageCodec> read GetBuiltInCodecs;
  end;

{ ============================================================================
   [Imaging - Images]
  ============================================================================ }

type
  /// <summary>
  ///  Filter type used by `TBLImage.Scale`.
  /// </summary>
  /// <seealso cref="TBLImage"/>
  TBLImageScaleFilter = (
    /// <summary>
    ///  No filter or uninitialized.
    /// </summary>
    None,

    /// <summary>
    ///  Nearest neighbor filter (radius 1.0).
    /// </summary>
    Nearest,

    /// <summary>
    ///  Bilinear filter (radius 1.0).
    /// </summary>
    Bilinear,

    /// <summary>
    ///  Bicubic filter (radius 2.0).
    /// </summary>
    Bicubic,

    /// <summary>
    ///  Lanczos filter (radius 2.0).
    /// </summary>
    Lanczos);

type
  /// <summary>
  ///  Data that describes a raster image. Used by `TBLImage`.
  /// </summary>
  /// <seealso cref="TBLImage"/>
  TBLImageData = record
  public
    /// <summary>
    ///  Pixel data, starting at the top left corner of the image.
    /// </summary>
    /// <remarks>
    ///  If the stride is negative the image data would start at the bottom.
    /// </remarks>
    PixelData: Pointer;

    /// <summary>
    ///  Stride (in bytes) of image data (positive when image data starts at
    ///  top-left, negative when it starts at bottom-left).
    /// </summary>
    Stride: IntPtr;

    /// <summary>
    ///  Size of the image.
    /// </summary>
    Size: TBLSizeI;

    /// <summary>
    ///  Pixel format.
    /// </summary>
    Format: TBLFormat;

    Flags: Cardinal;
  public
    /// <summary>
    ///  Resets the image data to represent an empty image (all members set to zeros).
    /// </summary>
    procedure Reset; inline;
  end;

type
  /// <summary>
  ///  2D raster image.
  ///
  ///  Raster image holds pixel data and additional information such as pixel
  ///  format. The underlying image data can be shared between multiple
  ///  instances of `TBLImage`, which can be used by multiple threads. Atomic
  ///  reference counting is used to safely manage the internal reference count
  ///  of the underlying image data.
  ///
  ///  When an image is copied to another TBLImage instance its called a
  ///  weak-copy as the underlying data is not copied, but the reference count
  ///  is increased instead (atomically).
  /// </summary>
  TBLImage = record
  {$REGION 'Internal Declarations'}
  private type
    TImpl = record
    public
      PixelData: Pointer;
      Stride: IntPtr;
      Size: TBLSizeI;
      Format: UInt8;
      Flags: UInt8;
      Depth: UInt16;
      Reserved: array [0..3] of UInt8;
    end;
    PImpl = ^TImpl;
  private
    FBase: TBLObjectCore;
    function GetIsEmpty: Boolean; inline;
    function GetWidth: Integer; inline;
    function GetHeight: Integer; inline;
    function GetSize: TBLSizeI; inline;
    function GetFormat: TBLFormat; inline;
    function GetDepth: Integer; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed image, which is an empty image having
    //   pixel format equal to `TBLFormat.None`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLFormat"/>
    class operator Initialize(out ADest: TBLImage);

    /// <summary>
    ///  Destroys the image data held by the instance.
    ///
    ///  The pixel data held by the image will only be deallocated if the
    ///  reference count of the underlying representation gets decremented to
    ///  zero.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Finalize(var ADest: TBLImage);

    /// <summary>
    ///  Creates a weak copy of `ASrc` image by incrementing the reference count
    ///  of the underlying representation.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLImage; const [ref] ASrc: TBLImage); inline;

    /// <summary>
    ///  Creates a new image data of `[AWidth, AHeight]` size (specified in
    ///  pixels) having the given pixel `AFormat`.
    ///
    ///  To create a valid image, both `AWidth` and `AHeight` must be greater
    ///  than zero and the pixel `AFormat` cannot be TBLFormat.None.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const AWidth, AHeight: Integer; const AFormat: TBLFormat);

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator Equal(const ALeft: TBLImage; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Tests whether the `ALeft` image is equal with `ARight` image.
    ///  See Equals for more details about image equality.
    /// </summary>
    class operator Equal(const ALeft, ARight: TBLImage): Boolean; inline; static;

    /// <summary>
    ///  Used to compare against `nil`.
    /// </summary>
    class operator NotEqual(const ALeft: TBLImage; const ARight: Pointer): Boolean; inline; static;

    /// <summary>
    ///  Tests whether the `ALeft` image is not equal with `ARight` image.
    ///  See Equals for more details about image equality.
    /// </summary>
    class operator NotEqual(const ALeft, ARight: TBLImage): Boolean; inline; static;

    /// <summary>
    ///  Resets the image to a default constructed image.
    ///
    ///  A default constructed image has zero size and a pixel format equal to
    ///  `TBLFormat.None`. Such image is considered `IsEmpty` and holds no data
    ///  that could be used by the rendering context or as a pattern.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLFormat"/>
    /// <seealso cref="IsEmpty"/>
    procedure Reset; inline;

    /// <summary>
    ///  Swaps the underlying data with the `AOther` image.
    /// </summary>
    procedure Swap(var AOther: TBLImage); inline;

    /// <summary>
    ///  Copy assignment replaces the underlying data of this image with
    ///  `AOther`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure AssignShallow(const AOther: TBLImage); inline;

    /// <summary>
    ///  Create a deep copy of the `AOther` image.
    /// </summary>
    procedure AssignDeep(const AOther: TBLImage); inline;

    /// <summary>
    ///  Tests whether the image is equal to `AOther` image.
    ///
    ///  Images are equal when the size, pixel format, and pixel data match.
    ///  This means that this operation could be very expensive if the images
    ///  are large.
    /// </summary>
    function Equals(const AOther: TBLImage): Boolean; inline;

    /// <summary>
    ///  Creates a new image of a specified 'AWidth`, `AHeight`, and `AFormat`.
    /// </summary>
    /// <remarks>
    ///  If invalid arguments (invalid size or format) were passed to the
    ///  method, an error with code `TBLResult.InvalidValue` will be returned
    ///  and no data will be allocated. It's also important to notice that this
    ///  method would not change anything if it fails (the previous image
    ///  content would be kept as is in such case).
    /// </remarks>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure Make(const AWidth, AHeight: Integer; const AFormat: TBLFormat); inline;

    /// <summary>
    ///  Creates a new image from external data passed in `APixelData`.
    ///
    ///  Blend2D can use pixel-data allocated outside of Blend2D, which is
    ///  useful for rendering into images that can be allocated by other
    ///  libraries. The only thing that the user should pay extra attention to
    ///  is the passed pixel `AFormat` and `AStride`.
    ///
    ///  If the image data you are passing is read-only, pass
    ///  `TBLDataAccessFlag.Read` in `AAccessFlags`, in that case Blend2D would
    ///  never attempt to modify the passed data and would create a copy instead
    ///  if such image gets modified.
    ///
    ///  Additionally, if you would like to get notified about the destruction
    ///  of the image (and thus Blend2D not holding the passed `APixelData`
    ///  anymore, pass your own function in `ADestroyFunc` parameter with an
    ///  optional `AUserData`).
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure MakeFromData(const AWidth, AHeight: Integer;
      const AFormat: TBLFormat; const APixelData: Pointer; const AStride: IntPtr;
      const AAccessFlags: TBLDataAccessFlags = TBLDataAccessFlags.RW;
      const ADestroyFunc: TBLDestroyExternalDataFunc = nil;
      const AUserData: Pointer = nil); inline;

    /// <summary>
    ///  Returns immutable in `ADataOut`, which contains pixel pointer, stride,
    ///  and other image properties like size and pixel format.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <remarks>
    ///  Although the data is filled in `TBLImageData`, which holds a
    ///  `PixelData` pointer, the data is immutable. If you intend to modify the
    ///  data, use `MakeMutable` method instead, which would copy the image
    ///  data if it's shared with another `TBLImage` instance.
    /// </remarks>
    /// <seealso cref="TBLImageData"/>
    /// <seealso cref="MakeMutable"/>
    procedure GetData(out ADataOut: TBLImageData); inline;

    /// <summary>
    ///  Makes the image data mutable and returns them in `ADataOut`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure MakeMutable(out ADataOut: TBLImageData); inline;

    /// <summary>
    ///  Converts the image to a different pixel `AFormat`.
    ///
    ///  This operation could be lossy if the given pixel `AFormat` has less
    ///  channels than this image.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <remarks>
    ///  If the image `IsEmpty` the image format would not be changed. It will
    ///  stay `TBLFormat.None` and an error with code TBLResult.NotInitialized
    ///  will be raised.
    /// </remarks>
    /// <seealso cref="IsEmpty"/>
    procedure Convert(const AFormat: TBLFormat); inline;

    /// <summary>
    ///  Reads an image from a file specified by `AFileName`.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by available image codecs, which can be
    ///  retrieved by `TBLImageCodec.BuiltInCodecs`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLImageCodec.BuiltInCodecs"/>
    procedure ReadFromFile(const AFilename: String); overload; inline;

    /// <summary>
    ///  Reads an image from a file specified by `AFilename` by using image
    ///  codecs passed via `ACodecs` parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReadFromFile(const AFilename: String;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-array starting at `AData` and
    ///  having `ASize` bytes.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by available image codecs, which can be
    ///  retrieved by `TBLImageCodec.BuiltInCodecs`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLImageCodec.BuiltInCodecs"/>
    procedure ReadFromData(const AData: Pointer; const ASize: NativeInt); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-array starting at `AData` and
    ///  having `ASize` bytes by using image codecs passed via `ACodecs`
    ///  parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReadFromData(const AData: Pointer; const ASize: NativeInt;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-buffer passed via `AArray`.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by available image codecs, which can be
    ///  retrieved by `TBLImageCodec.BuiltInCodecs`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLImageCodec.BuiltInCodecs"/>
    procedure ReadFromData(const AArray: TBLArray<Byte>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-buffer passed via `AArray` by
    ///  using image codecs passed via `ACodecs` parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReadFromData(const AArray: TBLArray<Byte>;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-buffer passed via `AArray`.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by available image codecs, which can be
    ///  retrieved by `TBLImageCodec.BuiltInCodecs`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLImageCodec.BuiltInCodecs"/>
    procedure ReadFromData(const AArray: TBytes); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-buffer passed via `AArray` by
    ///  using image codecs passed via `ACodecs` parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReadFromData(const AArray: TBytes;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-view passed via `AView`.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by available image codecs, which can be
    ///  retrieved by `TBLImageCodec.BuiltInCodecs`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <seealso cref="TBLImageCodec.BuiltInCodecs"/>
    procedure ReadFromData(const AView: TBLArrayView<Byte>); overload; inline;

    /// <summary>
    ///  Reads an image from an existing byte-view passed via `AView` by using
    ///  image codecs passed via `ACodecs` parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure ReadFromData(const AView: TBLArrayView<Byte>;
      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Writes an encoded image to a file specified by `AFilename`.
    ///
    ///  Image writer detects the image codec by inspecting the extension of a
    ///  file passed via `AFilename`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure WriteToFile(const AFilename: String); overload; inline;

    /// <summary>
    ///  Writes an encoded image to a file specified by `AFileName` using the
    ///  specified image `ACodec` to encode the image.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure WriteToFile(const AFilename: String;
      const ACodec: TBLImageCodec); overload; inline;

    /// <summary>
    ///  Writes an encoded image to a buffer `ADst` using the specified image
    ///  `ACodec` to encode the image.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure WriteToData(const ADst: TBLArray<Byte>;
      const ACodec: TBLImageCodec); overload; inline;

    /// <summary>
    ///  Encodes the image using the specified image `ACodec` and returns the
    ///  encoded data.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    function WriteToData(const ACodec: TBLImageCodec): TBytes; overload; inline;

    /// <summary>
    ///  Scales the `ASrc` image to the specified `ASize` by using `AFilter` and
    ///  writes the scaled image to `ADst`.
    ///
    ///  If the destination image `ADst` doesn't match `ASize` and the source
    ///  pixel format the underlying image data will be re-created.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class procedure Scale(const ADst, ASrc: TBLImage; const ASize: TBLSizeI;
      const AFilter: TBLImageScaleFilter); inline; static;

    /// <summary>
    ///  Whether the image is empty (such image has no size and a pixel format
    ///  is equal to `TBLFormat.None`.
    /// </summary>
    /// <seealso cref="TBLFormat"/>
    property IsEmpty: Boolean read GetIsEmpty;

    /// <summary>
    ///  Image width (in pixels).
    /// </summary>
    property Width: Integer read GetWidth;

    /// <summary>
    ///  Image height (in pixels).
    /// </summary>
    property Height: Integer read GetHeight;

    /// <summary>
    ///  Image size (in pixels).
    /// </summary>
    property Size: TBLSizeI read GetSize;

    /// <summary>
    ///  The image format.
    /// </summary>
    /// <remarks>
    ///  When an image `IsEmpty`, the pixel format returned is always
    ///  `TBLFormat.None`.
    /// </remarks>
    /// <seealso cref="TBLFormat"/>
    /// <seealso cref="IsEmpty"/>
    property Format: TBLFormat read GetFormat;

    /// <summary>
    ///  Image depth (in bits).
    /// </summary>
    property Depth: Integer read GetDepth;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLImageDecoder
  /// </summary>
  _TBLImageDecoderHelper = record helper for TBLImageDecoder
  {$REGION 'Internal Declarations'}
  private
    function GetCodec: TBLImageCodec; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    procedure ReadFrame(out ADst: TBLImage; const ABuffer: TBLArray<Byte>); overload; inline;
    procedure ReadFrame(out ADst: TBLImage; const ABuffer: TBytes); overload; inline;
    procedure ReadFrame(out ADst: TBLImage; const AView: TBLArrayView<Byte>); overload; inline;
    procedure ReadFrame(out ADst: TBLImage; const AData: Pointer;
      const ASize: NativeInt); overload; inline;

    property Codec: TBLImageCodec read GetCodec;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLImageEncoder
  /// </summary>
  _TBLImageEncoderHelper = record helper for TBLImageEncoder
  {$REGION 'Internal Declarations'}
  private
    function GetCodec: TBLImageCodec; inline;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    /// Encodes the given `AImage` and writes the encoded data to the
    /// destination buffer `ADst`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure WriteFrame(const ADst: TBLArray<Byte>; const AImage: TBLImage); overload; inline;

    /// <summary>
    /// Encodes the given `AImage` and returns the encoded data.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    function WriteFrame(const AImage: TBLImage): TBytes; overload; inline;

    property Codec: TBLImageCodec read GetCodec;
  end;
{$ENDREGION 'Imaging'}

{$REGION 'Rendering'}
type
  /// <summary>
  ///  Rendering context.
  /// </summary>
  TBLContext = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
  {$ENDREGION 'Internal Declarations'}
  public
    /// <summary>
    ///  Creates a default constructed rendering context.
    ///
    ///  Default constructed means that the instance is valid, but uninitialized,
    ///  which means the rendering context does not have attached any target. Any
    ///  attempt to use uninitialized context results in a
    ///  `TBLResult.NotInitialized` error.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Initialize(out ADest: TBLContext);

    /// <summary>
    ///  Destroys the rendering context.
    ///
    ///  Waits for all operations, detaches the target from the rendering context
    ///  and then destroys it. Does nothing if the context is not initialized.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <remarks>
    ///  Destroying the rendering context would always internally call `flush(BL_CONTEXT_FLUSH_SYNC)`, which would
    ///  flush the render calls queue in case multi-threaded rendering is used.
    /// </remarks>
    class operator Finalize(var ADest: TBLContext);

    /// <summary>
    ///  Copy constructor.
    ///
    ///  Creates a weak-copy of the `ASrc` rendering context by increasing it's
    ///  internal reference counter. This context and `ASrc` would point to the
    ///  same data and would be otherwise identical. Any change to `ASrc` would
    ///  also affect this context.
    ///
    ///  This operator is mostly provided for users that may keep a global
    ///  reference to the same rendering context, for example, otherwise sharing
    ///  is not that useful as the rendering context has states that are
    ///  manipulated during rendering.
    ///
    ///  Two weak copies of the same rendering context cannot be used by
    ///  different threads simultaneously.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    class operator Assign(var ADest: TBLContext; const [ref] ASrc: TBLContext); inline;

    /// <summary>
    ///  Creates a new rendering context for rendering to the image `ATarget`.
    ///
    ///  This is a simplified constructor that can be used to create a rendering
    ///  context without any additional parameters, which means that the
    ///  rendering context will use a single-threaded synchronous rendering.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    constructor Create(const ATarget: TBLImage);

    /// <summary>
    ///  Waits for completion of all render commands and detaches the rendering
    ///  context from the rendering target.
    ///  After `Finish` completes the rendering context implementation would be
    ///  released and replaced by a built-in nil instance (no context).
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <remarks>
    ///  Calling `Finish` would implicitly call `Flush(TBLContextFlush.Sync)`,
    ///  which would flush the render calls queue in case multi-threaded
    ///  rendering is used.
    /// </remarks>
    /// <seealso cref="Flush"/>
    procedure Finish; inline;

    /// <summary>
    ///  Clear everything to a transparent black, which is the same operation as
    ///  temporarily setting the composition operator to TBLCompOp.Clear and
    ///  then filling everything by `FillAll`.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    /// <remarks>
    ///  If the target surface doesn't have alpha, but has X component, like
    ///  `TBLFormat.Xrgb32`, the `X` component would be set to `1.0`, which
    ///  would translate to `$FF` in case of \ref `TBLFormat.Xrgb32`.
    /// </remarks>
    /// <seealso cref="TBLCompOp"/>
    /// <seealso cref="FillAll"/>
    /// <seealso cref="TBLFormat"/>
    procedure ClearAll; inline;

    /// <summary>
    ///  Fills the given `APath` with the default fill style.
    /// </summary>
    procedure FillPath(const APath: TBLPath); overload; inline;

    /// <summary>
    ///  Fills the given `APath` with the given `AColor`.
    /// </summary>
    procedure FillPath(const APath: TBLPath; const AColor: TBLRgba32); overload; inline;
  end;
{$ENDREGION 'Rendering'}

{$REGION 'Internal'}
type
  _TBLGenericUtils<T> = record // static
  public type
    {$POINTERMATH ON}
    /// <summary>
    ///  A pointer to type `T`.
    /// </summary>
    P = ^T;
    {$POINTERMATH OFF}
  public
    class procedure CopyToUnitialized(ADst: P; const ASrc: array of T;
      const AArrayType: TBLObjectType); static;
    class function AreEqual(const ALeft, ARight: T): Boolean; static;
  end;
{$ENDREGION 'Internal'}

{$INCLUDE 'Blend2D.Api.inc'}

implementation

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  System.Math,
  System.TypInfo;

{$REGION 'Error Handling'}

{ _TBLResultHelper }

function _TBLResultHelper.ToString: String;
const
  ERROR_STRINGS: array [Ord(TBLResult.OutOfMemory)..Ord(High(TBLResult))] of String = (
    'Out of memory.',
    'Invalid value/argument.',
    'Invalid state.',
    'Invalid handle or file.',
    'Invalid conversion.',
    'Value too large.',
    'Object not initialized.',
    'Not implemented.',
    'Operation not permitted.',
    'IO error.',
    'Device or resource busy.',
    'Operation interrupted.',
    'Try again.',
    'Timed out.',
    'Broken pipe.',
    'File is not seekable.',
    'Too many levels of symlinks.',
    'File is too large.',
    'File/directory already exists.',
    'Access denied.',
    'Media changed.',
    'The file/FS is read-only.',
    'Device doesn''t exist.',
    'Not found, no entry (fs).',
    'No media in drive/device.',
    'No more data / end of file.',
    'No more files.',
    'No space left on device.',
    'Directory is not empty.',
    'Not a file.',
    'Not a directory.',
    'Not same device.',
    'Not a block device.',
    'File/path name is invalid.',
    'File/path name is too long.',
    'Too many open files.',
    'Too many open files by OS.',
    'Too many symbolic links on FS.',
    'Too many threads.',
    'Thread pool is exhausted and couldn''t acquire the requested thread count.',
    'File is empty (not specific to any OS error).',
    'File open failed.',
    'Not a root device/directory.',
    'Unknown system error that failed to translate to Blend2D result code.',
    'Invalid data alignment.',
    'Invalid data signature or header.',
    'Invalid or corrupted data.',
    'Invalid string (invalid data of either UTF8, UTF16, or UTF32).',
    'Invalid key or property.',
    'Truncated data (more data required than memory/stream provides).',
    'Input data too large to be processed.',
    'Decompression failed due to invalid data (RLE, Huffman, etc).',
    'Invalid geometry (invalid path data or shape).',
    'Returned when there is no matching vertex in path data.',
    'Invalid create flags.',
    'No matching cookie.',
    'No states to restore.',
    'Cannot save state as the number of saved states reached the limit.',
    'The size of the image is too large.',
    'Image codec for a required format doesn''t exist.',
    'Unknown or invalid file format that cannot be read.',
    'Image codec doesn''t support reading the file format.',
    'Image codec doesn''t support writing the file format.',
    'Multiple IHDR chunks are not allowed (PNG).',
    'Invalid IDAT chunk (PNG).',
    'Invalid IEND chunk (PNG).',
    'Invalid PLTE chunk (PNG).',
    'Invalid tRNS chunk (PNG).',
    'Invalid filter type (PNG).',
    'Unsupported feature (JPEG).',
    'Invalid SOS marker or header (JPEG).',
    'Invalid SOF marker (JPEG).',
    'Multiple SOF markers (JPEG).',
    'Unsupported SOF marker (JPEG).',
    'Font doesn''t have any data as it''s not initialized.',
    'Font or font-face was not matched (TBLFontManager).',
    'Font has no character to glyph mapping data.',
    'Font has missing an important table.',
    'Font feature is not available.',
    'Font has an invalid CFF data.',
    'Font program terminated because the execution reached the limit.',
    'Glyph substitution requires too much space and was terminated.',
    'Invalid glyph identifier.');
begin
  if (Self = TBLResult.Success) then
    Result := 'Success'
  else if (Ord(Self) >= Low(ERROR_STRINGS)) and (Ord(Self) <= High(ERROR_STRINGS)) then
    Result := ERROR_STRINGS[Ord(Self)]
  else
    Result := Format('Unknown error (%d)', [Ord(Self)]);
end;

{ EBlend2DError }

constructor EBlend2DError.Create(const AResult: TBLResult);
begin
  inherited Create(AResult.ToString);
  FResult := AResult;
end;

var
  GErrorHandler: TBLErrorHandler = nil;
  GErrorUserData: Pointer = nil;
  GLastError: TBLResult = TBLResult.Success;

procedure BLSetErrorHandler(const AHandler: TBLErrorHandler;
  const AUserData: Pointer);
begin
  GErrorHandler := AHandler;
  GErrorUserData := AUserData;
end;

procedure ExceptionErrorHandler(const AResult: TBLResult;
  const AUserData: Pointer);
begin
  raise EBlend2DError.Create(AResult);
end;

procedure BLSetExceptionErrorHandler;
begin
  GErrorHandler := ExceptionErrorHandler;
  GErrorUserData := nil;
end;

procedure GetLastErrorHandler(const AResult: TBLResult;
  const AUserData: Pointer);
begin
  GLastError := AResult;
end;

function BLGetLastError: TBLResult;
begin
  Result := GLastError;
  GLastError := TBLResult.Success;
end;

procedure BLSetGetLastErrorHandler;
begin
  GErrorHandler := GetLastErrorHandler;
  GErrorUserData := nil;
end;

procedure _BLCheck(const AResult: Integer);
begin
  if (AResult <> 0) and Assigned(GErrorHandler) then
    GErrorHandler(TBLResult(AResult), GErrorUserData);
end;

procedure _BLCheck(const AResult: TBLResult);
begin
  if (AResult <> TBLResult.Success) and Assigned(GErrorHandler) then
    GErrorHandler(AResult, GErrorUserData);
end;

{$ENDREGION 'Error Handling'}

{$REGION 'Internal'}

{ TBLObjectCore }

//procedure TBLObjectCore.InitStatic(const AInfo: UInt32);
//begin
//  FImpl := nil;
//  FData := 0;
//  FInfo := AInfo;
//end;

function TBLObjectCore.NeedsCleanup: Boolean;
begin
  Result := (FInfo >= MDR_FLAGS);
end;

//class function TBLObjectCore.PackAbcp(const AA, AB, AC, AP: UInt32): UInt32;
//begin
//  Result := (AA shl A_SHIFT) or (AB shl B_SHIFT) or (AC shl C_SHIFT) or (AP shl P_SHIFT);
//end;
//
//class function TBLObjectCore.PackTypeWithMarker(
//  const AType: TBLObjectType): UInt32;
//begin
//  Result := (UInt32(Ord(AType)) shl TYPE_SHIFT) or M_FLAG;
//end;

procedure TBLObjectCore.Swap(var AOther: TBLObjectCore);
begin
  var Temp := AOther;
  AOther := Self;
  Self := Temp;
end;
{$ENDREGION 'Internal'}

{$REGION 'Geometries'}

function BLPoint(const AX, AY: Double): TBLPoint; inline;
begin
  Result.Reset(AX, AY);
end;

function BLAbs(const AValue: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Abs(AValue.X), Abs(AValue.Y));
end;

function BLMin(const AA, AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA.X, AB.X), Min(AA.Y, AB.Y));
end;

function BLMin(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA.X, AB), Min(AA.Y, AB));
end;

function BLMin(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Min(AA, AB.X), Min(AA, AB.Y));
end;

function BLMax(const AA, AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA.X, AB.X), Max(AA.Y, AB.Y));
end;

function BLMax(const AA: TBLPoint; const AB: Double): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA.X, AB), Max(AA.Y, AB));
end;

function BLMax(const AA: Double; const AB: TBLPoint): TBLPoint; overload; inline;
begin
  Result.Reset(Max(AA, AB.X), Max(AA, AB.Y));
end;

function BLClamp(const AA: TBLPoint; const AB, AC: Double): TBLPoint; inline;
begin
  Result := BLMin(AC, BLMax(AB, AA));
end;

function BLPointI(const AX, AY: Integer): TBLPointI; inline;
begin
  Result.Reset(AX, AY);
end;

function BLSize(const AW, AH: Double): TBLSize; inline;
begin
  Result.Reset(AW, AH);
end;

function BLAbs(const AValue: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Abs(AValue.W), Abs(AValue.H));
end;

function BLMin(const AA, AB: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Min(AA.W, AB.W), Min(AA.H, AB.H));
end;

function BLMax(const AA, AB: TBLSize): TBLSize; overload; inline;
begin
  Result.Reset(Max(AA.W, AB.W), Max(AA.H, AB.H));
end;

function BLSizeI(const AW, AH: Integer): TBLSizeI; inline;
begin
  Result.Reset(AW, AH);
end;

function BLBox(const AX0, AY0, AX1, AY1: Double): TBLBox; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function BLBoxI(const AX0, AY0, AX1, AY1: Integer): TBLBoxI; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function BLRect(const AX, AY, AW, AH: Double): TBLRect; inline;
begin
  Result.Reset(AX, AY, AW, AH);
end;

function BLRectI(const AX, AY, AW, AH: Integer): TBLRectI; inline;
begin
  Result.Reset(AX, AY, AW, AH);
end;

function BLRoundRect(const ARect: TBLRect; const AR: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(ARect, AR);
end;

function BLRoundRect(const ARect: TBLRect; const ARX, ARY: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(ARect, ARX, ARY);
end;

function BLRoundRect(const AX, AY, AW, AH, AR: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(AX, AY, AW, AH, AR);
end;

function BLRoundRect(const AX, AY, AW, AH, ARX, ARY: Double): TBLRoundRect; overload; inline;
begin
  Result.Reset(AX, AY, AW, AH, ARX, ARY);
end;

function BLCircle(const ACX, ACY, AR: Double): TBLCircle; overload; inline;
begin
  Result.Reset(ACX, ACY, AR);
end;

function BLEllipse(const ACX, ACY, AR: Double): TBLEllipse; overload; inline;
begin
  Result.Reset(ACX, ACY, AR);
end;

function BLEllipse(const ACX, ACY, ARX, ARY: Double): TBLEllipse; overload; inline;
begin
  Result.Reset(ACX, ACY, ARX, ARY);
end;

function BLArc(const ACX, ACY, ARX, ARY, AStart, ASweep: Double): TBLArc; inline;
begin
  Result.Reset(ACX, ACY, ARX, ARY, AStart, ASweep);
end;

function BLLine(const AX0, AY0, AX1, AY1: Double): TBLLine; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

function BLTriangle(const AX0, AY0, AX1, AY1, AX2, AY2: Double): TBLTriangle; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1, AX2, AY2);
end;

{ TBLPoint }

class operator TBLPoint.Add(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.X + ARight.X, ALeft.Y + ARight.Y);
end;

class operator TBLPoint.Add(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft + ARight.X, ALeft + ARight.Y);
end;

class operator TBLPoint.Add(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.X + ARight, ALeft.Y + ARight);
end;

constructor TBLPoint.Create(const AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

class operator TBLPoint.Divide(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.X / ARight, ALeft.Y / ARight);
end;

class operator TBLPoint.Divide(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft / ARight.X, ALeft / ARight.Y);
end;

class operator TBLPoint.Divide(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.X / ARight.X, ALeft.Y / ARight.Y);
end;

class operator TBLPoint.Equal(const ALeft, ARight: TBLPoint): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLPoint.Equals(const AOther: TBLPoint): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y);
end;

class operator TBLPoint.Multiply(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.X * ARight.X, ALeft.Y * ARight.Y);
end;

class operator TBLPoint.Multiply(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft * ARight.X, ALeft * ARight.Y);
end;

class operator TBLPoint.Multiply(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.X * ARight, ALeft.Y * ARight);
end;

class operator TBLPoint.Negative(const AValue: TBLPoint): TBLPoint;
begin
  Result.Reset(-AValue.X, -AValue.Y);
end;

class operator TBLPoint.NotEqual(const ALeft, ARight: TBLPoint): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLPoint.Reset(const AOther: TBLPoint);
begin
  Self := AOther;
end;

class operator TBLPoint.Subtract(const ALeft: TBLPoint;
  const ARight: Double): TBLPoint;
begin
  Result.Reset(ALeft.X - ARight, ALeft.Y - ARight);
end;

class operator TBLPoint.Subtract(const ALeft: Double;
  const ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft - ARight.X, ALeft - ARight.Y);
end;

class operator TBLPoint.Subtract(const ALeft, ARight: TBLPoint): TBLPoint;
begin
  Result.Reset(ALeft.X - ARight.X, ALeft.Y - ARight.Y);
end;

procedure TBLPoint.Reset(const AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

procedure TBLPoint.Reset;
begin
  X := 0;
  Y := 0;
end;

{ TBLPointI }

class operator TBLPointI.Add(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.X + ARight, ALeft.Y + ARight);
end;

class operator TBLPointI.Add(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft + ARight.X, ALeft + ARight.Y);
end;

class operator TBLPointI.Add(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.X + ARight.X, ALeft.X + ARight.Y);
end;

constructor TBLPointI.Create(const AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

class operator TBLPointI.Equal(const ALeft, ARight: TBLPointI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLPointI.Equals(const AOther: TBLPointI): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y);
end;

class operator TBLPointI.Multiply(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft * ARight.X, ALeft * ARight.Y);
end;

class operator TBLPointI.Multiply(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.X * ARight, ALeft.Y * ARight);
end;

class operator TBLPointI.Negative(const AValue: TBLPointI): TBLPointI;
begin
  Result.Reset(-AValue.X, -AValue.Y);
end;

class operator TBLPointI.NotEqual(const ALeft, ARight: TBLPointI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLPointI.Reset(const AOther: TBLPointI);
begin
  Self := AOther;
end;

class operator TBLPointI.Subtract(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.X - ARight.X, ALeft.X - ARight.Y);
end;

class operator TBLPointI.Subtract(const ALeft: Integer;
  const ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft - ARight.X, ALeft - ARight.Y);
end;

class operator TBLPointI.Subtract(const ALeft: TBLPointI;
  const ARight: Integer): TBLPointI;
begin
  Result.Reset(ALeft.X - ARight, ALeft.Y - ARight);
end;

procedure TBLPointI.Reset(const AX, AY: Integer);
begin
  X := AX;
  Y := AY;
end;

procedure TBLPointI.Reset;
begin
  X := 0;
  Y := 0;
end;

class operator TBLPointI.Multiply(const ALeft, ARight: TBLPointI): TBLPointI;
begin
  Result.Reset(ALeft.X * ARight.X, ALeft.X * ARight.Y);
end;

{ TBLSize }

constructor TBLSize.Create(const AW, AH: Double);
begin
  W := AW;
  H := AH;
end;

class operator TBLSize.Equal(const ALeft, ARight: TBLSize): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLSize.Equals(const AOther: TBLSize): Boolean;
begin
  Result := (W = AOther.W) and (H = AOther.H);
end;

class operator TBLSize.NotEqual(const ALeft, ARight: TBLSize): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLSize.Reset(const AOther: TBLSize);
begin
  Self := AOther;
end;

procedure TBLSize.Reset(const AW, AH: Double);
begin
  W := AW;
  H := AH;
end;

procedure TBLSize.Reset;
begin
  W := 0;
  H := 0;
end;

{ TBLSizeI }

constructor TBLSizeI.Create(const AW, AH: Integer);
begin
  W := AW;
  H := AH;
end;

class operator TBLSizeI.Equal(const ALeft, ARight: TBLSizeI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLSizeI.Equals(const AOther: TBLSizeI): Boolean;
begin
  Result := (W = AOther.W) and (H = AOther.H);
end;

class operator TBLSizeI.NotEqual(const ALeft, ARight: TBLSizeI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLSizeI.Reset(const AOther: TBLSizeI);
begin
  Self := AOther;
end;

procedure TBLSizeI.Reset(const AW, AH: Integer);
begin
  W := AW;
  H := AH;
end;

procedure TBLSizeI.Reset;
begin
  W := 0;
  H := 0;
end;

{ TBLBox }

function TBLBox.Contains(const AX, AY: Double): Boolean;
begin
  Result := (AX >= X0) and (AY >= Y0) and (AX < X1) and (AY < Y1);
end;

class operator TBLBox.Add(const ALeft: TBLPoint; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.X + ARight.X0, ALeft.Y + ARight.Y0, ALeft.X + ARight.X1, ALeft.Y + ARight.Y1);
end;

class operator TBLBox.Add(const ALeft: TBLBox; const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.X0 + ARight.X, ALeft.Y0 + ARight.Y, ALeft.X1 + ARight.X, ALeft.Y1 + ARight.Y);
end;

class operator TBLBox.Add(const ALeft: Double; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft + ARight.X0, ALeft + ARight.Y0, ALeft + ARight.X1, ALeft + ARight.Y1);
end;

class operator TBLBox.Add(const ALeft: TBLBox; const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.X0 + ARight, ALeft.Y0 + ARight, ALeft.X1 + ARight, ALeft.Y1 + ARight);
end;

function TBLBox.Contains(const APoint: TBLPoint): Boolean;
begin
  Result := Contains(APoint.X, APoint.Y);
end;

constructor TBLBox.Create(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

class operator TBLBox.Divide(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.X / ARight.X0, ALeft.Y / ARight.Y0, ALeft.X / ARight.X1, ALeft.Y / ARight.Y1);
end;

class operator TBLBox.Divide(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.X0 / ARight.X, ALeft.Y0 / ARight.Y, ALeft.X1 / ARight.X, ALeft.Y1 / ARight.Y);
end;

class operator TBLBox.Divide(const ALeft: Double; const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft / ARight.X0, ALeft / ARight.Y0, ALeft / ARight.X1, ALeft / ARight.Y1);
end;

class operator TBLBox.Divide(const ALeft: TBLBox; const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.X0 / ARight, ALeft.Y0 / ARight, ALeft.X1 / ARight, ALeft.Y1 / ARight);
end;

class operator TBLBox.Equal(const ALeft, ARight: TBLBox): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLBox.Equals(const AOther: TBLBox): Boolean;
begin
  Result := (X0 = AOther.X0) and (Y0 = AOther.Y0)
        and (X1 = AOther.X1) and (Y1 = AOther.Y1);
end;

class operator TBLBox.Multiply(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.X * ARight.X0, ALeft.Y * ARight.Y0, ALeft.X * ARight.X1, ALeft.Y * ARight.Y1);
end;

class operator TBLBox.Multiply(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.X0 * ARight.X, ALeft.Y0 * ARight.Y, ALeft.X1 * ARight.X, ALeft.Y1 * ARight.Y);
end;

class operator TBLBox.Multiply(const ALeft: Double;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft * ARight.X0, ALeft * ARight.Y0, ALeft * ARight.X1, ALeft * ARight.Y1);
end;

class operator TBLBox.Multiply(const ALeft: TBLBox;
  const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.X0 * ARight, ALeft.Y0 * ARight, ALeft.X1 * ARight, ALeft.Y1 * ARight);
end;

class operator TBLBox.NotEqual(const ALeft, ARight: TBLBox): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLBox.Reset(const AOther: TBLBox);
begin
  Self := AOther;
end;

class operator TBLBox.Subtract(const ALeft: TBLPoint;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft.X - ARight.X0, ALeft.Y - ARight.Y0, ALeft.X - ARight.X1, ALeft.Y - ARight.Y1);
end;

class operator TBLBox.Subtract(const ALeft: TBLBox;
  const ARight: TBLPoint): TBLBox;
begin
  Result.Reset(ALeft.X0 - ARight.X, ALeft.Y0 - ARight.Y, ALeft.X1 - ARight.X, ALeft.Y1 - ARight.Y);
end;

class operator TBLBox.Subtract(const ALeft: Double;
  const ARight: TBLBox): TBLBox;
begin
  Result.Reset(ALeft - ARight.X0, ALeft - ARight.Y0, ALeft - ARight.X1, ALeft - ARight.Y1);
end;

class operator TBLBox.Subtract(const ALeft: TBLBox;
  const ARight: Double): TBLBox;
begin
  Result.Reset(ALeft.X0 - ARight, ALeft.Y0 - ARight, ALeft.X1 - ARight, ALeft.Y1 - ARight);
end;

procedure TBLBox.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

procedure TBLBox.Reset;
begin
  X0 := 0;
  Y0 := 0;
  X1 := 0;
  Y1 := 0;
end;

{ TBLBoxI }

function TBLBoxI.Contains(const AX, AY: Integer): Boolean;
begin
  Result := (AX >= X0) and (AY >= Y0) and (AX < X1) and (AY < Y1);
end;

function TBLBoxI.Contains(const APoint: TBLPointI): Boolean;
begin
  Result := Contains(APoint.X, APoint.Y);
end;

constructor TBLBoxI.Create(const AX0, AY0, AX1, AY1: Integer);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

class operator TBLBoxI.Equal(const ALeft, ARight: TBLBoxI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLBoxI.Equals(const AOther: TBLBoxI): Boolean;
begin
  Result := (X0 = AOther.X0) and (Y0 = AOther.Y0)
        and (X1 = AOther.X1) and (Y1 = AOther.Y1);
end;

class operator TBLBoxI.NotEqual(const ALeft, ARight: TBLBoxI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLBoxI.Reset(const AOther: TBLBoxI);
begin
  Self := AOther;
end;

procedure TBLBoxI.Reset(const AX0, AY0, AX1, AY1: Integer);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

procedure TBLBoxI.Reset;
begin
  X0 := 0;
  Y0 := 0;
  X1 := 0;
  Y1 := 0;
end;

{ TBLRect }

constructor TBLRect.Create(const AX, AY, AW, AH: Double);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
end;

class operator TBLRect.Equal(const ALeft, ARight: TBLRect): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRect.Equals(const AOther: TBLRect): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y)
        and (W = AOther.W) and (H = AOther.H);
end;

class operator TBLRect.NotEqual(const ALeft, ARight: TBLRect): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRect.Reset(const AOther: TBLRect);
begin
  Self := AOther;
end;

procedure TBLRect.Reset(const AX, AY, AW, AH: Double);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
end;

procedure TBLRect.Reset;
begin
  X := 0;
  Y := 0;
  W := 0;
  H := 0;
end;

{ TBLRectI }

constructor TBLRectI.Create(const AX, AY, AW, AH: Integer);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
end;

class operator TBLRectI.Equal(const ALeft, ARight: TBLRectI): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRectI.Equals(const AOther: TBLRectI): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y)
        and (W = AOther.W) and (H = AOther.H);
end;

class operator TBLRectI.NotEqual(const ALeft, ARight: TBLRectI): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRectI.Reset(const AOther: TBLRectI);
begin
  Self := AOther;
end;

procedure TBLRectI.Reset(const AX, AY, AW, AH: Integer);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
end;

procedure TBLRectI.Reset;
begin
  X := 0;
  Y := 0;
  W := 0;
  H := 0;
end;

{ TBLRoundRect }

constructor TBLRoundRect.Create(const ARect: TBLRect; const AR: Double);
begin
  Reset(ARect, AR);
end;

constructor TBLRoundRect.Create(const ARect: TBLRect; const ARX, ARY: Double);
begin
  Reset(ARect, ARX, ARY);
end;

constructor TBLRoundRect.Create(const AX, AY, AW, AH, ARX, ARY: Double);
begin
  Reset(AX, AY, AW, AH, ARX, ARY);
end;

constructor TBLRoundRect.Create(const AX, AY, AW, AH, AR: Double);
begin
  Reset(AX, AY, AW, AH, AR);
end;

class operator TBLRoundRect.Equal(const ALeft, ARight: TBLRoundRect): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRoundRect.Equals(const AOther: TBLRoundRect): Boolean;
begin
  Result := (X = AOther.X) and (Y = AOther.Y)
        and (W = AOther.W) and (H = AOther.H)
        and (RX = AOther.RX) and (RY = AOther.RY);
end;

class operator TBLRoundRect.NotEqual(const ALeft,
  ARight: TBLRoundRect): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRoundRect.Reset;
begin
  X := 0;
  Y := 0;
  W := 0;
  H := 0;
  RX := 0;
  RY := 0;
end;

procedure TBLRoundRect.Reset(const ARect: TBLRect; const AR: Double);
begin
  X := ARect.X;
  Y := ARect.Y;
  W := ARect.W;
  H := ARect.H;
  RX := AR;
  RY := AR;
end;

procedure TBLRoundRect.Reset(const ARect: TBLRect; const ARX, ARY: Double);
begin
  X := ARect.X;
  Y := ARect.Y;
  W := ARect.W;
  H := ARect.H;
  RX := ARX;
  RY := ARY;
end;

procedure TBLRoundRect.Reset(const AOther: TBLRoundRect);
begin
  Self := AOther;
end;

procedure TBLRoundRect.Reset(const AX, AY, AW, AH, ARX, ARY: Double);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
  RX := ARX;
  RY := ARY;
end;

procedure TBLRoundRect.Reset(const AX, AY, AW, AH, AR: Double);
begin
  X := AX;
  Y := AY;
  W := AW;
  H := AH;
  RX := AR;
  RY := AR;
end;

{ TBLCircle }

constructor TBLCircle.Create(const ACX, ACY, AR: Double);
begin
  CX := ACX;
  CY := ACY;
  R := AR;
end;

class operator TBLCircle.Equal(const ALeft, ARight: TBLCircle): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLCircle.Equals(const AOther: TBLCircle): Boolean;
begin
  Result := (CX = AOther.CX) and (CY = AOther.CY) and (R = AOther.R);
end;

class operator TBLCircle.NotEqual(const ALeft, ARight: TBLCircle): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLCircle.Reset(const AOther: TBLCircle);
begin
  Self := AOther;
end;

procedure TBLCircle.Reset(const ACX, ACY, AR: Double);
begin
  CX := ACX;
  CY := ACY;
  R := AR;
end;

procedure TBLCircle.Reset;
begin
  CX := 0;
  CY := 0;
  R := 0;
end;

{ TBLEllipse }

constructor TBLEllipse.Create(const ACX, ACY, AR: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := AR;
  RY := AR;
end;

constructor TBLEllipse.Create(const ACX, ACY, ARX, ARY: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := ARX;
  RY := ARY;
end;

class operator TBLEllipse.Equal(const ALeft, ARight: TBLEllipse): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLEllipse.Equals(const AOther: TBLEllipse): Boolean;
begin
  Result := (CX = AOther.CX) and (CY = AOther.CY)
        and (RX = AOther.RX) and (RY = AOther.RY);
end;

class operator TBLEllipse.NotEqual(const ALeft, ARight: TBLEllipse): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLEllipse.Reset(const AOther: TBLEllipse);
begin
  Self := AOther;
end;

procedure TBLEllipse.Reset(const ACX, ACY, ARX, ARY: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := ARX;
  RY := ARY;
end;

procedure TBLEllipse.Reset(const ACX, ACY, AR: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := AR;
  RY := AR;
end;

procedure TBLEllipse.Reset;
begin
  CX := 0;
  CY := 0;
  RX := 0;
  RY := 0;
end;

{ TBLArc }

constructor TBLArc.Create(const ACX, ACY, ARX, ARY, AStart, ASweep: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := ARX;
  RY := ARY;
  Start := AStart;
  Sweep := ASweep;
end;

class operator TBLArc.Equal(const ALeft, ARight: TBLArc): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLArc.Equals(const AOther: TBLArc): Boolean;
begin
  Result := (CX = AOther.CX) and (CY = AOther.CY)
        and (RX = AOther.RX) and (RY = AOther.RY)
        and (Start = AOther.Start) and (Sweep = AOther.Sweep);
end;

class operator TBLArc.NotEqual(const ALeft, ARight: TBLArc): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLArc.Reset(const AOther: TBLArc);
begin
  Self := AOther;
end;

procedure TBLArc.Reset(const ACX, ACY, ARX, ARY, AStart, ASweep: Double);
begin
  CX := ACX;
  CY := ACY;
  RX := ARX;
  RY := ARY;
  Start := AStart;
  Sweep := ASweep;
end;

procedure TBLArc.Reset;
begin
  CX := 0;
  CY := 0;
  RX := 0;
  RY := 0;
  Start := 0;
  Sweep := 0;
end;

{ TBLLine }

constructor TBLLine.Create(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

class operator TBLLine.Equal(const ALeft, ARight: TBLLine): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLLine.Equals(const AOther: TBLLine): Boolean;
begin
  Result := (X0 = AOther.X0) and (Y0 = AOther.Y0)
        and (X1 = AOther.X1) and (Y1 = AOther.Y1);
end;

class operator TBLLine.NotEqual(const ALeft, ARight: TBLLine): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLLine.Reset(const AOther: TBLLine);
begin
  Self := AOther;
end;

procedure TBLLine.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

procedure TBLLine.Reset;
begin
  X0 := 0;
  Y0 := 0;
  X1 := 0;
  Y1 := 0;
end;

{ TBLTriangle }

constructor TBLTriangle.Create(const AX0, AY0, AX1, AY1, AX2, AY2: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
end;

class operator TBLTriangle.Equal(const ALeft, ARight: TBLTriangle): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLTriangle.Equals(const AOther: TBLTriangle): Boolean;
begin
  Result := (X0 = AOther.X0) and (Y0 = AOther.Y0)
        and (X1 = AOther.X1) and (Y1 = AOther.Y1)
        and (X2 = AOther.X2) and (Y2 = AOther.Y2);
end;

class operator TBLTriangle.NotEqual(const ALeft, ARight: TBLTriangle): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLTriangle.Reset(const AOther: TBLTriangle);
begin
  Self := AOther;
end;

procedure TBLTriangle.Reset(const AX0, AY0, AX1, AY1, AX2, AY2: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
  X2 := AX2;
  Y2 := AY2;
end;

procedure TBLTriangle.Reset;
begin
  X0 := 0;
  Y0 := 0;
  X1 := 0;
  Y1 := 0;
  X2 := 0;
  Y2 := 0;
end;

{ TBLMatrix2D }

constructor TBLMatrix2D.Create(const AM00, AM01, AM10, AM11, AM20,
  AM21: Double);
begin
  Reset(AM00, AM01, AM10, AM11, AM20, AM21);
end;

function TBLMatrix2D.Determinant: Double;
begin
  Result := (M00 * M11) - (M01 * M10);
end;

class operator TBLMatrix2D.Equal(const ALeft, ARight: TBLMatrix2D): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLMatrix2D.Equals(const AOther: TBLMatrix2D): Boolean;
begin
  Result := (M[0] = AOther.M[0]) and (M[1] = AOther.M[1])
        and (M[2] = AOther.M[2]) and (M[3] = AOther.M[3])
        and (M[4] = AOther.M[4]) and (M[5] = AOther.M[5]);
end;

function TBLMatrix2D.GetKind: TBLTransformKind;
begin
  Result := TBLTransformKind(_blMatrix2DGetType(@Self));
end;

class function TBLMatrix2D.Invert(const ASrc: TBLMatrix2D;
  out ADst: TBLMatrix2D): Boolean;
begin
  Result := (_blMatrix2DInvert(@ADst, @ASrc) = 0);
end;

function TBLMatrix2D.Invert: Boolean;
begin
  Result := (_blMatrix2DInvert(@Self, @Self) = 0);
end;

class function TBLMatrix2D.MakeIdentity: TBLMatrix2D;
begin
  Result := Identity;
end;

class function TBLMatrix2D.MakeRotation(const AAngle: Double): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, 0, 0);
end;

class function TBLMatrix2D.MakeRotation(const AAngle, AX,
  AY: Double): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, AX, AY);
end;

class function TBLMatrix2D.MakeRotation(const AAngle: Double;
  const AOrigin: TBLPoint): TBLMatrix2D;
begin
  Result.ResetToRotation(AAngle, AOrigin.X, AOrigin.Y);
end;

class function TBLMatrix2D.MakeScaling(const AXY: Double): TBLMatrix2D;
begin
  Result.Reset(AXY, 0, 0, AXY, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AX, AY: Double): TBLMatrix2D;
begin
  Result.Reset(AX, 0, 0, AY, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AP: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(AP.X, 0, 0, AP.Y, 0, 0);
end;

class function TBLMatrix2D.MakeScaling(const AP: TBLPointI): TBLMatrix2D;
begin
  Result.Reset(AP.X, 0, 0, AP.Y, 0, 0);
end;

class function TBLMatrix2D.MakeSinCos(const ASin, ACos: Double;
  const AP: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(ACos, ASin, -ASin, ACos, AP.X, AP.Y);
end;

class function TBLMatrix2D.MakeSinCos(const ASin, ACos, ATX,
  ATY: Double): TBLMatrix2D;
begin
  Result.Reset(ACos, ASin, -ASin, ACos, ATX, ATY);
end;

class function TBLMatrix2D.MakeSkewing(const AOrigin: TBLPoint): TBLMatrix2D;
begin
  Result.ResetToSkewing(AOrigin.X, AOrigin.Y);
end;

class function TBLMatrix2D.MakeSkewing(const AX, AY: Double): TBLMatrix2D;
begin
  Result.ResetToSkewing(AX, AY);
end;

class function TBLMatrix2D.MakeTranslation(const AX, AY: Double): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AX, AY);
end;

class function TBLMatrix2D.MakeTranslation(const AP: TBLPoint): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AP.X, AP.Y);
end;

class function TBLMatrix2D.MakeTranslation(const AP: TBLPointI): TBLMatrix2D;
begin
  Result.Reset(1, 0, 0, 1, AP.X, AP.Y);
end;

function TBLMatrix2D.MapPoint(const AP: TBLPoint): TBLPoint;
begin
  Result := MapPoint(AP.X, AP.Y);
end;

function TBLMatrix2D.MapPoint(const AX, AY: Double): TBLPoint;
begin
  Result.X := (AX * M00) + (AY * M10) + M20;
  Result.Y := (AX * M01) + (AY * M11) + M21;
end;

function TBLMatrix2D.MapVector(const AP: TBLPoint): TBLPoint;
begin
  Result := MapVector(AP.X, AP.Y);
end;

function TBLMatrix2D.MapVector(const AX, AY: Double): TBLPoint;
begin
  Result.X := (AX * M00) + (AY * M10);
  Result.Y := (AX * M01) + (AY * M11);
end;

class operator TBLMatrix2D.NotEqual(const ALeft, ARight: TBLMatrix2D): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLMatrix2D.PostTranslate(const AX, AY: Double);
begin
  M20 := M20 + AX;
  M21 := M21 + AY;
end;

procedure TBLMatrix2D.PostTranslate(const AP: TBLPoint);
begin
  M20 := M20 + AP.X;
  M21 := M21 + AP.Y;
end;

procedure TBLMatrix2D.PostScale(const AXY: Double);
begin
  PostScale(AXY, AXY);
end;

procedure TBLMatrix2D.PostScale(const AX, AY: Double);
begin
  M00 := M00 * AX;
  M01 := M01 * AY;
  M10 := M10 * AX;
  M11 := M11 * AY;
  M20 := M20 * AX;
  M21 := M21 * AY;
end;

procedure TBLMatrix2D.PostScale(const AP: TBLPoint);
begin
  PostScale(AP.X, AP.Y);
end;

procedure TBLMatrix2D.PostRotate(const AAngle: Double);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostRotate), @AAngle);
end;

procedure TBLMatrix2D.PostRotate(const AAngle, AX, AY: Double);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AX;
  OpData[2] := AY;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostRotatePoint), @OpData);
end;

procedure TBLMatrix2D.PostRotate(const AAngle: Double; const AP: TBLPoint);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AP.X;
  OpData[2] := AP.Y;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostRotatePoint), @OpData);
end;

procedure TBLMatrix2D.PostRotate(const AAngle: Double; const AP: TBLPointI);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AP.X;
  OpData[2] := AP.Y;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostRotatePoint), @OpData);
end;

procedure TBLMatrix2D.PostScale(const AP: TBLPointI);
begin
  PostScale(AP.X, AP.Y);
end;

procedure TBLMatrix2D.PostSkew(const AP: TBLPoint);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostSkew), @AP);
end;

procedure TBLMatrix2D.PostSkew(const AX, AY: Double);
begin
  var P: TBLPoint;
  P.Reset(AX, AY);
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostSkew), @P);
end;

procedure TBLMatrix2D.PostTransform(const AM: TBLMatrix2D);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.PostTransform), @AM);
end;

procedure TBLMatrix2D.PostTranslate(const AP: TBLPointI);
begin
  M20 := M20 + AP.X;
  M21 := M21 + AP.Y;
end;

procedure TBLMatrix2D.Reset;
begin
  M[0] := 1;
  M[1] := 0;
  M[2] := 0;
  M[3] := 1;
  M[4] := 0;
  M[5] := 0;
end;

procedure TBLMatrix2D.Reset(const AM00, AM01, AM10, AM11, AM20, AM21: Double);
begin
  M[0] := AM00;
  M[1] := AM01;
  M[2] := AM10;
  M[3] := AM11;
  M[4] := AM20;
  M[5] := AM21;
end;

procedure TBLMatrix2D.Reset(const AOther: TBLMatrix2D);
begin
  Self := AOther;
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle: Double);
begin
  _blMatrix2DSetRotation(@Self, AAngle, 0, 0);
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle, AX, AY: Double);
begin
  _blMatrix2DSetRotation(@Self, AAngle, AX, AY);
end;

procedure TBLMatrix2D.ResetToRotation(const AAngle: Double;
  const AOrigin: TBLPoint);
begin
  _blMatrix2DSetRotation(@Self, AAngle, AOrigin.X, AOrigin.Y);
end;

procedure TBLMatrix2D.ResetToScaling(const AXY: Double);
begin
  Reset(AXY, 0, 0, AXY, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AX, AY: Double);
begin
  Reset(AX, 0, 0, AY, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AP: TBLPoint);
begin
  Reset(AP.X, 0, 0, AP.Y, 0, 0);
end;

procedure TBLMatrix2D.ResetToScaling(const AP: TBLPointI);
begin
  Reset(AP.X, 0, 0, AP.Y, 0, 0);
end;

procedure TBLMatrix2D.ResetToSinCos(const ASin, ACos: Double;
  const AP: TBLPoint);
begin
  Reset(ACos, ASin, -ASin, ACos, AP.X, AP.Y);
end;

procedure TBLMatrix2D.ResetToSinCos(const ASin, ACos, ATX, ATY: Double);
begin
  Reset(ACos, ASin, -ASin, ACos, ATX, ATY);
end;

procedure TBLMatrix2D.ResetToSkewing(const AP: TBLPoint);
begin
  _blMatrix2DSetSkewing(@Self, AP.X, AP.Y);
end;

procedure TBLMatrix2D.ResetToSkewing(const AX, AY: Double);
begin
  _blMatrix2DSetSkewing(@Self, AX, AY);
end;

procedure TBLMatrix2D.ResetToTranslation(const AX, AY: Double);
begin
  Reset(1, 0, 0, 1, AX, AY);
end;

procedure TBLMatrix2D.ResetToTranslation(const AP: TBLPoint);
begin
  Reset(1, 0, 0, 1, AP.X, AP.Y);
end;

procedure TBLMatrix2D.ResetToTranslation(const AP: TBLPointI);
begin
  Reset(1, 0, 0, 1, AP.X, AP.Y);
end;

procedure TBLMatrix2D.Rotate(const AAngle: Double);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.Rotate), @AAngle);
end;

procedure TBLMatrix2D.Rotate(const AAngle, AX, AY: Double);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AX;
  OpData[2] := AY;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.RotatePoint), @OpData);
end;

procedure TBLMatrix2D.Rotate(const AAngle: Double; const AP: TBLPoint);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AP.X;
  OpData[2] := AP.Y;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.RotatePoint), @OpData);
end;

procedure TBLMatrix2D.Rotate(const AAngle: Double; const AP: TBLPointI);
var
  OpData: array [0..2] of Double;
begin
  OpData[0] := AAngle;
  OpData[1] := AP.X;
  OpData[2] := AP.Y;
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.RotatePoint), @OpData);
end;

procedure TBLMatrix2D.Scale(const AXY: Double);
begin
  Scale(AXY, AXY);
end;

procedure TBLMatrix2D.Scale(const AX, AY: Double);
begin
  M00 := M00 * AX;
  M01 := M01 * AX;
  M10 := M10 * AY;
  M11 := M11 * AY;
end;

procedure TBLMatrix2D.Scale(const AP: TBLPoint);
begin
  Scale(AP.X, AP.Y);
end;

procedure TBLMatrix2D.Scale(const AP: TBLPointI);
begin
  Scale(AP.X, AP.Y);
end;

procedure TBLMatrix2D.Skew(const AP: TBLPoint);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.Skew), @AP);
end;

procedure TBLMatrix2D.Skew(const AX, AY: Double);
begin
  var P: TBLPoint;
  P.Reset(AX, AY);
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.Skew), @P);
end;

procedure TBLMatrix2D.Transform(const AM: TBLMatrix2D);
begin
  _blMatrix2DApplyOp(@Self, Ord(TBLTransformOp.Transform), @AM);
end;

procedure TBLMatrix2D.Translate(const AX, AY: Double);
begin
  M20 := M20 + (AX * M00) + (AY * M10);
  M21 := M21 + (AX * M01) + (AY * M11);
end;

procedure TBLMatrix2D.Translate(const AP: TBLPoint);
begin
  Translate(AP.X, AP.Y);
end;

procedure TBLMatrix2D.Translate(const AP: TBLPointI);
begin
  Translate(AP.X, AP.Y);
end;

{ TBLStrokeOptions }

class operator TBLStrokeOptions.Assign(var ADest: TBLStrokeOptions;
  const [ref] ASrc: TBLStrokeOptions);
begin
  _BLCheck(_blStrokeOptionsInitWeak(@ADest, @ASrc));
end;

class operator TBLStrokeOptions.Equal(const ALeft,
  ARight: TBLStrokeOptions): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLStrokeOptions.Equals(const AOther: TBLStrokeOptions): Boolean;
begin
  Result := _blStrokeOptionsEquals(@Self, @AOther);
end;

class operator TBLStrokeOptions.Finalize(var ADest: TBLStrokeOptions);
begin
  _BLCheck(_blStrokeOptionsDestroy(@ADest));
end;

function TBLStrokeOptions.GetCap(
  const AIndex: TBLStrokeCapPosition): TBLStrokeCap;
begin
  Result := TBLStrokeCap(FValues.Caps[AIndex]);
end;

function TBLStrokeOptions.GetEndCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FValues.EndCap);
end;

function TBLStrokeOptions.GetJoin: TBLStrokeJoin;
begin
  Result := TBLStrokeJoin(FValues.Join);
end;

function TBLStrokeOptions.GetStartCap: TBLStrokeCap;
begin
  Result := TBLStrokeCap(FValues.StartCap);
end;

function TBLStrokeOptions.GetTransformOrder: TBLStrokeTransformOrder;
begin
  Result := TBLStrokeTransformOrder(FValues.TransformOrder);
end;

class operator TBLStrokeOptions.Initialize(out ADest: TBLStrokeOptions);
begin
  _BLCheck(_blStrokeOptionsInit(@ADest));
end;

class operator TBLStrokeOptions.NotEqual(const ALeft,
  ARight: TBLStrokeOptions): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLStrokeOptions.Reset;
begin
  _BLCheck(_blStrokeOptionsReset(@Self));
end;

procedure TBLStrokeOptions.SetCap(const AIndex: TBLStrokeCapPosition;
  const AValue: TBLStrokeCap);
begin
  FValues.Caps[AIndex] := Ord(AValue);
end;

procedure TBLStrokeOptions.SetCaps(const ACap: TBLStrokeCap);
begin
  FValues.StartCap := Ord(ACap);
  FValues.EndCap := Ord(ACap);
end;

procedure TBLStrokeOptions.SetEndCap(const AValue: TBLStrokeCap);
begin
  FValues.EndCap := Ord(AValue);
end;

procedure TBLStrokeOptions.SetJoin(const AValue: TBLStrokeJoin);
begin
  FValues.Join := Ord(AValue);
end;

procedure TBLStrokeOptions.SetStartCap(const AValue: TBLStrokeCap);
begin
  FValues.StartCap := Ord(AValue);
end;

procedure TBLStrokeOptions.SetTransformOrder(
  const AValue: TBLStrokeTransformOrder);
begin
  FValues.TransformOrder := Ord(AValue);
end;

{ TBLApproximationOptions }

function TBLApproximationOptions.GetFlattenMode: TBLFlattenMode;
begin
  Result := TBLFlattenMode(FFlattenMode);
end;

function TBLApproximationOptions.GetOffsetMode: TBLOffsetMode;
begin
  Result := TBLOffsetMode(FOffsetMode);
end;

procedure TBLApproximationOptions.SetFlattenMode(const AValue: TBLFlattenMode);
begin
  FFlattenMode := Ord(AValue);
end;

procedure TBLApproximationOptions.SetOffsetMode(const AValue: TBLOffsetMode);
begin
  FOffsetMode := Ord(AValue);
end;

{ TBLPathView }

procedure TBLPathView.Reset(const ACommandDataIn: PByte;
  const AVertexDataIn: PBLPoint; const ASizeIn: NativeInt);
begin
  FCommandData := ACommandDataIn;
  FVertexData := AVertexDataIn;
  FSize := ASizeIn;
end;

procedure TBLPathView.Reset;
begin
  FCommandData := nil;
  FVertexData := nil;
  FSize := 0;
end;

{ TBLPath }

procedure TBLPath.AddBox(const ABox: TBLBoxI; const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddBoxI(@Self, @ABox, Ord(ADir)));
end;

procedure TBLPath.AddBox(const ABox: TBLBox; const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddBoxD(@Self, @ABox, Ord(ADir)));
end;

procedure TBLPath.AddArc(const AArc: TBLArc; const ATransform: TBLMatrix2D;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Arc, @AArc, ATransform, ADir);
end;

procedure TBLPath.AddArc(const AArc: TBLArc; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Arc, @AArc, ADir);
end;

procedure TBLPath.AddBox(const AX0, AY0, AX1, AY1: Double;
  const ADir: TBLGeometryDirection);
begin
  var Box: TBLBox;
  Box.Reset(AX0, AY0, AX1, AY1);
  _BLCheck(_blPathAddBoxD(@Self, @Box, Ord(ADir)));
end;

procedure TBLPath.AddBoxArray(const AArray: TArray<TBLBoxI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBoxI>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddBoxArray(View, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: PBLBoxI; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBoxI>;
  View.Reset(Pointer(AArray), ACount);
  AddBoxArray(View, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: PBLBoxI; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBoxI>;
  View.Reset(Pointer(AArray), ACount);
  AddBoxArray(View, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TBLArrayView<TBLBoxI>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewBoxI, @AArray, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TBLArrayView<TBLBoxI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewBoxI, @AArray, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TArray<TBLBoxI>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBoxI>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddBoxArray(View, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TArray<TBLBox>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBox>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddBoxArray(View, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: PBLBox; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBox>;
  View.Reset(Pointer(AArray), ACount);
  AddBoxArray(View, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: PBLBox; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBox>;
  View.Reset(Pointer(AArray), ACount);
  AddBoxArray(View, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TBLArrayView<TBLBox>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewBoxD, @AArray, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TBLArrayView<TBLBox>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewBoxD, @AArray, ATransform, ADir);
end;

procedure TBLPath.AddBoxArray(const AArray: TArray<TBLBox>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLBox>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddBoxArray(View, ADir);
end;

procedure TBLPath.AddChord(const AChord: TBLArc; const ATransform: TBLMatrix2D;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Chord, @AChord, ATransform, ADir);
end;

procedure TBLPath.AddChord(const AChord: TBLArc;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Chord, @AChord, ADir);
end;

procedure TBLPath.AddCircle(const ACircle: TBLCircle;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Circle, @ACircle, ATransform, ADir);
end;

procedure TBLPath.AddCircle(const ACircle: TBLCircle;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Circle, @ACircle, ADir);
end;

procedure TBLPath.AddCubicToSegments(const APoints: PBLPoint;
  const ASegmentCount: NativeInt);
begin
  var CmdPtr: PByte;
  var VtxPtr: PBLPoint;
  var PointCount: NativeInt := ASegmentCount * 3;
  _BLCheck(_blPathModifyOp(@Self, Ord(TBLModifyOp.AppendGrow), PointCount, @CmdPtr, @VtxPtr));

  for var I := 0 to ASegmentCount - 1 do
  begin
    CmdPtr^ := Ord(TBLPathCmd.Cubic);
    Inc(CmdPtr);
    CmdPtr^ := Ord(TBLPathCmd.Cubic);
    Inc(CmdPtr);
    CmdPtr^ := Ord(TBLPathCmd.OnPath);
    Inc(CmdPtr);
  end;

  Move(APoints^, VtxPtr^, PointCount * SizeOf(TBLPoint));
end;

procedure TBLPath.AddEllipse(const AEllipse: TBLEllipse;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Ellipse, @AEllipse, ATransform, ADir);
end;

procedure TBLPath.AddEllipse(const AEllipse: TBLEllipse;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Ellipse, @AEllipse, ADir);
end;

procedure TBLPath.AddGeometry(const AGeometryType: TBLGeometryType;
  const AGeometryData: Pointer; const AMatrix: TBLMatrix2D;
  const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddGeometry(@Self, Ord(AGeometryType), AGeometryData, @AMatrix, Ord(ADir)));
end;

procedure TBLPath.AddGeometry(const AGeometryType: TBLGeometryType;
  const AGeometryData: Pointer; const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddGeometry(@Self, Ord(AGeometryType), AGeometryData, nil, Ord(ADir)));
end;

procedure TBLPath.AddCubicToSegments(const APoints: TArray<TBLPoint>);
begin
  Assert((Length(APoints) mod 3) = 0);
  AddCubicToSegments(Pointer(APoints), Length(APoints) div 3);
end;

procedure TBLPath.AddLine(const ALine: TBLLine; const ATransform: TBLMatrix2D;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Line, @ALine, ATransform, ADir);
end;

procedure TBLPath.AddLine(const ALine: TBLLine;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Line, @ALine, ADir);
end;

procedure TBLPath.AddLineToSegments(const APoints: PBLPoint;
  const ACount: NativeInt);
begin
  var CmdPtr: PByte;
  var VtxPtr: PBLPoint;
  _BLCheck(_blPathModifyOp(@Self, Ord(TBLModifyOp.AppendGrow), ACount, @CmdPtr, @VtxPtr));
  FillChar(CmdPtr^, ACount, Ord(TBLPathCmd.OnPath));
  Move(APoints^, VtxPtr^, ACount * SizeOf(TBLPoint));
end;

procedure TBLPath.AddPath(const APath: TBLPath; const AP: TBLPoint);
begin
  _BLCheck(_blPathAddTranslatedPath(@Self, @APath, nil, @AP));
end;

procedure TBLPath.AddPath(const APath: TBLPath; const ARange: TBLRange);
begin
  _BLCheck(_blPathAddPath(@Self, @APath, @ARange));
end;

procedure TBLPath.AddPath(const APath: TBLPath);
begin
  _BLCheck(_blPathAddPath(@Self, @APath, nil));
end;

procedure TBLPath.AddPath(const APath: TBLPath; const ARange: TBLRange;
  const ATransform: TBLMatrix2D);
begin
  _BLCheck(_blPathAddTransformedPath(@Self, @APath, @ARange, @ATransform));
end;

procedure TBLPath.AddPath(const APath: TBLPath; const ATransform: TBLMatrix2D);
begin
  _BLCheck(_blPathAddTransformedPath(@Self, @APath, nil, @ATransform));
end;

procedure TBLPath.AddPath(const APath: TBLPath; const ARange: TBLRange;
  const AP: TBLPoint);
begin
  _BLCheck(_blPathAddTranslatedPath(@Self, @APath, @ARange, @AP));
end;

procedure TBLPath.AddPie(const APie: TBLArc; const ATransform: TBLMatrix2D;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Pie, @APie, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TArray<TBLPointI>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolygon(View, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: PBLPointI; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), ACount);
  AddPolygon(View, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: PBLPointI; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), ACount);
  AddPolygon(View, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TBLArrayView<TBLPointI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolygonI, @APoly, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TBLArrayView<TBLPointI>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolygonI, @APoly, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TArray<TBLPointI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolygon(View, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TArray<TBLPoint>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolygon(View, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: PBLPoint; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), ACount);
  AddPolygon(View, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: PBLPoint; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), ACount);
  AddPolygon(View, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TBLArrayView<TBLPoint>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolygonD, @APoly, ATransform, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TBLArrayView<TBLPoint>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolygonD, @APoly, ADir);
end;

procedure TBLPath.AddPolygon(const APoly: TArray<TBLPoint>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolygon(View, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TArray<TBLPointI>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolyline(View, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: PBLPointI; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), ACount);
  AddPolyline(View, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: PBLPointI; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), ACount);
  AddPolyline(View, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TBLArrayView<TBLPointI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolylineI, @APoly, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TBLArrayView<TBLPointI>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolylineI, @APoly, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TArray<TBLPointI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPointI>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolyline(View, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TArray<TBLPoint>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolyline(View, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: PBLPoint; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), ACount);
  AddPolyline(View, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: PBLPoint; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), ACount);
  AddPolyline(View, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TBLArrayView<TBLPoint>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolylineD, @APoly, ATransform, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TBLArrayView<TBLPoint>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.PolylineD, @APoly, ADir);
end;

procedure TBLPath.AddPolyline(const APoly: TArray<TBLPoint>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLPoint>;
  View.Reset(Pointer(APoly), Length(APoly));
  AddPolyline(View, ATransform, ADir);
end;

procedure TBLPath.AddPie(const APie: TBLArc; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Pie, @APie, ADir);
end;

procedure TBLPath.AddQuadToSegments(const APoints: PBLPoint;
  const ASegmentCount: NativeInt);
begin
  var CmdPtr: PByte;
  var VtxPtr: PBLPoint;
  var PointCount: NativeInt := ASegmentCount shl 1;
  _BLCheck(_blPathModifyOp(@Self, Ord(TBLModifyOp.AppendGrow), PointCount, @CmdPtr, @VtxPtr));

  for var I := 0 to ASegmentCount - 1 do
  begin
    CmdPtr^ := Ord(TBLPathCmd.Quad);
    Inc(CmdPtr);
    CmdPtr^ := Ord(TBLPathCmd.OnPath);
    Inc(CmdPtr);
  end;

  Move(APoints^, VtxPtr^, PointCount * SizeOf(TBLPoint));
end;

procedure TBLPath.AddRect(const ARect: TBLRectI;
  const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddRectI(@Self, @ARect, Ord(ADir)));
end;

procedure TBLPath.AddRect(const ARect: TBLRect;
  const ADir: TBLGeometryDirection);
begin
  _BLCheck(_blPathAddRectD(@Self, @ARect, Ord(ADir)));
end;

procedure TBLPath.AddRect(const AX, AY, AW, AH: Double;
  const ADir: TBLGeometryDirection);
begin
  var R: TBLRect;
  R.Reset(AX, AY, AW, AH);
  _BLCheck(_blPathAddRectD(@Self, @R, Ord(ADir)));
end;

procedure TBLPath.AddRectArray(const AArray: TArray<TBLRectI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRectI>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddGeometry(TBLGeometryType.ArrayViewRectI, @View, ATransform, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: PBLRectI; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRectI>;
  View.Reset(Pointer(AArray), ACount);
  AddGeometry(TBLGeometryType.ArrayViewRectI, @View, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: PBLRectI; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRectI>;
  View.Reset(Pointer(AArray), ACount);
  AddGeometry(TBLGeometryType.ArrayViewRectI, @View, ATransform, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TBLArrayView<TBLRectI>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewRectI, @AArray, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TBLArrayView<TBLRectI>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewRectI, @AArray, ATransform, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TArray<TBLRectI>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRectI>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddGeometry(TBLGeometryType.ArrayViewRectI, @View, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TArray<TBLRect>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRect>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddGeometry(TBLGeometryType.ArrayViewRectD, @View, ATransform, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: PBLRect; const ACount: NativeInt;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRect>;
  View.Reset(Pointer(AArray), ACount);
  AddGeometry(TBLGeometryType.ArrayViewRectD, @View, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: PBLRect; const ACount: NativeInt;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRect>;
  View.Reset(Pointer(AArray), ACount);
  AddGeometry(TBLGeometryType.ArrayViewRectD, @View, ATransform, ADir);
end;

procedure TBLPath.AddReversedPath(const APath: TBLPath; const ARange: TBLRange;
  const AReverseMode: TBLPathReverseMode);
begin
  _BLCheck(_blPathAddReversedPath(@Self, @APath, @ARange, Ord(AReverseMode)));
end;

procedure TBLPath.AddReversedPath(const APath: TBLPath;
  const AReverseMode: TBLPathReverseMode);
begin
  _BLCheck(_blPathAddReversedPath(@Self, @APath, nil, Ord(AReverseMode)));
end;

procedure TBLPath.AddRectArray(const AArray: TBLArrayView<TBLRect>;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewRectD, @AArray, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TBLArrayView<TBLRect>;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.ArrayViewRectD, @AArray, ATransform, ADir);
end;

procedure TBLPath.AddRectArray(const AArray: TArray<TBLRect>;
  const ADir: TBLGeometryDirection);
begin
  var View: TBLArrayView<TBLRect>;
  View.Reset(Pointer(AArray), Length(AArray));
  AddGeometry(TBLGeometryType.ArrayViewRectD, @View, ADir);
end;

procedure TBLPath.AddRoundRect(const ARR: TBLRoundRect;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.RoundRect, @ARR, ATransform, ADir);
end;

procedure TBLPath.AddStrokedPath(const APath: TBLPath; const ARange: TBLRange;
  const AStrokeOptions: TBLStrokeOptions;
  const AApproximationOptions: TBLApproximationOptions);
begin
  _BLCheck(_blPathAddStrokedPath(@Self, @APath, @ARange, @AStrokeOptions, @AApproximationOptions));
end;

procedure TBLPath.AddStrokedPath(const APath: TBLPath;
  const AStrokeOptions: TBLStrokeOptions;
  const AApproximationOptions: TBLApproximationOptions);
begin
  _BLCheck(_blPathAddStrokedPath(@Self, @APath, nil, @AStrokeOptions, @AApproximationOptions));
end;

procedure TBLPath.AddTriangle(const ATriangle: TBLTriangle;
  const ATransform: TBLMatrix2D; const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Triangle, @ATriangle, ATransform, ADir);
end;

procedure TBLPath.AddTriangle(const ATriangle: TBLTriangle;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.Triangle, @ATriangle, ADir);
end;

procedure TBLPath.AddRoundRect(const ARR: TBLRoundRect;
  const ADir: TBLGeometryDirection);
begin
  AddGeometry(TBLGeometryType.RoundRect, @ARR, ADir);
end;

procedure TBLPath.AddQuadToSegments(const APoints: TArray<TBLPoint>);
begin
  Assert((Length(APoints) and 1) = 0);
  AddQuadToSegments(Pointer(APoints), Length(APoints) shr 1);
end;

procedure TBLPath.AddLineToSegments(const APoints: TArray<TBLPoint>);
begin
  AddLineToSegments(Pointer(APoints), Length(APoints));
end;

procedure TBLPath.ArcQuadrantTo(const AX1, AY1, AX2, AY2: Double);
begin
  _BLCheck(_blPathArcQuadrantTo(@Self, AX1, AY1, AX2, AY2));
end;

procedure TBLPath.ArcQuadrantTo(const AP1, AP2: TBLPoint);
begin
  _BLCheck(_blPathArcQuadrantTo(@Self, AP1.X, AP1.Y, AP2.X, AP2.Y));
end;

procedure TBLPath.ArcTo(const ACX, ACY, ARX, ARY, AStart, ASweep: Double;
  const AForceMoveTo: Boolean);
begin
  _BLCheck(_blPathArcTo(@Self, ACX, ACY, ARX, ARY, AStart, ASweep, AForceMoveTo));
end;

procedure TBLPath.ArcTo(const AC, AR: TBLPoint; const AStart, ASweep: Double;
  const AForceMoveTo: Boolean);
begin
  _BLCheck(_blPathArcTo(@Self, AC.X, AC.Y, AR.X, AR.Y, AStart, ASweep, AForceMoveTo));
end;

class operator TBLPath.Assign(var ADest: TBLPath; const [ref] ASrc: TBLPath);
begin
  _BLCheck(_blPathInitWeak(@ADest, @ASrc));
end;

procedure TBLPath.AssignDeep(const AOther: TBLPath);
begin
  _BLCheck(_blPathAssignDeep(@Self, @AOther));
end;

procedure TBLPath.Clear;
begin
  _BLCheck(_blPathClear(@Self));
end;

procedure TBLPath.Close;
begin
  _BLCheck(_blPathClose(@Self));
end;

procedure TBLPath.ConicTo(const AX1, AY1, AX2, AY2, AWeight: Double);
begin
  _BLCheck(_blPathConicTo(@Self, AX1, AY1, AX2, AY2, AWeight));
end;

procedure TBLPath.ConicTo(const AP1, AP2: TBLPoint; const AWeight: Double);
begin
  _BLCheck(_blPathConicTo(@Self, AP1.X, AP1.Y, AP2.X, AP2.Y, AWeight));
end;

procedure TBLPath.CubicTo(const AP1, AP2, AP3: TBLPoint);
begin
  _BLCheck(_blPathCubicTo(@Self, AP1.X, AP1.Y, AP2.X, AP2.Y, AP3.X, AP3.Y));
end;

procedure TBLPath.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double);
begin
  _BLCheck(_blPathCubicTo(@Self, AX1, AY1, AX2, AY2, AX3, AY3));
end;

class operator TBLPath.Equal(const ALeft: TBLPath;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

procedure TBLPath.EllipticArcTo(const ARX, ARY, AXAxisRotation: Double;
  const ALargeArcFlag, ASweepFlag: Boolean; const AX1, AY1: Double);
begin
  _BLCheck(_blPathEllipticArcTo(@Self, ARX, ARY, AXAxisRotation, ALargeArcFlag,
    ASweepFlag, AX1, AY1));
end;

procedure TBLPath.EllipticArcTo(const ARP: TBLPoint;
  const AXAxisRotation: Double; const ALargeArcFlag, ASweepFlag: Boolean;
  const AP1: TBLPoint);
begin
  _BLCheck(_blPathEllipticArcTo(@Self, ARP.X, ARP.Y, AXAxisRotation,
    ALargeArcFlag, ASweepFlag, AP1.X, AP1.Y));
end;

class operator TBLPath.Equal(const ALeft, ARight: TBLPath): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLPath.Equals(const AOther: TBLPath): Boolean;
begin
  Result := _blPathEquals(@Self, @AOther);
end;

class operator TBLPath.Finalize(var ADest: TBLPath);
begin
  _BLCheck(_blPathDestroy(@ADest));
end;

procedure TBLPath.FitTo(const ARange: TBLRange; const ARect: TBLRect);
begin
  _BLCheck(_blPathFitTo(@Self, @ARange, @ARect, 0));
end;

procedure TBLPath.FitTo(const ARect: TBLRect);
begin
  _BLCheck(_blPathFitTo(@Self, nil, @ARect, 0));
end;

function TBLPath.GetBoundingBox: TBLBox;
begin
  _BLCheck(_blPathGetBoundingBox(@Self, @Result));
end;

function TBLPath.GetCapacity: NativeInt;
begin
  Result := _blPathGetCapacity(@Self);
end;

function TBLPath.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double; out ADistanceOut: Double): NativeInt;
begin
  _BLCheck(_blPathGetClosestVertex(@Self, @AP, AMaxDistance, @Result, @ADistanceOut));
end;

function TBLPath.GetClosestVertex(const AP: TBLPoint;
  const AMaxDistance: Double): NativeInt;
begin
  var DistanceOut: Double;
  _BLCheck(_blPathGetClosestVertex(@Self, @AP, AMaxDistance, @Result, @DistanceOut));
end;

function TBLPath.GetCommandData: PByte;
begin
  Result := _blPathGetCommandData(@Self);
end;

function TBLPath.GetCommandDataEnd: PByte;
begin
  Result := _blPathGetCommandData(@Self);
  Inc(Result, Size);
end;

function TBLPath.GetControlBox: TBLBox;
begin
  _BLCheck(_blPathGetControlBox(@Self, @Result));
end;

function TBLPath.GetFigureRange(const AIndex: Integer): TBLRange;
begin
  _BLCheck(_blPathGetFigureRange(@Self, AIndex, @Result));
end;

function TBLPath.GetInfoFlags: TBLPathFlags;
begin
  _BLCheck(_blPathGetInfoFlags(@Self, @Result));
end;

function TBLPath.GetIsEmpty: Boolean;
begin
  Result := (Size = 0);
end;

function TBLPath.GetLastVertex: TBLPoint;
begin
  _BLCheck(_blPathGetLastVertex(@Self, @Result));
end;

function TBLPath.GetSize: NativeInt;
begin
  Result := _blPathGetSize(@Self);
end;

function TBLPath.GetVertexData: PBLPoint;
begin
  Result := _blPathGetVertexData(@Self);
end;

function TBLPath.GetVertexDataEnd: PBLPoint;
begin
  Result := _blPathGetVertexData(@Self);
  Inc(Result, Size);
end;

function TBLPath.HitTest(const AP: TBLPoint;
  const AFillRule: TBLFillRule): TBLHitTest;
begin
  Result := TBLHitTest(_blPathHitTest(@Self, @AP, Ord(AFillRule)));
end;

class operator TBLPath.Initialize(out ADest: TBLPath);
begin
  _BLCheck(_blPathInit(@ADest));
end;

procedure TBLPath.LineTo(const AX1, AY1: Double);
begin
  _BLCheck(_blPathLineTo(@Self, AX1, AY1));
end;

procedure TBLPath.LineTo(const AP1: TBLPoint);
begin
  _BLCheck(_blPathLineTo(@Self, AP1.X, AP1.Y));
end;

procedure TBLPath.ModifyOp(const AOp: TBLModifyOp; const ASize: NativeInt;
  out ACmdDataOut: PByte; out AVertexDataOut: PBLPoint);
begin
  _BLCheck(_blPathModifyOp(@Self, Ord(AOp), ASize, @ACmdDataOut, @AVertexDataOut));
end;

procedure TBLPath.MoveTo(const AP0: TBLPoint);
begin
  _BLCheck(_blPathMoveTo(@Self, AP0.X, AP0.Y));
end;

procedure TBLPath.MoveTo(const AX0, AY0: Double);
begin
  _BLCheck(_blPathMoveTo(@Self, AX0, AY0));
end;

class operator TBLPath.NotEqual(const ALeft: TBLPath;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLPath.NotEqual(const ALeft, ARight: TBLPath): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLPath.PolyTo(const APoly: TArray<TBLPoint>);
begin
  _BLCheck(_blPathPolyTo(@Self, Pointer(APoly), Length(APoly)));
end;

procedure TBLPath.QuadTo(const AX1, AY1, AX2, AY2: Double);
begin
  _BLCheck(_blPathQuadTo(@Self, AX1, AY1, AX2, AY2));
end;

procedure TBLPath.QuadTo(const AP1, AP2: TBLPoint);
begin
  _BLCheck(_blPathQuadTo(@Self, AP1.X, AP1.Y, AP2.X, AP2.Y));
end;

procedure TBLPath.PolyTo(const APoly: PBLPoint; const ACount: NativeInt);
begin
  _BLCheck(_blPathPolyTo(@Self, APoly, ACount));
end;

procedure TBLPath.RemoveRange(const ARange: TBLRange);
begin
  _BLCheck(_blPathRemoveRange(@Self, @ARange));
end;

procedure TBLPath.Reserve(const AMinCapacity: NativeInt);
begin
  _BLCheck(_blPathReserve(@Self, AMinCapacity));
end;

procedure TBLPath.Reset;
begin
  _BLCheck(_blPathReset(@Self));
end;

procedure TBLPath.SetVertexAt(const AIndex: NativeInt; const ACmd: TBLPathCmd;
  const AX, AY: Double);
begin
  _BLCheck(_blPathSetVertexAt(@Self, AIndex, Ord(ACmd), AX, AY));
end;

procedure TBLPath.SetVertexAt(const AIndex: NativeInt; const ACmd: TBLPathCmd;
  const APt: TBLPoint);
begin
  _BLCheck(_blPathSetVertexAt(@Self, AIndex, Ord(ACmd), APt.X, APt.Y));
end;

procedure TBLPath.Shrink;
begin
  _BLCheck(_blPathShrink(@Self));
end;

procedure TBLPath.SmoothCubicTo(const AX2, AY2, AX3, AY3: Double);
begin
  _BLCheck(_blPathSmoothCubicTo(@Self, AX2, AY2, AX3, AY3));
end;

procedure TBLPath.SmoothCubicTo(const AP2, AP3: TBLPoint);
begin
  _BLCheck(_blPathSmoothCubicTo(@Self, AP2.X, AP2.Y, AP3.X, AP3.Y));
end;

procedure TBLPath.SmoothQuadTo(const AX2, AY2: Double);
begin
  _BLCheck(_blPathSmoothQuadTo(@Self, AX2, AY2));
end;

procedure TBLPath.SmoothQuadTo(const AP2: TBLPoint);
begin
  _BLCheck(_blPathSmoothQuadTo(@Self, AP2.X, AP2.Y));
end;

procedure TBLPath.Swap(var AOther: TBLPath);
begin
  FBase.Swap(AOther.FBase);
end;

procedure TBLPath.Transform(const ARange: TBLRange; const AM: TBLMatrix2D);
begin
  _BLCheck(_blPathTransform(@Self, @ARange, @AM));
end;

procedure TBLPath.Transform(const AM: TBLMatrix2D);
begin
  _BLCheck(_blPathTransform(@Self, nil, @AM));
end;

procedure TBLPath.Translate(const ARange: TBLRange; const AP: TBLPoint);
begin
  _BLCheck(_blPathTranslate(@Self, @ARange, @AP));
end;

procedure TBLPath.Translate(const AP: TBLPoint);
begin
  _BLCheck(_blPathTranslate(@Self, nil, @AP));
end;

function TBLPath.View: TBLPathView;
type
  PBLPathView = ^TBLPathview;
begin
  Result := PBLPathView(FBase.FImpl)^;
end;

{$ENDREGION 'Geometries'}

{$REGION 'Containers'}

{ TBLArrayView<T> }

class constructor TBLArrayView<T>.Create;
begin
  Assert(not IsManagedType(T));
  Assert(not System.HasWeakRef(T));
end;

function TBLArrayView<T>.First: P;
begin
  Result := FData;
end;

function TBLArrayView<T>.GetItem(const AIndex: NativeInt): T;
begin
  Assert(NativeUInt(AIndex) < NativeUInt(FSize));
  Result := FData[AIndex];
end;

function TBLArrayView<T>.GetRef(const AIndex: NativeInt): P;
begin
  Assert(NativeUInt(AIndex) < NativeUInt(FSize));
  Result := FData + AIndex;
end;

class operator TBLArrayView<T>.Initialize(out ADest: TBLArrayView<T>);
begin
  ADest.FData := nil;
  ADest.FSize := 0;
end;

function TBLArrayView<T>.Last: P;
begin
  Result := FData + FSize - 1;
end;

procedure TBLArrayView<T>.Reset;
begin
  FData := nil;
  FSize := 0;
end;

procedure TBLArrayView<T>.Reset(const ADataIn: P; const ASizeIn: NativeInt);
begin
  FData := ADataIn;
  FSize := ASizeIn;
end;

{ TBLRange }

class operator TBLRange.Equal(const ALeft, ARight: TBLRange): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRange.Equals(const AOther: TBLRange): Boolean;
begin
  Result := (Start = AOther.Start) and (Stop = AOther.Stop);
end;

class operator TBLRange.NotEqual(const ALeft, ARight: TBLRange): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRange.Reset(const AStart, AStop: NativeInt);
begin
  Start := AStart;
  Stop := AStop;
end;

procedure TBLRange.Reset;
begin
  Start := 0;
  Stop := 0;
end;

{ TBLArray<T> }

class operator TBLArray<T>.Assign(var ADest: TBLArray<T>;
  const [ref] ASrc: TBLArray<T>);
begin
  _BLCheck(_blArrayInitWeak(@ADest, @ASrc));
end;

procedure TBLArray<T>.Append(const AItems: array of T);
begin
  Modify(TBLModifyOp.AppendGrow, AItems);
end;

procedure TBLArray<T>.AppendData(const AItems: P; const ASize: NativeInt);
begin
  _BLCheck(_blArrayAppendData(@Self, AItems, ASize));
end;

procedure TBLArray<T>.AppendData(const AView: TBLArrayView<T>);
begin
  _BLCheck(_blArrayAppendData(@Self, AView.FData, AView.FSize));
end;

procedure TBLArray<T>.Append(const AItem: T);
begin
  _BLCheck(_blArrayAppendItem(@Self, @AItem));
end;

procedure TBLArray<T>.Assign(const AItems: array of T);
begin
  Modify(TBLModifyOp.AssignFit, AItems);
end;

procedure TBLArray<T>.AssignData(const AView: TBLArrayView<T>);
begin
  _BLCheck(_blArrayAssignData(@Self, AView.FData, AView.FSize));
end;

procedure TBLArray<T>.AssignData(const AItems: P; const ASize: NativeInt);
begin
  _BLCheck(_blArrayAssignData(@Self, AItems, ASize));
end;

procedure TBLArray<T>.AssignDeep(const AOther: TBLArray<T>);
begin
  _BLCheck(_blArrayAssignDeep(@Self, @AOther));
end;

procedure TBLArray<T>.AssignExternalData(const AData: P; const ASize,
  ACapacity: NativeInt; const AAccessFlags: TBLDataAccessFlags;
  const ADestroyFunc: TBLDestroyExternalDataFunc; const AUserData: Pointer);
begin
  _BLCheck(_blArrayAssignExternalData(@Self, AData, ASize, ACapacity,
    Ord(AAccessFlags), ADestroyFunc, AUserData));
end;

function TBLArray<T>.At(const AIndex: NativeInt): T;
begin
  Assert(NativeUInt(AIndex) < NativeUInt(Size));
  var Data := P(_blArrayGetData(@Self));
  Result := Data[AIndex];
end;

procedure TBLArray<T>.Clear;
begin
  _BLCheck(_blArrayClear(@Self));
end;

class constructor TBLArray<T>.Create;
begin
  Assert(not IsManagedType(T));
  Assert(not System.HasWeakRef(T));

  FArrayType := TBLObjectType.Null;

  var Info := PTypeInfo(TypeInfo(T));
  var Data := GetTypeData(Info);

  case Info.Kind of
    tkInteger,
    tkEnumeration:
      case Data.OrdType of
        otSByte: FArrayType := TBLObjectType.ArrayInt8;
        otUByte: FArrayType := TBLObjectType.ArrayUInt8;
        otSWord: FArrayType := TBLObjectType.ArrayInt16;
        otUWord: FArrayType := TBLObjectType.ArrayUInt16;
        otSLong: FArrayType := TBLObjectType.ArrayInt32;
        otULong: FArrayType := TBLObjectType.ArrayUInt32;
      end;

    tkInt64:
      if (Data.MaxInt64Value > Data.MinInt64Value) then
        FArrayType := TBLObjectType.ArrayInt64
      else
        FArrayType := TBLObjectType.ArrayUInt64;

    tkFloat:
      case Data.FloatType of
        ftSingle: FArrayType := TBLObjectType.ArrayFloat32;
        ftDouble: FArrayType := TBLObjectType.ArrayFloat64;
      end;

    tkRecord:
      begin
        case SizeOf(T) of
          1: FArrayType := TBLObjectType.ArrayStruct1;
          2: FArrayType := TBLObjectType.ArrayStruct2;
          3: FArrayType := TBLObjectType.ArrayStruct3;
          4: FArrayType := TBLObjectType.ArrayStruct4;
          6: FArrayType := TBLObjectType.ArrayStruct6;
          8: FArrayType := TBLObjectType.ArrayStruct8;
         10: FArrayType := TBLObjectType.ArrayStruct10;
         12: FArrayType := TBLObjectType.ArrayStruct12;
         16: begin
               { Assume array of 16-byte record }
               FArrayType := TBLObjectType.ArrayStruct16;

               { Check if this record is compatible with TBLObject. That is, it
                 has a single field with name 'FBase' of type TBLObjectCore. }
               var P := PByte(Data.ManagedFldCount);
               Inc(P, SizeOf(Integer)); // Skip ManagedFldCount field

               // Skip ManagedFields
               var Count := Data.ManagedFldCount;
               Inc(P, Count * SizeOf(TManagedField));

               Count := P^; // NumOps
               Inc(P); // Skip NumOps field
               Inc(P, Count * SizeOf(Pointer)); // Skip RecOps

               Count := PInteger(P)^; // RecFldCnt
               if (Count = 1) then
               begin
                 Inc(P, SizeOf(Integer)); // Skip RecFldCnt
                 var RecType := PRecordTypeField(P);
                 if (RecType.NameFld.ToString = 'FBase') then
                   FArrayType := TBLObjectType.ArrayObject;
               end;
             end;
         20: FArrayType := TBLObjectType.ArrayStruct20;
         24: FArrayType := TBLObjectType.ArrayStruct24;
         32: FArrayType := TBLObjectType.ArrayStruct32;
        end;
      end;
  end;
  Assert(FArrayType <> TBLObjectType.Null);

//  var SsoCapacity := TBLObjectCore.STATIC_DATA_SIZE div SizeOf(T);
//  FSsoEmptySignature := TBLObjectCore.PackTypeWithMarker(FArrayType)
//    or TBLObjectCore.PackAbcp(0, SsoCapacity);
end;

class operator TBLArray<T>.Equal(const ALeft: TBLArray<T>;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.Size = 0) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLArray<T>.Equal(const ALeft, ARight: TBLArray<T>): Boolean;
begin
  Result := _blArrayEquals(@ALeft, @ARight);
end;

function TBLArray<T>.Equals(const AOther: TBLArray<T>): Boolean;
begin
  Result := _blArrayEquals(@Self, @AOther);
end;

class operator TBLArray<T>.Finalize(var ADest: TBLArray<T>);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blArrayDestroy(@ADest));
end;

function TBLArray<T>.First: P;
begin
  Result := RefAt(0);
end;

function TBLArray<T>.GetCapacity: NativeInt;
begin
  Result := _blArrayGetCapacity(@Self);
end;

function TBLArray<T>.GetData: P;
begin
  Result := _blArrayGetData(@Self);
end;

function TBLArray<T>.GetIsEmpty: Boolean;
begin
  Result := (Size = 0);
end;

function TBLArray<T>.GetItem(const AIndex: NativeInt): T;
begin
  Result := At(AIndex);
end;

function TBLArray<T>.GetRef(const AIndex: NativeInt): P;
begin
  Result := RefAt(AIndex);
end;

function TBLArray<T>.GetSize: NativeInt;
begin
  Result := _blArrayGetSize(@Self);
end;

function TBLArray<T>.IndexOf(const AItem: T): NativeInt;
begin
  Result := IndexOf(AItem, 0);
end;

function TBLArray<T>.IndexOf(const AItem: T;
  const AFromIndex: NativeInt): NativeInt;
begin
  var P := GetData;
  var IEnd := GetSize;
  for var I: NativeInt := AFromIndex to IEnd - 1 do
  begin
    if (_TBLGenericUtils<T>.AreEqual(P^, AItem)) then
      Exit(I);
    Inc(P);
  end;
  Result := -1;
end;

class operator TBLArray<T>.Initialize(out ADest: TBLArray<T>);
begin
  _BLCheck(_blArrayInit(@ADest, Ord(FArrayType)));
end;

procedure TBLArray<T>.Insert(const AIndex: NativeInt; const AItems: array of T);
begin
  var Dst: P;
  _BLCheck(_blArrayInsertOp(@Self, AIndex, Length(AItems), @Dst));
  _TBLGenericUtils<T>.CopyToUnitialized(Dst, AItems, FArrayType);
end;

procedure TBLArray<T>.InsertData(const AIndex: NativeInt; const AItems: P;
  const ASize: NativeInt);
begin
  _BLCheck(_blArrayInsertData(@Self, AIndex, AItems, ASize));
end;

procedure TBLArray<T>.InsertData(const AIndex: NativeInt;
  const AView: TBLArrayView<T>);
begin
  _BLCheck(_blArrayInsertData(@Self, AIndex, AView.FData, AView.FSize));
end;

procedure TBLArray<T>.Insert(const AIndex: NativeInt; const AItem: T);
begin
  _BLCheck(_blArrayInsertItem(@Self, AIndex, @AItem));
end;

function TBLArray<T>.InsertOp(const AIndex, ASize: NativeInt): P;
begin
  _BLCheck(_blArrayInsertOp(@Self, AIndex, ASize, @Result));
end;

function TBLArray<T>.Last: P;
begin
  Result := RefAt(Size - 1);
end;

function TBLArray<T>.LastIndexOf(const AItem: T;
  const AFromIndex: NativeInt): NativeInt;
begin
  var P := GetData;
  var I := GetSize - 1;
  if (I < 0) then
    Exit(-1);

  I := Min(I, AFromIndex);
  Inc(P, I);
  while (not _TBLGenericUtils<T>.AreEqual(P^, AItem)) and (I >= 0) do
  begin
    Dec(P);
    Dec(I);
  end;

  Result := I;
end;

function TBLArray<T>.LastIndexOf(const AItem: T): NativeInt;
begin
  var P := GetData;
  var Count := GetSize;
  Inc(P, Count - 1);
  for var I: NativeInt := Count - 1 downto 0 do
  begin
    if (_TBLGenericUtils<T>.AreEqual(P^, AItem)) then
      Exit(I);
    Dec(P);
  end;
  Result := -1;
end;

function TBLArray<T>.MakeMutable: P;
begin
  _BLCheck(_blArrayMakeMutable(@Self, @Result));
end;

procedure TBLArray<T>.Modify(const AOp: TBLModifyOp; const AItems: array of T);
begin
  var Dst: P;
  _BLCheck(_blArrayModifyOp(@Self, Ord(AOp), Length(AItems), @Dst));
  _TBLGenericUtils<T>.CopyToUnitialized(Dst, AItems, FArrayType);
end;

function TBLArray<T>.ModifyOp(const AOp: TBLModifyOp;
  const ASize: NativeInt): P;
begin
  _BLCheck(_blArrayModifyOp(@Self, Ord(AOp), ASize, @Result));
end;

class operator TBLArray<T>.NotEqual(const ALeft: TBLArray<T>;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.Size = 0) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLArray<T>.NotEqual(const ALeft, ARight: TBLArray<T>): Boolean;
begin
  Result := not _blArrayEquals(@ALeft, @ARight);
end;

procedure TBLArray<T>.Prepend(const AItem: T);
begin
  _BLCheck(_blArrayInsertItem(@Self, 0, @AItem));
end;

procedure TBLArray<T>.Prepend(const AItems: array of T);
begin
  Insert(0, AItems);
end;

procedure TBLArray<T>.PrependData(const AItems: P; const ASize: NativeInt);
begin
  _BLCheck(_blArrayInsertData(@Self, 0, AItems, ASize));
end;

procedure TBLArray<T>.PrependData(const AView: TBLArrayView<T>);
begin
  _BLCheck(_blArrayInsertData(@Self, 0, AView.FData, AView.FSize));
end;

function TBLArray<T>.RefAt(const AIndex: NativeInt): P;
begin
  Assert(NativeUInt(AIndex) < NativeUInt(Size));
  var Data := P(_blArrayGetData(@Self));
  Result := Data + AIndex;
end;

procedure TBLArray<T>.Remove(const ARange: TBLRange);
begin
  _BLCheck(_blArrayRemoveRange(@Self, ARange.Start, ARange.Stop));
end;

procedure TBLArray<T>.Remove(const AIndex: NativeInt);
begin
  _BLCheck(_blArrayRemoveIndex(@Self, AIndex));
end;

procedure TBLArray<T>.Replace(const AIndex: NativeInt; const AItem: T);
begin
  _BLCheck(_blArrayReplaceItem(@Self, AIndex, @AItem));
end;

procedure TBLArray<T>.ReplaceData(const ARange: TBLRange; const AItems: P;
  const ASize: NativeInt);
begin
  _BLCheck(_blArrayReplaceData(@Self, ARange.Start, ARange.Stop, AItems, ASize));
end;

procedure TBLArray<T>.ReplaceData(const ARange: TBLRange;
  const AView: TBLArrayView<T>);
begin
  _BLCheck(_blArrayReplaceData(@Self, ARange.Start, ARange.Stop, AView.FData, AView.FSize));
end;

procedure TBLArray<T>.Reserve(const AMinCapacity: NativeInt);
begin
  _BLCheck(_blArrayReserve(@Self, AMinCapacity));
end;

procedure TBLArray<T>.Reset;
begin
  _BLCheck(_blArrayReset(@Self));
end;

procedure TBLArray<T>.Resize(const ASize: NativeInt; const AFill: T);
begin
  _BLCheck(_blArrayResize(@Self, ASize, @AFill));
end;

procedure TBLArray<T>.SetItem(const AIndex: NativeInt; const AValue: T);
begin
  _BLCheck(_blArrayReplaceItem(@Self, AIndex, @AValue));
end;

procedure TBLArray<T>.Shrink;
begin
  _BLCheck(_blArrayShrink(@Self));
end;

procedure TBLArray<T>.Swap(var AOther: TBLArray<T>);
begin
  FBase.Swap(AOther.FBase);
end;

procedure TBLArray<T>.Truncate(const AMaxSize: NativeInt);
begin
  _BLCheck(_blArrayResize(@Self, Min(Size, AMaxSize), nil));
end;

function TBLArray<T>.View: TBLArrayView<T>;
begin
  Result.Reset(Data, Size);
end;

{ TBLString }

class operator TBLString.Assign(var ADest: TBLString; const [ref] ASrc: TBLString);
begin
  _BLCheck(_blStringInitWeak(@ADest, @ASrc));
end;

constructor TBLString.Create(const AView: TBLStringView);
begin
  _BLCheck(_blStringInitWithData(@Self, Pointer(AView.FData), AView.FSize));
end;

constructor TBLString.Create(const AStr: PUTF8Char; const ASize: NativeInt);
begin
  _BLCheck(_blStringInitWithData(@Self, AStr, ASize));
end;

function TBLString.Compare(const AOther: TBLString): Integer;
begin
  Result := _blStringCompare(@Self, @AOther);
end;

function TBLString.Compare(const AOther: TBLStringView): Integer;
begin
  Result := _blStringCompareData(@Self, Pointer(AOther.FData), AOther.FSize);
end;

function TBLString.Compare(const AOther: PUTF8Char;
  const ASize: NativeInt): Integer;
begin
  Result := _blStringCompareData(@Self, AOther, ASize);
end;

function TBLString.Compare(const AOther: UTF8String): Integer;
begin
  Result := Compare(PUTF8Char(AOther), Length(AOther));
end;

procedure TBLString.Assign(const AChar: UTF8Char; const ACount: NativeInt);
begin
  _BLCheck(_blStringApplyOpChar(@Self, Ord(TBLModifyOp.AssignFit), AChar, ACount));
end;

procedure TBLString.Assign(const AView: TBLStringView);
begin
  _BLCheck(_blStringAssignData(@Self, Pointer(AView.FData), AView.FSize));
end;

procedure TBLString.Append(const AChar: UTF8Char; const ACount: NativeInt);
begin
  _BLCheck(_blStringApplyOpChar(@Self, Ord(TBLModifyOp.AppendGrow), AChar, ACount));
end;

procedure TBLString.Append(const AOther: TBLString);
begin
  _BLCheck(_blStringApplyOpString(@Self, Ord(TBLModifyOp.AppendGrow), @AOther));
end;

procedure TBLString.Append(const AView: TBLStringView);
begin
  _BLCheck(_blStringApplyOpData(@Self, Ord(TBLModifyOp.AppendGrow), Pointer(AView.FData), AView.FSize));
end;

procedure TBLString.Append(const AStr: PUTF8Char; const ASize: NativeInt);
begin
  _BLCheck(_blStringApplyOpData(@Self, Ord(TBLModifyOp.AppendGrow), AStr, ASize));
end;

procedure TBLString.Append(const AStr: UTF8String);
begin
  _BLCheck(_blStringApplyOpData(@Self, Ord(TBLModifyOp.AppendGrow), PUTF8Char(AStr), Length(AStr)));
end;

procedure TBLString.Append(const AStr: String);
begin
  Append(UTF8String(AStr));
end;

procedure TBLString.AppendFormat(const AFmt: String;
  const AArgs: array of const);
begin
  Append(Format(AFmt, AArgs));
end;

procedure TBLString.Assign(const AStr: PUTF8Char; const ASize: NativeInt);
begin
  _BLCheck(_blStringAssignData(@Self, AStr, ASize));
end;

procedure TBLString.AssignDeep(const AOther: TBLString);
begin
  _BLCheck(_blStringAssignDeep(@Self, @AOther));
end;

procedure TBLString.AssignFormat(const AFmt: String;
  const AArgs: array of const);
begin
  var Formatted := Format(AFmt, AArgs);
  var S := UTF8String(Formatted);
  _BLCheck(_blStringAssignData(@Self, PUTF8Char(S), Length(S)));
end;

function TBLString.At(const AIndex: NativeInt): UTF8Char;
begin
  Assert(NativeUInt(AIndex) < NativeUInt(GetSize));
  Result := Data[AIndex];
end;

procedure TBLString.Clear;
begin
  _BLCheck(_blStringClear(@Self));
end;

function TBLString.Compare(const AOther: String): Integer;
begin
  var S := UTF8String(AOther);
  Result := Compare(PUTF8Char(S), Length(S));
end;

constructor TBLString.Create(const AStr: UTF8String);
begin
  _BLCheck(_blStringInitWithData(@Self, Pointer(AStr), Length(AStr)));
end;

constructor TBLString.Create(const AStr: String);
begin
  var S := UTF8String(AStr);
  _BLCheck(_blStringInitWithData(@Self, Pointer(S), Length(S)));
end;

class operator TBLString.Equal(const ALeft: TBLString;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLString.Equal(const ALeft, ARight: TBLString): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

class operator TBLString.Equal(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

class operator TBLString.Equal(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

class operator TBLString.Equal(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

class operator TBLString.Equal(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLString.Equals(const AOther: PUTF8Char;
  const ASize: NativeInt): Boolean;
begin
  Result := _blStringEqualsData(@Self, AOther, ASize);
end;

function TBLString.Equals(const AOther: TBLStringView): Boolean;
begin
  Result := _blStringEqualsData(@Self, Pointer(AOther.FData), AOther.FSize);
end;

function TBLString.Equals(const AOther: TBLString): Boolean;
begin
  Result := _blStringEquals(@Self, @AOther);
end;

function TBLString.Equals(const AOther: UTF8String): Boolean;
begin
  Result := Equals(PUTF8Char(AOther), Length(AOther));
end;

function TBLString.Equals(const AOther: String): Boolean;
begin
  var S := UTF8String(AOther);
  Result := Equals(PUTF8Char(S), Length(S));
end;

class operator TBLString.Finalize(var ADest: TBLString);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blStringDestroy(@ADest));
end;

function TBLString.GetCapacity: NativeInt;
begin
  Result := _blStringGetCapacity(@Self);
end;

function TBLString.GetChar(const AIndex: NativeInt): UTF8Char;
begin
  Result := At(AIndex);
end;

function TBLString.GetData: PUTF8Char;
begin
  Result := _blStringGetData(@Self);
end;

function TBLString.GetIsEmpty: Boolean;
begin
  Result := (Size = 0);
end;

function TBLString.GetSize: NativeInt;
begin
  Result := _blStringGetSize(@Self);
end;

class operator TBLString.GreaterThan(const ALeft, ARight: TBLString): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLString.GreaterThan(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLString.GreaterThan(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLString.GreaterThan(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLString.GreaterThan(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLString.GreaterThanOrEqual(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

class operator TBLString.GreaterThanOrEqual(const ALeft,
  ARight: TBLString): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

class operator TBLString.GreaterThanOrEqual(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

class operator TBLString.GreaterThanOrEqual(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

class operator TBLString.GreaterThanOrEqual(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

class operator TBLString.Implicit(const AStr: String): TBLString;
begin
  var S := UTF8String(AStr);
  _BLCheck(_blStringInitWithData(@Result, Pointer(S), Length(S)));
end;

class operator TBLString.Implicit(const AStr: UTF8String): TBLString;
begin
  _BLCheck(_blStringInitWithData(@Result, Pointer(AStr), Length(AStr)));
end;

function TBLString.IndexOf(const AChar: UTF8Char): NativeInt;
begin
  Result := IndexOf(AChar, 0);
end;

function TBLString.IndexOf(const AChar: UTF8Char;
  const AFromIndex: NativeInt): NativeInt;
begin
  var IEnd := GetSize;
  var P := GetData;

  for var I := AFromIndex to IEnd - 1 do
  begin
    if (P[I] = AChar) then
      Exit(I);
  end;

  Result := -1;
end;

class operator TBLString.Initialize(out ADest: TBLString);
begin
  _BLCheck(_blStringInit(@ADest));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AView: TBLStringView);
begin
  _BLCheck(_blStringInsertData(@Self, AIndex, PUTF8Char(AView.FData), AView.FSize));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AOther: TBLString);
begin
  _BLCheck(_blStringInsertString(@Self, AIndex, @AOther));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AChar: UTF8Char;
  const ACount: NativeInt);
begin
  _BLCheck(_blStringInsertChar(@Self, AIndex, AChar, ACount));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AStr: String);
begin
  Insert(AIndex, UTF8String(AStr));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AStr: UTF8String);
begin
  _BLCheck(_blStringInsertData(@Self, AIndex, PUTF8Char(AStr), Length(AStr)));
end;

procedure TBLString.Insert(const AIndex: NativeInt; const AStr: PUTF8Char;
  const ASize: NativeInt);
begin
  _BLCheck(_blStringInsertData(@Self, AIndex, AStr, ASize));
end;

function TBLString.InsertOp(const AIndex, ASize: NativeInt): PUTF8Char;
begin
  _BLCheck(_blStringInsertOp(@Self, AIndex, ASize, @Result));
end;

class operator TBLString.LessThan(const ALeft, ARight: TBLString): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

class operator TBLString.LessThan(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

class operator TBLString.LessThan(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

class operator TBLString.LessThan(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

function TBLString.LastIndexOf(const AChar: UTF8Char): NativeInt;
begin
  var I := GetSize - 1;
  var P := GetData;

  while (I >= 0) and (P[I] <> AChar) do
    Dec(I);

  Result := I;
end;

function TBLString.LastIndexOf(const AChar: UTF8Char;
  const AFromIndex: NativeInt): NativeInt;
begin
  var I := GetSize - 1;
  var P := GetData;

  if (I < 0) then
    Exit(-1);

  I := Min(I, AFromIndex);
  while (P[I] <> AChar) and (I >= 0) do
    Dec(I);

  Result := I;
end;

class operator TBLString.LessThan(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

class operator TBLString.LessThanOrEqual(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

class operator TBLString.LessThanOrEqual(const ALeft,
  ARight: TBLString): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

class operator TBLString.LessThanOrEqual(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

class operator TBLString.LessThanOrEqual(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

class operator TBLString.LessThanOrEqual(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

function TBLString.MakeMutable: PUTF8Char;
begin
  _BLCheck(_blStringMakeMutable(@Self, @Result));
end;

function TBLString.ModifyOp(const AOp: TBLModifyOp;
  const ASize: NativeInt): PUTF8Char;
begin
  _BLCheck(_blStringModifyOp(@Self, Ord(AOp), ASize, @Result));
end;

class operator TBLString.NotEqual(const ALeft: TBLString;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLString.NotEqual(const ALeft, ARight: TBLString): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

class operator TBLString.NotEqual(const ALeft: TBLString;
  const ARight: TBLStringView): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

class operator TBLString.NotEqual(const ALeft: TBLString;
  const ARight: PUTF8Char): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

class operator TBLString.NotEqual(const ALeft: TBLString;
  const ARight: UTF8String): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

class operator TBLString.NotEqual(const ALeft: TBLString;
  const ARight: String): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLString.Prepend(const AView: TBLStringView);
begin
  _BLCheck(_blStringInsertData(@Self, 0, Pointer(AView.FData), AView.FSize));
end;

procedure TBLString.Prepend(const AOther: TBLString);
begin
  _BLCheck(_blStringInsertString(@Self, 0, @AOther));
end;

procedure TBLString.Prepend(const AChar: UTF8Char; const ACount: NativeInt);
begin
  _BLCheck(_blStringInsertChar(@Self, 0, AChar, ACount));
end;

procedure TBLString.Prepend(const AStr: String);
begin
  Prepend(UTF8String(AStr));
end;

procedure TBLString.Prepend(const AStr: UTF8String);
begin
  _BLCheck(_blStringInsertData(@Self, 0, PUTF8Char(AStr), Length(AStr)));
end;

procedure TBLString.Prepend(const AStr: PUTF8Char; const ASize: NativeInt);
begin
  _BLCheck(_blStringInsertData(@Self, 0, AStr, ASize));
end;

procedure TBLString.Remove(const AIndex: NativeInt);
begin
  _BLCheck(_blStringRemoveIndex(@Self, AIndex));
end;

procedure TBLString.Remove(const ARange: TBLRange);
begin
  _BLCheck(_blStringRemoveRange(@Self, ARange.Start, ARange.Stop));
end;

procedure TBLString.Reserve(const AMinSize: NativeInt);
begin
  _BLCheck(_blStringReserve(@Self, AMinSize));
end;

procedure TBLString.Reset;
begin
  _BLCheck(_blStringReset(@Self));
end;

procedure TBLString.Resize(const ASize: NativeInt; const AFill: UTF8Char);
begin
  _BLCheck(_blStringResize(@Self, ASize, AFill));
end;

procedure TBLString.Shrink;
begin
  _BLCheck(_blStringShrink(@Self));
end;

procedure TBLString.Swap(var AOther: TBLString);
begin
  FBase.Swap(AOther.FBase);
end;

procedure TBLString.Truncate(const ASize: NativeInt);
begin
  _BLCheck(_blStringResize(@Self, ASize, #0));
end;

function TBLString.View: TBLStringView;
begin
  Result.Reset(Pointer(Data), Size);
end;

{ TBLBitArray }

procedure TBLBitArray.AppendBit(const ABitValue: Boolean);
begin
  _BLCheck(_blBitArrayAppendBit(@Self, ABitValue));
end;

procedure TBLBitArray.AppendWord(const AWordValue: UInt32);
begin
  _BLCheck(_blBitArrayAppendWord(@Self, AWordValue));
end;

procedure TBLBitArray.AppendWords(const AWordData: PUInt32;
  const AWordCount: Integer);
begin
  _BLCheck(_blBitArrayAppendWords(@Self, AWordData, AWordCount));
end;

procedure TBLBitArray.AppendWords(const AWordData: TArray<UInt32>);
begin
  _BLCheck(_blBitArrayAppendWords(@Self, Pointer(AWordData), Length(AWordData)));
end;

class operator TBLBitArray.Assign(var ADest: TBLBitArray;
  const [ref] ASrc: TBLBitArray);
begin
  _BLCheck(_blBitArrayInitWeak(@ADest, @ASrc));
end;

class operator TBLBitArray.Equal(const ALeft: TBLBitArray;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

procedure TBLBitArray.AssignWords(const AWordData: PUInt32;
  const AWordCount: Integer);
begin
  _BLCheck(_blBitArrayAssignWords(@Self, AWordData, AWordCount));
end;

procedure TBLBitArray.AssignWords(const AWordData: TArray<UInt32>);
begin
  _BLCheck(_blBitArrayAssignWords(@Self, Pointer(AWordData), Length(AWordData)));
end;

function TBLBitArray.CardinalityInRange(const AStartBit,
  AEndBit: Integer): Integer;
begin
  Result := _blBitArrayGetCardinalityInRange(@Self, AStartBit, AEndBit);
end;

procedure TBLBitArray.Clear;
begin
  _BLCheck(_blBitArrayClear(@Self));
end;

procedure TBLBitArray.ClearBit(const ABitIndex: Integer);
begin
  _BLCheck(_blBitArrayClearBit(@Self, ABitIndex));
end;

procedure TBLBitArray.ClearRange(const AStartBit, AEndBit: Integer);
begin
  _BLCheck(_blBitArrayClearRange(@Self, AStartBit, AEndBit));
end;

procedure TBLBitArray.ClearWord(const ABitIndex: Integer;
  const AWordValue: UInt32);
begin
  _BLCheck(_blBitArrayClearWord(@Self, ABitIndex, AWordValue));
end;

procedure TBLBitArray.ClearWords(const ABitIndex: Integer;
  const AWordData: PUInt32; const AWordCount: UInt32);
begin
  _BLCheck(_blBitArrayClearWords(@Self, ABitIndex, AWordData, AWordCount));
end;

procedure TBLBitArray.ClearWords(const ABitIndex: Integer;
  const AWordData: TArray<UInt32>);
begin
  _BLCheck(_blBitArrayClearWords(@Self, ABitIndex, Pointer(AWordData), Length(AWordData)));
end;

function TBLBitArray.Compare(const AOther: TBLBitArray): Integer;
begin
  Result := _blBitArrayCompare(@Self, @AOther);
end;

class operator TBLBitArray.Equal(const ALeft, ARight: TBLBitArray): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLBitArray.Equals(const AOther: TBLBitArray): Boolean;
begin
  Result := _blBitArrayEquals(@Self, @AOther);
end;

procedure TBLBitArray.FillRange(const AStartBit, AEndBit: Integer);
begin
  _BLCheck(_blBitArrayFillRange(@Self, AStartBit, AEndBit));
end;

procedure TBLBitArray.FillWords(const ABitIndex: Integer;
  const AWordData: PUInt32; const AWordCount: UInt32);
begin
  _BLCheck(_blBitArrayFillWords(@Self, ABitIndex, AWordData, AWordCount));
end;

procedure TBLBitArray.FillWords(const ABitIndex: Integer;
  const AWordData: TArray<UInt32>);
begin
  _BLCheck(_blBitArrayFillWords(@Self, ABitIndex, Pointer(AWordData), Length(AWordData)));
end;

class operator TBLBitArray.Finalize(var ADest: TBLBitArray);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blBitArrayDestroy(@ADest));
end;

function TBLBitArray.GetBit(const AIndex: Integer): Boolean;
begin
  Result := _blBitArrayHasBit(@Self, AIndex);
end;

function TBLBitArray.GetCapacity: Integer;
begin
  Result := _blBitArrayGetCapacity(@Self);
end;

function TBLBitArray.GetCardinality: Integer;
begin
  Result := _blBitArrayGetCardinality(@Self);
end;

function TBLBitArray.GetData: PUInt32;
begin
  Result := _blBitArrayGetData(@Self);
end;

function TBLBitArray.GetIsEmpty: Boolean;
begin
  Result := _blBitArrayIsEmpty(@Self);
end;

function TBLBitArray.GetSize: Integer;
begin
  Result := _blBitArrayGetSize(@Self);
end;

function TBLBitArray.GetWordCount: Integer;
begin
  Result := _blBitArrayGetWordCount(@Self);
end;

class operator TBLBitArray.GreaterThan(const ALeft,
  ARight: TBLBitArray): Boolean;
begin
  Result := (ALeft.Compare(ARight) > 0);
end;

class operator TBLBitArray.GreaterThanOrEqual(const ALeft,
  ARight: TBLBitArray): Boolean;
begin
  Result := (ALeft.Compare(ARight) >= 0);
end;

function TBLBitArray.HasBit(const ABitIndex: Integer): Boolean;
begin
  Result := _blBitArrayHasBit(@Self, ABitIndex);
end;

function TBLBitArray.HasBitsInRange(const AStartBit, AEndBit: Integer): Boolean;
begin
  Result := _blBitArrayHasBitsInRange(@Self, AStartBit, AEndBit);
end;

class operator TBLBitArray.Initialize(out ADest: TBLBitArray);
begin
  _BLCheck(_blBitArrayInit(@ADest));
end;

function TBLBitArray.Intersects(const AOther: TBLBitArray): Boolean;
begin
  Result := _blBitArrayIntersects(@Self, @AOther);
end;

class operator TBLBitArray.LessThan(const ALeft, ARight: TBLBitArray): Boolean;
begin
  Result := (ALeft.Compare(ARight) < 0);
end;

class operator TBLBitArray.LessThanOrEqual(const ALeft,
  ARight: TBLBitArray): Boolean;
begin
  Result := (ALeft.Compare(ARight) <= 0);
end;

class operator TBLBitArray.NotEqual(const ALeft: TBLBitArray;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLBitArray.NotEqual(const ALeft, ARight: TBLBitArray): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLBitArray.ReplaceBit(const ABitIndex: Integer;
  const ABitValue: Boolean);
begin
  _BLCheck(_blBitArrayReplaceBit(@Self, ABitIndex, ABitValue));
end;

function TBLBitArray.ReplaceOp(const ANumBits: Integer): PUInt32;
begin
  _BLCheck(_blBitArrayReplaceOp(@Self, ANumBits, @Result));
end;

procedure TBLBitArray.ReplaceWord(const ABitIndex: Integer;
  const AWordValue: UInt32);
begin
  _BLCheck(_blBitArrayReplaceWord(@Self, ABitIndex, AWordValue));
end;

procedure TBLBitArray.ReplaceWords(const ABitIndex: Integer;
  const AWordData: PUInt32; const AWordCount: UInt32);
begin
  _BLCheck(_blBitArrayReplaceWords(@Self, ABitIndex, AWordData, AWordCount));
end;

procedure TBLBitArray.ReplaceWords(const ABitIndex: Integer;
  const AWordData: TArray<UInt32>);
begin
  _BLCheck(_blBitArrayReplaceWords(@Self, ABitIndex, Pointer(AWordData), Length(AWordData)));
end;

procedure TBLBitArray.Reserve(const ANumBits: Integer);
begin
  _BLCheck(_blBitArrayReserve(@Self, ANumBits));
end;

procedure TBLBitArray.Reset;
begin
  _BLCheck(_blBitArrayReset(@Self));
end;

procedure TBLBitArray.Resize(const ANumBits: Integer);
begin
  _BLCheck(_blBitArrayResize(@Self, ANumBits));
end;

procedure TBLBitArray.SetBit(const ABitIndex: Integer);
begin
  _BLCheck(_blBitArraySetBit(@Self, ABitIndex));
end;

procedure TBLBitArray.SetBit(const AIndex: Integer; const AValue: Boolean);
begin
  _BLCheck(_blBitArrayReplaceBit(@Self, AIndex, AValue));
end;

procedure TBLBitArray.Shrink;
begin
  _BLCheck(_blBitArrayShrink(@Self));
end;

function TBLBitArray.Subsumes(const AOther: TBLBitArray): Boolean;
begin
  Result := _blBitArraySubsumes(@Self, @AOther);
end;

procedure TBLBitArray.Swap(var AOther: TBLBitArray);
begin
  FBase.Swap(AOther.FBase);
end;

{$ENDREGION 'Containers'}

{$REGION 'Styling'}

{ TBLRgba32 }

function BLRgba32: TBLRgba32; overload; inline;
begin
  Result.Reset;
end;

function BLRgba32(const ARgba32: UInt32): TBLRgba32; overload; inline;
begin
  Result.Reset(ARgba32);
end;

function BLRgba32(const ARgba64: TBLRgba64): TBLRgba32; overload; inline;
begin
  Result.Reset(ARgba64);
end;

function BLRgba32(const AR, AG, AB: Byte; const AA: Byte = $FF): TBLRgba32; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLMin(const AA, AB: TBLRgba32): TBLRgba32; overload; inline;
begin
  Result.Reset(Min(AA.R, AB.R), Min(AA.G, AB.G), Min(AA.B, AB.B), Min(AA.A, AB.A));
end;

function BLMax(const AA, AB: TBLRgba32): TBLRgba32; overload; inline;
begin
  Result.Reset(Max(AA.R, AB.R), Max(AA.G, AB.G), Max(AA.B, AB.B), Max(AA.A, AB.A));
end;

constructor TBLRgba32.Create(const ARgba32: UInt32);
begin
  Value := ARgba32;
end;

constructor TBLRgba32.Create(const AR, AG, AB, AA: Byte);
begin
  Value := (AA shl 24) or (AR shl 16) or (AG shl 8) or AB;
end;

class function TBLRgba32.Create: TBLRgba32;
begin
  Result.Value := 0;
end;

class operator TBLRgba32.Equal(const ALeft, ARight: TBLRgba32): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRgba32.Equals(const AOther: TBLRgba32): Boolean;
begin
  Result := (Value = AOther.Value);
end;

function TBLRgba32.GetA: Byte;
begin
  Result := Value shr 24;
end;

function TBLRgba32.GetB: Byte;
begin
  Result := Value;
end;

function TBLRgba32.GetG: Byte;
begin
  Result := Value shr 8;
end;

function TBLRgba32.GetIsOpaque: Boolean;
begin
  Result := (Value >= $FF000000);
end;

function TBLRgba32.GetIsTransparent: Boolean;
begin
  Result := (Value <= $00FFFFFF);
end;

function TBLRgba32.GetR: Byte;
begin
  Result := Value shr 16;
end;

class operator TBLRgba32.Implicit(const AValue: TBLRgba32): UInt32;
begin
  Result := AValue.Value;
end;

class operator TBLRgba32.Implicit(const AValue: UInt32): TBLRgba32;
begin
  Result.Value := AValue;
end;

class operator TBLRgba32.NotEqual(const ALeft, ARight: TBLRgba32): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRgba32.Reset;
begin
  Value := 0;
end;

procedure TBLRgba32.Reset(const AR, AG, AB, AA: Byte);
begin
  Value := (AA shl 24) or (AR shl 16) or (AG shl 8) or AB;
end;

procedure TBLRgba32.Reset(const ARgba32: UInt32);
begin
  Value := ARgba32;
end;

procedure TBLRgba32.SetA(const AValue: Byte);
begin
  Value := (Value and $00FFFFFF) or (AValue shl 24);
end;

procedure TBLRgba32.SetB(const AValue: Byte);
begin
  Value := (Value and $FFFFFF00) or AValue;
end;

procedure TBLRgba32.SetG(const AValue: Byte);
begin
  Value := (Value and $FFFF00FF) or (AValue shl 8);
end;

procedure TBLRgba32.SetR(const AValue: Byte);
begin
  Value := (Value and $FF00FFFF) or (AValue shl 16);
end;

{ _TBLRgba32Helper }

constructor _TBLRgba32Helper.Create(const ARgba64: TBLRgba64);
begin
  Reset(ARgba64);
end;

procedure _TBLRgba32Helper.Reset(const ARgba64: TBLRgba64);
begin
  Reset(ARgba64.Value shr 40,
        ARgba64.Value shr 24,
        ARgba64.Value shr  8,
        ARgba64.Value shr 56);
end;

{ TBLRgba64 }

function BLRgba64: TBLRgba64; overload; inline;
begin
  Result.Reset;
end;

function BLRgba64(const ARgba64: UInt64): TBLRgba64; overload; inline;
begin
  Result.Reset(ARgba64);
end;

function BLRgba64(const ARgba32: TBLRgba32): TBLRgba64; overload; inline;
begin
  Result.Reset(ARgba32);
end;

function BLRgba64(const AR, AG, AB: Word; const AA: Word = $FFFF): TBLRgba64; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLMin(const AA, AB: TBLRgba64): TBLRgba64; overload; inline;
begin
  Result.Reset(Min(AA.R, AB.R), Min(AA.G, AB.G), Min(AA.B, AB.B), Min(AA.A, AB.A));
end;

function BLMax(const AA, AB: TBLRgba64): TBLRgba64; overload; inline;
begin
  Result.Reset(Max(AA.R, AB.R), Max(AA.G, AB.G), Max(AA.B, AB.B), Max(AA.A, AB.A));
end;

constructor TBLRgba64.Create(const ARgba64: UInt64);
begin
  Value := ARgba64;
end;

constructor TBLRgba64.Create(const ARgba32: TBLRgba32);
begin
  Reset(ARgba32);
end;

constructor TBLRgba64.Create(const AR, AG, AB, AA: Word);
begin
  Value := (UInt64(AA) shl 48) or (AR shl 32) or (AG shl 16) or AB;
end;

class function TBLRgba64.Create: TBLRgba64;
begin
  Result.Value := 0;
end;

class operator TBLRgba64.Equal(const ALeft, ARight: TBLRgba64): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRgba64.Equals(const AOther: TBLRgba64): Boolean;
begin
  Result := (Value = AOther.Value);
end;

function TBLRgba64.GetA: Word;
begin
  Result := Value shr 48;
end;

function TBLRgba64.GetB: Word;
begin
  Result := Value;
end;

function TBLRgba64.GetG: Word;
begin
  Result := Value shr 16;
end;

function TBLRgba64.GetIsOpaque: Boolean;
begin
  Result := (Value >= $FFFF000000000000);
end;

function TBLRgba64.GetIsTransparent: Boolean;
begin
  Result := (Value <= $0000FFFFFFFFFFFF);
end;

function TBLRgba64.GetR: Word;
begin
  Result := Value shr 32;
end;

class operator TBLRgba64.Implicit(const AValue: TBLRgba64): UInt64;
begin
  Result := AValue.Value;
end;

class operator TBLRgba64.Implicit(const AValue: UInt64): TBLRgba64;
begin
  Result.Value := AValue;
end;

class operator TBLRgba64.NotEqual(const ALeft, ARight: TBLRgba64): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRgba64.Reset(const AR, AG, AB, AA: Word);
begin
  Value := (UInt64(AA) shl 48) or (AR shl 32) or (AG shl 16) or AB;
end;

procedure TBLRgba64.Reset(const ARgba32: TBLRgba32);
begin
  Value := (UInt64(ARgba32.A) shl 48) or (ARgba32.R shl 32) or (ARgba32.G shl 16) or ARgba32.B;
  Value := Value * $0101;
end;

procedure TBLRgba64.Reset(const ARgba64: UInt64);
begin
  Value := ARgba64;
end;

procedure TBLRgba64.Reset;
begin
  Value := 0;
end;

procedure TBLRgba64.SetA(const AValue: Word);
begin
  Value := (Value and $0000FFFFFFFFFFFF) or (UInt64(AValue) shl 48);
end;

procedure TBLRgba64.SetB(const AValue: Word);
begin
  Value := (Value and $FFFFFFFFFFFF0000) or AValue;
end;

procedure TBLRgba64.SetG(const AValue: Word);
begin
  Value := (Value and $FFFFFFFF0000FFFF) or (AValue shl 16);
end;

procedure TBLRgba64.SetR(const AValue: Word);
begin
  Value := (Value and $FFFF0000FFFFFFFF) or (AValue shl 32);
end;

{ TBLRgba }

function BLRgba: TBLRgba; overload; inline;
begin
  Result.Reset;
end;

function BLRgba(const ARgba32: TBLRgba32): TBLRgba; overload; inline;
begin
  Result.Reset(ARgba32);
end;

function BLRgba(const ARgba64: TBLRgba64): TBLRgba; overload; inline;
begin
  Result.Reset(ARgba64);
end;

function BLRgba(const AR, AG, AB: Single; const AA: Single = 1): TBLRgba; overload; inline;
begin
  Result.Reset(AR, AG, AB, AA);
end;

function BLMin(const AA, AB: TBLRgba): TBLRgba; overload; inline;
begin
  Result.Reset(Min(AA.R, AB.R), Min(AA.G, AB.G), Min(AA.B, AB.B), Min(AA.A, AB.A));
end;

function BLMax(const AA, AB: TBLRgba): TBLRgba; overload; inline;
begin
  Result.Reset(Max(AA.R, AB.R), Max(AA.G, AB.G), Max(AA.B, AB.B), Max(AA.A, AB.A));
end;

class function TBLRgba.Create: TBLRgba;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

constructor TBLRgba.Create(const AR, AG, AB, AA: Single);
begin
  R := AR;
  G := AG;
  B := AB;
  A := AA;
end;

constructor TBLRgba.Create(const ARgba32: TBLRgba32);
begin
  Reset(ARgba32);
end;

constructor TBLRgba.Create(const ARgba64: TBLRgba64);
begin
  Reset(ARgba64);
end;

class operator TBLRgba.Equal(const ALeft, ARight: TBLRgba): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLRgba.Equals(const AOther: TBLRgba32): Boolean;
begin
  var Other: TBLRgba;
  Other.Reset(AOther);
  Result := Equals(Other);
end;

function TBLRgba.Equals(const AOther: TBLRgba64): Boolean;
begin
  var Other: TBLRgba;
  Other.Reset(AOther);
  Result := Equals(Other);
end;

function TBLRgba.Equals(const AR, AG, AB, AA: Single): Boolean;
begin
  Result := (R = AR) and (G = AG) and (B = AB) and (A = AA);
end;

function TBLRgba.GetIsOpaque: Boolean;
begin
  Result := (A >= 1);
end;

function TBLRgba.GetIsTransparent: Boolean;
begin
  Result := (A <= 0);
end;

function TBLRgba.Equals(const AOther: TBLRgba): Boolean;
begin
  Result := (R = AOther.R) and (G = AOther.G) and (B = AOther.B) and (A = AOther.A);
end;

class operator TBLRgba.NotEqual(const ALeft, ARight: TBLRgba): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLRgba.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TBLRgba.Reset(const ARgba32: TBLRgba32);
const
  FACTOR: Single = 1 / $FF;
begin
  R := ARgba32.R * FACTOR;
  G := ARgba32.G * FACTOR;
  B := ARgba32.B * FACTOR;
  A := ARgba32.A * FACTOR;
end;

procedure TBLRgba.Reset(const ARgba64: TBLRgba64);
const
  FACTOR: Single = 1 / $FFFF;
begin
  R := ARgba64.R * FACTOR;
  G := ARgba64.G * FACTOR;
  B := ARgba64.B * FACTOR;
  A := ARgba64.A * FACTOR;
end;

procedure TBLRgba.Reset(const AR, AG, AB, AA: Single);
begin
  R := AR;
  G := AG;
  B := AB;
  A := AA;
end;

function TBLRgba.ToRgba32: TBLRgba32;
begin
  Result.Reset(Trunc(EnsureRange(R, 0, 1) * $FF + 0.5),
               Trunc(EnsureRange(G, 0, 1) * $FF + 0.5),
               Trunc(EnsureRange(B, 0, 1) * $FF + 0.5),
               Trunc(EnsureRange(A, 0, 1) * $FF + 0.5));
end;

function TBLRgba.ToRgba64: TBLRgba64;
begin
  Result.Reset(Trunc(EnsureRange(R, 0, 1) * $FFFF + 0.5),
               Trunc(EnsureRange(G, 0, 1) * $FFFF + 0.5),
               Trunc(EnsureRange(B, 0, 1) * $FFFF + 0.5),
               Trunc(EnsureRange(A, 0, 1) * $FFFF + 0.5));
end;

{ TBLGradientStop }

function BLGradientStop: TBLGradientStop; overload; inline;
begin
  Result.Reset;
end;

function BLGradientStop(const AOffset: Double; const ARgba32: TBLRgba32): TBLGradientStop; overload; inline;
begin
  Result.Reset(AOffset, ARgba32);
end;

function BLGradientStop(const AOffset: Double; const ARgba64: TBLRgba64): TBLGradientStop; overload; inline;
begin
  Result.Reset(AOffset, ARgba64);
end;

class function TBLGradientStop.Create: TBLGradientStop;
begin
  Result.Offset := 0;
  Result.Rgba := 0;
end;

constructor TBLGradientStop.Create(const AOffset: Double;
  const ARgba32: TBLRgba32);
begin
  Offset := AOffset;
  Rgba.Reset(ARgba32);
end;

constructor TBLGradientStop.Create(const AOffset: Double;
  const ARgba64: TBLRgba64);
begin
  Offset := AOffset;
  Rgba := ARgba64;
end;

class operator TBLGradientStop.Equal(const ALeft,
  ARight: TBLGradientStop): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLGradientStop.Equals(const AOther: TBLGradientStop): Boolean;
begin
  Result := (Offset = AOther.Offset) and (Rgba = AOther.Rgba);
end;

class operator TBLGradientStop.NotEqual(const ALeft,
  ARight: TBLGradientStop): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLGradientStop.Reset;
begin
  Offset := 0;
  Rgba := 0;
end;

procedure TBLGradientStop.Reset(const AOffset: Double;
  const ARgba32: TBLRgba32);
begin
  Offset := AOffset;
  Rgba.Reset(ARgba32);
end;

procedure TBLGradientStop.Reset(const AOffset: Double;
  const ARgba64: TBLRgba64);
begin
  Offset := AOffset;
  Rgba := ARgba64;
end;

{ TBLLinearGradientValues }

function BLLinearGradientValues: TBLLinearGradientValues; overload; inline;
begin
  Result.Reset;
end;

function BLLinearGradientValues(const AX0, AY0, AX1, AY1: Double): TBLLinearGradientValues; overload; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1);
end;

class function TBLLinearGradientValues.Create: TBLLinearGradientValues;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

constructor TBLLinearGradientValues.Create(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

procedure TBLLinearGradientValues.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TBLLinearGradientValues.Reset(const AX0, AY0, AX1, AY1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
end;

{ TBLRadialGradientValues }

function BLRadialGradientValues: TBLRadialGradientValues; overload; inline;
begin
  Result.Reset;
end;

function BLRadialGradientValues(const AX0, AY0, AX1, AY1, AR0: Double;
  const AR1: Double = 0): TBLRadialGradientValues; overload; inline;
begin
  Result.Reset(AX0, AY0, AX1, AY1, AR0, AR1);
end;

class function TBLRadialGradientValues.Create: TBLRadialGradientValues;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

constructor TBLRadialGradientValues.Create(const AX0, AY0, AX1, AY1, AR0,
  AR1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
  R0 := AR0;
  R1 := AR1;
end;

procedure TBLRadialGradientValues.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TBLRadialGradientValues.Reset(const AX0, AY0, AX1, AY1, AR0,
  AR1: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  X1 := AX1;
  Y1 := AY1;
  R0 := AR0;
  R1 := AR1;
end;

{ TBLConicGradientValues }

function BLConicGradientValues: TBLConicGradientValues; overload; inline;
begin
  Result.Reset;
end;

function BLConicGradientValues(const AX0, AY0, AAngle: Double;
  const ARepeat: Double = 1): TBLConicGradientValues; overload; inline;
begin
  Result.Reset(AX0, AY0, AAngle, ARepeat);
end;

class function TBLConicGradientValues.Create: TBLConicGradientValues;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

constructor TBLConicGradientValues.Create(const AX0, AY0, AAngle,
  ARepeat: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  Angle := AAngle;
  Repetition := ARepeat;
end;

procedure TBLConicGradientValues.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TBLConicGradientValues.Reset(const AX0, AY0, AAngle, ARepeat: Double);
begin
  X0 := AX0;
  Y0 := AY0;
  Angle := AAngle;
  Repetition := ARepeat;
end;

{ TBLGradient }

class operator TBLGradient.Assign(var ADest: TBLGradient;
  const [ref] ASrc: TBLGradient);
begin
  _BLCheck(_blGradientInitWeak(@ADest, @ASrc));
end;

class operator TBLGradient.Equal(const ALeft: TBLGradient;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLGradient.Equal(const ALeft, ARight: TBLGradient): Boolean;
begin
  Result := _blGradientEquals(@ALeft, @ARight);
end;

class operator TBLGradient.Finalize(var ADest: TBLGradient);
begin
  _BLCheck(_blGradientDestroy(@ADest));
end;

function TBLGradient.GetIsEmpty: Boolean;
begin
  Result := (PImpl(FBase.FImpl).Size = 0);
end;

class operator TBLGradient.Initialize(out ADest: TBLGradient);
begin
  _BLCheck(_blGradientInit(@ADest));
end;

class operator TBLGradient.NotEqual(const ALeft: TBLGradient;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsEmpty) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLGradient.NotEqual(const ALeft, ARight: TBLGradient): Boolean;
begin
  Result := not _blGradientEquals(@ALeft, @ARight);
end;

{$ENDREGION 'Styling'}

{$REGION 'Imaging'}

{ TBLFormatInfo }

procedure TBLFormatInfo.AddFlags(const AFlags: TBLFormatFlags);
begin
  FFlags := FFlags + AFlags;
end;

procedure TBLFormatInfo.ClearFlags(const AFlags: TBLFormatFlags);
begin
  FFlags := FFlags - AFlags;
end;

class operator TBLFormatInfo.Equal(const ALeft, ARight: TBLFormatInfo): Boolean;
begin
  Result := CompareMem(@ALeft, @ARight, SizeOf(TBLFormatInfo));
end;

function TBLFormatInfo.GetPalette: PBLRgba32;
type
  PPBLRgba32 = ^PBLRgba32;
begin
  var P := PPBLRgba32(@FSizes);
  Result := P^;
end;

function TBLFormatInfo.GetShift(const AIndex: Integer): Byte;
begin
  Assert(Cardinal(AIndex) < 4);
  Result := FSizes[AIndex];
end;

function TBLFormatInfo.GetSize(const AIndex: Integer): Byte;
begin
  Assert(Cardinal(AIndex) < 4);
  Result := FSizes[AIndex];
end;

function TBLFormatInfo.HasFlag(const AFlag: TBLFormatFlag): Boolean;
begin
  Result := (AFlag in FFlags);
end;

procedure TBLFormatInfo.Init(const ADepth: Integer;
  const AFlags: TBLFormatFlags; const ASizes, AShifts: TBLFourBytes);
begin
  FDepth := ADepth;
  FFlags := AFlags;
  FSizes := ASizes;
  FShifts := AShifts;
end;

class operator TBLFormatInfo.NotEqual(const ALeft,
  ARight: TBLFormatInfo): Boolean;
begin
  Result := not CompareMem(@ALeft, @ARight, SizeOf(TBLFormatInfo));
end;

procedure TBLFormatInfo.Query(const AFormat: TBLFormat);
begin
  _BLCheck(_blFormatInfoQuery(@Self, Ord(AFormat)));
end;

procedure TBLFormatInfo.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TBLFormatInfo.Sanitize;
begin
  _BLCheck(_blFormatInfoSanitize(@Self));
end;

procedure TBLFormatInfo.SetShifts(const AR, AG, AB, AA: Byte);
begin
  FShifts[0] := AR;
  FShifts[1] := AG;
  FShifts[2] := AB;
  FShifts[3] := AA;
end;

procedure TBLFormatInfo.SetSizes(const AR, AG, AB, AA: Byte);
begin
  FSizes[0] := AR;
  FSizes[1] := AG;
  FSizes[2] := AB;
  FSizes[3] := AA;
end;

{ _TBLFormatHelper }

function _TBLFormatHelper.Info: TBLFormatInfo;
begin
  _BLCheck(_blFormatInfoQuery(@Result, Ord(Self)));
end;

{ TBLPixelConverter }

class operator TBLPixelConverter.Assign(var ADest: TBLPixelConverter;
  const [ref] ASrc: TBLPixelConverter);
begin
  _BLCheck(_blPixelConverterInitWeak(@ADest, @ASrc));
end;

procedure TBLPixelConverter.ConvertRect(const ADstData: Pointer;
  const ADstStride: NativeInt; const ASrcData: Pointer;
  const ASrcStride: NativeInt; const AWidth, AHeight: Integer;
  const AOptions: TBLPixelConverterOptions);
begin
  _BLCheck(_blPixelConverterConvert(@Self, ADstData, ADstStride, ASrcData,
    ASrcStride, AWidth, AHeight, @AOptions));
end;

procedure TBLPixelConverter.ConvertRect(const ADstData: Pointer;
  const ADstStride: NativeInt; const ASrcData: Pointer;
  const ASrcStride: NativeInt; const AWidth, AHeight: Integer);
begin
  _BLCheck(_blPixelConverterConvert(@Self, ADstData, ADstStride, ASrcData,
    ASrcStride, AWidth, AHeight, nil));
end;

procedure TBLPixelConverter.ConvertSpan(const ADstData, ASrcData: Pointer;
  const AWidth: Integer; const AOptions: TBLPixelConverterOptions);
begin
  _BLCheck(_blPixelConverterConvert(@Self, ADstData, 0, ASrcData, 0, AWidth, 1, @AOptions));
end;

procedure TBLPixelConverter.ConvertSpan(const ADstData, ASrcData: Pointer;
  const AWidth: Integer);
begin
  _BLCheck(_blPixelConverterConvert(@Self, ADstData, 0, ASrcData, 0, AWidth, 1, nil));
end;

class operator TBLPixelConverter.Finalize(var ADest: TBLPixelConverter);
begin
  _BLCheck(_blPixelConverterDestroy(@ADest));
end;

function TBLPixelConverter.GetIsInitialized: Boolean;
begin
  Result := (FCore.InternalFlags <> 0);
end;

class operator TBLPixelConverter.Initialize(out ADest: TBLPixelConverter);
begin
  _BLCheck(_blPixelConverterInit(@ADest));
end;

procedure TBLPixelConverter.Make(const ADstInfo, ASrcInfo: TBLFormatInfo;
  const ACreateFlags: TBLPixelConverterCreateFlags);
begin
  _BLCheck(_blPixelConverterCreate(@Self, @ADstInfo, @ASrcInfo, Byte(ACreateFlags)));
end;

procedure TBLPixelConverter.Reset;
begin
  _BLCheck(_blPixelConverterReset(@Self));
end;

{ TBLImageInfo }

function TBLImageInfo.GetCompression: String;
begin
  Result := String(UTF8String(PUTF8Char(@FCompression)));
end;

function TBLImageInfo.GetFlags: TBLImageInfoFlags;
begin
  Byte(Result) := FFlags;
end;

function TBLImageInfo.GetFormat: String;
begin
  Result := String(UTF8String(PUTF8Char(@FFormat)));
end;

procedure TBLImageInfo.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TBLImageDecoder }

class operator TBLImageDecoder.Assign(var ADest: TBLImageDecoder;
  const [ref] ASrc: TBLImageDecoder);
begin
  _BLCheck(_blImageDecoderInitWeak(@ADest, @ASrc));
end;

class operator TBLImageDecoder.Equal(const ALeft: TBLImageDecoder;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLImageDecoder.Equal(const ALeft,
  ARight: TBLImageDecoder): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLImageDecoder.Equals(const AOther: TBLImageDecoder): Boolean;
begin
  Result := (FBase.FImpl = AOther.FBase.FImpl);
end;

class operator TBLImageDecoder.Finalize(var ADest: TBLImageDecoder);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blImageDecoderDestroy(@ADest));
end;

function TBLImageDecoder.GetBufferIndex: NativeInt;
begin
  Result := PImpl(FBase.FImpl).BufferIndex;
end;

function TBLImageDecoder.GetFrameIndex: Int64;
begin
  Result := PImpl(FBase.FImpl).FrameIndex;
end;

function TBLImageDecoder.GetIsValid: Boolean;
begin
  Result := (LastResult <> TBLResult.NotInitialized);
end;

function TBLImageDecoder.GetLastResult: TBLResult;
begin
  Result := TBLResult(PImpl(FBase.FImpl).LastResult);
end;

class operator TBLImageDecoder.Initialize(out ADest: TBLImageDecoder);
begin
  _BLCheck(_blImageDecoderInit(@ADest));
end;

class operator TBLImageDecoder.NotEqual(const ALeft: TBLImageDecoder;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLImageDecoder.NotEqual(const ALeft,
  ARight: TBLImageDecoder): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLImageDecoder.ReadInfo(out ADst: TBLImageInfo;
  const ABuffer: TBLArray<Byte>);
begin
  _BLCheck(_blImageDecoderReadInfo(@Self, @ADst, ABuffer.Data, ABuffer.Size));
end;

procedure TBLImageDecoder.ReadInfo(out ADst: TBLImageInfo;
  const ABuffer: TBytes);
begin
  _BLCheck(_blImageDecoderReadInfo(@Self, @ADst, ABuffer, Length(ABuffer)));
end;

procedure TBLImageDecoder.ReadInfo(out ADst: TBLImageInfo;
  const AView: TBLArrayView<Byte>);
begin
  _BLCheck(_blImageDecoderReadInfo(@Self, @ADst, AView.FData, AView.FSize));
end;

procedure TBLImageDecoder.ReadInfo(out ADst: TBLImageInfo; const AData: Pointer;
  const ASize: NativeInt);
begin
  _BLCheck(_blImageDecoderReadInfo(@Self, @ADst, AData, ASize));
end;

procedure TBLImageDecoder.Reset;
begin
  _BLCheck(_blImageDecoderReset(@Self));
end;

procedure TBLImageDecoder.Restart;
begin
  _BLCheck(_blImageDecoderRestart(@Self));
end;

procedure TBLImageDecoder.Swap(var AOther: TBLImageDecoder);
begin
  FBase.Swap(AOther.FBase);
end;

{ _TBLImageDecoderHelper }

function _TBLImageDecoderHelper.GetCodec: TBLImageCodec;
begin
  Result.FBase := PImpl(FBase.FImpl).Codec;
end;

procedure _TBLImageDecoderHelper.ReadFrame(out ADst: TBLImage;
  const ABuffer: TBLArray<Byte>);
begin
  _BLCheck(_blImageDecoderReadFrame(@Self, @ADst, ABuffer.Data, ABuffer.Size));
end;

procedure _TBLImageDecoderHelper.ReadFrame(out ADst: TBLImage;
  const ABuffer: TBytes);
begin
  _BLCheck(_blImageDecoderReadFrame(@Self, @ADst, ABuffer, Length(ABuffer)));
end;

procedure _TBLImageDecoderHelper.ReadFrame(out ADst: TBLImage;
  const AView: TBLArrayView<Byte>);
begin
  _BLCheck(_blImageDecoderReadFrame(@Self, @ADst, AView.FData, AView.FSize));
end;

procedure _TBLImageDecoderHelper.ReadFrame(out ADst: TBLImage;
  const AData: Pointer; const ASize: NativeInt);
begin
  _BLCheck(_blImageDecoderReadFrame(@Self, @ADst, AData, ASize));
end;

{ TBLImageEncoder }

class operator TBLImageEncoder.Assign(var ADest: TBLImageEncoder;
  const [ref] ASrc: TBLImageEncoder);
begin
  _BLCheck(_blImageEncoderInitWeak(@ADest, @ASrc));
end;

class operator TBLImageEncoder.Equal(const ALeft: TBLImageEncoder;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

class operator TBLImageEncoder.Equal(const ALeft,
  ARight: TBLImageEncoder): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLImageEncoder.Equals(const AOther: TBLImageEncoder): Boolean;
begin
  Result := (FBase.FImpl = AOther.FBase.FImpl);
end;

class operator TBLImageEncoder.Finalize(var ADest: TBLImageEncoder);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blImageEncoderDestroy(@ADest));
end;

function TBLImageEncoder.GetBufferIndex: NativeInt;
begin
  Result := PImpl(FBase.FImpl).BufferIndex;
end;

function TBLImageEncoder.GetFrameIndex: Int64;
begin
  Result := PImpl(FBase.FImpl).FrameIndex;
end;

function TBLImageEncoder.GetIsValid: Boolean;
begin
  Result := (LastResult <> TBLResult.NotInitialized);
end;

function TBLImageEncoder.GetLastResult: TBLResult;
begin
  Result := TBLResult(PImpl(FBase.FImpl).LastResult);
end;

class operator TBLImageEncoder.Initialize(out ADest: TBLImageEncoder);
begin
  _BLCheck(_blImageEncoderInit(@ADest));
end;

class operator TBLImageEncoder.NotEqual(const ALeft: TBLImageEncoder;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLImageEncoder.NotEqual(const ALeft,
  ARight: TBLImageEncoder): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLImageEncoder.Reset;
begin
  _BLCheck(_blImageEncoderReset(@Self));
end;

procedure TBLImageEncoder.Restart;
begin
  _BLCheck(_blImageEncoderRestart(@Self));
end;

procedure TBLImageEncoder.Swap(var AOther: TBLImageEncoder);
begin
  FBase.Swap(AOther.FBase);
end;

{ _TBLImageEncoderHelper }

function _TBLImageEncoderHelper.GetCodec: TBLImageCodec;
begin
  Result.FBase := PImpl(FBase.FImpl).Codec;
end;

procedure _TBLImageEncoderHelper.WriteFrame(const ADst: TBLArray<Byte>;
  const AImage: TBLImage);
begin
  _BLCheck(_blImageEncoderWriteFrame(@Self, @ADst, @AImage));
end;

function _TBLImageEncoderHelper.WriteFrame(const AImage: TBLImage): TBytes;
begin
  var Dst: TBLArray<Byte>;
  _BLCheck(_blImageEncoderWriteFrame(@Self, @Dst, @AImage));
  SetLength(Result, Dst.Size);
  Move(Dst.Data^, Result[0], Dst.Size);
end;

{ TBLImageCodec }

class procedure TBLImageCodec.AddToBuiltIn(const ACodec: TBLImageCodec);
begin
  _BLCheck(_blImageCodecAddToBuiltIn(@ACodec));
end;

class operator TBLImageCodec.Assign(var ADest: TBLImageCodec;
  const [ref] ASrc: TBLImageCodec);
begin
  _BLCheck(_blImageCodecInitWeak(@ADest, @ASrc));
end;

class operator TBLImageCodec.Equal(const ALeft: TBLImageCodec;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

function TBLImageCodec.CreateDecoder: TBLImageDecoder;
begin
  _BLCheck(_blImageCodecCreateDecoder(@Self, @Result));
end;

function TBLImageCodec.CreateEncoder: TBLImageEncoder;
begin
  _BLCheck(_blImageCodecCreateEncoder(@Self, @Result));
end;

class operator TBLImageCodec.Equal(const ALeft, ARight: TBLImageCodec): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLImageCodec.Equals(const AOther: TBLImageCodec): Boolean;
begin
  Result := (FBase.FImpl = AOther.FBase.FImpl);
end;

class operator TBLImageCodec.Finalize(var ADest: TBLImageCodec);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blImageCodecDestroy(@ADest));
end;

procedure TBLImageCodec.FindByData(const AView: TBLArrayView<Byte>;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, AView.FData, AView.FSize, @ACodecs));
end;

procedure TBLImageCodec.FindByData(const AView: TBLArrayView<Byte>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, AView.FData, AView.FSize, nil));
end;

procedure TBLImageCodec.FindByData(const AData: Pointer; const ASize: NativeInt;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, AData, ASize, @ACodecs));
end;

procedure TBLImageCodec.FindByData(const AData: Pointer;
  const ASize: NativeInt);
begin
  _BLCheck(_blImageCodecFindByData(@Self, AData, ASize, nil));
end;

procedure TBLImageCodec.FindByData(const ABuffer: TBytes;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, Pointer(ABuffer), Length(ABuffer), @ACodecs));
end;

procedure TBLImageCodec.FindByData(const ABuffer: TBytes);
begin
  _BLCheck(_blImageCodecFindByData(@Self, Pointer(ABuffer), Length(ABuffer), nil));
end;

procedure TBLImageCodec.FindByData(const ABuffer: TBLArray<Byte>;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, ABuffer.Data, ABuffer.Size, @ACodecs));
end;

procedure TBLImageCodec.FindByData(const ABuffer: TBLArray<Byte>);
begin
  _BLCheck(_blImageCodecFindByData(@Self, ABuffer.Data, ABuffer.Size, nil));
end;

procedure TBLImageCodec.FindByExtension(const AExt: String;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  var Name := UTF8String(AExt);
  _BLCheck(_blImageCodecFindByExtension(@Self, PUTF8Char(Name), Length(Name), @ACodecs));
end;

procedure TBLImageCodec.FindByExtension(const AExt: String);
begin
  var Name := UTF8String(AExt);
  _BLCheck(_blImageCodecFindByExtension(@Self, PUTF8Char(Name), Length(Name), nil));
end;

procedure TBLImageCodec.FindByExtension(const AExt: TBLStringView;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByExtension(@Self, Pointer(AExt.FData), AExt.FSize, @ACodecs));
end;

procedure TBLImageCodec.FindByExtension(const AExt: TBLStringView);
begin
  _BLCheck(_blImageCodecFindByExtension(@Self, Pointer(AExt.FData), AExt.FSize, nil));
end;

procedure TBLImageCodec.FindByName(const AName: String;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  var Name := UTF8String(AName);
  _BLCheck(_blImageCodecFindByName(@Self, PUTF8Char(Name), Length(Name), @ACodecs));
end;

procedure TBLImageCodec.FindByName(const AName: String);
begin
  var Name := UTF8String(AName);
  _BLCheck(_blImageCodecFindByName(@Self, PUTF8Char(Name), Length(Name), nil));
end;

procedure TBLImageCodec.FindByName(const AName: TBLStringView;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageCodecFindByName(@Self, Pointer(AName.FData), AName.FSize, @ACodecs));
end;

procedure TBLImageCodec.FindByName(const AName: TBLStringView);
begin
  _BLCheck(_blImageCodecFindByName(@Self, Pointer(AName.FData), AName.FSize, nil));
end;

class function TBLImageCodec.GetBuiltInCodecs: TBLArray<TBLImageCodec>;
begin
  _BLCheck(_blImageCodecArrayInitBuiltInCodecs(@Result));
end;

function TBLImageCodec.GetExtensions: TBLString;
begin
  Result.FBase := PImpl(FBase.FImpl).Extensions;
end;

function TBLImageCodec.GetFeatures: TBLImageCodecFeatures;
begin
  Cardinal(Result) := PImpl(FBase.FImpl).Features;
end;

function TBLImageCodec.GetIsValid: Boolean;
begin
  var Features := TBLImageCodecFeatures(PImpl(FBase.FImpl).Features);
  Result := ((Features * [TBLImageCodecFeature.Read, TBLImageCodecFeature.Write]) <> []);
end;

function TBLImageCodec.GetMimeType: TBLString;
begin
  Result.FBase := PImpl(FBase.FImpl).MimeType;
end;

function TBLImageCodec.GetName: TBLString;
begin
  Result.FBase := PImpl(FBase.FImpl).Name;
end;

function TBLImageCodec.GetVendor: TBLString;
begin
  Result.FBase := PImpl(FBase.FImpl).Vendor;
end;

function TBLImageCodec.HasFeature(
  const AFeature: TBLImageCodecFeature): Boolean;
begin
  var Features := TBLImageCodecFeatures(PImpl(FBase.FImpl).Features);
  Result := (AFeature in Features);
end;

class operator TBLImageCodec.Initialize(out ADest: TBLImageCodec);
begin
  _BLCheck(_blImageCodecInit(@ADest));
end;

function TBLImageCodec.InspectData(const ABuffer: TBLArray<Byte>): Cardinal;
begin
  Result := _blImageCodecInspectData(@Self, ABuffer.Data, ABuffer.Size);
end;

function TBLImageCodec.InspectData(const ABuffer: TBytes): Cardinal;
begin
  Result := _blImageCodecInspectData(@Self, ABuffer, Length(ABuffer));
end;

function TBLImageCodec.InspectData(const AView: TBLArrayView<Byte>): Cardinal;
begin
  Result := _blImageCodecInspectData(@Self, AView.FData, AView.FSize);
end;

function TBLImageCodec.InspectData(const AData: Pointer;
  const ASize: NativeInt): Cardinal;
begin
  Result := _blImageCodecInspectData(@Self, AData, ASize);
end;

class operator TBLImageCodec.NotEqual(const ALeft,
  ARight: TBLImageCodec): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

class procedure TBLImageCodec.RemoveFromBuiltIn(const ACodec: TBLImageCodec);
begin
  _BLCheck(_blImageCodecRemoveFromBuiltIn(@ACodec));
end;

procedure TBLImageCodec.Reset;
begin
  _BLCheck(_blImageCodecReset(@Self));
end;

procedure TBLImageCodec.Swap(var AOther: TBLImageCodec);
begin
  FBase.Swap(AOther.FBase);
end;

class operator TBLImageCodec.NotEqual(const ALeft: TBLImageCodec;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.IsValid) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

{ TBLImageData }

procedure TBLImageData.Reset;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TBLImage }

class operator TBLImage.Assign(var ADest: TBLImage; const [ref] ASrc: TBLImage);
begin
  _BLCheck(_blImageInitWeak(@ADest, @ASrc));
end;

procedure TBLImage.AssignDeep(const AOther: TBLImage);
begin
  _BLCheck(_blImageAssignDeep(@Self, @AOther));
end;

procedure TBLImage.AssignShallow(const AOther: TBLImage);
begin
  _BLCheck(_blImageAssignWeak(@Self, @AOther));
end;

procedure TBLImage.Convert(const AFormat: TBLFormat);
begin
  _BLCheck(_blImageConvert(@Self, Ord(AFormat)));
end;

constructor TBLImage.Create(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat);
begin
  _BLCheck(_blImageInitAs(@Self, AWidth, AHeight, Ord(AFormat)));
end;

class operator TBLImage.Equal(const ALeft, ARight: TBLImage): Boolean;
begin
  Result := ALeft.Equals(ARight);
end;

function TBLImage.Equals(const AOther: TBLImage): Boolean;
begin
  Result := _blImageEquals(@Self, @AOther);
end;

class operator TBLImage.Equal(const ALeft: TBLImage;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.Format = TBLFormat.None) then
    Result := (ARight = nil)
  else
    Result := (ARight <> nil);
end;

class operator TBLImage.Finalize(var ADest: TBLImage);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blImageDestroy(@ADest));
end;

procedure TBLImage.GetData(out ADataOut: TBLImageData);
begin
  _BLCheck(_blImageGetData(@Self, @ADataOut));
end;

function TBLImage.GetDepth: Integer;
begin
  Result := PImpl(FBase.FImpl).Depth;
end;

function TBLImage.GetFormat: TBLFormat;
begin
  Result := TBLFormat(PImpl(FBase.FImpl).Format);
end;

function TBLImage.GetHeight: Integer;
begin
  Result := PImpl(FBase.FImpl).Size.H;
end;

function TBLImage.GetIsEmpty: Boolean;
begin
  Result := (Format = TBLFormat.None);
end;

function TBLImage.GetSize: TBLSizeI;
begin
  Result := PImpl(FBase.FImpl).Size;
end;

function TBLImage.GetWidth: Integer;
begin
  Result := PImpl(FBase.FImpl).Size.W;
end;

class operator TBLImage.Initialize(out ADest: TBLImage);
begin
  _BLCheck(_blImageInit(@ADest));
end;

procedure TBLImage.Make(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat);
begin
  _BLCheck(_blImageCreate(@Self, AWidth, AHeight, Ord(AFormat)));
end;

procedure TBLImage.MakeFromData(const AWidth, AHeight: Integer;
  const AFormat: TBLFormat; const APixelData: Pointer; const AStride: IntPtr;
  const AAccessFlags: TBLDataAccessFlags;
  const ADestroyFunc: TBLDestroyExternalDataFunc; const AUserData: Pointer);
begin
  _BLCheck(_blImageCreateFromData(@Self, AWidth, AHeight, Ord(AFormat),
    APixelData, AStride, Byte(AAccessFlags), ADestroyFunc, AUserData));
end;

procedure TBLImage.MakeMutable(out ADataOut: TBLImageData);
begin
  _BLCheck(_blImageMakeMutable(@Self, @ADataOut));
end;

class operator TBLImage.NotEqual(const ALeft, ARight: TBLImage): Boolean;
begin
  Result := not ALeft.Equals(ARight);
end;

procedure TBLImage.ReadFromData(const AArray: TBLArray<Byte>;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageReadFromData(@Self, AArray.Data, AArray.Size, @ACodecs));
end;

procedure TBLImage.ReadFromData(const AArray: TBLArray<Byte>);
begin
  _BLCheck(_blImageReadFromData(@Self, AArray.Data, AArray.Size, nil));
end;

procedure TBLImage.ReadFromData(const AData: Pointer; const ASize: NativeInt;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageReadFromData(@Self, AData, ASize, @ACodecs));
end;

procedure TBLImage.ReadFromData(const AData: Pointer; const ASize: NativeInt);
begin
  _BLCheck(_blImageReadFromData(@Self, AData, ASize, nil));
end;

procedure TBLImage.ReadFromData(const AView: TBLArrayView<Byte>;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageReadFromData(@Self, AView.FData, AView.FSize, @ACodecs));
end;

procedure TBLImage.ReadFromData(const AView: TBLArrayView<Byte>);
begin
  _BLCheck(_blImageReadFromData(@Self, AView.FData, AView.FSize, nil));
end;

procedure TBLImage.ReadFromData(const AArray: TBytes;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageReadFromData(@Self, AArray, Length(AArray), @ACodecs));
end;

procedure TBLImage.ReadFromData(const AArray: TBytes);
begin
  _BLCheck(_blImageReadFromData(@Self, AArray, Length(AArray), nil));
end;

procedure TBLImage.ReadFromFile(const AFilename: String);
begin
  _BLCheck(_blImageReadFromFile(@Self, PUTF8Char(UTF8String(AFilename)), nil));
end;

procedure TBLImage.ReadFromFile(const AFilename: String;
  const ACodecs: TBLArray<TBLImageCodec>);
begin
  _BLCheck(_blImageReadFromFile(@Self, PUTF8Char(UTF8String(AFilename)), @ACodecs));
end;

procedure TBLImage.Reset;
begin
  _BLCheck(_blImageReset(@Self));
end;

class procedure TBLImage.Scale(const ADst, ASrc: TBLImage;
  const ASize: TBLSizeI; const AFilter: TBLImageScaleFilter);
begin
  _BLCheck(_blImageScale(@ADst, @ASrc, @ASize, Ord(AFilter)));
end;

procedure TBLImage.Swap(var AOther: TBLImage);
begin
  FBase.Swap(AOther.FBase);
end;

function TBLImage.WriteToData(const ACodec: TBLImageCodec): TBytes;
begin
  var Dst: TBLArray<Byte>;
  _BLCheck(_blImageWriteToData(@Self, @Dst, @ACodec));
  SetLength(Result, Dst.Size);
  Move(Dst.Data^, Result[0], Dst.Size);
end;

procedure TBLImage.WriteToData(const ADst: TBLArray<Byte>;
  const ACodec: TBLImageCodec);
begin
  _BLCheck(_blImageWriteToData(@Self, @ADst, @ACodec));
end;

procedure TBLImage.WriteToFile(const AFilename: String;
  const ACodec: TBLImageCodec);
begin
  _BLCheck(_blImageWriteToFile(@Self, PUTF8Char(UTF8String(AFilename)), @ACodec));
end;

procedure TBLImage.WriteToFile(const AFilename: String);
begin
  _BLCheck(_blImageWriteToFile(@Self, PUTF8Char(UTF8String(AFilename)), nil));
end;

class operator TBLImage.NotEqual(const ALeft: TBLImage;
  const ARight: Pointer): Boolean;
begin
  if (ALeft.Format = TBLFormat.None) then
    Result := (ARight <> nil)
  else
    Result := (ARight = nil);
end;

{$ENDREGION 'Imaging'}

{$REGION 'Rendering'}

{ TBLContext }

class operator TBLContext.Assign(var ADest: TBLContext; const [ref] ASrc: TBLContext);
begin
  _BLCheck(_blContextInitWeak(@ADest, @ASrc));
end;

procedure TBLContext.ClearAll;
begin
  _BLCheck(_blContextClearAll(@Self));
end;

constructor TBLContext.Create(const ATarget: TBLImage);
begin
  _BLCheck(_blContextInitAs(@Self, @ATarget, nil));
end;

procedure TBLContext.FillPath(const APath: TBLPath);
begin
  _BLCheck(_blContextFillPathD(@Self, @TBLPoint.Empty, @APath));
end;

procedure TBLContext.FillPath(const APath: TBLPath; const AColor: TBLRgba32);
begin
  _BLCheck(_blContextFillPathDRgba32(@Self, @TBLPoint.Empty, @APath, AColor.Value));
end;

class operator TBLContext.Finalize(var ADest: TBLContext);
begin
  if (ADest.FBase.NeedsCleanup) then
    _BLCheck(_blContextDestroy(@ADest));
end;

procedure TBLContext.Finish;
begin
  _BLCheck(_blContextEnd(@Self));
end;

class operator TBLContext.Initialize(out ADest: TBLContext);
begin
  _BLCheck(_blContextInit(@ADest));
end;
{$ENDREGION 'Rendering'}

{$REGION 'Internal'}

{ _TBLGenericUtils<T> }

class function _TBLGenericUtils<T>.AreEqual(const ALeft, ARight: T): Boolean;
type
  UInt24 = packed record
    Lo: UInt16;
    Hi: UInt8;
  end;
type
  UInt128 = record
    Lo: UInt64;
    Hi: UInt64;
  end;
var
  Left8: UInt8 absolute ALeft;
  Right8: UInt8 absolute ARight;
  Left16: UInt16 absolute ALeft;
  Right16: UInt16 absolute ARight;
  Left24: UInt24 absolute ALeft;
  Right24: UInt24 absolute ARight;
  Left32: UInt32 absolute ALeft;
  Right32: UInt32 absolute ARight;
  Left64: UInt64 absolute ALeft;
  Right64: UInt64 absolute ARight;
  Left128: UInt128 absolute ALeft;
  Right128: UInt128 absolute ARight;
  LeftObj: TBLObjectCore absolute ALeft;
  RightObj: TBLObjectCore absolute ARight;
begin
  case SizeOf(T) of
    1: Result := (Left8 = Right8);
    2: Result := (Left16 = Right16);
    3: Result := (Left24.Lo = Right24.Lo) and (Left24.Hi = Right24.Hi);
    4: Result := (Left32 = Right32);
    8: Result := (Left64 = Right64);
   16: {case AType of
         TBLObjectType.Null,
         TBLObjectType.Pattern,
         TBLObjectType.Gradient,
         TBLObjectType.Image,
         TBLObjectType.Path,
         TBLObjectType.Font,
         TBLObjectType.FontFeatureSettings,
         TBLObjectType.FontVariationSettings,
         TBLObjectType.BitArray,
         TBLObjectType.BitSet,
         TBLObjectType.String,
         TBLObjectType.ArrayObject,
         TBLObjectType.Context,
         TBLObjectType.ImageCodec,
         TBLObjectType.ImageDecoder,
         TBLObjectType.ImageEncoder,
         TBLObjectType.FontFace,
         TBLObjectType.FontData,
         TBLObjectType.FontManager:

       else}
         Result := (Left128.Lo = Right24.Lo) and (Left128.Hi = Right24.Hi);
       {end;}
  else
    Result := CompareMem(@ALeft, @ARight, SizeOf(T));
  end;
end;

class procedure _TBLGenericUtils<T>.CopyToUnitialized(ADst: P;
  const ASrc: array of T; const AArrayType: TBLObjectType);
begin
  if (AArrayType = TBLObjectType.ArrayObject) then
  begin
    FillChar(ADst^, Length(ASrc) * SizeOf(T), 0); { TODO : Is this needed? }
    for var I := 0 to Length(ASrc) - 1 do
      ADst[I] := ASrc[I];
  end
  else
    Move(ASrc[0], ADst^, Length(ASrc) * SizeOf(T));
end;
{$ENDREGION 'Internal'}

{$REGION 'Initialization'}
initialization
  Assert(SizeOf(TBLObjectCore) = 16);
  Assert(SizeOf(TBLImage) = 16);
  Assert(SizeOf(TBLStrokeOptions.TValues) = 8);
{$ENDREGION 'Initialization'}

end.
