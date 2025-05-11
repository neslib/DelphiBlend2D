unit Blend2D;
{ OPP wrappers for Blend2D C API.

  Follows the C++ API where possible. The following C++ classes are not
  converted because Delphi provides built-in alternatives for these:
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
{$ENDREGION 'Containers'}

{$REGION 'Geometries'}

{ ============================================================================
   [Geometries - Lightweight Geometries and Structs]
  ============================================================================ }

type
  /// <summary>
  ///  Size specified as [W, H] using `Integer` as a storage type.
  /// </summary>
  TBLSizeI = record
  public
    W: Integer;
    H: Integer;
  end;

type
  /// <summary>
  ///  Point specified as [X, Y] using `Double` as a storage type.
  /// </summary>
  TBLPoint = record
  public
    X: Double;
    Y: Double;
  end;

type
  /// <summary>
  ///  Adds functionality to TBLPoint
  /// </summary>
  _TBLPointHelper = record helper for TBLPoint
  public const
    Empty: TBLPoint = (X: 0; Y: 0);
  end;

{ ============================================================================
   [Geometries - Paths]
  ============================================================================ }

type
  /// <summary>
  ///  2D vector path.
  /// </summary>
  TBLPath = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
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
    ///  Moves to `[AX0, AY0]`.
    ///
    ///  Appends `TBLPathCmd.Move[AX0, AY0]` command to the path.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
    procedure MoveTo(const AX0, AY0: Double); inline;

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
    procedure CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double); inline;
  end;

{$ENDREGION 'Geometries'}

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

{ ============================================================================
   [Imaging - Image Codecs]
  ============================================================================ }

type
  TBLImageCodec = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
  {$ENDREGION 'Internal Declarations'}
  end;

{ ============================================================================
   [Imaging - Images]
  ============================================================================ }

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
//    procedure ReadFromFile(const AFilename: String;
//      const ACodecs: TBLArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Reads an image from a file specified by `AFileName` by using image
    ///  codecs passed via `ACodecs` parameter.
    ///
    ///  Image reader will automatically detect the image format by checking
    ///  whether it's supported by the passed image `ACodecs` - only codecs
    ///  passed in will be considered.
    /// </summary>
    /// <exception name="EBlend2DError">Raised on failure.</exception>
//    procedure ReadFromFile(const AFilename: String;
//      const ACodecs: TArray<TBLImageCodec>); overload; inline;

    /// <summary>
    ///  Writes an encoded image to a file specified by `AFilename`.
    ///
    ///  Image writer detects the image codec by inspecting the extension of a
    ///  file passed via `AFilename`.
    /// </summary>
    procedure WriteToFile(const AFilename: String); overload; inline;

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
{$ENDREGION 'Imaging'}

{$REGION 'Styling'}

{ ============================================================================
   [Styling - Colors]
  ============================================================================ }

type
  /// <summary>
  ///  32-bit RGBA color (8-bit per component) stored as `$AARRGGBB`.
  /// </summary>
  TBLRgba32 = record
  public
    /// <summary>
    ///  Packed 32-bit RGBA value.
    /// </summary>
    Value: UInt32;
  public
    /// <summary>
    ///  Implicitly converts from a packed 32-bit RGBA value to a TBLRgba32;
    /// </summary>
    class operator Implicit(const AValue: UInt32): TBLRgba32; inline; static;
  end;
{$ENDREGION 'Styling'}

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

{ TBLPath }

class operator TBLPath.Assign(var ADest: TBLPath; const [ref] ASrc: TBLPath);
begin
  _BLCheck(_blPathInitWeak(@ADest, @ASrc));
end;

procedure TBLPath.CubicTo(const AX1, AY1, AX2, AY2, AX3, AY3: Double);
begin
  _BLCheck(_blPathCubicTo(@Self, AX1, AY1, AX2, AY2, AX3, AY3));
end;

class operator TBLPath.Finalize(var ADest: TBLPath);
begin
  _BLCheck(_blPathDestroy(@ADest));
end;

class operator TBLPath.Initialize(out ADest: TBLPath);
begin
  _BLCheck(_blPathInit(@ADest));
end;

procedure TBLPath.MoveTo(const AX0, AY0: Double);
begin
  _BLCheck(_blPathMoveTo(@Self, AX0, AY0));
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

{$ENDREGION 'Containers'}

{$REGION 'Imaging'}

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

procedure TBLImage.ReadFromFile(const AFilename: String);
begin
  _BLCheck(_blImageReadFromFile(@Self, PUTF8Char(UTF8String(AFilename)), nil));
end;

procedure TBLImage.Reset;
begin
  _BLCheck(_blImageReset(@Self));
end;

procedure TBLImage.Swap(var AOther: TBLImage);
begin
  FBase.Swap(AOther.FBase);
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

{$REGION 'Styling'}

{ TBLRgba32 }

class operator TBLRgba32.Implicit(const AValue: UInt32): TBLRgba32;
begin
  Result.Value := AValue;
end;
{$ENDREGION 'Styling'}

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
{$ENDREGION 'Initialization'}

end.
