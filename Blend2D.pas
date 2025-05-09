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
   [Globals - Enums and Sets]
  ============================================================================ }

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
{$ENDREGION 'Globals'}

{$REGION 'Objects'}
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
  private
    FImpl: Pointer;
    {$IFDEF CPU32BITS}
    FData: array [0..1] of UInt32;
    {$ELSE}
    FData: UInt32;
    {$ENDIF}
    FInfo: UInt32;
  private
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
type
  /// <summary>
  ///  Generic array container.
  /// </summary>
  TBLArray<T> = record
  {$REGION 'Internal Declarations'}
  private
    FBase: TBLObjectCore;
  {$ENDREGION 'Internal Declarations'}
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

{$INCLUDE 'Blend2D.Api.inc'}

implementation

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

function TBLObjectCore.NeedsCleanup: Boolean;
begin
  Result := (FInfo >= MDR_FLAGS);
end;

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

{$REGION 'Initialization'}
initialization
  Assert(SizeOf(TBLObjectCore) = 16);
  Assert(SizeOf(TBLImage) = 16);
{$ENDREGION 'Initialization'}

end.
