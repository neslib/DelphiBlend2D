const
  {$IF Defined(WIN32)}
  _LIB_BLEND2D = 'blend2d_win32.dll';
  _PU = '';
  {$ELSEIF Defined(WIN64)}
  _LIB_BLEND2D = 'blend2d_win64.dll';
  _PU = '';
  {$ELSEIF Defined(MACOS64) and not Defined(IOS)}
    {$MESSAGE Error 'Blend2D for macOS not available yet'}
//  LIB_BLEND2D = 'libblend2d_mac64.a';
//  _PU = '';
  {$ELSEIF Defined(IOS64)}
    {$MESSAGE Error 'Blend2D for iOS not available yet'}
  {$ELSEIF Defined(ANDROID)}
    {$MESSAGE Error 'Blend2D for Android not available yet'}
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

type
  PUInt8 = PByte;
  PPUInt8 = ^PByte;
  {$IF Defined(MACOS)}
  Size_T = LongWord;
  {$ELSE}
  Size_T = NativeUInt;
  {$ENDIF}
  PSize_T = ^Size_T;

{$REGION 'object.h'}
type
  _BLDestroyExternalDataFunc = procedure(impl, externalData, userData: Pointer); cdecl;
{$ENDREGION 'object.h'}

{$REGION 'context.h'}
function _blContextInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextInit';

function _blContextInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextInitMove';

function _blContextInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextInitWeak';

function _blContextInitAs(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextInitAs';

function _blContextDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextDestroy';

function _blContextReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextReset';

function _blContextAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextAssignMove';

function _blContextAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextAssignWeak';

function _blContextGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetType';

function _blContextGetTargetSize(const self: Pointer; targetSizeOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetTargetSize';

function _blContextGetTargetImage(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetTargetImage';

function _blContextBegin(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextBegin';

function _blContextEnd(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextEnd';

function _blContextFlush(self: Pointer; flags: Cardinal): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFlush';

function _blContextSave(self: Pointer; cookie: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSave';

function _blContextRestore(self: Pointer; const cookie: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextRestore';

function _blContextGetMetaTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetMetaTransform';

function _blContextGetUserTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetUserTransform';

function _blContextGetFinalTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetFinalTransform';

function _blContextUserToMeta(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextUserToMeta';

function _blContextApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextApplyTransformOp';

function _blContextGetHint(const self: Pointer; hintType: Integer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetHint';

function _blContextSetHint(self: Pointer; hintType: Integer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetHint';

function _blContextGetHints(const self: Pointer; hintsOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetHints';

function _blContextSetHints(self: Pointer; const hints: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetHints';

function _blContextSetFlattenMode(self: Pointer; mode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFlattenMode';

function _blContextSetFlattenTolerance(self: Pointer; tolerance: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFlattenTolerance';

function _blContextSetApproximationOptions(self: Pointer; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetApproximationOptions';

function _blContextGetFillStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetFillStyle';

function _blContextGetTransformedFillStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetTransformedFillStyle';

function _blContextSetFillStyle(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillStyle';

function _blContextSetFillStyleWithMode(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillStyleWithMode';

function _blContextSetFillStyleRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillStyleRgba';

function _blContextSetFillStyleRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillStyleRgba32';

function _blContextSetFillStyleRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillStyleRgba64';

function _blContextDisableFillStyle(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextDisableFillStyle';

function _blContextGetFillAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetFillAlpha';

function _blContextSetFillAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillAlpha';

function _blContextGetStrokeStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeStyle';

function _blContextGetTransformedStrokeStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetTransformedStrokeStyle';

function _blContextSetStrokeStyle(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeStyle';

function _blContextSetStrokeStyleWithMode(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeStyleWithMode';

function _blContextSetStrokeStyleRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeStyleRgba';

function _blContextSetStrokeStyleRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeStyleRgba32';

function _blContextSetStrokeStyleRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeStyleRgba64';

function _blContextDisableStrokeStyle(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextDisableStrokeStyle';

function _blContextGetStrokeAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeAlpha';

function _blContextSetStrokeAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeAlpha';

function _blContextSwapStyles(self: Pointer; mode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSwapStyles';

function _blContextGetGlobalAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetGlobalAlpha';

function _blContextSetGlobalAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetGlobalAlpha';

function _blContextGetCompOp(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetCompOp';

function _blContextSetCompOp(self: Pointer; compOp: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetCompOp';

function _blContextGetFillRule(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetFillRule';

function _blContextSetFillRule(self: Pointer; fillRule: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetFillRule';

function _blContextGetStrokeWidth(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeWidth';

function _blContextSetStrokeWidth(self: Pointer; width: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeWidth';

function _blContextGetStrokeMiterLimit(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeMiterLimit';

function _blContextSetStrokeMiterLimit(self: Pointer; miterLimit: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeMiterLimit';

function _blContextGetStrokeCap(const self: Pointer; position: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeCap';

function _blContextSetStrokeCap(self: Pointer; position: Integer; strokeCap: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeCap';

function _blContextSetStrokeCaps(self: Pointer; strokeCap: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeCaps';

function _blContextGetStrokeJoin(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeJoin';

function _blContextSetStrokeJoin(self: Pointer; strokeJoin: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeJoin';

function _blContextGetStrokeTransformOrder(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeTransformOrder';

function _blContextSetStrokeTransformOrder(self: Pointer; transformOrder: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeTransformOrder';

function _blContextGetStrokeDashOffset(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeDashOffset';

function _blContextSetStrokeDashOffset(self: Pointer; dashOffset: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeDashOffset';

function _blContextGetStrokeDashArray(const self: Pointer; dashArrayOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeDashArray';

function _blContextSetStrokeDashArray(self: Pointer; const dashArray: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeDashArray';

function _blContextGetStrokeOptions(const self: Pointer; options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextGetStrokeOptions';

function _blContextSetStrokeOptions(self: Pointer; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextSetStrokeOptions';

function _blContextClipToRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextClipToRectI';

function _blContextClipToRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextClipToRectD';

function _blContextRestoreClipping(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextRestoreClipping';

function _blContextClearAll(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextClearAll';

function _blContextClearRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextClearRectI';

function _blContextClearRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextClearRectD';

function _blContextFillAll(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillAll';

function _blContextFillAllRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillAllRgba32';

function _blContextFillAllRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillAllRgba64';

function _blContextFillAllExt(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillAllExt';

function _blContextFillRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectI';

function _blContextFillRectIRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectIRgba32';

function _blContextFillRectIRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectIRgba64';

function _blContextFillRectIExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectIExt';

function _blContextFillRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectD';

function _blContextFillRectDRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectDRgba32';

function _blContextFillRectDRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectDRgba64';

function _blContextFillRectDExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillRectDExt';

function _blContextFillPathD(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillPathD';

function _blContextFillPathDRgba32(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillPathDRgba32';

function _blContextFillPathDRgba64(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillPathDRgba64';

function _blContextFillPathDExt(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillPathDExt';

function _blContextFillGeometry(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGeometry';

function _blContextFillGeometryRgba32(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGeometryRgba32';

function _blContextFillGeometryRgba64(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGeometryRgba64';

function _blContextFillGeometryExt(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGeometryExt';

function _blContextFillUtf8TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextI';

function _blContextFillUtf8TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextIRgba32';

function _blContextFillUtf8TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextIRgba64';

function _blContextFillUtf8TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextIExt';

function _blContextFillUtf8TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextD';

function _blContextFillUtf8TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextDRgba32';

function _blContextFillUtf8TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextDRgba64';

function _blContextFillUtf8TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf8TextDExt';

function _blContextFillUtf16TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextI';

function _blContextFillUtf16TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextIRgba32';

function _blContextFillUtf16TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextIRgba64';

function _blContextFillUtf16TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextIExt';

function _blContextFillUtf16TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextD';

function _blContextFillUtf16TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextDRgba32';

function _blContextFillUtf16TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextDRgba64';

function _blContextFillUtf16TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf16TextDExt';

function _blContextFillUtf32TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextI';

function _blContextFillUtf32TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextIRgba32';

function _blContextFillUtf32TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextIRgba64';

function _blContextFillUtf32TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextIExt';

function _blContextFillUtf32TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextD';

function _blContextFillUtf32TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextDRgba32';

function _blContextFillUtf32TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextDRgba64';

function _blContextFillUtf32TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillUtf32TextDExt';

function _blContextFillGlyphRunI(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunI';

function _blContextFillGlyphRunIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunIRgba32';

function _blContextFillGlyphRunIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunIRgba64';

function _blContextFillGlyphRunIExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunIExt';

function _blContextFillGlyphRunD(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunD';

function _blContextFillGlyphRunDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunDRgba32';

function _blContextFillGlyphRunDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunDRgba64';

function _blContextFillGlyphRunDExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillGlyphRunDExt';

function _blContextFillMaskI(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskI';

function _blContextFillMaskIRgba32(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskIRgba32';

function _blContextFillMaskIRgba64(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskIRgba64';

function _blContextFillMaskIExt(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskIExt';

function _blContextFillMaskD(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskD';

function _blContextFillMaskDRgba32(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskDRgba32';

function _blContextFillMaskDRgba64(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskDRgba64';

function _blContextFillMaskDExt(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextFillMaskDExt';

function _blContextStrokeRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectI';

function _blContextStrokeRectIRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectIRgba32';

function _blContextStrokeRectIRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectIRgba64';

function _blContextStrokeRectIExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectIExt';

function _blContextStrokeRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectD';

function _blContextStrokeRectDRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectDRgba32';

function _blContextStrokeRectDRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectDRgba64';

function _blContextStrokeRectDExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeRectDExt';

function _blContextStrokePathD(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokePathD';

function _blContextStrokePathDRgba32(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokePathDRgba32';

function _blContextStrokePathDRgba64(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokePathDRgba64';

function _blContextStrokePathDExt(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokePathDExt';

function _blContextStrokeGeometry(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGeometry';

function _blContextStrokeGeometryRgba32(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGeometryRgba32';

function _blContextStrokeGeometryRgba64(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGeometryRgba64';

function _blContextStrokeGeometryExt(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGeometryExt';

function _blContextStrokeUtf8TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextI';

function _blContextStrokeUtf8TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextIRgba32';

function _blContextStrokeUtf8TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextIRgba64';

function _blContextStrokeUtf8TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextIExt';

function _blContextStrokeUtf8TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextD';

function _blContextStrokeUtf8TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextDRgba32';

function _blContextStrokeUtf8TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextDRgba64';

function _blContextStrokeUtf8TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf8TextDExt';

function _blContextStrokeUtf16TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextI';

function _blContextStrokeUtf16TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextIRgba32';

function _blContextStrokeUtf16TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextIRgba64';

function _blContextStrokeUtf16TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextIExt';

function _blContextStrokeUtf16TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextD';

function _blContextStrokeUtf16TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextDRgba32';

function _blContextStrokeUtf16TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextDRgba64';

function _blContextStrokeUtf16TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf16TextDExt';

function _blContextStrokeUtf32TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextI';

function _blContextStrokeUtf32TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextIRgba32';

function _blContextStrokeUtf32TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextIRgba64';

function _blContextStrokeUtf32TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextIExt';

function _blContextStrokeUtf32TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextD';

function _blContextStrokeUtf32TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextDRgba32';

function _blContextStrokeUtf32TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextDRgba64';

function _blContextStrokeUtf32TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeUtf32TextDExt';

function _blContextStrokeGlyphRunI(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunI';

function _blContextStrokeGlyphRunIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunIRgba32';

function _blContextStrokeGlyphRunIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunIRgba64';

function _blContextStrokeGlyphRunIExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunIExt';

function _blContextStrokeGlyphRunD(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunD';

function _blContextStrokeGlyphRunDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunDRgba32';

function _blContextStrokeGlyphRunDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunDRgba64';

function _blContextStrokeGlyphRunDExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextStrokeGlyphRunDExt';

function _blContextBlitImageI(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextBlitImageI';

function _blContextBlitImageD(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextBlitImageD';

function _blContextBlitScaledImageI(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextBlitScaledImageI';

function _blContextBlitScaledImageD(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blContextBlitScaledImageD';
{$ENDREGION 'context.h'}

{$REGION 'image.h'}
function _blImageInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageInit';

function _blImageInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageInitMove';

function _blImageInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageInitWeak';

function _blImageInitAs(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageInitAs';

function _blImageInitAsFromData(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageInitAsFromData';

function _blImageDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDestroy';

function _blImageReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageReset';

function _blImageAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageAssignMove';

function _blImageAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageAssignWeak';

function _blImageAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageAssignDeep';

function _blImageCreate(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCreate';

function _blImageCreateFromData(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCreateFromData';

function _blImageGetData(const self: Pointer; dataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageGetData';

function _blImageMakeMutable(self: Pointer; dataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageMakeMutable';

function _blImageConvert(self: Pointer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageConvert';

function _blImageEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEquals';

function _blImageScale(dst: Pointer; const src: Pointer; const size: Pointer; filter: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageScale';

function _blImageReadFromFile(self: Pointer; const fileName: PUTF8Char; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageReadFromFile';

function _blImageReadFromData(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageReadFromData';

function _blImageWriteToFile(const self: Pointer; const fileName: PUTF8Char; const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageWriteToFile';

function _blImageWriteToData(const self: Pointer; dst: Pointer; const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageWriteToData';
{$ENDREGION 'image.h'}

{$REGION 'path.h'}
type
  _BLPathStrokeSinkFunc = function(a: Pointer; b: Pointer; c: Pointer; inputStart: Size_T; inputEnd: Size_T; userData: Pointer): Integer; cdecl;

function _blPathInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathInit';

function _blPathInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathInitMove';

function _blPathInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathInitWeak';

function _blPathDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathDestroy';

function _blPathReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathReset';

function _blPathGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetSize';

function _blPathGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetCapacity';

function _blPathGetCommandData(const self: Pointer): PUInt8; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetCommandData';

function _blPathGetVertexData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetVertexData';

function _blPathClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathClear';

function _blPathShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathShrink';

function _blPathReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathReserve';

function _blPathModifyOp(self: Pointer; op: Integer; n: Size_T; cmdDataOut: PPUInt8; vtxDataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathModifyOp';

function _blPathAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAssignMove';

function _blPathAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAssignWeak';

function _blPathAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAssignDeep';

function _blPathSetVertexAt(self: Pointer; index: Size_T; cmd: UInt32; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathSetVertexAt';

function _blPathMoveTo(self: Pointer; x0: Double; y0: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathMoveTo';

function _blPathLineTo(self: Pointer; x1: Double; y1: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathLineTo';

function _blPathPolyTo(self: Pointer; const poly: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathPolyTo';

function _blPathQuadTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathQuadTo';

function _blPathConicTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; w: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathConicTo';

function _blPathCubicTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathCubicTo';

function _blPathSmoothQuadTo(self: Pointer; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathSmoothQuadTo';

function _blPathSmoothCubicTo(self: Pointer; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathSmoothCubicTo';

function _blPathArcTo(self: Pointer; x: Double; y: Double; rx: Double; ry: Double; start: Double; sweep: Double; forceMoveTo: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathArcTo';

function _blPathArcQuadrantTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathArcQuadrantTo';

function _blPathEllipticArcTo(self: Pointer; rx: Double; ry: Double; xAxisRotation: Double; largeArcFlag: Boolean; sweepFlag: Boolean; x1: Double; y1: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathEllipticArcTo';

function _blPathClose(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathClose';

function _blPathAddGeometry(self: Pointer; geometryType: Integer; const geometryData: Pointer; const m: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddGeometry';

function _blPathAddBoxI(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddBoxI';

function _blPathAddBoxD(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddBoxD';

function _blPathAddRectI(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddRectI';

function _blPathAddRectD(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddRectD';

function _blPathAddPath(self: Pointer; const other: Pointer; const range: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddPath';

function _blPathAddTranslatedPath(self: Pointer; const other: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddTranslatedPath';

function _blPathAddTransformedPath(self: Pointer; const other: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddTransformedPath';

function _blPathAddReversedPath(self: Pointer; const other: Pointer; const range: Pointer; reverseMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddReversedPath';

function _blPathAddStrokedPath(self: Pointer; const other: Pointer; const range: Pointer; const options: Pointer; const approx: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathAddStrokedPath';

function _blPathRemoveRange(self: Pointer; const range: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathRemoveRange';

function _blPathTranslate(self: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathTranslate';

function _blPathTransform(self: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathTransform';

function _blPathFitTo(self: Pointer; const range: Pointer; const rect: Pointer; fitFlags: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathFitTo';

function _blPathEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathEquals';

function _blPathGetInfoFlags(const self: Pointer; flagsOut: PUInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetInfoFlags';

function _blPathGetControlBox(const self: Pointer; boxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetControlBox';

function _blPathGetBoundingBox(const self: Pointer; boxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetBoundingBox';

function _blPathGetFigureRange(const self: Pointer; index: Size_T; rangeOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetFigureRange';

function _blPathGetLastVertex(const self: Pointer; vtxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetLastVertex';

function _blPathGetClosestVertex(const self: Pointer; const p: Pointer; maxDistance: Double; indexOut: PSize_T; distanceOut: PDouble): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathGetClosestVertex';

function _blPathHitTest(const self: Pointer; const p: Pointer; fillRule: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathHitTest';

function _blStrokeOptionsInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsInit';

function _blStrokeOptionsInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsInitMove';

function _blStrokeOptionsInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsInitWeak';

function _blStrokeOptionsDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsDestroy';

function _blStrokeOptionsReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsReset';

function _blStrokeOptionsEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsEquals';

function _blStrokeOptionsAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsAssignMove';

function _blStrokeOptionsAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStrokeOptionsAssignWeak';

function _blPathStrokeToSink(const self: Pointer; const range: Pointer; const strokeOptions: Pointer; const approximationOptions: Pointer; a: Pointer; b: Pointer; c: Pointer; sink: _BLPathStrokeSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPathStrokeToSink';
{$ENDREGION 'path.h'}

