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
  PPUInt32 = ^PUInt32;
  PPUTF8Char = ^PUTF8Char;

{$REGION 'object.h'}
type
  _BLDestroyExternalDataFunc = procedure(impl, externalData, userData: Pointer); cdecl;
{$ENDREGION 'object.h'}

{$REGION 'array.h'}
function _blArrayInit(self: Pointer; arrayType: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInit';

function _blArrayInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInitMove';

function _blArrayInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInitWeak';

function _blArrayDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayDestroy';

function _blArrayReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReset';

function _blArrayGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayGetSize';

function _blArrayGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayGetCapacity';

function _blArrayGetItemSize(self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayGetItemSize';

function _blArrayGetData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayGetData';

function _blArrayClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayClear';

function _blArrayShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayShrink';

function _blArrayReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReserve';

function _blArrayResize(self: Pointer; n: Size_T; const fill: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayResize';

function _blArrayMakeMutable(self: Pointer; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayMakeMutable';

function _blArrayModifyOp(self: Pointer; op: Integer; n: Size_T; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayModifyOp';

function _blArrayInsertOp(self: Pointer; index: Size_T; n: Size_T; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertOp';

function _blArrayAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAssignMove';

function _blArrayAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAssignWeak';

function _blArrayAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAssignDeep';

function _blArrayAssignData(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAssignData';

function _blArrayAssignExternalData(self: Pointer; data: Pointer; size: Size_T; capacity: Size_T; dataAccessFlags: Integer; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAssignExternalData';

function _blArrayAppendU8(self: Pointer; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendU8';

function _blArrayAppendU16(self: Pointer; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendU16';

function _blArrayAppendU32(self: Pointer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendU32';

function _blArrayAppendU64(self: Pointer; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendU64';

function _blArrayAppendF32(self: Pointer; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendF32';

function _blArrayAppendF64(self: Pointer; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendF64';

function _blArrayAppendItem(self: Pointer; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendItem';

function _blArrayAppendData(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayAppendData';

function _blArrayInsertU8(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertU8';

function _blArrayInsertU16(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertU16';

function _blArrayInsertU32(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertU32';

function _blArrayInsertU64(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertU64';

function _blArrayInsertF32(self: Pointer; index: Size_T; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertF32';

function _blArrayInsertF64(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertF64';

function _blArrayInsertItem(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertItem';

function _blArrayInsertData(self: Pointer; index: Size_T; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInsertData';

function _blArrayReplaceU8(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceU8';

function _blArrayReplaceU16(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceU16';

function _blArrayReplaceU32(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceU32';

function _blArrayReplaceU64(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceU64';

function _blArrayReplaceF32(self: Pointer; index: Size_T; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceF32';

function _blArrayReplaceF64(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceF64';

function _blArrayReplaceItem(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceItem';

function _blArrayReplaceData(self: Pointer; rStart: Size_T; rEnd: Size_T; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayReplaceData';

function _blArrayRemoveIndex(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayRemoveIndex';

function _blArrayRemoveRange(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayRemoveRange';

function _blArrayEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayEquals';
{$ENDREGION 'array.h'}

{$REGION 'bitarray.h'}
function _blBitArrayInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayInit';

function _blBitArrayInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayInitMove';

function _blBitArrayInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayInitWeak';

function _blBitArrayDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayDestroy';

function _blBitArrayReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReset';

function _blBitArrayAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAssignMove';

function _blBitArrayAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAssignWeak';

function _blBitArrayAssignWords(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAssignWords';

function _blBitArrayIsEmpty(const self: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayIsEmpty';

function _blBitArrayGetSize(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetSize';

function _blBitArrayGetWordCount(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetWordCount';

function _blBitArrayGetCapacity(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetCapacity';

function _blBitArrayGetData(const self: Pointer): PUInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetData';

function _blBitArrayGetCardinality(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetCardinality';

function _blBitArrayGetCardinalityInRange(const self: Pointer; startBit: UInt32; endBit: UInt32): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetCardinalityInRange';

function _blBitArrayHasBit(const self: Pointer; bitIndex: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayHasBit';

function _blBitArrayHasBitsInRange(const self: Pointer; startBit: UInt32; endBit: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayHasBitsInRange';

function _blBitArraySubsumes(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArraySubsumes';

function _blBitArrayIntersects(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayIntersects';

function _blBitArrayGetRange(const self: Pointer; startOut: PUInt32; endOut: PUInt32): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayGetRange';

function _blBitArrayEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayEquals';

function _blBitArrayCompare(const a: Pointer; const b: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayCompare';

function _blBitArrayClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayClear';

function _blBitArrayResize(self: Pointer; nBits: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayResize';

function _blBitArrayReserve(self: Pointer; nBits: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReserve';

function _blBitArrayShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayShrink';

function _blBitArraySetBit(self: Pointer; bitIndex: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArraySetBit';

function _blBitArrayFillRange(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayFillRange';

function _blBitArrayFillWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayFillWords';

function _blBitArrayClearBit(self: Pointer; bitIndex: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayClearBit';

function _blBitArrayClearRange(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayClearRange';

function _blBitArrayClearWord(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayClearWord';

function _blBitArrayClearWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayClearWords';

function _blBitArrayReplaceOp(self: Pointer; nBits: UInt32; dataOut: PPUInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReplaceOp';

function _blBitArrayReplaceBit(self: Pointer; bitIndex: UInt32; bitValue: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReplaceBit';

function _blBitArrayReplaceWord(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReplaceWord';

function _blBitArrayReplaceWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayReplaceWords';

function _blBitArrayAppendBit(self: Pointer; bitValue: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAppendBit';

function _blBitArrayAppendWord(self: Pointer; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAppendWord';

function _blBitArrayAppendWords(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blBitArrayAppendWords';
{$ENDREGION 'bitarray.h'}

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

{$REGION 'font.h'}
function _blFontInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontInit';

function _blFontInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontInitMove';

function _blFontInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontInitWeak';

function _blFontDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontDestroy';

function _blFontReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontReset';

function _blFontAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontAssignMove';

function _blFontAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontAssignWeak';

function _blFontEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontEquals';

function _blFontCreateFromFace(self: Pointer; const face: Pointer; size: Single): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontCreateFromFace';

function _blFontCreateFromFaceWithSettings(self: Pointer; const face: Pointer; size: Single; const featureSettings: Pointer; const variationSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontCreateFromFaceWithSettings';

function _blFontGetFace(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetFace';

function _blFontGetSize(const self: Pointer): Single; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetSize';

function _blFontSetSize(self: Pointer; size: Single): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontSetSize';

function _blFontGetMetrics(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetMetrics';

function _blFontGetMatrix(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetMatrix';

function _blFontGetDesignMetrics(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetDesignMetrics';

function _blFontGetFeatureSettings(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetFeatureSettings';

function _blFontSetFeatureSettings(self: Pointer; const featureSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontSetFeatureSettings';

function _blFontResetFeatureSettings(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontResetFeatureSettings';

function _blFontGetVariationSettings(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetVariationSettings';

function _blFontSetVariationSettings(self: Pointer; const variationSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontSetVariationSettings';

function _blFontResetVariationSettings(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontResetVariationSettings';

function _blFontShape(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontShape';

function _blFontMapTextToGlyphs(const self: Pointer; gb: Pointer; stateOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontMapTextToGlyphs';

function _blFontPositionGlyphs(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontPositionGlyphs';

function _blFontApplyKerning(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontApplyKerning';

function _blFontApplyGSub(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontApplyGSub';

function _blFontApplyGPos(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontApplyGPos';

function _blFontGetTextMetrics(const self: Pointer; gb: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetTextMetrics';

function _blFontGetGlyphBounds(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetGlyphBounds';

function _blFontGetGlyphAdvances(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetGlyphAdvances';

function _blFontGetGlyphOutlines(const self: Pointer; glyphId: TBLGlyphId; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetGlyphOutlines';

function _blFontGetGlyphRunOutlines(const self: Pointer; const glyphRun: Pointer; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFontGetGlyphRunOutlines';
{$ENDREGION 'font.h'}

{$REGION 'format.h'}
function _blFormatInfoQuery(self: Pointer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFormatInfoQuery';

function _blFormatInfoSanitize(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blFormatInfoSanitize';
{$ENDREGION 'format.h'}

{$REGION 'glyphbuffer.h'}
function _blGlyphBufferInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferInit';

function _blGlyphBufferInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferInitMove';

function _blGlyphBufferDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferDestroy';

function _blGlyphBufferReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferReset';

function _blGlyphBufferClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferClear';

function _blGlyphBufferGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetSize';

function _blGlyphBufferGetFlags(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetFlags';

function _blGlyphBufferGetGlyphRun(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetGlyphRun';

function _blGlyphBufferGetContent(const self: Pointer): PUInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetContent';

function _blGlyphBufferGetInfoData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetInfoData';

function _blGlyphBufferGetPlacementData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferGetPlacementData';

function _blGlyphBufferSetText(self: Pointer; const textData: Pointer; size: Size_T; encoding: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferSetText';

function _blGlyphBufferSetGlyphs(self: Pointer; const glyphData: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferSetGlyphs';

function _blGlyphBufferSetGlyphsFromStruct(self: Pointer; const glyphData: Pointer; size: Size_T; glyphIdSize: Size_T; glyphIdAdvance: IntPtr): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferSetGlyphsFromStruct';

//function _blGlyphBufferSetDebugSink(self: Pointer; sink: _BLDebugMessageSinkFunc; userData: Pointer): Integer; cdecl;
//  external _LIB_BLEND2D name _PU + 'blGlyphBufferSetDebugSink';

function _blGlyphBufferResetDebugSink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGlyphBufferResetDebugSink';
{$ENDREGION 'glyphbuffer.h'}

{$REGION 'gradient.h'}
function _blGradientInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientInit';

function _blGradientInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientInitMove';

function _blGradientInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientInitWeak';

function _blGradientInitAs(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientInitAs';

function _blGradientDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientDestroy';

function _blGradientReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientReset';

function _blGradientAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientAssignMove';

function _blGradientAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientAssignWeak';

function _blGradientCreate(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientCreate';

function _blGradientShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientShrink';

function _blGradientReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientReserve';

function _blGradientGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetType';

function _blGradientSetType(self: Pointer; &type: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientSetType';

function _blGradientGetExtendMode(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetExtendMode';

function _blGradientSetExtendMode(self: Pointer; extendMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientSetExtendMode';

function _blGradientGetValue(const self: Pointer; index: Size_T): Double; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetValue';

function _blGradientSetValue(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientSetValue';

function _blGradientSetValues(self: Pointer; index: Size_T; const values: PDouble; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientSetValues';

function _blGradientGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetSize';

function _blGradientGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetCapacity';

function _blGradientGetStops(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetStops';

function _blGradientResetStops(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientResetStops';

function _blGradientAssignStops(self: Pointer; const stops: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientAssignStops';

function _blGradientAddStopRgba32(self: Pointer; offset: Double; argb32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientAddStopRgba32';

function _blGradientAddStopRgba64(self: Pointer; offset: Double; argb64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientAddStopRgba64';

function _blGradientRemoveStop(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientRemoveStop';

function _blGradientRemoveStopByOffset(self: Pointer; offset: Double; all: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientRemoveStopByOffset';

function _blGradientRemoveStopsByIndex(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientRemoveStopsByIndex';

function _blGradientRemoveStopsByOffset(self: Pointer; offsetMin: Double; offsetMax: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientRemoveStopsByOffset';

function _blGradientReplaceStopRgba32(self: Pointer; index: Size_T; offset: Double; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientReplaceStopRgba32';

function _blGradientReplaceStopRgba64(self: Pointer; index: Size_T; offset: Double; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientReplaceStopRgba64';

function _blGradientIndexOfStop(const self: Pointer; offset: Double): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientIndexOfStop';

function _blGradientGetTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetTransform';

function _blGradientGetTransformType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientGetTransformType';

function _blGradientApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientApplyTransformOp';

function _blGradientEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blGradientEquals';
{$ENDREGION 'gradient.h'}

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

{$REGION 'imagecodec.h'}
function _blImageCodecInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecInit';

function _blImageCodecInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecInitMove';

function _blImageCodecInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecInitWeak';

function _blImageCodecInitByName(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecInitByName';

function _blImageCodecDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecDestroy';

function _blImageCodecReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecReset';

function _blImageCodecAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecAssignMove';

function _blImageCodecAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecAssignWeak';

function _blImageCodecFindByName(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecFindByName';

function _blImageCodecFindByExtension(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecFindByExtension';

function _blImageCodecFindByData(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecFindByData';

function _blImageCodecInspectData(const self: Pointer; const data: Pointer; size: Size_T): UInt32; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecInspectData';

function _blImageCodecCreateDecoder(const self: Pointer; dst: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecCreateDecoder';

function _blImageCodecCreateEncoder(const self: Pointer; dst: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecCreateEncoder';

function _blImageCodecArrayInitBuiltInCodecs(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecArrayInitBuiltInCodecs';

function _blImageCodecArrayAssignBuiltInCodecs(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecArrayAssignBuiltInCodecs';

function _blImageCodecAddToBuiltIn(const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecAddToBuiltIn';

function _blImageCodecRemoveFromBuiltIn(const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageCodecRemoveFromBuiltIn';
{$ENDREGION 'imagecodec.h'}

{$REGION 'imagedecoder.h'}
function _blImageDecoderInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderInit';

function _blImageDecoderInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderInitMove';

function _blImageDecoderInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderInitWeak';

function _blImageDecoderDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderDestroy';

function _blImageDecoderReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderReset';

function _blImageDecoderAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderAssignMove';

function _blImageDecoderAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderAssignWeak';

function _blImageDecoderRestart(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderRestart';

function _blImageDecoderReadInfo(self: Pointer; infoOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderReadInfo';

function _blImageDecoderReadFrame(self: Pointer; imageOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageDecoderReadFrame';
{$ENDREGION 'imagedecoder.h'}

{$REGION 'imageencoder.h'}
function _blImageEncoderInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderInit';

function _blImageEncoderInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderInitMove';

function _blImageEncoderInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderInitWeak';

function _blImageEncoderDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderDestroy';

function _blImageEncoderReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderReset';

function _blImageEncoderAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderAssignMove';

function _blImageEncoderAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderAssignWeak';

function _blImageEncoderRestart(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderRestart';

function _blImageEncoderWriteFrame(self: Pointer; dst: Pointer; const image: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blImageEncoderWriteFrame';
{$ENDREGION 'imageencoder.h'}

{$REGION 'matrix.h'}
function _blMatrix2DSetIdentity(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DSetIdentity';

function _blMatrix2DSetTranslation(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DSetTranslation';

function _blMatrix2DSetScaling(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DSetScaling';

function _blMatrix2DSetSkewing(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DSetSkewing';

function _blMatrix2DSetRotation(self: Pointer; angle: Double; cx: Double; cy: Double): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DSetRotation';

function _blMatrix2DApplyOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DApplyOp';

function _blMatrix2DInvert(dst: Pointer; const src: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DInvert';

function _blMatrix2DGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DGetType';

function _blMatrix2DMapPointDArray(const self: Pointer; dst: Pointer; const src: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blMatrix2DMapPointDArray';
{$ENDREGION 'matrix.h'}

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

{$REGION 'pattern.h'}
function _blPatternInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternInit';

function _blPatternInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternInitMove';

function _blPatternInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternInitWeak';

function _blPatternInitAs(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternInitAs';

function _blPatternDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternDestroy';

function _blPatternReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternReset';

function _blPatternAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternAssignMove';

function _blPatternAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternAssignWeak';

function _blPatternAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternAssignDeep';

function _blPatternCreate(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternCreate';

function _blPatternGetImage(const self: Pointer; image: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternGetImage';

function _blPatternSetImage(self: Pointer; const image: Pointer; const area: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternSetImage';

function _blPatternResetImage(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternResetImage';

function _blPatternGetArea(const self: Pointer; areaOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternGetArea';

function _blPatternSetArea(self: Pointer; const area: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternSetArea';

function _blPatternResetArea(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternResetArea';

function _blPatternGetExtendMode(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternGetExtendMode';

function _blPatternSetExtendMode(self: Pointer; extendMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternSetExtendMode';

function _blPatternGetTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternGetTransform';

function _blPatternGetTransformType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternGetTransformType';

function _blPatternApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternApplyTransformOp';

function _blPatternEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blPatternEquals';
{$ENDREGION 'pattern.h'}

{$REGION 'pixelconverter.h'}
function _blPixelConverterInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterInit';

function _blPixelConverterInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterInitWeak';

function _blPixelConverterDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterDestroy';

function _blPixelConverterReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterReset';

function _blPixelConverterAssign(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterAssign';

function _blPixelConverterCreate(self: Pointer; const dstInfo: Pointer; const srcInfo: Pointer; createFlags: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterCreate';

function _blPixelConverterConvert(const self: Pointer; dstData: Pointer; dstStride: IntPtr; const srcData: Pointer; srcStride: IntPtr; w: UInt32; h: UInt32; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blPixelConverterConvert';
{$ENDREGION 'pixelconverter.h'}

{$REGION 'string.h'}
function _blStringInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInit';

function _blStringInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInitMove';

function _blStringInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInitWeak';

function _blStringInitWithData(self: Pointer; const str: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInitWithData';

function _blStringDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringDestroy';

function _blStringReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringReset';

function _blStringGetData(const self: Pointer): PUTF8Char; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringGetData';

function _blStringGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringGetSize';

function _blStringGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringGetCapacity';

function _blStringClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringClear';

function _blStringShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringShrink';

function _blStringReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringReserve';

function _blStringResize(self: Pointer; n: Size_T; fill: UTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringResize';

function _blStringMakeMutable(self: Pointer; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringMakeMutable';

function _blStringModifyOp(self: Pointer; op: Integer; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringModifyOp';

function _blStringInsertOp(self: Pointer; index: Size_T; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInsertOp';

function _blStringAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringAssignMove';

function _blStringAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringAssignWeak';

function _blStringAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringAssignDeep';

function _blStringAssignData(self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringAssignData';

function _blStringApplyOpChar(self: Pointer; op: Integer; c: UTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringApplyOpChar';

function _blStringApplyOpData(self: Pointer; op: Integer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringApplyOpData';

function _blStringApplyOpString(self: Pointer; op: Integer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringApplyOpString';

function _blStringApplyOpFormat(self: Pointer; op: Integer; const fmt: PUTF8Char): Integer varargs; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringApplyOpFormat';

function _blStringApplyOpFormatV(self: Pointer; op: Integer; const fmt: PUTF8Char; ap: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringApplyOpFormatV';

function _blStringInsertChar(self: Pointer; index: Size_T; c: UTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInsertChar';

function _blStringInsertData(self: Pointer; index: Size_T; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInsertData';

function _blStringInsertString(self: Pointer; index: Size_T; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringInsertString';

function _blStringRemoveIndex(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringRemoveIndex';

function _blStringRemoveRange(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringRemoveRange';

function _blStringEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringEquals';

function _blStringEqualsData(const self: Pointer; const str: PUTF8Char; n: Size_T): Boolean; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringEqualsData';

function _blStringCompare(const a: Pointer; const b: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringCompare';

function _blStringCompareData(const self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blStringCompareData';
{$ENDREGION 'string.h'}

