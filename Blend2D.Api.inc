type
  PUInt8 = PByte;
  PPUInt8 = ^PByte;
  PInt32 = ^Int32;
  PPUInt32 = ^PUInt32;
  PPUTF8Char = ^PUTF8Char;

// object.h

type
  _BLDestroyExternalDataFunc = procedure(impl, externalData, userData: Pointer); cdecl;

// path.h

type
  _BLPathStrokeSinkFunc = function(a: Pointer; b: Pointer; c: Pointer; inputStart: Size_T; inputEnd: Size_T; userData: Pointer): Integer; cdecl;

{$IFDEF MSWINDOWS}

var
  // array.h'
  _blArrayInit: function(self: Pointer; arrayType: Integer): Integer; cdecl = nil;
  _blArrayInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blArrayInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blArrayDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blArrayReset: function(self: Pointer): Integer; cdecl = nil;
  _blArrayGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blArrayGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blArrayGetItemSize: function(self: Pointer): Size_T; cdecl = nil;
  _blArrayGetData: function(const self: Pointer): Pointer; cdecl = nil;
  _blArrayClear: function(self: Pointer): Integer; cdecl = nil;
  _blArrayShrink: function(self: Pointer): Integer; cdecl = nil;
  _blArrayReserve: function(self: Pointer; n: Size_T): Integer; cdecl = nil;
  _blArrayResize: function(self: Pointer; n: Size_T; const fill: Pointer): Integer; cdecl = nil;
  _blArrayMakeMutable: function(self: Pointer; dataOut: PPointer): Integer; cdecl = nil;
  _blArrayModifyOp: function(self: Pointer; op: Integer; n: Size_T; dataOut: PPointer): Integer; cdecl = nil;
  _blArrayInsertOp: function(self: Pointer; index: Size_T; n: Size_T; dataOut: PPointer): Integer; cdecl = nil;
  _blArrayAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blArrayAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blArrayAssignDeep: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blArrayAssignData: function(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl = nil;
  _blArrayAssignExternalData: function(self: Pointer; data: Pointer; size: Size_T; capacity: Size_T; dataAccessFlags: Integer; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl = nil;
  _blArrayAppendU8: function(self: Pointer; value: UInt8): Integer; cdecl = nil;
  _blArrayAppendU16: function(self: Pointer; value: UInt16): Integer; cdecl = nil;
  _blArrayAppendU32: function(self: Pointer; value: UInt32): Integer; cdecl = nil;
  _blArrayAppendU64: function(self: Pointer; value: UInt64): Integer; cdecl = nil;
  _blArrayAppendF32: function(self: Pointer; value: Single): Integer; cdecl = nil;
  _blArrayAppendF64: function(self: Pointer; value: Double): Integer; cdecl = nil;
  _blArrayAppendItem: function(self: Pointer; const item: Pointer): Integer; cdecl = nil;
  _blArrayAppendData: function(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl = nil;
  _blArrayInsertU8: function(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl = nil;
  _blArrayInsertU16: function(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl = nil;
  _blArrayInsertU32: function(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl = nil;
  _blArrayInsertU64: function(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl = nil;
  _blArrayInsertF32: function(self: Pointer; index: Size_T; value: Single): Integer; cdecl = nil;
  _blArrayInsertF64: function(self: Pointer; index: Size_T; value: Double): Integer; cdecl = nil;
  _blArrayInsertItem: function(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl = nil;
  _blArrayInsertData: function(self: Pointer; index: Size_T; const data: Pointer; n: Size_T): Integer; cdecl = nil;
  _blArrayReplaceU8: function(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl = nil;
  _blArrayReplaceU16: function(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl = nil;
  _blArrayReplaceU32: function(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl = nil;
  _blArrayReplaceU64: function(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl = nil;
  _blArrayReplaceF32: function(self: Pointer; index: Size_T; value: Single): Integer; cdecl = nil;
  _blArrayReplaceF64: function(self: Pointer; index: Size_T; value: Double): Integer; cdecl = nil;
  _blArrayReplaceItem: function(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl = nil;
  _blArrayReplaceData: function(self: Pointer; rStart: Size_T; rEnd: Size_T; const data: Pointer; n: Size_T): Integer; cdecl = nil;
  _blArrayRemoveIndex: function(self: Pointer; index: Size_T): Integer; cdecl = nil;
  _blArrayRemoveRange: function(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl = nil;
  _blArrayEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // bitarray.h
  _blBitArrayInit: function(self: Pointer): Integer; cdecl = nil;
  _blBitArrayInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blBitArrayInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blBitArrayDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blBitArrayReset: function(self: Pointer): Integer; cdecl = nil;
  _blBitArrayAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blBitArrayAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blBitArrayAssignWords: function(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl = nil;
  _blBitArrayIsEmpty: function(const self: Pointer): Boolean; cdecl = nil;
  _blBitArrayGetSize: function(const self: Pointer): UInt32; cdecl = nil;
  _blBitArrayGetWordCount: function(const self: Pointer): UInt32; cdecl = nil;
  _blBitArrayGetCapacity: function(const self: Pointer): UInt32; cdecl = nil;
  _blBitArrayGetData: function(const self: Pointer): PUInt32; cdecl = nil;
  _blBitArrayGetCardinality: function(const self: Pointer): UInt32; cdecl = nil;
  _blBitArrayGetCardinalityInRange: function(const self: Pointer; startBit: UInt32; endBit: UInt32): UInt32; cdecl = nil;
  _blBitArrayHasBit: function(const self: Pointer; bitIndex: UInt32): Boolean; cdecl = nil;
  _blBitArrayHasBitsInRange: function(const self: Pointer; startBit: UInt32; endBit: UInt32): Boolean; cdecl = nil;
  _blBitArraySubsumes: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blBitArrayIntersects: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blBitArrayGetRange: function(const self: Pointer; startOut: PUInt32; endOut: PUInt32): Boolean; cdecl = nil;
  _blBitArrayEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blBitArrayCompare: function(const a: Pointer; const b: Pointer): Integer; cdecl = nil;
  _blBitArrayClear: function(self: Pointer): Integer; cdecl = nil;
  _blBitArrayResize: function(self: Pointer; nBits: UInt32): Integer; cdecl = nil;
  _blBitArrayReserve: function(self: Pointer; nBits: UInt32): Integer; cdecl = nil;
  _blBitArrayShrink: function(self: Pointer): Integer; cdecl = nil;
  _blBitArraySetBit: function(self: Pointer; bitIndex: UInt32): Integer; cdecl = nil;
  _blBitArrayFillRange: function(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl = nil;
  _blBitArrayFillWords: function(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl = nil;
  _blBitArrayClearBit: function(self: Pointer; bitIndex: UInt32): Integer; cdecl = nil;
  _blBitArrayClearRange: function(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl = nil;
  _blBitArrayClearWord: function(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl = nil;
  _blBitArrayClearWords: function(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl = nil;
  _blBitArrayReplaceOp: function(self: Pointer; nBits: UInt32; dataOut: PPUInt32): Integer; cdecl = nil;
  _blBitArrayReplaceBit: function(self: Pointer; bitIndex: UInt32; bitValue: Boolean): Integer; cdecl = nil;
  _blBitArrayReplaceWord: function(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl = nil;
  _blBitArrayReplaceWords: function(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl = nil;
  _blBitArrayAppendBit: function(self: Pointer; bitValue: Boolean): Integer; cdecl = nil;
  _blBitArrayAppendWord: function(self: Pointer; wordValue: UInt32): Integer; cdecl = nil;
  _blBitArrayAppendWords: function(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl = nil;

  // context.h
  _blContextInit: function(self: Pointer): Integer; cdecl = nil;
  _blContextInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blContextInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blContextInitAs: function(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl = nil;
  _blContextDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blContextReset: function(self: Pointer): Integer; cdecl = nil;
  _blContextAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blContextAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blContextGetType: function(const self: Pointer): Integer; cdecl = nil;
  _blContextGetTargetSize: function(const self: Pointer; targetSizeOut: Pointer): Integer; cdecl = nil;
  _blContextGetTargetImage: function(const self: Pointer): Pointer; cdecl = nil;
  _blContextBegin: function(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl = nil;
  _blContextEnd: function(self: Pointer): Integer; cdecl = nil;
  _blContextFlush: function(self: Pointer; flags: Cardinal): Integer; cdecl = nil;
  _blContextSave: function(self: Pointer; cookie: Pointer): Integer; cdecl = nil;
  _blContextRestore: function(self: Pointer; const cookie: Pointer): Integer; cdecl = nil;
  _blContextGetMetaTransform: function(const self: Pointer; transformOut: Pointer): Integer; cdecl = nil;
  _blContextGetUserTransform: function(const self: Pointer; transformOut: Pointer): Integer; cdecl = nil;
  _blContextGetFinalTransform: function(const self: Pointer; transformOut: Pointer): Integer; cdecl = nil;
  _blContextUserToMeta: function(self: Pointer): Integer; cdecl = nil;
  _blContextApplyTransformOp: function(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl = nil;
  _blContextGetHint: function(const self: Pointer; hintType: Integer): UInt32; cdecl = nil;
  _blContextSetHint: function(self: Pointer; hintType: Integer; value: UInt32): Integer; cdecl = nil;
  _blContextGetHints: function(const self: Pointer; hintsOut: Pointer): Integer; cdecl = nil;
  _blContextSetHints: function(self: Pointer; const hints: Pointer): Integer; cdecl = nil;
  _blContextSetFlattenMode: function(self: Pointer; mode: Integer): Integer; cdecl = nil;
  _blContextSetFlattenTolerance: function(self: Pointer; tolerance: Double): Integer; cdecl = nil;
  _blContextSetApproximationOptions: function(self: Pointer; const options: Pointer): Integer; cdecl = nil;
  _blContextGetFillStyle: function(const self: Pointer; styleOut: Pointer): Integer; cdecl = nil;
  _blContextGetTransformedFillStyle: function(const self: Pointer; styleOut: Pointer): Integer; cdecl = nil;
  _blContextSetFillStyle: function(self: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextSetFillStyleWithMode: function(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl = nil;
  _blContextSetFillStyleRgba: function(self: Pointer; const rgba: Pointer): Integer; cdecl = nil;
  _blContextSetFillStyleRgba32: function(self: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextSetFillStyleRgba64: function(self: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextDisableFillStyle: function(self: Pointer): Integer; cdecl = nil;
  _blContextGetFillAlpha: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetFillAlpha: function(self: Pointer; alpha: Double): Integer; cdecl = nil;
  _blContextGetStrokeStyle: function(const self: Pointer; styleOut: Pointer): Integer; cdecl = nil;
  _blContextGetTransformedStrokeStyle: function(const self: Pointer; styleOut: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeStyle: function(self: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeStyleWithMode: function(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl = nil;
  _blContextSetStrokeStyleRgba: function(self: Pointer; const rgba: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeStyleRgba32: function(self: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextSetStrokeStyleRgba64: function(self: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextDisableStrokeStyle: function(self: Pointer): Integer; cdecl = nil;
  _blContextGetStrokeAlpha: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetStrokeAlpha: function(self: Pointer; alpha: Double): Integer; cdecl = nil;
  _blContextSwapStyles: function(self: Pointer; mode: Integer): Integer; cdecl = nil;
  _blContextGetGlobalAlpha: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetGlobalAlpha: function(self: Pointer; alpha: Double): Integer; cdecl = nil;
  _blContextGetCompOp: function(const self: Pointer): Integer; cdecl = nil;
  _blContextSetCompOp: function(self: Pointer; compOp: Integer): Integer; cdecl = nil;
  _blContextGetFillRule: function(const self: Pointer): Integer; cdecl = nil;
  _blContextSetFillRule: function(self: Pointer; fillRule: Integer): Integer; cdecl = nil;
  _blContextGetStrokeWidth: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetStrokeWidth: function(self: Pointer; width: Double): Integer; cdecl = nil;
  _blContextGetStrokeMiterLimit: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetStrokeMiterLimit: function(self: Pointer; miterLimit: Double): Integer; cdecl = nil;
  _blContextGetStrokeCap: function(const self: Pointer; position: Integer): Integer; cdecl = nil;
  _blContextSetStrokeCap: function(self: Pointer; position: Integer; strokeCap: Integer): Integer; cdecl = nil;
  _blContextSetStrokeCaps: function(self: Pointer; strokeCap: Integer): Integer; cdecl = nil;
  _blContextGetStrokeJoin: function(const self: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeJoin: function(self: Pointer; strokeJoin: Integer): Integer; cdecl = nil;
  _blContextGetStrokeTransformOrder: function(const self: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeTransformOrder: function(self: Pointer; transformOrder: Integer): Integer; cdecl = nil;
  _blContextGetStrokeDashOffset: function(const self: Pointer): Double; cdecl = nil;
  _blContextSetStrokeDashOffset: function(self: Pointer; dashOffset: Double): Integer; cdecl = nil;
  _blContextGetStrokeDashArray: function(const self: Pointer; dashArrayOut: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeDashArray: function(self: Pointer; const dashArray: Pointer): Integer; cdecl = nil;
  _blContextGetStrokeOptions: function(const self: Pointer; options: Pointer): Integer; cdecl = nil;
  _blContextSetStrokeOptions: function(self: Pointer; const options: Pointer): Integer; cdecl = nil;
  _blContextClipToRectI: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextClipToRectD: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextRestoreClipping: function(self: Pointer): Integer; cdecl = nil;
  _blContextClearAll: function(self: Pointer): Integer; cdecl = nil;
  _blContextClearRectI: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextClearRectD: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextFillAll: function(self: Pointer): Integer; cdecl = nil;
  _blContextFillAllRgba32: function(self: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillAllRgba64: function(self: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillAllExt: function(self: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillRectI: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextFillRectIRgba32: function(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillRectIRgba64: function(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillRectIExt: function(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillRectD: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextFillRectDRgba32: function(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillRectDRgba64: function(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillRectDExt: function(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillPathD: function(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl = nil;
  _blContextFillPathDRgba32: function(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillPathDRgba64: function(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillPathDExt: function(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillGeometry: function(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl = nil;
  _blContextFillGeometryRgba32: function(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillGeometryRgba64: function(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillGeometryExt: function(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf8TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf8TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf8TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf8TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf8TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf8TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf8TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf8TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf16TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf16TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf16TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf16TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf16TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf16TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf16TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf16TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf32TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf32TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf32TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf32TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillUtf32TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl = nil;
  _blContextFillUtf32TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillUtf32TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillUtf32TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextFillGlyphRunI: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl = nil;
  _blContextFillGlyphRunIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillGlyphRunIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillGlyphRunIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillGlyphRunD: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl = nil;
  _blContextFillGlyphRunDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillGlyphRunDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillGlyphRunDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillMaskI: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl = nil;
  _blContextFillMaskIRgba32: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillMaskIRgba64: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillMaskIExt: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextFillMaskD: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl = nil;
  _blContextFillMaskDRgba32: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextFillMaskDRgba64: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextFillMaskDExt: function(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeRectI: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextStrokeRectIRgba32: function(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeRectIRgba64: function(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeRectIExt: function(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeRectD: function(self: Pointer; const rect: Pointer): Integer; cdecl = nil;
  _blContextStrokeRectDRgba32: function(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeRectDRgba64: function(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeRectDExt: function(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokePathD: function(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl = nil;
  _blContextStrokePathDRgba32: function(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokePathDRgba64: function(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokePathDExt: function(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeGeometry: function(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl = nil;
  _blContextStrokeGeometryRgba32: function(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeGeometryRgba64: function(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeGeometryExt: function(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf8TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf8TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf8TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf8TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf8TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf8TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf8TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf8TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf16TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf16TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf16TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf16TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf16TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf16TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf16TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf16TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf32TextI: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf32TextIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf32TextIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf32TextIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeUtf32TextD: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl = nil;
  _blContextStrokeUtf32TextDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeUtf32TextDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeUtf32TextDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeGlyphRunI: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl = nil;
  _blContextStrokeGlyphRunIRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeGlyphRunIRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeGlyphRunIExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextStrokeGlyphRunD: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl = nil;
  _blContextStrokeGlyphRunDRgba32: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blContextStrokeGlyphRunDRgba64: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blContextStrokeGlyphRunDExt: function(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl = nil;
  _blContextBlitImageI: function(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl = nil;
  _blContextBlitImageD: function(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl = nil;
  _blContextBlitScaledImageI: function(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl = nil;
  _blContextBlitScaledImageD: function(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl = nil;

  // filesystem.h
  _blFileInit: function(self: Pointer): Integer; cdecl = nil;
  _blFileReset: function(self: Pointer): Integer; cdecl = nil;
  _blFileOpen: function(self: Pointer; const fileName: PUTF8Char; openFlags: Cardinal): Integer; cdecl = nil;
  _blFileClose: function(self: Pointer): Integer; cdecl = nil;
  _blFileSeek: function(self: Pointer; offset: Int64; seekType: Integer; positionOut: PInt64): Integer; cdecl = nil;
  _blFileRead: function(self: Pointer; buffer: Pointer; n: Size_T; bytesReadOut: PSize_T): Integer; cdecl = nil;
  _blFileWrite: function(self: Pointer; const buffer: Pointer; n: Size_T; bytesWrittenOut: PSize_T): Integer; cdecl = nil;
  _blFileTruncate: function(self: Pointer; maxSize: Int64): Integer; cdecl = nil;
  _blFileGetInfo: function(self: Pointer; infoOut: Pointer): Integer; cdecl = nil;
  _blFileGetSize: function(self: Pointer; fileSizeOut: PUInt64): Integer; cdecl = nil;
  _blFileSystemGetInfo: function(const fileName: PUTF8Char; infoOut: Pointer): Integer; cdecl = nil;
  _blFileSystemReadFile: function(const fileName: PUTF8Char; dst: Pointer; maxSize: Size_T; readFlags: UInt32): Integer; cdecl = nil;
  _blFileSystemWriteFile: function(const fileName: PUTF8Char; const data: Pointer; size: Size_T; bytesWrittenOut: PSize_T): Integer; cdecl = nil;

  // font.h
  _blFontInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blFontCreateFromFace: function(self: Pointer; const face: Pointer; size: Single): Integer; cdecl = nil;
  _blFontCreateFromFaceWithSettings: function(self: Pointer; const face: Pointer; size: Single; const featureSettings: Pointer; const variationSettings: Pointer): Integer; cdecl = nil;
  _blFontGetFace: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontGetSize: function(const self: Pointer): Single; cdecl = nil;
  _blFontSetSize: function(self: Pointer; size: Single): Integer; cdecl = nil;
  _blFontGetMetrics: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontGetMatrix: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontGetDesignMetrics: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontGetFeatureSettings: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontSetFeatureSettings: function(self: Pointer; const featureSettings: Pointer): Integer; cdecl = nil;
  _blFontResetFeatureSettings: function(self: Pointer): Integer; cdecl = nil;
  _blFontGetVariationSettings: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontSetVariationSettings: function(self: Pointer; const variationSettings: Pointer): Integer; cdecl = nil;
  _blFontResetVariationSettings: function(self: Pointer): Integer; cdecl = nil;
  _blFontShape: function(const self: Pointer; gb: Pointer): Integer; cdecl = nil;
  _blFontMapTextToGlyphs: function(const self: Pointer; gb: Pointer; stateOut: Pointer): Integer; cdecl = nil;
  _blFontPositionGlyphs: function(const self: Pointer; gb: Pointer): Integer; cdecl = nil;
  _blFontApplyKerning: function(const self: Pointer; gb: Pointer): Integer; cdecl = nil;
  _blFontApplyGSub: function(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl = nil;
  _blFontApplyGPos: function(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl = nil;
  _blFontGetTextMetrics: function(const self: Pointer; gb: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontGetGlyphBounds: function(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl = nil;
  _blFontGetGlyphAdvances: function(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl = nil;
  _blFontGetGlyphOutlines: function(const self: Pointer; glyphId: TBLGlyphId; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl = nil;
  _blFontGetGlyphRunOutlines: function(const self: Pointer; const glyphRun: Pointer; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl = nil;

  // fontdata.h
  _blFontDataInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontDataInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontDataInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontDataDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontDataReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontDataAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontDataAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontDataCreateFromFile: function(self: Pointer; const fileName: PUTF8Char; readFlags: UInt32): Integer; cdecl = nil;
  _blFontDataCreateFromDataArray: function(self: Pointer; const dataArray: Pointer): Integer; cdecl = nil;
  _blFontDataCreateFromData: function(self: Pointer; const data: Pointer; dataSize: Size_T; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl = nil;
  _blFontDataEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blFontDataGetFaceCount: function(const self: Pointer): UInt32; cdecl = nil;
  _blFontDataGetFaceType: function(const self: Pointer): Integer; cdecl = nil;
  _blFontDataGetFlags: function(const self: Pointer): Cardinal; cdecl = nil;
  _blFontDataGetTableTags: function(const self: Pointer; faceIndex: UInt32; dst: Pointer): Integer; cdecl = nil;
  _blFontDataGetTables: function(const self: Pointer; faceIndex: UInt32; dst: Pointer; const tags: Pointer; count: Size_T): Size_T; cdecl = nil;

  // fontface.h
  _blFontFaceInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontFaceInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontFaceInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontFaceDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontFaceReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontFaceAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontFaceAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontFaceEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blFontFaceCreateFromFile: function(self: Pointer; const fileName: PUTF8Char; readFlags: UInt32): Integer; cdecl = nil;
  _blFontFaceCreateFromData: function(self: Pointer; const fontData: Pointer; faceIndex: UInt32): Integer; cdecl = nil;
  _blFontFaceGetFullName: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetFamilyName: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetSubfamilyName: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetPostScriptName: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetFaceInfo: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetDesignMetrics: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetUnicodeCoverage: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetCharacterCoverage: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceHasScriptTag: function(const self: Pointer; scriptTag: UInt32): Boolean; cdecl = nil;
  _blFontFaceHasFeatureTag: function(const self: Pointer; featureTag: UInt32): Boolean; cdecl = nil;
  _blFontFaceHasVariationTag: function(const self: Pointer; variationTag: UInt32): Boolean; cdecl = nil;
  _blFontFaceGetScriptTags: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetFeatureTags: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFaceGetVariationTags: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;

  // fontfeaturesettings.h
  _blFontFeatureSettingsInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsClear: function(self: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsShrink: function(self: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontFeatureSettingsGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontFeatureSettingsGetView: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontFeatureSettingsHasValue: function(const self: Pointer; featureTag: UInt32): Boolean; cdecl = nil;
  _blFontFeatureSettingsGetValue: function(const self: Pointer; featureTag: UInt32): UInt32; cdecl = nil;
  _blFontFeatureSettingsSetValue: function(self: Pointer; featureTag: UInt32; value: UInt32): Integer; cdecl = nil;
  _blFontFeatureSettingsRemoveValue: function(self: Pointer; featureTag: UInt32): Integer; cdecl = nil;
  _blFontFeatureSettingsEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // fontmanager.h
  _blFontManagerInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontManagerInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontManagerInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontManagerInitNew: function(self: Pointer): Integer; cdecl = nil;
  _blFontManagerDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontManagerReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontManagerAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontManagerAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontManagerCreate: function(self: Pointer): Integer; cdecl = nil;
  _blFontManagerGetFaceCount: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontManagerGetFamilyCount: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontManagerHasFace: function(const self: Pointer; const face: Pointer): Boolean; cdecl = nil;
  _blFontManagerAddFace: function(self: Pointer; const face: Pointer): Integer; cdecl = nil;
  _blFontManagerQueryFace: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; const properties: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontManagerQueryFacesByFamilyName: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; &out: Pointer): Integer; cdecl = nil;
  _blFontManagerEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // fontvariationsettings.h
  _blFontVariationSettingsInit: function(self: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsReset: function(self: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsClear: function(self: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsShrink: function(self: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontVariationSettingsGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blFontVariationSettingsGetView: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blFontVariationSettingsHasValue: function(const self: Pointer; variationTag: UInt32): Boolean; cdecl = nil;
  _blFontVariationSettingsGetValue: function(const self: Pointer; variationTag: UInt32): Single; cdecl = nil;
  _blFontVariationSettingsSetValue: function(self: Pointer; variationTag: UInt32; value: Single): Integer; cdecl = nil;
  _blFontVariationSettingsRemoveValue: function(self: Pointer; variationTag: UInt32): Integer; cdecl = nil;
  _blFontVariationSettingsEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // format.h
  _blFormatInfoQuery: function(self: Pointer; format: Integer): Integer; cdecl = nil;
  _blFormatInfoSanitize: function(self: Pointer): Integer; cdecl = nil;

  // glyphbuffer.h
  _blGlyphBufferInit: function(self: Pointer): Integer; cdecl = nil;
  _blGlyphBufferInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blGlyphBufferDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blGlyphBufferReset: function(self: Pointer): Integer; cdecl = nil;
  _blGlyphBufferClear: function(self: Pointer): Integer; cdecl = nil;
  _blGlyphBufferGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blGlyphBufferGetFlags: function(const self: Pointer): UInt32; cdecl = nil;
  _blGlyphBufferGetGlyphRun: function(const self: Pointer): Pointer; cdecl = nil;
  _blGlyphBufferGetContent: function(const self: Pointer): PUInt32; cdecl = nil;
  _blGlyphBufferGetInfoData: function(const self: Pointer): Pointer; cdecl = nil;
  _blGlyphBufferGetPlacementData: function(const self: Pointer): Pointer; cdecl = nil;
  _blGlyphBufferSetText: function(self: Pointer; const textData: Pointer; size: Size_T; encoding: Integer): Integer; cdecl = nil;
  _blGlyphBufferSetGlyphs: function(self: Pointer; const glyphData: PUInt32; size: Size_T): Integer; cdecl = nil;
  _blGlyphBufferSetGlyphsFromStruct: function(self: Pointer; const glyphData: Pointer; size: Size_T; glyphIdSize: Size_T; glyphIdAdvance: IntPtr): Integer; cdecl = nil;
  _blGlyphBufferSetDebugSink: function(self: Pointer; sink: TBLDebugMessageSinkFunc; userData: Pointer): Integer; cdecl = nil;
  _blGlyphBufferResetDebugSink: function(self: Pointer): Integer; cdecl = nil;

  // gradient.h
  _blGradientInit: function(self: Pointer): Integer; cdecl = nil;
  _blGradientInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blGradientInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blGradientInitAs: function(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl = nil;
  _blGradientDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blGradientReset: function(self: Pointer): Integer; cdecl = nil;
  _blGradientAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blGradientAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blGradientCreate: function(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl = nil;
  _blGradientShrink: function(self: Pointer): Integer; cdecl = nil;
  _blGradientReserve: function(self: Pointer; n: Size_T): Integer; cdecl = nil;
  _blGradientGetType: function(const self: Pointer): Integer; cdecl = nil;
  _blGradientSetType: function(self: Pointer; &type: Integer): Integer; cdecl = nil;
  _blGradientGetExtendMode: function(const self: Pointer): Integer; cdecl = nil;
  _blGradientSetExtendMode: function(self: Pointer; extendMode: Integer): Integer; cdecl = nil;
  _blGradientGetValue: function(const self: Pointer; index: Size_T): Double; cdecl = nil;
  _blGradientSetValue: function(self: Pointer; index: Size_T; value: Double): Integer; cdecl = nil;
  _blGradientSetValues: function(self: Pointer; index: Size_T; const values: PDouble; n: Size_T): Integer; cdecl = nil;
  _blGradientGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blGradientGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blGradientGetStops: function(const self: Pointer): Pointer; cdecl = nil;
  _blGradientResetStops: function(self: Pointer): Integer; cdecl = nil;
  _blGradientAssignStops: function(self: Pointer; const stops: Pointer; n: Size_T): Integer; cdecl = nil;
  _blGradientAddStopRgba32: function(self: Pointer; offset: Double; argb32: UInt32): Integer; cdecl = nil;
  _blGradientAddStopRgba64: function(self: Pointer; offset: Double; argb64: UInt64): Integer; cdecl = nil;
  _blGradientRemoveStop: function(self: Pointer; index: Size_T): Integer; cdecl = nil;
  _blGradientRemoveStopByOffset: function(self: Pointer; offset: Double; all: UInt32): Integer; cdecl = nil;
  _blGradientRemoveStopsByIndex: function(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl = nil;
  _blGradientRemoveStopsByOffset: function(self: Pointer; offsetMin: Double; offsetMax: Double): Integer; cdecl = nil;
  _blGradientReplaceStopRgba32: function(self: Pointer; index: Size_T; offset: Double; rgba32: UInt32): Integer; cdecl = nil;
  _blGradientReplaceStopRgba64: function(self: Pointer; index: Size_T; offset: Double; rgba64: UInt64): Integer; cdecl = nil;
  _blGradientIndexOfStop: function(const self: Pointer; offset: Double): Size_T; cdecl = nil;
  _blGradientGetTransform: function(const self: Pointer; transformOut: Pointer): Integer; cdecl = nil;
  _blGradientGetTransformType: function(const self: Pointer): Integer; cdecl = nil;
  _blGradientApplyTransformOp: function(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl = nil;
  _blGradientEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // image.h
  _blImageInit: function(self: Pointer): Integer; cdecl = nil;
  _blImageInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageInitAs: function(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl = nil;
  _blImageInitAsFromData: function(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl = nil;
  _blImageDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blImageReset: function(self: Pointer): Integer; cdecl = nil;
  _blImageAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageAssignDeep: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageCreate: function(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl = nil;
  _blImageCreateFromData: function(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl = nil;
  _blImageGetData: function(const self: Pointer; dataOut: Pointer): Integer; cdecl = nil;
  _blImageMakeMutable: function(self: Pointer; dataOut: Pointer): Integer; cdecl = nil;
  _blImageConvert: function(self: Pointer; format: Integer): Integer; cdecl = nil;
  _blImageEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blImageScale: function(dst: Pointer; const src: Pointer; const size: Pointer; filter: Integer): Integer; cdecl = nil;
  _blImageReadFromFile: function(self: Pointer; const fileName: PUTF8Char; const codecs: Pointer): Integer; cdecl = nil;
  _blImageReadFromData: function(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl = nil;
  _blImageWriteToFile: function(const self: Pointer; const fileName: PUTF8Char; const codec: Pointer): Integer; cdecl = nil;
  _blImageWriteToData: function(const self: Pointer; dst: Pointer; const codec: Pointer): Integer; cdecl = nil;

  // imagecodec.h
  _blImageCodecInit: function(self: Pointer): Integer; cdecl = nil;
  _blImageCodecInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageCodecInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageCodecInitByName: function(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl = nil;
  _blImageCodecDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blImageCodecReset: function(self: Pointer): Integer; cdecl = nil;
  _blImageCodecAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageCodecAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageCodecFindByName: function(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl = nil;
  _blImageCodecFindByExtension: function(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl = nil;
  _blImageCodecFindByData: function(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl = nil;
  _blImageCodecInspectData: function(const self: Pointer; const data: Pointer; size: Size_T): UInt32; cdecl = nil;
  _blImageCodecCreateDecoder: function(const self: Pointer; dst: Pointer): Integer; cdecl = nil;
  _blImageCodecCreateEncoder: function(const self: Pointer; dst: Pointer): Integer; cdecl = nil;
  _blImageCodecArrayInitBuiltInCodecs: function(self: Pointer): Integer; cdecl = nil;
  _blImageCodecArrayAssignBuiltInCodecs: function(self: Pointer): Integer; cdecl = nil;
  _blImageCodecAddToBuiltIn: function(const codec: Pointer): Integer; cdecl = nil;
  _blImageCodecRemoveFromBuiltIn: function(const codec: Pointer): Integer; cdecl = nil;

  // imagedecoder.h
  _blImageDecoderInit: function(self: Pointer): Integer; cdecl = nil;
  _blImageDecoderInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageDecoderInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageDecoderDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blImageDecoderReset: function(self: Pointer): Integer; cdecl = nil;
  _blImageDecoderAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageDecoderAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageDecoderRestart: function(self: Pointer): Integer; cdecl = nil;
  _blImageDecoderReadInfo: function(self: Pointer; infoOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl = nil;
  _blImageDecoderReadFrame: function(self: Pointer; imageOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl = nil;

  // imageencoder.h
  _blImageEncoderInit: function(self: Pointer): Integer; cdecl = nil;
  _blImageEncoderInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageEncoderInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageEncoderDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blImageEncoderReset: function(self: Pointer): Integer; cdecl = nil;
  _blImageEncoderAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blImageEncoderAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blImageEncoderRestart: function(self: Pointer): Integer; cdecl = nil;
  _blImageEncoderWriteFrame: function(self: Pointer; dst: Pointer; const image: Pointer): Integer; cdecl = nil;

  // matrix.h
  _blMatrix2DSetIdentity: function(self: Pointer): Integer; cdecl = nil;
  _blMatrix2DSetTranslation: function(self: Pointer; x: Double; y: Double): Integer; cdecl = nil;
  _blMatrix2DSetScaling: function(self: Pointer; x: Double; y: Double): Integer; cdecl = nil;
  _blMatrix2DSetSkewing: function(self: Pointer; x: Double; y: Double): Integer; cdecl = nil;
  _blMatrix2DSetRotation: function(self: Pointer; angle: Double; cx: Double; cy: Double): Integer; cdecl = nil;
  _blMatrix2DApplyOp: function(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl = nil;
  _blMatrix2DInvert: function(dst: Pointer; const src: Pointer): Integer; cdecl = nil;
  _blMatrix2DGetType: function(const self: Pointer): Integer; cdecl = nil;
  _blMatrix2DMapPointDArray: function(const self: Pointer; dst: Pointer; const src: Pointer; count: Size_T): Integer; cdecl = nil;

  // object.h
  _blObjectAllocImpl: function(self: Pointer; objectInfo: UInt32; implSize: Size_T): Integer; cdecl = nil;
  _blObjectAllocImplAligned: function(self: Pointer; objectInfo: UInt32; implSize: Size_T; implAlignment: Size_T): Integer; cdecl = nil;
  _blObjectAllocImplExternal: function(self: Pointer; objectInfo: UInt32; implSize: Size_T; immutable: Boolean; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl = nil;
  _blObjectFreeImpl: function(impl: Pointer): Integer; cdecl = nil;
  _blObjectInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blObjectInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blObjectReset: function(self: Pointer): Integer; cdecl = nil;
  _blObjectAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blObjectAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blObjectGetProperty: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: Pointer): Integer; cdecl = nil;
  _blObjectGetPropertyBool: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PBoolean): Integer; cdecl = nil;
  _blObjectGetPropertyInt32: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PInt32): Integer; cdecl = nil;
  _blObjectGetPropertyInt64: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PInt64): Integer; cdecl = nil;
  _blObjectGetPropertyUInt32: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PUInt32): Integer; cdecl = nil;
  _blObjectGetPropertyUInt64: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PUInt64): Integer; cdecl = nil;
  _blObjectGetPropertyDouble: function(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PDouble): Integer; cdecl = nil;
  _blObjectSetProperty: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; const value: Pointer): Integer; cdecl = nil;
  _blObjectSetPropertyBool: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Boolean): Integer; cdecl = nil;
  _blObjectSetPropertyInt32: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Int32): Integer; cdecl = nil;
  _blObjectSetPropertyInt64: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Int64): Integer; cdecl = nil;
  _blObjectSetPropertyUInt32: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: UInt32): Integer; cdecl = nil;
  _blObjectSetPropertyUInt64: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: UInt64): Integer; cdecl = nil;
  _blObjectSetPropertyDouble: function(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Double): Integer; cdecl = nil;

  // path.h
  _blPathInit: function(self: Pointer): Integer; cdecl = nil;
  _blPathInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blPathInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPathDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blPathReset: function(self: Pointer): Integer; cdecl = nil;
  _blPathGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blPathGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blPathGetCommandData: function(const self: Pointer): PUInt8; cdecl = nil;
  _blPathGetVertexData: function(const self: Pointer): Pointer; cdecl = nil;
  _blPathClear: function(self: Pointer): Integer; cdecl = nil;
  _blPathShrink: function(self: Pointer): Integer; cdecl = nil;
  _blPathReserve: function(self: Pointer; n: Size_T): Integer; cdecl = nil;
  _blPathModifyOp: function(self: Pointer; op: Integer; n: Size_T; cmdDataOut: PPUInt8; vtxDataOut: Pointer): Integer; cdecl = nil;
  _blPathAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blPathAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPathAssignDeep: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPathSetVertexAt: function(self: Pointer; index: Size_T; cmd: UInt32; x: Double; y: Double): Integer; cdecl = nil;
  _blPathMoveTo: function(self: Pointer; x0: Double; y0: Double): Integer; cdecl = nil;
  _blPathLineTo: function(self: Pointer; x1: Double; y1: Double): Integer; cdecl = nil;
  _blPathPolyTo: function(self: Pointer; const poly: Pointer; count: Size_T): Integer; cdecl = nil;
  _blPathQuadTo: function(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl = nil;
  _blPathConicTo: function(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; w: Double): Integer; cdecl = nil;
  _blPathCubicTo: function(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl = nil;
  _blPathSmoothQuadTo: function(self: Pointer; x2: Double; y2: Double): Integer; cdecl = nil;
  _blPathSmoothCubicTo: function(self: Pointer; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl = nil;
  _blPathArcTo: function(self: Pointer; x: Double; y: Double; rx: Double; ry: Double; start: Double; sweep: Double; forceMoveTo: Boolean): Integer; cdecl = nil;
  _blPathArcQuadrantTo: function(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl = nil;
  _blPathEllipticArcTo: function(self: Pointer; rx: Double; ry: Double; xAxisRotation: Double; largeArcFlag: Boolean; sweepFlag: Boolean; x1: Double; y1: Double): Integer; cdecl = nil;
  _blPathClose: function(self: Pointer): Integer; cdecl = nil;
  _blPathAddGeometry: function(self: Pointer; geometryType: Integer; const geometryData: Pointer; const m: Pointer; dir: Integer): Integer; cdecl = nil;
  _blPathAddBoxI: function(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl = nil;
  _blPathAddBoxD: function(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl = nil;
  _blPathAddRectI: function(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl = nil;
  _blPathAddRectD: function(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl = nil;
  _blPathAddPath: function(self: Pointer; const other: Pointer; const range: Pointer): Integer; cdecl = nil;
  _blPathAddTranslatedPath: function(self: Pointer; const other: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl = nil;
  _blPathAddTransformedPath: function(self: Pointer; const other: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl = nil;
  _blPathAddReversedPath: function(self: Pointer; const other: Pointer; const range: Pointer; reverseMode: Integer): Integer; cdecl = nil;
  _blPathAddStrokedPath: function(self: Pointer; const other: Pointer; const range: Pointer; const options: Pointer; const approx: Pointer): Integer; cdecl = nil;
  _blPathRemoveRange: function(self: Pointer; const range: Pointer): Integer; cdecl = nil;
  _blPathTranslate: function(self: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl = nil;
  _blPathTransform: function(self: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl = nil;
  _blPathFitTo: function(self: Pointer; const range: Pointer; const rect: Pointer; fitFlags: UInt32): Integer; cdecl = nil;
  _blPathEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blPathGetInfoFlags: function(const self: Pointer; flagsOut: PUInt32): Integer; cdecl = nil;
  _blPathGetControlBox: function(const self: Pointer; boxOut: Pointer): Integer; cdecl = nil;
  _blPathGetBoundingBox: function(const self: Pointer; boxOut: Pointer): Integer; cdecl = nil;
  _blPathGetFigureRange: function(const self: Pointer; index: Size_T; rangeOut: Pointer): Integer; cdecl = nil;
  _blPathGetLastVertex: function(const self: Pointer; vtxOut: Pointer): Integer; cdecl = nil;
  _blPathGetClosestVertex: function(const self: Pointer; const p: Pointer; maxDistance: Double; indexOut: PSize_T; distanceOut: PDouble): Integer; cdecl = nil;
  _blPathHitTest: function(const self: Pointer; const p: Pointer; fillRule: Integer): Integer; cdecl = nil;
  _blStrokeOptionsInit: function(self: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsReset: function(self: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blStrokeOptionsAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blStrokeOptionsAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPathStrokeToSink: function(const self: Pointer; const range: Pointer; const strokeOptions: Pointer; const approximationOptions: Pointer; a: Pointer; b: Pointer; c: Pointer; sink: _BLPathStrokeSinkFunc; userData: Pointer): Integer; cdecl = nil;

  // pattern.h
  _blPatternInit: function(self: Pointer): Integer; cdecl = nil;
  _blPatternInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blPatternInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPatternInitAs: function(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl = nil;
  _blPatternDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blPatternReset: function(self: Pointer): Integer; cdecl = nil;
  _blPatternAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blPatternAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPatternAssignDeep: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPatternCreate: function(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl = nil;
  _blPatternGetImage: function(const self: Pointer; image: Pointer): Integer; cdecl = nil;
  _blPatternSetImage: function(self: Pointer; const image: Pointer; const area: Pointer): Integer; cdecl = nil;
  _blPatternResetImage: function(self: Pointer): Integer; cdecl = nil;
  _blPatternGetArea: function(const self: Pointer; areaOut: Pointer): Integer; cdecl = nil;
  _blPatternSetArea: function(self: Pointer; const area: Pointer): Integer; cdecl = nil;
  _blPatternResetArea: function(self: Pointer): Integer; cdecl = nil;
  _blPatternGetExtendMode: function(const self: Pointer): Integer; cdecl = nil;
  _blPatternSetExtendMode: function(self: Pointer; extendMode: Integer): Integer; cdecl = nil;
  _blPatternGetTransform: function(const self: Pointer; transformOut: Pointer): Integer; cdecl = nil;
  _blPatternGetTransformType: function(const self: Pointer): Integer; cdecl = nil;
  _blPatternApplyTransformOp: function(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl = nil;
  _blPatternEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;

  // pixelconverter.h
  _blPixelConverterInit: function(self: Pointer): Integer; cdecl = nil;
  _blPixelConverterInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPixelConverterDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blPixelConverterReset: function(self: Pointer): Integer; cdecl = nil;
  _blPixelConverterAssign: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blPixelConverterCreate: function(self: Pointer; const dstInfo: Pointer; const srcInfo: Pointer; createFlags: Integer): Integer; cdecl = nil;
  _blPixelConverterConvert: function(const self: Pointer; dstData: Pointer; dstStride: IntPtr; const srcData: Pointer; srcStride: IntPtr; w: UInt32; h: UInt32; const options: Pointer): Integer; cdecl = nil;

  // random.h
  _blRandomReset: function(self: Pointer; seed: UInt64): Integer; cdecl = nil;
  _blRandomNextUInt32: function(self: Pointer): UInt32; cdecl = nil;
  _blRandomNextUInt64: function(self: Pointer): UInt64; cdecl = nil;
  _blRandomNextDouble: function(self: Pointer): Double; cdecl = nil;

  // runtime.h
  _blRuntimeInit: function: Integer; cdecl = nil;
  _blRuntimeShutdown: function: Integer; cdecl = nil;
  _blRuntimeCleanup: function(cleanupFlags: Cardinal): Integer; cdecl = nil;
  _blRuntimeQueryInfo: function(infoType: Integer; infoOut: Pointer): Integer; cdecl = nil;
  _blRuntimeMessageOut: function(const msg: PUTF8Char): Integer; cdecl = nil;
  _blRuntimeMessageFmt: function(const fmt: PUTF8Char): Integer varargs; cdecl = nil;
  _blRuntimeMessageVFmt: function(const fmt: PUTF8Char; ap: Pointer): Integer; cdecl = nil;
  _blResultFromWinError: function(e: UInt32): Integer; cdecl = nil;

  // runtimescope.h
  _blRuntimeScopeBegin: function(self: Pointer): Integer; cdecl = nil;
  _blRuntimeScopeEnd: function(self: Pointer): Integer; cdecl = nil;
  _blRuntimeScopeIsActive: function(const self: Pointer): Boolean; cdecl = nil;

  // string.h
  _blStringInit: function(self: Pointer): Integer; cdecl = nil;
  _blStringInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blStringInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blStringInitWithData: function(self: Pointer; const str: PUTF8Char; size: Size_T): Integer; cdecl = nil;
  _blStringDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blStringReset: function(self: Pointer): Integer; cdecl = nil;
  _blStringGetData: function(const self: Pointer): PUTF8Char; cdecl = nil;
  _blStringGetSize: function(const self: Pointer): Size_T; cdecl = nil;
  _blStringGetCapacity: function(const self: Pointer): Size_T; cdecl = nil;
  _blStringClear: function(self: Pointer): Integer; cdecl = nil;
  _blStringShrink: function(self: Pointer): Integer; cdecl = nil;
  _blStringReserve: function(self: Pointer; n: Size_T): Integer; cdecl = nil;
  _blStringResize: function(self: Pointer; n: Size_T; fill: UTF8Char): Integer; cdecl = nil;
  _blStringMakeMutable: function(self: Pointer; dataOut: PPUTF8Char): Integer; cdecl = nil;
  _blStringModifyOp: function(self: Pointer; op: Integer; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl = nil;
  _blStringInsertOp: function(self: Pointer; index: Size_T; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl = nil;
  _blStringAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blStringAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blStringAssignDeep: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blStringAssignData: function(self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl = nil;
  _blStringApplyOpChar: function(self: Pointer; op: Integer; c: UTF8Char; n: Size_T): Integer; cdecl = nil;
  _blStringApplyOpData: function(self: Pointer; op: Integer; const str: PUTF8Char; n: Size_T): Integer; cdecl = nil;
  _blStringApplyOpString: function(self: Pointer; op: Integer; const other: Pointer): Integer; cdecl = nil;
  _blStringApplyOpFormat: function(self: Pointer; op: Integer; const fmt: PUTF8Char): Integer varargs; cdecl = nil;
  _blStringApplyOpFormatV: function(self: Pointer; op: Integer; const fmt: PUTF8Char; ap: Pointer): Integer; cdecl = nil;
  _blStringInsertChar: function(self: Pointer; index: Size_T; c: UTF8Char; n: Size_T): Integer; cdecl = nil;
  _blStringInsertData: function(self: Pointer; index: Size_T; const str: PUTF8Char; n: Size_T): Integer; cdecl = nil;
  _blStringInsertString: function(self: Pointer; index: Size_T; const other: Pointer): Integer; cdecl = nil;
  _blStringRemoveIndex: function(self: Pointer; index: Size_T): Integer; cdecl = nil;
  _blStringRemoveRange: function(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl = nil;
  _blStringEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blStringEqualsData: function(const self: Pointer; const str: PUTF8Char; n: Size_T): Boolean; cdecl = nil;
  _blStringCompare: function(const a: Pointer; const b: Pointer): Integer; cdecl = nil;
  _blStringCompareData: function(const self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl = nil;

  // var.h
  _blVarInitType: function(self: Pointer; &type: Integer): Integer; cdecl = nil;
  _blVarInitNull: function(self: Pointer): Integer; cdecl = nil;
  _blVarInitBool: function(self: Pointer; value: Boolean): Integer; cdecl = nil;
  _blVarInitInt32: function(self: Pointer; value: Int32): Integer; cdecl = nil;
  _blVarInitInt64: function(self: Pointer; value: Int64): Integer; cdecl = nil;
  _blVarInitUInt32: function(self: Pointer; value: UInt32): Integer; cdecl = nil;
  _blVarInitUInt64: function(self: Pointer; value: UInt64): Integer; cdecl = nil;
  _blVarInitDouble: function(self: Pointer; value: Double): Integer; cdecl = nil;
  _blVarInitRgba: function(self: Pointer; const rgba: Pointer): Integer; cdecl = nil;
  _blVarInitRgba32: function(self: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blVarInitRgba64: function(self: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blVarInitMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blVarInitWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blVarDestroy: function(self: Pointer): Integer; cdecl = nil;
  _blVarReset: function(self: Pointer): Integer; cdecl = nil;
  _blVarAssignNull: function(self: Pointer): Integer; cdecl = nil;
  _blVarAssignBool: function(self: Pointer; value: Boolean): Integer; cdecl = nil;
  _blVarAssignInt32: function(self: Pointer; value: Int32): Integer; cdecl = nil;
  _blVarAssignInt64: function(self: Pointer; value: Int64): Integer; cdecl = nil;
  _blVarAssignUInt32: function(self: Pointer; value: UInt32): Integer; cdecl = nil;
  _blVarAssignUInt64: function(self: Pointer; value: UInt64): Integer; cdecl = nil;
  _blVarAssignDouble: function(self: Pointer; value: Double): Integer; cdecl = nil;
  _blVarAssignRgba: function(self: Pointer; const rgba: Pointer): Integer; cdecl = nil;
  _blVarAssignRgba32: function(self: Pointer; rgba32: UInt32): Integer; cdecl = nil;
  _blVarAssignRgba64: function(self: Pointer; rgba64: UInt64): Integer; cdecl = nil;
  _blVarAssignMove: function(self: Pointer; other: Pointer): Integer; cdecl = nil;
  _blVarAssignWeak: function(self: Pointer; const other: Pointer): Integer; cdecl = nil;
  _blVarGetType: function(const self: Pointer): Integer; cdecl = nil;
  _blVarToBool: function(const self: Pointer; &out: PBoolean): Integer; cdecl = nil;
  _blVarToInt32: function(const self: Pointer; &out: PInt32): Integer; cdecl = nil;
  _blVarToInt64: function(const self: Pointer; &out: PInt64): Integer; cdecl = nil;
  _blVarToUInt32: function(const self: Pointer; &out: PUInt32): Integer; cdecl = nil;
  _blVarToUInt64: function(const self: Pointer; &out: PUInt64): Integer; cdecl = nil;
  _blVarToDouble: function(const self: Pointer; &out: PDouble): Integer; cdecl = nil;
  _blVarToRgba: function(const self: Pointer; &out: Pointer): Integer; cdecl = nil;
  _blVarToRgba32: function(const self: Pointer; &out: PUInt32): Integer; cdecl = nil;
  _blVarToRgba64: function(const self: Pointer; &out: PUInt64): Integer; cdecl = nil;
  _blVarEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
  _blVarEqualsNull: function(const self: Pointer): Boolean; cdecl = nil;
  _blVarEqualsBool: function(const self: Pointer; value: Boolean): Boolean; cdecl = nil;
  _blVarEqualsInt64: function(const self: Pointer; value: Int64): Boolean; cdecl = nil;
  _blVarEqualsUInt64: function(const self: Pointer; value: UInt64): Boolean; cdecl = nil;
  _blVarEqualsDouble: function(const self: Pointer; value: Double): Boolean; cdecl = nil;
  _blVarEqualsRgba: function(const self: Pointer; const rgba: Pointer): Boolean; cdecl = nil;
  _blVarEqualsRgba32: function(const self: Pointer; rgba32: UInt32): Boolean; cdecl = nil;
  _blVarEqualsRgba64: function(const self: Pointer; rgba64: UInt64): Boolean; cdecl = nil;
  _blVarStrictEquals: function(const a: Pointer; const b: Pointer): Boolean; cdecl = nil;
{$ELSE}

const
  {$IF Defined(IOS64)}
  _LIB_BLEND2D = 'libblend2d_ios.a';
  {$ELSEIF Defined(MACOS64)}
  _LIB_BLEND2D = 'libblend2d_macos.a';
  {$ELSEIF Defined(ANDROID32)}
  _LIB_BLEND2D = 'libblend2d_android32.a';
  {$ELSEIF Defined(ANDROID64)}
  _LIB_BLEND2D = 'libblend2d_android64.a';
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

// array.h'

function _blArrayInit(self: Pointer; arrayType: Integer): Integer; cdecl;
  external _LIB_BLEND2D name _PU + 'blArrayInit';

function _blArrayInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInitMove';

function _blArrayInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInitWeak';

function _blArrayDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayDestroy';

function _blArrayReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReset';

function _blArrayGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blArrayGetSize';

function _blArrayGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blArrayGetCapacity';

function _blArrayGetItemSize(self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blArrayGetItemSize';

function _blArrayGetData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blArrayGetData';

function _blArrayClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayClear';

function _blArrayShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayShrink';

function _blArrayReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReserve';

function _blArrayResize(self: Pointer; n: Size_T; const fill: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayResize';

function _blArrayMakeMutable(self: Pointer; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayMakeMutable';

function _blArrayModifyOp(self: Pointer; op: Integer; n: Size_T; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayModifyOp';

function _blArrayInsertOp(self: Pointer; index: Size_T; n: Size_T; dataOut: PPointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertOp';

function _blArrayAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAssignMove';

function _blArrayAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAssignWeak';

function _blArrayAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAssignDeep';

function _blArrayAssignData(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAssignData';

function _blArrayAssignExternalData(self: Pointer; data: Pointer; size: Size_T; capacity: Size_T; dataAccessFlags: Integer; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAssignExternalData';

function _blArrayAppendU8(self: Pointer; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendU8';

function _blArrayAppendU16(self: Pointer; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendU16';

function _blArrayAppendU32(self: Pointer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendU32';

function _blArrayAppendU64(self: Pointer; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendU64';

function _blArrayAppendF32(self: Pointer; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendF32';

function _blArrayAppendF64(self: Pointer; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendF64';

function _blArrayAppendItem(self: Pointer; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendItem';

function _blArrayAppendData(self: Pointer; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayAppendData';

function _blArrayInsertU8(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertU8';

function _blArrayInsertU16(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertU16';

function _blArrayInsertU32(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertU32';

function _blArrayInsertU64(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertU64';

function _blArrayInsertF32(self: Pointer; index: Size_T; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertF32';

function _blArrayInsertF64(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertF64';

function _blArrayInsertItem(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertItem';

function _blArrayInsertData(self: Pointer; index: Size_T; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayInsertData';

function _blArrayReplaceU8(self: Pointer; index: Size_T; value: UInt8): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceU8';

function _blArrayReplaceU16(self: Pointer; index: Size_T; value: UInt16): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceU16';

function _blArrayReplaceU32(self: Pointer; index: Size_T; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceU32';

function _blArrayReplaceU64(self: Pointer; index: Size_T; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceU64';

function _blArrayReplaceF32(self: Pointer; index: Size_T; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceF32';

function _blArrayReplaceF64(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceF64';

function _blArrayReplaceItem(self: Pointer; index: Size_T; const item: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceItem';

function _blArrayReplaceData(self: Pointer; rStart: Size_T; rEnd: Size_T; const data: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayReplaceData';

function _blArrayRemoveIndex(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayRemoveIndex';

function _blArrayRemoveRange(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blArrayRemoveRange';

function _blArrayEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blArrayEquals';

// bitarray.h

function _blBitArrayInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayInit';

function _blBitArrayInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayInitMove';

function _blBitArrayInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayInitWeak';

function _blBitArrayDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayDestroy';

function _blBitArrayReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReset';

function _blBitArrayAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAssignMove';

function _blBitArrayAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAssignWeak';

function _blBitArrayAssignWords(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAssignWords';

function _blBitArrayIsEmpty(const self: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayIsEmpty';

function _blBitArrayGetSize(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetSize';

function _blBitArrayGetWordCount(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetWordCount';

function _blBitArrayGetCapacity(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetCapacity';

function _blBitArrayGetData(const self: Pointer): PUInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetData';

function _blBitArrayGetCardinality(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetCardinality';

function _blBitArrayGetCardinalityInRange(const self: Pointer; startBit: UInt32; endBit: UInt32): UInt32; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetCardinalityInRange';

function _blBitArrayHasBit(const self: Pointer; bitIndex: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayHasBit';

function _blBitArrayHasBitsInRange(const self: Pointer; startBit: UInt32; endBit: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayHasBitsInRange';

function _blBitArraySubsumes(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArraySubsumes';

function _blBitArrayIntersects(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayIntersects';

function _blBitArrayGetRange(const self: Pointer; startOut: PUInt32; endOut: PUInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayGetRange';

function _blBitArrayEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blBitArrayEquals';

function _blBitArrayCompare(const a: Pointer; const b: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayCompare';

function _blBitArrayClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayClear';

function _blBitArrayResize(self: Pointer; nBits: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayResize';

function _blBitArrayReserve(self: Pointer; nBits: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReserve';

function _blBitArrayShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayShrink';

function _blBitArraySetBit(self: Pointer; bitIndex: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArraySetBit';

function _blBitArrayFillRange(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayFillRange';

function _blBitArrayFillWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayFillWords';

function _blBitArrayClearBit(self: Pointer; bitIndex: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayClearBit';

function _blBitArrayClearRange(self: Pointer; startBit: UInt32; endBit: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayClearRange';

function _blBitArrayClearWord(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayClearWord';

function _blBitArrayClearWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayClearWords';

function _blBitArrayReplaceOp(self: Pointer; nBits: UInt32; dataOut: PPUInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReplaceOp';

function _blBitArrayReplaceBit(self: Pointer; bitIndex: UInt32; bitValue: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReplaceBit';

function _blBitArrayReplaceWord(self: Pointer; bitIndex: UInt32; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReplaceWord';

function _blBitArrayReplaceWords(self: Pointer; bitIndex: UInt32; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayReplaceWords';

function _blBitArrayAppendBit(self: Pointer; bitValue: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAppendBit';

function _blBitArrayAppendWord(self: Pointer; wordValue: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAppendWord';

function _blBitArrayAppendWords(self: Pointer; const wordData: PUInt32; wordCount: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blBitArrayAppendWords';

// context.h

function _blContextInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextInit';

function _blContextInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextInitMove';

function _blContextInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextInitWeak';

function _blContextInitAs(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextInitAs';

function _blContextDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextDestroy';

function _blContextReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextReset';

function _blContextAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextAssignMove';

function _blContextAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextAssignWeak';

function _blContextGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetType';

function _blContextGetTargetSize(const self: Pointer; targetSizeOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetTargetSize';

function _blContextGetTargetImage(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blContextGetTargetImage';

function _blContextBegin(self: Pointer; image: Pointer; const cci: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextBegin';

function _blContextEnd(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextEnd';

function _blContextFlush(self: Pointer; flags: Cardinal): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFlush';

function _blContextSave(self: Pointer; cookie: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSave';

function _blContextRestore(self: Pointer; const cookie: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextRestore';

function _blContextGetMetaTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetMetaTransform';

function _blContextGetUserTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetUserTransform';

function _blContextGetFinalTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetFinalTransform';

function _blContextUserToMeta(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextUserToMeta';

function _blContextApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextApplyTransformOp';

function _blContextGetHint(const self: Pointer; hintType: Integer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blContextGetHint';

function _blContextSetHint(self: Pointer; hintType: Integer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetHint';

function _blContextGetHints(const self: Pointer; hintsOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetHints';

function _blContextSetHints(self: Pointer; const hints: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetHints';

function _blContextSetFlattenMode(self: Pointer; mode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFlattenMode';

function _blContextSetFlattenTolerance(self: Pointer; tolerance: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFlattenTolerance';

function _blContextSetApproximationOptions(self: Pointer; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetApproximationOptions';

function _blContextGetFillStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetFillStyle';

function _blContextGetTransformedFillStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetTransformedFillStyle';

function _blContextSetFillStyle(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillStyle';

function _blContextSetFillStyleWithMode(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillStyleWithMode';

function _blContextSetFillStyleRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillStyleRgba';

function _blContextSetFillStyleRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillStyleRgba32';

function _blContextSetFillStyleRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillStyleRgba64';

function _blContextDisableFillStyle(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextDisableFillStyle';

function _blContextGetFillAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetFillAlpha';

function _blContextSetFillAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillAlpha';

function _blContextGetStrokeStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeStyle';

function _blContextGetTransformedStrokeStyle(const self: Pointer; styleOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetTransformedStrokeStyle';

function _blContextSetStrokeStyle(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeStyle';

function _blContextSetStrokeStyleWithMode(self: Pointer; const style: Pointer; transformMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeStyleWithMode';

function _blContextSetStrokeStyleRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeStyleRgba';

function _blContextSetStrokeStyleRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeStyleRgba32';

function _blContextSetStrokeStyleRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeStyleRgba64';

function _blContextDisableStrokeStyle(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextDisableStrokeStyle';

function _blContextGetStrokeAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeAlpha';

function _blContextSetStrokeAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeAlpha';

function _blContextSwapStyles(self: Pointer; mode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSwapStyles';

function _blContextGetGlobalAlpha(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetGlobalAlpha';

function _blContextSetGlobalAlpha(self: Pointer; alpha: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetGlobalAlpha';

function _blContextGetCompOp(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetCompOp';

function _blContextSetCompOp(self: Pointer; compOp: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetCompOp';

function _blContextGetFillRule(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetFillRule';

function _blContextSetFillRule(self: Pointer; fillRule: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetFillRule';

function _blContextGetStrokeWidth(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeWidth';

function _blContextSetStrokeWidth(self: Pointer; width: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeWidth';

function _blContextGetStrokeMiterLimit(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeMiterLimit';

function _blContextSetStrokeMiterLimit(self: Pointer; miterLimit: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeMiterLimit';

function _blContextGetStrokeCap(const self: Pointer; position: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeCap';

function _blContextSetStrokeCap(self: Pointer; position: Integer; strokeCap: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeCap';

function _blContextSetStrokeCaps(self: Pointer; strokeCap: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeCaps';

function _blContextGetStrokeJoin(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeJoin';

function _blContextSetStrokeJoin(self: Pointer; strokeJoin: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeJoin';

function _blContextGetStrokeTransformOrder(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeTransformOrder';

function _blContextSetStrokeTransformOrder(self: Pointer; transformOrder: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeTransformOrder';

function _blContextGetStrokeDashOffset(const self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeDashOffset';

function _blContextSetStrokeDashOffset(self: Pointer; dashOffset: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeDashOffset';

function _blContextGetStrokeDashArray(const self: Pointer; dashArrayOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeDashArray';

function _blContextSetStrokeDashArray(self: Pointer; const dashArray: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeDashArray';

function _blContextGetStrokeOptions(const self: Pointer; options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextGetStrokeOptions';

function _blContextSetStrokeOptions(self: Pointer; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextSetStrokeOptions';

function _blContextClipToRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextClipToRectI';

function _blContextClipToRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextClipToRectD';

function _blContextRestoreClipping(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextRestoreClipping';

function _blContextClearAll(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextClearAll';

function _blContextClearRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextClearRectI';

function _blContextClearRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextClearRectD';

function _blContextFillAll(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillAll';

function _blContextFillAllRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillAllRgba32';

function _blContextFillAllRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillAllRgba64';

function _blContextFillAllExt(self: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillAllExt';

function _blContextFillRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectI';

function _blContextFillRectIRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectIRgba32';

function _blContextFillRectIRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectIRgba64';

function _blContextFillRectIExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectIExt';

function _blContextFillRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectD';

function _blContextFillRectDRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectDRgba32';

function _blContextFillRectDRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectDRgba64';

function _blContextFillRectDExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillRectDExt';

function _blContextFillPathD(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillPathD';

function _blContextFillPathDRgba32(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillPathDRgba32';

function _blContextFillPathDRgba64(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillPathDRgba64';

function _blContextFillPathDExt(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillPathDExt';

function _blContextFillGeometry(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGeometry';

function _blContextFillGeometryRgba32(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGeometryRgba32';

function _blContextFillGeometryRgba64(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGeometryRgba64';

function _blContextFillGeometryExt(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGeometryExt';

function _blContextFillUtf8TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextI';

function _blContextFillUtf8TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextIRgba32';

function _blContextFillUtf8TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextIRgba64';

function _blContextFillUtf8TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextIExt';

function _blContextFillUtf8TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextD';

function _blContextFillUtf8TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextDRgba32';

function _blContextFillUtf8TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextDRgba64';

function _blContextFillUtf8TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf8TextDExt';

function _blContextFillUtf16TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextI';

function _blContextFillUtf16TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextIRgba32';

function _blContextFillUtf16TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextIRgba64';

function _blContextFillUtf16TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextIExt';

function _blContextFillUtf16TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextD';

function _blContextFillUtf16TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextDRgba32';

function _blContextFillUtf16TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextDRgba64';

function _blContextFillUtf16TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf16TextDExt';

function _blContextFillUtf32TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextI';

function _blContextFillUtf32TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextIRgba32';

function _blContextFillUtf32TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextIRgba64';

function _blContextFillUtf32TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextIExt';

function _blContextFillUtf32TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextD';

function _blContextFillUtf32TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextDRgba32';

function _blContextFillUtf32TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextDRgba64';

function _blContextFillUtf32TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillUtf32TextDExt';

function _blContextFillGlyphRunI(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunI';

function _blContextFillGlyphRunIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunIRgba32';

function _blContextFillGlyphRunIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunIRgba64';

function _blContextFillGlyphRunIExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunIExt';

function _blContextFillGlyphRunD(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunD';

function _blContextFillGlyphRunDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunDRgba32';

function _blContextFillGlyphRunDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunDRgba64';

function _blContextFillGlyphRunDExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillGlyphRunDExt';

function _blContextFillMaskI(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskI';

function _blContextFillMaskIRgba32(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskIRgba32';

function _blContextFillMaskIRgba64(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskIRgba64';

function _blContextFillMaskIExt(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskIExt';

function _blContextFillMaskD(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskD';

function _blContextFillMaskDRgba32(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskDRgba32';

function _blContextFillMaskDRgba64(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskDRgba64';

function _blContextFillMaskDExt(self: Pointer; const origin: Pointer; const mask: Pointer; const maskArea: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextFillMaskDExt';

function _blContextStrokeRectI(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectI';

function _blContextStrokeRectIRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectIRgba32';

function _blContextStrokeRectIRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectIRgba64';

function _blContextStrokeRectIExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectIExt';

function _blContextStrokeRectD(self: Pointer; const rect: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectD';

function _blContextStrokeRectDRgba32(self: Pointer; const rect: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectDRgba32';

function _blContextStrokeRectDRgba64(self: Pointer; const rect: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectDRgba64';

function _blContextStrokeRectDExt(self: Pointer; const rect: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeRectDExt';

function _blContextStrokePathD(self: Pointer; const origin: Pointer; const path: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokePathD';

function _blContextStrokePathDRgba32(self: Pointer; const origin: Pointer; const path: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokePathDRgba32';

function _blContextStrokePathDRgba64(self: Pointer; const origin: Pointer; const path: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokePathDRgba64';

function _blContextStrokePathDExt(self: Pointer; const origin: Pointer; const path: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokePathDExt';

function _blContextStrokeGeometry(self: Pointer; &type: Integer; const data: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGeometry';

function _blContextStrokeGeometryRgba32(self: Pointer; &type: Integer; const data: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGeometryRgba32';

function _blContextStrokeGeometryRgba64(self: Pointer; &type: Integer; const data: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGeometryRgba64';

function _blContextStrokeGeometryExt(self: Pointer; &type: Integer; const data: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGeometryExt';

function _blContextStrokeUtf8TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextI';

function _blContextStrokeUtf8TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextIRgba32';

function _blContextStrokeUtf8TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextIRgba64';

function _blContextStrokeUtf8TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextIExt';

function _blContextStrokeUtf8TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextD';

function _blContextStrokeUtf8TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextDRgba32';

function _blContextStrokeUtf8TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextDRgba64';

function _blContextStrokeUtf8TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUTF8Char; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf8TextDExt';

function _blContextStrokeUtf16TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextI';

function _blContextStrokeUtf16TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextIRgba32';

function _blContextStrokeUtf16TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextIRgba64';

function _blContextStrokeUtf16TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextIExt';

function _blContextStrokeUtf16TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextD';

function _blContextStrokeUtf16TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextDRgba32';

function _blContextStrokeUtf16TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextDRgba64';

function _blContextStrokeUtf16TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PWideChar; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf16TextDExt';

function _blContextStrokeUtf32TextI(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextI';

function _blContextStrokeUtf32TextIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextIRgba32';

function _blContextStrokeUtf32TextIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextIRgba64';

function _blContextStrokeUtf32TextIExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextIExt';

function _blContextStrokeUtf32TextD(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextD';

function _blContextStrokeUtf32TextDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextDRgba32';

function _blContextStrokeUtf32TextDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextDRgba64';

function _blContextStrokeUtf32TextDExt(self: Pointer; const origin: Pointer; const font: Pointer; const text: PUInt32; size: Size_T; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeUtf32TextDExt';

function _blContextStrokeGlyphRunI(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunI';

function _blContextStrokeGlyphRunIRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunIRgba32';

function _blContextStrokeGlyphRunIRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunIRgba64';

function _blContextStrokeGlyphRunIExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunIExt';

function _blContextStrokeGlyphRunD(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunD';

function _blContextStrokeGlyphRunDRgba32(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunDRgba32';

function _blContextStrokeGlyphRunDRgba64(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunDRgba64';

function _blContextStrokeGlyphRunDExt(self: Pointer; const origin: Pointer; const font: Pointer; const glyphRun: Pointer; const style: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextStrokeGlyphRunDExt';

function _blContextBlitImageI(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextBlitImageI';

function _blContextBlitImageD(self: Pointer; const origin: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextBlitImageD';

function _blContextBlitScaledImageI(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextBlitScaledImageI';

function _blContextBlitScaledImageD(self: Pointer; const rect: Pointer; const img: Pointer; const imgArea: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blContextBlitScaledImageD';

// filesystem.h

function _blFileInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileInit';

function _blFileReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileReset';

function _blFileOpen(self: Pointer; const fileName: PUTF8Char; openFlags: Cardinal): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileOpen';

function _blFileClose(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileClose';

function _blFileSeek(self: Pointer; offset: Int64; seekType: Integer; positionOut: PInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileSeek';

function _blFileRead(self: Pointer; buffer: Pointer; n: Size_T; bytesReadOut: PSize_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileRead';

function _blFileWrite(self: Pointer; const buffer: Pointer; n: Size_T; bytesWrittenOut: PSize_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileWrite';

function _blFileTruncate(self: Pointer; maxSize: Int64): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileTruncate';

function _blFileGetInfo(self: Pointer; infoOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileGetInfo';

function _blFileGetSize(self: Pointer; fileSizeOut: PUInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileGetSize';

function _blFileSystemGetInfo(const fileName: PUTF8Char; infoOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileSystemGetInfo';

function _blFileSystemReadFile(const fileName: PUTF8Char; dst: Pointer; maxSize: Size_T; readFlags: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileSystemReadFile';

function _blFileSystemWriteFile(const fileName: PUTF8Char; const data: Pointer; size: Size_T; bytesWrittenOut: PSize_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blFileSystemWriteFile';

// font.h

function _blFontInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontInit';

function _blFontInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontInitMove';

function _blFontInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontInitWeak';

function _blFontDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDestroy';

function _blFontReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontReset';

function _blFontAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontAssignMove';

function _blFontAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontAssignWeak';

function _blFontEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontEquals';

function _blFontCreateFromFace(self: Pointer; const face: Pointer; size: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontCreateFromFace';

function _blFontCreateFromFaceWithSettings(self: Pointer; const face: Pointer; size: Single; const featureSettings: Pointer; const variationSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontCreateFromFaceWithSettings';

function _blFontGetFace(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetFace';

function _blFontGetSize(const self: Pointer): Single; cdecl;
  external _LIB_BLEND2D name 'blFontGetSize';

function _blFontSetSize(self: Pointer; size: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontSetSize';

function _blFontGetMetrics(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetMetrics';

function _blFontGetMatrix(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetMatrix';

function _blFontGetDesignMetrics(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetDesignMetrics';

function _blFontGetFeatureSettings(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetFeatureSettings';

function _blFontSetFeatureSettings(self: Pointer; const featureSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontSetFeatureSettings';

function _blFontResetFeatureSettings(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontResetFeatureSettings';

function _blFontGetVariationSettings(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetVariationSettings';

function _blFontSetVariationSettings(self: Pointer; const variationSettings: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontSetVariationSettings';

function _blFontResetVariationSettings(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontResetVariationSettings';

function _blFontShape(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontShape';

function _blFontMapTextToGlyphs(const self: Pointer; gb: Pointer; stateOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontMapTextToGlyphs';

function _blFontPositionGlyphs(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontPositionGlyphs';

function _blFontApplyKerning(const self: Pointer; gb: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontApplyKerning';

function _blFontApplyGSub(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontApplyGSub';

function _blFontApplyGPos(const self: Pointer; gb: Pointer; const lookups: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontApplyGPos';

function _blFontGetTextMetrics(const self: Pointer; gb: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetTextMetrics';

function _blFontGetGlyphBounds(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetGlyphBounds';

function _blFontGetGlyphAdvances(const self: Pointer; const glyphData: PUInt32; glyphAdvance: IntPtr; &out: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetGlyphAdvances';

function _blFontGetGlyphOutlines(const self: Pointer; glyphId: TBLGlyphId; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetGlyphOutlines';

function _blFontGetGlyphRunOutlines(const self: Pointer; const glyphRun: Pointer; const userTransform: Pointer; &out: Pointer; sink: TBLPathSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontGetGlyphRunOutlines';

// fontdata.h

function _blFontDataInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataInit';

function _blFontDataInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataInitMove';

function _blFontDataInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataInitWeak';

function _blFontDataDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataDestroy';

function _blFontDataReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataReset';

function _blFontDataAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataAssignMove';

function _blFontDataAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataAssignWeak';

function _blFontDataCreateFromFile(self: Pointer; const fileName: PUTF8Char; readFlags: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataCreateFromFile';

function _blFontDataCreateFromDataArray(self: Pointer; const dataArray: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataCreateFromDataArray';

function _blFontDataCreateFromData(self: Pointer; const data: Pointer; dataSize: Size_T; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataCreateFromData';

function _blFontDataEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontDataEquals';

function _blFontDataGetFaceCount(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blFontDataGetFaceCount';

function _blFontDataGetFaceType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataGetFaceType';

function _blFontDataGetFlags(const self: Pointer): Cardinal; cdecl;
  external _LIB_BLEND2D name 'blFontDataGetFlags';

function _blFontDataGetTableTags(const self: Pointer; faceIndex: UInt32; dst: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontDataGetTableTags';

function _blFontDataGetTables(const self: Pointer; faceIndex: UInt32; dst: Pointer; const tags: Pointer; count: Size_T): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontDataGetTables';

// fontface.h

function _blFontFaceInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceInit';

function _blFontFaceInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceInitMove';

function _blFontFaceInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceInitWeak';

function _blFontFaceDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceDestroy';

function _blFontFaceReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceReset';

function _blFontFaceAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceAssignMove';

function _blFontFaceAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceAssignWeak';

function _blFontFaceEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFaceEquals';

function _blFontFaceCreateFromFile(self: Pointer; const fileName: PUTF8Char; readFlags: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceCreateFromFile';

function _blFontFaceCreateFromData(self: Pointer; const fontData: Pointer; faceIndex: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceCreateFromData';

function _blFontFaceGetFullName(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetFullName';

function _blFontFaceGetFamilyName(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetFamilyName';

function _blFontFaceGetSubfamilyName(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetSubfamilyName';

function _blFontFaceGetPostScriptName(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetPostScriptName';

function _blFontFaceGetFaceInfo(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetFaceInfo';

function _blFontFaceGetDesignMetrics(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetDesignMetrics';

function _blFontFaceGetUnicodeCoverage(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetUnicodeCoverage';

function _blFontFaceGetCharacterCoverage(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetCharacterCoverage';

function _blFontFaceHasScriptTag(const self: Pointer; scriptTag: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFaceHasScriptTag';

function _blFontFaceHasFeatureTag(const self: Pointer; featureTag: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFaceHasFeatureTag';

function _blFontFaceHasVariationTag(const self: Pointer; variationTag: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFaceHasVariationTag';

function _blFontFaceGetScriptTags(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetScriptTags';

function _blFontFaceGetFeatureTags(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetFeatureTags';

function _blFontFaceGetVariationTags(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFaceGetVariationTags';

// fontfeaturesettings.h
function _blFontFeatureSettingsInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsInit';

function _blFontFeatureSettingsInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsInitMove';

function _blFontFeatureSettingsInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsInitWeak';

function _blFontFeatureSettingsDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsDestroy';

function _blFontFeatureSettingsReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsReset';

function _blFontFeatureSettingsClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsClear';

function _blFontFeatureSettingsShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsShrink';

function _blFontFeatureSettingsAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsAssignMove';

function _blFontFeatureSettingsAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsAssignWeak';

function _blFontFeatureSettingsGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsGetSize';

function _blFontFeatureSettingsGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsGetCapacity';

function _blFontFeatureSettingsGetView(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsGetView';

function _blFontFeatureSettingsHasValue(const self: Pointer; featureTag: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsHasValue';

function _blFontFeatureSettingsGetValue(const self: Pointer; featureTag: UInt32): UInt32; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsGetValue';

function _blFontFeatureSettingsSetValue(self: Pointer; featureTag: UInt32; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsSetValue';

function _blFontFeatureSettingsRemoveValue(self: Pointer; featureTag: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsRemoveValue';

function _blFontFeatureSettingsEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontFeatureSettingsEquals';

// fontmanager.h

function _blFontManagerInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerInit';

function _blFontManagerInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerInitMove';

function _blFontManagerInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerInitWeak';

function _blFontManagerInitNew(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerInitNew';

function _blFontManagerDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerDestroy';

function _blFontManagerReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerReset';

function _blFontManagerAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerAssignMove';

function _blFontManagerAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerAssignWeak';

function _blFontManagerCreate(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerCreate';

function _blFontManagerGetFaceCount(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontManagerGetFaceCount';

function _blFontManagerGetFamilyCount(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontManagerGetFamilyCount';

function _blFontManagerHasFace(const self: Pointer; const face: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontManagerHasFace';

function _blFontManagerAddFace(self: Pointer; const face: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerAddFace';

function _blFontManagerQueryFace(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; const properties: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerQueryFace';

function _blFontManagerQueryFacesByFamilyName(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontManagerQueryFacesByFamilyName';

function _blFontManagerEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontManagerEquals';

// fontvariationsettings.h

function _blFontVariationSettingsInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsInit';

function _blFontVariationSettingsInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsInitMove';

function _blFontVariationSettingsInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsInitWeak';

function _blFontVariationSettingsDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsDestroy';

function _blFontVariationSettingsReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsReset';

function _blFontVariationSettingsClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsClear';

function _blFontVariationSettingsShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsShrink';

function _blFontVariationSettingsAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsAssignMove';

function _blFontVariationSettingsAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsAssignWeak';

function _blFontVariationSettingsGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsGetSize';

function _blFontVariationSettingsGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsGetCapacity';

function _blFontVariationSettingsGetView(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsGetView';

function _blFontVariationSettingsHasValue(const self: Pointer; variationTag: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsHasValue';

function _blFontVariationSettingsGetValue(const self: Pointer; variationTag: UInt32): Single; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsGetValue';

function _blFontVariationSettingsSetValue(self: Pointer; variationTag: UInt32; value: Single): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsSetValue';

function _blFontVariationSettingsRemoveValue(self: Pointer; variationTag: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsRemoveValue';

function _blFontVariationSettingsEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blFontVariationSettingsEquals';

// format.h

function _blFormatInfoQuery(self: Pointer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFormatInfoQuery';

function _blFormatInfoSanitize(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blFormatInfoSanitize';

// glyphbuffer.h

function _blGlyphBufferInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferInit';

function _blGlyphBufferInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferInitMove';

function _blGlyphBufferDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferDestroy';

function _blGlyphBufferReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferReset';

function _blGlyphBufferClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferClear';

function _blGlyphBufferGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetSize';

function _blGlyphBufferGetFlags(const self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetFlags';

function _blGlyphBufferGetGlyphRun(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetGlyphRun';

function _blGlyphBufferGetContent(const self: Pointer): PUInt32; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetContent';

function _blGlyphBufferGetInfoData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetInfoData';

function _blGlyphBufferGetPlacementData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferGetPlacementData';

function _blGlyphBufferSetText(self: Pointer; const textData: Pointer; size: Size_T; encoding: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferSetText';

function _blGlyphBufferSetGlyphs(self: Pointer; const glyphData: PUInt32; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferSetGlyphs';

function _blGlyphBufferSetGlyphsFromStruct(self: Pointer; const glyphData: Pointer; size: Size_T; glyphIdSize: Size_T; glyphIdAdvance: IntPtr): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferSetGlyphsFromStruct';

function _blGlyphBufferSetDebugSink(self: Pointer; sink: TBLDebugMessageSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferSetDebugSink';

function _blGlyphBufferResetDebugSink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGlyphBufferResetDebugSink';

// gradient.h

function _blGradientInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientInit';

function _blGradientInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientInitMove';

function _blGradientInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientInitWeak';

function _blGradientInitAs(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientInitAs';

function _blGradientDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientDestroy';

function _blGradientReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientReset';

function _blGradientAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientAssignMove';

function _blGradientAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientAssignWeak';

function _blGradientCreate(self: Pointer; &type: Integer; const values: Pointer; extendMode: Integer; const stops: Pointer; n: Size_T; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientCreate';

function _blGradientShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientShrink';

function _blGradientReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientReserve';

function _blGradientGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientGetType';

function _blGradientSetType(self: Pointer; &type: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientSetType';

function _blGradientGetExtendMode(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientGetExtendMode';

function _blGradientSetExtendMode(self: Pointer; extendMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientSetExtendMode';

function _blGradientGetValue(const self: Pointer; index: Size_T): Double; cdecl;
  external _LIB_BLEND2D name 'blGradientGetValue';

function _blGradientSetValue(self: Pointer; index: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientSetValue';

function _blGradientSetValues(self: Pointer; index: Size_T; const values: PDouble; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientSetValues';

function _blGradientGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blGradientGetSize';

function _blGradientGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blGradientGetCapacity';

function _blGradientGetStops(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blGradientGetStops';

function _blGradientResetStops(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientResetStops';

function _blGradientAssignStops(self: Pointer; const stops: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientAssignStops';

function _blGradientAddStopRgba32(self: Pointer; offset: Double; argb32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientAddStopRgba32';

function _blGradientAddStopRgba64(self: Pointer; offset: Double; argb64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientAddStopRgba64';

function _blGradientRemoveStop(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientRemoveStop';

function _blGradientRemoveStopByOffset(self: Pointer; offset: Double; all: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientRemoveStopByOffset';

function _blGradientRemoveStopsByIndex(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientRemoveStopsByIndex';

function _blGradientRemoveStopsByOffset(self: Pointer; offsetMin: Double; offsetMax: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientRemoveStopsByOffset';

function _blGradientReplaceStopRgba32(self: Pointer; index: Size_T; offset: Double; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientReplaceStopRgba32';

function _blGradientReplaceStopRgba64(self: Pointer; index: Size_T; offset: Double; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientReplaceStopRgba64';

function _blGradientIndexOfStop(const self: Pointer; offset: Double): Size_T; cdecl;
  external _LIB_BLEND2D name 'blGradientIndexOfStop';

function _blGradientGetTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientGetTransform';

function _blGradientGetTransformType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientGetTransformType';

function _blGradientApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blGradientApplyTransformOp';

function _blGradientEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blGradientEquals';

// image.h

function _blImageInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageInit';

function _blImageInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageInitMove';

function _blImageInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageInitWeak';

function _blImageInitAs(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageInitAs';

function _blImageInitAsFromData(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageInitAsFromData';

function _blImageDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDestroy';

function _blImageReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageReset';

function _blImageAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageAssignMove';

function _blImageAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageAssignWeak';

function _blImageAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageAssignDeep';

function _blImageCreate(self: Pointer; w: Integer; h: Integer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCreate';

function _blImageCreateFromData(self: Pointer; w: Integer; h: Integer; format: Integer; pixelData: Pointer; stride: IntPtr; accessFlags: Cardinal; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCreateFromData';

function _blImageGetData(const self: Pointer; dataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageGetData';

function _blImageMakeMutable(self: Pointer; dataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageMakeMutable';

function _blImageConvert(self: Pointer; format: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageConvert';

function _blImageEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blImageEquals';

function _blImageScale(dst: Pointer; const src: Pointer; const size: Pointer; filter: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageScale';

function _blImageReadFromFile(self: Pointer; const fileName: PUTF8Char; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageReadFromFile';

function _blImageReadFromData(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageReadFromData';

function _blImageWriteToFile(const self: Pointer; const fileName: PUTF8Char; const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageWriteToFile';

function _blImageWriteToData(const self: Pointer; dst: Pointer; const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageWriteToData';

// imagecodec.h

function _blImageCodecInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecInit';

function _blImageCodecInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecInitMove';

function _blImageCodecInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecInitWeak';

function _blImageCodecInitByName(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecInitByName';

function _blImageCodecDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecDestroy';

function _blImageCodecReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecReset';

function _blImageCodecAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecAssignMove';

function _blImageCodecAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecAssignWeak';

function _blImageCodecFindByName(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecFindByName';

function _blImageCodecFindByExtension(self: Pointer; const name: PUTF8Char; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecFindByExtension';

function _blImageCodecFindByData(self: Pointer; const data: Pointer; size: Size_T; const codecs: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecFindByData';

function _blImageCodecInspectData(const self: Pointer; const data: Pointer; size: Size_T): UInt32; cdecl;
  external _LIB_BLEND2D name 'blImageCodecInspectData';

function _blImageCodecCreateDecoder(const self: Pointer; dst: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecCreateDecoder';

function _blImageCodecCreateEncoder(const self: Pointer; dst: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecCreateEncoder';

function _blImageCodecArrayInitBuiltInCodecs(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecArrayInitBuiltInCodecs';

function _blImageCodecArrayAssignBuiltInCodecs(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecArrayAssignBuiltInCodecs';

function _blImageCodecAddToBuiltIn(const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecAddToBuiltIn';

function _blImageCodecRemoveFromBuiltIn(const codec: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageCodecRemoveFromBuiltIn';

// imagedecoder.h

function _blImageDecoderInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderInit';

function _blImageDecoderInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderInitMove';

function _blImageDecoderInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderInitWeak';

function _blImageDecoderDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderDestroy';

function _blImageDecoderReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderReset';

function _blImageDecoderAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderAssignMove';

function _blImageDecoderAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderAssignWeak';

function _blImageDecoderRestart(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderRestart';

function _blImageDecoderReadInfo(self: Pointer; infoOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderReadInfo';

function _blImageDecoderReadFrame(self: Pointer; imageOut: Pointer; const data: Pointer; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageDecoderReadFrame';

// imageencoder.h

function _blImageEncoderInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderInit';

function _blImageEncoderInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderInitMove';

function _blImageEncoderInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderInitWeak';

function _blImageEncoderDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderDestroy';

function _blImageEncoderReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderReset';

function _blImageEncoderAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderAssignMove';

function _blImageEncoderAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderAssignWeak';

function _blImageEncoderRestart(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderRestart';

function _blImageEncoderWriteFrame(self: Pointer; dst: Pointer; const image: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blImageEncoderWriteFrame';

// matrix.h

function _blMatrix2DSetIdentity(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DSetIdentity';

function _blMatrix2DSetTranslation(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DSetTranslation';

function _blMatrix2DSetScaling(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DSetScaling';

function _blMatrix2DSetSkewing(self: Pointer; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DSetSkewing';

function _blMatrix2DSetRotation(self: Pointer; angle: Double; cx: Double; cy: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DSetRotation';

function _blMatrix2DApplyOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DApplyOp';

function _blMatrix2DInvert(dst: Pointer; const src: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DInvert';

function _blMatrix2DGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DGetType';

function _blMatrix2DMapPointDArray(const self: Pointer; dst: Pointer; const src: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blMatrix2DMapPointDArray';

// object.h

function _blObjectAllocImpl(self: Pointer; objectInfo: UInt32; implSize: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectAllocImpl';

function _blObjectAllocImplAligned(self: Pointer; objectInfo: UInt32; implSize: Size_T; implAlignment: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectAllocImplAligned';

function _blObjectAllocImplExternal(self: Pointer; objectInfo: UInt32; implSize: Size_T; immutable: Boolean; destroyFunc: _BLDestroyExternalDataFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectAllocImplExternal';

function _blObjectFreeImpl(impl: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectFreeImpl';

function _blObjectInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectInitMove';

function _blObjectInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectInitWeak';

function _blObjectReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectReset';

function _blObjectAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectAssignMove';

function _blObjectAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectAssignWeak';

function _blObjectGetProperty(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetProperty';

function _blObjectGetPropertyBool(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PBoolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyBool';

function _blObjectGetPropertyInt32(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyInt32';

function _blObjectGetPropertyInt64(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyInt64';

function _blObjectGetPropertyUInt32(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PUInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyUInt32';

function _blObjectGetPropertyUInt64(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PUInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyUInt64';

function _blObjectGetPropertyDouble(const self: Pointer; const name: PUTF8Char; nameSize: Size_T; valueOut: PDouble): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectGetPropertyDouble';

function _blObjectSetProperty(self: Pointer; const name: PUTF8Char; nameSize: Size_T; const value: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetProperty';

function _blObjectSetPropertyBool(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyBool';

function _blObjectSetPropertyInt32(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Int32): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyInt32';

function _blObjectSetPropertyInt64(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Int64): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyInt64';

function _blObjectSetPropertyUInt32(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyUInt32';

function _blObjectSetPropertyUInt64(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyUInt64';

function _blObjectSetPropertyDouble(self: Pointer; const name: PUTF8Char; nameSize: Size_T; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blObjectSetPropertyDouble';

// path.h

function _blPathInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathInit';

function _blPathInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathInitMove';

function _blPathInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathInitWeak';

function _blPathDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathDestroy';

function _blPathReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathReset';

function _blPathGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blPathGetSize';

function _blPathGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blPathGetCapacity';

function _blPathGetCommandData(const self: Pointer): PUInt8; cdecl;
  external _LIB_BLEND2D name 'blPathGetCommandData';

function _blPathGetVertexData(const self: Pointer): Pointer; cdecl;
  external _LIB_BLEND2D name 'blPathGetVertexData';

function _blPathClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathClear';

function _blPathShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathShrink';

function _blPathReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathReserve';

function _blPathModifyOp(self: Pointer; op: Integer; n: Size_T; cmdDataOut: PPUInt8; vtxDataOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathModifyOp';

function _blPathAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAssignMove';

function _blPathAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAssignWeak';

function _blPathAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAssignDeep';

function _blPathSetVertexAt(self: Pointer; index: Size_T; cmd: UInt32; x: Double; y: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathSetVertexAt';

function _blPathMoveTo(self: Pointer; x0: Double; y0: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathMoveTo';

function _blPathLineTo(self: Pointer; x1: Double; y1: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathLineTo';

function _blPathPolyTo(self: Pointer; const poly: Pointer; count: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathPolyTo';

function _blPathQuadTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathQuadTo';

function _blPathConicTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; w: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathConicTo';

function _blPathCubicTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathCubicTo';

function _blPathSmoothQuadTo(self: Pointer; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathSmoothQuadTo';

function _blPathSmoothCubicTo(self: Pointer; x2: Double; y2: Double; x3: Double; y3: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathSmoothCubicTo';

function _blPathArcTo(self: Pointer; x: Double; y: Double; rx: Double; ry: Double; start: Double; sweep: Double; forceMoveTo: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathArcTo';

function _blPathArcQuadrantTo(self: Pointer; x1: Double; y1: Double; x2: Double; y2: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathArcQuadrantTo';

function _blPathEllipticArcTo(self: Pointer; rx: Double; ry: Double; xAxisRotation: Double; largeArcFlag: Boolean; sweepFlag: Boolean; x1: Double; y1: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathEllipticArcTo';

function _blPathClose(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathClose';

function _blPathAddGeometry(self: Pointer; geometryType: Integer; const geometryData: Pointer; const m: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddGeometry';

function _blPathAddBoxI(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddBoxI';

function _blPathAddBoxD(self: Pointer; const box: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddBoxD';

function _blPathAddRectI(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddRectI';

function _blPathAddRectD(self: Pointer; const rect: Pointer; dir: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddRectD';

function _blPathAddPath(self: Pointer; const other: Pointer; const range: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddPath';

function _blPathAddTranslatedPath(self: Pointer; const other: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddTranslatedPath';

function _blPathAddTransformedPath(self: Pointer; const other: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddTransformedPath';

function _blPathAddReversedPath(self: Pointer; const other: Pointer; const range: Pointer; reverseMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddReversedPath';

function _blPathAddStrokedPath(self: Pointer; const other: Pointer; const range: Pointer; const options: Pointer; const approx: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathAddStrokedPath';

function _blPathRemoveRange(self: Pointer; const range: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathRemoveRange';

function _blPathTranslate(self: Pointer; const range: Pointer; const p: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathTranslate';

function _blPathTransform(self: Pointer; const range: Pointer; const m: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathTransform';

function _blPathFitTo(self: Pointer; const range: Pointer; const rect: Pointer; fitFlags: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathFitTo';

function _blPathEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blPathEquals';

function _blPathGetInfoFlags(const self: Pointer; flagsOut: PUInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetInfoFlags';

function _blPathGetControlBox(const self: Pointer; boxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetControlBox';

function _blPathGetBoundingBox(const self: Pointer; boxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetBoundingBox';

function _blPathGetFigureRange(const self: Pointer; index: Size_T; rangeOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetFigureRange';

function _blPathGetLastVertex(const self: Pointer; vtxOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetLastVertex';

function _blPathGetClosestVertex(const self: Pointer; const p: Pointer; maxDistance: Double; indexOut: PSize_T; distanceOut: PDouble): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathGetClosestVertex';

function _blPathHitTest(const self: Pointer; const p: Pointer; fillRule: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathHitTest';

function _blStrokeOptionsInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsInit';

function _blStrokeOptionsInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsInitMove';

function _blStrokeOptionsInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsInitWeak';

function _blStrokeOptionsDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsDestroy';

function _blStrokeOptionsReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsReset';

function _blStrokeOptionsEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsEquals';

function _blStrokeOptionsAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsAssignMove';

function _blStrokeOptionsAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStrokeOptionsAssignWeak';

function _blPathStrokeToSink(const self: Pointer; const range: Pointer; const strokeOptions: Pointer; const approximationOptions: Pointer; a: Pointer; b: Pointer; c: Pointer; sink: _BLPathStrokeSinkFunc; userData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPathStrokeToSink';

// pattern.h

function _blPatternInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternInit';

function _blPatternInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternInitMove';

function _blPatternInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternInitWeak';

function _blPatternInitAs(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternInitAs';

function _blPatternDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternDestroy';

function _blPatternReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternReset';

function _blPatternAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternAssignMove';

function _blPatternAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternAssignWeak';

function _blPatternAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternAssignDeep';

function _blPatternCreate(self: Pointer; const image: Pointer; const area: Pointer; extendMode: Integer; const transform: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternCreate';

function _blPatternGetImage(const self: Pointer; image: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternGetImage';

function _blPatternSetImage(self: Pointer; const image: Pointer; const area: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternSetImage';

function _blPatternResetImage(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternResetImage';

function _blPatternGetArea(const self: Pointer; areaOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternGetArea';

function _blPatternSetArea(self: Pointer; const area: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternSetArea';

function _blPatternResetArea(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternResetArea';

function _blPatternGetExtendMode(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternGetExtendMode';

function _blPatternSetExtendMode(self: Pointer; extendMode: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternSetExtendMode';

function _blPatternGetTransform(const self: Pointer; transformOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternGetTransform';

function _blPatternGetTransformType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternGetTransformType';

function _blPatternApplyTransformOp(self: Pointer; opType: Integer; const opData: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPatternApplyTransformOp';

function _blPatternEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blPatternEquals';

// pixelconverter.h

function _blPixelConverterInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterInit';

function _blPixelConverterInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterInitWeak';

function _blPixelConverterDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterDestroy';

function _blPixelConverterReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterReset';

function _blPixelConverterAssign(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterAssign';

function _blPixelConverterCreate(self: Pointer; const dstInfo: Pointer; const srcInfo: Pointer; createFlags: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterCreate';

function _blPixelConverterConvert(const self: Pointer; dstData: Pointer; dstStride: IntPtr; const srcData: Pointer; srcStride: IntPtr; w: UInt32; h: UInt32; const options: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blPixelConverterConvert';

// random.h

function _blRandomReset(self: Pointer; seed: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blRandomReset';

function _blRandomNextUInt32(self: Pointer): UInt32; cdecl;
  external _LIB_BLEND2D name 'blRandomNextUInt32';

function _blRandomNextUInt64(self: Pointer): UInt64; cdecl;
  external _LIB_BLEND2D name 'blRandomNextUInt64';

function _blRandomNextDouble(self: Pointer): Double; cdecl;
  external _LIB_BLEND2D name 'blRandomNextDouble';

// runtime.h

function _blRuntimeInit(): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeInit';

function _blRuntimeShutdown(): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeShutdown';

function _blRuntimeCleanup(cleanupFlags: Cardinal): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeCleanup';

function _blRuntimeQueryInfo(infoType: Integer; infoOut: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeQueryInfo';

function _blRuntimeMessageOut(const msg: PUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeMessageOut';

function _blRuntimeMessageFmt(const fmt: PUTF8Char): Integer varargs; cdecl;
  external _LIB_BLEND2D name 'blRuntimeMessageFmt';

function _blRuntimeMessageVFmt(const fmt: PUTF8Char; ap: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeMessageVFmt';

function _blResultFromPosixError(e: Integer): Integer; cdecl;
  external _LIB_BLEND2D name '_blResultFromPosixError';

// runtimescope.h

function _blRuntimeScopeBegin(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeScopeBegin';

function _blRuntimeScopeEnd(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blRuntimeScopeEnd';

function _blRuntimeScopeIsActive(const self: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blRuntimeScopeIsActive';

// string.h

function _blStringInit(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInit';

function _blStringInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInitMove';

function _blStringInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInitWeak';

function _blStringInitWithData(self: Pointer; const str: PUTF8Char; size: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInitWithData';

function _blStringDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringDestroy';

function _blStringReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringReset';

function _blStringGetData(const self: Pointer): PUTF8Char; cdecl;
  external _LIB_BLEND2D name 'blStringGetData';

function _blStringGetSize(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blStringGetSize';

function _blStringGetCapacity(const self: Pointer): Size_T; cdecl;
  external _LIB_BLEND2D name 'blStringGetCapacity';

function _blStringClear(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringClear';

function _blStringShrink(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringShrink';

function _blStringReserve(self: Pointer; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringReserve';

function _blStringResize(self: Pointer; n: Size_T; fill: UTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringResize';

function _blStringMakeMutable(self: Pointer; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringMakeMutable';

function _blStringModifyOp(self: Pointer; op: Integer; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringModifyOp';

function _blStringInsertOp(self: Pointer; index: Size_T; n: Size_T; dataOut: PPUTF8Char): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInsertOp';

function _blStringAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringAssignMove';

function _blStringAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringAssignWeak';

function _blStringAssignDeep(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringAssignDeep';

function _blStringAssignData(self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringAssignData';

function _blStringApplyOpChar(self: Pointer; op: Integer; c: UTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringApplyOpChar';

function _blStringApplyOpData(self: Pointer; op: Integer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringApplyOpData';

function _blStringApplyOpString(self: Pointer; op: Integer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringApplyOpString';

function _blStringApplyOpFormat(self: Pointer; op: Integer; const fmt: PUTF8Char): Integer varargs; cdecl;
  external _LIB_BLEND2D name 'blStringApplyOpFormat';

function _blStringApplyOpFormatV(self: Pointer; op: Integer; const fmt: PUTF8Char; ap: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringApplyOpFormatV';

function _blStringInsertChar(self: Pointer; index: Size_T; c: UTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInsertChar';

function _blStringInsertData(self: Pointer; index: Size_T; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInsertData';

function _blStringInsertString(self: Pointer; index: Size_T; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringInsertString';

function _blStringRemoveIndex(self: Pointer; index: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringRemoveIndex';

function _blStringRemoveRange(self: Pointer; rStart: Size_T; rEnd: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringRemoveRange';

function _blStringEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blStringEquals';

function _blStringEqualsData(const self: Pointer; const str: PUTF8Char; n: Size_T): Boolean; cdecl;
  external _LIB_BLEND2D name 'blStringEqualsData';

function _blStringCompare(const a: Pointer; const b: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringCompare';

function _blStringCompareData(const self: Pointer; const str: PUTF8Char; n: Size_T): Integer; cdecl;
  external _LIB_BLEND2D name 'blStringCompareData';

// var.h

function _blVarInitType(self: Pointer; &type: Integer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitType';

function _blVarInitNull(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitNull';

function _blVarInitBool(self: Pointer; value: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitBool';

function _blVarInitInt32(self: Pointer; value: Int32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitInt32';

function _blVarInitInt64(self: Pointer; value: Int64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitInt64';

function _blVarInitUInt32(self: Pointer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitUInt32';

function _blVarInitUInt64(self: Pointer; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitUInt64';

function _blVarInitDouble(self: Pointer; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitDouble';

function _blVarInitRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitRgba';

function _blVarInitRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitRgba32';

function _blVarInitRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitRgba64';

function _blVarInitMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitMove';

function _blVarInitWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarInitWeak';

function _blVarDestroy(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarDestroy';

function _blVarReset(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarReset';

function _blVarAssignNull(self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignNull';

function _blVarAssignBool(self: Pointer; value: Boolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignBool';

function _blVarAssignInt32(self: Pointer; value: Int32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignInt32';

function _blVarAssignInt64(self: Pointer; value: Int64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignInt64';

function _blVarAssignUInt32(self: Pointer; value: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignUInt32';

function _blVarAssignUInt64(self: Pointer; value: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignUInt64';

function _blVarAssignDouble(self: Pointer; value: Double): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignDouble';

function _blVarAssignRgba(self: Pointer; const rgba: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignRgba';

function _blVarAssignRgba32(self: Pointer; rgba32: UInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignRgba32';

function _blVarAssignRgba64(self: Pointer; rgba64: UInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignRgba64';

function _blVarAssignMove(self: Pointer; other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignMove';

function _blVarAssignWeak(self: Pointer; const other: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarAssignWeak';

function _blVarGetType(const self: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarGetType';

function _blVarToBool(const self: Pointer; &out: PBoolean): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToBool';

function _blVarToInt32(const self: Pointer; &out: PInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToInt32';

function _blVarToInt64(const self: Pointer; &out: PInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToInt64';

function _blVarToUInt32(const self: Pointer; &out: PUInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToUInt32';

function _blVarToUInt64(const self: Pointer; &out: PUInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToUInt64';

function _blVarToDouble(const self: Pointer; &out: PDouble): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToDouble';

function _blVarToRgba(const self: Pointer; &out: Pointer): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToRgba';

function _blVarToRgba32(const self: Pointer; &out: PUInt32): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToRgba32';

function _blVarToRgba64(const self: Pointer; &out: PUInt64): Integer; cdecl;
  external _LIB_BLEND2D name 'blVarToRgba64';

function _blVarEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEquals';

function _blVarEqualsNull(const self: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsNull';

function _blVarEqualsBool(const self: Pointer; value: Boolean): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsBool';

function _blVarEqualsInt64(const self: Pointer; value: Int64): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsInt64';

function _blVarEqualsUInt64(const self: Pointer; value: UInt64): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsUInt64';

function _blVarEqualsDouble(const self: Pointer; value: Double): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsDouble';

function _blVarEqualsRgba(const self: Pointer; const rgba: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsRgba';

function _blVarEqualsRgba32(const self: Pointer; rgba32: UInt32): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsRgba32';

function _blVarEqualsRgba64(const self: Pointer; rgba64: UInt64): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarEqualsRgba64';

function _blVarStrictEquals(const a: Pointer; const b: Pointer): Boolean; cdecl;
  external _LIB_BLEND2D name 'blVarStrictEquals';

{$ENDIF}
