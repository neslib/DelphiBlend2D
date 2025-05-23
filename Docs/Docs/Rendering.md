# Rendering

2D rendering context API, structures, and constants.

## Rendering Context

* [TBLContext](Reference/Blend2D/classes/TBLContext.md) - a 2D rendering context.
* [TBLContextType](Reference/Blend2D/types/TBLContextType.md) - rendering context type.
* [TBLContextCookie](Reference/Blend2D/classes/TBLContextCookie.md) - cookie can be used with [TBLContext.Save](Reference/Blend2D/classes/TBLContext.md/#Save_1) and [TBLContext.Restore](Reference/Blend2D/classes/TBLContext.md/#Restore_0).
* [TBLContextCreateInfo](Reference/Blend2D/classes/TBLContextCreateInfo.md) - additional options that can be used when creating a rendering context.
* [TBLContextCreateFlags](Reference/Blend2D/types/TBLContextCreateFlags.md) - flags that can be used by [TBLContextCreateInfo](Reference/Blend2D/classes/TBLContextCreateInfo.md).
* [TBLContextErrorFlags](Reference/Blend2D/types/TBLContextErrorFlags.md) - accumulated error flags during the lifetime of a rendering context.
* [TBLContextFlushFlags](Reference/Blend2D/types/TBLContextFlushFlags.md) - flags that can be passed to [TBLContext.Flush](Reference/Blend2D/classes/TBLContext.md/#Flush).
* [TBLContextHint](Reference/Blend2D/types/TBLContextHint.md) - rendering hint.
* [TBLContextHints](Reference/Blend2D/classes/TBLContextHints.md) - all rendering hints in a single record.
* [TBLContextStyleSlot](Reference/Blend2D/types/TBLContextStyleSlot.md) - style slot (either fill or stroke).
* [TBLContextStyleSwapMode](Reference/Blend2D/types/TBLContextStyleSwapMode.md) - style swap mode (how to swap a fill and stroke styles).
* [TBLContextStyleTransformMode](Reference/Blend2D/types/TBLContextStyleTransformMode.md) - style transform mode (how to combine with existing transform).
* [TBLContextRenderTextOp](Reference/Blend2D/types/TBLContextRenderTextOp.md) - type of a text rendering operation (low-level).
* [TBLCompOp](Reference/Blend2D/types/TBLCompOp.md) - composition operator.
* [TBLRenderingQuality](Reference/Blend2D/types/TBLRenderingQuality.md) - rendering quality (aliased rendering or the quality of anti-aliasing).
