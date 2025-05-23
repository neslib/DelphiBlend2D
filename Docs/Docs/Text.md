# Text

Fonts & Text support.

## Glyph Containers & Processing

* [TBLGlyphBuffer](Reference/Blend2D/classes/TBLGlyphBuffer.md) - holds glyphs and additional metadata.
* [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md) - provides a glyph run data.
* [TBLGlyphRunFlags](Reference/Blend2D/types/TBLGlyphRunFlags.md) - flags used by [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md).
* [TBLGlyphRunIterator](Reference/Blend2D/classes/TBLGlyphRunIterator.md) - iterates a [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md).
* [TBLGlyphId](Reference/Blend2D/types/TBLGlyphId.md) - a type representing a single glyph (32-bit value).
* [TBLGlyphInfo](Reference/Blend2D/classes/TBLGlyphInfo.md) - glyph information used by [TBLGlyphBuffer](Reference/Blend2D/classes/TBLGlyphBuffer.md) and [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md).
* [TBLGlyphMappingState](Reference/Blend2D/classes/TBLGlyphMappingState.md) - information accumulated during mapping characters to glyphs.
* [TBLGlyphPlacement](Reference/Blend2D/classes/TBLGlyphPlacement.md) - glyph placement used by [TBLGlyphBuffer](Reference/Blend2D/classes/TBLGlyphBuffer.md) and [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md).
* [TBLGlyphPlacementType](Reference/Blend2D/types/TBLGlyphPlacementType.md) - glyph placement type used by [TBLGlyphBuffer](Reference/Blend2D/classes/TBLGlyphBuffer.md) and [TBLGlyphRun](Reference/Blend2D/classes/TBLGlyphRun.md).

## Fonts

* [TBLFont](Reference/Blend2D/classes/TBLFont.md) - represents a displayable font (having size, font properties, and variations configured).
  * [TBLFontMatrix](Reference/Blend2D/classes/TBLFontMatrix.md) - a simple matrix that can be used to transform a font, used by [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontMetrics](Reference/Blend2D/classes/TBLFontMetrics.md) - font metrics, used by [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontStretch](Reference/Blend2D/types/TBLFontStretch.md) - font stretch property, used by [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontStyle](Reference/Blend2D/types/TBLFontStyle.md) - font style property, used by [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontWeight](Reference/Blend2D/types/TBLFontWeight.md) - font weight property, used by [TBLFont](Reference/Blend2D/classes/TBLFont.md).
* [TBLFontData](Reference/Blend2D/classes/TBLFontData.md) - provides font data that can be used by [TBLFont](Reference/Blend2D/classes/TBLFont.md) and [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontDataFlags](Reference/Blend2D/types/TBLFontDataFlags.md) - Flags used by [TBLFontData](Reference/Blend2D/classes/TBLFontData.md).
  * [TBLFontTable](Reference/Blend2D/classes/TBLFontTable.md) - represents a TrueType/OpenType table identified by a [TBLTag](Reference/Blend2D/types/TBLTag.md).
* [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md) - represents a font face (OpenType file loaded from file or memory).
  * [TBLFontFaceInfo](Reference/Blend2D/classes/TBLFontFaceInfo.md) - font face information, used by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontFaceFlags](Reference/Blend2D/types/TBLFontFaceFlags.md) - flags used by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontFaceDiagFlags](Reference/Blend2D/types/TBLFontFaceDiagFlags.md) - diagnostic flags used by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontFaceType](Reference/Blend2D/types/TBLFontFaceType.md) - type of a font face, provided by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontDesignMetrics](Reference/Blend2D/classes/TBLFontDesignMetrics.md) - design font metrics, used by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontOutlineType](Reference/Blend2D/types/TBLFontOutlineType.md) - type of outlines used by a font, used by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontPanose](Reference/Blend2D/classes/TBLFontPanose.md) - panose information, provided by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
  * [TBLFontUnicodeCoverage](Reference/Blend2D/classes/TBLFontUnicodeCoverage.md) - unicode coverage bits, provided by by [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md).
* [TBLFontFeatureSettings](Reference/Blend2D/classes/TBLFontFeatureSettings.md) - provides feature settings of a [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontFeatureSettingsView](Reference/Blend2D/classes/TBLFontFeatureSettingsView.md) - view of [TBLFontFeatureSettings](Reference/Blend2D/classes/TBLFontFeatureSettings.md).
  * [TBLFontFeatureItem](Reference/Blend2D/classes/TBLFontFeatureItem.md) - associates a font feature tag ([TBLTag](Reference/Blend2D/types/TBLTag.md)) with a value.
* [TBLFontVariationSettings](Reference/Blend2D/classes/TBLFontVariationSettings.md) - provides variation settings of a [TBLFont](Reference/Blend2D/classes/TBLFont.md).
  * [TBLFontVariationSettingsView](Reference/Blend2D/classes/TBLFontVariationSettingsView.md) - view of [TBLFontVariationSettings](Reference/Blend2D/classes/TBLFontVariationSettings.md).
  * [TBLFontVariationItem](Reference/Blend2D/classes/TBLFontVariationItem.md) - associates a font variation tag ([TBLTag](Reference/Blend2D/types/TBLTag.md)) with a value.

## Font Management

* [TBLFontManager](Reference/Blend2D/classes/TBLFontManager.md) - simple font management that can store and query [TBLFontFace](Reference/Blend2D/classes/TBLFontFace.md) instances.
  * [TBLFontQueryProperties](Reference/Blend2D/classes/TBLFontQueryProperties.md) - font query properties, used by [TBLFontManager](Reference/Blend2D/classes/TBLFontManager.md).

## Text

* [TBLOrientation](Reference/Blend2D/types/TBLOrientation.md) - text or glyph run orientation.
* [TBLTextDirection](Reference/Blend2D/types/TBLTextDirection.md) - specifies text direction.
* [TBLTextEncoding](Reference/Blend2D/types/TBLTextEncoding.md) - specifies text encoding.
* [TBLTextMetrics](Reference/Blend2D/classes/TBLTextMetrics.md) - metrics of a whole text run.
