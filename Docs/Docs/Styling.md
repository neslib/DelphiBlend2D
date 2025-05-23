# Styling

Colors, gradients, and patterns.

Styling provides various classes that represent colors, gradients, and patterns. While colors are used universally across the library (not just as styles), gradients and patterns are always used as fill or stroke styles.

## Colors

* [TBLRgba](Reference/Blend2D/classes/TBLRgba.md) - RGBA color specified as 32-bit floating point value per channel.
* [TBLRgba32](Reference/Blend2D/classes/TBLRgba32.md) - RGBA color specified as 8-bit value per channel as `$AARRGGBB`. This type is compatible with Delphi's `TAlphaColor` type.
* [TBLRgba64](Reference/Blend2D/classes/TBLRgba64.md) - RGBA color specified as 16-bit value per channel as `$AAAARRRRGGGGBBBB`.

!!! note

    The order if bytes in `TBLRgba32` and `TBLRgba64` is ARGB (from MSB to LSB) for compatibility with other libraries and common representations.

## Gradients

* [TBLGradient](Reference/Blend2D/classes/TBLGradient.md) - container that holds gradient values and stops.
* [TBLGradientStop](Reference/Blend2D/classes/TBLGradientStop.md) - associates a color with offset (from 0.0 to 1.0).
* [TBLGradientKind](Reference/Blend2D/types/TBLGradientKind.md) - describes a gradient type.
* [TBLGradientValue](Reference/Blend2D/types/TBLGradientValue.md) - index of a gradient value (overlaps between various gradient types).
* [TBLGradientQuality](Reference/Blend2D/types/TBLGradientQuality.md) - describes a gradient quality.
* [TBLExtendMode](Reference/Blend2D/types/TBLExtendMode.md) - specifies a gradient extend mode (only simple extend modes can be used with gradients).
* [TBLLinearGradientValues](Reference/Blend2D/classes/TBLLinearGradientValues.md) - values describing a linear gradient.
* [TBLRadialGradientValues](Reference/Blend2D/classes/TBLRadialGradientValues.md) - values describing a radial gradient.
* [TBLConicGradientValues](Reference/Blend2D/classes/TBLConicGradientValues.md) - values describing a conic gradient.

## Patterns

* [TBLPattern](Reference/Blend2D/classes/TBLPattern.md) - represents a pattern.
* [TBLPatternQuality](Reference/Blend2D/types/TBLPatternQuality.md) - describes a pattern quality.
* [TBLExtendMode](Reference/Blend2D/types/TBLExtendMode.md) - specifies a pattern extend mode (all extend modes can be used with patterns).

## Variant

* [TBLVar](Reference/Blend2D/classes/TBLVar.md) - variant type can be used to hold any style and then passed to the rendering context.
