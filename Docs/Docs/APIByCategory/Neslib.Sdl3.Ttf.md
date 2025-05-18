# Neslib.Sdl3.Ttf

!!! abstract "Reference"

    [Neslib.Sdl3.Ttf](../Reference/Neslib.Sdl3.Ttf/index.md)

## SDL3_ttf satellite library

This is a separate SDL satellite library that is different from the main SDL library. If you use this unit, then for the Windows platform, you must deploy the appropriate SDL3_ttf.dll (either the 32-bit or 64-bit version) with your application. On other platforms, this library will be linked into your executable.

## What is it?

This library is a wrapper around the FreeType and Harfbuzz libraries, allowing you to use TrueType and OpenType fonts to render text in SDL applications.

This library also uses the following libraries:

- [FreeType](https://freetype.org/), licensed under the [FTL](https://gitlab.freedesktop.org/freetype/freetype/-/blob/master/docs/FTL.TXT). Used for loading font files and rendering glyphs.
- [HarfBuzz](https://harfbuzz.github.io/), licensed under the [MIT license](https://github.com/harfbuzz/harfbuzz/blob/main/COPYING). Used for complex text shaping, including right-to-left support and ligatures.
- [PlutoSVG](https://github.com/sammycage/plutosvg), licensed under the [MIT license](https://github.com/sammycage/plutosvg/blob/master/LICENSE). Used for rendering SVG elements in (emoji) fonts.
- [PlutoVG](https://github.com/sammycage/plutovg), licensed under the [MIT license](https://github.com/sammycage/plutovg/blob/master/LICENSE). Small vector graphics library used by PlutoSVG to render emojis.

Please follow the licenses of any fonts you include in your application, as many of them are copyrighted. The Microsoft fonts, for example, are not freely redistributable.