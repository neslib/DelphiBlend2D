# Neslib.Sdl3.Image

!!! abstract "Reference"

    [Neslib.Sdl3.Image](../Reference/Neslib.Sdl3.Image/index.md)

## SDL3_image satellite library

This is a separate SDL satellite library that is different from the main SDL library. If you use this unit, then for the Windows platform, you must deploy the appropriate SDL3_image.dll (either the 32-bit or 64-bit version) with your application. On other platforms, this library will be linked into your executable.

## Additional File Formats

The base SDL3 library only supports loading image files in the BMP format. The [TSdlImage](../Reference/Neslib.Sdl3.Image/classes/TSdlImage.md) record allows loading files in these additional formats:

- BMP (Windows Bitmap Files)
- ICO (Icon files)
- CUR (Cursor files)
- GIF
- JPEG
- LBM (Deluxe Paint bitmap)
- PCX (Picture Exchange bitmap)
- PNG (Portable Network Graphics)
- PNM (Portable Any Map)
- SVG (Scalable Vector Graphics)
- QOI (Quite OK Image format)
- TGA (Targa image format)
- XCF (GIMP bitmap)
- XPM (X Pixmap)
- XV (Kronos Visualization Image file)

## Animated Images

The [TSdlAnimatedImage](../Reference/Neslib.Sdl3.Image/classes/TSdlAnimatedImage.md) record can be used to load animated images. Currently, only animated GIF's are supported.