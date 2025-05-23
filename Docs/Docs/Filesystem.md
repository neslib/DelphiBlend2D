# Filesystem

Filesystem utilities.

Blend2D doesn't do much with filesystem, however, since the library provides API for loading and saving raster images and for loading font files, it internally needs a lightweight filesystem access. The API is also provided for users that would like to use a very simple API to access a filesystem.

## File

* [TBLFile](Reference/Blend2D/classes/TBLFile.md) - a lightweight non-shareable file API that uses a system file descriptor API where possible.
* [TBLFileOpenFlags](Reference/Blend2D/types/TBLFileOpenFlags.md) - flags used by [TBLFile.Open](Reference/Blend2D/classes/TBLFile.md/#Open) method.
* [TBLFileReadFlags](Reference/Blend2D/types/TBLFileReadFlags.md) - flags used when reading whole files by [TBLFileSystem.ReadFile](Reference/Blend2D/classes/TBLFileSystem.md/#ReadFile).
* [TBLFileSeekType](Reference/Blend2D/types/TBLFileSeekType.md) - flags used by [TBLFile.Seek](Reference/Blend2D/classes/TBLFile.md/#Seek) method.

## Filesystem

* [TBLFileSystem](Reference/Blend2D/classes/TBLFileSystem.md) - filesystem utilities.
* [TBLFileInfo](Reference/Blend2D/classes/TBLFileInfo.md) - file information.
* [TBLFileInfoFlags](Reference/Blend2D/types/TBLFileInfoFlags.md) - flags used by [TBLFileInfo](Reference/Blend2D/classes/TBLFileInfo.md).
