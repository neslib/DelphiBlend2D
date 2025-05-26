unit Blend2D.Fmx;

interface

uses
  FMX.Types,
  Blend2D;

const
  PIXEL_FORMAT_TO_BL_FORMAT: array [TPixelFormat] of TBLFormat = (
    { None     } TBLFormat.Prgb32,
    { RGB      } TBLFormat.Xrgb32,
    { RGBA     } TBLFormat.Prgb32,
    { BGR      } TBLFormat.Xrgb32,
    { BGRA     } TBLFormat.Prgb32,
    { RGBA16   } TBLFormat.Prgb32,
    { BGR_565  } TBLFormat.Prgb32,
    { BGRA4    } TBLFormat.Prgb32,
    { BGR4     } TBLFormat.Prgb32,
    { BGR5_A1  } TBLFormat.Prgb32,
    { BGR5     } TBLFormat.Prgb32,
    { BGR10_A2 } TBLFormat.Prgb32,
    { RGB10_A2 } TBLFormat.Prgb32,
    { L        } TBLFormat.A8,
    { LA       } TBLFormat.Prgb32,
    { LA4      } TBLFormat.Prgb32,
    { L16      } TBLFormat.Prgb32,
    { A        } TBLFormat.A8,
    { R16F     } TBLFormat.Prgb32,
    { RG16F    } TBLFormat.Prgb32,
    { RGBA16F  } TBLFormat.Prgb32,
    { R32F     } TBLFormat.Prgb32,
    { RG32F    } TBLFormat.Prgb32,
    { RGBA32F  } TBLFormat.Prgb32);

var
  /// <summary>
  ///  Allows use of Blend2D Canvas for UI rendering, replacing FMX's default
  ///  Canvas
  /// </summary>
  GlobalUseBlend2D: Boolean = False;

implementation

uses
  Blend2D.Fmx.Canvas;

end.
