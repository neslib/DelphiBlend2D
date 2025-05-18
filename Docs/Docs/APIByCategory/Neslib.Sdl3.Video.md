# Neslib.Sdl3.Video

!!! abstract "Reference"

    [Neslib.Sdl3.Video](../Reference/Neslib.Sdl3.Video/index.md)

## Display and Window Management

SDL's video subsystem is largely interested in abstracting window management from the underlying operating system. You can create windows, manage them in various ways, set them fullscreen, and get events when interesting things happen with them, such as the mouse or keyboard interacting with a window.

The video subsystem is also interested in abstracting away some platform-specific differences in OpenGL: context creation, swapping buffers, etc. This may be crucial to your app, but also you are not required to use OpenGL at all. In fact, SDL can provide rendering to those windows as well, either with an easy-to-use [2D API](#api2d) or with a more-powerful [GPU API](Neslib.Sdl3.Gpu.md) . Of course, it can simply get out of your way and give you the window handles you need to use Vulkan, Direct3D, Metal, or whatever else you like directly, too.

The video subsystem covers a lot of functionality, out of necessity, so it is worth perusing the list of functions just to see what's available, but most apps can get by with simply creating a window and listening for events, so start with [TSdlWindow](../Reference/Neslib.Sdl3.Video/classes/TSdlWindow.md) and [TSdlApp.Event](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Event).

## 2D Accelerated Rendering {#api2d}

[TSdlRenderer](../Reference/Neslib.Sdl3.Video/classes/TSdlRenderer.md/) and related APIs supports the following features:

- single pixel points
- single pixel lines
- filled rectangles
- texture images
- 2D polygons

The primitives may be drawn in opaque, blended, or additive modes.

The texture images may be drawn in opaque, blended, or additive modes. They can have an additional color tint or alpha modulation applied to them, and may also be stretched with linear interpolation.

This API is designed to accelerate simple 2D operations. You may want more functionality such as polygons and particle effects and in that case you should use SDL's OpenGL/Direct3D support, the SDL3 GPU API, or one of the many good 3D engines.

These functions must be called from the main thread. See [this bug](https://github.com/libsdl-org/SDL/issues/986) for details.

## Pixel Formats and Conversion Routines

SDL offers facilities for pixel management.

Largely these facilities deal with pixel *format* (see [TSdlPixelFormat](../Reference/Neslib.Sdl3.Video/types/TSdlPixelFormat.md)).

If you mostly want to think of a pixel as some combination of red, green, blue, and maybe alpha intensities, this is all pretty straightforward, and in many cases, is enough information to build a perfectly fine game. See [TSdlColor](../Reference/Neslib.Sdl3.Video/classes/TSdlColor.md/) and [TSdlColorF](../Reference/Neslib.Sdl3.Video/classes/TSdlColorF.md).

However, the actual definition of a pixel is more complex than that:

Pixels are a representation of a color in a particular color space (see [TSdlColorspace](../Reference/Neslib.Sdl3.Video/types/TSdlColorspace.md)).

The first characteristic of a color space is the color type. SDL understands two different color types, RGB and YCbCr, or in SDL also referred to as YUV.

[RGB colors](https://en.wikipedia.org/wiki/RGB_color_model) consist of red, green, and blue channels of color that are added together to represent the colors we see on the screen.

[YCbCr colors](https://en.wikipedia.org/wiki/YCbCr) represent colors as a Y luma brightness component and red and blue chroma color offsets. This color representation takes advantage of the fact that the human eye is more sensitive to brightness than the color in an image. The Cb and Cr components are often compressed and have lower resolution than the luma component.

When the color information in YCbCr is compressed, the Y pixels are left at full resolution and each Cr and Cb pixel represents an average of the color information in a block of Y pixels. The chroma location determines where in that block of pixels the color information is coming from.

The color range defines how much of the pixel to use when converting a pixel into a color on the display. When the full color range is used, the entire numeric range of the pixel bits is significant. When narrow color range is used, for historical reasons, the pixel uses only a portion of the numeric range to represent colors.

The color primaries and white point are a definition of the colors in the color space relative to the standard [XYZ color space](https://en.wikipedia.org/wiki/CIE_1931_color_space).

The [transfer characteristic](https://en.wikipedia.org/wiki/Rec._709#Transfer_characteristics), or opto-electrical transfer function (OETF), is the way a color is converted from mathematically linear space into a non-linear output signals.

The matrix coefficients are used to convert between YCbCr and RGB colors.

## Blend Modes

Blend modes decide how two colors will mix together. There are both standard modes for basic needs and a means to create custom modes, dictating what sort of math to do on what color components. 

Standard blend modes are defined in [TSdlBlendMode](../Reference/Neslib.Sdl3.Video/types/TSdlBlendMode.md), but you can also define custom blend modes using [TSdlBlendMode.Compose](../Reference/Neslib.Sdl3.Video/types/TSdlBlendMode.md/#Compose).

## Points and Rectangles

Some helper APIs for managing rectangles ([TSdlRect](../Reference/Neslib.Sdl3.Video/classes/TSdlRect.md) and [TSdlRectF](../Reference/Neslib.Sdl3.Video/classes/TSdlRectF.md)), 2D points ([TSdlPoint](../Reference/Neslib.Sdl3.Video/classes/TSdlPoint.md) and [TSdlPointF](../Reference/Neslib.Sdl3.Video/classes/TSdlPointF.md)) and 2D sizes ([TSdlSize](../Reference/Neslib.Sdl3.Video/classes/TSdlSize.md) and [TSdlSizeF](../Reference/Neslib.Sdl3.Video/classes/TSdlSizeF.md)), in both integer and floating point versions.

## Surfaces

SDL surfaces are buffers of pixels in system RAM. These are useful for passing around and manipulating images that are not stored in GPU memory.

[TSdlSurface](../Reference/Neslib.Sdl3.Video/classes/TSdlSurface.md) makes serious efforts to manage images in various formats, and provides a reasonable toolbox for transforming the data, including copying between surfaces, filling rectangles in the image data, etc.

There is also a simple .bmp loader, [TSdlSurface.LoadBmp](../Reference/Neslib.Sdl3.Video/classes/TSdlSurface.md/#LoadBmp_0). SDL itself does not provide loaders for various other file formats, but there are several excellent external libraries that do, including its own satellite library, [SDL_image](https://wiki.libsdl.org/SDL3/SDL_image).

## Vulkan Support

APS's for creating Vulkan surfaces on SDL windows.

For the most part, Vulkan operates independent of SDL, but it benefits from a little support during setup.

Use [SdlVulkanGetInstanceExtensions](../Reference/Neslib.Sdl3.Video/routines/SdlVulkanGetInstanceExtensions.md) to get platform-specific bits for creating a VkInstance, then [SdlVulkanGetVkGetInstanceProcAddr](../Reference/Neslib.Sdl3.Video/routines/SdlVulkanGetVkGetInstanceProcAddr.md) to get the appropriate function for querying Vulkan entry points. Then [SdlVulkanCreateSurface](../Reference/Neslib.Sdl3.Video/routines/SdlVulkanCreateSurface.md) will get you the final pieces you need to prepare for rendering into an [TSdlWindow](../Reference/Neslib.Sdl3.Video/classes/TSdlWindow.md) with Vulkan.

Unlike OpenGL, most of the details of "context" creation and window buffer swapping are handled by the Vulkan API directly, so SDL doesn't provide Vulkan equivalents of [TSdlGL.SwapWindow](../Reference/Neslib.Sdl3.Video/classes/TSdlGL.md/#SwapWindow), etc; they aren't necessary.

## Metal Support

API's for creating Metal layers and views (see [TSdlMetalView](../Reference/Neslib.Sdl3.Video/classes/TSdlMetalView.md)) on SDL windows.

This provides some platform-specific glue for Apple platforms. Most macOS and iOS apps can use SDL without these functions, but this API they can be useful for specific OS-level integration tasks.

## Camera Support

Video capture for the SDL library.

This API lets apps read input from video sources, like webcams. [TSdlCamera](../Reference/Neslib.Sdl3.Video/classes/TSdlCamera.md) devices can be enumerated, queried, and opened. Once opened, it will provide [TSdlSurface](../Reference/Neslib.Sdl3.Video/classes/TSdlSurface.md) objects as new frames of video come in. These surfaces can be uploaded to an [TSdlTexture](../Reference/Neslib.Sdl3.Video/classes/TSdlTexture.md) or processed as pixels in memory.

Several platforms will alert the user if an app tries to access a camera, and some will present a UI asking the user if your application should be allowed to obtain images at all, which they can deny. A successfully opened camera will not provide images until permission is granted. Applications, after opening a camera device, can see if they were granted access by either polling with the [TSdlCamera.PermissionState](../Reference/Neslib.Sdl3.Video/classes/TSdlCamera.md/#PermissionState) property, or waiting for an [TSdlEventKind.CameraDeviceApproved](../Reference/Neslib.Sdl3.Events/types/TSdlEventKind.md) or `TSdlEventKind.CameraDeviceDenied` event. Platforms that don't have any user approval process will report approval immediately.

Note that SDL cameras only provide video as individual frames; they will not provide full-motion video encoded in a movie file format, although an app is free to encode the acquired frames into any format it likes. It also does not provide audio from the camera hardware through this API; not only do many webcams not have microphones at all, many people--from streamers to people on Zoom calls--will want to use a separate microphone regardless of the camera. In any case, recorded audio will be available through SDL's audio API no matter what hardware provides the microphone.

### Camera gotchas

Consumer-level camera hardware tends to take a little while to warm up, once the device has been opened. Generally most camera apps have some sort of UI to take a picture (a button to snap a pic while a preview is showing, some sort of multi-second countdown for the user to pose, like a photo booth), which puts control in the users' hands, or they are intended to stay on for long times (Pokemon Go, etc).

It's not uncommon that a newly-opened camera will provide a couple of completely black frames, maybe followed by some under-exposed images. If taking a single frame automatically, or recording video from a camera's input without the user initiating it from a preview, it could be wise to drop the first several frames (if not the first several *seconds* worth of frames!) before using images from a camera.