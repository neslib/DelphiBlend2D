# Neslib.Sdl3

Delphi bindings and OOP-like wrappers for [SDL3](https://libsdl.org/index.php) and its satellite libraries [SDL_image](https://github.com/libsdl-org/SDL_image), [SDL_ttf](https://github.com/libsdl-org/SDL_ttf), SDL_mixer (coming soon), SDL_net (coming soon) and SDL_rtf (coming soon).

## Supported Platforms

Neslib.Sdl3 is supported on these platforms:

* Windows (32-bit and 64-bit).
* macOS (currently 64-bit Intel only. ARM support will be added later).
* iOS (64-bit device only).
* Android (32-bit and 64-bit).

We provide the static and dynamic libraries for these platforms, so everything you need is contained in this repository. For Windows, you need to deploy the SDL3.dll file with your application. You can find this is the "Deploy\Win32" or "Deploy\Win64" directory. For the other platforms, a static library is used that is linked directly into the application, so there is no need to deploy anything.

## API Levels

We provide 2 API levels:

* A low level API that uses the C API directly. This API is contained in a single unit called `Neslib.Sdl3.Api.pas`, which contains the SDL C header translations (which have been generated using [Chet](https://github.com/neslib/Chet) and cleaned up by hand).
* A higher level OOP-like API, that can be found in the other `Neslib.Sdl3.*.pas` units. These form a very lightweight (often zero-overhead) layer on top of the SDL3 C API's. These OOP-like APIs are more type-safe and are more natural to use for Delphi developers. Since these API's are very lightweight, it is recommended to use them instead of the C APIs.

## Documentation

Like SDL3 itself, Neslib.Sdl3 is very well documented.

If you plan to use the C API, you can use the official [SDL3 Wiki documentation](https://wiki.libsdl.org/SDL3/FrontPage).

For the OOP-like API, use the [API by category](APIByCategory/index.md) page as a starting point, or the [API Reference](Reference/Neslib.Sdl3/index.md) pages for complete documentation by unit. This documentation is extracted from the source code, so you can use the (XML) documentation in the source code as well. Delphi's CodeInsight will also show this documentation when you hover the mouse over an SDL declaration.

## Examples

The Examples directory contains Delphi versions of some SDL3 examples. These all use the OOP-like Delphi API instead of the C API. 

These examples should give you a good head start. The easiest way to create your own SDL3 application is to copy and rename one of these examples and work from there.

## Platform Specifics

Some platforms require some additional setup.

### Windows

There are no additional requirements other than deploying the 32-bit or 64-bit version of SDL3.dll with your application. For the SDL satellite libraries, you must deploy the corresponding DLL's as well.

### macOS

Only Intel macOS is currently supported (since I don't have an ARM Mac to test on).

You don't need to deploy any files. The SDL library, as well as its satellite libraries, are linked into the application.

You may need to add the following frameworks to the SDK manager if you haven't done so already:

* AudioToolbox
* CoreHaptics
* ForceFeedback
* GameController
* UniformTypeIdentifiers

To add these in Delphi:

* Choose "Tools | Options... | Deployment | SDK Manager".
* Select the desired macOS SDK in the "SDK versions" list.
* Click the "Add a new path item" glyph button near the top-right. In the window that appears, enter:
  * **Path on remote machine**: $(SDKROOT)/System/Library/Frameworks
  * **Framework name**: the name of the framework (from the list above)
  * Set **Path type** to **Framework**.
* Don't forget to click the "Update Local File Cache" button once you have added these frameworks.

### iOS

Only deploying to an actual iOS device is supported. The simulator is not supported since it is not supported by the underlying SDL library.

You don't need to deploy any files. The SDL library, as well as its satellite libraries, are linked into the application.

You may need to add the following frameworks to the SDK manager if you haven't sone so already (see above under macOS for instructions on how to add them):

* CoreHaptics
* CoreMotion
* GameController

### Android

You don't need to deploy any files. The SDL library, as well as its satellite libraries, are linked into the application.

You need to add the "sdl3.jar" Java archive to your applications though (for both 32-bit and 64-bit build):

* In the project window, open the "Android 32-bit" or ("Android 64-bit") target.
* Right-click on the "Libraries" node and choose "Add..."
* Add the "sdl3.jar" file, which you can find in the "Neslib.Sdl3\Deploy" directory.

Future Delphi versions may change the list of .jar files, leading to compilation errors. When that happens, right-click the "Libraries" node and select "Revert System Files to Default". This should keep the "sdl3.jar" file, but you can always re-add it if it doesn't.

## License

Both SDL3 and Neslib.Sdl3 are released under the zlib license. See LICENSE.txt for details.
