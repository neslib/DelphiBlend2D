# Building the native Blend2D libraries

This is internal documentation. It gives a short bullet point descriptions for building the Blend2D libraries. For details information, see https://blend2d.com/doc/build-instructions.html.

## Version

https://blend2d.com/download/blend2d-0.12.0-all.zip, containing state as of 3/10/2025:

* https://github.com/blend2d/blend2d/tree/717cbf4bc0f2ca164cf2f0c48f0497779241b6c5
* https://github.com/blend2d/blend2d-apps/tree/c3ded4bf5426fe7f3b8e383dc15c52d436d3f646

## Windows

Use CMake with default options.

## macOS

* Use CMake GUI. After Configure, set:
  * Check BLEND2D_STATIC
  * CMAKE_OSX_DEPLOYMENT_TARGET to 10.13
* Configure again.
* Press Generate.
* Open project.
* Choose the "blend2D" project in the sidebar and sub-sidebar.
* Under "Build Settings", set "Architectures" to "Standard Architectures", and set "Build Active Architecture Only" to "No" (for all configurations).
* In Xcode, choose "Product | Scheme | Edit Scheme.."
* Select the "Run" scheme and set Build Configuration to Release
* Choose "Product | Build For | Running"
* In the project view, expand the "Products" node to locate the libblend2d.a file. Right click it and select "Show in Finder". Copy and rename this file to "libblend2d_macos.a".

## iOS

* Use CMake GUI. 
* When pressing "Configure", choose "Specify options for cross-compiling".
* On the next page, set "Operating System" to "iOS" (case sensitive) and "Version" to "12.0"
* After Configure, set:
  * Check BLEND2D_STATIC
  * Check BLEND2D_NO_JIT (or add it if needed) since runtime code generation is not permitted on iOS
  * CMAKE_OSX_DEPLOYMENT_TARGET to 12.0 (add if needed)
  
* Configure again.
* Press Generate.
* Open project.
* In Xcode, choose "Product | Scheme | Edit Scheme.."
* Select the "Run" scheme and set Build Configuration to Release
* Choose "Product | Build For | Running"
* In the project view, expand the "Products" tab to locate the libblend2d.a file. Right click it and select "Show in Finder". Copy and rename this file to "libblend2d_ios.a".

## Android

### 32-Bit

* Use CMake GUI.
* When pressing "Configure", set the generator to "Ninja" and choose "Specify toolchain for cross-compiling".
* On next page, select the "android.toolchain.cmake" file from your NDK directory (eg. C:/Users/Public/Documents/Embarcadero/Studio/23.0/CatalogRepository/AndroidSDK-2525-23.0.55362.2017/ndk/27.1.12297006/build/cmake/android.toolchain.cmake).
* After Configure:

  * check BLEND2D_STATIC
  * set CMAKE_BUILD_TYPE to Release

* And add the following options:
  * ANDROID_ABI: armeabi-v7a
  * ANDROID_PLATFORM: android-16
* Configure again.

* Press Generate.
* Open a command prompt in the build directory and enter `Ninja`.
* Once the static library has been generated, strip the debug symbols using `strip -g`, where the strip command is part of the NDK. For example: 
  ```Shell
  C:\Users\Public\Documents\Embarcadero\Studio\23.0\CatalogRepository\AndroidSDK-2525-23.0.55362.2017\ndk\27.1.12297006\toolchains\llvm\prebuilt\windows-x86_64\bin\llvm-strip.exe -g libblend2d.a
  ```
* Rename the library to "libblend2d_android32.a"

### 64-Bit

Use same steps as above with the following changes:

* Open the CMakeLists.txt file in the blend2d directory and search for a line with the text "mfpu" (there should be only one). Comment out this line (#) because the Android compiler is strict and generates an error when Neon is used (instead of ignoring this).
* Set ANDROID_ABI to : arm64-v8a
* Add BLEND2D_NO_JIT boolean option and check it. Note that Arm64 jitting is supported, but I'm currently not able to build a version with it. We need to try this again in the future.
* Configure, Generate, Ninja and Strip as for the 32-bit version.
* Rename the library to "libblend2d_android64.a"
* Make sure to uncomment the line in CMakeLists.txt again
