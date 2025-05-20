# Building the native Blend2D libraries

This is internal documentation. It gives a short bullet point descriptions for building the Blend2D libraries. For details information, see https://blend2d.com/doc/build-instructions.html.

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

Not supported yet because of AsmJit compilation errors for iOS target.

* Use CMake GUI. 
* When pressing "Configure", choose "Specify options for cross-compiling".
* On the next page, set "Operating System" to "iOS" (case sensitive) and "Version" to "12.0"
* After Configure, set:
  * Check BLEND2D_STATIC
  * CMAKE_OSX_DEPLOYMENT_TARGET to 12.0 (add if needed)

* Configure again.
* Press Generate.
* Open project.
* In Xcode, choose "Product | Scheme | Edit Scheme.."
* Select the "Run" scheme and set Build Configuration to Release
* Choose "Product | Build For | Running"
* In the project view, expand the "Products" tab to locate the libblend2d.a file. Right click it and select "Show in Finder". Copy and rename this file to "libblend2d_ios.a".

## Android

Not supported yet.
