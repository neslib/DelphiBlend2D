# Neslib.Sdl3.Additional

!!! abstract "Reference"

    [Neslib.Sdl3.Additional](../Reference/Neslib.Sdl3.Additional/index.md)

## CPU Feature Detection

These functions are largely concerned with reporting if the system has access to various SIMD instruction sets, but also has other important info to share, such as system RAM size and number of logical CPU cores.

CPU instruction set checks, like [TSdlCpu.HasSse](../Reference/Neslib.Sdl3.Additional/classes/TSdlCpu.md/#HasSse) and [TSdlCpu.HasNeon](../Reference/Neslib.Sdl3.Additional/classes/TSdlCpu.md/#HasNeon), are available on all platforms, even if they don't make sense (an ARM processor will never have SSE and an x86 processor will never have NEON, for example, but these functions still exist and will simply return False in these cases).

## Process Control

These functions provide a cross-platform way to spawn and manage OS-level processes.

You can create a new subprocess with [TSdlProcess](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md) and optionally read and write to it using [TSdlProcess.Read](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Read_1) or [TSdlProcess.Input](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Input) and [TSdlProcess.Output](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Output). If more advanced functionality like chaining input between processes is necessary, you can use the [TSdlProcess.Create](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Create_0) constructor that takes a set of properties.

You can get the status of a created process with [TSdlProcess.Wait](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Wait_0), or terminate the process with [TSdlProcess.Kill](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Kill).

Don't forget to call [TSdlProcess.Free](../Reference/Neslib.Sdl3.Additional/classes/TSdlProcess.md/#Free) to clean up, whether the process process was killed, terminated on its own, or is still running!

## Power Management Status

There is a single function in this category: [SdlGetPowerInfo](../Reference/Neslib.Sdl3.Additional/routines/SdlGetPowerInfo.md).

This function is useful for games on the go. This allows an app to know if it's running on a draining battery, which can be useful if the app wants to reduce processing, or perhaps framerate, to extend the duration of the battery's charge. Perhaps the app just wants to show a battery meter when fullscreen, or alert the user when the power is getting extremely low, so they can save their game.

## Message Boxes

SDL offers a simple message box API, which is useful for simple alerts, such as informing the user when something fatal happens at startup without the need to build a UI for it (or informing the user *before* your UI is ready).

These message boxes are native system dialogs where possible.

There is both a customizable function ([SdlShowMessageBox](../Reference/Neslib.Sdl3.Additional/routines/SdlShowMessageBox.md)) that offers lots of options for what to display and reports on what choice the user made, and also a much-simplified version ([SdlShowSimpleMessageBox](../Reference/Neslib.Sdl3.Additional/routines/SdlShowSimpleMessageBox_0.md)), merely takes a text message and title, and waits until the user presses a single "OK" UI button. Often, this is all that is necessary.

## File Dialogs

SDL offers file dialogs, to let users select files with native GUI interfaces. There are "open" dialogs, "save" dialogs, and folder selection dialogs. All are centered around then [TSdlDialog](../Reference/Neslib.Sdl3.Additional/classes/TSdlDialog.md) record. The app can control some details, such as filtering to specific files, or whether multiple files can be selected by the user.

Note that launching a file dialog is a non-blocking operation; control returns to the app immediately, and a callback is called later (possibly in another thread) when the user makes a choice.

## System Tray

SDL offers a way to add items to the "system tray" (more correctly called the "notification area" on Windows). On platforms that offer this concept, an SDL app can add a tray icon ([TSdlTray](../Reference/Neslib.Sdl3.Additional/classes/TSdlTray.md)), submenus ([TSdlTrayMenu](../Reference/Neslib.Sdl3.Additional/classes/TSdlTrayMenu.md)), checkboxes and clickable entries ([TSdlTrayEntry](../Reference/Neslib.Sdl3.Additional/classes/TSdlTrayEntry.md)), and register a callback that is fired when the user clicks on these pieces.

## Locale Info

This provides a way to get a list of preferred locales (language plus country) for the user. The property [TSdlLocale.PreferredLocales](../Reference/Neslib.Sdl3.Additional/classes/TSdlLocale.md/#PreferredLocales) handles all the heavy lifting, and offers documentation on all the strange ways humans might have configured their language settings.

## Platform-specific Functionality

Platform-specific SDL API functions. These are functions that deal with needs of specific operating systems, that didn't make sense to offer as platform-independent, generic APIs.

Most apps can make do without these functions, but they can be useful for integrating with other parts of a specific system, adding platform-specific polish to an app, or solving problems that only affect one target.

## Runtime Library

SDL provides its own implementation of some of the most important C and Delphi runtime functions. The SDL implementations work identically across platforms.

SDL also offers other runtime-adjacent functionality in this unit that either isn't, strictly speaking, part of any runtime standards, like [SdlCrc32](../Reference/Neslib.Sdl3.Additional/routines/SdlCrc32.md) etc.
