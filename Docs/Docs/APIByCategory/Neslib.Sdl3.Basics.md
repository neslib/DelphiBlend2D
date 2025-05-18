# Neslib.Sdl3.Basics

!!! abstract "Reference"

    [Neslib.Sdl3.Basics](../Reference/Neslib.Sdl3.Basics/index.md)

## Initialization and Shutdown

All SDL programs need to initialize the library before starting to work with it.

Almost everything can simply call [SdlInit](../Reference/Neslib.Sdl3.Basics/routines/SdlInit.md) near startup, with a handful of flags to specify subsystems to touch. These are here to make sure SDL does not even attempt to touch low-level pieces of the operating system that you don't intend to use. For example, you might be using SDL for video and input but chose an external library for audio, and in this case you would just need to leave off the `TSdlInitFlag.Audio` flag to make sure that external library has complete control.

Most apps, when terminating, should call [SdlQuit](../Reference/Neslib.Sdl3.Basics/routines/SdlQuit.md). This will clean up (nearly) everything that SDL might have allocated, and crucially, it'll make sure that the display's resolution is back to what the user expects if you had previously changed it for your game.

SDL3 apps are strongly encouraged to call [SdlSetAppMetadata(AAppName, AAppVersion, AAppIdentifier: String)](../Reference/Neslib.Sdl3.Basics/routines/SdlSetAppMetadata_0.md) at startup to fill in details about the program. This is completely optional, but it helps in small ways (we can provide an About dialog box for the macOS menu, we can name the app in the system's audio mixer, etc). Those that want to provide a lot of information should look at the more-detailed [SdlSetAppMetadata(AName, AValue: String)](../Reference/Neslib.Sdl3.Basics/routines/SdlSetAppMetadata_1.md).

## Configuration Variables

[TSdlHints](../Reference/Neslib.Sdl3.Basics/classes/TSdlHints.md) contains functions to set and get configuration hints, as well as listing each of them alphabetically.

In general these hints are just that - they may or may not be supported or applicable on any given platform, but they provide a way for an application or user to give the library a hint as to how they would like the library to work.

## Properties

A property is a variable that can be created and retrieved by name at runtime.

All properties are part of a property bag (or property group) called [TSdlProperties](../Reference/Neslib.Sdl3.Basics/classes/TSdlProperties.md). A list of available properties is available as constants in the [TSdlProperty](../Reference/Neslib.Sdl3.Basics/classes/TSdlProperty.md) record, and these key by of various types, such as strings, integer numbers, floating point numbers and Booleans (see [TSdlPropertyType](../Reference/Neslib.Sdl3.Basics/types/TSdlPropertyType.md)).

## Error Handling

When an SDL API returns an error, the [TSdlApp.HandleError](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#HandleError) method is called. By default, this method raises an exception of type [ESdlError](../Reference/Neslib.Sdl3.Basics/classes/ESdlError.md), but you can override this method perform some other action, such as logging the error instead.

## Log Handling
[TSdlLog](../Reference/Neslib.Sdl3.Basics/classes/TSdlLog.md) enables simple log messages with priorities and categories. A message's [TSdlLogPriority](../Reference/Neslib.Sdl3.Basics/types/TSdlLogPriority.md) signifies how important the message is. A message's [TSdlLogCategory](../Reference/Neslib.Sdl3.Basics/types/TSdlLogCategory.md) signifies from what domain it belongs to. Every category has a minimum priority specified: when a message belongs to that category, it will only be sent out if it has that minimum priority or higher.

SDL's own logs are sent below the default priority threshold, so they are quiet by default.

You can change the log verbosity programmatically using [TSdlLog.Priorities](../Reference/Neslib.Sdl3.Basics/classes/TSdlLog.md/#Priorities) or with [TSdlHints.Logging](../Reference/Neslib.Sdl3.Basics/classes/TSdlHints.md/#Logging), or with the `SDL_LOGGING` environment variable. This variable is a comma separated set of category=level tokens that define the default logging levels for SDL applications.

The category can be a numeric category, one of "app", "error", "assert", "system", "audio", "video", "render", "input", "test", or * for any unspecified category.

The level can be a numeric level, one of "trace", "verbose", "debug", "info", "warn", "error", "critical", or "quiet" to disable that category.

You can omit the category if you want to set the logging level for all categories.

If this hint isn't set, the default log levels are equivalent to:

`app=info,assert=warn,test=verbose,*=error`

Here's where the messages go on different platforms:

- Windows: debug output stream
- Android: log output
- Others: standard error output (stderr)

You don't need to have a newline (#10) on the end of messages, the functions will do that for you. For consistent behavior cross-platform, you shouldn't have any newlines in messages, such as to log multiple lines in one call; unusual platform-specific behavior can be observed in such usage. Do one log call per line instead, with no newlines in messages.

Each log call is atomic, so you won't see log messages cut off one another when logging from multiple threads.

## Version Information

[TSdlVersion](../Reference/Neslib.Sdl3.Basics/classes/TSdlVersion.md) provides functionality to query the current SDL version, both as headers the app was compiled against, and a library the app is linked to.