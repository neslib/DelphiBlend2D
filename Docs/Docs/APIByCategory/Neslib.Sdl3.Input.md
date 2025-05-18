# Neslib.Sdl3.Input

!!! abstract "Reference"

    [Neslib.Sdl3.Input](../Reference/Neslib.Sdl3.Input/index.md)

## Keyboard Support

Please refer to the [Best Keyboard Practices](../Misc/KeyboardBestPractices.md) document for details on how best to accept keyboard input in various types of programs.

Keyboard support revolves around the [TSdlKeyboard](../Reference/Neslib.Sdl3.Input/classes/TSdlKeyboard.md) record and the enumerated types [TSdlScancode](../Reference/Neslib.Sdl3.Input/types/TSdlScancode.md), [TSdlKeycode](../Reference/Neslib.Sdl3.Input/types/TSdlKeycode.md) and [TSdlKeyMod](../Reference/Neslib.Sdl3.Input/types/TSdlKeyMod.md).

## Mouse Support

Any GUI application has to deal with the mouse, and SDL provides functions to manage mouse input and the displayed cursor.

Most interactions with the mouse will come through the event subsystem. Moving a mouse generates an `TSdlEventKind.MouseMotion` event, pushing a button generates `TSdlEventKind.MouseButtonDown`, etc, but one can also query the current state of the mouse at any time with [TSdlMouse.GetState](../Reference/Neslib.Sdl3.Input/classes/TSdlMouse.md/#GetState_0).

For certain games, it's useful to disassociate the mouse cursor from mouse input. An FPS, for example, would not want the player's motion to stop as the mouse hits the edge of the window. For these scenarios, use [TSdlWindow.IsRelativeMouseMode](../Reference/Neslib.Sdl3.Video/classes/TSdlWindow.md/#IsRelativeMouseMode), which hides the cursor, grabs mouse input to the window, and reads mouse input no matter how far it moves.

Games that want the system to track the mouse but want to draw their own cursor can use [TSdlCursor.Hide](../Reference/Neslib.Sdl3.Input/classes/TSdlCursor.md/#Hide) and [TSdlCursor.Show](../Reference/Neslib.Sdl3.Input/classes/TSdlCursor.md/#Show). It might be more efficient to let the system manage the cursor, if possible, using [TSdlCursor.Active](../Reference/Neslib.Sdl3.Input/classes/TSdlCursor.md/#Active) with a custom image made through [TSdlCursor.Create](../Reference/Neslib.Sdl3.Input/classes/TSdlCursor.md/#Create_1), or perhaps just a specific system cursor from another version of [TSdlCursor.Create](../Reference/Neslib.Sdl3.Input/classes/TSdlCursor.md/#Create_0).

SDL can, on many platforms, differentiate between multiple connected mice, allowing for interesting input scenarios and multiplayer games. They can be enumerated with [TSdlMouse.Mice](../Reference/Neslib.Sdl3.Input/classes/TSdlMouse.md/#Mice), and SDL will send `TSdlEventKind.MouseAdded` and `TSdlEventKind.MouseRemoved` events as they are connected and unplugged.

Since many apps only care about basic mouse input, SDL offers a virtual mouse device for touch and pen input, which often can make a desktop application work on a touchscreen phone without any code changes. 

## Joystick Support

This is the lower-level joystick handling. If you want the simpler option, where what each button does is well-defined, you should use the gamepad API instead.

The term "InstanceID" is the current instantiation of a joystick device in the system, if the joystick is removed and then re-inserted then it will get a new InstanceID, InstanceID's are monotonically increasing identifiers of a joystick plugged in.

The term "PlayerIndex" is the number assigned to a player on a specific controller. For XInput controllers this returns the XInput user index. Many joysticks will not be able to supply this information.

A joystick device has a stable GUID identifier that does not change over time. It identifies class of the device (a X360 wired controller for example). This identifier is platform dependent.

In order to use these functions, [SdlInit](../Reference/Neslib.Sdl3.Basics/routines/SdlInit.md) must have been called with the [TSdlInitFlag.Joystick](../Reference/Neslib.Sdl3.Basics/types/TSdlInitFlag.md) flag. This causes SDL to scan the system for joysticks, and load appropriate drivers.

If you would like to receive joystick updates while the application is in the background, you should set the following hint before calling SdlInit: [TSdlHints.JoystickAllowBackgroundEvents](../Reference/Neslib.Sdl3.Basics/classes/TSdlHints.md/#JoystickAllowBackgroundEvents).

## Gamepad Support

SDL provides a low-level joystick API, which just treats joysticks as an arbitrary pile of buttons, axes, and hat switches. If you're planning to write your own control configuration screen, this can give you a lot of flexibility, but that's a lot of work, and most things that we consider "joysticks" now are actually console-style gamepads. So SDL provides the gamepad API on top of the lower-level joystick functionality.

The difference between a joystick and a gamepad is that a gamepad tells you *where* a button or axis is on the device. You don't speak to gamepads in terms of arbitrary numbers like "button 3" or "axis 2" but in standard locations: the d-pad, the shoulder buttons, triggers, A/B/X/Y (or X/O/Square/Triangle, if you will).

One turns a joystick into a gamepad by providing a magic configuration string, which tells SDL the details of a specific device: when you see this specific hardware, if button 2 gets pressed, this is actually D-Pad Up, etc.

SDL has many popular controllers configured out of the box, and users can add their own controller details through an environment variable if it's otherwise unknown to SDL.

In order to use these functions, [SdlInit](../Reference/Neslib.Sdl3.Basics/routines/SdlInit.md) must have been called with the [TSdlInitFlag.Gamepad](../Reference/Neslib.Sdl3.Basics/types/TSdlInitFlag.md) flag. This causes SDL to scan the system for gamepads, and load appropriate drivers.

If you would like to receive gamepad updates while the application is in the background, you should set the following hint before calling [SdlInit](../Reference/Neslib.Sdl3.Basics/routines/SdlInit.md): [TSdlHints.JoystickAllowBackgroundEvents](../Reference/Neslib.Sdl3.Basics/classes/TSdlHints.md/#JoystickAllowBackgroundEvents).

Gamepads support various optional features such as rumble, color LEDs, touchpad, gyro, etc. The support for these features varies depending on the controller and OS support available. You can check for LED and rumble capabilities at runtime by using [TSdlGamepad.Properties](../Reference/Neslib.Sdl3.Input/classes/TSdlGamepad.md/#Properties) and checking the various capability properties. You can check for touchpad by using [TSdlGamepad.NumTouchpads](../Reference/Neslib.Sdl3.Input/classes/TSdlGamepad.md/#NumTouchpads) and check for gyro and accelerometer by calling [TSdlGamepad.HasSensor](../Reference/Neslib.Sdl3.Input/classes/TSdlGamepad.md/#HasSensor).

By default SDL will try to use the most capable driver available, but you can tune which OS drivers to use with the various joystick hints in [TSdlHints](../Reference/Neslib.Sdl3.Basics/classes/TSdlHints.md).

Your application should always support gamepad hotplugging. On some platforms like Xbox, Steam Deck, etc., this is a requirement for certification. On other platforms, like macOS and Windows when using Windows.Gaming.Input, controllers may not be available at startup and will come in at some point after you've started processing events.

## Touch Support

SDL offers touch input, on platforms that support it. It can manage multiple touch devices and track multiple fingers on those devices.

Touches are mostly dealt with through the event system, in the `TSdlEventKind.FingerDown`, `TSdlEventKind.FingerMotion` and `TSdlEventKind.FingerUp` events, but there are also functions to query for hardware details, etc.

The touch system, by default, will also send virtual mouse events; this can be useful for making a some desktop apps work on a phone without significant changes. 

## Pen Support

SDL provides an API for pressure-sensitive pen (stylus and/or eraser) handling, e.g., for input and drawing tablets or suitably equipped mobile / tablet devices.

To get started with pens, simply handle `TSdlEventKind.Pen*` events. When a pen starts providing input, SDL will assign it a unique [TSdlPenID](../Reference/Neslib.Sdl3.Input/types/TSdlPenID.md), which will remain for the life of the process, as long as the pen stays connected.

Pens may provide more than simple touch input; they might have other axes, such as pressure, tilt, rotation, etc.

## Sensors

These APIs grant access to gyros and accelerometers on various platforms.

In order to use these functions, [SdlInit](../Reference/Neslib.Sdl3.Basics/routines/SdlInit.md) must have been called with the [TSdlInitFlag.Sensor](../Reference/Neslib.Sdl3.Basics/types/TSdlInitFlag.md) flag. This causes SDL to scan the system for sensors, and load appropriate drivers.
