# Neslib.Sdl3.Events

!!! abstract "Reference"

    [Neslib.Sdl3.Events](../Reference/Neslib.Sdl3.Events/index.md)

Event queue management.

It's extremely common--often required--that an app deal with SDL's event queue. Almost all useful information about interactions with the real world flow through here: the user interacting with the computer and app, hardware coming and going, the system changing in some way, etc.

An app generally takes a moment, perhaps at the start of a new frame, to examine any events that have occurred since the last time and process or ignore them. This is generally done by overriding [TSdlApp.Event](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Event) (which is called for each event, just before [TSdlApp.Iterate](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Iterate) is called).

There is other forms of control, too: [TSdlEvents.Peep](../Reference/Neslib.Sdl3.Events/classes/TSdlEvents.md/#Peep_0) has more functionality at the cost of more complexity, and [TSdlEvents.Wait](../Reference/Neslib.Sdl3.Events/classes/TSdlEvents.md/#Wait_0) can block the process until something interesting happens, which might be beneficial for certain types of programs on low-power hardware. One may also call [TSdlEvents.AddWatch](../Reference/Neslib.Sdl3.Events/classes/TSdlEvents.md/#AddWatch) to set a callback when new events arrive.

The app is free to generate their own events, too: [TSdlEvents.Push](../Reference/Neslib.Sdl3.Events/classes/TSdlEvents.md/#Push) allows the app to put events onto the queue for later retrieval; [TSdlEvents.Register](../Reference/Neslib.Sdl3.Events/classes/TSdlEvents.md/#Register) can guarantee that these events have a type that isn't in use by other parts of the system.
