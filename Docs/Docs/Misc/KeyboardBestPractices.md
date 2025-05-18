# Best keyboard practices in SDL3

## First things first

Keyboard input is a surprisingly complicated topic after you step outside of your usual country and language (and sometimes before you do, too).

Almost any game, no matter what approach it is taking, should probably offer a means for users to configure key bindings; not only will this solve concerns about what kind of keyboard a user is typing on, it will also make your game flexible to whatever is *most comfortable* for a user. This is not just a question of keyboard layouts, but being kind to those that don't have full motion of their hands and would benefit from moving keys to accessible locations that don't make obvious sense to an outside observer. At least offer a config file, if not a user interface; people you've never met will thank you for it!

## The four approaches

There are, as far as we can tell, four common ways that apps want to use keyboard input. Sometimes they want different approaches at different moments, too.

## The 101-Button Joystick

Many games just want to treat a keyboard not as a way to input text, but just as a joystick that has a *lot* of buttons. The well-known ["WASD" key pattern](https://en.wikipedia.org/wiki/WASD_keys) for FPS games is a fine example of this: you want the *physical* location of a key, regardless of what symbol is printed on the key. After all, on a French keyboard, instead of "WASD", you'd press "ZQSD", and on a Hiragana keyboard "てちとし". Same locations on the keyboard, totally different symbols.

For these, you want [TSdlScancode](../Reference/Neslib.Sdl3.Input/types/TSdlScancode.md)s: these are guaranteed to reference the physical location on the keyboard and not what is printed on it.

Specifically, they assume a US English QWERTY keyboard layout, no matter what the keyboard in use actually has at that location, but that's okay because here we just want physical location of the key, not its meaning.

Simply grab the [TSdlKeyboardEvent.Scancode](../Reference/Neslib.Sdl3.Events/classes/TSdlKeyboardEvent.md/#Scancode) field from `TSdlEventKind.KeyDown` and `TSdlEventKind.KeyUp` events.

```delphi
{ Returns 1 if moving forward with this keypress, 
  -1 if moving backward, 0 if not moving. }
function DirectionUserShouldMove(const AEvent: TSdlEvent)
begin
  { Just checking key presses here... }
  Assert(AEvent.Kind = TSdlEventKind.KeyDown);
  if (AEvent.Key.Scancode = TSdlScancode.W) then
    { Pressed what would be "W" on a US QWERTY keyboard. Move forward! }
    Result := 1
  else if (AEvent.Key.Scancode = TSdlScancode.S) then
    { Pressed what would be "s" on a US QWERTY keyboard. Move backward! }
    Result := -1
  else
    { Wasn't key in W or S location, don't move. }
    Result := 0;
end;
```

## The Specific Key

Some games might want to know the *symbol* on a key. This tends to be a "press 'I' to open your inventory" thing, and you don't really care *where* the 'I' key is on the keyboard.

(But, again: offer keybindings, because some keyboards don't *have* an 'I' key!)

This is also useful for looking for the ESC key to cancel an operation, or Enter to confirm, etc; it doesn't matter where the key is, you still want *that* key.

These are [TSdlKeycode](../Reference/Neslib.Sdl3.Input/types/TSdlKeycode.md)s. They name specific keys and don't care *where* on the user's keyboard they actually are. Like scancodes, you also get these from `TSdlEventKind.KeyDown` and `TSdlEventKind.KeyUp` events.

```delphi
{ Sit in a loop forever until the user presses Escape. }
var QuitTheApp := False;
while (not QuitTheApp) do
begin
  var E: TSdlEvent;
  TSdlEvents.Wait(E);
  
  { User has pressed a key? }
  if (E.Kind = TSdlEventKind.KeyDown) then
  begin
    { The pressed key was Escape? }
    if (E.Key.Key = TSdlKeycode.Escape) then
      QuitTheApp := True;
  end;
end;
```

## The Chat Box

Unicode is hard! If you are composing text a string at a time ("Enter your name, adventurer!" screens or accepting sentences for a chat interface, etc) you should *not* be using key press events! This will *never* do the right thing across various keyboards and human languages around the world.

One should instead call [TSdlWindow.StartTextInput](../Reference/Neslib.Sdl3.Video/classes/TSdlWindow.md/#StartTextInput_1), and listen for `TSdlEventKind.TextInput` events. When done accepting input, call [TSdlWindow.StopTextInput](../Reference/Neslib.Sdl3.Video/classes/TSdlWindow.md/#StopTextInput). This approach will let the system provide input interfaces that are familiar to the user (including popping up a virtual keyboard on mobile devices, and other UI for composing in various languages). Then the event will provide Unicode strings, which might be complete lines of text or a single character, depending on the system. **You will not be able to replicate these interfaces in your application for everyone in the world**, do not try to build this yourself on top of individual keypress events.

The downside of this is that a virtual keyboard, etc, might be disruptive to your game, so you need to design accordingly.

```delphi
{ Set the text area and start text input }
var TextInputComplete := False;
var Area := SdlRect(TextField.X, TextField.Y, TextField.W, TextField.H);
var Cursor := 0;
var Text := '';
Window.SetTextInputArea(Area, Cursor);
Window.StartTextInput;

while (not TextInputComplete) do
begin
  var E: TSdlEvent;
  while (TSdlEvents.Poll(E)) do
  begin
    { User has pressed a key? }
    if (E.Kind = TSdlEventKind.KeyDown) then
    begin
      { The key pressed was Escape or Return? }
      if (E.Key.Key = TSdlKeycode.Escape) or (E.Key.Key = TSdlKeyCode.Return) then
      begin
        Window.StopTextInput;
        TextInputComplete := True;
      end;
      
      { Handle arrow keys, etc. }
    end
    else if (E.Kind = TSdlEventKind.TextInput) then
      Text := Text + E.Text.Text;
  end;

  { Render the text, adjusting cursor to the offset 
    in pixels from the left edge of the textfield }
  ...

  { Update the text input area, adjusting the cursor 
    so IME UI shows up at the correct location. }
  Window.SetTextInputArea(Area, Cursor);
end;
```

If you're writing a fullscreen game, you might want to render IME UI yourself, so you'd set `TSdlHints.ImeImplementedUI` appropriately and handle the `TSdlEventKind.TextEditing` and `TSdlEventKind.TextEditingCandidates` events.

## The Text Editor

This is a special case, and if you think your game fits here, you should think very hard about how to change that before thinking about how to go this route, because often times you are just on a path to build The Chat Box, incorrectly, from scratch.

If you were writing an SDL frontend for [Vim](https://www.vim.org/), you would need to know what keypresses-plus-modifiers produce: hitting shift-Z twice produces a different result than pressing z twice, but also you want arrow keys to do the right thing on the numpad, unless NumLock is pressed, when they should produce numbers. On top of all this, it's difficult to correlate between keypress and proper text input events and untangle them to get correct results.

In this case, you would need to handle both key events and input events as above, in addition you might want to know what the modified keycode for the event is:

```delphi
var QuitTheApp := False;
while (not QuitTheApp) do
begin
  var E: TSdlEvent;
  TSdlEvents.Wait(E);
  
  { User has pressed a key? }
  if (E.Kind = TSdlEventKind.KeyDown) then
  begin
    { Was the pressed key '$', possibly generated by Shift+4 on a 
      US keyboard or the '$' key on the French keyboard? }
    var Keycode := TSdlKeycode.FromScancode(
      E.Key.Scancode, E.Key.Mods, False);
      
    if (Keycode = TSdlKeycode.Dollar) then
    begin
      { Show me the money! }
    end;
  end;
end;
```

Obviously there are many keys that *don't* generate a character, or characters that are composed by pressing multiple keys (or navigating through IME interfaces that don't map to specific keypresses at all), so this is niche functionality and not how one should accept input in a general sense.

## Showing key names to users

So you've made your game, and now you're taking the original advice about adding user-configurable keybindings, and you need to know how to show the user the key's name in your config UI, and maybe also in a "press [current keybinding] to jump" tutorial message.

For this, use [TSdlKeycode.Name](../Reference/Neslib.Sdl3.Input/types/TSdlKeycode.md/#Name) with the [TSdlKeycode](../Reference/Neslib.Sdl3.Input/types/TSdlKeycode.md) you get from an event:

```delphi
var QuitTheApp := false;
while (not QuitTheApp) do
begin
  var E: TSdlEvent;
  TSdlEvents.Wait(E);

  { User has pressed a key? }
  if (E.Kind = TSdlEventKind.KeyDown) then
  begin
    TSdlLog.Info('Wow, you just pressed the %s key!', 
      [E.Key.Key.Name]);
  end;
end;
```

Note that [TSdlKeycode.Name](../Reference/Neslib.Sdl3.Input/types/TSdlKeycode.md/#Name) only returns uppercase characters, which is appropriate for showing a user a "press the button with this symbol on it" message.
