# Neslib.Sdl3.Haptic

!!! abstract "Reference"

    [Neslib.Sdl3.Haptic](../Reference/Neslib.Sdl3.Haptic/index.md)

The SDL haptic subsystem manages haptic (force feedback) devices.

The basic usage is as follows:

- Initialize the subsystem ([TSdlInitFlag.Haptic](../Reference/Neslib.Sdl3.Basics/types/TSdlInitFlag.md)).
- Open a haptic device.
- [TSdlHaptic.Open](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#Open) to open from index.
- [TSdlHaptic.OpenFromJoystick](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#OpenFromJoystick) to open from an existing joystick.
- Create an effect ([TSdlHapticEffect](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHapticEffect.md)).
- Upload the effect with [TSdlHaptic.CreateEffect](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#CreateEffect).
- Run the effect with [TSdlHaptic.RunEffect](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#RunEffect).
- (Optionally) free the effect with [TSdlHaptic.FreeEffect](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#FreeEffect) (it is also automatically freed when the haptic device is closed).
- Close the haptic device with [TSdlHaptic.Close](../Reference/Neslib.Sdl3.Haptic/classes/TSdlHaptic.md/#Close).

Simple rumble example:

```delphi
{ Open the device }
var Devices := TSdlHapticID.Devices;
if (Devices = nil) then
  Exit;
  
var Haptic := TSdlHaptic.Open(Devices[0]);
try
  { Initialize simple rumble }
  Haptic.InitRumble;
  
  { Play effect at 50% strength for 2 seconds }
  Haptic.PlayRumble(0.5, 2000);
  
  SdlDelay(2000);
finally
  Haptic.Close;
end;
```

Complete example:

```delphi
function TestHaptic(const AJoystick: TSdlJoystick)
begin
  { Open the device }
  var Haptic := TSdlHaptic.OpenFromJoystick(AJoystick);
  try
    { See if it can do sine waves }
    if (not (TSdlHapticFeature.Sine in Haptic.Features)) then
      Exit(False);
      
    { Create the effect }
    var Effect: TSdlHapticEffect;
    FillChar(Effect, SizeOf(Effect), 0);
    Effect.Kind := TSdlHapticKind.Sine;
    
    { Use polar coordinates }
    Effect.Periodic.Direction.Kind := TSdlHapticDirectionKind.Polar;
    
    { Force comes from south }
    Effect.Periodic.Direction.Dir[0] := 18000;
    
    { Repeats every 1 second }
    Effect.Periodic.Period := 1000; 
    
    { 20000/32767 strength }
    Effect.Periodic.Magnitude := 20000; 
    
    { 5 seconds long }
    Effect.Periodic.Length := 5000; 
    
    { Takes 1 second to get max strength }
    Effect.Periodic.AttackLength := 1000; 
    
    { Takes 1 second to fade away }
    Effect.Periodic.FadeLength := 1000; 
    
    { Upload the effect }
    var EffectID := Haptic.CreateEffect(Effect);
    
    { Test the effect }
    Haptic.RunEffect(EffectID, 1);
         
    { Wait for the effect to finish }
    SdlDelay(5000);
    
    { We destroy the effect, although closing
      the device also does this. }
    Haptic.FreeEffect(EffectID);
  finally
    { Close the device }
    Haptic.Close;
  end;
  
  { Success }
  Result := True;
end;
```

Note that the SDL haptic subsystem is not thread-safe.
