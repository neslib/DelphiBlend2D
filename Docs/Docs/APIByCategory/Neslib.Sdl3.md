# Neslib.Sdl3

!!! abstract "Reference"

    [Neslib.Sdl3](../Reference/Neslib.Sdl3/index.md)

This unit defines the [TSdlApp](../Reference/Neslib.Sdl3/classes/TSdlApp.md) class, which is the entry point for any SDL application.

You need to derive your own application class from this class, and then run that application in your project file by calling [RunApp](../Reference/Neslib.Sdl3/routines/RunApp.md).

In your application class, you can override up to 5 methods to customize your application:

* [Init](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Init): This method is called by SDL once, at startup. The method should initialize whatever is necessary, possibly create windows and open audio devices, etc.
* [Iterate](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Iterate): This method is called repeatedly by SDL. The method should operate as a single iteration the program's primary loop; it should update whatever state it needs and draw a new frame of video, usually.
* [Event](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Event): This method is called as needed by SDL. It is called once for each new event.
* [Quit](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#Quit): This method is called once by SDL before terminating the program.
* [HandleError](../Reference/Neslib.Sdl3/classes/TSdlApp.md/#HandleError): This method is called when an SDL API returned an error. By default, it raises an exception (of type `ESdlError`) but you can override it to perform some other action, such as logging the error instead.

Below follows an example taken from the Clear example project:

```Delphi
program Clear;

{$R *.res}

uses
  System.SysUtils,
  Neslib.Sdl3.Api,
  Neslib.Sdl3.Time,
  Neslib.Sdl3.Basics,
  Neslib.Sdl3.Video,
  Neslib.Sdl3;

type
  TApp = class(TSdlApp)
  private
    FWindow: TSdlWindow;
    FRenderer: TSdlRenderer;
  protected
    function Init: TSdlAppResult; override;
    function Iterate: TSdlAppResult; override;
  end;

{ TApp }

function TApp.Init: TSdlAppResult;
{ This function runs once at startup }
begin
  ReportMemoryLeaksOnShutdown := True;

  SdlSetAppMetadata('Example Renderer Clear', '1.0', 'com.example.renderer-clear');

  SdlInit([TSdlInitFlag.Video]);

  FRenderer := TSdlRenderer.Create('Examples/Renderer/Clear', 640, 480, [], FWindow);

  { Carry on with the program! }
  Result := TSdlAppResult.Continue;
end;

function TApp.Iterate: TSdlAppResult;
{ This method runs once per frame, and is the heart of the program. }
begin
  { Convert from milliseconds to seconds. }
  var Now: Double := SdlGetTicks / 1000;

  { Choose the color for the frame we will draw.
    The sine wave trick makes it fade between colors smoothly. }
  var Red: Single := 0.5 + 0.5 * Sin(Now);
  var Green: Single := 0.5 + 0.5 * Sin(Now + PI * 2 / 3);
  var Blue: Single := 0.5 + 0.5 * Sin(Now + PI * 4 / 3);

  { New color, full alpha. }
  FRenderer.SetDrawColorFloat(Red, Green, Blue);

  { Clear the window to the draw color.  }
  FRenderer.Clear;

  { Put the newly-cleared rendering on the screen. }
  FRenderer.Present;

  { Carry on with the program! }
  Result := TSdlAppResult.Continue;
end;

begin
  RunApp(TApp);
end.
```
