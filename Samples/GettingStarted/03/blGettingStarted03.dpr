program blGettingStarted03;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
begin
  ReportMemoryLeaksOnShutdown := True;
  var Image := TBLImage.Create(480, 480, TBLFormat.Prgb32);
  var Context := TBLContext.Create(Image);

  Context.ClearAll;

  { Read an image from file. }
  var Texture: TBLImage;
  Texture.ReadFromFile('Resources/Leaves.jpeg');

  { Create a pattern and use it to fill a rounded-rect.
    By default a repeat extend mode is used, but it can be configured to use
    more extend modes. }
  var Pattern := TBLPattern.Create(Texture);

  Context.FillRoundRect(40, 40, 400, 400, 45.5, Pattern);
  Context.Finish;

  Image.WriteToFile('blGettingStarted03.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
