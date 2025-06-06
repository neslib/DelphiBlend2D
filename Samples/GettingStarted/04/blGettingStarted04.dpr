program blGettingStarted04;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Math,
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

  { Rotate by 45 degrees about a point at [240, 240]. }
  Context.Rotate(DegToRad(45), 240, 240);

  { Create a pattern and use it to fill a rounded-rect. }
  var Pattern := TBLPattern.Create(Texture);

  Context.FillRoundRect(50, 50, 380, 380, 80.5, Pattern);
  Context.Finish;

  Image.WriteToFile('blGettingStarted04.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
