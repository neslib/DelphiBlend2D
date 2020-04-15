program blGettingStarted04;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
var
  Image, Texture: IBLImage;
  Pattern: IBLPattern;
  Context: IBLContext;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  Context := TBLContext.Create(Image);
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  { Read an image from file. }
  Texture := TBLImage.Create;
  Texture.ReadFromFile('Resources/texture.jpeg');

  { Rotate by 45 degrees about a point at [240, 240]. }
  Context.Rotate(0.785398, 240, 240);

  { Create a pattern. }
  Pattern := TBLPattern.Create(Texture);

  Context.CompOp := TBLCompOp.SrcOver;
  Context.SetFillStyle(Pattern);
  Context.FillRoundRect(50, 50, 380, 380, 80.5);
  Context.Finish;

  Image.WriteToFile('blGettingStarted04.bmp');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
