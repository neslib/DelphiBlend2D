program blGettingStarted03;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
var
  Image, Texture: IBLImage;
  Context: IBLContext;
  Pattern: IBLPattern;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  Context := TBLContext.Create(Image);
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  { Read an image from file. }
  Texture := TBLImage.Create;
  Texture.ReadFromFile('Resources/texture.jpeg');

  { Create a pattern and use it to fill a rounded-rect. }
  Pattern := TBLPattern.Create(Texture);

  Context.CompOp := TBLCompOp.SrcOver;
  Context.SetFillStyle(Pattern);
  Context.FillRoundRect(40, 40, 400, 400, 45.5);
  Context.Finish;

  Image.WriteToFile('blGettingStarted03.bmp');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
