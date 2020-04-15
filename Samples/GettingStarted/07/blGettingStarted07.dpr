program blGettingStarted07;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
var
  Image: IBLImage;
  Context: IBLContext;
  Face: IBLFontFace;
  Font: IBLFont;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  Context := TBLContext.Create(Image);
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  Face := TBLFontFace.Create;
  Face.InitializeFromFile('Resources/NotoSans-Regular.ttf');

  Font := TBLFont.Create;
  Font.InitializeFromFace(Face, 50);

  Context.FillColor := $FFFFFFFF;
  Context.FillText(BLPoint(60, 80), Font, 'Hello Blend2D!');

  Context.Rotate(0.785398);
  Context.FillText(BLPoint(250, 80), Font, 'Rotated Text');

  Context.Finish;

  Image.WriteToFile('blGettingStarted07.bmp');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
