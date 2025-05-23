program blGettingStarted07;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.UITypes,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
begin
  ReportMemoryLeaksOnShutdown := True;
  var Image := TBLImage.Create(480, 480, TBLFormat.Prgb32);
  var Context := TBLContext.Create(Image);

  Context.ClearAll;

  var Face: TBLFontFace;
  Face.MakeFromFile('Resources/ABeeZee-Regular.ttf');

  var Font: TBLFont;
  Font.MakeFromFace(Face, 50);

  Context.SetFillStyle($FF000000);
  Context.FillText(BLPoint(60, 80), Font, 'Hello Blend2D!');

  Context.Rotate(0.785398);
  Context.FillText(BLPoint(250, 80), Font, 'Rotated Text');

  Context.Finish;

  Image.WriteToFile('blGettingStarted07.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
