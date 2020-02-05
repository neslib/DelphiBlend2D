program blGettingStarted02;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend4D in '..\..\..\Blend4D.pas';

procedure Run;
var
  Image: IBLImage;
  Context: IBLContext;
  Linear: IBLGradient;
  Codec: IBLImageCodec;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  Context := TBLContext.Create(Image);
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  { Coordinates can be specified now or changed later. }
  Linear := TBLGradient.Create(BLLinearGradientValues(0, 0, 0, 480));

  { Color stops can be added in any order. }
  Linear.AddStop(0.0, BLRgba32($FFFFFFFF));
  Linear.AddStop(0.5, BLRgba32($FF5FAFDF));
  Linear.AddStop(1.0, BLRgba32($FF2F5FDF));

  { SetFillStyle can be used for both colors and styles. }
  Context.SetFillStyle(Linear);

  Context.CompOp := TBLCompOp.SrcOver;
  Context.FillRoundRect(40, 40, 400, 400, 45.5);
  Context.Finish;

  Codec := TBLImageCodec.Create;
  if (Codec.FindByName('BMP')) then
    Image.WriteToFile('blGettingStarted02.bmp', Codec);
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
