program blGettingStarted05;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
var
  Image: IBLImage;
  Context: IBLContext;
  Radial, Linear: IBLGradient;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  Context := TBLContext.Create(Image);
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  { First shape filled by a radial gradient. }
  Radial := TBLGradient.Create(BLRadialGradientValues(180, 180, 180, 180, 180));
  Radial.AddStop(0.0, BLRgba32($FFFFFFFF));
  Radial.AddStop(1.0, BLRgba32($FFFF6F3F));

  Context.CompOp := TBLCompOp.SrcOver;
  Context.SetFillStyle(Radial);
  Context.FillCircle(180, 180, 160);

  { Second shape filled by a linear gradient. }
  Linear := TBLGradient.Create(BLLinearGradientValues(195, 195, 470, 470));
  Linear.AddStop(0.0, BLRgba32($FFFFFFFF));
  Linear.AddStop(1.0, BLRgba32($FF3F9FFF));

  Context.CompOp := TBLCompOp.Difference;
  Context.SetFillStyle(Linear);
  Context.FillRoundRect(195, 195, 270, 270, 25);

  Context.Finish;

  Image.WriteToFile('blGettingStarted05.bmp');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
