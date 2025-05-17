program blGettingStarted05;

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

  { First shape filled with a radial gradient.
    By default, SrcOver composition is used. }
  var Radial := TBLGradient.Create(
    BLRadialGradientValues(180, 180, 180, 180, 180));
  Radial.AddStop(0.0, $FFFFFFFF);
  Radial.AddStop(1.0, $FFFF6F3F);
  Context.FillCircle(180, 180, 160, Radial);

  { Second shape filled by a linear gradient. }
  var Linear := TBLGradient.Create(
    BLLinearGradientValues(195, 195, 470, 470));
  Linear.AddStop(0.0, $FFFFFFFF);
  Linear.AddStop(1.0, $FF3F9FFF);

  { Use CompOp to change a composition operator. }
  Context.CompOp := TBLCompOp.Difference;
  Context.FillRoundRect(195, 195, 270, 270, 25, Linear);

  Context.Finish;

  Image.WriteToFile('blGettingStarted05.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
