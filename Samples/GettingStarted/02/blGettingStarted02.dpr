program blGettingStarted02;

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

  var Linear := TBLGradient.Create(
    BLLinearGradientValues(0, 0, 0, 480));

  { Coordinates can be specified now or changed later via TBLGradient accessors. }
  Linear := TBLGradient.Create(BLLinearGradientValues(0, 0, 0, 480));

  { Color stops can be added in any order. }
  Linear.AddStop(0.0, $FFFFFFFF);
  Linear.AddStop(0.5, $FF5FAFDF);
  Linear.AddStop(1.0, $FF2F5FDF);

  { SetFillStyle can be used for both colors and styles. Alternatively, a color
    or style can be passed explicitly to a render function. }
  Context.SetFillStyle(Linear);

  { Rounded rect will be filled with the linear gradient. }
  Context.FillRoundRect(40, 40, 400, 400, 45.5);
  Context.Finish;

  Image.WriteToFile('blGettingStarted02.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
