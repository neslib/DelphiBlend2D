program blGettingStarted06;

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
  Linear.AddStop(0.0, $FFFFFFFF);
  Linear.AddStop(0.5, $FFFF1F7F);
  Linear.AddStop(1.0, $FF1F7FFF);

  var Path: TBLPath;
  Path.MoveTo(119, 49);
  Path.CubicTo(259, 29, 99, 279, 275, 267);
  Path.CubicTo(537, 245, 300, -170, 274, 430);

  { Change stroke options. }
  Context.StrokeWidth := 15;
  Context.StrokeStartCap := TBLStrokeCap.Round;
  Context.StrokeEndCap := TBLStrokeCap.Butt;
  Context.StrokePath(Path, Linear);

  Context.Finish;

  Image.WriteToFile('blGettingStarted06.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
