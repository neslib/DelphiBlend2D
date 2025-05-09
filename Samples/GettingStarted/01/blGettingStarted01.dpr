program blGettingStarted01;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend2D in '..\..\..\Blend2D.pas';

procedure Run;
begin
  ReportMemoryLeaksOnShutdown := True;

  { Use constructor or `Make` method to allocate a new image data of the
    required format. }
  var Image := TBLImage.Create(480, 480, TBLFormat.Prgb32);

  { Attach a rendering context into Image. }
  var Context := TBLContext.Create(Image);

  { Clearing the image would make it transparent. }
  Context.ClearAll;

  { Create a path having cubic curves. }
  var Path: TBLPath;
  Path.MoveTo(26, 31);
  Path.CubicTo(642, 132, 587, -136, 25, 464);
  Path.CubicTo(882, 404, 144, 267, 27, 31);

  { Fill a path with opaque white - $AARRGGBB. }
  Context.FillPath(Path, $FFFFFFFF);

  { Detach the rendering context from `AImg`.}
  Context.Finish;

  { Let's use some built-in codecs provided by Blend2D. }
  Image.WriteToFile('bl_sample_1.png');
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
