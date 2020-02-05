program blGettingStarted01;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Blend4D in '..\..\..\Blend4D.pas';

procedure Run;
var
  Image: IBLImage;
  Context: IBLContext;
  Path: IBLPath;
  Codec: IBLImageCodec;
begin
  ReportMemoryLeaksOnShutdown := True;
  Image := TBLImage.Create(480, 480);

  { Attach a rendering context into Image. }
  Context := TBLContext.Create(Image);

  { Clear the image. }
  Context.CompOp := TBLCompOp.SrcCopy;
  Context.FillAll;

  { Fill some path. }
  Path := TBLPath.Create;
  Path.MoveTo(26, 31);
  Path.CubicTo(642, 132, 587, -136, 25, 464);
  Path.CubicTo(882, 404, 144, 267, 27, 31);

  Context.CompOp := TBLCompOp.SrcOver;
  Context.FillColor := $FFFFFFFF;
  Context.FillPath(Path);

  { Detach the rendering context from Image. }
  Context.Finish;

  { Let's use some built-in codecs provided by Blend2D. }
  Codec := TBLImageCodec.Create;
  if (Codec.FindByName('BMP')) then
    Image.WriteToFile('blGettingStarted01.bmp', Codec);
end;

begin
  try
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
