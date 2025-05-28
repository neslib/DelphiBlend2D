program blFmxCanvas;

uses
//  FastMM4,
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FMain in 'FMain.pas' {FormMain},
  Blend2D.Fmx in '..\..\..\Blend2D.Fmx.pas';

{$R *.res}

begin
  GlobalUseBlend2D := True;
//  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
