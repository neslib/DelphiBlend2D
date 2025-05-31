program blFmxCanvas;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  FMain in 'FMain.pas' {FormMain},
  Blend2D.Fmx in '..\..\..\Blend2D.Fmx.pas',
  Blend2D.Fmx.TextLayout in '..\..\..\Blend2D.Fmx.TextLayout.pas';

{$R *.res}

begin
  GlobalUseBlend2D := True;
//  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
