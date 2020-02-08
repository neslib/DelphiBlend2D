program blFmxStroke;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMain in 'FMain.pas' {FormMain},
  FBase in '..\Common\FBase.pas' {FormBase},
  Blend2D in '..\..\..\Blend2D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
