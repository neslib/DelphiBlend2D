program blVclPaintBox;

uses
  Vcl.Forms,
  FMain in 'FMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
