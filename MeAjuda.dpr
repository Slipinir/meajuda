program MeAjuda;

uses
  System.StartUpCopy,
  FMX.Forms,
  untLogin in 'untLogin.pas' {Form1},
  untMenu in 'untMenu.pas' {Form2},
  untFormulario in 'untFormulario.pas' {Form3},
  untMap in 'untMap.pas' {FormMapa};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormMapa, FormMapa);
  Application.Run;
end.
