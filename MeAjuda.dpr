program MeAjuda;

uses
  System.StartUpCopy,
  FMX.Forms,
  untLogin in 'untLogin.pas' {Form1},
  untMenu in 'untMenu.pas' {Form2},
  untFormulario in 'untFormulario.pas' {Form3},
  untMap in 'untMap.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
//  Application.CreateForm(TForm1, Form1);
//  Application.CreateForm(TForm2, Form2);
//  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
