program Project2;

uses
  Forms,
  Unit4 in 'Unit4.pas' {Form4},
  Unit5 in 'Unit5.pas',
  Unit6 in 'Unit6.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
