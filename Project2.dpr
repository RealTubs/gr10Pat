program Project2;

uses
  Forms,
  Unit4 in 'Unit4.pas' { Form4 },
  AppController in 'AppController.pas',
  LayoutLoader in 'LayoutLoader.pas',
  CanvasEngine in 'CanvasEngine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, MainForm);
  Application.Run;

end.
