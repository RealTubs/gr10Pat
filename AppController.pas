unit AppController;

interface

uses
  SysUtils, Classes, Forms, Unit4, uLkJSON;

type
  TAppController = class
  private
    FMainForm: TForm4;
  public
    // procedure HandleControlAction(Sender: Tobject);
    constructor Create(MainForm: TForm4);
    // destructor Destroy; override;
    // procedure navigateTo(AScreenName: string);
  end;

implementation

uses
  LayoutLoader;

var
  LayoutLoader: TLayoutLoader;

constructor TAppController.Create(MainForm: TForm4);
begin
  inherited Create;
  FMainForm := MainForm;
  LayoutLoader := TLayoutLoader.Create
    (TlkJSon.ParseText('{}') as TlkJSONobject, self);
end;

end.
