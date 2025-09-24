unit LayoutLoader;

interface

uses
  SysUtils, Classes, Forms, uLkJSON, AppController, Graphics, Windows,
  StdCtrls, ExtCtrls, Messages, Dialogs, Controls, Unit4;

type
  // TControlType = (ctButton, ctLabel, ctEdit);

  TLayoutLoader = class
  private
    FLayout: TlkJSONobject;
    FController: TAppController;
  public
    constructor Create(Json: TlkJSONobject; AController: TAppController);
    destructor Destroy; override;
    procedure BuildScreen(AParent: TWinControl; AScreenName: string);
    function getControlType(ATypeName: string): TControlType;
  end;

implementation

constructor TLayoutLoader.Create(Json: TlkJSONobject;
  AController: TAppController);
begin
  FLayout := Json;
  FController := AController;
end;

destructor TLayoutLoader.Destroy;
begin
  FLayout.Free;
  inherited Destroy;
end;

function TLayoutLoader.getControlType(ATypeName: string): TControlType;
begin
  if ATypeName = 'ctButton' then
    Result := ctButton
  else if ATypeName = 'ctEdit' then
    Result := ctEdit
  else if ATypeName = 'ctLabel' then
    Result := ctLabel;
end;

procedure TLayoutLoader.BuildScreen(AParent: TWinControl; AScreenName: string);
var
  I: integer;
  Screen, ControlJSON, Screens, Properties: TlkJSONobject;
  ControlType: TControlType;
  ControlList: TlkJSONlist;
  Control: TControl;
begin
  Screens := FLayout.Field['Screens'] as TlkJSONobject;
  Screen := Screens.Field[AScreenName] as TlkJSONobject;
  ControlList := Screen.Field['controls'] as TlkJSONlist;

  TForm4(AParent).clearScreen;

  for I := 0 to ControlList.Count - 1 do
  begin
    ControlJSON := ControlJSON.Child[I] as TlkJSONobject;
    Properties := ControlJSON.Field['properties'] as TlkJSONobject;
    ControlType := getControlType(ControlJSON.getString('type'));
    TForm4(AParent).createControl(ControlType, ControlJSON.getString('name'),
      Properties.getString('Caption'), Properties.getInt('width'),
      Properties.getInt('height'), Properties.getInt('top'),
      Properties.getInt('left'));
  end;
  ShowMessage('BUilt');
end;

end.
