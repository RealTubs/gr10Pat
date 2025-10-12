unit LayoutLoader;

interface

uses
  SysUtils, Classes, Forms, uLkJSON, AppController, Graphics, Windows,
  StdCtrls, ExtCtrls, Messages, Dialogs, Controls, Unit4;

type

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

function RGBtoColor(RGBstring: string): TColor;
var
  HexValue: Integer;
  R, G, B: Byte;
begin
  HexValue := StrToInt('$' + copy(RGBstring, 2, 6));
  R := (HexValue shr 16) and $FF;
  G := (HexValue shr 8) and $FF;
  B := HexValue and $FF;
  Result := RGB(R, G, B);
end;

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
  I: Integer;
  Screen, ControlJSON, Screens, Properties: TlkJSONobject;
  ControlType: TControlType;
  ControlList: TlkJSONlist;
  LControl: TControl;
begin
  Screens := FLayout.Field['screens'] as TlkJSONobject;
  Screen := Screens.Field[AScreenName] as TlkJSONobject;
  ControlList := Screen.Field['controls'] as TlkJSONlist;
  FController.LoadScreenLogic(Screen);

  TForm4(AParent).clearScreen;

  for I := 0 to ControlList.Count - 1 do
  begin
    ControlJSON := ControlList.Child[I] as TlkJSONobject;
    Properties := ControlJSON.Field['properties'] as TlkJSONobject;
    ControlType := getControlType(ControlJSON.getString('type'));
    LControl := TForm4(AParent).createControl(ControlType,
      ControlJSON.getString('name'), Properties.getString('caption'),
      Properties.getInt('width'), Properties.getInt('height'),
      Properties.getInt('left'), Properties.getInt('top'));
    if (Properties.Field['align'] <> nil) and
      (Properties.getString('alClient') = 'alClient') then
      TImage(LControl).Align := alClient;

    if ControlJSON.Field['font'] <> nil then
      TLabel(LControl).font.Color := RGBtoColor
        (TlkJSONobject(ControlJSON.Field['font']).getString('color'));

    if ControlJSON.Field['action'] <> nil then
      LControl.Hint := ControlJSON.getString('action');
  end;
end;

end.
