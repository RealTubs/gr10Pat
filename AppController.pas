unit AppController;

interface

uses
  SysUtils, Classes, Forms, Unit4, uLkJSON, Dialogs, Controls, Variants,
  StdCtrls;

type
  TAppController = class
  private
    FMainForm: TForm4;
    FScreenLogic: TlkJSONobject;

  public
    procedure HandleControlAction(Sender: Tobject);
    procedure LoadScreenLogic(AScreenLogic: TlkJSONobject);
    procedure ProcessHandler(AHandler: TlkJSONobject);
    procedure ExecuteAction(AAction: TlkJSONobject);
    function CheckCondition(ACondition: TlkJSONobject): Boolean;
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
var
  LStringList: TStringList;
begin
  inherited Create;
  FMainForm := MainForm;
  LStringList := TStringList.Create;
  LStringList.LoadFromFile('test2.json');
  LayoutLoader := TLayoutLoader.Create(TlkJSon.ParseText(LStringList.text)
      as TlkJSONobject, self);
  LayoutLoader.BuildScreen(MainForm, 'TestScreen');
end;

function TAppController.CheckCondition(ACondition: TlkJSONobject): Boolean;
var
  Target: TControl;
  PropValue: Variant;
  TestValue, sOperator, sProperty: string;
begin
  Result := False;
  Target := MainForm.FindChildControl(ACondition.getString('target'));
  sOperator := ACondition.getString('operator');
  TestValue := ACondition.getString('value');
  sProperty := ACondition.getString('property');

  if (sProperty = 'text') then
  begin
    PropValue := TEdit(Target).text;
    if sOperator = 'equals' then
      Result := (vartostr(PropValue) = TestValue)
  end
end;

procedure TAppController.ExecuteAction(AAction: TlkJSONobject);
begin
  ShowMessage('Action');
end;

procedure TAppController.ProcessHandler(AHandler: TlkJSONobject);
var
  I: Integer;
  ConditionsMet: Boolean;
  ExecuteList, Conditions: TlkJSONlist;
begin
  ConditionsMet := true;
  Conditions := AHandler.Field['conditions'] as TlkJSONlist;
  for I := 0 to Conditions.Count - 1 do
  begin
    if not CheckCondition(Conditions.Child[I] as TlkJSONobject) then
    begin
      ConditionsMet := False;
      Break;
    end;
  end;

  if ConditionsMet then
  begin
    ExecuteList := AHandler.Field['execute'] as TlkJSONlist;
    for I := 0 to ExecuteList.Count - 1 do
    begin
      ExecuteAction(ExecuteList.Child[I] as TlkJSONobject)
    end;
  end;
end;

procedure TAppController.HandleControlAction(Sender: Tobject);
var
  Action: string;
  Handlers: TlkJSONlist;
  I: Integer;
begin
  Action := TControl(Sender).Hint;
  Handlers := FScreenLogic.Field['Handlers'] as TlkJSONlist;
  for I := 0 to Handlers.Count - 1 do
  begin
    if TlkJSONobject(Handlers.Child[I]).getString('trigger') = Action then
      ProcessHandler(Handlers.Child[I] as TlkJSONobject);
  end;
end;

procedure TAppController.LoadScreenLogic(AScreenLogic: TlkJSONobject);
begin
  FScreenLogic := AScreenLogic;
end;

end.
