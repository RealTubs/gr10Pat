unit AppController;

interface

uses
  SysUtils, Classes, Forms, Unit4, uLkJSON, Dialogs, Controls, Variants,
  StdCtrls;

type
  // TAppEngine = class 
  // procedure MainLoop(Sender: TObject); 
  // 
  // constructor Create; 
  // end; 

  TAppController = class
  private
    FMainForm: TForm4;
    FScreenLogic: TlkJSONobject;

  public
    procedure HandleControlAction(Sender: TObject);
    procedure LoadScreenLogic(AScreenLogic: TlkJSONobject);
    procedure ProcessHandler(AHandler: TlkJSONobject);
    procedure ExecuteAction(AAction: TlkJSONobject);
    procedure triggerEvent(AEvent: string);
    function CheckCondition(ACondition: TlkJSONobject): Boolean;
    constructor Create(MainForm: TForm4);
    // destructor Destroy; override; 
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
  // Application.OnIdle := Mainloop 
  LStringList := TStringList.Create;
  LStringList.LoadFromFile('test2.json');
  LayoutLoader := TLayoutLoader.Create(TlkJSon.ParseText(LStringList.text)
      as TlkJSONobject, self);
  LayoutLoader.BuildScreen(FMainForm, 'HomeScreen');
end;

function TAppController.CheckCondition(ACondition: TlkJSONobject): Boolean;
var
  Target: TControl;
  PropValue: Variant;
  TestValue, sOperator, sProperty: string;
begin
  Result := False;
  Target := FMainForm.FindChildControl(ACondition.getString('target'));
  sOperator := ACondition.getString('operator');
  TestValue := ACondition.getString('value');
  sProperty := ACondition.getString('property');

  if (sProperty = 'text') then
  begin
    PropValue := TEdit(Target).text;
    if sOperator = 'equals' then
      Result := (vartostr(PropValue) = TestValue)
    else if sOperator = 'notEquals' then
      Result := (vartostr(PropValue) <> TestValue)
  end
end;

procedure TAppController.ExecuteAction(AAction: TlkJSONobject);
var
  params: TlkJSONobject;
  I: Integer;
  Target: TControl;
  sProperty: string;
  PropValue: Variant;
begin
  params := AAction.Field['params'] as TlkJSONobject;
  if AAction.getString('type') = 'triggerEvent' then
  begin
    for I := 0 to TlkJSONlist(params.Field['events']).Count - 1 do
    begin
      triggerEvent(TlkJSONlist(params.Field['events']).getString(I));
    end;
  end
  else if AAction.getString('type') = 'navigate' then
  begin
    LayoutLoader.BuildScreen(FMainForm, params.getString('screen'));
  end
  else if AAction.getString('type') = 'updateProperty' then
  begin
    for I := 0 to FMainForm.ControlCount - 1 do
    begin
      if FMainForm.Controls[I].Name = params.getString('target') then
        Target := FMainForm.Controls[I];
    end;
    sProperty := params.getString('property');
    PropValue := params.Field['value'].Value;
    if sProperty = 'caption' then
    begin
      TLabel(Target).Caption := PropValue;
    end
    else if sProperty = 'visible' then
    begin
      TLabel(Target).Visible := PropValue;
    end
    else if sProperty = 'enabled' then
    begin
      TEdit(Target).Enabled := PropValue;
    end;
  end;
end;

procedure TAppController.triggerEvent(AEvent: string);
var
  Events: TlkJSONobject;
  ExecuteList: TlkJSONlist;
  I: Integer;
begin
  Events := FScreenLogic.Field['events'] as TlkJSONobject;
  ExecuteList := TlkJSONobject(Events.Field[AEvent]).Field['execute']
    as TlkJSONlist;
  for I := 0 to ExecuteList.Count - 1 do
  begin
    ExecuteAction(ExecuteList.Child[I] as TlkJSONobject);
  end;
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

procedure TAppController.HandleControlAction(Sender: TObject);
var
  Action: string;
  Handlers: TlkJSONlist;
  I: Integer;
begin
  Action := TControl(Sender).Hint;
  Handlers := FScreenLogic.Field['handlers'] as TlkJSONlist;
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
