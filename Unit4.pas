unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TControlType = (ctButton, ctLabel, ctEdit);

  TForm4 = class(TForm)
    procedure FormCreate(Sender: TObject);
  public
    procedure clearScreen;
    procedure ControlAction(Sender: TObject);
    function createControl(AControlType: TControlType; AName, ACaption: string;
      AWidth, AHeight, ALeft, ATop: integer; ATag: integer = 1): TControl;
  end;

var
  MainForm: TForm4;

implementation

uses AppController;

var
  FAppController: TAppCOntroller;

procedure TForm4.ControlAction(Sender: TObject);
begin
  // FAppController.HandleControlAction(Sender);
end;

procedure TForm4.clearScreen;
var
  I: integer;
begin
  for I := ControlCount - 1 downto 0 do
  begin
    Controls[I].Free;
  end;
end;

function TForm4.createControl(AControlType: TControlType;
  AName, ACaption: string; AWidth, AHeight, ALeft, ATop: integer;
  ATag: integer = 1): TControl;
var
  LControl: TControl;
begin
  case AControlType of
    ctButton:
      begin
        LControl := TButton.Create(self);
        TButton(LControl).OnClick := ControlAction;
      end;
    ctLabel:
      LControl := TLabel.Create(self);
    ctEdit:
      LControl := TEdit.Create(self);
  end;
  LControl.Name := AName;
  LControl.Width := AWidth;
  LControl.Height := AHeight;
  LControl.Top := ATop;
  LControl.Left := ALeft;
  LControl.Tag := ATag;
  LControl.Parent := self;

  if LControl is (TEdit) then
    TEdit(LControl).Text := ACaption
  else
    TButton(LControl).Caption := ACaption;

  Result := LControl;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  ShowMessage('Hello world');
  FAppController := TAppCOntroller.Create(self);
end;
{$R *.dfm}

end.
