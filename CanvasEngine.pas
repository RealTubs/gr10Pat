unit CanvasEngine;

interface

uses SysUtils, Classes, Forms, Unit4, uLkJSON, Dialogs, Controls, ExtCtrls,
  Windows, Graphics;

type
  TVector = class
    x, y: Integer;
  end;

  TPosition = class(TVector)
    constructor Create(AJson: TlkJSONobject);
  end;

  TSize = record
    height, width: Integer;
  end;

  TCanvasObject = class
    fId, ftype, fColor: string;
    fSize: TSize;
    fPosition: TPosition;
    fRaduis: Integer;
    constructor Create(AJson: TlkJSONobject);
  end;

  TCanvasEngine = class
  private
    fPrevTime: Int64;
    fCanvasData: TlkJSONobject;
    fCanvasObjects: TList;
    fCanvas: TCanvas;
    fImage: TImage;
  public
    procedure Enable;
    procedure Disable;
    procedure RenderCanvas;
    procedure ClearCanvas;
    procedure loadCanvasData(ACanvasData: TlkJSONobject);
    procedure CanvasUpdate(Sender: Tobject; var Done: Boolean);
    constructor Create;
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

constructor TCanvasObject.Create(AJson: TlkJSONobject);
begin
  fId := AJson.getString('id');
  ftype := AJson.getString('type');
  fColor := AJson.getString('color');
  fPosition := TPosition.Create(AJson.Field['position'] as TlkJSONobject);
  if ftype = 'circle' then
    fRaduis := TlkJSONobject(AJson.Field['size']).getInt('radius')
  else
  begin
    fSize.height := TlkJSONobject(AJson.Field['size']).getInt('height');
    fSize.width := TlkJSONobject(AJson.Field['size']).getInt('width');
  end;

end;

constructor TPosition.Create(AJson: TlkJSONobject);
begin
  x := AJson.getInt('x');
  y := AJson.getInt('y');
end;

procedure TCanvasEngine.loadCanvasData(ACanvasData: TlkJSONobject);
var
  initObjects: TlkJSONlist;
  initObject: TlkJSONobject;
  I: Integer;
begin
  fCanvasData := ACanvasData;
  fCanvasObjects.Clear;
  initObjects := fCanvasData.Field['initObjects'] as TlkJSONlist;
  for I := 0 to initObjects.Count - 1 do
  begin
    initObject := initObjects.Child[I] as TlkJSONobject;
    fCanvasObjects.Add(TCanvasObject.Create(initObject));
  end;
end;

procedure TCanvasEngine.ClearCanvas;
begin
  fCanvas.Brush.Color := MainForm.Color;
  fCanvas.FillRect(fImage.ClientRect);
end;

procedure TCanvasEngine.RenderCanvas;
var
  LCanvasObject: TCanvasObject;
  LPos: TPosition;
  LSize: TSize;
  I: Integer;
begin
  for I := 0 to fCanvasObjects.Count - 1 do
  begin
    LCanvasObject := fCanvasObjects[I];
    LPos := LCanvasObject.fPosition;
    LSize := LCanvasObject.fSize;
    fCanvas.Brush.Color := RGBtoColor(LCanvasObject.fColor);
    fCanvas.FillRect(Rect(LPos.x, LPos.y, LSize.width + LPos.x,
        LSize.height + LPos.y));
  end;
end;

constructor TCanvasEngine.Create;
begin
  fCanvasObjects := TList.Create;
  fImage := TImage.Create(MainForm);
  fImage.Parent := MainForm;
  fImage.Align := alClient;
  fImage.Name := 'imgCanvas';
  fCanvas := fImage.Canvas;
end;

procedure TCanvasEngine.Enable;
begin
  fPrevTime := GetTickCount;
  RenderCanvas;
  Application.OnIdle := CanvasUpdate;
end;

procedure TCanvasEngine.Disable;
begin
  Application.OnIdle := nil;
  ClearCanvas;
end;

procedure TCanvasEngine.CanvasUpdate(Sender: Tobject; var Done: Boolean);
var
  CurrentTime: Int64;
  DeltaTime: Real;
begin
  CurrentTime := GetTickCount;
  DeltaTime := (fPrevTime - CurrentTime) / 1000;
  fPrevTime := CurrentTime;
  // ClearCanvas;
  // RenderCanvas;
end;

end.
