unit Unit6;

interface

uses
  SysUtils, Classes, Forms, UNit4,uklJson;

type
  TControlLoader = class
  public
  constructor Create(Json:TlkJson); override;
  end;
implementation

end.
