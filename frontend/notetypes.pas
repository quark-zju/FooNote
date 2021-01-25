unit NoteTypes;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils;

type
  NoteBackendId = Int32;
  NodeId = Int32;
  NodeMtime = Int32;

  FullId = record
    Id: NodeId;
    BackendId: NoteBackendId;
    constructor Create(B: NoteBackendId; I: NodeId);
    class operator < (X, Y: FullId): boolean;
    class operator > (X, Y: FullId): boolean;
    class operator = (X, Y: FullId): boolean;
    function ToString(): string;
  end;
  VecFullId = array of FullId;

  FullIdText = record
    Id: FullId;
    Text: string;
  end;
  VecFullIdText = array of FullIdText;

implementation

constructor FullId.Create(B: NoteBackendId; I: NodeId);
begin
  Id := I;
  BackendId := B;
end;

class operator FullId. < (X, Y: FullId): boolean;
begin
  Result := (X.BackendId < Y.BackendId) or ((X.BackendId = Y.BackendId) and (X.Id < Y.Id));
end;

class operator FullId. > (X, Y: FullId): boolean;
begin
  Result := (X.BackendId > Y.BackendId) or ((X.BackendId = Y.BackendId) and (X.Id > Y.Id));
end;

class operator FullId. = (X, Y: FullId): boolean;
begin
  Result := (X.BackendId = Y.BackendId) and (X.Id = Y.Id);
end;

function FullId.ToString: string;
begin
  Result := Format('%d:%d', [BackendId, Id]);
end;


end.

