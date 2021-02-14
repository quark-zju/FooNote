unit NoteBackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NoteTypes, Math, StackFFI, LogFFI;

// Main APIs.

procedure CloseAll;
function Open(Url: string): FullId;
function UrlType(Url: string): string;
function GetRootId(): FullId;
function GetChildren(Id: FullId): VecFullId;
function GetParent(Id: FullId): FullId;
function GetAncestors(Id: FullId): VecFullId;
function GetRawMeta(Id: FullId): string;
function GetMtime(Id: FullId): NodeMtime;
function GetText(Id: FullId): string;
function GetTextFirstLine(Id: FullId): string;
function GetHeads(IdList: VecFullId): VecFullId;
function ExtractMeta(Id: FullId; Prefix: string): string;
function InsertNode(Id: FullId; Pos: integer; Text: string; meta: string; AutoFill: boolean = True): FullId;

function CopyToBytes(Ids: VecFullId): TBytes;
function PasteFromBytes(DestId: FullId; Pos: Int32; Bytes: TBytes): VecFullId;

function IsFolder(Id: FullId): boolean;

procedure StartSearch(Ids: VecFullId; Text: string);
procedure StopSearch();
function IsSearchComplete(): boolean;
function GetSearchResult(Skip: integer): VecFullIdText;
function GetSearchInput(): string;

// Return True on success.

function TryPersist(): boolean;
function TryPersistAsync(): boolean;
function TryPersistAsyncWait(var message: string; var errno: Int32): boolean;
function TryRemove(Id: FullId): boolean;
function TryRemove(Ids: VecFullId): boolean;
function TrySetRawMeta(Id: FullId; Meta: string): boolean;
function TryUpdateMeta(Id: FullId; Prefix, Value: string): boolean;
function TrySetParent(Id: FullId; Parent: FullId; Pos: Int32): boolean;
function TrySetParent(var Ids: VecFullId; Parent: FullId; Pos: Int32): boolean;
function TrySetText(Id: FullId; Text: string): boolean;


implementation

// Main FFI APIs. Their input and output are based on the thread-local stack.
// The return value is for error codes.

function notebackend_open_root_url(): Int32; cdecl; external 'notebackend';
function notebackend_type_of_url(): Int32; cdecl; external 'notebackend';
function notebackend_get_root_id(): Int32; cdecl; external 'notebackend';
function notebackend_get_children(): Int32; cdecl; external 'notebackend';
function notebackend_get_parent(): Int32; cdecl; external 'notebackend';
function notebackend_get_mtime(): Int32; cdecl; external 'notebackend';
function notebackend_get_text(): Int32; cdecl; external 'notebackend';
function notebackend_get_text_first_line(): Int32; cdecl; external 'notebackend';
function notebackend_get_raw_meta(): Int32; cdecl; external 'notebackend';
function notebackend_get_heads(): Int32; cdecl; external 'notebackend';
function notebackend_extract_meta(): Int32; cdecl; external 'notebackend';
function notebackend_insert(): Int32; cdecl; external 'notebackend';
function notebackend_autofill(): Int32; cdecl; external 'notebackend';
function notebackend_set_parent(): Int32; cdecl; external 'notebackend';
function notebackend_set_parent_batch(): Int32; cdecl; external 'notebackend';
function notebackend_set_text(): Int32; cdecl; external 'notebackend';
function notebackend_set_raw_meta(): Int32; cdecl; external 'notebackend';
function notebackend_update_meta(): Int32; cdecl; external 'notebackend';
function notebackend_remove(): Int32; cdecl; external 'notebackend';
function notebackend_remove_batch(): Int32; cdecl; external 'notebackend';
function notebackend_persist(): Int32; cdecl; external 'notebackend';
function notebackend_persist_async(): Int32; cdecl; external 'notebackend';
function notebackend_persist_try_wait(): Int32; cdecl; external 'notebackend';
function notebackend_copy(): Int32; cdecl; external 'notebackend';
function notebackend_paste(): Int32; cdecl; external 'notebackend';
function notebackend_search_start(): Int32; cdecl; external 'notebackend';
function notebackend_search_stop(): Int32; cdecl; external 'notebackend';
function notebackend_search_result(): Int32; cdecl; external 'notebackend';
function notebackend_search_is_complete(): Int32; cdecl; external 'notebackend';
function notebackend_search_input(): Int32; cdecl; external 'notebackend';
procedure notebackend_close_all(); cdecl; external 'notebackend';

// Main API implemenation by communicating using stack APIs.

function LogResultErrorMessage(ErrNo: integer; var ErrorMessage: string): integer;
var
  S: string;
begin
  if ErrNo = EWOULDBLOCK then begin
    // Can happen by TimerCheckSaveResult frequently. Log in TRACE level.
    if LogHasTrace then begin
      S := StackLastString();
      if not S.IsEmpty then begin
        ErrorMessage := S;
        LogTrace(Format('FFI Error: %s', [S]));
      end;
    end;
  end else if ErrNo <> OK then begin
    if LogHasError then begin
      S := StackLastString();
      if not S.IsEmpty then begin
        ErrorMessage := S;
        LogError(Format('FFI Error: %s', [S]));
      end;
    end;
  end;
  Result := ErrNo;
end;

function LogResultError(ErrNo: integer): integer;
var
  S: string;
begin
  Result := LogResultErrorMessage(ErrNo, S);
end;

procedure CloseAll;
begin
  notebackend_close_all;
end;

function Open(Url: string): FullId;
var
  S: string;
begin
  StackClear();
  StackPushString(Url);
  if LogResultErrorMessage(notebackend_open_root_url(), S) <> OK then begin
    raise EExternal.Create(Format('Open(%s) failed: %s', [Url, S]));
  end;
  Result := StackPopFullId();
end;

function UrlType(Url: string): string;
var
  S: string;
begin
  StackClear();
  StackPushString(Url);
  if LogResultErrorMessage(notebackend_type_of_url(), S) = OK then begin
    Result := StackPopString();
  end;
end;

function GetRootId(): FullId;
begin
  StackClear();
  if notebackend_get_root_id() <> OK then begin
    raise EExternal.Create('GetRootId() failed');
  end;
  Result := StackPopFullId();
end;

function GetChildren(Id: FullId): VecFullId;
var
  I, Length: Int32;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_children()) <> OK then begin
    raise EExternal.Create(Format('GetChildren(%s) failed', [Id.ToString()]));
  end;
  Length := StackPopInt();
  Result := VecFullId.Create;
  SetLength(Result, Length);
  for I := 1 to Length do begin
    Result[Length - I] := StackPopFullId();
  end;
end;

function GetParent(Id: FullId): FullId;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_parent()) <> OK then begin
    raise EExternal.Create(Format('GetParent(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopFullId();
end;

function GetAncestors(Id: FullId): VecFullId;
const
  MinCapacity: integer = 5;
var
  Parent: FullId;
  Count: integer;
begin
  SetLength(Result, MinCapacity);
  Count := 0;
  Parent := GetParent(Id);
  while Parent <> Id do begin
    Inc(Count);
    SetLength(Result, Max(MinCapacity, Count));
    Result[Count - 1] := Parent;
    Id := Parent;
    Parent := GetParent(Id);
  end;
  SetLength(Result, Count);
end;

function GetMtime(Id: FullId): NodeMtime;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_mtime()) <> OK then begin
    raise EExternal.Create(Format('GetMtime(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopInt();
end;

function GetText(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_text()) <> OK then begin
    raise EExternal.Create(Format('GetText(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetTextFirstLine(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_text_first_line()) <> OK then begin
    raise EExternal.Create(Format('GetTextFirstLine(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetRawMeta(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogResultError(notebackend_get_raw_meta()) <> OK then begin
    raise EExternal.Create(Format('GetMeta(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetHeads(IdList: VecFullId): VecFullId;
begin
  StackClear();
  StackPushFullIdList(IdList);
  if LogResultError(notebackend_get_heads()) <> OK then begin
    raise EExternal.Create('GetHeads() failed');
  end;
  Result := StackPopFullIdList();
end;


function ExtractMeta(Id: FullId; Prefix: string): string;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Prefix);
  if LogResultError(notebackend_extract_meta()) <> OK then begin
    raise EExternal.Create(Format('ExtractMeta(%s, %s) failed', [Id.ToString(), Prefix]));
  end;
  Result := StackPopString();
end;

function InsertNode(Id: FullId; Pos: integer; Text: string; meta: string; AutoFill: boolean = True): FullId;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushInt(Pos);
  StackPushString(Text);
  StackPushString(meta);
  if LogResultError(notebackend_insert()) <> OK then begin
    raise EExternal.Create(Format('InsertNode(%s, %d, %s, %s) failed', [Id.ToString(), Pos, Text, meta]));
  end;
  Result := StackPopFullId();
  if autofill then begin
    StackClear();
    StackPushFullId(Result);
    if LogResultError(notebackend_autofill()) <> OK then begin
      raise EExternal.Create(Format('Autofill(%s) failed', [Result.ToString()]));
    end;
  end;
end;

function CopyToBytes(Ids: VecFullId): TBytes;
begin
  StackClear();
  StackPushFullIdList(Ids);
  if LogResultError(notebackend_copy()) <> OK then begin
    raise EExternal.Create('CopyToBytes() failed');
  end;
  Result := StackPopBytes();
end;

function PasteFromBytes(DestId: FullId; Pos: Int32; Bytes: TBytes): VecFullId;
begin
  StackClear();
  StackPushFullId(DestId);
  StackPushInt(Pos);
  StackPushBytes(Bytes);
  if LogResultError(notebackend_paste()) <> OK then begin
    raise EExternal.Create('PasteFromBytes() failed');
  end;
  Result := StackPopFullIdList();
end;

function TrySetParent(Id: FullId; Parent: FullId; Pos: Int32): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushFullId(Parent);
  StackPushInt(Pos);
  Result := (LogResultError(notebackend_set_parent()) = OK);
end;

function TrySetParent(var Ids: VecFullId; Parent: FullId; Pos: Int32): boolean;
begin
  StackClear();
  StackPushFullIdList(Ids);
  StackPushFullId(Parent);
  StackPushInt(Pos);
  Result := (LogResultError(notebackend_set_parent_batch()) = OK);
  if Result then begin
    Ids := StackPopFullIdList();
  end;
end;


function TrySetText(Id: FullId; Text: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Text);
  Result := (LogResultError(notebackend_set_text()) = OK);
end;

function TrySetRawMeta(Id: FullId; Meta: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Meta);
  Result := (LogResultError(notebackend_set_raw_meta()) = OK);
end;

function TryUpdateMeta(Id: FullId; Prefix, Value: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Prefix);
  StackPushString(Value);
  Result := (LogResultError(notebackend_update_meta()) = OK);
end;

function TryRemove(Id: FullId): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  Result := (LogResultError(notebackend_remove()) = OK);
end;

function TryRemove(Ids: VecFullId): boolean;
begin
  StackClear();
  StackPushFullIdList(Ids);
  Result := (LogResultError(notebackend_remove_batch()) = OK);
end;

function TryPersist(): boolean;
begin
  StackClear();
  Result := (LogResultError(notebackend_persist()) = OK);
end;

function TryPersistAsync(): boolean;
begin
  StackClear();
  Result := (LogResultError(notebackend_persist_async()) = OK);
end;

function TryPersistAsyncWait(var message: string; var errno: Int32): boolean;
begin
  StackClear();
  Result := (LogResultError(notebackend_persist_try_wait()) = OK);
  if Result then begin
    errno := StackPopInt();
    message := StackPopString();
  end;
end;

function IsFolder(Id: FullId): boolean;
var
  S: string;
begin
  S := ExtractMeta(Id, 'type=');
  Result := (S = 'folder') or (S = 'trash') or (S = 'mount');
end;

procedure StartSearch(Ids: VecFullId; Text: string);
begin
  StackClear();
  StackPushString(Text);
  StackPushFullIdList(Ids);
  if LogResultError(notebackend_search_start()) <> OK then begin
    raise EExternal.Create('StartSearch() failed');
  end;
end;

procedure StopSearch();
begin
  StackClear();
  if LogResultError(notebackend_search_stop()) <> OK then begin
    raise EExternal.Create('StopSearch() failed');
  end;
end;

function IsSearchComplete(): boolean;
begin
  StackClear();
  if LogResultError(notebackend_search_is_complete()) <> OK then begin
    raise EExternal.Create('IsSearchComplete() failed');
  end;
  Result := StackPopInt() <> 0;
end;

function GetSearchResult(Skip: integer): VecFullIdText;
var
  T: FullIdText;
  N, I: integer;
begin
  StackClear();
  StackPushInt(Skip);
  if LogResultError(notebackend_search_result()) <> OK then begin
    raise EExternal.Create('GetSearchResult() failed');
  end;
  N := StackPopInt();
  SetLength(Result, N);
  for I := 1 to N do begin
    T.Text := StackPopString();
    T.Id := StackPopFullId();
    Result[I - 1] := T;
  end;
end;

function GetSearchInput(): string;
begin
  StackClear();
  if LogResultError(notebackend_search_input()) <> OK then begin
    raise EExternal.Create('GetSearchInput() failed');
  end;
  Result := StackPopString();
end;


end.
