unit NoteBackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLogger, NoteTypes, Math;

const
  OK: Int32 = 0;
  ENONE: Int32 = -1;
  ETYPE: Int32 = -2;

// Main APIs.

procedure CloseAll;
function Open(Url: string): FullId;
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
function IsMount(Id: FullId): boolean;

procedure StartSearch(Ids: VecFullId; Text: string);
procedure StopSearch();
function IsSearchComplete(): boolean;
function GetSearchResult(Skip: integer): VecFullIdText;
function GetSearchInput(): string;

// Return True on success.

function TryPersist(): boolean;
function TryRemove(Id: FullId): boolean;
function TryRemove(Ids: VecFullId): boolean;
function TrySetRawMeta(Id: FullId; Meta: string): boolean;
function TryUpdateMeta(Id: FullId; Prefix, Value: string): boolean;
function TrySetParent(Id: FullId; Parent: FullId; Pos: Int32): boolean;
function TrySetParent(var Ids: VecFullId; Parent: FullId; Pos: Int32): boolean;
function TrySetText(Id: FullId; Text: string): boolean;
function TryMount(Id: FullId; Url: string): boolean;
function TryUmount(Id: FullId): boolean;


implementation

// Communicate with Rust via a stack. References (like string or bytes) are copied
// when crossing language boundaries.

procedure StackClear(); cdecl; external 'notebackend' Name 'notebackend_stack_clear';
procedure StackPop(); cdecl; external 'notebackend' Name 'notebackend_stack_pop';

function StackLen(): UInt32; cdecl;
  external 'notebackend' Name 'notebackend_stack_len';
procedure StackDebug(); cdecl; external 'notebackend' Name 'notebackend_stack_debug';
procedure StackPushInt(Value: Int32); cdecl;
  external 'notebackend' Name 'notebackend_stack_push_i32';

procedure notebackend_stack_push_bytes(Offset: PByte; Length: NativeUInt); cdecl;
  external 'notebackend';
procedure notebackend_stack_push_str(Offset: PByte; Length: NativeUInt); cdecl;
  external 'notebackend';

function notebackend_stack_last_bytes(Offset: PPByte; Length: PNativeUInt): Int32; cdecl;
  external 'notebackend';
function notebackend_stack_last_str(Offset: PPByte; Length: PNativeUInt): Int32; cdecl;
  external 'notebackend';
function notebackend_stack_last_i32(Value: PInt32): Int32; cdecl;
  external 'notebackend';

// Main FFI APIs. Their input and output are based on the thread-local stack.
// The return value is for error codes.

function notebackend_open(): Int32; cdecl; external 'notebackend';
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
function notebackend_copy(): Int32; cdecl; external 'notebackend';
function notebackend_paste(): Int32; cdecl; external 'notebackend';
function notebackend_mount(): Int32; cdecl; external 'notebackend';
function notebackend_is_mount(): Int32; cdecl; external 'notebackend';
function notebackend_umount(): Int32; cdecl; external 'notebackend';
function notebackend_search_start(): Int32; cdecl; external 'notebackend';
function notebackend_search_stop(): Int32; cdecl; external 'notebackend';
function notebackend_search_result(): Int32; cdecl; external 'notebackend';
function notebackend_search_is_complete(): Int32; cdecl; external 'notebackend';
function notebackend_search_input(): Int32; cdecl; external 'notebackend';
procedure notebackend_close_all(); cdecl; external 'notebackend';

procedure StackPushBytes(Bytes: TBytes);
var
  L: integer;
  S: PByte;
begin
  S := @Bytes[0];
  L := Length(Bytes);
  notebackend_stack_push_bytes(S, L);
end;

procedure StackPushString(S: string);
var
  L: integer;
  P: PByte;
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetAnsiBytes(S);
  L := Length(Bytes);
  if L = 0 then begin
    P := nil;
  end else begin
    P := @Bytes[0];
  end;
  notebackend_stack_push_str(P, L);
end;

procedure StackPushFullId(Id: FullId);
begin
  StackPushInt(Id.BackendId);
  StackPushInt(Id.Id);
end;

procedure StackPushFullIdList(IdList: VecFullId);
var
  I, L: integer;
begin
  L := Length(IdList);
  for I := 1 to L do begin
    StackPushFullId(IdList[L - I]);
  end;
  StackPushInt(L);
end;

function StackLastBytes(): TBytes;
var
  L: NativeUInt;
  S: PByte;
  R: Int32;
begin
  S := nil;
  L := 0;
  R := notebackend_stack_last_bytes(@S, @L);
  if R <> OK then begin
    raise EExternal.Create('StackLastBytes(): no bytes to return');
  end;
  Result := TBytes.Create;
  SetLength(Result, L);
  if L > 0 then begin
    Move(S^, Result[0], L);
  end;
end;

function StackLastString(): string;
var
  L: NativeUInt;
  S: PByte;
  R: Int32;
  Bytes: TBytes;
begin
  S := nil;
  L := 0;
  R := notebackend_stack_last_str(@S, @L);
  if R <> OK then begin
    raise EExternal.Create('StackLastString(): no string to return');
  end;
  Bytes := TBytes.Create;
  SetLength(Bytes, L);
  if L > 0 then begin
    Move(S^, Bytes[0], L);
  end;
  Result := TEncoding.UTF8.GetAnsiString(Bytes);
end;


function StackLastInt(): Int32;
var
  R: Int32;
begin
  R := notebackend_stack_last_i32(@Result);
  if R <> OK then begin
    raise EExternal.Create('StackLastInt(): no int to return');
  end;
end;

function StackPopBytes(): TBytes;
begin
  Result := StackLastBytes();
  StackPop();
end;

function StackPopString(): string;
begin
  Result := StackLastString();
  StackPop();
end;

function StackPopInt(): Int32;
begin
  Result := StackLastInt();
  StackPop();
end;

function StackPopFullId(): FullId;
begin
  Result.Id := StackPopInt();
  Result.BackendId := StackPopInt();
end;

function StackPopFullIdList(): VecFullId;
var
  I, L: integer;
begin
  L := StackPopInt();
  Result := VecFullId.Create;
  SetLength(Result, L);
  for I := 1 to L do begin
    Result[I - 1] := StackPopFullId();
  end;
end;

// Main API implemenation by communicating using stack APIs.

function LogError(ErrNo: integer): integer;
var
  S: string;
begin
  if ErrNo <> OK then begin
    S := StackLastString();
    if not S.IsEmpty then begin
      DebugLn('Error: %s', [S]);
    end;
  end;
  Result := ErrNo;
end;

procedure CloseAll;
begin
  notebackend_close_all;
end;

function Open(Url: string): FullId;
begin
  StackClear();
  StackPushString(Url);
  if LogError(notebackend_open()) <> OK then begin
    raise EExternal.Create(Format('Open(%s) failed', [Url]));
  end;
  Result := StackPopFullId();
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
  if LogError(notebackend_get_children()) <> OK then begin
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
  if LogError(notebackend_get_parent()) <> OK then begin
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
  if LogError(notebackend_get_mtime()) <> OK then begin
    raise EExternal.Create(Format('GetMtime(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopInt();
end;

function GetText(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogError(notebackend_get_text()) <> OK then begin
    raise EExternal.Create(Format('GetText(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetTextFirstLine(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogError(notebackend_get_text_first_line()) <> OK then begin
    raise EExternal.Create(Format('GetTextFirstLine(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetRawMeta(Id: FullId): string;
begin
  StackClear();
  StackPushFullId(Id);
  if LogError(notebackend_get_raw_meta()) <> OK then begin
    raise EExternal.Create(Format('GetMeta(%s) failed', [Id.ToString()]));
  end;
  Result := StackPopString();
end;

function GetHeads(IdList: VecFullId): VecFullId;
begin
  StackClear();
  StackPushFullIdList(IdList);
  if LogError(notebackend_get_heads()) <> OK then begin
    raise EExternal.Create('GetHeads() failed');
  end;
  Result := StackPopFullIdList();
end;


function ExtractMeta(Id: FullId; Prefix: string): string;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Prefix);
  if LogError(notebackend_extract_meta()) <> OK then begin
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
  if LogError(notebackend_insert()) <> OK then begin
    raise EExternal.Create(Format('InsertNode(%s, %d, %s, %s) failed', [Id.ToString(), Pos, Text, meta]));
  end;
  Result := StackPopFullId();
  if autofill then begin
    StackClear();
    StackPushFullId(Result);
    if LogError(notebackend_autofill()) <> OK then begin
      raise EExternal.Create(Format('Autofill(%s) failed', [Result.ToString()]));
    end;
  end;
end;

function CopyToBytes(Ids: VecFullId): TBytes;
begin
  StackClear();
  StackPushFullIdList(Ids);
  if LogError(notebackend_copy()) <> OK then begin
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
  if LogError(notebackend_paste()) <> OK then begin
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
  Result := (LogError(notebackend_set_parent()) = OK);
end;

function TrySetParent(var Ids: VecFullId; Parent: FullId; Pos: Int32): boolean;
begin
  StackClear();
  StackPushFullIdList(Ids);
  StackPushFullId(Parent);
  StackPushInt(Pos);
  Result := (LogError(notebackend_set_parent_batch()) = OK);
  Ids := StackPopFullIdList();
end;


function TrySetText(Id: FullId; Text: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Text);
  Result := (LogError(notebackend_set_text()) = OK);
end;

function TrySetRawMeta(Id: FullId; Meta: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Meta);
  Result := (LogError(notebackend_set_raw_meta()) = OK);
end;

function TryUpdateMeta(Id: FullId; Prefix, Value: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Prefix);
  StackPushString(Value);
  Result := (LogError(notebackend_update_meta()) = OK);
end;

function TryRemove(Id: FullId): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  Result := (LogError(notebackend_remove()) = OK);
end;

function TryRemove(Ids: VecFullId): boolean;
begin
  StackClear();
  StackPushFullIdList(Ids);
  Result := (LogError(notebackend_remove_batch()) = OK);
end;

function TryPersist(): boolean;
begin
  StackClear();
  Result := (LogError(notebackend_persist()) = OK);
end;

function IsFolder(Id: FullId): boolean;
var
  S: string;
begin
  S := ExtractMeta(Id, 'type=');
  Result := (S = 'folder') or (S = 'trash') or (S = 'mount');
end;

function TryMount(Id: FullId; Url: string): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  StackPushString(Url);
  Result := (LogError(notebackend_mount()) = OK);
end;

function TryUmount(Id: FullId): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  Result := (LogError(notebackend_umount()) = OK);
end;

function IsMount(Id: FullId): boolean;
begin
  StackClear();
  StackPushFullId(Id);
  if LogError(notebackend_is_mount()) <> OK then begin
    raise EExternal.Create('IsMount() failed');
  end;
  Result := StackPopInt() <> 0;
end;

procedure StartSearch(Ids: VecFullId; Text: string);
begin
  StackClear();
  StackPushString(Text);
  StackPushFullIdList(Ids);
  if LogError(notebackend_search_start()) <> OK then begin
    raise EExternal.Create('StartSearch() failed');
  end;
end;

procedure StopSearch();
begin
  StackClear();
  if LogError(notebackend_search_stop()) <> OK then begin
    raise EExternal.Create('StopSearch() failed');
  end;
end;

function IsSearchComplete(): boolean;
begin
  StackClear();
  if LogError(notebackend_search_is_complete()) <> OK then begin
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
  if LogError(notebackend_search_result()) <> OK then begin
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
  if LogError(notebackend_search_input()) <> OK then begin
    raise EExternal.Create('GetSearchInput() failed');
  end;
  Result := StackPopString();
end;


end.
