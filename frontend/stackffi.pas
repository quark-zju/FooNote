unit StackFFI;

{$mode objfpc}{$H+}

interface

uses
  NoteTypes, Classes, SysUtils;

const
  OK: Int32 = 0;
  ENONE: Int32 = -1;
  ETYPE: Int32 = -2;


// Communicate with Rust via a stack. References (like string or bytes) are copied
// when crossing language boundaries.

procedure StackClear(); cdecl; external 'notebackend' Name 'notebackend_stack_clear';
procedure StackPop(); cdecl; external 'notebackend' Name 'notebackend_stack_pop';

function StackLen(): UInt32; cdecl;
  external 'notebackend' Name 'notebackend_stack_len';
procedure StackDebug(); cdecl; external 'notebackend' Name 'notebackend_stack_debug';
procedure StackPushInt(Value: Int32); cdecl;
  external 'notebackend' Name 'notebackend_stack_push_i32';

function StackLastBytes(): TBytes;
function StackLastInt(): Int32;
function StackLastString(): string;
function StackPopBytes(): TBytes;
function StackPopFullId(): FullId;
function StackPopFullIdList(): VecFullId;
function StackPopInt(): Int32;
function StackPopString(): string;
procedure StackPushBytes(Bytes: TBytes);
procedure StackPushFullId(Id: FullId);
procedure StackPushFullIdList(IdList: VecFullId);
procedure StackPushString(S: string);


implementation

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


end.


