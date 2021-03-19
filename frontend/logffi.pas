unit LogFFI;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, StackFFI;

// Log booleans for fast paths.
var
  LogHasError, LogHasWarn, LogHasInfo, LogHasDebug, LogHasTrace: boolean;

// Log APIs
procedure LogError(S: string);
procedure LogWarn(S: string);
procedure LogInfo(S: string);
procedure LogDebug(S: string);
procedure LogTrace(S: string);

procedure InitLogFFI;

function GitDesc: string;

implementation

var
  LogLevel: Int32;

function notebackend_enable_env_logger(): Int32; cdecl; external 'notebackend';
function notebackend_log_max_level(): Int32; cdecl; external 'notebackend';
procedure notebackend_log_error(); cdecl; external 'notebackend';
procedure notebackend_log_warn(); cdecl; external 'notebackend';
procedure notebackend_log_info(); cdecl; external 'notebackend';
procedure notebackend_log_debug(); cdecl; external 'notebackend';
procedure notebackend_log_trace(); cdecl; external 'notebackend';
procedure notebackend_git_desc(); cdecl; external 'notebackend';

procedure InitLogFFI;
begin
  notebackend_enable_env_logger();
  LogLevel := notebackend_log_max_level();
  LogHasTrace := (LogLevel >= 5);
  LogHasDebug := (LogLevel >= 4);
  LogHasInfo := (LogLevel >= 3);
  LogHasWarn := (LogLevel >= 2);
  LogHasError := (LogLevel >= 1);
  LogInfo(Format('Frontend Log Level: %d', [LogLevel]));
end;

procedure LogError(S: string);
begin
  StackPushString(S);
  notebackend_log_error();
end;

procedure LogWarn(S: string);
begin
  StackPushString(S);
  notebackend_log_warn();
end;

procedure LogInfo(S: string);
begin
  StackPushString(S);
  notebackend_log_info();
end;

procedure LogDebug(S: string);
begin
  StackPushString(S);
  notebackend_log_debug();
end;

procedure LogTrace(S: string);
begin
  StackPushString(S);
  notebackend_log_trace();
end;

function GitDesc: string;
begin
  notebackend_git_desc();
  Result := StackPopString();
end;

initialization

end.


