unit LocaleUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gettext, LogFFI, StackFFI, LCLTranslator;

procedure InitLocale(lang: string);

implementation

function notebackend_set_lang(): Int32; cdecl; external 'notebackend';
function notebackend_get_lang(): Int32; cdecl; external 'notebackend';

procedure InitLocale(Lang: string);
var
  SysLang, Fallback: string;
begin
  if lang.IsEmpty then begin
    GetLanguageIDs(SysLang, Fallback);
    StackFFI.StackPushString(SysLang);
    notebackend_set_lang();
    notebackend_get_lang();
    Lang := StackFFI.StackPopString();
    LogFFI.LogInfo(Format('System SysLang = %s => %s', [SysLang, Lang]));
  end else begin
    LogFFI.LogInfo(Format('Specified Lang = %s', [Lang]));
    StackFFI.StackPushString(Lang);
    notebackend_set_lang();
  end;

  SetDefaultLang(Lang, 'locale');
end;

end.


