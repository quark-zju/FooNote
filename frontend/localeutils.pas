unit LocaleUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, gettext, LogFFI, StackFFI, LCLTranslator,
  LResources, Translations, Forms;

procedure InitLocale(lang: string);

var
  LocaleName: string;

implementation

function notebackend_set_lang(): Int32; cdecl; external 'notebackend';
function notebackend_get_lang(): Int32; cdecl; external 'notebackend';

// See LCLTranslator.SetDefaultLang.
// https://wiki.lazarus.freepascal.org/Everything_else_about_translations#Compiling_po_files_into_the_executable_and_change_language_while_running
function ApplyPo(POFileName: string): boolean;
var
  Res: TLResource;
  I: integer;
  PO: TPOFile;
  LocalTranslator: TUpdateTranslator;
begin
  Result := False;
  Res := LazarusResources.Find(POFileName, 'PO');
  if not Assigned(Res) then begin
    LogFFI.LogWarn(Format('Locale resource %s (PO) does not exist', [POFileName]));
    Exit;
  end;
  if LogFFI.LogHasDebug then begin
    LogFFI.LogDebug(Format('Loading locale resource %s (PO)', [POFileName]));
  end;

  PO := TPOFile.Create();
  PO.ReadPOText(Res.Value);

  Translations.TranslateResourceStrings(PO);
  LocalTranslator := TPOTranslator.Create(PO);

  if Assigned(LocalTranslator) then begin
    if Assigned(LRSTranslator) then begin
      LRSTranslator.Free;
    end;
    LRSTranslator := LocalTranslator;

    for I := 0 to Screen.CustomFormCount - 1 do begin
      LocalTranslator.UpdateTranslation(Screen.CustomForms[I]);
    end;
    for I := 0 to Screen.DataModuleCount - 1 do begin
      LocalTranslator.UpdateTranslation(Screen.DataModules[I]);
    end;
  end;
end;

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
    LocaleName := Lang;
  end else begin
    LogFFI.LogInfo(Format('Specified Lang = %s', [Lang]));
    StackFFI.StackPushString(Lang);
    notebackend_set_lang();
    LocaleName := Lang;
  end;

  if not Lang.IsEmpty then begin
    ApplyPo(Format('foonote.%s', [Lang]));
  end;
end;


initialization
  // lazres cn.lrs foonote.cn.mo
  {$I locale/cn.lrs}

end.
