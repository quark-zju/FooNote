unit settingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, Settings, LogFFI, SciEdit;

type

  { TFormFooNoteSettings }

  TFormFooNoteSettings = class(TForm)
    ButtonOk: TButton;
    ButtonSelFont: TButton;
    ButtonSelEditorFont: TButton;
    CheckBoxUseSciEdit: TCheckBox;
    CheckBoxTreeHorizonScrollbar: TCheckBox;
    CheckBoxOnTop: TCheckBox;
    CheckBoxNoteHorizonScrollbar: TCheckBox;
    CheckBoxSciDirectWrite: TCheckBox;
    ComboBoxLang: TComboBox;
    FontDialog1: TFontDialog;
    GroupBoxOther: TGroupBox;
    GroupBoxInterfaceSettings: TGroupBox;
    Label1: TLabel;
    LabelAutoSaveInterval: TLabel;
    SpinEditAutoSave: TSpinEdit;
    procedure ButtonSelEditorFontClick(Sender: TObject);
    procedure ButtonSelFontClick(Sender: TObject);
    procedure CheckBoxNoteHorizonScrollbarChange(Sender: TObject);
    procedure CheckBoxOnTopChange(Sender: TObject);
    procedure CheckBoxSciDirectWriteChange(Sender: TObject);
    procedure CheckBoxTreeHorizonScrollbarChange(Sender: TObject);
    procedure CheckBoxUseSciEditChange(Sender: TObject);
    procedure ComboBoxLangChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEditAutoSaveChange(Sender: TObject);
  private

  public

  end;

var
  FormFooNoteSettings: TFormFooNoteSettings;

implementation

{$R *.lfm}

{ TFormFooNoteSettings }

function LocaleToComboIndex(Locale: string): integer;
begin
  if Locale = 'cn' then begin
    Result := 1;
  end else if locale = 'en' then begin
    Result := 2;
  end else begin
    Result := 0;
  end;
end;

function ComboIndexToLocale(Index: integer): string;
begin
  if Index = 1 then begin
    Result := 'cn';
  end else if Index = 2 then begin
    Result := 'en';
  end else begin
    Result := '';
  end;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  I: integer;
  B, E: boolean;
begin
  if (FormFooNoteSettings.CheckBoxOnTop.Checked <> Config.StayOnTop) then begin
    FormFooNoteSettings.CheckBoxOnTop.Checked := Config.StayOnTop;
  end;
  if (FormFooNoteSettings.CheckBoxNoteHorizonScrollbar.Checked <> Config.NoteHorizonScrollBar) then begin
    FormFooNoteSettings.CheckBoxNoteHorizonScrollbar.Checked := Config.NoteHorizonScrollBar;
  end;
  if (FormFooNoteSettings.CheckBoxTreeHorizonScrollbar.Checked <> Config.TreeHorizonScrollBar) then begin
    FormFooNoteSettings.CheckBoxTreeHorizonScrollbar.Checked := Config.TreeHorizonScrollBar;
  end;
  I := Config.AutoSaveInterval;
  if I <> FormFooNoteSettings.SpinEditAutoSave.Value then begin
    FormFooNoteSettings.SpinEditAutoSave.Value := I;
  end;
  I := LocaleToComboIndex(Config.Locale);
  if I <> FormFooNoteSettings.ComboBoxLang.ItemIndex then begin
    FormFooNoteSettings.ComboBoxLang.ItemIndex := I;
  end;
  E := SciEdit.TSciEdit.IsAvailable;
  FormFooNoteSettings.CheckBoxUseSciEdit.Enabled := E;
  FormFooNoteSettings.CheckBoxSciDirectWrite.Enabled := E and Config.UseSciEdit;
  if E then begin
    B := Config.UseSciEdit;
    if B <> FormFooNoteSettings.CheckBoxUseSciEdit.Checked then begin
      FormFooNoteSettings.CheckBoxUseSciEdit.Checked := B;
    end;
    B := Config.SciDirectWrite;
    if B <> FormFooNoteSettings.CheckBoxSciDirectWrite.Checked then begin
      FormFooNoteSettings.CheckBoxSciDirectWrite.Checked := B;
    end;
  end else begin
    FormFooNoteSettings.CheckBoxUseSciEdit.Checked := False;
    FormFooNoteSettings.CheckBoxSciDirectWrite.Checked := False;
  end;
end;

procedure SetFont(LinkedFont: TFont; Font: TFont);
begin
  LinkedFont.Assign(Font);
  //LinkedFont.Name := Font.Name;
  //LinkedFont.Size := Font.Size;
end;

procedure TFormFooNoteSettings.CheckBoxOnTopChange(Sender: TObject);
begin
  AppConfig.StayOnTop := CheckBoxOnTop.Checked;
end;

procedure TFormFooNoteSettings.CheckBoxSciDirectWriteChange(Sender: TObject);
begin
  AppConfig.SciDirectWrite := CheckBoxSciDirectWrite.Checked;
end;

procedure TFormFooNoteSettings.CheckBoxTreeHorizonScrollbarChange(Sender: TObject);
begin
  AppConfig.TreeHorizonScrollBar := CheckBoxTreeHorizonScrollbar.Checked;
end;

procedure TFormFooNoteSettings.CheckBoxUseSciEditChange(Sender: TObject);
begin
  AppConfig.UseSciEdit := CheckBoxUseSciEdit.Checked;
end;

procedure TFormFooNoteSettings.ComboBoxLangChange(Sender: TObject);
begin
  AppConfig.Locale := ComboIndexToLocale(ComboBoxLang.ItemIndex);
  Label1.Visible := True;
end;

procedure TFormFooNoteSettings.FormCreate(Sender: TObject);
begin
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TFormFooNoteSettings.FormShow(Sender: TObject);
begin
  OnConfigChange(AnyConfigName, AppConfig);
end;

procedure TFormFooNoteSettings.SpinEditAutoSaveChange(Sender: TObject);
begin
  AppConfig.AutoSaveInterval := SpinEditAutoSave.Value;
end;

procedure TFormFooNoteSettings.ButtonSelFontClick(Sender: TObject);
var
  F: TFont;
begin
  FontDialog1.Font := AppConfig.EditorFont;
  if FontDialog1.Execute then begin
    F := FontDialog1.Font;
    if LogFFI.LogHasDebug then begin
      LogFFI.LogDebug(Format('Selected Font: %s %d', [F.Name, F.Size]));
    end;
    AppConfig.EditorFont := F;
    SetFont(AppConfig.TreeViewSearchFont, F);
    SetFont(AppConfig.TreeViewFont, F);
    SetFont(AppConfig.SearchBarFont, F);
  end;
end;

procedure TFormFooNoteSettings.ButtonSelEditorFontClick(Sender: TObject);
var
  F: TFont;
begin
  FontDialog1.Font := AppConfig.EditorFont;
  if FontDialog1.Execute then begin
    F := FontDialog1.Font;
    if LogFFI.LogHasDebug then begin
      LogFFI.LogDebug(Format('Selected Editor Font: %s %d', [F.Name, F.Size]));
    end;
    AppConfig.EditorFont := F;
  end;
end;

procedure TFormFooNoteSettings.CheckBoxNoteHorizonScrollbarChange(Sender: TObject);
begin
  AppConfig.NoteHorizonScrollBar := CheckBoxNoteHorizonScrollbar.Checked;
end;

end.
