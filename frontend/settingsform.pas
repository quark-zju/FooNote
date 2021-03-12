unit settingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Settings, LogFFI;

type

  { TFormFooNoteSettings }

  TFormFooNoteSettings = class(TForm)
    ButtonOk: TButton;
    ButtonSelFont: TButton;
    CheckBoxTreeHorizonScrollbar: TCheckBox;
    CheckBoxOnTop: TCheckBox;
    CheckBoxNoteHorizonScrollbar: TCheckBox;
    FontDialog1: TFontDialog;
    GroupBoxInterfaceSettings: TGroupBox;
    procedure ButtonSelFontClick(Sender: TObject);
    procedure CheckBoxNoteHorizonScrollbarChange(Sender: TObject);
    procedure CheckBoxOnTopChange(Sender: TObject);
    procedure CheckBoxTreeHorizonScrollbarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormFooNoteSettings: TFormFooNoteSettings;

implementation

{$R *.lfm}

{ TFormFooNoteSettings }

procedure OnConfigChange(Name: string; Config: TAppConfig);
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

procedure TFormFooNoteSettings.CheckBoxTreeHorizonScrollbarChange(Sender: TObject);
begin
  AppConfig.TreeHorizonScrollBar := CheckBoxTreeHorizonScrollbar.Checked;
end;

procedure TFormFooNoteSettings.FormCreate(Sender: TObject);
begin
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TFormFooNoteSettings.FormShow(Sender: TObject);
begin
  OnConfigChange(AnyConfigName, AppConfig);
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
    SetFont(AppConfig.EditorFont, F);
    SetFont(AppConfig.TreeViewSearchFont, F);
    SetFont(AppConfig.TreeViewFont, F);
    SetFont(AppConfig.SearchBarFont, F);
  end;
end;

procedure TFormFooNoteSettings.CheckBoxNoteHorizonScrollbarChange(Sender: TObject);
begin
  AppConfig.NoteHorizonScrollBar := CheckBoxNoteHorizonScrollbar.Checked;
end;

end.



