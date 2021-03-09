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
    CheckBoxOnTop: TCheckBox;
    FontDialog1: TFontDialog;
    GroupBoxInterfaceSettings: TGroupBox;
    procedure ButtonSelFontClick(Sender: TObject);
    procedure CheckBoxOnTopChange(Sender: TObject);
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
  if (Name = 'StayOnTop') and (FormFooNoteSettings.CheckBoxOnTop.Checked <> Config.StayOnTop) then begin
    FormFooNoteSettings.CheckBoxOnTop.Checked := Config.StayOnTop;
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

procedure TFormFooNoteSettings.FormCreate(Sender: TObject);
begin
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TFormFooNoteSettings.FormShow(Sender: TObject);
begin
  CheckBoxOnTop.Checked := AppConfig.StayOnTop;
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

end.

