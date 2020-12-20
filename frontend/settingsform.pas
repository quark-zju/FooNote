unit settingsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, RTTIGrids, RTTICtrls, Settings;

type

  { TFooNoteSettingsForm }

  TFooNoteSettingsForm = class(TForm)
    ButtonOk: TButton;
    CheckBoxReset: TCheckBox;
    LabelHint: TLabel;
    PanelButtons: TPanel;
    PanelHint: TPanel;
    PropEditor: TTIPropertyGrid;
    SettingTabs: TTabControl;
    procedure ButtonOkClick(Sender: TObject);
    procedure CheckBoxResetChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PropEditorClick(Sender: TObject);
    procedure PropEditorKeyPress(Sender: TObject; var Key: char);
    procedure SettingTabsChange(Sender: TObject);
  private
    procedure UpdateHint;
    procedure UpdateTabs;
  public

  end;

var
  FooNoteSettingsForm: TFooNoteSettingsForm;

implementation

{$R *.lfm}

resourcestring
  RSStayOnTop = 'Stay on top of other windows.';
  RSDebugTab = 'Debug';

var
  Registered: boolean = False;

{ TFooNoteSettingsForm }

procedure TFooNoteSettingsForm.UpdateTabs;
begin
  if AppConfig.FeatureLevel = flDebug then begin
    if SettingTabs.Tabs.Count < 2 then begin
      SettingTabs.Tabs.Append(RSDebugTab);
    end;
  end else begin
    while SettingTabs.Tabs.Count >= 2 do begin
      SettingTabs.Tabs.Pop;
    end;
  end;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  This: TFooNoteSettingsForm;
begin
  This := FooNoteSettingsForm;
  if This = nil then begin
    exit;
  end;
  if This.Visible then begin
    This.PropEditor.RefreshPropertyValues;
  end;
  if Name = 'FeatureLevel' then begin
    This.UpdateTabs;
  end;
end;


procedure TFooNoteSettingsForm.PropEditorClick(Sender: TObject);
begin
  UpdateHint;
end;

procedure TFooNoteSettingsForm.PropEditorKeyPress(Sender: TObject; var Key: char);
begin
  UpdateHint;
end;

procedure TFooNoteSettingsForm.SettingTabsChange(Sender: TObject);
begin
  if SettingTabs.TabIndex = 0 then begin
    PropEditor.TIObject := AppConfig;
    PanelHint.Visible := True;
  end else if SettingTabs.TabIndex = 1 then begin
    PropEditor.TIObject := AppState;
    PanelHint.Visible := False;
  end;
end;

procedure TFooNoteSettingsForm.FormShow(Sender: TObject);
begin
  if not Registered then begin
    AppConfig.RegisterOnChangeCallback(@OnConfigChange);
    Registered := True;
  end;
  UpdateTabs;
  SettingTabsChange(Sender);
end;

procedure TFooNoteSettingsForm.ButtonOkClick(Sender: TObject);
begin
  Close;
end;

procedure TFooNoteSettingsForm.CheckBoxResetChange(Sender: TObject);
begin
  AppState.ResetOnNextStartup := CheckBoxReset.Checked;
end;

procedure TFooNoteSettingsForm.UpdateHint;
var
  HintText: string;
begin
  if not Assigned(PropEditor.GetActiveRow) then begin
    exit;
  end;
  HintText := PropEditor.GetActiveRow.Name;
  if HintText = 'StayOnTop' then begin
    HintText := RSStayOnTop;
  end;
  LabelHint.Caption := HintText;
end;

end.
