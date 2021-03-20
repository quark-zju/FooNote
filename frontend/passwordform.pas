unit passwordform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormPassword }

  TFormPassword = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    EditPassword: TEdit;
    EditRetypePassword: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure EditPasswordChange(Sender: TObject);
    procedure EditPasswordKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditRetypePasswordChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

procedure PrepareForm(Retype: boolean);
function PasswordResult: string;

var
  FormPassword: TFormPassword;

implementation

{$R *.lfm}

procedure PrepareForm(Retype: boolean);
begin
  if not Assigned(FormPassword) then begin
    Application.CreateForm(TFormPassword, FormPassword);
  end;
  FormPassword.EditRetypePassword.Visible := Retype;
  FormPassword.EditRetypePassword.Enabled := Retype;
  FormPassword.Label2.Visible := Retype;
  FormPassword.EditPassword.Clear;
  FormPassword.EditRetypePassword.Clear;
end;

function PasswordResult: string;
begin
  Result := FormPassword.EditPassword.Text;
  FormPassword.EditPassword.Clear;
  FormPassword.EditRetypePassword.Clear;
end;

{ TFormPassword }

procedure TFormPassword.EditRetypePasswordChange(Sender: TObject);
var
  E: boolean;
  S: string;
begin
  S := EditPassword.Text;
  E := (S.Length > 0);
  if E and EditRetypePassword.Visible and (EditRetypePassword.Text <> EditPassword.Text) then begin
    E := False;
  end;
  ButtonOk.Enabled := E;
end;

procedure TFormPassword.FormShow(Sender: TObject);
begin
  ButtonOk.Enabled := False;
  EditPassword.Clear;
  EditRetypePassword.Clear;
  EditPassword.SetFocus;
end;

procedure TFormPassword.ButtonCancelClick(Sender: TObject);
begin
  EditPassword.Clear;
  EditRetypePassword.Clear;
end;

procedure TFormPassword.EditPasswordChange(Sender: TObject);
begin
  EditRetypePasswordChange(Sender);
end;

procedure TFormPassword.EditPasswordKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
  S: string;
begin
  S := EditPassword.Text;
  if (Key = 10) or (Key = 13) and (not S.IsEmpty) and EditRetypePassword.CanFocus then begin
    EditRetypePassword.SetFocus;
    Key := 0;
  end;
end;

end.
