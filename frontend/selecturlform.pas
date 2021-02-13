unit selecturlform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormOpenUrl }

  TFormOpenUrl = class(TForm)
    ButtonOpen: TButton;
    ButtonCancel: TButton;
    EditUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioMemory: TRadioButton;
    RadioLocalFile: TRadioButton;
    RadioCustom: TRadioButton;
    SaveDialogFooNote: TSaveDialog;
    procedure EditUrlChange(Sender: TObject);
    procedure RadioCustomChange(Sender: TObject);
    procedure RadioGitChange(Sender: TObject);
    procedure RadioLocalFileChange(Sender: TObject);
    procedure RadioMemoryChange(Sender: TObject);
  private

  public

  end;

var
  FormOpenUrl: TFormOpenUrl;

implementation

{$R *.lfm}

{ TFormOpenUrl }

procedure TFormOpenUrl.RadioMemoryChange(Sender: TObject);
begin
  EditUrl.Text := 'memory:memory';
  EditUrl.ReadOnly := True;
  EditUrl.Enabled := False;
end;

procedure TFormOpenUrl.RadioGitChange(Sender: TObject);
var
  S: string;
begin
  S := EditUrl.Text;
  if not (S.StartsWith('git:') or S.EndsWith('.git')) then begin
    S += '.git';
    EditUrl.Text := S;
  end;
end;

procedure TFormOpenUrl.EditUrlChange(Sender: TObject);
var
  S: string;
begin
  S := EditUrl.Text;
  ButtonOpen.Enabled := not S.IsEmpty;
end;

procedure TFormOpenUrl.RadioCustomChange(Sender: TObject);
begin
  EditUrl.ReadOnly := False;
  EditUrl.Enabled := True;
end;

procedure TFormOpenUrl.RadioLocalFileChange(Sender: TObject);
var
  S: string;
begin
  if RadioLocalFile.Checked then begin
    if SaveDialogFooNote.Execute then begin
      S := SaveDialogFooNote.FileName;
      if not S.EndsWith('.foonote') then begin
        S := Format('foonote:%s', [S]);
      end;
      EditUrl.Text := S;
      EditUrl.ReadOnly := True;
      EditUrl.Enabled := False;
    end else begin
      RadioCustom.Checked := True;
    end;
  end;
end;

end.
