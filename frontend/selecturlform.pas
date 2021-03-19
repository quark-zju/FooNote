unit selecturlform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, NoteBackend;

type

  { TFormOpenUrl }

  TFormOpenUrl = class(TForm)
    ButtonOpen: TButton;
    ButtonCancel: TButton;
    EditUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    RadioGit: TRadioButton;
    RadioMemory: TRadioButton;
    RadioLocalFile: TRadioButton;
    RadioCustom: TRadioButton;
    procedure EditUrlChange(Sender: TObject);
    procedure EditUrlKeyPress(Sender: TObject; var Key: char);
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

procedure TFormOpenUrl.EditUrlChange(Sender: TObject);
var
  S: string;
begin
  S := EditUrl.Text;
  ButtonOpen.Enabled := not S.IsEmpty;
  S := NoteBackend.UrlType(S);
  ButtonOpen.Enabled := not S.IsEmpty;
  if S = 'foonote' then begin
    RadioLocalFile.Checked := True;
  end else if S = 'memory' then begin
    RadioMemory.Checked := True;
  end else if S = 'git' then begin
    RadioGit.Checked := True;
  end else begin
    RadioCustom.Checked := True;
  end;
end;

procedure TFormOpenUrl.EditUrlKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #10) or (key = #13) then begin
    ButtonOpen.Click;
  end;
end;

procedure TFormOpenUrl.RadioCustomChange(Sender: TObject);
begin
  EditUrlChange(Sender);
end;

procedure TFormOpenUrl.RadioGitChange(Sender: TObject);
begin
  EditUrlChange(Sender);
end;

procedure TFormOpenUrl.RadioLocalFileChange(Sender: TObject);
begin
  EditUrlChange(Sender);
end;

procedure TFormOpenUrl.RadioMemoryChange(Sender: TObject);
begin
  EditUrlChange(Sender);
end;

end.
