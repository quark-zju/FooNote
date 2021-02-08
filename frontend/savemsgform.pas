unit savemsgform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Settings,
  Process;

type

  { TFormSaveFailure }

  TFormSaveFailure = class(TForm)
    ButtonClose: TButton;
    ButtonNewInstance: TButton;
    EditNewUrl: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MemoErrorMessage: TMemo;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonNewInstanceClick(Sender: TObject);
    procedure EditNewUrlChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormSaveFailure: TFormSaveFailure;

implementation

{$R *.lfm}

{ TFormSaveFailure }

procedure TFormSaveFailure.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormSaveFailure.ButtonNewInstanceClick(Sender: TObject);
var
  P: TProcess;
begin
  P := TProcess.Create(nil);
  P.Executable := Application.ExeName;
  P.Parameters.Add(EditNewUrl.Text);
  P.ShowWindow := swoShowNormal;
  P.Execute;
  FreeAndNil(P);
  Close;
end;

procedure TFormSaveFailure.EditNewUrlChange(Sender: TObject);
var
  S: string;
begin
  S := EditNewUrl.Text;
  ButtonNewInstance.Enabled := not S.IsEmpty;
end;

procedure TFormSaveFailure.FormShow(Sender: TObject);
var
  I: integer;
  S: string;
begin
  MemoErrorMessage.Text := AppConfig.SaveFailureMessage;
  // Find an unused file name.
  if FileExists('a.foonote') then begin
    for I := 1 to 1000 do begin
      S := format('a%d.foonote', [I]);
      if not FileExists(S) then begin
        EditNewUrl.Text := S;
        break;
      end;
    end;
  end;
end;

end.
