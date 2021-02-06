unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, FileInfo, Settings;

type

  { TAboutFooNoteForm }

  TAboutFooNoteForm = class(TForm)
    ButtonOk: TButton;
    EditRootTreeUrl: TEdit;
    ImageIcon: TImage;
    LabelDesc: TLabel;
    LabelLink: TLabel;
    LabelBuildDate: TLabel;
    LabelBuildDateValue: TLabel;
    LabelRootTreeUrl: TLabel;
    LabelVersion: TLabel;
    LabelVersionNumber: TLabel;
    MemoGPL3: TMemo;
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  AboutFooNoteForm: TAboutFooNoteForm;

const
  VersionText = '1.1';
  BuildDateText = {$I %DATE%};

implementation

{$R *.lfm}

{ TAboutFooNoteForm }

procedure TAboutFooNoteForm.ButtonOkClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutFooNoteForm.FormCreate(Sender: TObject);
begin
  LabelVersionNumber.Caption := VersionText;
  LabelBuildDateValue.Caption := BuildDateText;
  EditRootTreeUrl.Text := AppState.RootTreeUrl;
end;

end.

