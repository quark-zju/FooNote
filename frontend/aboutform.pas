unit AboutForm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Settings, BuildInfo, LclIntf, LogFFI;

type

  { TAboutFooNoteForm }

  TAboutFooNoteForm = class(TForm)
    ButtonOk: TButton;
    EditRootTreeUrl: TEdit;
    EditConfigFilePath: TEdit;
    ImageIcon: TImage;
    LabelDesc: TLabel;
    LabelLink: TLabel;
    LabelBuildDate: TLabel;
    LabelBuildDateValue: TLabel;
    LabelRootTreeUrl: TLabel;
    LabelConfigPath: TLabel;
    LabelVersion: TLabel;
    LabelVersionNumber: TLabel;
    MemoGPL3: TMemo;
    PageControl1: TPageControl;
    PanelButtons: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelLinkClick(Sender: TObject);
  private

  public

  end;

var
  AboutFooNoteForm: TAboutFooNoteForm;

implementation

{$R *.lfm}

{ TAboutFooNoteForm }

procedure TAboutFooNoteForm.ButtonOkClick(Sender: TObject);
begin
  Close;
end;

procedure TAboutFooNoteForm.FormCreate(Sender: TObject);
begin
  LabelVersionNumber.Caption := LogFFI.GitDesc();
  LabelBuildDateValue.Caption := BuildDateText;
  EditRootTreeUrl.Text := AppConfig.RootTreeUrl;
  EditConfigFilePath.Text := AppConfig.ConfigFileName;
end;

procedure TAboutFooNoteForm.LabelLinkClick(Sender: TObject);
begin
  OpenUrl(LabelLink.Caption);
end;

end.

