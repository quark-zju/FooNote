unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  PlatformWindows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, ActnList, ClipBrd, LCLType,
  LazUtf8, FGL, LogFFI, Math, NoteBackend, NoteTypes, MemoUtil,
  TreeNodeData, StackFFI,
  TreeViewSync, Settings, PreviewForm, AboutForm, FileUtil, md5,
  savemsgform, selecturlform, LocaleUtils, SettingsForm, PasswordForm, Types,
  SciEdit;

type

  { TFormFooNoteMain }

  TFormFooNoteMain = class(TForm)
    ActionNewLocalFile: TAction;
    ActionNewGit: TAction;
    ActionNewMemory: TAction;
    ActionEncryptLock: TAction;
    ActionEncryptUnlock: TAction;
    ActionNewAes256: TAction;
    ActionToggleFolder: TAction;
    ActionViewWarnUnsaved: TAction;
    ActionEditReload: TAction;
    MenuItem10: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItemLockUnlockSep: TMenuItem;
    MenuItemRootPath: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    PanelZen: TPanel;
    SaveDialogFooNote: TSaveDialog;
    TimerAutoRefresh: TTimer;
    TimerAutoSave: TTimer;
    ActionViewToggleZenMode: TAction;
    ActionEditPaste: TAction;
    ActionEditDelete: TAction;
    ActionEditCopy: TAction;
    ActionAppAbout: TAction;
    PanelDockSplitterRight: TPanel;
    IdleTimerWndProc: TIdleTimer;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem8: TMenuItem;
    MemoNote: TMemo;
    PanelDockSplitterLeft: TPanel;
    TimerCheckSaveResult: TTimer;
    ToolButtonWarning: TToolButton;
    TreeViewNoteTree: TTreeView;
    PanelTree: TPanel;
    PanelEdit: TPanel;
    SaveDialog: TSaveDialog;
    TimerSearchResult: TTimer;
    SplitterTreeNote: TSplitter;
    TreeViewSearchTree: TTreeView;
    ActionViewDockRight: TAction;
    ActionViewDockLeft: TAction;
    ActionViewStayOnTop: TAction;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    ActionViewUndock: TAction;
    ActionAppQuit: TAction;
    ImageListElementaryIcons: TImageList;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    ActionAppSettingPreferences: TAction;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuSepMount: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    ActionNewMountUrl: TAction;
    ActionNewSeparator: TAction;
    ActionNewFolder: TAction;
    ActionNewNote: TAction;
    ActionEditSave: TAction;
    ActionListMain: TActionList;
    PopupMenuEdit: TPopupMenu;
    EditNoteSearch: TEdit;
    ToolBarDefault: TToolBar;
    MenuItemNewSeparator: TMenuItem;
    MenuItemNewFolder: TMenuItem;
    MenuItemNewNote: TMenuItem;
    PopupMenuDockMenu: TPopupMenu;
    ToolButtonDockedMenu: TToolButton;
    ToolButtonNew: TToolButton;
    ToolBarDocked: TToolBar;
    PanelTop: TPanel;
    MenuAddMenu: TPopupMenu;

    procedure ActionAppAboutExecute(Sender: TObject);
    procedure ActionEditReloadExecute(Sender: TObject);
    procedure ActionEncryptLockExecute(Sender: TObject);
    procedure ActionEncryptUnlockExecute(Sender: TObject);
    procedure ActionNewAes256Execute(Sender: TObject);
    procedure ActionNewGitExecute(Sender: TObject);
    procedure ActionNewLocalFileExecute(Sender: TObject);
    procedure ActionNewMemoryExecute(Sender: TObject);
    procedure ActionToggleFolderExecute(Sender: TObject);
    procedure ActionViewWarnUnsavedExecute(Sender: TObject);
    procedure EditNoteSearchKeyPress(Sender: TObject; var Key: char);
    procedure EditNoteSearchKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure PanelDockSplitterLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PanelDockSplitterLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PanelDockSplitterLeftMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerWndProcTimer(Sender: TObject);
    procedure ActionNewMountUrlExecute(Sender: TObject);
    procedure EditNoteSearchChange(Sender: TObject);
    procedure TimerAutoRefreshTimer(Sender: TObject);
    procedure TimerAutoSaveTimer(Sender: TObject);
    procedure TimerCheckSaveResultTimer(Sender: TObject);
    procedure TreeViewNoteTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
    procedure TreeViewNoteTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
    procedure TreeViewNoteTreeDblClick(Sender: TObject);
    procedure TreeViewNoteTreeDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure TreeViewNoteTreeDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
    procedure TreeViewNoteTreeEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure TreeViewNoteTreeEnter(Sender: TObject);
    procedure TreeViewNoteTreeExit(Sender: TObject);
    procedure TreeViewNoteTreeKeyPress(Sender: TObject; var Key: char);
    procedure TreeViewNoteTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TreeViewNoteTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TreeViewNoteTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure TreeViewSearchTreeDblClick(Sender: TObject);
    procedure TreeViewSearchTreeEnter(Sender: TObject);
    procedure TreeViewSearchTreeKeyPress(Sender: TObject; var Key: char);
    procedure TreeViewSearchTreeSelectionChanged(Sender: TObject);
    procedure TimerSearchResultTimer(Sender: TObject);
    procedure PanelTopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PanelTopMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PanelTopMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure SplitterTreeNoteMoved(Sender: TObject);
    procedure ActionViewDockLeftExecute(Sender: TObject);
    procedure ActionViewDockRightExecute(Sender: TObject);
    procedure ActionViewStayOnTopExecute(Sender: TObject);
    procedure ActionViewToggleZenModeExecute(Sender: TObject);
    procedure ActionViewUndockExecute(Sender: TObject);
    procedure ActionAppQuitExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure ActionEditSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ActionNewFolderExecute(Sender: TObject);
    procedure ActionNewNoteExecute(Sender: TObject);
    procedure ActionNewSeparatorExecute(Sender: TObject);
    procedure MemoNoteChange(Sender: TObject);
    procedure MemoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TreeViewNoteTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewNoteTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
    procedure TreeViewNoteTreeSelectionChanged(Sender: TObject);
    procedure ActionAppSettingPreferencesExecute(Sender: TObject);
  protected
    // Alternative to MemoNote
    SciEditNote: TSciEdit;
    procedure SciEditNoteChange(Sender: TObject);
    procedure SciEditNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure EditorSetFocus;
  private
    // NoteTreeView state.
    RootNodeData: TTreeNodeData;
    FSelectedId: FullId;

    // NoteTreeView drag-n-drop state.
    CandidateDropNode: TTreeNode;
    CandidateInsertPosition: integer; // 0: inside; -1: before; 1: after.
    DragOverHappened: boolean;
    DragOverHappenedAndPreviewDrawn: boolean;
    StartDragCursorPos: TPoint;
    StartDragLocalPos: TPoint;

    // Search state.
    LastSearchText: string;

    // Top panel as "title bar" when docked.
    TopPanelMouseLeftDownStartPos: TPoint;
    TopPanelMouseLeftIsDown: boolean;

    // Window splitter when docked.
    DockSplitterLeftDownOrigCursorPos: TPoint;
    DockSplitterLeftDownOrigWidth: longint;
    DockSplitterLeftDownOrigLeft: longint;
    DockSplitterLeftIsDown: boolean;
    function DockSplitterNewWidth: longint;

    procedure InitRootTreeUrlAndConfigFileName;
    procedure InitRootBackend;
    procedure InitAppConfigLink;
    procedure InitPlatformSpecific;
    procedure InitOnConfigChange;

    procedure Reposition;
    procedure LoadAppConfigFromDiskWithoutApply;
    procedure InitI18n;
    procedure ApplyAppConfigToThisForm;
    procedure LoadSplitterPosition;
    procedure ApplyThisFromToAppConfig; // Reverse of ApplyAppConfigToThisForm.
    procedure SaveConfigFile;

    procedure ScheduleAutoSave;

    procedure UpdateTitle;
    procedure RefreshFullTree;
    procedure UpdatePopupMenu;
    procedure RefreshSelectedText;
    function InsertLocation(Id: FullId; NParent: integer; out Pos: integer): FullId;
    function NewNode(AText, AMeta: string; NParent: integer = 0): FullId;
    function NewMountNode(AText, Url: string; Copyable: boolean = True; NParent: integer = 1): FullId;
    procedure SelectExpandNode(Id: FullId);
    procedure SetSelectedId(Id: FullId);
    function GetSelectedIds(): VecFullId;
    procedure SetSelectedIds(Ids: VecFullId);
    procedure DrawTreeSelectionPreview;

    // Set the main selected Id. This updates the text editor but does not update the tree view.
    // Use SelectExpandNode to update the tree view and also update the selected id.
    property SelectedId: FullId read FSelectedId write SetSelectedId;

    // Set the selected ids. This updates the tree view, and the text editor, and SelectedId.
    // The first id in selection is used to update SelectedId.
    property SelectedIds: VecFullId read GetSelectedIds write SetSelectedIds;

    procedure FocusEditorEnd;
  public

  end;

var
  FormFooNoteMain: TFormFooNoteMain;
  PaintingPreviewForSelectedTreeItems: boolean = False;

resourcestring
  RSFailSaveStillExit = 'Failed to save changes. Still exit?';
  RSReloadConfirm = 'Reload will discard unsaved changes. Still reload?';
  RSExitNoSave = 'Exit without saving';
  RSNoExit = 'Do not exit';
  RSYesReload = 'Reload';
  RSNoReload = 'Do not reload';
  RSFailOpenUrl = 'Failed to open [%s]: %s';
  RSExit = 'Exit';
  RSOk = 'OK';
  RSEncryptedTextPrefix = 'Encrypted' + #10 + #10 + 'Do not edit lines below:' + #10;
  RSMemoryRootTitle = 'Temporary (Clear on Exit)';
  RSInputGitUrl = 'Input Git Repository Url: ' + #10 + #10 +
    'Note: The repository cannot be empty. Git binary installed in the system is used to access the repo.';
  RSNewGitTitle = 'New Git Mountpoint';
  RSNotGitUrl = '%s is not a Git url';
  RSCancel = 'Cancel';

implementation

var
  BinaryClipboardFormat: TClipboardFormat;

const
  DefaultUrl: string = 'Default.foonote';
  StartDragThreshold: ValReal = 10;

{$R *.lfm}

type
  TIdBoolMap = specialize TFPGMap<FullId, boolean>;

{ TFormFooNoteMain }

function UniqueStringId: string;
var
  guid: TGUID;
begin
  CreateGUID(guid);
  Result := guid.ToString(True);
end;

procedure ErrorDlg(Message: string);
begin
  QuestionDlg('FooNote', Message, mtError, [mbOK, RSOk], 0);
end;

procedure TFormFooNoteMain.RefreshFullTree;
begin
  if RootNodeData.SyncFromBackend() then begin
    // Change detected. Refresh the tree.
    TreeViewNoteTree.BeginUpdate;
    TreeViewSync.SyncChildTreeNode(TreeViewNoteTree, nil, TreeViewNoteTree.Items.GetFirstNode,
      RootNodeData, True);
    TreeViewNoteTree.EndUpdate;
  end else begin
    if LogHasTrace then begin
      LogTrace('Root Note Not changed');
    end;
  end;
end;

procedure TFormFooNoteMain.RefreshSelectedText;
var
  S: string;
begin
  if not Assigned(TreeViewNoteTree.Selected) then begin
    exit;
  end;
  S := NoteBackend.GetText(SelectedId);
  if MemoNote.Visible and (MemoNote.Text <> S) then begin
    MemoNote.Text := S;
  end;
  if Assigned(SciEditNote) and SciEditNote.Visible and (SciEditNote.Text <> S) then begin
    SciEditNote.Text := S;
    SciEditNote.SetSavepoint;
  end;
end;

procedure TFormFooNoteMain.InitRootTreeUrlAndConfigFileName;
var
  Url: string;
begin
  if ParamCount() >= 1 then begin
    Url := ParamStr(1);
  end else begin
    Url := DefaultUrl;
    MenuItemRootPath.Visible := False;
  end;
  AppConfig.ConfigFileName := Format('FooNote-%s.cfg', [MDPrint(MD5String(url)).Substring(0, 7)]);
  AppConfig.RootTreeUrl := Url;
end;

procedure TFormFooNoteMain.InitRootBackend;
var
  RootId: FullId;
  Url: string;
const
  FallbackUrl = 'memory:memory';
begin
  Url := AppConfig.RootTreeUrl;
  try
    RootId := NoteBackend.Open(Url);
  except
    on e: EExternal do begin
      // Use fallback Url.
      AppConfig.RootTreeUrl := FallbackUrl;
      RootId := NoteBackend.Open(FallbackUrl);
      ErrorDlg(Format(RSFailOpenUrl, [Url, e.Message]));
      Application.Terminate;
    end;
  end;

  FreeAndNil(RootNodeData);
  RootNodeData := TTreeNodeData.Create(RootId);
  SelectedId := RootId;
  RefreshFullTree;
  MenuItemRootPath.Caption := ExtractFileName(Url);
  ActionViewWarnUnsaved.Visible := False;

  UpdateTitle;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  I: longint;
  B: boolean;
  This: TFormFooNoteMain;
begin
  This := FormFooNoteMain;
  if (Name = AnyConfigName) or (Name = 'StayOnTop') then begin
    if Config.StayOnTop and (not AppConfig.ForceNotTop) then begin
      if This.FormStyle <> fsSystemStayOnTop then begin
        if LogHasDebug then begin
          LogDebug('Set FormStyle to fsSystemStayOnTop');
        end;
        This.FormStyle := fsSystemStayOnTop;
      end;
    end else begin
      if This.FormStyle <> fsNormal then begin
        if LogHasDebug then begin
          LogDebug('Set FormStyle to fsNormal');
        end;
        This.FormStyle := fsNormal;
      end;
    end;
    This.ActionViewStayOnTop.Checked := Config.StayOnTop;
  end;
  if (Name = AnyConfigName) or (Name = 'DockSide') then begin
    // When docked, the title bar is hidden. Show extra controls.
    This.ToolBarDocked.Visible := (Config.DockSide <> dsNone);
    This.ActionViewUndock.Enabled := (Config.DockSide <> dsNone);
    This.ActionViewDockLeft.Enabled := (Config.DockSide <> dsLeft);
    This.ActionViewDockRight.Enabled := (Config.DockSide <> dsRight);
    This.PanelDockSplitterLeft.Visible := (Config.DockSide = dsRight);
    This.PanelDockSplitterRight.Visible := (Config.DockSide = dsLeft);
    if Config.DockSide = dsNone then begin
      I := AppConfig.NonDockNoteSplitTop;
    end else begin
      I := AppConfig.DockNoteSplitTop;
    end;
    if (I > 0) and (I < This.Height) then begin
      This.SplitterTreeNote.Top := I;
    end;
  end;
  if (Name = AnyConfigName) or (Name = 'ZenMode') then begin
    B := Config.ZenMode;
    This.PanelTop.Visible := not B;
    This.PanelEdit.Visible := not B;
    This.PanelTree.Visible := not B;
    This.PanelZen.Visible := B;
    This.ActionViewToggleZenMode.Checked := B;
    This.SplitterTreeNote.Visible := not B;
    if B then begin
      This.MemoNote.Parent := This.PanelZen;
      if Assigned(This.SciEditNote) then begin
        This.SciEditNote.Parent := This.PanelZen;
      end;
    end else begin
      This.MemoNote.Parent := This.PanelEdit;
      if Assigned(This.SciEditNote) then begin
        This.SciEditNote.Parent := This.PanelEdit;
      end;
    end;
    if This.MemoNote.Visible and not This.MemoNote.Focused and This.MemoNote.CanSetFocus then begin
      This.MemoNote.SetFocus;
    end;
    if Assigned(This.SciEditNote) and This.SciEditNote.Visible and not This.SciEditNote.Focused and
      This.SciEditNote.CanSetFocus then begin
      This.SciEditNote.SetFocus;
    end;
  end;
  if (Name = AnyConfigName) or (Name = 'AutoSaveInterval') then begin
    I := Config.AutoSaveInterval;
    if I < 0 then begin
      I := 0;
    end else if I = 0 then begin
      // Default: 30 seconds.
      I := 30;
    end;
    if LogHasInfo then begin
      LogInfo(Format('AutoSaveInterval: %d seconds', [I]));
    end;
    This.TimerAutoSave.Interval := I * 1000;
  end;
  if (Name = AnyConfigName) or (Name = 'TreeHorizonScrollBar') then begin
    B := Config.TreeHorizonScrollBar;
    if B then begin
      This.TreeViewNoteTree.ScrollBars := ssAutoBoth;
    end else begin
      This.TreeViewNoteTree.ScrollBars := ssAutoVertical;
    end;
  end;
  if (Name = AnyConfigName) or (Name = 'NoteHorizonScrollBar') then begin
    B := Config.NoteHorizonScrollBar;
    if B then begin
      This.MemoNote.WordWrap := False;
      This.MemoNote.ScrollBars := ssAutoBoth;
      if Assigned(This.SciEditNote) then begin
        This.SciEditNote.WordWrap := False;
      end;
    end else begin
      This.MemoNote.WordWrap := True;
      This.MemoNote.ScrollBars := ssAutoVertical;
      if Assigned(This.SciEditNote) then begin
        This.SciEditNote.WordWrap := True;
      end;
    end;
  end;
  if (Name = AnyConfigName) or (Name = 'EditorFont') then begin
    This.MemoNote.Font.Assign(Config.EditorFont);
    if Assigned(This.SciEditNote) then begin
      This.SciEditNote.SetDefaultFont(Config.EditorFont);
    end;
  end;
  if (Name = AnyConfigName) or (Name = 'UseSciEdit') then begin
    B := Config.UseSciEdit and TSciEdit.IsAvailable;
    if LogHasDebug then begin
      LogDebug(Format('UseSciEdit = %d', [B.ToInteger]));
    end;
    if B and This.MemoNote.Visible then begin
      // Switch to SciEdit
      if not Assigned(This.SciEditNote) then begin
        LogDebug('Creating SciEdit');
        This.SciEditNote := TSciEdit.Create(This);
        This.SciEditNote.TabOrder := This.MemoNote.TabOrder;
        This.SciEditNote.TabStop := True;
        This.SciEditNote.Font.Assign(This.MemoNote.Font);
        This.SciEditNote.Align := This.MemoNote.Align;
        This.SciEditNote.OnChange := @This.SciEditNoteChange;
        This.SciEditNote.OnKeyDown := @This.SciEditNoteKeyDown;
        This.SciEditNote.Parent := This.MemoNote.Parent;
        This.SciEditNote.HandleNeeded;
        LogDebug(Format('Created SciEdit: %d', [This.SciEditNote.Handle]));
        // Refresh other things.
        OnConfigChange('NoteHorizonScrollBar', Config);
        OnConfigChange('EditorFont', Config);
      end;
      This.MemoNote.Visible := False;
      This.SciEditNote.Visible := False;
      This.SciEditNote.Text := This.MemoNote.Text;
      This.SciEditNote.SetSavepoint;
      This.SciEditNote.Visible := True;
    end;
    if not B and not This.MemoNote.Visible then begin
      // Switch to Memo
      if Assigned(This.SciEditNote) then begin
        This.SciEditNote.Visible := False;
      end;
      This.SciEditNote.Visible := False;
      This.MemoNote.Visible := False;
      This.MemoNote.Text := This.SciEditNote.Text;
      This.MemoNote.Visible := True;
    end;
  end;
end;

procedure TFormFooNoteMain.InitOnConfigChange;
begin
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TFormFooNoteMain.Reposition;
begin
  Left := Screen.WorkAreaWidth - Width * 3 div 2;
  Top := Width div 2;
end;

procedure TFormFooNoteMain.LoadAppConfigFromDiskWithoutApply;
var
  S: string;
begin
  S := '{}';
  try
    S := ReadFileToString(AppConfig.ConfigFileName);
  except
    on e: EFileNotFoundException do S := '{}';
  end;
  if not S.IsEmpty then begin
    try
      AppConfig.LoadFromJSONString(S);
    except
      on e: EParserError do if LogFFI.LogHasWarn then begin
          LogWarn(Format('Cannot parse config %s: %s', [AppConfig.ConfigFileName, e.ToString]));
        end;
    end;
  end;
end;

procedure TFormFooNoteMain.InitI18n;
begin
  LocaleUtils.InitLocale(AppConfig.Locale);
end;

procedure TFormFooNoteMain.LoadSplitterPosition;
var
  I: integer;
begin
  if AppConfig.DockSide = dsNone then begin
    I := AppConfig.NonDockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      SplitterTreeNote.Top := I;
    end; // After Height update
  end else begin
    I := AppConfig.DockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      SplitterTreeNote.Top := I;
    end; // After Height update
  end;
  if LogHasTrace then begin
    LogTrace(Format(' TreeNoteSplitter.Top=%d', [SplitterTreeNote.Top]));
  end;
end;

procedure TFormFooNoteMain.ApplyAppConfigToThisForm;
var
  I: integer;
  Id: FullId;
begin
  // Apply size constraint.
  if AppConfig.MaxWidth >= 80 then begin
    Constraints.MaxWidth := AppConfig.MaxWidth;
  end;

  // Apply window position and size.
  if AppConfig.DockSide = dsNone then begin
    I := AppConfig.NonDockWidth;
    if (I > 0) then begin
      Width := I;
    end;
    I := AppConfig.NonDockHeight;
    if (I > 0) then begin
      Height := I;
    end;
    Reposition;
    I := AppConfig.Left;
    if (I > 0) then begin
      Left := I;
    end;
    I := AppConfig.Top;
    if (I > 0) then begin
      Top := I;
    end;
  end;

  LoadSplitterPosition;

  // Apply previously selected node.
  if AppConfig.LastSelectedId > 0 then begin
    Id := FullId.Create(RootNodeData.Id.BackendId, AppConfig.LastSelectedId);
    try
      SelectExpandNode(Id);
    except
      on e: EExternal do ;
    end;
  end;

  // Apply window color.
  Color := AppConfig.WindowColor;
end;

procedure TFormFooNoteMain.ApplyThisFromToAppConfig;
begin
  if AppConfig.RememberPosition and (AppConfig.DockSide = dsNone) then begin
    AppConfig.Left := Left;
    AppConfig.Top := Top;
  end;
  AppConfig.LastSelectedId := SelectedId.Id;
  AppConfig.WindowColor := FormFooNoteMain.Color;
  if LogHasDebug then begin
    LogDebug(Format(' TreeNoteSplitter.Top=%d %d,%d', [SplitterTreeNote.Top, AppConfig.NonDockNoteSplitTop,
      AppConfig.DockNoteSplitTop]));
  end;
end;

procedure TFormFooNoteMain.SaveConfigFile;
var
  S: string;
  F: TFileStream;
begin
  if AppConfig.ResetOnNextStartup then begin
    DeleteFile(AppConfig.ConfigFileName);
  end else begin
    S := AppConfig.ToJSONString();
    F := TFileStream.Create(AppConfig.ConfigFileName, fmOpenWrite or fmCreate);
    try
      F.Write(PChar(S)^, Length(S));
    finally
      FreeAndNil(F);
    end;
  end;
end;

procedure TFormFooNoteMain.InitPlatformSpecific;
begin
{$ifdef Windows}
  PlatformWindows.SetupMainForm(Self);
{$endif}
end;

procedure TFormFooNoteMain.InitAppConfigLink;
begin
  AppConfig.EditorFont := MemoNote.Font;
  AppConfig.TreeViewFont := TreeViewNoteTree.Font;
  AppConfig.TreeViewSearchFont := TreeViewSearchTree.Font;
  AppConfig.SearchBarFont := EditNoteSearch.Font;
end;

function TFormFooNoteMain.InsertLocation(Id: FullId; NParent: integer; out Pos: integer): FullId;
begin
  Pos := 1; // Insert after.
  Result := Id;

  if (NParent = 0) and (IsFolder(Id)) then begin
    Pos := 0; // Append inside.
  end;
end;

procedure TFormFooNoteMain.SelectExpandNode(Id: FullId);
var
  Node: TTreeNode;
  Ancestors: VecFullId;
  Ancestor: FullId;
  I: integer;
begin
  if LogHasDebug then begin
    LogDebug(Format('SelectExpandNode %s', [Id.ToString()]));
  end;

  // Update tree selection.
  if Assigned(TreeViewNoteTree.Selected) and (NodeData(TreeViewNoteTree.Selected).Id = Id) then begin
    // Update editor text box selection.
    if SelectedId <> Id then begin
      SelectedId := Id;
    end;
    exit;
  end;

  // Expand ancestors recursively.
  Ancestors := NoteBackend.GetAncestors(Id);
  Node := TreeViewNoteTree.Items.GetFirstNode;
  for I := Length(Ancestors) - 1 downto 0 do begin
    Ancestor := Ancestors[I];
    if Ancestor = RootNodeData.Id then begin
      // RootNode is not in the tree view.
      continue;
    end;
    while Assigned(Node) do begin
      if LogHasTrace then begin
        LogTrace(Format('Visit %s', [NodeData(Node).Id.ToString()]));
      end;
      if NodeData(Node).Id = Ancestor then begin
        Node.Expand(False);
        if LogHasDebug then begin
          LogDebug(Format(' Expand ancestor %s', [Ancestor.ToString()]));
        end;
        Node := Node.GetFirstChild;
        break; // Next ancestor
      end else begin
        Node := Node.GetNextSibling;
      end;
    end;
    if not Assigned(Node) then begin
      if LogHasWarn then begin
        LogWarn(Format('Cannot find ancestor %s to select %s!', [Ancestor.ToString(), Id.ToString()]));
      end;
      Exit;
    end;
  end;

  while Assigned(Node) do begin
    if NodeData(Node).Id = Id then begin
      if LogHasDebug then begin
        LogDebug(Format('Select %s', [Id.ToString()]));
      end;
      // This will also update SelectedId.
      TreeViewNoteTree.Select(Node);
      break;
    end else begin
      Node := Node.GetNextSibling;
    end;
  end;
  if not Assigned(Node) then begin
    // Special case: root node.
    if Id = RootNodeData.Id then begin
      SelectedId := Id;
    end else if LogHasWarn then begin
      LogWarn(Format('Cannot find %s to select!', [Id.ToString()]));
    end;
  end;
end;

procedure TFormFooNoteMain.SetSelectedId(Id: FullId);
var
  AText, ReadOnly: string;
begin
  if Id <> FSelectedId then begin
    MemoNote.ReadOnly := True; // Disable MemoNote.OnChange
    if Assigned(SciEditNote) then begin
      SciEditNote.ReadOnly := True;
    end;
    // Special case: root node is not editable (see OnChange), and has empty text.
    if Id <> RootNodeData.Id then begin
      try
        AText := NoteBackend.GetText(Id);
        ReadOnly := NoteBackend.ExtractMeta(Id, 'readonly=');
      except
        on e: EExternal do begin
          if LogFFI.LogHasWarn then begin
            LogFFI.LogWarn(Format('Cannot SetSelectedId to %s. Fallback to Root Id', [Id.ToString()]));
          end;
          Id := RootNodeData.Id;
        end
      end;
    end;
    if MemoNote.Visible then begin
      MemoNote.Text := AText;
    end;
    if Assigned(SciEditNote) and SciEditNote.Visible then begin
      SciEditNote.Text := AText;
      SciEditNote.SetSavepoint;  // Clear Undo history
    end;
    if not (ReadOnly = 'true') then begin
      MemoNote.ReadOnly := False;
      if Assigned(SciEditNote) then begin
        SciEditNote.ReadOnly := False;
      end;
      if MemoNote.Lines.Count < 2 then begin
        MemoNote.SelStart := LazUtf8.UTF8Length(AText);
      end;
      MemoNote.Color := clWindow;
      if Assigned(SciEditNote) then begin
        SciEditNote.Color := clWindow;
      end;
    end else begin
      MemoNote.Color := clBtnFace;
      if Assigned(SciEditNote) then begin
        SciEditNote.Color := clBtnFace;
      end;
    end;
    FSelectedId := Id;
  end;
end;

procedure TFormFooNoteMain.SetSelectedIds(Ids: VecFullId);
var
  Item: TTreeNode;
  IdSet: TIdBoolMap;
  I: integer;
  B: boolean;
  Id: FullId;
  First: boolean = True;
begin
  IdSet := TIdBoolMap.Create;
  for I := 0 to Length(ids) - 1 do begin
    IdSet.Add(Ids[I], True);
  end;
  TreeViewNoteTree.ClearSelection();
  for Item in TreeViewNoteTree.Items do begin
    Id := TTreeNodeData(Item.Data).Id;
    B := False;
    IdSet.TryGetData(Id, B);
    item.MultiSelected := B;
    if B then begin
      if Assigned(item.Parent) and not IdSet.TryGetData(TTreeNodeData(Item.Parent.Data).Id, B) then begin
        item.Parent.Expanded := True;
      end;
      if First then begin
        SelectedId := Id;
        First := False;
      end;
      if LogHasDebug then begin
        LogDebug(format('Multi Select %s', [Id.ToString()]));
      end;
      // Mark as selected. This avoids expanding and selecting recusive nodes.
      IdSet.Remove(Id);
    end;
  end;
  FreeAndNil(IdSet);
end;

procedure TFormFooNoteMain.FormCreate(Sender: TObject);
begin
  SciEditNote := nil;

  InitLogFFI; // Run after AllocConsole for env_logger to detect colors.
  InitRootTreeUrlAndConfigFileName; // Affects config name.
  InitAppConfigLink; // Link Editor TFont to AppConfig's Font.
  LoadAppConfigFromDiskWithoutApply;
  InitI18n; // Used by many places. Need to be initialized first.
  InitRootBackend; // Open the backend. Make NoteBackend APIs working.
  InitPlatformSpecific; // Register OnConfigChange (affected by i18n).
  InitOnConfigChange;   // Register OnConfigChange.
  ApplyAppConfigToThisForm; // Apply non-callback (one-time) configs.
  AppConfig.NotifyAll; // Trigger "callback" to apply config changes.
end;

procedure TFormFooNoteMain.FormDestroy(Sender: TObject);
begin
  TreeViewNoteTree.Items.Clear;
  FreeAndNil(RootNodeData);
  NoteBackend.CloseAll;
end;

procedure TFormFooNoteMain.FormResize(Sender: TObject);
begin
  if (not AppConfig.MovingPreview) and (WindowState = wsNormal) then begin
    if AppConfig.DockSide = dsNone then begin
      AppConfig.NonDockWidth := Width;
      AppConfig.NonDockHeight := Height;
      if LogHasDebug then begin
        LogDebug(Format('  NonDockHeight := %d', [Height]));
      end;
      AppConfig.DockWidth := 0;
    end else begin
      AppConfig.DockWidth := Width;
    end;
  end;
end;

procedure TFormFooNoteMain.FormWindowStateChange(Sender: TObject);
begin
{$ifdef Windows}
  PlatformWindows.RepositionDock;
{$endif}
end;

function TFormFooNoteMain.NewMountNode(AText, Url: string; Copyable: boolean = True; NParent: integer = 1): FullId;
var
  Meta: string;
begin
  Meta := 'type=mount' + #10 + 'mount=' + Url + #10;
  if not Copyable then begin
    Meta += 'copyable=false'#10;
  end;
  Result := NewNode(AText, Meta, NParent);
end;

function TFormFooNoteMain.NewNode(AText, AMeta: string; NParent: integer = 0): FullId;
var
  P, Id: FullId;
  Index: integer;
begin
  P := InsertLocation(SelectedId, NParent, Index);
  Id := NoteBackend.InsertNode(P, Index, AText, AMeta);

  if LogHasDebug then begin
    LogDebug(Format('NewNode Id=%d Index=%d Meta=%s', [Id.Id, Index, AMeta.Trim]));
  end;
  RefreshFullTree;
  Result := Id;

  ScheduleAutoSave;
end;

procedure TFormFooNoteMain.ScheduleAutoSave;
begin
  TimerAutoSave.Enabled := False;
  TimerAutoSave.Enabled := True;
end;

procedure TFormFooNoteMain.ActionNewFolderExecute(Sender: TObject);
begin
  try
    SelectExpandNode(NewNode('', 'type=folder' + #10, 1));
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
  EditorSetFocus;
end;

procedure TFormFooNoteMain.ActionNewNoteExecute(Sender: TObject);
begin
  try
    SelectExpandNode(NewNode('', ''));
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
  EditorSetFocus;
end;

procedure TFormFooNoteMain.ActionNewSeparatorExecute(Sender: TObject);
begin
  try
    SelectExpandNode(NewNode('', 'type=separator' + #10 + 'readonly=true' + #10));
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
end;

procedure TFormFooNoteMain.MemoNoteChange(Sender: TObject);
begin
  if MemoNote.ReadOnly or not MemoNote.Visible then begin
    exit;
  end;
  if SelectedId = RootNodeData.Id then begin
    // Create a new node on demand.
    try
      SelectExpandNode(NewNode(MemoNote.Text, ''));
    except
      on e: EExternal do begin
        ErrorDlg(e.Message);
        exit;
      end;
    end;
    Exit;
  end;
  NoteBackend.TrySetText(SelectedId, MemoNote.Text);
  // Only refresh when changing the first line.
  if MemoNote.CaretPos.Y <= 1 then begin
    RefreshFullTree;
  end;
  // Schedule AutoSave
  ScheduleAutoSave;
  // Seems broken after Drag-and-drop move:
  // TreeViewSync.SyncTreeNode(TreeViewNoteTree.Selected);
end;

procedure TFormFooNoteMain.SciEditNoteChange(Sender: TObject);
begin
  if SciEditNote.ReadOnly or not SciEditNote.Visible then begin
    exit;
  end;
  if SelectedId = RootNodeData.Id then begin
    // Create a new node on demand.
    try
      SelectExpandNode(NewNode(SciEditNote.Text, ''));
    except
      on e: EExternal do begin
        ErrorDlg(e.Message);
        exit;
      end;
    end;
    Exit;
  end;
  NoteBackend.TrySetText(SelectedId, SciEditNote.Text);
  // Only refresh when changing the first line.
  if SciEditNote.CaretPos.Y <= 1 then begin
    RefreshFullTree;
  end;
  // Schedule AutoSave
  ScheduleAutoSave;
  // Seems broken after Drag-and-drop move:
  // TreeViewSync.SyncTreeNode(TreeViewNoteTree.Selected);
end;


procedure TFormFooNoteMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not NoteBackend.TryPersist() then begin
    if QuestionDlg('FooNote', RSFailSaveStillExit, mtWarning, [mrYes, RSExitNoSave, mrNo, RSNoExit], '') <>
      mrYes then begin
      CloseAction := caNone;
    end;
  end;
  ApplyThisFromToAppConfig;
  SaveConfigFile;
end;

procedure TFormFooNoteMain.ActionAppQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormFooNoteMain.ActionViewUndockExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsNone;
end;

procedure TFormFooNoteMain.ActionViewStayOnTopExecute(Sender: TObject);
begin
  AppConfig.StayOnTop := not AppConfig.StayOnTop;
end;

procedure TFormFooNoteMain.ActionViewToggleZenModeExecute(Sender: TObject);
begin
  AppConfig.ZenMode := not AppConfig.ZenMode;
end;

procedure TFormFooNoteMain.ActionViewDockLeftExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsLeft;
end;

procedure TFormFooNoteMain.PanelTopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and (not TopPanelMouseLeftIsDown) then begin
    TopPanelMouseLeftDownStartPos.X := X;
    TopPanelMouseLeftDownStartPos.Y := Y;
    TopPanelMouseLeftIsDown := True;
  end;
end;

procedure TFormFooNoteMain.PanelDockSplitterLeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and (not DockSplitterLeftIsDown) and (AppConfig.DockSide <> dsNone) then begin
    if LogHasDebug then begin
      LogDebug('Dock Splitter MouseDown');
    end;
    DockSplitterLeftDownOrigCursorPos := Mouse.CursorPos;
    DockSplitterLeftDownOrigWidth := Width;
    DockSplitterLeftDownOrigLeft := Left;
    DockSplitterLeftIsDown := True;
  end;
end;

procedure TFormFooNoteMain.ActionAppAboutExecute(Sender: TObject);
begin
  if AboutFooNoteForm = nil then begin
    Application.CreateForm(TAboutFooNoteForm, AboutFooNoteForm);
  end;
  AboutFooNoteForm.ShowModal;
end;

procedure TFormFooNoteMain.ActionEditReloadExecute(Sender: TObject);
begin
  if QuestionDlg('FooNote', RSReloadConfirm, mtWarning, [mrYes, RSYesReload, mrNo, RSNoReload], '') = mrYes then begin
    TreeViewNoteTree.Items.Clear;
    NoteBackend.CloseAll;
    InitRootBackend;
  end;
end;

procedure TFormFooNoteMain.ActionEncryptLockExecute(Sender: TObject);
var
  Id: FullId;
begin
  if Assigned(TreeViewNoteTree.Selected) then begin
    Id := SelectedId;
    if IsEncryptedFolder(Id) then begin
      if IsMounted(Id) then begin
        // Encrypt - Reset password to empty.
        TryUpdateMeta(Id, 'password=', '');
        RefreshFullTree;
        RefreshSelectedText;
      end;
    end;
  end;
end;

procedure TFormFooNoteMain.ActionEncryptUnlockExecute(Sender: TObject);
var
  Id: FullId;
  S: string;
begin
  if Assigned(TreeViewNoteTree.Selected) then begin
    Id := SelectedId;
    if IsEncryptedFolder(Id) then begin
      if not IsMounted(Id) then begin
        // Decrypt - Ask for password.
        PasswordForm.PrepareForm(False);
        if PasswordForm.FormPassword.ShowModal = mrOk then begin
          S := PasswordForm.PasswordResult;
          TryUpdateMeta(Id, 'password=', S);
          RefreshFullTree;
          RefreshSelectedText;
        end;
      end;
    end;
  end;
end;

procedure TFormFooNoteMain.ActionNewAes256Execute(Sender: TObject);
var
  S: string;
  Id: FullId;
begin
  PasswordForm.PrepareForm(True);
  if PasswordForm.FormPassword.ShowModal = mrOk then begin
    S := PasswordForm.PasswordResult;
    try
      // Not copyable to prevent IV reuse.
      Id := NewMountNode(RSEncryptedTextPrefix, Format('aes256:%s', [UniqueStringId]), False);
      TryUpdateMeta(Id, 'password=', S);
      SelectExpandNode(Id);
    except
      on e: EExternal do begin
        ErrorDlg(e.Message);
        exit;
      end;
    end;
  end;
end;

procedure TFormFooNoteMain.ActionNewGitExecute(Sender: TObject);
var
  Id: FullId;
  Url: string;
begin
  Url := InputBox(RSNewGitTitle, RSInputGitUrl, '');
  if Url.IsEmpty then begin
    exit;
  end;
  if UrlType(Url) <> 'git' then begin
    if UrlType(Url + '.git') = 'git' then begin
      Url := Url + '.git';
    end else begin
      ErrorDlg(Format(RSNotGitUrl, [Url]));
    end;
  end;

  try
    Id := NewMountNode(Url, Url);
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
  RefreshFullTree;
  SelectExpandNode(Id);
end;

procedure TFormFooNoteMain.ActionNewLocalFileExecute(Sender: TObject);
var
  B: boolean;
  S: string;
  Id: FullId;
begin
  // The save dialog might crash FooNote if WndProc is replaced.
  // Disable it temporarily.
  IdleTimerWndProc.Enabled := False;
  B := SaveDialogFooNote.Execute;
  IdleTimerWndProc.Enabled := True;
  if not B then begin
    exit;
  end;

  S := SaveDialogFooNote.FileName;
  if S.IsEmpty or (UrlType(S) <> 'foonote') then begin
    exit;
  end;

  try
    Id := NewMountNode(ExtractFileName(S), S);
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
  RefreshFullTree;
  SelectExpandNode(Id);
end;

procedure TFormFooNoteMain.ActionNewMemoryExecute(Sender: TObject);
var
  Id: FullId;
begin
  try
    Id := NewMountNode(RSMemoryRootTitle, Format('memory:%s', [UniqueStringId]), False);
  except
    on e: EExternal do begin
      ErrorDlg(e.Message);
      exit;
    end;
  end;
  RefreshFullTree;
  SelectExpandNode(Id);
end;

procedure TFormFooNoteMain.ActionToggleFolderExecute(Sender: TObject);
var
  S: string;
begin
  if SelectedId = RootNodeData.Id then begin
    exit;
  end;
  S := NoteBackend.ExtractMeta(SelectedId, 'type=');
  if S = 'folder' then begin
    if Length(NoteBackend.GetChildren(SelectedId)) = 0 then begin
      // Switch to text if the folder has no children.
      NoteBackend.TryUpdateMeta(SelectedId, 'type=', '');
    end;
  end else if S = '' then begin
    NoteBackend.TryUpdateMeta(SelectedId, 'type=', 'folder');
  end;
  RefreshFullTree;
end;

procedure TFormFooNoteMain.ActionViewWarnUnsavedExecute(Sender: TObject);
begin
  if not Assigned(FormSaveFailure) then begin
    Application.CreateForm(TFormSaveFailure, FormSaveFailure);
  end;
  FormSaveFailure.ShowModal;
end;

procedure TFormFooNoteMain.EditNoteSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then begin
    EditNoteSearch.Clear;
  end;
end;

procedure TFormFooNoteMain.EditNoteSearchKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (key = 10) or (key = 13) or (key = VK_DOWN) then begin
    if TreeViewSearchTree.Visible then begin
      if TreeViewSearchTree.CanSetFocus then begin
        TreeViewSearchTree.SetFocus;
      end;
    end else begin
      if TreeViewNoteTree.CanSetFocus then begin
        TreeViewNoteTree.SetFocus;
      end;
    end;
  end;
end;

procedure TFormFooNoteMain.DrawTreeSelectionPreview;
var
  PreviewCanvas: TCanvas;
  OrigSelectionColor, OrigBackColor: TColor;
  OrigScrollBars: TScrollStyle;
begin
  // ClientRect.Width does not include the scrollbar.
  PreviewCanvas := PreviewForm.PrepareCleanCanvas(TreeViewNoteTree.ClientRect.Width, TreeViewNoteTree.Height);

  TreeViewNoteTree.BeginUpdate;
  PaintingPreviewForSelectedTreeItems := True;
  OrigSelectionColor := TreeViewNoteTree.SelectionColor;
  OrigBackColor := TreeViewNoteTree.BackgroundColor;
  OrigScrollBars := TreeViewNoteTree.ScrollBars;

  // Temporarily change background color to avoid drawing white blank area.
  TreeViewNoteTree.BackgroundColor := clFuchsia;
  // Temporarily ensure selection color is with focus.
  TreeViewNoteTree.SelectionColor := clHighlight;
  TreeViewNoteTree.ScrollBars := ssAutoVertical;
  TreeViewNoteTree.EndUpdate;
  // Draw to preview canvas.
  TreeViewNoteTree.PaintTo(PreviewCanvas, 0, 0);

  // Restore colors.
  TreeViewNoteTree.BeginUpdate;
  TreeViewNoteTree.SelectionColor := OrigSelectionColor;
  TreeViewNoteTree.BackgroundColor := OrigBackColor;
  TreeViewNoteTree.ScrollBars := OrigScrollBars;
  PaintingPreviewForSelectedTreeItems := False;
  TreeViewNoteTree.EndUpdate;
end;

function TFormFooNoteMain.DockSplitterNewWidth: longint;
var
  Pos: TPoint;
  DX, NewWidth: longint;
begin
  Pos := Mouse.CursorPos;
  DX := Pos.X - DockSplitterLeftDownOrigCursorPos.X;
  if AppConfig.DockSide = dsRight then begin
    DX := -DX;
  end;
  NewWidth := DockSplitterLeftDownOrigWidth + DX;
  NewWidth := Max(NewWidth, Constraints.MinWidth);
  Result := NewWidth;
end;

procedure TFormFooNoteMain.PanelDockSplitterLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  NewWidth, PreviewLeft, SplitterWidth: longint;
begin
  if DockSplitterLeftIsDown then begin
    Assert(AppConfig.DockSide <> dsNone);
    // Do not use X, Y. They are relative to the form, which is changing.
    NewWidth := DockSplitterNewWidth;
    SplitterWidth := PanelDockSplitterLeft.Width;
    if LogHasTrace then begin
      LogTrace(Format('Dock Splitter MouseMove; NewWidth = %d', [NewWidth]));
    end;
    if AppConfig.DockSide = dsRight then begin
      PreviewLeft := DockSplitterLeftDownOrigLeft - NewWidth + DockSplitterLeftDownOrigWidth;
    end else begin
      PreviewLeft := Left + NewWidth - SplitterWidth;
    end;
    // Draw Preview using the preview form.
    // Drawing using the current form can be flaky (ex. when dsLeft, left != 0 but is width, why?).
    if Abs(NewWidth - DockSplitterLeftDownOrigWidth) > SplitterWidth then begin
      PreviewForm.ShowSolidColorAt(PreviewLeft, Top, SplitterWidth, Height);
    end else begin
      PreviewForm.Hide;
    end;
  end;
end;

procedure TFormFooNoteMain.PanelDockSplitterLeftMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  NewWidth: longint;
begin
  if DockSplitterLeftIsDown then begin
    Assert(AppConfig.DockSide <> dsNone);
    NewWidth := DockSplitterNewWidth;
    if LogHasDebug then begin
      LogDebug(Format('Dock Splitter MouseUp; DockWidth = %d', [NewWidth]));
    end;
    AppConfig.DockWidth := NewWidth;
    PreviewForm.Hide;
    DockSplitterLeftIsDown := False;
    {$ifdef Windows}
    PlatformWindows.RepositionDock;
    {$endif}
  end;
end;

procedure TFormFooNoteMain.ActionEditCopyExecute(Sender: TObject);
var
  T: string = '';
  B: TBytes;
  S: TBytesStream = nil;
  Ids: VecFullId;
  Id: FullId;
begin
  if TreeViewNoteTree.Focused then begin
    Ids := SelectedIds;
    for Id in Ids do begin
      T += NoteBackend.GetText(Id);
      if Length(Ids) > 1 then begin
        T += #10;
      end;
    end;
    ClipBoard.Open;
    try
      ClipBoard.Clear;
      // As text.
      ClipBoard.AsText := T;
      // As binary.
      try
        B := NoteBackend.CopyToBytes(Ids);
      except
        on e: EExternal do begin
          ErrorDlg(e.Message);
          exit;
        end;
      end;
      S := TBytesStream.Create(B);
      if LogHasDebug then begin
        LogDebug(Format('Copy binary len %d', [Length(B)]));
      end;
      ClipBoard.AddFormat(BinaryClipboardFormat, S);
    finally
      ClipBoard.Close;
      FreeAndNil(S);
    end;
  end else if MemoNote.Visible and MemoNote.Focused then begin
    MemoNote.CopyToClipboard;
  end else if Assigned(SciEditNote) and SciEditNote.Focused then begin
    SciEditNote.CopyToClipboard;
  end;
end;

procedure TFormFooNoteMain.ActionEditPasteExecute(Sender: TObject);
var
  Pos: integer = 1;
  DestId: FullId;
  T: string = '';
  L: integer;
  S: TBytesStream = nil;
  B: TBytes;
  Ids: VecFullId;
begin
  if TreeViewNoteTree.Focused then begin
    try
      S := TBytesStream.Create;
      if ClipBoard.HasFormat(BinaryClipboardFormat) and ClipBoard.GetFormat(BinaryClipboardFormat, S) then begin
        L := S.Position;
        if LogHasDebug then begin
          LogDebug(Format('Paste binary len %d', [L]));
        end;
        B := S.bytes;
        SetLength(B, L);
        DestId := InsertLocation(SelectedId, 0, Pos);
        try
          Ids := NoteBackend.PasteFromBytes(DestId, Pos, B);
        except
          on e: EExternal do begin
            ErrorDlg(e.Message);
            exit;
          end;
        end;
        RefreshFullTree;
        // For some reason, accessing Selected can SIGSEGV here? RootId?
        // TreeViewNoteTree.Selected.Expanded := True;
        SelectedIds := Ids;
        ScheduleAutoSave;
      end else begin
        // Paste into a new node.
        T := ClipBoard.AsText;
        if not T.IsEmpty then begin
          try
            SelectExpandNode(NewNode(T, ''));
          except
            on e: EExternal do begin
              ErrorDlg(e.Message);
              exit;
            end;
          end;
          if MemoNote.Visible then begin
            MemoNote.SetFocus;
          end else if Assigned(SciEditNote) and SciEditNote.Visible then begin
            SciEditNote.SetFocus;
          end;
        end;

      end;
    finally
      FreeAndNil(S);
    end;
  end else begin
    if MemoNote.Visible and MemoNote.Focused then begin
      MemoNote.PasteFromClipboard;
    end else if Assigned(SciEditNote) and SciEditNote.Visible then begin
      SciEditNote.PasteFromClipboard;
    end;
    // Might cause title change.
    RefreshFullTree;
  end;
end;

procedure TFormFooNoteMain.FormShow(Sender: TObject);
begin
  // Setting SplitterTreeNote.Top in FormCreate might be updated (due to autosize?).
  // Set it again here.
  LoadSplitterPosition;

  UpdateTitle;

  // Make the preview window "visible" so it won't steal the focus.
  {$ifdef Windows}
  PreviewForm.EnsureInit;
  {$endif}
end;

procedure TFormFooNoteMain.UpdateTitle;
begin
  // Update Form Title. Does not seem effective in FormCreate.
  if AppConfig.RootTreeUrl <> DefaultUrl then begin
    Caption := Format('FooNote (%s)', [ExtractFileName(AppConfig.RootTreeUrl)]);
  end else begin
    Caption := 'FooNote';
  end;
end;

procedure TFormFooNoteMain.IdleTimerWndProcTimer(Sender: TObject);
begin
  {$ifdef Windows}
  // HACK: For some reason, LCL rewrites WndProc periodically.
  PlatformWindows.EnsureWrappedWndProc;
  {$endif}
end;

procedure TFormFooNoteMain.ActionNewMountUrlExecute(Sender: TObject);
var
  url: string;
  Id: FullId;
begin
  if not Assigned(FormOpenUrl) then begin
    Application.CreateForm(TFormOpenUrl, FormOpenUrl);
  end;

  if FormOpenUrl.ShowModal = mrOk then begin
    url := FormOpenUrl.EditUrl.Text;
    if not url.IsEmpty then begin
      try
        Id := NewNode(url, 'type=mount' + #10 + 'mount=' + url + #10, 0);
      except
        on e: EExternal do begin
          ErrorDlg(e.Message);
          exit;
        end;
      end;
      RefreshFullTree;
      SelectExpandNode(Id);
    end;
  end;
end;

procedure TFormFooNoteMain.EditNoteSearchChange(Sender: TObject);
var
  T: string;
begin
  T := EditNoteSearch.Text;
  if T.IsEmpty then begin
    NoteBackend.StopSearch();
    TimerSearchResult.Enabled := False;
    TreeViewSearchTree.Visible := False;
    TreeViewNoteTree.Visible := True;
    TreeViewSearchTree.Items.Clear;
    LastSearchText := '';
  end else begin
    NoteBackend.StartSearch([RootNodeData.Id], T);
    TimerSearchResult.Enabled := True;
    TreeViewNoteTree.Visible := False;
    TreeViewSearchTree.Visible := True;
  end;
end;

procedure TFormFooNoteMain.TimerAutoRefreshTimer(Sender: TObject);
begin
  RefreshFullTree;
end;

procedure TFormFooNoteMain.TimerAutoSaveTimer(Sender: TObject);
begin
  ActionEditSave.Execute;
end;

procedure TFormFooNoteMain.TimerCheckSaveResultTimer(Sender: TObject);
var
  SaveErrno: Int32;
  Message: string;
begin
  if NoteBackend.TryPersistAsyncWait(Message, SaveErrno) then begin
    // Save thread completed. Result in (Message, SaveErrno).
    if SaveErrno <> StackFFI.OK then begin
      if LogHasError then begin
        LogError(Format('Save failed: %d %s', [SaveErrno, Message]));
      end;
      ActionViewWarnUnsaved.Visible := True;
      AppConfig.SaveFailureMessage := Message;
    end else begin
      if LogHasInfo then begin
        LogInfo('Save succeeded');
      end;
      ActionViewWarnUnsaved.Visible := False;
      AppConfig.SaveFailureMessage := '';
    end;
    TreeViewNoteTree.Cursor := crDefault;
    ActionEditSave.Enabled := True;
    TimerCheckSaveResult.Enabled := False;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
var
  C: TCanvas;
  H, I, Y: longint;
  R: TRect;
  Selected: boolean;
const
  TriangleSize = 5;
  BorderLineWidth = 2;
begin
  if PaintingPreviewForSelectedTreeItems then begin
    // Skip items not selected.
    if not node.Selected then begin
      DefaultDraw := False;
      exit;
    end;
  end;

  // Separator
  if (Stage = cdPrePaint) and (TTreeNodeData(Node.Data).ExtractMeta('type=') = 'separator') then begin
    C := Sender.Canvas;
    H := (Sender.DefaultItemHeight div 2);
    Node.Height := H;
    R := Node.DisplayRect(False);
    Y := R.Top;
    Selected := Node.MultiSelected or Node.Selected;
    // Clear
    if Selected then begin
      C.Brush.Color := Sender.SelectionColor;
    end else begin
      C.Brush.Color := Sender.BackgroundColor;
    end;
    C.FillRect(0, R.Top, C.Width, R.Bottom);
    // Draw a line
    if Selected then begin
      C.Pen.Color := clHighlightText;
    end else begin
      C.Pen.Color := Sender.SeparatorColor;
    end;
    C.Pen.Style := psDot;
    C.Line(Node.DisplayExpandSignRect.Right, Y + H div 2, C.Width, Y + H div 2);
    C.Pen.Style := psSolid;
    Stage := cdPostPaint; // Trigger "dragging marker" drawing below.
    DefaultDraw := False;
  end else begin
    if Node.Height <> Sender.DefaultItemHeight then begin
      Node.Height := Sender.DefaultItemHeight;
    end;
  end;

  // Draw markers for DragOver destination.
  if TreeViewNoteTree.Dragging and (Stage = cdPostPaint) and (CandidateDropNode = Node) and
    (not PaintingPreviewForSelectedTreeItems) then begin
    C := Sender.Canvas;
    H := Node.Height;
    R := Node.DisplayRect(False);
    R.Left := Node.DisplayExpandSignRect.Right;
    C.Pen.Width := 1;
    if CandidateInsertPosition = 0 then begin
      // Draw a box.
      C.Pen.Color := clHighlightText;
      C.Frame(R);
      C.Pen.Color := clBtnText;
      for I := 1 to BorderLineWidth do begin
        R.Inflate(-1, -1);
        C.Frame(R);
      end;
      C.Pen.Color := clHighlightText;
      R.Inflate(-1, -1);
      C.Frame(R);
    end else begin
      // Draw a line with a small triangle indicator.
      if CandidateInsertPosition = -1 then begin
        Y := R.Top;
      end else begin
        Y := R.Bottom - 1;
      end;
      C.Pen.Color := clHighlightText;
      C.Line(R.Left - 1, Y, R.Left - 1, Y - (TriangleSize + BorderLineWidth + 2) * CandidateInsertPosition);
      C.Line(R.Left, Y, R.Right, Y);
      Y -= CandidateInsertPosition * (BorderLineWidth + 1);
      C.Line(R.Left, Y, R.Right, Y);
      I := TriangleSize;
      C.Line(R.Left, Y - CandidateInsertPosition * I, R.Left + I, Y);
      C.Pen.Color := clBtnText;
      Y += CandidateInsertPosition * (BorderLineWidth + 1);
      for I := 1 to BorderLineWidth do begin
        Y -= CandidateInsertPosition;
        C.Line(R.Left, Y, R.Right, Y);
      end;
      for I := 1 to TriangleSize do begin
        C.Line(R.Left, Y - CandidateInsertPosition * I, R.Left + I, Y);
        C.Line(R.Left, Y - CandidateInsertPosition * I, R.Left + I, Y);
      end;
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeContextPopup(Sender: TObject; MousePos: TPoint; var Handled: boolean);
begin
  UpdatePopupMenu;
end;

procedure TFormFooNoteMain.UpdatePopupMenu();
var
  LockEnabled, UnlockEnabled: boolean;
  Id: FullId;
begin
  LockEnabled := False;
  UnlockEnabled := False;

  if Assigned(TreeViewNoteTree.Selected) then begin
    Id := SelectedId;
    if IsEncryptedFolder(Id) then begin
      if IsMounted(Id) then begin
        LockEnabled := True;
      end else begin
        UnlockEnabled := True;
      end;
    end;
  end;
  ActionEncryptLock.Enabled := LockEnabled;
  ActionEncryptLock.Visible := LockEnabled;
  ActionEncryptUnlock.Enabled := UnlockEnabled;
  ActionEncryptUnlock.Visible := UnlockEnabled;
  MenuItemLockUnlockSep.Visible := UnlockEnabled or LockEnabled;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeDblClick(Sender: TObject);
var
  Id: FullId;
begin
  if Assigned(TreeViewNoteTree.Selected) then begin
    Id := SelectedId;
    if IsEncryptedFolder(Id) then begin
      if not IsMounted(Id) then begin
        ActionEncryptUnlockExecute(Sender);
        Exit;
      end;
    end;
    // Focus note (including folders).
    FocusEditorEnd;
  end;
end;

procedure TFormFooNoteMain.FocusEditorEnd;
begin
  if MemoNote.Visible and MemoNote.CanSetFocus and not MemoNote.ReadOnly then begin
    MemoNote.SelStart := 32767;
    MemoNote.SetFocus;
  end else if Assigned(SciEditNote) and not SciEditNote.ReadOnly then begin
    SciEditNote.SelStart := -1; // Select the end. See SCI_SETSEL
    SciEditNote.SetFocus;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeDragDrop(Sender, Source: TObject; X, Y: integer);
var
  Ids: VecFullId;
  DestId: FullId;
begin
  if LogHasDebug then begin
    LogDebug('DragDrop');
  end;
  if CandidateDropNode <> nil then begin
    DestId := NodeData(CandidateDropNode).Id;
    // Move selections to the drop location.
    Ids := SelectedIds;

    try
      NoteBackend.SetParentNode(Ids, DestId, CandidateInsertPosition);
    except
      on e: EExternal do begin
        ErrorDlg(e.Message);
        exit;
      end;
    end;

    RefreshFullTree;
    SelectedIds := Ids;
    ScheduleAutoSave;
  end;
end;

function TFormFooNoteMain.GetSelectedIds(): VecFullId;
var
  I, C: integer;
begin
  Result := VecFullId.Create;
  C := TreeViewNoteTree.Items.SelectionCount;
  if C > 0 then begin
    SetLength(Result, C);
    for I := 0 to TreeViewNoteTree.Items.SelectionCount - 1 do begin
      Result[I] := TTreeNodeData(TreeViewNoteTree.Selections[I].Data).Id;
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeDragOver(Sender, Source: TObject; X, Y: integer;
  State: TDragState; var Accept: boolean);
var
  N: TTreeNode;
  P: longint = 0;
  R: TRect;
  D: TTreeNodeData;
begin
  if (Source = TreeViewNoteTree) and DragOverHappenedAndPreviewDrawn then begin
    Accept := True;
    N := TreeViewNoteTree.GetNodeAt(X, Y);
    if Assigned(N) and (N.Selected or N.MultiSelected) then begin
      // Do not use selected nodes as DnD targets.
      N := nil;
    end;
    if N <> nil then begin
      R := N.DisplayRect(False);
      D := TTreeNodeData(N.Data);
      if D.IsFolder() then begin
        // 3 areas: insert before; insert into (this folder); insert after
        if Y < R.Top + R.Height div 3 then begin
          P := -1;
        end else if Y < R.Top + R.Height * 2 div 3 then begin
          P := 0;
        end else begin
          P := 1;
        end;
      end else begin
        // 2 areas: insert before; insert after
        if Y >= R.CenterPoint.Y then begin
          P := 1;
        end else begin
          P := -1;
        end;
      end;
    end;
    if not ((CandidateDropNode = N) and (CandidateInsertPosition = P)) then begin
      CandidateDropNode := N;
      CandidateInsertPosition := P;
      TreeViewNoteTree.Repaint;
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  // This can be called if drag was aborted by the ESC key.
  // In that case, the DnD shouldn't be handled.
  // The real DnD handling is OnDragDrop.
  if Assigned(CandidateDropNode) then begin
    CandidateDropNode := nil;
    TreeViewNoteTree.Repaint;
  end;
  PreviewForm.Hide;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeEnter(Sender: TObject);
var
  Node: TTreeNode;
begin
  TreeViewNoteTree.SelectionColor := clHighlight;
  ActionEditDelete.Enabled := True;
  ActionEditCopy.Enabled := True;
  ActionEditPaste.Enabled := True;
  if not Assigned(TreeViewNoteTree.Selected) then begin
    Node := TreeViewNoteTree.Items.GetFirstNode;
    if Assigned(Node) then begin
      TreeViewNoteTree.Select(Node);
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeExit(Sender: TObject);
begin
  TreeViewNoteTree.SelectionColor := clSilver;
  ActionEditDelete.Enabled := False;
  ActionEditCopy.Enabled := False;
  ActionEditPaste.Enabled := False;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeKeyPress(Sender: TObject; var Key: char);

begin
  if (Key = '/') or (Key = #27) and EditNoteSearch.CanSetFocus then begin
    // Focus on search.
    EditNoteSearch.SetFocus;
    Key := #0; // Marked as already handled.
  end else if (key = '.') then begin
    UpdatePopupMenu;
    TreeViewNoteTree.PopupMenu.PopUp;
  end else if (Key = #10) or (Key = #13) then begin
    TreeViewNoteTreeDblClick(Sender);
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Point: TPoint;
begin
  if TreeViewNoteTree.Dragging then begin
    if not DragOverHappened then begin
      // Record the start position.
      StartDragCursorPos := Mouse.CursorPos;
      StartDragLocalPos := TPoint.Create(X, Y);
      DragOverHappened := True;
      DragOverHappenedAndPreviewDrawn := False;
    end else begin
      Point := (Mouse.CursorPos - StartDragLocalPos);
      if not DragOverHappenedAndPreviewDrawn then begin
        // Should we draw preview?
        if Point.Distance(Point.Zero) > StartDragThreshold then begin
          // Take a "screenshot" of the tree view.
          // Do not do it in OnStartDrag to reduce cost.
          if LogHasDebug then begin
            LogDebug('Render Selection Preview');
          end;
          DrawTreeSelectionPreview;
          DragOverHappenedAndPreviewDrawn := True;
        end;
      end else begin
        // Show preview at the right position.
        PreviewForm.ShowCanvasAt(Point.X, Point.Y);
      end;
    end;
  end else begin
    if Assigned(TreeViewNoteTree.GetNodeAt(X, Y)) then begin
      TreeViewNoteTree.DragMode := dmAutomatic;
    end else begin
      TreeViewNoteTree.DragMode := dmManual;
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not TreeViewNoteTree.Dragging then begin
    PreviewForm.Hide;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  DragOverHappened := False;
  DragOverHappenedAndPreviewDrawn := False;
end;

procedure TFormFooNoteMain.TreeViewSearchTreeDblClick(Sender: TObject);
begin
  SelectExpandNode(SelectedId);
  EditNoteSearch.Clear;
  TreeViewNoteTreeDblClick(Sender);
end;

procedure TFormFooNoteMain.TreeViewSearchTreeEnter(Sender: TObject);
var
  Node: TTreeNode;
begin
  // Attempt to select one item
  if not Assigned(TreeViewSearchTree.Selected) then begin
    Node := TreeViewSearchTree.Items.GetFirstNode;
    if Assigned(Node) then begin
      TreeViewSearchTree.Select(Node);
    end;
  end;
end;

procedure TFormFooNoteMain.TreeViewSearchTreeKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #27) then begin
    Key := #0;
    EditNoteSearch.SetFocus;
    EditNoteSearch.SelStart := 32767;
  end;
  if (key = #10) or (key = #13) then begin
    TreeViewSearchTreeDblClick(Sender);
    FocusEditorEnd;
  end;
end;

procedure TFormFooNoteMain.TreeViewSearchTreeSelectionChanged(Sender: TObject);
var
  Id: FullId;
begin
  if Assigned(TreeViewSearchTree.Selected) then begin
    Id := NodeData(TreeViewSearchTree.Selected).Id;
    SelectedId := Id;
    SelectedIds := [Id];
    // Do not expand it here. Expand it on double click or ENTER // SelectExpandNode(Id);
  end;
end;

procedure TFormFooNoteMain.TimerSearchResultTimer(Sender: TObject);
var
  S: FullIdText;
  N: TTreeNode;
  D: TTreeNodeData;
  SearchText: string;
begin
  SearchText := NoteBackend.GetSearchInput();
  if LastSearchText <> SearchText then begin
    TreeViewSearchTree.Items.Clear;
    LastSearchText := SearchText;
  end;
  TimerSearchResult.Enabled := not NoteBackend.IsSearchComplete();
  if not TimerSearchResult.Enabled then begin
    if LogHasDebug then begin
      LogDebug('Search Completed');
    end;
  end;
  N := TreeViewSearchTree.Items.GetLastNode;
  for S in NoteBackend.GetSearchResult(TreeViewSearchTree.Items.Count) do begin
    if LogHasTrace then begin
      LogTrace(Format('Search Result: %s (%s)', [S.Text, S.Id.ToString()]));
    end;
    N := TreeViewSearchTree.Items.Add(N, '');
    D := TTreeNodeData.Create(S.Id);
    D.SyncFromBackend();
    D.SearchText := S.Text;
    TreeViewSync.ApplyData(N, D);
    N.Data := D;
  end;
end;

procedure TFormFooNoteMain.PanelTopMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
const
  THRESHOLD = 30;
begin
  if TopPanelMouseLeftIsDown and (TopPanelMouseLeftDownStartPos.Distance(TPoint.Create(X, Y)) > THRESHOLD) then begin
    TopPanelMouseLeftIsDown := False;
    AppConfig.DockSide := dsNone;
    {$ifdef Windows}
    PlatformWindows.StartMoving(Self);
    {$endif}
  end;
end;

procedure TFormFooNoteMain.PanelTopMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then begin
    TopPanelMouseLeftIsDown := False;
  end;
end;

procedure TFormFooNoteMain.SplitterTreeNoteMoved(Sender: TObject);
begin
  if AppConfig.DockSide = dsNone then begin
    AppConfig.NonDockNoteSplitTop := SplitterTreeNote.Top;
    AppConfig.DockNoteSplitTop := 0;
  end else begin
    AppConfig.DockNoteSplitTop := SplitterTreeNote.Top;
    // AppConfig.NonDockNoteSplitTop := 0;
  end;
end;

procedure TFormFooNoteMain.ActionViewDockRightExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsRight;
end;

procedure TFormFooNoteMain.EditDeleteExecute(Sender: TObject);
var
  ToSelect: FullId;
begin
  if TreeViewNoteTree.Focused then begin
    ToSelect := NoteBackend.GetPrevious(SelectedId);
    try
      NoteBackend.RemoveNodes(SelectedIds);
    except
      on e: EExternal do begin
        ErrorDlg(e.Message);
        exit;
      end;
    end;
    // Select the old parent node.
    SelectExpandNode(ToSelect);
    RefreshFullTree;
    ScheduleAutoSave;
  end;
end;

procedure TFormFooNoteMain.ActionEditSaveExecute(Sender: TObject);
var
  Scheduled: boolean;
begin
  Scheduled := NoteBackend.TryPersistAsync();
  if Scheduled then begin
    if LogHasDebug then begin
      LogDebug('Async save started');
    end;
  end else begin
    if LogHasWarn then begin
      LogWarn('Failed to start async save');
    end;
  end;

  if Scheduled then begin
    // Prevent stacking the save threads.
    ActionEditSave.Enabled := False;

    // Change cursor to "busy".
    TreeViewNoteTree.Cursor := crAppStart;

    // TimerCheckSaveResult will re-enable ActionEditSave.
    TimerCheckSaveResult.Enabled := True;

    // Cancel auto-save. Auto-save will be re-enabled by text editing.
    TimerAutoSave.Enabled := False;
  end;
end;

procedure TFormFooNoteMain.MemoNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then begin
    // ESC - lose focus, or exit ZenMode
    if AppConfig.ZenMode then begin
      AppConfig.ZenMode := False;
    end else begin
      TreeViewNoteTree.SetFocus;
    end;
    Key := 0; // Mark as handled.
    exit;
  end;
  MemoUtil.SmartKeyDown(MemoNote, Key, Shift);
end;

procedure TFormFooNoteMain.SciEditNoteKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then begin
    // ESC - lose focus, or exit ZenMode
    if AppConfig.ZenMode then begin
      AppConfig.ZenMode := False;
    end else begin
      TreeViewNoteTree.SetFocus;
    end;
    exit;
  end;
end;

procedure TFormFooNoteMain.EditorSetFocus;
begin
  if MemoNote.Visible then begin
    MemoNote.SetFocus;
  end;
  if Assigned(SciEditNote) and SciEditNote.Visible then begin
    SciEditNote.SetFocus;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data <> nil then begin
    TTreeNodeData(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
begin
  TreeViewSync.SyncTreeNode(Node, True);
end;

procedure TFormFooNoteMain.TreeViewNoteTreeSelectionChanged(Sender: TObject);
var
  Selected: TTreeNode;
begin
  Selected := TreeViewNoteTree.Selected;
  if Selected = nil then begin
    exit;
  end;
  SelectedId := TTreeNodeData(Selected.Data).Id;
end;

procedure TFormFooNoteMain.ActionAppSettingPreferencesExecute(Sender: TObject);
begin
  if FormFooNoteSettings = nil then begin
    Application.CreateForm(TFormFooNoteSettings, FormFooNoteSettings);
  end;
  FormFooNoteSettings.ShowModal;
end;

initialization

  BinaryClipboardFormat := ClipBrd.RegisterClipboardFormat('FooNote Binary');

end.
