unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  PlatformWindows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, ActnList, PairSplitter, StdActns, ClipBrd, LCLType,
  LazUtf8, FGL, LogFFI, Math, NoteBackend, NoteTypes, MemoUtil,
  LCLTranslator, Buttons, JSONPropStorage, TreeNodeData, StackFFI,
  TreeViewSync, Settings, PreviewForm, AboutForm, FileUtil, md5,
  savemsgform;

type

  { TFormFooNoteMain }

  TFormFooNoteMain = class(TForm)
    ActionViewWarnUnsaved: TAction;
    ActionEditReload: TAction;
    MenuItem10: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItemRootPath: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem6: TMenuItem;
    PanelZen: TPanel;
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
    MenuItemMount: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    ActionMountUrl: TAction;
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
    procedure ActionViewWarnUnsavedExecute(Sender: TObject);
    procedure PanelDockSplitterLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure PanelDockSplitterLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PanelDockSplitterLeftMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerWndProcTimer(Sender: TObject);
    procedure ActionMountUrlExecute(Sender: TObject);
    procedure EditNoteSearchChange(Sender: TObject);
    procedure EditNoteSearchKeyPress(Sender: TObject; var Key: char);
    procedure TimerAutoSaveTimer(Sender: TObject);
    procedure TimerCheckSaveResultTimer(Sender: TObject);
    procedure TreeViewNoteTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
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
  private
    // NoteTreeView state.
    RootNodeData: TTreeNodeData;
    FSelectedId: FullId;

    // NoteTreeView drag-n-drop state.
    CandidateDropNode: TTreeNode;
    CandidateInsertPosition: integer; // 0: inside; -1: before; 1: after.
    DragOverHappened: boolean;
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
    function InsertLocation(Id: FullId; NParent: integer; out Pos: integer): FullId;
    function NewNode(AText, AMeta: string; NParent: integer = 0): FullId;
    procedure SelectExpandNode(Id: FullId);
    procedure SetSelectedId(Id: FullId);
    function GetSelectedIds(): VecFullId;
    procedure SetSelectedIds(Ids: VecFullId);
    procedure DrawTreeSelectionPreview;
    property SelectedId: FullId read FSelectedId write SetSelectedId;
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
  RSFailOpenUrl = 'Failed to open [%s]: %s. What to do?';
  RSOpenFallbackNote = 'Open Fallback Note';
  RSExit = 'Exit';

implementation

var
  BinaryClipboardFormat: TClipboardFormat;

const
  DefaultUrl: string = 'Default.FooNote';

{$R *.lfm}

type
  TIdBoolMap = specialize TFPGMap<FullId, boolean>;

{ TFormFooNoteMain }

procedure TFormFooNoteMain.RefreshFullTree;
begin
  if RootNodeData.SyncFromBackend() then begin
    // Change detected. Refresh the tree.
    TreeViewNoteTree.BeginUpdate;
    TreeViewSync.SyncChildTreeNode(TreeViewNoteTree, nil, TreeViewNoteTree.Items.GetFirstNode,
      RootNodeData, True);
    TreeViewNoteTree.EndUpdate;
  end else begin
    if LogHasDebug then begin
      LogDebug('Root Note Not changed');
    end;
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
      if QuestionDlg('FooNote', Format(RSFailOpenUrl, [Url, e.Message]), mtWarning,
        [mrYes, RSOpenFallbackNote, mrNo, RSExit], '') = mrNo then begin
        Close;
      end;
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
        This.FormStyle := fsSystemStayOnTop;
      end;
    end else begin
      if This.FormStyle <> fsNormal then begin
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
  if (Name = AnyConfigName) or (Name = 'FeatureLevel') then begin
    B := (Config.FeatureLevel >= flAdvanced);
    This.MenuSepMount.Visible := B;
    This.MenuItemMount.Visible := B;
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
    end else begin
      This.MemoNote.Parent := This.PanelEdit;
    end;
    if not This.MemoNote.Focused and This.MemoNote.CanSetFocus then begin
      This.MemoNote.SetFocus;
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
      on e: EParserError do ;
    end;
  end;
end;

procedure TFormFooNoteMain.InitI18n;
begin
  SetDefaultLang(AppConfig.Locale, 'locale');
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
  S: string;
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
  if Assigned(TreeViewNoteTree.Selected) and (NodeData(TreeViewNoteTree.Selected).Id = Id) then begin
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
      TreeViewNoteTree.Select(Node);
      SelectedId := Id;
      break;
    end else begin
      Node := Node.GetNextSibling;
    end;
  end;
  if not Assigned(Node) then begin
    if LogHasWarn then begin
      LogWarn(Format('Cannot find %s to select!', [Id.ToString()]));
    end;
  end;
end;

procedure TFormFooNoteMain.SetSelectedId(Id: FullId);
var
  AText, ReadOnly: string;
begin
  FSelectedId := Id;
  MemoNote.ReadOnly := True; // Disable MemoNote.OnChange
  AText := NoteBackend.GetText(Id);
  MemoNote.Text := AText;
  ReadOnly := NoteBackend.ExtractMeta(Id, 'readonly=');
  if not (ReadOnly = 'true') then begin
    MemoNote.ReadOnly := False;
    if MemoNote.Lines.Count < 2 then begin
      MemoNote.SelStart := LazUtf8.UTF8Length(AText);
    end;
    MemoNote.Color := clWindow;
  end else begin
    MemoNote.Color := clBtnFace;
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
    item.MultiSelected := IdSet.TryGetData(Id, B);
    if B then begin
      if Assigned(item.Parent) and not IdSet.TryGetData(TTreeNodeData(Item.Parent.Data).Id, B) then begin
        item.Parent.Expanded := True;
      end;
      if First then begin
        item.Selected := True;
        FSelectedId := Id;
        First := False;
      end;
    end;
  end;
  FreeAndNil(IdSet);
end;

procedure TFormFooNoteMain.FormCreate(Sender: TObject);
begin
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
  SelectExpandNode(NewNode('', 'type=folder' + #10, 1));
  MemoNote.SetFocus;
end;

procedure TFormFooNoteMain.ActionNewNoteExecute(Sender: TObject);
begin
  SelectExpandNode(NewNode('', ''));
  MemoNote.SetFocus;
end;

procedure TFormFooNoteMain.ActionNewSeparatorExecute(Sender: TObject);
begin
  SelectExpandNode(NewNode('', 'type=separator' + #10 + 'readonly=true' + #10));
end;

procedure TFormFooNoteMain.MemoNoteChange(Sender: TObject);
begin
  if MemoNote.ReadOnly then begin
    exit;
  end;
  if SelectedId.Id = 0 then begin
    // Create a new node on demand.
    SelectExpandNode(NewNode(MemoNote.Text, ''));
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

procedure TFormFooNoteMain.ActionViewWarnUnsavedExecute(Sender: TObject);
begin
  if not Assigned(FormSaveFailure) then begin
    Application.CreateForm(TFormSaveFailure, FormSaveFailure);
  end;
  FormSaveFailure.ShowModal;
end;

procedure TFormFooNoteMain.DrawTreeSelectionPreview;
var
  PreviewCanvas: TCanvas;
  OrigSelectionColor, OrigBackColor: TColor;
begin
  // ClientRect.Width does not include the scrollbar.
  PreviewCanvas := PreviewForm.PrepareCleanCanvas(TreeViewNoteTree.ClientRect.Width, TreeViewNoteTree.Height);

  TreeViewNoteTree.BeginUpdate;
  PaintingPreviewForSelectedTreeItems := True;
  OrigSelectionColor := TreeViewNoteTree.SelectionColor;
  OrigBackColor := TreeViewNoteTree.BackgroundColor;

  // Temporarily change background color to avoid drawing white blank area.
  TreeViewNoteTree.BackgroundColor := clFuchsia;
  // Temporarily ensure selection color is with focus.
  TreeViewNoteTree.SelectionColor := clHighlight;
  TreeViewNoteTree.EndUpdate;
  // Draw to preview canvas.
  TreeViewNoteTree.PaintTo(PreviewCanvas, 0, 0);

  // Restore colors.
  TreeViewNoteTree.BeginUpdate;
  TreeViewNoteTree.SelectionColor := OrigSelectionColor;
  TreeViewNoteTree.BackgroundColor := OrigBackColor;
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
      B := NoteBackend.CopyToBytes(Ids);
      S := TBytesStream.Create(B);
      if LogHasDebug then begin
        LogDebug(Format('Copy binary len %d', [Length(B)]));
      end;
      ClipBoard.AddFormat(BinaryClipboardFormat, S);
    finally
      ClipBoard.Close;
      FreeAndNil(S);
    end;
  end else if MemoNote.Focused then begin
    MemoNote.CopyToClipboard;
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
        Ids := NoteBackend.PasteFromBytes(DestId, Pos, B);
        RefreshFullTree;
        // For some reason, accessing Selected can SIGSEGV here? RootId?
        // TreeViewNoteTree.Selected.Expanded := True;
        SelectedIds := Ids;
        ScheduleAutoSave;
      end else begin
        T := ClipBoard.AsText;
        if not T.IsEmpty then begin
          SelectExpandNode(NewNode(T, ''));
          MemoNote.SetFocus;
        end;
      end;
    finally
      FreeAndNil(S);
    end;
  end else if MemoNote.Focused then begin
    MemoNote.PasteFromClipboard;
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

procedure TFormFooNoteMain.ActionMountUrlExecute(Sender: TObject);
var
  url: string;
  Id: FullId;
begin
  Id := SelectedId;
  if Id = RootNodeData.Id then begin
    exit;
  end;
  url := InputBox('Mount', 'URL', '1.foonote');
  if url.IsEmpty then begin
    exit;
  end;
  if NoteBackend.TryMount(Id, url) then begin
    NoteBackend.TryUpdateMeta(Id, 'type=', 'mount');
    RefreshFullTree;
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

procedure TFormFooNoteMain.EditNoteSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then begin
    EditNoteSearch.Clear;
  end else if (key = #10) or (key = #13) then begin
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
  if (Stage = cdPrePaint) and (Node.Text.IsEmpty() or Node.Text.StartsWith('-')) and
    (TTreeNodeData(Node.Data).ExtractMeta('type=') = 'separator') then begin
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

procedure TFormFooNoteMain.TreeViewNoteTreeDblClick(Sender: TObject);
begin
  if Assigned(TreeViewNoteTree.Selected) and not IsFolder(NodeData(TreeViewNoteTree.Selected).Id) then begin
    FocusEditorEnd;
  end;
end;

procedure TFormFooNoteMain.FocusEditorEnd;
begin
  if MemoNote.CanSetFocus and not MemoNote.ReadOnly then begin
    MemoNote.SelStart := 32767;
    MemoNote.SetFocus;
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
    if NoteBackend.TrySetParent(Ids, DestId, CandidateInsertPosition) then begin
      RefreshFullTree;
      SelectedIds := Ids;
      ScheduleAutoSave;
    end;
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
  if Source = TreeViewNoteTree then begin
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
  if CandidateDropNode <> nil then begin
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
    TreeViewNoteTree.PopupMenu.PopUp;
  end else if (Key = #10) or (Key = #13) then begin
    FocusEditorEnd;
  end;
end;

procedure TFormFooNoteMain.TreeViewNoteTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Point: TPoint;
begin
  if TreeViewNoteTree.Dragging then begin
    if not DragOverHappened then begin
      // Take a "screenshot" of the tree view.
      // Do not do it in OnStartDrag to reduce cost.
      if LogHasDebug then begin
        LogDebug('Render Selection Preview');
      end;
      DrawTreeSelectionPreview;
      StartDragCursorPos := Mouse.CursorPos;
      StartDragLocalPos := TPoint.Create(X, Y);
      DragOverHappened := True;
    end else begin
      // Show preview.
      Point := (Mouse.CursorPos - StartDragLocalPos);
      PreviewForm.ShowCanvasAt(Point.X, Point.Y);
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
  if (key = #27) or (key = #10) or (key = #13) then begin
    TreeViewSearchTreeDblClick(Sender);
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
begin
  if TreeViewNoteTree.Focused then begin
    NoteBackend.TryRemove(SelectedIds);
    TreeViewNoteTree.Select([]);
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
    TreeViewNoteTree.Cursor := crHourGlass;

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
  //if FooNoteSettingsForm = nil then begin
  //  Application.CreateForm(TFooNoteSettingsForm, FooNoteSettingsForm);
  //end;
  //FooNoteSettingsForm.Show;
  //FooNoteSettingsForm.SetFocus;
end;

initialization

  BinaryClipboardFormat := ClipBrd.RegisterClipboardFormat('FooNote Binary');

end.
