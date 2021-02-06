unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  PlatformWindows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, ActnList, PairSplitter, StdActns, ClipBrd, LCLType,
  LazUtf8, FGL, LazLogger, Math, NoteBackend, NoteTypes, MemoUtil,
  LCLTranslator, Buttons, JSONPropStorage, TreeNodeData,
  TreeViewSync, Settings, SettingsForm, PreviewForm, AboutForm;

type

  { TFormFooNoteMain }

  TFormFooNoteMain = class(TForm)
    MenuItem10: TMenuItem;
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
    JSONPropAppConfig: TJSONPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem8: TMenuItem;
    MemoNote: TMemo;
    PanelDockSplitterLeft: TPanel;
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

    procedure InitRootBackend;
    procedure InitAppConfigLink;
    procedure InitPlatformSpecific;
    procedure InitOnConfigChange;

    procedure Reposition;
    procedure LoadAppState; // Locale might affect InitPlatformSpecific
    procedure LoadAppConfig; // Load config that might trigger AppConfig callbacks
    procedure LoadSplitterPosition;
    procedure SaveConfigFile;

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
  public

  end;

var
  FormFooNoteMain: TFormFooNoteMain;
  PaintingPreviewForSelectedTreeItems: boolean = False;

resourcestring
  RSFailSaveStillExit = 'Failed to save changes. Still exit?';
  RSExitNoSave = 'Exit without saving';
  RSNoExit = 'Do not exit';

implementation

var
  BinaryClipboardFormat: TClipboardFormat;

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
    DebugLn('Root Note Not changed');
  end;
end;

procedure TFormFooNoteMain.InitRootBackend;
const
  DefaultUrl: string = 'Default.FooNote';
var
  RootId: FullId;
  Url: string;
begin
  if ParamCount() >= 1 then begin
    Url := ParamStr(1);
    Caption := Format('%s - FooNote', [ExtractFileName(Url)]);
  end else begin
    Url := DefaultUrl;
  end;
  AppState.RootTreeUrl := Url;
  MenuItemRootPath.Caption := ExtractFileName(Url);
  MenuItemRootPath.Hint := Url;
  RootId := NoteBackend.Open(Url);
  RootNodeData := TTreeNodeData.Create(RootId);
  SelectedId := RootId;
  RefreshFullTree;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  I: longint;
  B: boolean;
  This: TFormFooNoteMain;
begin
  This := FormFooNoteMain;
  if Name = 'StayOnTop' then begin
    if Config.StayOnTop and (not AppState.ForceNotTop) then begin
      if This.FormStyle <> fsSystemStayOnTop then begin
        This.FormStyle := fsSystemStayOnTop;
      end;
    end else begin
      if This.FormStyle <> fsNormal then begin
        This.FormStyle := fsNormal;
      end;
    end;
    This.ActionViewStayOnTop.Checked := Config.StayOnTop;
  end else if Name = 'DockSide' then begin
    // When docked, the title bar is hidden. Show extra controls.
    This.ToolBarDocked.Visible := (Config.DockSide <> dsNone);
    This.ActionViewUndock.Enabled := (Config.DockSide <> dsNone);
    This.ActionViewDockLeft.Enabled := (Config.DockSide <> dsLeft);
    This.ActionViewDockRight.Enabled := (Config.DockSide <> dsRight);
    This.PanelDockSplitterLeft.Visible := (Config.DockSide = dsRight);
    This.PanelDockSplitterRight.Visible := (Config.DockSide = dsLeft);
    if Config.DockSide = dsNone then begin
      I := AppState.NonDockNoteSplitTop;
    end else begin
      I := AppState.DockNoteSplitTop;
    end;
    if (I > 0) and (I < This.Height) then begin
      This.SplitterTreeNote.Top := I;
    end;
  end else if Name = 'FeatureLevel' then begin
    B := (Config.FeatureLevel >= flAdvanced);
    This.MenuSepMount.Visible := B;
    This.MenuItemMount.Visible := B;
  end else if Name = 'ZenMode' then begin
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

procedure TFormFooNoteMain.LoadAppState;
var
  S: string;
begin
  S := JSONPropAppConfig.ReadString('AppState', '');
  if not S.IsEmpty then begin
    AppState.LoadFromJSON(S);
  end;

  if AppState.MaxWidth >= 0 then begin
    Constraints.MaxWidth := AppState.MaxWidth;
  end;
  SetDefaultLang(AppState.Locale, 'locale');
end;

procedure TFormFooNoteMain.LoadSplitterPosition;
var
  I: integer;
begin
  if AppConfig.DockSide = dsNone then begin
    I := AppState.NonDockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      SplitterTreeNote.Top := I;
    end; // After Height update
  end else begin
    I := AppState.DockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      SplitterTreeNote.Top := I;
    end; // After Height update
  end;
  // DebugLn(' TreeNoteSplitter.Top=%d', [SplitterTreeNote.Top]);
end;

procedure TFormFooNoteMain.LoadAppConfig;
var
  S: string;
  I: integer;
begin
  DebugLn('Before loading AppConfig %d x %d', [Width, Height]);
  S := JSONPropAppConfig.ReadString('AppConfig', '');
  if not S.IsEmpty then begin
    AppConfig.LoadFromJSON(S);
  end; // Might update Height. But docking is updated asyncly.
  DebugLn('After  loading AppConfig %d x %d', [Width, Height]);

  if AppConfig.DockSide = dsNone then begin
    I := AppState.NonDockWidth;
    if (I > 0) then begin
      Width := I;
    end;
    I := AppState.NonDockHeight;
    if (I > 0) then begin
      Height := I;
    end;
    Reposition;
    I := AppState.Left;
    if (I > 0) then begin
      Left := I;
    end;
    I := AppState.Top;
    if (I > 0) then begin
      Top := I;
    end;
  end;
  LoadSplitterPosition;
end;

procedure TFormFooNoteMain.SaveConfigFile;
begin
  if AppState.ResetOnNextStartup then begin
    DeleteFile(JSONPropAppConfig.JSONFileName);
  end else begin
    if JSONPropAppConfig.ReadBoolean('RememberPosition', True) and (AppConfig.DockSide = dsNone) then begin
      AppState.Left := Left;
      AppState.Top := Top;
    end;
    DebugLn(' TreeNoteSplitter.Top=%d %d,%d', [SplitterTreeNote.Top, AppState.NonDockNoteSplitTop,
      AppState.DockNoteSplitTop]);

    JSONPropAppConfig.WriteString('AppConfig', AppConfig.ToJSON());
    JSONPropAppConfig.WriteString('AppState', AppState.ToJSON());
    JSONPropAppConfig.Save;
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
var
  Data: TTreeNodeData;
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
      // DebugLn('Visit %s', [NodeData(Node).Id.ToString()]);
      if NodeData(Node).Id = Ancestor then begin
        Node.Expand(False);
        DebugLn(' Expand ancestor %s', [Ancestor.ToString()]);
        Node := Node.GetFirstChild;
        break; // Next ancestor
      end else begin
        Node := Node.GetNextSibling;
      end;
    end;
    if not Assigned(Node) then begin
      DebugLn('Cannot find ancestor %s to select %s!', [Ancestor.ToString(), Id.ToString()]);
      Exit;
    end;
  end;

  while Assigned(Node) do begin
    if NodeData(Node).Id = Id then begin
      DebugLn('Select %s', [Id.ToString()]);
      TreeViewNoteTree.Select(Node);
      SelectedId := Id;
      break;
    end else begin
      Node := Node.GetNextSibling;
    end;
  end;
  if not Assigned(Node) then begin
    DebugLn('Cannot find %s to select!', [Id.ToString()]);
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
  InitAppConfigLink;
  LoadAppState; // Might affect InitPlatformSpecific
  InitPlatformSpecific; // Might register OnConfigChange
  InitOnConfigChange;   // After InitPlatformSpecific
  LoadAppConfig;   // Affect registered OnConfigChange (InitOnConfigChange, InitPlatformSpecific)
  InitRootBackend;
end;

procedure TFormFooNoteMain.FormDestroy(Sender: TObject);
begin
  TreeViewNoteTree.Items.Clear;
  FreeAndNil(RootNodeData);
  NoteBackend.CloseAll;
end;

procedure TFormFooNoteMain.FormResize(Sender: TObject);
begin
  if (not AppState.MovingPreview) and (WindowState = wsNormal) then begin
    if AppConfig.DockSide = dsNone then begin
      AppState.NonDockWidth := Width;
      AppState.NonDockHeight := Height;
      DebugLn('  NonDockHeight := %d', [Height]);
      AppState.DockWidth := 0;
    end else begin
      AppState.DockWidth := Width;
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

  DebugLn('NewNode Id=%d Index=%d Meta=%s', [Id.Id, Index, AMeta.Trim]);
  RefreshFullTree;
  Result := Id;
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
  SelectExpandNode(NewNode('-', 'type=separator' + #10 + 'readonly=true' + #10));
end;

procedure TFormFooNoteMain.MemoNoteChange(Sender: TObject);
begin
  if MemoNote.ReadOnly then begin
    exit;
  end;
  if SelectedId.Id = 0 then begin
    SelectExpandNode(NewNode(MemoNote.Text, ''));
    Exit;
  end;
  NoteBackend.TrySetText(SelectedId, MemoNote.Text);
  // Only refresh when changing the first line.
  if MemoNote.CaretPos.Y <= 1 then begin
    RefreshFullTree;
  end;
end;

procedure TFormFooNoteMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfigFile;
  if not NoteBackend.TryPersist() then begin
    if QuestionDlg('FooNote', RSFailSaveStillExit, mtCustom, [mrYes, RSExitNoSave, mrNo, RSNoExit], '') <>
      mrYes then begin
      CloseAction := caNone;
    end;
  end;
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
    DebugLn('Dock Splitter MouseDown');
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
    // DebugLn('Dock Splitter MouseMove; NewWidth = %d', [NewWidth]);
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
    DebugLn('Dock Splitter MouseUp; DockWidth = %d', [NewWidth]);
    AppState.DockWidth := NewWidth;
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
    DebugLn('Copy binary len %d', [Length(B)]);
    ClipBoard.AddFormat(BinaryClipboardFormat, S);
  finally
    ClipBoard.Close;
    FreeAndNil(S);
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
  try
    S := TBytesStream.Create;
    if ClipBoard.HasFormat(BinaryClipboardFormat) and ClipBoard.GetFormat(BinaryClipboardFormat, S) then begin
      L := S.Position;
      DebugLn('Paste binary len %d', [L]);
      B := S.bytes;
      SetLength(B, L);
      DestId := InsertLocation(SelectedId, 0, Pos);
      Ids := NoteBackend.PasteFromBytes(DestId, Pos, B);
      RefreshFullTree;
      // For some reason, accessing Selected can SIGSEGV here? RootId?
      // NoteTree.Selected.Expanded := True;
      SelectedIds := Ids;
    end else begin
      T := ClipBoard.AsText;
      if not T.IsEmpty then begin
        SelectNode(NewNode(T, ''));
        NoteMemo.SetFocus;
      end;
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TFormFooNoteMain.FormShow(Sender: TObject);
begin
  // Setting SplitterTreeNote.Top in FormCreate might be updated (due to autosize?).
  // Set it again here.
  LoadSplitterPosition;

  // Make the preview window "visible" so it won't steal the focus.
  {$ifdef Windows}
  PreviewForm.EnsureInit;
  {$endif}
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
  url := InputBox('Mount', 'URL', 'file:1.foonote');
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
var
  Node: TTreeNode;
begin
  if key = #27 then begin
    EditNoteSearch.Clear;
  end else if (key = #10) or (key = #13) then begin
    if TreeViewSearchTree.Visible then begin
      if TreeViewSearchTree.CanSetFocus then begin
        if not Assigned(TreeViewSearchTree.Selected) then begin
          Node := TreeViewSearchTree.Items.GetFirstNode;
          if Assigned(Node) then begin
            TreeViewSearchTree.Select(Node);
          end;
        end;
        TreeViewSearchTree.SetFocus;
      end;
    end else begin
      if TreeViewNoteTree.CanSetFocus then begin
        TreeViewNoteTree.SetFocus;
      end;
    end;
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
  if (Stage = cdPrePaint) and (Node.Text.StartsWith('-')) and
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
  DebugLn('DragDrop');
  if CandidateDropNode <> nil then begin
    DestId := NodeData(CandidateDropNode).Id;
    // Move selections to the drop location.
    Ids := SelectedIds;
    if NoteBackend.TrySetParent(Ids, DestId, CandidateInsertPosition) then begin
      RefreshFullTree;
      SelectedIds := Ids;
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
begin
  TreeViewNoteTree.SelectionColor := clHighlight;
  ActionEditDelete.Enabled := True;
  ActionEditCopy.Enabled := True;
  ActionEditPaste.Enabled := True;
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
      // Take a "screenshot" of the tree view.
      // Do not do it in OnStartDrag to reduce cost.
      DebugLn('Render Selection Preview');
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
    DebugLn('Search Completed');
  end;
  N := TreeViewSearchTree.Items.GetLastNode;
  for S in NoteBackend.GetSearchResult(TreeViewSearchTree.Items.Count) do begin
    DebugLn('Search Result: %s (%s)', [S.Text, S.Id.ToString()]);
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
    AppState.NonDockNoteSplitTop := SplitterTreeNote.Top;
    AppState.DockNoteSplitTop := 0;
  end else begin
    AppState.DockNoteSplitTop := SplitterTreeNote.Top;
    // AppState.NonDockNoteSplitTop := 0;
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
  end;
end;

procedure TFormFooNoteMain.ActionEditSaveExecute(Sender: TObject);
begin
  // TODO: Print failure.
  NoteBackend.TryPersist();
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
