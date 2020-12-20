unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  PlatformWindows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, ActnList, PairSplitter, StdActns, ClipBrd, LCLType, LazUtf8, FGL,
  LazLogger, Math, NoteBackend, MemoUtil, LCLTranslator, Buttons,
  JSONPropStorage, TreeNodeData, TreeViewSync, Settings, SettingsForm,
  PreviewForm, AboutForm;

type

  { TFooNoteForm }

  TFooNoteForm = class(TForm)
    EditPasteAction: TAction;
    EditDeleteAction: TAction;
    EditCopyAction: TAction;
    AppAboutAction: TAction;
    DockSplitterRight: TPanel;
    IdleTimerWndProc: TIdleTimer;
    JSONProp: TJSONPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem8: TMenuItem;
    NoteMemo: TMemo;
    DockSplitterLeft: TPanel;
    NoteTree: TTreeView;
    PanelTree: TPanel;
    PanelEdit: TPanel;
    SaveDialog: TSaveDialog;
    TimerSearchResult: TTimer;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TreeNoteSplitter: TSplitter;
    SearchTree: TTreeView;
    ViewDockRight: TAction;
    ViewDockLeft: TAction;
    ViewStayOnTop: TAction;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    ViewUndock: TAction;
    AppQuitAction: TAction;
    ElementaryIcons: TImageList;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    AppSettingPreferences: TAction;
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
    MountUrl: TAction;
    NewSeparator: TAction;
    NewFolder: TAction;
    NewNote: TAction;
    EditSave: TAction;
    ActionList: TActionList;
    EditMenu: TPopupMenu;
    NoteSearch: TEdit;
    NoteToolBar: TToolBar;
    MenuItemNewSeparator: TMenuItem;
    MenuItemNewFolder: TMenuItem;
    MenuItemNewNote: TMenuItem;
    DockMenu: TPopupMenu;
    ToolButtonDockedMenu: TToolButton;
    ToolButtonNew: TToolButton;
    DockedToolBar: TToolBar;
    TopPanel: TPanel;
    AddMenu: TPopupMenu;

    procedure AppAboutActionExecute(Sender: TObject);
    procedure DockSplitterLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure DockSplitterLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure DockSplitterLeftMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure EditCopyActionExecute(Sender: TObject);
    procedure EditPasteActionExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimerWndProcTimer(Sender: TObject);
    procedure MountUrlExecute(Sender: TObject);
    procedure NoteSearchChange(Sender: TObject);
    procedure NoteSearchKeyPress(Sender: TObject; var Key: char);
    procedure NoteTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
    procedure NoteTreeDragDrop(Sender, Source: TObject; X, Y: integer);
    procedure NoteTreeDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState; var Accept: boolean);
    procedure NoteTreeEndDrag(Sender, Target: TObject; X, Y: integer);
    procedure NoteTreeEnter(Sender: TObject);
    procedure NoteTreeExit(Sender: TObject);
    procedure NoteTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure NoteTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure NoteTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure SearchTreeDblClick(Sender: TObject);
    procedure SearchTreeKeyPress(Sender: TObject; var Key: char);
    procedure SearchTreeSelectionChanged(Sender: TObject);
    procedure TimerSearchResultTimer(Sender: TObject);
    procedure TopPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TopPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure TopPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TreeNoteSplitterMoved(Sender: TObject);
    procedure ViewDockLeftExecute(Sender: TObject);
    procedure ViewDockRightExecute(Sender: TObject);
    procedure ViewStayOnTopExecute(Sender: TObject);
    procedure ViewUndockExecute(Sender: TObject);
    procedure AppQuitActionExecute(Sender: TObject);
    procedure EditDeleteExecute(Sender: TObject);
    procedure EditSaveExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure NewNoteExecute(Sender: TObject);
    procedure NewSeparatorExecute(Sender: TObject);
    procedure NoteMemoChange(Sender: TObject);
    procedure NoteMemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure NoteTreeDeletion(Sender: TObject; Node: TTreeNode);
    procedure NoteTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
    procedure NoteTreeSelectionChanged(Sender: TObject);
    procedure AppSettingPreferencesExecute(Sender: TObject);
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
    procedure SelectNode(Id: FullId);
    procedure SetSelectedId(Id: FullId);
    function GetSelectedIds(): VecFullId;
    procedure SetSelectedIds(Ids: VecFullId);
    procedure DrawTreeSelectionPreview;
    property SelectedId: FullId read FSelectedId write SetSelectedId;
    property SelectedIds: VecFullId read GetSelectedIds write SetSelectedIds;
  public

  end;

var
  FooNoteForm: TFooNoteForm;
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

{ TFooNoteForm }

procedure TFooNoteForm.RefreshFullTree;
begin
  if RootNodeData.SyncFromBackend() then begin
    // Change detected. Refresh the tree.
    NoteTree.BeginUpdate;
    TreeViewSync.SyncChildTreeNode(NoteTree, nil, NoteTree.Items.GetFirstNode,
      RootNodeData, True);
    NoteTree.EndUpdate;
  end else begin
    DebugLn('Root Note Not changed');
  end;
end;

procedure TFooNoteForm.InitRootBackend;
var
  RootId: FullId;
begin
  RootId := NoteBackend.Open('foonote:root.foonote');
  RootNodeData := TTreeNodeData.Create(RootId);
  SelectedId := RootId;
  RefreshFullTree;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  I: longint;
  B: boolean;
  This: TFooNoteForm;
begin
  This := FooNoteForm;
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
    This.ViewStayOnTop.Checked := Config.StayOnTop;
  end else if Name = 'DockSide' then begin
    // When docked, the title bar is hidden. Show extra controls.
    This.DockedToolBar.Visible := (Config.DockSide <> dsNone);
    This.ViewUndock.Enabled := (Config.DockSide <> dsNone);
    This.ViewDockLeft.Enabled := (Config.DockSide <> dsLeft);
    This.ViewDockRight.Enabled := (Config.DockSide <> dsRight);
    This.DockSplitterLeft.Visible := (Config.DockSide = dsRight);
    This.DockSplitterRight.Visible := (Config.DockSide = dsLeft);
    if Config.DockSide = dsNone then begin
      I := AppState.NonDockNoteSplitTop;
    end else begin
      I := AppState.DockNoteSplitTop;
    end;
    if (I > 0) and (I < This.Height) then begin
      This.TreeNoteSplitter.Top := I;
    end;
  end else if Name = 'FeatureLevel' then begin
    B := (Config.FeatureLevel >= flAdvanced);
    This.MenuSepMount.Visible := B;
    This.MenuItemMount.Visible := B;
  end;
end;

procedure TFooNoteForm.InitOnConfigChange;
begin
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TFooNoteForm.Reposition;
begin
  Left := Screen.WorkAreaWidth - Width * 3 div 2;
  Top := Width div 2;
end;

procedure TFooNoteForm.LoadAppState;
var
  S: string;
begin
  S := JsonProp.ReadString('AppState', '');
  if not S.IsEmpty then begin
    AppState.LoadFromJSON(S);
  end;

  if AppState.MaxWidth >= 0 then begin
    Constraints.MaxWidth := AppState.MaxWidth;
  end;
  SetDefaultLang(AppState.Locale, 'locale');
end;

procedure TFooNoteForm.LoadSplitterPosition;
var
  I: integer;
begin
  if AppConfig.DockSide = dsNone then begin
    I := AppState.NonDockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      TreeNoteSplitter.Top := I;
    end; // After Height update
  end else begin
    I := AppState.DockNoteSplitTop;
    if (I > 0) and (I < Height) then begin
      TreeNoteSplitter.Top := I;
    end; // After Height update
  end;
  // DebugLn(' TreeNoteSplitter.Top=%d', [TreeNoteSplitter.Top]);
end;

procedure TFooNoteForm.LoadAppConfig;
var
  S: string;
  I: integer;
begin
  DebugLn('Before loading AppConfig %d x %d', [Width, Height]);
  S := JsonProp.ReadString('AppConfig', '');
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

procedure TFooNoteForm.SaveConfigFile;
begin
  if AppState.ResetOnNextStartup then begin
    DeleteFile(JsonProp.JSONFileName);
  end else begin
    if JsonProp.ReadBoolean('RememberPosition', True) and (AppConfig.DockSide = dsNone) then begin
      AppState.Left := Left;
      AppState.Top := Top;
    end;
    DebugLn(' TreeNoteSplitter.Top=%d %d,%d', [TreeNoteSplitter.Top, AppState.NonDockNoteSplitTop,
      AppState.DockNoteSplitTop]);

    JsonProp.WriteString('AppConfig', AppConfig.ToJSON());
    JsonProp.WriteString('AppState', AppState.ToJSON());
    JsonProp.Save;
  end;
end;

procedure TFooNoteForm.InitPlatformSpecific;
begin
{$ifdef Windows}
  PlatformWindows.SetupMainForm(Self);
{$endif}
end;

procedure TFooNoteForm.InitAppConfigLink;
begin
  AppConfig.EditorFont := NoteMemo.Font;
end;

function TFooNoteForm.InsertLocation(Id: FullId; NParent: integer; out Pos: integer): FullId;
var
  Data: TTreeNodeData;
begin
  Pos := 1; // Insert after.
  Result := Id;

  if (NParent = 0) and (IsFolder(Id)) then begin
    Pos := 0; // Append inside.
  end;
end;

procedure TFooNoteForm.SelectNode(Id: FullId);
var
  Node: TTreeNode;
begin
  if SelectedId = Id then begin
    exit;
  end;
  // PERF: This might be improved.
  for Node in NoteTree.Items do begin
    if TTreeNodeData(Node.Data).Id = Id then begin
      NoteTree.Select(Node);
      SelectedId := Id;
      break;
    end;
  end;
end;

procedure TFooNoteForm.SetSelectedId(Id: FullId);
var
  AText, ReadOnly: string;
begin
  FSelectedId := Id;
  NoteMemo.ReadOnly := True; // Disable NoteMemo.OnChange
  AText := NoteBackend.GetText(Id);
  NoteMemo.Text := AText;
  ReadOnly := NoteBackend.ExtractMeta(Id, 'readonly=');
  if not (ReadOnly = 'true') then begin
    NoteMemo.ReadOnly := False;
    if NoteMemo.Lines.Count < 2 then begin
      NoteMemo.SelStart := LazUtf8.UTF8Length(AText);
    end;
  end;
end;

procedure TFooNoteForm.SetSelectedIds(Ids: VecFullId);
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
  NoteTree.ClearSelection();
  for Item in NoteTree.Items do begin
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

procedure TFooNoteForm.FormCreate(Sender: TObject);
begin
  InitAppConfigLink;
  LoadAppState; // Might affect InitPlatformSpecific
  InitPlatformSpecific; // Might register OnConfigChange
  InitOnConfigChange;   // After InitPlatformSpecific
  LoadAppConfig;   // Affect registered OnConfigChange (InitOnConfigChange, InitPlatformSpecific)
  InitRootBackend;
end;

procedure TFooNoteForm.FormDestroy(Sender: TObject);
begin
  NoteTree.Items.Clear;
  FreeAndNil(RootNodeData);
  NoteBackend.CloseAll;
end;

procedure TFooNoteForm.FormResize(Sender: TObject);
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

procedure TFooNoteForm.FormWindowStateChange(Sender: TObject);
begin
{$ifdef Windows}
  PlatformWindows.RepositionDock;
{$endif}
end;

function TFooNoteForm.NewNode(AText, AMeta: string; NParent: integer = 0): FullId;
var
  P, Id: FullId;
  Index: integer;
begin
  P := InsertLocation(SelectedId, NParent, Index);
  Id := NoteBackend.Insert(P, Index, AText, AMeta);

  DebugLn('NewNode Id=%d Index=%d Meta=%s', [Id.Id, Index, AMeta.Trim]);
  RefreshFullTree;
  Result := Id;
end;

procedure TFooNoteForm.NewFolderExecute(Sender: TObject);
begin
  SelectNode(NewNode('', 'type=folder' + #10, 1));
  NoteMemo.SetFocus;
end;

procedure TFooNoteForm.NewNoteExecute(Sender: TObject);
begin
  SelectNode(NewNode('', ''));
  NoteMemo.SetFocus;
end;

procedure TFooNoteForm.NewSeparatorExecute(Sender: TObject);
begin
  SelectNode(NewNode('-', 'type=separator' + #10 + 'readonly=true' + #10));
end;

procedure TFooNoteForm.NoteMemoChange(Sender: TObject);
begin
  if NoteMemo.ReadOnly then begin
    exit;
  end;
  NoteBackend.TrySetText(SelectedId, NoteMemo.Text);
  // Only refresh the selected node.
  TreeViewSync.SyncTreeNode(NoteTree.Selected);
end;

procedure TFooNoteForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfigFile;
  if not NoteBackend.TryPersist() then begin
    if QuestionDlg('FooNote', RSFailSaveStillExit, mtCustom, [mrYes, RSExitNoSave, mrNo, RSNoExit], '') <>
      mrYes then begin
      CloseAction := caNone;
    end;
  end;
end;

procedure TFooNoteForm.AppQuitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TFooNoteForm.ViewUndockExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsNone;
end;

procedure TFooNoteForm.ViewStayOnTopExecute(Sender: TObject);
begin
  AppConfig.StayOnTop := not AppConfig.StayOnTop;
end;

procedure TFooNoteForm.ViewDockLeftExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsLeft;
end;

procedure TFooNoteForm.TopPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) and (not TopPanelMouseLeftIsDown) then begin
    TopPanelMouseLeftDownStartPos.X := X;
    TopPanelMouseLeftDownStartPos.Y := Y;
    TopPanelMouseLeftIsDown := True;
  end;
end;

procedure TFooNoteForm.DockSplitterLeftMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TFooNoteForm.AppAboutActionExecute(Sender: TObject);
begin
  if AboutFooNoteForm = nil then begin
    Application.CreateForm(TAboutFooNoteForm, AboutFooNoteForm);
  end;
  AboutFooNoteForm.ShowModal;
end;

procedure TFooNoteForm.DrawTreeSelectionPreview;
var
  PreviewCanvas: TCanvas;
  OrigSelectionColor, OrigBackColor: TColor;
begin
  // ClientRect.Width does not include the scrollbar.
  PreviewCanvas := PreviewForm.PrepareCleanCanvas(NoteTree.ClientRect.Width, NoteTree.Height);

  NoteTree.BeginUpdate;
  PaintingPreviewForSelectedTreeItems := True;
  OrigSelectionColor := NoteTree.SelectionColor;
  OrigBackColor := Notetree.BackgroundColor;

  // Temporarily change background color to avoid drawing white blank area.
  NoteTree.BackgroundColor := clFuchsia;
  // Temporarily ensure selection color is with focus.
  NoteTree.SelectionColor := clHighlight;
  NoteTree.EndUpdate;
  // Draw to preview canvas.
  NoteTree.PaintTo(PreviewCanvas, 0, 0);

  // Restore colors.
  NoteTree.BeginUpdate;
  NoteTree.SelectionColor := OrigSelectionColor;
  NoteTree.BackgroundColor := OrigBackColor;
  PaintingPreviewForSelectedTreeItems := False;
  NoteTree.EndUpdate;
end;

function TFooNoteForm.DockSplitterNewWidth: longint;
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

procedure TFooNoteForm.DockSplitterLeftMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  NewWidth, PreviewLeft, SplitterWidth: longint;
begin
  if DockSplitterLeftIsDown then begin
    Assert(AppConfig.DockSide <> dsNone);
    // Do not use X, Y. They are relative to the form, which is changing.
    NewWidth := DockSplitterNewWidth;
    SplitterWidth := DockSplitterLeft.Width;
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

procedure TFooNoteForm.DockSplitterLeftMouseUp(Sender: TObject; Button: TMouseButton;
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

procedure TFooNoteForm.EditCopyActionExecute(Sender: TObject);
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

procedure TFooNoteForm.EditPasteActionExecute(Sender: TObject);
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

procedure TFooNoteForm.FormShow(Sender: TObject);
begin
  // Setting TreeNoteSplitter.Top in FormCreate might be updated (due to autosize?).
  // Set it again here.
  LoadSplitterPosition;

  // Make the preview window "visible" so it won't steal the focus.
  {$ifdef Windows}
  PreviewForm.EnsureInit;
  {$endif}
end;

procedure TFooNoteForm.IdleTimerWndProcTimer(Sender: TObject);
begin
  {$ifdef Windows}
  // HACK: For some reason, LCL rewrites WndProc periodically.
  PlatformWindows.EnsureWrappedWndProc;
  {$endif}
end;

procedure TFooNoteForm.MountUrlExecute(Sender: TObject);
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

procedure TFooNoteForm.NoteSearchChange(Sender: TObject);
var
  T: string;
begin
  T := NoteSearch.Text;
  if T.IsEmpty then begin
    NoteBackend.StopSearch();
    TimerSearchResult.Enabled := False;
    SearchTree.Visible := False;
    NoteTree.Visible := True;
    SearchTree.Items.Clear;
    LastSearchText := '';
  end else begin
    NoteBackend.StartSearch([RootNodeData.Id], T);
    TimerSearchResult.Enabled := True;
    NoteTree.Visible := False;
    SearchTree.Visible := True;
  end;
end;

procedure TFooNoteForm.NoteSearchKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then begin
    NoteSearch.Clear;
  end;
end;

procedure TFooNoteForm.NoteTreeAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages, DefaultDraw: boolean);
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
  if NoteTree.Dragging and (Stage = cdPostPaint) and (CandidateDropNode = Node) and
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

procedure TFooNoteForm.NoteTreeDragDrop(Sender, Source: TObject; X, Y: integer);
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

function TFooNoteForm.GetSelectedIds(): VecFullId;
var
  I, C: integer;
begin
  Result := VecFullId.Create;
  C := NoteTree.Items.SelectionCount;
  if C > 0 then begin
    SetLength(Result, C);
    for I := 0 to NoteTree.Items.SelectionCount - 1 do begin
      Result[I] := TTreeNodeData(NoteTree.Selections[I].Data).Id;
    end;
  end;
end;

procedure TFooNoteForm.NoteTreeDragOver(Sender, Source: TObject; X, Y: integer; State: TDragState;
  var Accept: boolean);
var
  N: TTreeNode;
  P: longint = 0;
  R: TRect;
  D: TTreeNodeData;
begin
  if Source = NoteTree then begin
    Accept := True;
    N := NoteTree.GetNodeAt(X, Y);
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
      NoteTree.Repaint;
    end;
  end;
end;

procedure TFooNoteForm.NoteTreeEndDrag(Sender, Target: TObject; X, Y: integer);
begin
  // This can be called if drag was aborted by the ESC key.
  if CandidateDropNode <> nil then begin
    CandidateDropNode := nil;
    NoteTree.Repaint;
  end;
  PreviewForm.Hide;
end;

procedure TFooNoteForm.NoteTreeEnter(Sender: TObject);
begin
  NoteTree.SelectionColor := clHighlight;
end;

procedure TFooNoteForm.NoteTreeExit(Sender: TObject);
begin
  NoteTree.SelectionColor := clGray;
end;

procedure TFooNoteForm.NoteTreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  Point: TPoint;
begin
  if NoteTree.Dragging then begin
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

procedure TFooNoteForm.NoteTreeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not NoteTree.Dragging then begin
    PreviewForm.Hide;
  end;
end;

procedure TFooNoteForm.NoteTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  DragOverHappened := False;
end;

procedure TFooNoteForm.SearchTreeDblClick(Sender: TObject);
begin
  NoteSearch.Clear;
end;

procedure TFooNoteForm.SearchTreeKeyPress(Sender: TObject; var Key: char);
begin
  if key = #27 then begin
    NoteSearch.Clear;
  end;
end;

procedure TFooNoteForm.SearchTreeSelectionChanged(Sender: TObject);
var
  Id: FullId;
begin
  if Assigned(SearchTree.Selected) then begin
    Id := NodeData(SearchTree.Selected).Id;
    SelectedIds := [Id];
    SelectedId := Id;
  end;
end;

procedure TFooNoteForm.TimerSearchResultTimer(Sender: TObject);
var
  S: FullIdText;
  N: TTreeNode;
  D: TTreeNodeData;
  SearchText: string;
begin
  SearchText := NoteBackend.GetSearchInput();
  if LastSearchText <> SearchText then begin
    SearchTree.Items.Clear;
    LastSearchText := SearchText;
  end;
  TimerSearchResult.Enabled := not NoteBackend.IsSearchComplete();
  if not TimerSearchResult.Enabled then begin
    DebugLn('Search Completed');
  end;
  N := SearchTree.Items.GetLastNode;
  for S in NoteBackend.GetSearchResult(SearchTree.Items.Count) do begin
    DebugLn('Search Result: %s (%s)', [S.Text, S.Id.ToString()]);
    N := SearchTree.Items.Add(N, '');
    D := TTreeNodeData.Create(S.Id);
    D.SyncFromBackend();
    D.SearchText := S.Text;
    TreeViewSync.ApplyData(N, D);
    N.Data := D;
  end;
end;

procedure TFooNoteForm.TopPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
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

procedure TFooNoteForm.TopPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then begin
    TopPanelMouseLeftIsDown := False;
  end;
end;

procedure TFooNoteForm.TreeNoteSplitterMoved(Sender: TObject);
begin
  if AppConfig.DockSide = dsNone then begin
    AppState.NonDockNoteSplitTop := TreeNoteSplitter.Top;
    AppState.DockNoteSplitTop := 0;
  end else begin
    AppState.DockNoteSplitTop := TreeNoteSplitter.Top;
    // AppState.NonDockNoteSplitTop := 0;
  end;
end;

procedure TFooNoteForm.ViewDockRightExecute(Sender: TObject);
begin
  AppConfig.DockSide := dsRight;
end;

procedure TFooNoteForm.EditDeleteExecute(Sender: TObject);
begin
  NoteBackend.TryRemove(SelectedIds);
  NoteTree.Select([]);
  RefreshFullTree;
end;

procedure TFooNoteForm.EditSaveExecute(Sender: TObject);
begin
  // TODO: Print failure.
  NoteBackend.TryPersist();
end;

procedure TFooNoteForm.NoteMemoKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then begin
    // ESC - lose focus.
    NoteTree.SetFocus;
    Key := 0;
    exit;
  end;
  MemoUtil.SmartKeyDown(NoteMemo, Key, Shift);
end;

procedure TFooNoteForm.NoteTreeDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data <> nil then begin
    TTreeNodeData(Node.Data).Free;
    Node.Data := nil;
  end;
end;

procedure TFooNoteForm.NoteTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: boolean);
begin
  TreeViewSync.SyncTreeNode(Node, True);
end;

procedure TFooNoteForm.NoteTreeSelectionChanged(Sender: TObject);
var
  Selected: TTreeNode;
begin
  Selected := NoteTree.Selected;
  if Selected = nil then begin
    exit;
  end;
  SelectedId := TTreeNodeData(Selected.Data).Id;
end;

procedure TFooNoteForm.AppSettingPreferencesExecute(Sender: TObject);
begin
  if FooNoteSettingsForm = nil then begin
    Application.CreateForm(TFooNoteSettingsForm, FooNoteSettingsForm);
  end;
  FooNoteSettingsForm.Show;
  FooNoteSettingsForm.SetFocus;
end;

initialization

  BinaryClipboardFormat := ClipBrd.RegisterClipboardFormat('FooNote Binary');

end.
