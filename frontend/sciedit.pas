unit SciEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LCLType, LCLIntf, LMessages, Graphics,
  Scintilla, Settings, LocaleUtils,
  {$ifdef Windows}
  Windows, win32int, win32proc
  {$endif};

type
  // Main control.
  TSciEdit = class(TWinControl)
  private
    // https://www.scintilla.org/ScintillaDoc.html#DirectAccess
    pSciWndData: PtrInt;
    pSciMsgFn: SciFnDirect;
    // https://www.scintilla.org/ScintillaDoc.html#Notifications
    procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
    procedure SciNotify(var Notif: SCNotification);
  private
    // Inside SetText? (prevent OnChange events)
    Changing: boolean;
    FOnChange: TNotifyEvent;
    procedure InitSettings;
    function GetText: string;
    procedure SetText(Value: string);
    function GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    function GetCaretPos: TPoint;
    function GetWordWrap: boolean;
    procedure SetWordWrap(Value: boolean);
    function GetSelStart: integer;
    procedure SetSelStart(Val: integer);
  protected
    // https://www.scintilla.org/Steps.html
    procedure CreateWnd; override;
  public
    function SciMsg(iMessage: UInt32; wParam: PtrUInt = 0; lParam: PtrInt = 0): PtrInt;
    function SciMsgCStr(iMessage: UInt32; wParam: PtrUInt; S: string): PtrInt;
  public
    class function IsAvailable: boolean;
    procedure SetSavepoint;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;

    property Text: string read GetText write SetText;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default True;
    property CaretPos: TPoint read GetCaretPos;
    property SelStart: integer read GetSelStart write SetSelStart;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

var
  SciLibraryHandle: HINST;

class function TSciEdit.IsAvailable: boolean;
begin
  Result := SciLibraryHandle <> 0;
end;

procedure TSciEdit.SetSavepoint;
begin
  SciMsg(SCI_EMPTYUNDOBUFFER);
  SciMsg(SCI_SETSAVEPOINT);
  SciMsg(SCI_SETSCROLLWIDTH, 50);
  SciMsg(SCI_SETSCROLLWIDTHTRACKING, 1);
end;

procedure TSciEdit.CopyToClipboard;
begin
  SciMsg(SCI_COPYALLOWLINE);
end;

procedure TSciEdit.PasteFromClipboard;
begin
  SciMsg(SCI_PASTE);
end;

procedure TSciEdit.SetText(Value: string);
var
  OrigReadOnly: boolean;
begin
  OrigReadOnly := GetReadOnly;
  SetReadOnly(False);
  Changing := True;
  SciMsgCStr(SCI_SETTEXT, 0, Value);
  Changing := False;
  SetReadOnly(OrigReadOnly);
end;

function TSciEdit.GetText: string;
var
  Len: PtrInt;
  P: PChar;
begin
  Len := SciMsg(SCI_GETLENGTH, 0, 0);
  P := StrAlloc(Len + 1);
  SciMsg(SCI_GETTEXT, Len + 1, PtrInt(P));
  Result := StrPas(P);
  StrDispose(P);
end;

function TSciEdit.GetReadOnly: boolean;
begin
  Result := (SciMsg(SCI_GETREADONLY) <> 0);
end;

procedure TSciEdit.SetReadOnly(Value: boolean);
begin
  SciMsg(SCI_SETREADONLY, Value.ToInteger);
end;

function TSciEdit.GetWordWrap: boolean;
begin
  Result := (SciMsg(SCI_GETWRAPMODE) <> SC_WRAP_NONE);
end;

procedure TSciEdit.SetWordWrap(Value: boolean);
begin
  if Value then begin
    SciMsg(SCI_SETWRAPMODE, SC_WRAP_CHAR);
    SciMsg(SCI_SETHSCROLLBAR, 0);
  end else begin
    SciMsg(SCI_SETWRAPMODE, SC_WRAP_NONE);
    SciMsg(SCI_SETHSCROLLBAR, 1);
    SciMsg(SCI_SETSCROLLWIDTH, 50);
  end;
end;

function TSciEdit.GetSelStart: integer;
begin
  Result := SciMsg(SCI_GETANCHOR);
end;

procedure TSciEdit.SetSelStart(Val: integer);
begin
  SciMsg(SCI_GOTOPOS, Val);
end;

function TSciEdit.GetCaretPos: TPoint;
var
  P: PtrInt;
begin
  P := SciMsg(SCI_GETCURRENTPOS);
  Result.Y := SciMsg(SCI_LINEFROMPOSITION, P);
  // XXX: This is incorrect. But we don't use X.
  Result.X := 0;
end;

procedure TSciEdit.CNNotify(var Message: TLMNotify);
begin
  SciNotify(PSCNotification(Message.NMHdr)^);
end;

procedure TSciEdit.SciNotify(var Notif: SCNotification);
var
  code: dword;
  c: char;
begin
  code := Notif.nmhdr.code;
  if (code = SCN_PAINTED) or (code = SCN_STYLENEEDED) then begin
    exit;
  end;
  if (code = SCN_MODIFIED) and Assigned(OnChange) and not Changing then begin
    OnChange(Self);
  end;
  if (code = SCN_CHARADDED) then begin
    if (Notif.ch = 10) or (Notif.ch = 13) then begin
      // TODO: Insert spaces?
    end;
  end;
end;

function TSciEdit.SciMsg(iMessage: UInt32; wParam: PtrUInt = 0; lParam: PtrInt = 0): PtrInt;
begin
  HandleNeeded;
  if Assigned(pSciMsgFn) then begin
    Result := pSciMsgFn(pSciWndData, iMessage, wParam, lParam);
  end;
end;

function TSciEdit.SciMsgCStr(iMessage: UInt32; wParam: PtrUInt; S: string): PtrInt;
var
  P: PChar;
  S2: string;
begin
  if S.EndsWith(#0) then begin
    P := PChar(S);
  end else begin
    S2 := S + #0;
    P := PChar(S2);
  end;
  Result := SciMsg(iMessage, wParam, PtrInt(P));
end;

{$ifdef Windows}
function WrappedSciEditWndProc(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  WindowInfo: PWin32WindowInfo;
  This: TSciEdit;
  SciWndProc: WNDPROC;
  ShouldHandle: boolean;
begin
  WindowInfo := GetWin32WindowInfo(Ahwnd);
  This := TSciEdit(WindowInfo^.WinControl);
  if not Assigned(This) then begin
    // No longer managed by LCL? Should not happen?
    Result := DefWindowProc(Ahwnd, uMsg, wParam, lParam);
    exit;
  end;

  // Copy WndProc, WM_NCDESTROY might de-alloc WindowInfo.
  SciWndProc := WindowInfo^.DefWndProc;

  // Should LCL handle the WndProc? Currently we want LCL handle ESC KeyPress and Focus changes.
  ShouldHandle := False;

  // Forward some events as LCL CM message.
  if ((uMsg = WM_KEYDOWN) or (uMsg = WM_KEYUP)) and (wParam = VK_ESCAPE) then begin
    ShouldHandle := True;
  end;
  if (uMsg = WM_SETFOCUS) or (uMsg = WM_KILLFOCUS) or (uMsg = WM_SHOWWINDOW) or
    (uMsg = WM_ENABLE) or (uMsg = WM_GETFONT) or (uMsg = WM_SETFONT) or (uMsg = WM_ACTIVATE) then begin
    ShouldHandle := True;
  end;

  // Release WindowInfo on destroy. This is required according to MSDN.
  if uMsg = WM_NCDESTROY then begin
    ShouldHandle := True;
  end;

  if ShouldHandle then begin
    WindowProc(Ahwnd, uMsg, wParam, lParam);
  end;

  // Call Scintilla WndProc.
  Result := CallWindowProc(SciWndProc, Ahwnd, uMsg, wParam, lParam);
end;

{$endif}

procedure TSciEdit.InitSettings;
var
  FontSize: integer;
  FontName: string;
begin
  // Utf-8!
  SciMsg(SCI_SETCODEPAGE, SC_CP_UTF8);

  // Line-wrap, Scrolling
  SetWordWrap(True);

  // Popup
  SciMsg(SCI_USEPOPUP, SC_POPUP_TEXT);

  // IME
  SciMsg(SCI_SETIMEINTERACTION, SC_IME_INLINE);

  // Tab Indent
  SciMsg(SCI_SETTABWIDTH, 2);
  SciMsg(SCI_SETMARGINS, 0);

  // Multi-selection
  SciMsg(SCI_SETMULTIPLESELECTION, 1);
  SciMsg(SCI_SETADDITIONALSELECTIONTYPING, 1);

  // Font
  FontSize := Round(Abs(Graphics.GetFontData(Font.Reference.Handle).Height) * 72 / Font.PixelsPerInch);
  FontName := Font.Name;
  if FontName = 'default' then begin
    FontName := 'Microsoft YaHei';
  end;
  SciMsgCStr(SCI_STYLESETFONT, STYLE_DEFAULT, FontName);
  SciMsg(SCI_STYLESETSIZE, STYLE_DEFAULT, FontSize);

  // Colors
  SciMsg(SCI_SETSELBACK, 1, ColorToRGB(clHighlight));
  SciMsg(SCI_SETSELFORE, 1, ColorToRGB(clHighlightText));

  {$IFDEF WINDOWS}
  if AppConfig.SciDirectWrite then begin
    // DirectWrite on Windows allows colorful emoji.
    SciMsg(SCI_SETTECHNOLOGY, SC_TECHNOLOGY_DIRECTWRITE);
    // Direct2D has buffering.
    SciMsg(SCI_SETBUFFEREDDRAW, 0);
  end;
  {$ENDIF}

  // Ctrl+Y, Ctrl+Shift+Z: Undo; Home/End are smarter.
  SciMsg(SCI_ASSIGNCMDKEY, (SCMOD_CTRL shl 16) or Ord('y'), SCI_REDO);
  SciMsg(SCI_ASSIGNCMDKEY, ((SCMOD_CTRL + SCMOD_SHIFT) shl 16) or Ord('Z'), SCI_REDO);
  SciMsg(SCI_ASSIGNCMDKEY, SCK_HOME, SCI_VCHOMEWRAP);
  SciMsg(SCI_ASSIGNCMDKEY, SCK_END, SCI_LINEENDWRAP);

  if LocaleUtils.LocaleName = 'cn' then begin
    SciMsg(SCI_SETLOCALE, SC_LOCALE_CN);
  end else begin
    SciMsg(SCI_SETLOCALE, SC_LOCALE_EN);
  end;
end;

procedure TSciEdit.CreateWnd;
{$ifdef Windows}
var
  WindowInfo: PWin32WindowInfo;
  {$endif}
begin
  if Assigned(Parent) and not Parent.HandleAllocated then begin
    Parent.HandleNeeded;
  end;

  Changing := False;

  {$ifdef Windows}
  Include(FWinControlFlags, wcfCreatingHandle);
  try
    if not HandleAllocated then begin
      // Main logic creating a Scintilla control.
      Handle := Windows.CreateWindowEx(0, 'Scintilla', '', WS_CHILD or WS_TABSTOP or
        WS_CLIPCHILDREN or WS_MAXIMIZE, 0, 0, 0, 0, Parent.Handle, 0, SciLibraryHandle, nil);
      pSciMsgFn := SciFnDirect(Windows.SendMessage(Handle, SCI_GETDIRECTFUNCTION, 0, 0));
      pSciWndData := Windows.SendMessage(Handle, SCI_GETDIRECTPOINTER, 0, 0);
      InitSettings;
      ShowWindow(Handle, SW_SHOWNA);
      // Ideally we don't replace Scintilla WndProc. However Scintilla eats "Escape" key press and we want to handle it.
      // WindowInfo glues hwnd and LCL window. See also win32callback and win32proc.
      WindowInfo := AllocWindowInfo(Handle);
      WindowInfo^.WinControl := Self;
      WindowInfo^.DefWndProc := Windows.WNDPROC(SetWindowLongPtr(Handle, GWL_WNDPROC,
        PtrUInt(@WrappedSciEditWndProc)));
    end;

    Constraints.UpdateInterfaceConstraints;
    InvalidateClientRectCache(False);

    if Assigned(Parent) then begin
      AddControl;
    end else if ParentWindow <> 0 then begin
      LCLIntf.SetParent(Handle, ParentWindow);
    end;

    Include(FWinControlFlags, wcfInitializing);
    InitializeWnd;
  finally
    Exclude(FWinControlFlags, wcfInitializing);
    Exclude(FWinControlFlags, wcfCreatingHandle);
  end;

  InvalidatePreferredSize;
  AdjustSize;
  {$else}
  raise EOSError.Create('SciEdit is not supported on this platform');
  {$endif}
end;


initialization
{$ifdef Windows}
  SciLibraryHandle := Windows.LoadLibrary('Scintilla.dll');
  AppConfig.HasSciEdit := (SciLibraryHandle <> 0);
{$else}
  SciLibraryHandle := 0;
{$endif}

end.
