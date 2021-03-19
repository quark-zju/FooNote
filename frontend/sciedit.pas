unit SciEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, LCLIntf, LMessages, Graphics,
  Scintilla, Settings, LocaleUtils, LogFFI, Forms
  {$ifdef Windows}
  , Windows, win32int, win32proc
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
    // States. Useful if the Window is re-created (ex. docking, setting LCL form border).
    FText: string;
    FReadOnly: boolean;
    FColor: TColor;
    FWordWrap: boolean;
    FFont: TFont;
    FAnchor: PtrInt;
    FCaret: PtrInt;

    // Whether the SciEdit is fully initialized.
    // If not, avoid setting out OnChange events.
    IsInitialized: boolean;
  private
    // Inside SetText? (prevent OnChange events)
    Changing: boolean;
    FOnChange: TNotifyEvent;

    procedure InitSettings;
    procedure UpdateSelectionColor(AFocused: boolean);

    function GetText: string;
    procedure SetText(Value: string);
    function GetReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    function GetCaretPos: TPoint;
    function GetWordWrap: boolean;
    procedure SetWordWrap(Value: boolean);
    function GetSelStart: integer;
    procedure SetSelStart(Val: integer);
    function GetColor: TColor;
    procedure SetColor(Val: TColor);
  protected
    // https://www.scintilla.org/Steps.html
    procedure CreateWnd; override;
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

    function GetCachedText(var CachedText: TCaption): boolean; override;
  public
    function SciMsg(iMessage: UInt32; wParam: PtrUInt = 0; lParam: PtrInt = 0): PtrInt;
    function SciMsgSetStr(iMessage: UInt32; wParam: PtrUInt; S: string): PtrInt;
    function SciMsgGetStr(iMessage: UInt32; wParam: PtrUInt; Len: PtrInt): string;
  public
    class function IsAvailable: boolean;
    procedure SetSavepoint;
    procedure CopyToClipboard;
    procedure PasteFromClipboard;
    procedure SetDefaultFont(Value: TFont);

    constructor Create(TheOwner: TComponent); override;

    property Text: string read GetText write SetText;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly;
    property WordWrap: boolean read GetWordWrap write SetWordWrap default True;
    property CaretPos: TPoint read GetCaretPos;
    property SelStart: integer read GetSelStart write SetSelStart;
    property Color: TColor read GetColor write SetColor;
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
  FText := Value;
  if OrigReadOnly then begin
    SetReadOnly(False);
  end;
  // Prevent OnChange event.
  Changing := True;
  SciMsgSetStr(SCI_SETTEXT, 0, Value);
  Changing := False;
  if OrigReadOnly then begin
    SetReadOnly(OrigReadOnly);
  end;
end;

function TSciEdit.GetText: string;
var
  Len: PtrInt;
begin
  Len := SciMsg(SCI_GETLENGTH, 0, 0);
  Result := SciMsgGetStr(SCI_GETTEXT, Len + 1, Len);
end;

function TSciEdit.GetReadOnly: boolean;
begin
  Result := (SciMsg(SCI_GETREADONLY) <> 0);
end;

procedure TSciEdit.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  SciMsg(SCI_SETREADONLY, Value.ToInteger);
end;

function TSciEdit.GetWordWrap: boolean;
begin
  Result := (SciMsg(SCI_GETWRAPMODE) <> SC_WRAP_NONE);
end;

procedure TSciEdit.SetWordWrap(Value: boolean);
begin
  FWordWrap := Value;
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
  if Val < 0 then begin
    SciMsg(SCI_SETSEL, PtrUInt(-1), -1);
  end else begin
    SciMsg(SCI_GOTOPOS, Val);
  end;
end;

function TSciEdit.GetColor: TColor;
begin
  Result := SciMsg(SCI_STYLEGETBACK, STYLE_DEFAULT);
end;

procedure TSciEdit.SetColor(Val: TColor);
begin
  FColor := Val;
  SciMsg(SCI_STYLESETBACK, STYLE_DEFAULT, ColorToRGB(Val));
end;

procedure TSciEdit.SetDefaultFont(Value: TFont);
var
  FontSize: integer;
  FontName: string;
begin
  if Assigned(Value) then begin
    FFont.Assign(Value);
    FontSize := Round(Abs(Graphics.GetFontData(Value.Handle).Height) * 72 / Value.PixelsPerInch);
    FontName := Graphics.GetFontData(Value.Handle).Name;
    if LogHasDebug then begin
      LogDebug(Format('SciEdit Font: %s %d', [FontName, FontSize]));
    end;
    SciMsgSetStr(SCI_STYLESETFONT, STYLE_DEFAULT, FontName);
    SciMsg(SCI_STYLESETSIZE, STYLE_DEFAULT, FontSize);
  end;
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
  Line, LineLen, I: longint;
  LineContent: string;
begin
  code := Notif.nmhdr.code;
  if (code = SCN_PAINTED) or (code = SCN_STYLENEEDED) then begin
    exit;
  end;
  if (code = SCN_MODIFIED) and IsInitialized and Assigned(OnChange) and not Changing then begin
    OnChange(Self);
  end;
  if (code = SCN_CHARADDED) then begin
    if (Notif.ch = 10) or (Notif.ch = 13) then begin
      // Line: start from 0; new Line (after "\n")
      Line := CaretPos.Y;
      LineLen := SciMsg(SCI_LINELENGTH, Line);
      if (Line > 0) and (LineLen <= 2) and (SciMsg(SCI_GETSELECTIONS) <= 1) then begin
        // Count leading spaces from the previous line.
        LineLen := SciMsg(SCI_LINELENGTH, Line - 1);
        if LineLen < 1000 then begin
          LineContent := SciMsgGetStr(SCI_GETLINE, Line - 1, LineLen);
          for I := 0 to LineLen do begin
            case LineContent[I] of
              #9, #32: begin
                continue;
              end;
              else begin
                if I > 0 then begin
                  LineContent[I] := #0;
                  if LogHasDebug then begin
                    LogDebug(Format('Auto Indent (prefix: %d chars)', [I - 1]));
                  end;
                  SciMsg(SCI_REPLACESEL, 0, PtrInt(PChar(LineContent)));
                  break;
                end;
              end;
            end;
          end;
        end;
      end;
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

function TSciEdit.SciMsgSetStr(iMessage: UInt32; wParam: PtrUInt; S: string): PtrInt;
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

function TSciEdit.SciMsgGetStr(iMessage: UInt32; wParam: PtrUInt; Len: PtrInt): string;
var
  P: PChar;
begin
  P := StrAlloc(Len + 1);
  SciMsg(iMessage, wParam, PtrInt(P));
  P[Len] := #0;
  Result := StrPas(P);
  StrDispose(P);
end;

{$ifdef Windows}
function WrappedSciEditWndProc(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  WindowInfo: PWin32WindowInfo;
  This: TSciEdit;
  SciWndProc: WNDPROC;
  BypassLCL: boolean;
begin
  WindowInfo := GetWin32WindowInfo(Ahwnd);
  This := TSciEdit(WindowInfo^.WinControl);
  if not Assigned(This) then begin
    // No longer managed by LCL? Should not happen?
    Result := DefWindowProc(Ahwnd, uMsg, wParam, lParam);
    exit;
  end;

  // Should LCL handle the WndProc? Note LCL can also forward to SciWndProc.
  // LCL will emit LM/CN messages, and response to Action List.
  // See win32callback.inc. It calls CallDefaultWindowProc when WinProcess is True.
  // Pay attention to when it sets WinProcess to False.
  BypassLCL := False;

  // "WantTab". Do not make LCL handle "Tab" key.
  if ((uMsg >= WM_KEYFIRST) and (uMsg <= WM_KEYLAST)) and (wParam = VK_TAB) then begin
    BypassLCL := True;
  end;

  if (uMsg = WM_ERASEBKGND) or (uMsg = WM_PAINT) then begin
    BypassLCL := True;
  end;

  if (uMsg = WM_SETFOCUS) then begin
    This.UpdateSelectionColor(True);
  end;
  if (uMsg = WM_KILLFOCUS) then begin
    This.UpdateSelectionColor(False);
  end;

  if BypassLCL then begin
    // Scintilla WndProc.
    SciWndProc := WindowInfo^.DefWndProc;
    Result := CallWindowProc(SciWndProc, Ahwnd, uMsg, wParam, lParam);
  end else begin
    // LCL WndProc.
    Result := WindowProc(Ahwnd, uMsg, wParam, lParam);
  end;
end;

{$endif}

procedure TSciEdit.UpdateSelectionColor(AFocused: boolean);
begin
  if Focused then begin
    SciMsg(SCI_SETSELBACK, 1, ColorToRGB(clHighlight));
    SciMsg(SCI_SETSELFORE, 1, ColorToRGB(clHighlightText));
  end else begin
    SciMsg(SCI_SETSELBACK, 1, clSilver);
    SciMsg(SCI_SETSELFORE, 1, ColorToRGB(clWindowText));
  end;
end;

procedure TSciEdit.InitSettings;
begin
  // Utf-8!
  SciMsg(SCI_SETCODEPAGE, SC_CP_UTF8);
  SciMsg(SCI_SETEOLMODE, SC_EOL_LF);

  // Line-wrap, Scrolling
  SetWordWrap(True);

  // Popup
  SciMsg(SCI_USEPOPUP, SC_POPUP_TEXT);

  // IME
  SciMsg(SCI_SETIMEINTERACTION, SC_IME_INLINE);

  // Tab Indent
  SciMsg(SCI_SETTABWIDTH, 4);
  SciMsg(SCI_SETMARGINS, 0);

  // Multi-selection
  SciMsg(SCI_SETMULTIPLESELECTION, 1);
  SciMsg(SCI_SETADDITIONALSELECTIONTYPING, 1);

  // Colors
  SciMsg(SCI_SETSELBACK, 1, ColorToRGB(clHighlight));
  SciMsg(SCI_SETSELFORE, 1, ColorToRGB(clHighlightText));

  // Fonts
  if Assigned(Font) then begin
    SetDefaultFont(Font);
  end;

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

  // Restore some states.
  SetWordWrap(FWordWrap);
  SetReadOnly(FReadOnly);
  UpdateSelectionColor(Focused);
end;

procedure TSciEdit.FinalizeWnd;
begin
  FText := GetText;
  FAnchor := SciMsg(SCI_GETANCHOR);
  FCaret := SciMsg(SCI_GETCURRENTPOS);
  IsInitialized := False;
  inherited;
end;

function TSciEdit.GetCachedText(var CachedText: TCaption): boolean;
begin
  // Disable TWinControl.InitializeWnd from calling WSSetText.
  Result := False;
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
      // WrappedSciEditWndProc -> LCL WndProc -> Sci WndProc (WindowInfo.DefWndProc)
      // WrappedSciEditWndProc -> Sci WndProc (bypass LCL)
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

procedure TSciEdit.InitializeWnd;
begin
  // Restore more states.
  inherited;
  SetColor(FColor);
  SetDefaultFont(FFont);
  SetText(FText);
  SciMsg(SCI_SETSEL, FAnchor, FCaret);
  IsInitialized := True;
end;

constructor TSciEdit.Create(TheOwner: TComponent);
begin
  FText := '';
  FColor := clWindow;
  FWordWrap := True;
  FFont := Screen.SystemFont;
  IsInitialized := False;
  inherited;
end;

initialization
{$ifdef Windows}
  SciLibraryHandle := Windows.LoadLibrary('Scintilla.dll');
  AppConfig.HasSciEdit := (SciLibraryHandle <> 0);
{$else}
  SciLibraryHandle := 0;
{$endif}

end.
