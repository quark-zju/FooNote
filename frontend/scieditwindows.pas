unit SciEditWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LCLType, LCLIntf, LMessages, Graphics,
  Scintilla,
  Windows, win32int, win32proc;

type
  // Main control.
  TSciEdit = class(TWinControl)
  private
    // https://www.scintilla.org/ScintillaDoc.html#DirectAccess
    pSciWndData: PtrInt;
    pSciMsgFn: SciFnDirect;
    function SciMsg(iMessage: UInt32; wParam: PtrUInt = 0; lParam: PtrInt = 0): PtrInt;
    function SciMsgCStr(iMessage: UInt32; wParam: PtrUInt; S: string): PtrInt;
    // https://www.scintilla.org/ScintillaDoc.html#Notifications
    procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
    procedure SciNotify(var Notif: SCNotification);
  private
    FOnChange: TNotifyEvent;
  private
    procedure InitSettings;
    function GetText: string;
    procedure SetText(Value: string);
  protected
    // https://www.scintilla.org/Steps.html
    procedure CreateWnd; override;
  public
    class function IsAvailable: boolean;
    procedure SetSavepoint;
    property Text: string read GetText write SetText;
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

  SciMsg(SCI_SETSCROLLWIDTH, 100);
  SciMsg(SCI_SETSCROLLWIDTHTRACKING, 1);
end;

procedure TSciEdit.SetText(Value: string);
begin
  SciMsgCStr(SCI_SETTEXT, 0, Value);
end;

function TSciEdit.GetText: string;
var
  Len: PtrInt;
  P: PChar;
begin
  Len := SciMsg(SCI_GETLENGTH, 0, 0);
  WriteLn('Len ', Len);
  P := StrAlloc(Len + 1);
  SciMsg(SCI_GETTEXT, Len + 1, PtrInt(P));
  Result := StrPas(P);
  StrDispose(P);
  WriteLn('Text: ', Result);
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
  if (code = SCN_MODIFIED) and Assigned(OnChange) then begin
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
  Result := pSciMsgFn(pSciWndData, iMessage, wParam, lParam);
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
    Writeln('Not MANAGED');
    Result := DefWindowProc(Ahwnd, uMsg, wParam, lParam);
    exit;
  end;

  // Copy WndProc, WM_NCDESTROY might de-alloc WindowInfo.
  SciWndProc := WindowInfo^.DefWndProc;

  ShouldHandle := False;

  // Forward some events as LCL CM message.
  if ((uMsg = WM_KEYDOWN) or (uMsg = WM_KEYUP)) and (wParam = VK_ESCAPE) then begin
    ShouldHandle := True;
  end;
  if (uMsg = WM_SETFOCUS) or (uMsg = WM_KILLFOCUS) then begin
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

procedure TSciEdit.InitSettings;
var
  FontSize: integer;
  FontName: string;
begin
  // Utf-8!
  SciMsg(SCI_SETCODEPAGE, SC_CP_UTF8);

  // Line-wrap
  //SciMsg(SCI_SETWRAPMODE, SC_WRAP_CHAR);
  //SciMsg(SCI_SETWRAPVISUALFLAGS, SC_WRAPVISUALFLAG_START);

  // IME
  SciMsg(SCI_SETIMEINTERACTION, SC_IME_INLINE);

  // Tab Indent
  SciMsg(SCI_SETTABWIDTH, 2);
  SciMsg(SCI_SETMARGINS, 0);

  // Scrolling
  SciMsg(SCI_SETSCROLLWIDTH, 100);
  SciMsg(SCI_SETSCROLLWIDTHTRACKING, 1);

  // Multi-selection
  SciMsg(SCI_SETMULTIPLESELECTION, 1);
  SciMsg(SCI_SETADDITIONALSELECTIONTYPING, 1);

  // Font
  FontSize := Round(Abs(Graphics.GetFontData(Font.Reference.Handle).Height) * 72 / Font.PixelsPerInch);
  FontName := Font.Name;
  if FontName = 'default' then begin
    FontName := 'Microsoft YaHei';
  end;
  WriteLn('FontSize ', FontSize, ' Name ', FontName);
  SciMsgCStr(SCI_STYLESETFONT, STYLE_DEFAULT, FontName);
  SciMsg(SCI_STYLESETSIZE, STYLE_DEFAULT, FontSize);

  // Colors
  SciMsg(SCI_SETSELBACK, 1, ColorToRGB(clHighlight));
  SciMsg(SCI_SETSELFORE, 1, ColorToRGB(clHighlightText));

  {$IFDEF WINDOWS}
  // DirectWrite on Windows allows colorful emoji.
  SciMsg(SCI_SETTECHNOLOGY, SC_TECHNOLOGY_DIRECTWRITE);
  // Direct2D has buffering.
  SciMsg(SCI_SETBUFFEREDDRAW, 0);
  {$ENDIF}

  // Ctrl+Y, Ctrl+Shift+Z: Undo; Home/End are smarter.
  SciMsg(SCI_ASSIGNCMDKEY, (SCMOD_CTRL shl 16) or Ord('y'), SCI_REDO);
  SciMsg(SCI_ASSIGNCMDKEY, ((SCMOD_CTRL + SCMOD_SHIFT) shl 16) or Ord('Z'), SCI_REDO);
  SciMsg(SCI_ASSIGNCMDKEY, SCK_HOME, SCI_VCHOMEWRAP);
  SciMsg(SCI_ASSIGNCMDKEY, SCK_END, SCI_LINEENDWRAP);

  SciMsg(SCI_SETLOCALE, SC_LOCALE_EN);
end;

procedure TSciEdit.CreateWnd;
var
  WindowInfo: PWin32WindowInfo;
begin
  if Assigned(Parent) and not Parent.HandleAllocated then begin
    Parent.HandleNeeded;
  end;

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
end;


initialization
  SciLibraryHandle := Windows.LoadLibrary('Scintilla.dll');

end.
