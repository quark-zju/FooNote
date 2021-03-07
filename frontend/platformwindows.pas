unit PlatformWindows;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows, Forms, Controls, Settings, ShellAPI, LogFFI, LazUTF8;

procedure SetupMainForm(Form: TForm);
procedure StartMoving(Form: TForm);
procedure StartResizing(Form: TForm);
procedure SetClickThrough(Form: TForm);
procedure SetTransparentColor(Form: TForm; Color: COLORREF);
procedure RepositionDock;
procedure MoveTopMostWithoutFocus(Form: TForm);
procedure ShowInfocus(Form: TForm);

procedure EnsureWrappedWndProc;

implementation

resourcestring
  RSStayOnTop = 'Stay On &Top';
  RSDockLeft = 'Dock &Left';
  RSDockRight = 'Dock R&ight';

const
  WMToggleStayOnTop = WM_USER + 1;
  WMDockLeft = WM_USER + 2;
  WMDockRight = WM_USER + 3;
  WMAppBar = WM_USER + 4;

var
  // State needed but not associated with WndCallback.
  PrevWndProc: WNDPROC = nil;
  RefForm: TForm;

  // Width - ClientWidth. Workaround LCL issue where ClientWidt = Width. See
  // https://wiki.lazarus.freepascal.org/Lazarus_Faq#Why_are_TForm.ClientWidth.2FClientHeight_the_same_as_TForm.Width.2FHeight
  // Basically, LCL Width + OuterBorderWidth = Win32 RECT Width.
  OuterBorderWidth: longint;
  OuterBorderHeight: longint;
  OuterBorderTop: longint;
  OuterBorderLeft: longint;

  // Docking.
  // https://docs.microsoft.com/en-us/windows/win32/shell/application-desktop-toolbars
  BarData: APPBARDATA;
  BarRegistered: boolean = False;
  BarDockSide: TDockSide = dsNone;
  OrigRect: RECT;

  // WM_ENTERSIZEMOVE, WM_EXITSIZEMOVE, WM_MOVING.
  MovingStartRect: RECT;
  MovingStartCursorPos: TPoint;
  MovingHasMoved: boolean;

function MenuFlags(Checked: boolean): UINT; forward;

procedure SetTransparentColor(Form: TForm; Color: COLORREF);
begin
  SetLayeredWindowAttributes(Form.Handle, Color, Form.AlphaBlendValue, LWA_ALPHA + LWA_COLORKEY);
end;

procedure SetClickThrough(Form: TForm);
var
  S: LONG;
begin
  S := GetWindowLong(Form.Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT or WS_EX_LAYERED or WS_EX_NOACTIVATE;
  SetWindowLong(Form.Handle, GWL_EXSTYLE, S);
  S := GetWindowLong(Form.Handle, GWL_STYLE) or WS_POPUP;
  SetWindowLong(Form.Handle, GWL_STYLE, S);
  EnableWindow(Form.Handle, False);
end;

procedure MoveTopMostWithoutFocus(Form: TForm);
begin
  SetWindowPos(form.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure ShowInfocus(Form: TForm);
begin
  ShowWindow(form.Handle, SW_SHOWNOACTIVATE);
end;

procedure StartMoving(Form: TForm);
begin
  if LogHasDebug then begin
    LogDebug('Start moving via SC_MOVE');
  end;
  SendMessage(Form.Handle, WM_SYSCOMMAND, SC_MOVE, 0);
end;

procedure StartResizing(Form: TForm);
begin
  if LogHasDebug then begin
    LogDebug('Start resizing via SC_SIZE');
  end;
  PostMessage(Form.Handle, WM_SYSCOMMAND, SC_SIZE, 0);
end;

procedure UpdateCurrentMonitor();
var
  Point: TPoint;
  Monitor: TMonitor;
  I: integer;
begin
  Point := Mouse.CursorPos;
  Monitor := Screen.MonitorFromPoint(Point);
  for I := 0 to Screen.MonitorCount - 1 do begin
    if Screen.Monitors[I] = Monitor then begin
      if AppConfig.DockMonitorIndex <> I then begin
        if LogHasDebug then begin
          LogDebug(Format('Current Monitor: %d', [I]));
        end;
        AppConfig.DockMonitorIndex := I;
      end;
      exit;
    end;
  end;
end;

function GetDockMonitor(): TMonitor;
var
  I: integer;
begin
  I := AppConfig.DockMonitorIndex;
  if I >= Screen.MonitorCount then begin
    I := Screen.MonitorCount - 1;
  end;
  Exit(Screen.Monitors[I]);
end;

function RegisterAppBar: boolean;
begin
  if not BarRegistered then begin
    Assert(RefForm <> nil);
    Assert(BarData.cbSize = sizeof(BarData));
    BarData.hWnd := RefForm.Handle;
    BarData.uCallbackMessage := WMAppBar;
    BarData.lParam := 0;
    // https://docs.microsoft.com/en-us/windows/win32/shell/abm-new
    if SHAppBarMessage(ABM_NEW, @BarData) <> 0 then begin
      BarRegistered := True;
    end;
  end;
  Result := BarRegistered;
end;

procedure UnregisterAppBar;
begin
  if BarRegistered then begin
    SHAppBarMessage(ABM_REMOVE, @BarData);
    BarRegistered := False;
  end;
end;

procedure ProposeDockRect(side: TDockSide; var rc: RECT; preview: boolean = False);
var
  Width: longint;
  Monitor: TMonitor;
begin
  if side = dsNone then begin
    rc.Width := AppConfig.NonDockWidth + OuterBorderWidth;
    rc.Height := AppConfig.NonDockHeight + OuterBorderHeight;
    exit;
  end;

  if RefForm.WindowState = wsMinimized then begin
    Width := 0;
  end else begin
    if AppConfig.DockWidth = 0 then begin
      Width := AppConfig.NonDockWidth + OuterBorderWidth;
    end else begin
      Width := AppConfig.DockWidth;
    end;
  end;

  Monitor := GetDockMonitor();
  rc.Top := Monitor.WorkareaRect.Top;
  rc.Height := Monitor.WorkareaRect.Height;

  // preview: true for previewing moving/resize result (incompatible with ABM_QUERYPOS);
  // false for ABM_QUERYPOS.

  if (Side = dsRight) then begin
    if preview then begin
      rc.Left := Monitor.WorkareaRect.Right - Width;
    end else begin
      // Do use WorkspaceWidth! The current AppBar affects WorkspaceWidth and it might move recursively.
      rc.Left := Monitor.Width + Monitor.Left - Width;
    end;
    rc.Width := Width;
  end else if Side = dsLeft then begin
    if preview then begin
      rc.Left := Monitor.WorkareaRect.Left;
    end else begin
      rc.Left := Monitor.Left;
    end;
    rc.Width := Width;
  end;
end;

procedure DebugRectChange(prefix: string; a, b: RECT);
begin
  if a <> b then begin
    if LogHasDebug then begin
      LogDebug(Format('%s (%d,%d,%dx%d) -> (%d,%d,%dx%d)', [prefix, a.Left, a.Top, a.Width,
        a.Height, b.Left, b.Top, b.Width, b.Height]));
    end;
  end;
end;

procedure AdjustAppBarRect(side: TDockSide; var rc: RECT; ADebug: boolean = False);
var
  ProposedWidth, NewLeft: longint;
begin
  assert(BarRegistered);
  ProposedWidth := rc.Width;
  BarData.rc := rc;
  case Side of
    dsLeft: begin
      BarData.uEdge := ABE_LEFT;
    end;
    dsRight: begin
      BarData.uEdge := ABE_RIGHT;
    end;
  end;
  SHAppBarMessage(ABM_QUERYPOS, @BarData);
  if ADebug then begin
    DebugRectChange(' ABM_QUERYPOS ', rc, BarData.rc);
  end;
  rc := BarData.rc;
  if (rc.Width < ProposedWidth) then begin
    // Extend left instead of reduce size.
    // ABM_QUERYPOS can reduce width to avoid overlapping.
    if Side = dsRight then begin
      NewLeft := rc.Left - (ProposedWidth - rc.Width);
      if NewLeft > 0 then begin
        rc.Left := NewLeft;
        rc.Width := ProposedWidth;
        if ADebug then begin
          DebugRectChange(' Preserve Width Right ', BarData.rc, rc);
        end;
      end;
    end else if Side = dsLeft then begin
      rc.Width := ProposedWidth;
      if ADebug then begin
        DebugRectChange(' Preserve Width Left ', BarData.rc, rc);
      end;
    end;
  end else if (BarData.rc.Width > ProposedWidth) then begin
    // TODO: Can this happen?
  end;
end;

procedure RepositionDock;
var
  rc: RECT;
  Side: TDockSide;
begin
  EnsureWrappedWndProc;

  Side := AppConfig.DockSide;
  if (Side = dsNone) or (RefForm = nil) then begin
    exit;
  end;

  assert(BarRegistered);
  ProposeDockRect(Side, rc, False);
  AdjustAppBarRect(Side, rc, True);

  BarData.rc := rc;
  SHAppBarMessage(ABM_SETPOS, @BarData);
  DebugRectChange(' ABM_SETPOS ', rc, BarData.rc);

  if RefForm.WindowState = wsMinimized then begin
    exit;
  end;
  GetWindowRect(RefForm.Handle, rc);
  if rc <> BarData.rc then begin
    DebugRectChange(' Resize ', rc, BarData.rc);
    rc := BarData.rc;
    // Update LCL internals.
    assert(RefForm.BorderStyle = bsNone);
    RefForm.SetBounds(rc.Left, rc.Top, rc.Width, rc.Height);
    //MoveWindow(RefForm.Handle, rc.Left, rc.Top, rc.Width, rc.Height, True);
  end;
end;

procedure ApplyDock;
var
  Side: TDockSide;
  S1, S2: string;
begin
  Side := AppConfig.DockSide; // Desired (new) config.
  WriteStr(S1, BarDockSide);
  WriteStr(S2, Side);
  if BarDockSide = Side then begin
    exit;
  end;
  if LogHasDebug then begin
    LogDebug(Format('ApplyDock %s -> %s', [S1, S2]));
  end;

  if BarDockSide = dsNone then begin
    // Remember original position.
    GetWindowRect(RefForm.Handle, OrigRect);
  end;

  if Side = dsNone then begin
    // Undock
    if LogHasDebug then begin
      LogDebug('  Undock');
    end;
    AppConfig.MovingPreview := True;
    UnregisterAppBar;
    // Restore position and style.
    RefForm.BorderStyle := bsSizeable;
    RefForm.BorderIcons := RefForm.BorderIcons + [biMinimize, biSystemMenu];
    RefForm.SetBounds(OrigRect.Left - OuterBorderLeft, OrigRect.Top - OuterBorderTop,
      AppConfig.NonDockWidth, AppConfig.NonDockHeight);
    //MoveWindow(RefForm.Handle, OrigRect.Left, OrigRect.Top,
    //  AppConfig.NonDockWidth + OuterBorderWidth,
    //  AppConfig.NonDockHeight + OuterBorderHeight, True);
    // LCL might add biMaximize automatically. Drop it.
    // Without biMaximize, "Snap assistant" won't try to snap to left/right half of the screen.
    RefForm.BorderIcons := RefForm.BorderIcons - [biMaximize];
    EnsureWrappedWndProc;
    AppConfig.MovingPreview := False;
  end else begin
    // Dock
    RefForm.BorderStyle := bsNone;
    RefForm.BorderIcons := RefForm.BorderIcons - [biMinimize, biSystemMenu];
    // WndProc might be overridden by LCL.
    EnsureWrappedWndProc;
    if not RegisterAppBar then begin
      if LogHasWarn then begin
        LogWarn('  Failed to register AppBar');
      end;
      exit;
    end;
    RepositionDock;
  end;
  BarDockSide := Side;
end;


procedure OnConfigChange(Name: string; Config: TAppConfig);
var
  WRSStayOnTop, WRSDockLeft, WRSDockRight: UnicodeString;
  SysMenu: HMENU;
begin
  if (Name = AnyConfigName) or (Name = 'StayOnTop') then begin
    SysMenu := GetSystemMenu(RefForm.Handle, False);

    WRSStayOnTop := UTF8ToUTF16(RSStayOnTop);
    WRSDockLeft := UTF8ToUTF16(RSDockLeft);
    WRSDockRight := UTF8ToUTF16(RSDockRight);
    ModifyMenuW(SysMenu, WMToggleStayOnTop, MenuFlags(Config.StayOnTop), WMToggleStayOnTop, PWChar(WRSStayOnTop));
    ModifyMenuW(SysMenu, WMDockLeft, MenuFlags(AppConfig.DockSide = dsLeft), WMDockLeft, PWChar(WRSDockLeft));
    ModifyMenuW(SysMenu, WMDockRight, MenuFlags(AppConfig.DockSide = dsRight), WMDockRight, PWChar(WRSDockRight));
  end;
  if (Name = AnyConfigName) or (Name = 'DockSide') then begin
    ApplyDock;
  end;
end;

function InferDockSide(CursorPos: TPoint): TDockSide;
var
  Area: TRect;
  Monitor: TMonitor;
const
  Threshold = 70;
begin
  Monitor := GetDockMonitor();
  Area := Monitor.WorkAreaRect;
  if CursorPos.X <= Area.Left + Threshold then begin
    Result := dsLeft;
  end else if CursorPos.X + 1 + Threshold >= Area.Right then begin
    Result := dsRight;
  end else begin
    Result := dsNone;
  end;
end;

// Preview of moving window RECT. Consider docking. Write docking preview to Side.
function MovingPreviewRect(var Side: TDockSide): RECT;
var
  Pos: TPoint;
begin
  // Calculate "undocked" RECT by original window size and cursor movement.
  Result := MovingStartRect;
  Pos := Mouse.CursorPos;
  Result.Offset(Pos - MovingStartCursorPos);
  UpdateCurrentMonitor;
  Side := InferDockSide(Pos);
  // Calculate "docked" RECT.
  ProposeDockRect(Side, Result, True);
end;

function WrappedWndProc(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam): LRESULT; stdcall;
var
  PtrRect: PRECT;
  Side: TDockSide;
begin
  if uMsg <> WM_SETCURSOR then begin
    if LogHasTrace then begin
      LogTrace(Format('WrappedWndProc uMsg=%d wParam=%d', [uMsg, wParam]));
    end;
  end;
  if (uMsg = WM_SYSCOMMAND) then begin
    case wParam of
      WMToggleStayOnTop: begin
        AppConfig.StayOnTop := not AppConfig.StayOnTop;
      end;
      WMDockLeft: begin
        AppConfig.DockSide := dsLeft;
      end;
      WMDockRight: begin
        AppConfig.DockSide := dsRight;
      end;
    end;
  end else if (uMsg = WM_ENTERSIZEMOVE) then begin
    if LogHasDebug then begin
      LogDebug('EnterSizeMove');
    end;
    AppConfig.MovingPreview := True;
    GetWindowRect(RefForm.Handle, MovingStartRect);
    MovingStartCursorPos := Mouse.CursorPos;
    MovingHasMoved := False;
  end else if (uMsg = WM_EXITSIZEMOVE) then begin
    if LogHasDebug then begin
      LogDebug(Format('ExitSizeMove: Moved=%d', [Ord(MovingHasMoved)]));
    end;
    if MovingHasMoved then begin
      // Do not update DockSize if nothing has moved (ex. resize).
      MovingPreviewRect(Side);
      AppConfig.DockSide := Side;
    end;
    MovingHasMoved := False;
    AppConfig.MovingPreview := False;
    // OnResize was called before WrappedWndProc with AppConfigMovingPreview = True.
    RefForm.OnResize(RefForm);
  end else if (uMsg = WM_MOVING) then begin
    // WM_ENTERSIZEMOVE; WM_MOVING, WM_MOVING, ...; WM_EXITSIZEMOVE
    // Compatible with "Show window contents while dragging" ticked/unticked.
    MovingHasMoved := True;
    PtrRect := PRECT(LParam);
    PtrRect^ := MovingPreviewRect(Side);
  end else if wParam = WM_ACTIVATE then begin
    if BarRegistered then begin
      SHAppBarMessage(ABM_ACTIVATE, @BarData);
    end;
  end else if wParam = WM_WINDOWPOSCHANGED then begin
    if LogHasDebug then begin
      LogDebug('WindowPosChanged');
    end;
    if BarRegistered then begin
      SHAppBarMessage(ABM_WINDOWPOSCHANGED, @BarData);
    end;
  end else if (uMsg = WMAppBar) then begin
    if LogHasDebug then begin
      LogDebug('WMAppBar');
    end;
    case wParam of
      ABN_STATECHANGE: begin
        if LogHasDebug then begin
          LogDebug(' ABN_STATECHANGE');
        end;
        // Windows Taskbar "always on top" change. Reposition.
        RepositionDock;
      end;
      ABN_FULLSCREENAPP: begin
        if LogHasDebug then begin
          LogDebug(Format(' ABN_FULLSCREENAPP %d', [lParam]));
        end;
        // Fullscreen app starts or exits.
        // Avoid "Stay on Top" if a fullscreen app is running.
        AppConfig.ForceNotTop := (lParam <> 0);
        // Trigger updating window style.
        if lParam = 0 then begin
          // Refresh "On Top" setting.
          AppConfig.StayOnTop := not AppConfig.StayOnTop;
          AppConfig.StayOnTop := not AppConfig.StayOnTop;
        end else begin
          // Put to bottom.
          SetWindowPos(Ahwnd, HWND_BOTTOM,
            0, 0, 0, 0,
            SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
        end;
      end;
      ABN_POSCHANGED: begin
        // Other AppBar changed. AppBar needs to be resized.
        if LogHasDebug then begin
          LogDebug(' ABN_POSCHANGED');
        end;
        RepositionDock;
      end;
    end;
  end;
  Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;

function MenuFlags(Checked: boolean): UINT;
begin
  Result := 0;
  if Checked then begin
    Result := MF_CHECKED;
  end;
end;

procedure EnsureWrappedWndProc;
var
  Handle: HWND;
  SysMenu: HMENU;
  WRSStayOnTop, WRSDockLeft, WRSDockRight: UnicodeString;
begin
  if RefForm = nil then begin
    exit;
  end;
  Handle := RefForm.Handle;
  if GetWindowLongPtr(Handle, GWL_WNDPROC) = PtrUInt(@WrappedWndProc) then begin
    exit;
  end;
  if LogHasDebug then begin
    LogDebug('Setup WndProc');
  end;
  Handle := RefForm.Handle;
  BarData.hWnd := Handle;

  WRSStayOnTop := UTF8ToUTF16(RSStayOnTop);
  WRSDockLeft := UTF8ToUTF16(RSDockLeft);
  WRSDockRight := UTF8ToUTF16(RSDockRight);

  SysMenu := GetSystemMenu(Handle, False);
  AppendMenu(SysMenu, MF_SEPARATOR, 0, '');
  AppendMenuW(SysMenu, MenuFlags(AppConfig.DockSide = dsLeft), WMDockLeft, PWChar(WRSDockLeft));
  AppendMenuW(SysMenu, MenuFlags(AppConfig.DockSide = dsRight), WMDockRight, PWChar(WRSDockRight));
  AppendMenu(SysMenu, MF_SEPARATOR, 0, '');
  AppendMenuW(SysMenu, MenuFlags(AppConfig.StayOnTop), WMToggleStayOnTop, PWChar(WRSStayOnTop));
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(Handle, GWL_WNDPROC, PtrUInt(@WrappedWndProc)));
end;

procedure SetupMainForm(Form: TForm);
var
  R: RECT;
begin
  if PrevWndProc = nil then begin
    GetWindowRect(Form.Handle, R);
    OuterBorderWidth := R.Width - Form.Width;
    OuterBorderHeight := R.Height - Form.Height;
    OuterBorderLeft := R.Left - Form.Left;
    OuterBorderTop := R.Top - Form.Top;
    RefForm := Form;
    EnsureWrappedWndProc;
    AppConfig.RegisterOnChangeCallback(@OnConfigChange);
  end;
end;

initialization
  BarData.cbSize := sizeof(BarData);

  if not SysUtils.GetEnvironmentVariable('FOONOTE_LOG').IsEmpty() then begin
    // Allocate a console if logging is enabled.
    AllocConsole;
    // AttachConsole does not work well - parent cmd.exe won't wait for us.
    // AttachConsole(ATTACH_PARENT_PROCESS);
  end;

finalization
  UnregisterAppBar;

end.
