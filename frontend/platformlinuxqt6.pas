unit PlatformLinuxQt6;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, Controls;

procedure SetupMainForm(Form: TForm);
procedure RepositionDock;
procedure TeardownMainForm;

implementation

uses
  Settings, Qt6, QtWidgets;

type
  TLayerShellWindow = Pointer;

function FooNoteLayerShellSupported: Boolean; cdecl;
  external 'foonote_layer_shell' name 'foonote_layer_shell_supported';
function FooNoteLayerShellCreate(Window: QWindowH; Side, Width: Integer): TLayerShellWindow; cdecl;
  external 'foonote_layer_shell' name 'foonote_layer_shell_create';
procedure FooNoteLayerShellConfigure(LayerWindow: TLayerShellWindow; Side, Width: Integer); cdecl;
  external 'foonote_layer_shell' name 'foonote_layer_shell_configure';
procedure FooNoteLayerShellDestroy(LayerWindow: TLayerShellWindow); cdecl;
  external 'foonote_layer_shell' name 'foonote_layer_shell_destroy';

var
  RefForm: TForm;
  LayerWindow: TLayerShellWindow;

function NativeWindow: QWindowH;
var
  Widget: QWidgetH;
begin
  Widget := TQtWidget(RefForm.Handle).Widget;
  Result := QWidget_windowHandle(Widget);
  if Result = nil then begin
    QWidget_winId(Widget);
    Result := QWidget_windowHandle(Widget);
  end;
end;

function CurrentDockWidth: Integer;
begin
  Result := AppConfig.DockWidth;
  if Result <= 0 then begin
    Result := AppConfig.NonDockWidth;
  end;
  if Result <= 0 then begin
    Result := RefForm.Width;
  end;
end;

procedure RecreateSurface(Side: TDockSide);
var
  WasVisible: Boolean;
  Window: QWindowH;
begin
  WasVisible := RefForm.Visible;
  RefForm.Hide;

  Window := NativeWindow;
  if LayerWindow <> nil then begin
    FooNoteLayerShellDestroy(LayerWindow);
    LayerWindow := nil;
  end;
  QWindow_destroyPlatformResources(Window);

  if Side = dsNone then begin
    RefForm.BorderStyle := bsSizeable;
    if AppConfig.NonDockWidth > 0 then begin
      RefForm.Width := AppConfig.NonDockWidth;
    end;
    if AppConfig.NonDockHeight > 0 then begin
      RefForm.Height := AppConfig.NonDockHeight;
    end;
  end else begin
    RefForm.BorderStyle := bsNone;
    Window := NativeWindow;
    QWindow_destroyPlatformResources(Window);
    LayerWindow := FooNoteLayerShellCreate(Window, Ord(Side), CurrentDockWidth);
  end;

  if WasVisible then begin
    RefForm.Show;
  end;
end;

procedure RepositionDock;
begin
  if (LayerWindow <> nil) and (AppConfig.DockSide <> dsNone) then begin
    FooNoteLayerShellConfigure(LayerWindow, Ord(AppConfig.DockSide), CurrentDockWidth);
  end;
end;

procedure OnConfigChange(Name: string; Config: TAppConfig);
begin
  if (Name = AnyConfigName) or (Name = 'DockSide') then begin
    if not FooNoteLayerShellSupported then begin
      Exit;
    end;
    if ((Config.DockSide = dsNone) and (LayerWindow <> nil)) or
       ((Config.DockSide <> dsNone) and (LayerWindow = nil)) then begin
      RecreateSurface(Config.DockSide);
    end else begin
      RepositionDock;
    end;
  end;
end;

procedure SetupMainForm(Form: TForm);
begin
  RefForm := Form;
  AppConfig.RegisterOnChangeCallback(@OnConfigChange);
end;

procedure TeardownMainForm;
begin
  if LayerWindow <> nil then begin
    FooNoteLayerShellDestroy(LayerWindow);
    LayerWindow := nil;
  end;
  RefForm := nil;
end;

end.
