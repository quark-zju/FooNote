unit Settings;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils, fpjsonrtti, Graphics, FpJson, StdCtrls, LogFFI;

type
  TDockSide = (dsNone = 0, dsLeft = 1, dsRight = 2);
  TFeatureLevel = (flBasic = 0, flAdvanced = 1, flDebug = 2);
  TAppConfig = class;
  TConfigChangeCallback = procedure(Name: string; NewConfig: TAppConfig);

  TJsonSerializable = class(TPersistent)
  public
    function ToJSON(): TJSONObject;
    function ToJSONString(): TJSONStringType;
    procedure LoadFromJSON(json: TJSONObject);
    procedure LoadFromJSONString(json: TJSONStringType);
  end;

  // Main state.
  TAppConfig = class(TJsonSerializable)
  private
    // Saved on disk. Changes will trigger callbacks.
    FStayOnTop: boolean;
    FDockSide: TDockSide;
    FDockMonitorIndex: integer;
    FFeatureLevel: TFeatureLevel;
    FZenMode: boolean;
    FAutoSaveInterval: integer;
    FTreeHorizonScrollBar: boolean;
    FNoteHorizonScrollBar: boolean;

    // Saved on disk. But changes do not trigger callbacks.
    FDockWidth: longint;
    FDockNoteSplitTop: longint;
    FNonDockWidth: longint;
    FNonDockHeight: longint;
    FNonDockNoteSplitTop: longint;
    FLeft: longint;
    FTop: longint;
    FMaxWidth: longint;
    FLocale: string;
    FNodeShowId: boolean;
    FRememberPosition: boolean;
    FLastSelection: Int32;
    FEditorFont: TFont;
    FTreeViewFont: TFont;
    FTreeViewSearchFont: TFont;
    FSearchBarFont: TFont;
    FEditorScrollBars: TScrollStyle;
    FWindowColor: TColor;
    FUseSciEdit: boolean;
    FSciDirectWrite: boolean;
    FShowMenuIcons: boolean;

    // Callback.
    Callbacks: array of TConfigChangeCallback;

    procedure RunCallbacks(Name: string);
    procedure SetStayOnTop(Value: boolean);
    procedure SetFeatureLevel(Value: TFeatureLevel);
    procedure SetDockSide(Value: TDockSide);
    procedure SetZenMode(Value: boolean);
    procedure SetAutoSaveInterval(Value: integer);
    procedure SetTreeHorizonScrollBar(Value: boolean);
    procedure SetNoteHorizonScrollBar(Value: boolean);
    procedure SetUseSciEdit(Value: boolean);
    procedure SetEditorFont(AFont: TFont);
  public
    // Not saved on disk. Do not trigger callbacks.
    ForceNotTop: boolean;
    MovingPreview: boolean;
    ResetOnNextStartup: boolean;
    RootTreeUrl: string;
    ConfigFileName: string;
    SaveFailureMessage: string;
    HasSciEdit: boolean;

    procedure RegisterOnChangeCallback(callback: TConfigChangeCallback);
    procedure NotifyAll;
  published
    // Trigger callbacks.
    property StayOnTop: boolean read FStayOnTop write SetStayOnTop;
    property DockSide: TDockSide read FDockSide write SetDockSide;
    property ZenMode: boolean read FZenMode write SetZenMode;
    property FeatureLevel: TFeatureLevel read FFeatureLevel write SetFeatureLevel;
    property AutoSaveInterval: integer read FAutoSaveInterval write SetAutoSaveInterval;
    property TreeHorizonScrollBar: boolean read FTreeHorizonScrollBar write SetTreeHorizonScrollBar;
    property NoteHorizonScrollBar: boolean read FNoteHorizonScrollBar write SetNoteHorizonScrollBar;
    property UseSciEdit: boolean read FUseSciEdit write SetUseSciEdit default True;

    // Do not trigger callbacks.
    property EditorFont: TFont read FEditorFont write SetEditorFont;
    property TreeViewFont: TFont read FTreeViewFont write FTreeViewFont;
    property TreeViewSearchFont: TFont read FTreeViewSearchFont write FTreeViewSearchFont;
    property SearchBarFont: TFont read FSearchBarFont write FSearchBarFont;

    property DockMonitorIndex: integer read FDockMonitorIndex write FDockMonitorIndex;
    property DockWidth: longint read FDockWidth write FDockWidth;
    property DockNoteSplitTop: longint read FDockNoteSplitTop write FDockNoteSplitTop;
    property NonDockWidth: longint read FNonDockWidth write FNonDockWidth;
    property NonDockHeight: longint read FNonDockHeight write FNonDockHeight;
    property NonDockNoteSplitTop: longint read FNonDockNoteSplitTop write FNonDockNoteSplitTop;
    property Left: longint read FLeft write FLeft;
    property Top: longint read FTop write FTop;
    property MaxWidth: longint read FMaxWidth write FMaxWidth;
    property Locale: string read FLocale write FLocale;
    property ShowNodeId: boolean read FNodeShowId write FNodeShowId;
    property RememberPosition: boolean read FRememberPosition write FRememberPosition;
    property LastSelectedId: Int32 read FLastSelection write FLastSelection;
    property WindowColor: TColor read FWindowColor write FWindowColor;
    property EditorScrollBars: TScrollStyle read FEditorScrollBars write FEditorScrollBars;
    property SciDirectWrite: boolean read FSciDirectWrite write FSciDirectWrite;
    property ShowMenuIcons: boolean read FShowMenuIcons write FShowMenuIcons;
  end;

const
  AnyConfigName = '_any_';

var
  AppConfig: TAppConfig;

implementation

procedure TAppConfig.RegisterOnChangeCallback(callback: TConfigChangeCallback);
begin
  Insert(Callback, Callbacks, 0);
end;

procedure TAppConfig.NotifyAll;
begin
  RunCallbacks(AnyConfigName);
end;

function TJsonSerializable.ToJSON(): TJSONObject;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSON(Self);
  finally
    FreeAndNil(Streamer);
  end;
end;

function TJsonSerializable.ToJSONString(): TJSONStringType;
var
  Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  try
    Result := Streamer.ObjectToJSONString(Self);
  finally
    FreeAndNil(Streamer);
  end;
end;

procedure TJsonSerializable.LoadFromJSON(json: TJSONObject);
var
  D: TJSONDeStreamer;
begin
  D := TJSONDeStreamer.Create(nil);
  try
    D.JSONToObject(json, self);
  finally
    FreeAndNil(D);
  end;
end;

procedure TJsonSerializable.LoadFromJSONString(json: TJSONStringType);
var
  D: TJSONDeStreamer;
begin
  D := TJSONDeStreamer.Create(nil);
  try
    D.JSONToObject(json, self);
  finally
    FreeAndNil(D);
  end;
end;

procedure TAppConfig.SetStayOnTop(Value: boolean);
begin
  FStayOnTop := Value;
  if LogHasDebug then begin
    LogDebug(Format('StayOnTop = %d', [FStayOnTop.ToInteger]));
  end;
  RunCallbacks('StayOnTop');
end;

procedure TAppConfig.SetTreeHorizonScrollBar(Value: boolean);
begin
  FTreeHorizonScrollBar := Value;
  RunCallbacks('TreeHorizonScrollBar');
end;

procedure TAppConfig.SetNoteHorizonScrollBar(Value: boolean);
begin
  FNoteHorizonScrollBar := Value;
  RunCallbacks('NoteHorizonScrollBar');
end;

procedure TAppConfig.SetUseSciEdit(Value: boolean);
begin
  if HasSciEdit then begin
    FUseSciEdit := Value;
  end else begin
    FUseSciEdit := False;
  end;
  RunCallbacks('UseSciEdit');
end;

procedure TAppConfig.SetEditorFont(AFont: TFont);
begin
  if Assigned(FEditorFont) then begin
    FEditorFont.Assign(AFont);
  end else begin
    FEditorFont := AFont;
  end;
  RunCallbacks('EditorFont');
end;

procedure TAppConfig.SetFeatureLevel(Value: TFeatureLevel);
begin
  FFeatureLevel := Value;
  RunCallbacks('FeatureLevel');
end;

procedure TAppConfig.SetDockSide(Value: TDockSide);
begin
{$ifndef Windows}
  Value := dsNone;
{$endif}
  if FDockSide <> Value then begin
    FDockSide := Value;
    RunCallbacks('DockSide');
  end;
end;

procedure TAppConfig.SetZenMode(Value: boolean);
begin
  FZenMode := Value;
  RunCallbacks('ZenMode');
end;

procedure TAppConfig.SetAutoSaveInterval(Value: integer);
begin
  FAutoSaveInterval := Value;
  RunCallbacks('AutoSaveInterval');
end;

procedure TAppConfig.RunCallbacks(Name: string);
var
  Callback: TConfigChangeCallback;
begin
  for Callback in Callbacks do begin
    Callback(Name, Self);
  end;
end;


initialization
  AppConfig := TAppConfig.Create;

  // Defaults (Property default is not effective).
  AppConfig.FMaxWidth := 600;
  AppConfig.FAutoSaveInterval := 30;
  AppConfig.FRememberPosition := True;
  AppConfig.ConfigFileName := 'FooNoteConfig.json';
  AppConfig.FWindowColor := clWindow;
  AppConfig.FUseSciEdit := True;
  AppConfig.FEditorScrollBars := ssAutoVertical;

  {$ifdef DARWIN}
  // On macOS, menu item with icons and without items align in an ugly way:
  //   macOS: [I] Text     Windows: [I] Text
  //          Text                      Text
  // Disable menu icons.
  AppConfig.ShowMenuIcons := False;
  {$else}
  AppConfig.ShowMenuIcons := True;
  {$endif}

finalization
  FreeAndNil(AppConfig);
end.
