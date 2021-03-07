unit Settings;

{$mode objfpc}{$H+}
{$M+}

interface

uses
  Classes, SysUtils, fpjsonrtti, Graphics, NoteTypes, FpJson, StdCtrls;

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

    // Callback.
    Callbacks: array of TConfigChangeCallback;

    procedure RunCallbacks(Name: string);
    procedure SetStayOnTop(Value: boolean);
    procedure SetFeatureLevel(Value: TFeatureLevel);
    procedure SetDockSide(Value: TDockSide);
    procedure SetZenMode(Value: boolean);
    procedure SetAutoSaveInterval(Value: integer);
  public
    // Not saved on disk. Do not trigger callbacks.
    ForceNotTop: boolean;
    MovingPreview: boolean;
    ResetOnNextStartup: boolean;
    RootTreeUrl: string;
    ConfigFileName: string;
    SaveFailureMessage: string;

    procedure RegisterOnChangeCallback(callback: TConfigChangeCallback);
    procedure NotifyAll;
  published
    // Trigger callbacks.
    property StayOnTop: boolean read FStayOnTop write SetStayOnTop;
    property DockSide: TDockSide read FDockSide write SetDockSide;
    property ZenMode: boolean read FZenMode write SetZenMode;
    property FeatureLevel: TFeatureLevel read FFeatureLevel write SetFeatureLevel;
    property AutoSaveInterval: integer read FAutoSaveInterval write SetAutoSaveInterval;

    // Do not trigger callbacks.
    property EditorFont: TFont read FEditorFont write FEditorFont;
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
  RunCallbacks('StayOnTop');
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
  FDockSide := Value;
  RunCallbacks('DockSide');
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

  // Defaults
  AppConfig.FMaxWidth := 600;
  AppConfig.FAutoSaveInterval := 30;
  AppConfig.FRememberPosition := True;
  AppConfig.ConfigFileName := 'FooNoteConfig.json';
  AppConfig.FWindowColor := clWindow;
  AppConfig.FEditorScrollBars := ssAutoVertical;

finalization
  FreeAndNil(AppConfig);
end.
