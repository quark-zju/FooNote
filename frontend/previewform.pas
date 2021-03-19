unit PreviewForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
  PlatformWindows,
  {$endif}
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls;

type

  { TSolidColorPreviewForm }

  TSolidColorPreviewForm = class(TForm)
    MainImage: TImage;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

procedure EnsureInit;
procedure ShowSolidColorAt(Left, Top, Width, Height: longint);
procedure ShowCanvasAt(ALeft, ATop: longint);
procedure Hide;
function PrepareCleanCanvas(AWidth, AHeight: longint): TCanvas;

var
  SolidColorPreviewForm: TSolidColorPreviewForm;

implementation

{$R *.lfm}

const
  NegPos = -30000;

procedure EnsureInit;
begin
  if SolidColorPreviewForm = nil then begin
    Application.CreateForm(TSolidColorPreviewForm, SolidColorPreviewForm);
    // "Invisible". Avoids stealing the focus when making the window "visible".
    SolidColorPreviewForm.SetBounds(NegPos, NegPos, 0, 0);
    assert(SolidColorPreviewForm.Width = 0);
    assert(SolidColorPreviewForm.Height = 0);
    // TODO: Transparent preview window supported by other platforms.
    {$ifdef Windows}
    SolidColorPreviewForm.Visible := True;
    {$endif}
  end;
end;

function MainCanvas: TCanvas;
begin
  EnsureInit;
  Result := SolidColorPreviewForm.MainImage.Canvas;
end;

function PrepareCleanCanvas(AWidth, AHeight: longint): TCanvas;
begin
  EnsureInit;
  with SolidColorPreviewForm.MainImage do begin
    SetBounds(0, 0, AWidth, AHeight);
    // Resize the canvas after first drawn.
    // https://forum.lazarus.freepascal.org/index.php?topic=10368.0
    Picture.Bitmap.SetSize(AWidth, AHeight);
    Canvas.Clear;
    Canvas.Brush.Color := clFuchsia;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(0, 0, AWidth, AHeight);
    Result := Canvas;
  end;
end;

procedure ShowCanvasAt(ALeft, ATop: longint);
begin
  EnsureInit;
  {$ifndef Windows}
  exit;
{$endif}
  with SolidColorPreviewForm do begin
    SetBounds(ALeft, ATop, MainImage.Width, MainImage.Height);
    if not MainImage.Visible then begin
      MainImage.Visible := True;
    end;
    {$ifdef Windows}
    PlatformWindows.MoveTopMostWithoutFocus(SolidColorPreviewForm);
    {$endif}
  end;
end;


procedure ShowSolidColorAt(Left, Top, Width, Height: longint);
begin
  EnsureInit;
  {$ifndef Windows}
  exit;
{$endif}
  SolidColorPreviewForm.MainImage.Hide;
  SolidColorPreviewForm.SetBounds(Left, Top, Width, Height);
  {$ifdef Windows}
  PlatformWindows.MoveTopMostWithoutFocus(SolidColorPreviewForm);
  {$endif}
end;

procedure Hide;
begin
  if (SolidColorPreviewForm <> nil) and (SolidColorPreviewForm.Width > 0) then begin
    SolidColorPreviewForm.SetBounds(NegPos, NegPos, 0, 0);
  end;
end;

{ TSolidColorPreviewForm }

procedure TSolidColorPreviewForm.FormCreate(Sender: TObject);
begin
  // Make the window "click-through" so events won't be sent to this form.
  {$ifdef Windows}
  PlatformWindows.SetTransparentColor(Self, clFuchsia);
  PlatformWindows.SetClickThrough(Self);
  {$endif}
end;

end.
