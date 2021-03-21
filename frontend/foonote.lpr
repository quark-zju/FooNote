program foonote;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'FooNote';
{$ifdef Windows}
  // Window Minimize will have the right (minimize, not hide) animations.
  Application.MainFormOnTaskBar:=true;
{$endif}
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormFooNoteMain, FormFooNoteMain);
  Application.Run;
end.
