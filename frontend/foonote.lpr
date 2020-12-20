program foonote;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  MainForm,
  RunTimeTypeInfoControls;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='FooNote';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFooNoteForm, FooNoteForm);
  Application.Run;
end.
