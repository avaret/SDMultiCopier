program SDMultiCopier;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{ IFDEF UseCThreads}
  cthreads,
  {ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm,
  { you can add units after this }
  SynchrosThreadsRW, detectremovdisk, Semaphore;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormSDMultiCopier, FormSDMultiCopier);
  Application.Run;
end.

