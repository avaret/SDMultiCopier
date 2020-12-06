unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, RichMemo, SynEdit,
  SynchrosThreadsRW;



type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    testthread: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    AppLog: TRichMemo;
    TimerProgression: TTimer;
    procedure Button1Click(Sender: TObject);
//    procedure CallbackCopy(Sender: TObject; AData : Pointer);
    procedure testthreadClick(Sender: TObject);
    procedure TimerProgressionTimer(Sender: TObject);
  private

  public
    sync : TPublisherSubscribersSynchronization;
    pr : TPublisher;
    c1, c2 : TSubscriber;
  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

{ TForm1 }
          {
procedure TForm1.CallbackCopy(Sender: TObject; AData : Pointer);
begin
  TThread.Synchronize(
    (Sender as TProgressBar).StepIt
    );
end;     }

procedure TForm1.testthreadClick(Sender: TObject);
begin

end;

procedure TForm1.TimerProgressionTimer(Sender: TObject);
begin
  ProgressBar1.tag := IntPtr(pr.Progression - ProgressBar1.Position); // Contient donc la dérivée !
  // Mais il faut mieux avoir une barre pour l'avancement + une pour la vitesse !

  if(pr <> nil) then
    ProgressBar1.Position:= pr.Progression;
  if(c1 <> nil) then
    ProgressBar2.Position:= c1.Progression;
  if(c2 <> nil) then
    ProgressBar3.Position:= c2.Progression;

  writeln('progression : ', ProgressBar1.Position, '/', ProgressBar2.Position, '/',  ProgressBar3.Position, ' delta pr = ', Integer(ProgressBar1.tag));


  // TODO if c*.Progression >= MAX, then Enabled := false
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ProgressBar1.Max:= 9800;
  ProgressBar2.Max:= 9800;
  ProgressBar3.Max:= 9800;

  sync := TPublisherSubscribersSynchronization.Create(8);
  pr := TPublisher.Create('/tmp/pr', sync); // @pre:  cp /etc/services /tmp/pr
  c1 := TSubscriber.Create('/tmp/c1', sync);
  c2 := TSubscriber.Create('/tmp/c2', sync);

  TimerProgression.Enabled:= True;
  pr.Resume; // Maintenant que tous les souscripteurs sont arrivés, on le lance
end;

end.

