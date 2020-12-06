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
  if(pr <> nil) then
    ProgressBar1.Step:= pr.Progression;
  if(c1 <> nil) then
    ProgressBar2.Step:= c1.Progression;
  if(c2 <> nil) then
    ProgressBar3.Step:= c2.Progression;

  // TODO if c*.Progression >= MAX, then Enabled := false
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  sync := TPublisherSubscribersSynchronization.Create(8);
  pr := TPublisher.Create('/tmp/pr', sync); // @pre:  cp /etc/services /tmp/pr
  c1 := TSubscriber.Create('/tmp/c1', sync);
  c2 := TSubscriber.Create('/tmp/c2', sync);

  TimerProgression.Enabled:= True;
  pr.Resume; // Maintenant que tous les souscripteurs sont arrivés, on le lance
end;

end.

