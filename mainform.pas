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
    sync : TProducerConsumersSynchronization;
    pr : TProducer;
    c1, c2 : TConsumer;
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
  ProgressBar1.Step:= pr.Progression;
  ProgressBar2.Step:= c1.Progression;
  ProgressBar3.Step:= c2.Progression;

  // TODO if c*.Progression >= MAX, then Enabled := false
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  sync := TProducerConsumersSynchronization.Create(2, 8);
  pr := TProducer.Create('/tmp/pr', sync);
  c1 := TConsumer.Create('/tmp/c1', sync);
  c2 := TConsumer.Create('/tmp/c2', sync);
  TimerProgression.Enabled:= True;
end;

end.

