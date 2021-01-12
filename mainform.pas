unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, RichMemo, SynEdit,
  SynchrosThreadsRW, detectRemovDisk;


type

  { TAvanceur : un panneau affichant l'état pour une carte mémoire }

  TAvanceur = class(TPanel)
    lblCible : TLabel;
    prgProgression, prgRapidite : TProgressBar;
    btnAbandonner : TButton;
    ownerForm : TForm;

    constructor Create(TheOwner: TComponent; cCaption : String; avcMax : Integer);
  public
    procedure Avancer(pas : Integer); // Pour faire avancer les barres de progression...
    procedure SetRapiditeMax(newValue : Integer);
  end;


type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonAbandonner: TButton;
    PanelAvanceurs: TPanel;
    testthread: TButton;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    ProgressBar3: TProgressBar;
    AppLog: TRichMemo;
    TrouverDisques: TTimer;
    TimerProgression: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonAbandonnerClick(Sender: TObject);
    procedure testthreadClick(Sender: TObject);
    procedure TimerProgressionTimer(Sender: TObject);
    procedure ChangerRapidite(newValue : integer);
    procedure TrouverDisquesTimer(Sender: TObject);
  private

  public
    sync : TPublisherSubscribersSynchronization;
    pr : TPublisher;
    c1, c2 : TSubscriber;

    Avanceurs : array of TAvanceur;
  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

{ TAvanceur }

constructor TAvanceur.Create(TheOwner: TComponent; cCaption : String; avcMax : Integer);
begin
  inherited Create(TheOwner);
  Parent := TheOwner as TPanel;
  ownerForm := (TheOwner.GetParentComponent as TForm); // Déclenchera une exception sinon

  Align := alTop;
  Height := 50;
  BorderSpacing.Around:=15;
  ChildSizing.LeftRightSpacing:=10;
  ChildSizing.TopBottomSpacing:=ChildSizing.LeftRightSpacing;

  lblCible := TLabel.Create(Self);
  lblCible.Parent := Self;
  lblCible.Caption := cCaption;
  lblCible.Align:=alLeft;
  lblCible.BorderSpacing.Around:=8;

  prgProgression := TProgressBar.Create(Self);
  prgProgression.Parent := Self;
  prgProgression.Left:= 100;
  prgProgression.Width:= 200;
  prgProgression.Max:= avcMax;
  prgProgression.BarShowText:=true;
  prgProgression.Smooth:=true;
  prgProgression.Align:=alLeft;
  prgProgression.BorderSpacing.Around:=8;

  prgRapidite := TProgressBar.Create(Self);
  prgRapidite.Parent := Self;
  prgRapidite.Left:= 350;
  prgRapidite.Width:= 200;
  prgRapidite.Max:= 50;
  prgRapidite.BarShowText:=true;
  prgRapidite.Smooth:=true;
  prgRapidite.Align:=alLeft;
  prgRapidite.BorderSpacing.Around:=8;

  btnAbandonner := TButton.Create(Self);
  btnAbandonner.Parent := Self;
  btnAbandonner.Left:=600;
  btnAbandonner.OnClick := (ownerForm as TForm1).ButtonAbandonner.OnClick; // MainForm...
  btnAbandonner.Align:=alLeft;
end;

procedure TAvanceur.Avancer(pas : Integer); // Pour faire avancer les barres de progression...
var rapid : Integer;
begin
  prgProgression.Tag := prgProgression.Position; // On stocke l'ancienne valeur
  prgProgression.StepBy(pas); // On avance le curseur
  rapid := prgProgression.Position - Integer(prgProgression.Tag); // Et on calcule la dérivée pour avoir la vitesse
  if rapid > prgRapidite.Max Then
  Begin
    // Changer la Rapidité Max pour tous les indicateurs
    (ownerForm as TForm1).ChangerRapidite(round(rapid * 1.05));
  end;
  prgRapidite.Position := rapid;
end;

procedure TAvanceur.SetRapiditeMax(newValue : Integer);
Begin
  prgRapidite.Max:= newValue;
end;

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
var i : Integer;
begin
  If False Then
    Begin
      // Pour tester les algos de synchro

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
    end else begin
      AppLog.Lines.Add('avancement...');
      // Pour tester le panneau d'avanceur
      for i := 0 to High(Avanceurs) do
        Avanceurs[i].Avancer(random(10));
      Refresh;
    end;


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

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetLength(Avanceurs, Length(Avanceurs)+1);
  Avanceurs[High(Avanceurs)] := TAvanceur.Create(PanelAvanceurs, 'Coucou '+IntToStr(random(10)), 999);
  AppLog.Lines.Add('Ajout d''un avanceur');
  TimerProgression.Enabled:= true;
end;

procedure TForm1.ButtonAbandonnerClick(Sender: TObject);
var Avanceur : TAvanceur;
begin
  AppLog.Lines.Add('Abandon d''un avanceur');
  Avanceur := (Sender as TButton).Parent as TAvanceur;
  Avanceur.Color:=clRed;
end;

procedure TForm1.ChangerRapidite(newValue : integer);
var i : integer;
begin
  for i := 0 to High(Avanceurs) do
    Avanceurs[i].SetRapiditeMax(newValue);
end;

procedure TForm1.TrouverDisquesTimer(Sender: TObject);
var i : Integer; S : string;
begin
  // Récupération des disques amovibles
  identifierDisquesAmovibles();
  S := '';
  if(listDisks <> nil) Then
    for i := 0 to High(listDisks) do
      S := S + ' ' + listDisks[i];
  if S <> '' Then
    AppLog.Lines.Add('Les disques amovibles sont : ' + S);
end;

end.

