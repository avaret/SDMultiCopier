unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, CheckLst, Buttons, RichMemo, SynEdit, Process,
  SynchrosThreadsRW, detectRemovDisk;


const
  ProgressBarAvancement_Diviser = 1 << 10; // La progressbar ne sera pas en octets mais en Kio

type

  { TAvanceur : un panneau affichant l'état pour une carte mémoire + le thread correpondant}

  TAvanceur = class(TPanel)
    lblCible : TLabel;
    prgProgression, prgRapidite : TProgressBar;
    btnAbandonner : TBitBtn;
    ownerForm : TForm;

    procedure BtnAbandonnerClick(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    constructor Create(TheOwnerPanel: TPanel; TheOwnerForm: TForm; cCaption : String; avcMax : Int64; thrDup : TAPublisherOrASubscriber);
  public
    thrDuplicateur : TAPublisherOrASubscriber;

    procedure Avancer(newpos : Integer); // Pour faire avancer les barres de progression...
    procedure SetRapiditeMax(newValue : Integer);

    procedure Abort; // Pour arrêter la copie
  end;


type
  TModeAppli = (
    maUnitialized,
    maDisk2Disks,
    maDisk2Img,
    maImg2Disk );


type

  { TFormSDMultiCopier }

  TFormSDMultiCopier = class(TForm)
    BitBtnArreterLaCopie: TBitBtn;
    BitBtnLancerLaCopie: TBitBtn;
    CheckBoxDbgCopy: TCheckBox;
    DiskTargets: TCheckListBox;
    ImageAppliIcon: TImage;
    ImageListProgression: TImageList;
    ImgLoadFile: TBitBtn;
    ImgSaveFile: TBitBtn;
    DiskSource: TComboBox;
    ImgFilename: TEdit;
    GroupBoxDiskSrc: TGroupBox;
    GroupBoxdisks: TGroupBox;
    GroupBoxImage: TGroupBox;
    GroupBoxMode: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    MemoImgNotes: TMemo;
    OpenDialog1: TOpenDialog;
    PageControlMainForm: TPageControl;
    PanelAvanceurs: TPanel;
    ProcessEject: TProcess;
    ProcessSync: TProcess;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    TabSheetconf: TTabSheet;
    TabSheetCopying: TTabSheet;
    AppLog: TRichMemo;
    TimerSauverNotesPerso: TTimer;
    TimerProgression: TTimer;
    TimerTrouverDisques: TTimer;
    procedure BitBtnArreterLaCopieClick(Sender: TObject);
    procedure BitBtnLancerLaCopieClick(Sender: TObject);
    procedure DiskSourceSelect(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure ImgLoadFileClick(Sender: TObject);
    procedure ImgSaveFileClick(Sender: TObject);
    procedure MemoImgNotesChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TimerProgressionTimer(Sender: TObject);
    procedure ChangerRapidite(newValue : integer);
    procedure TimerSauverNotesPersoTimer(Sender: TObject);
    procedure TimerTrouverDisquesTimer(Sender: TObject);
  private
    FModeAppli : TModeAppli;
    FCopyInProgress : boolean;
    procedure SetCopyInProgress(NewValue : boolean); // Met à jour l'ihm en conséquence
    procedure SetModeAppli(NewValue : TModeAppli); // Met à jour l'ihm en conséquence

  public
    sync : TPublisherSubscribersSynchronization;

    Avanceurs : array of TAvanceur;

    HeureDebutCopie : TDateTime; // Pour estimer l'avancement

    procedure AddAppLog(Msg : String);
    property CopyInProgress : boolean read FCopyInProgress write SetCopyInProgress;
    property Modeappli: TModeAppli read FModeAppli write SetModeAppli;
  end;

var
  FormSDMultiCopier: TFormSDMultiCopier;

implementation


{$R *.lfm}

{ TAvanceur }

constructor TAvanceur.Create(TheOwner: TComponent);
begin
  TheOwner := TheOwner;
  raise EInvalidOp.Create('Il ne faut pas appeler ce constructeur mais le suivant.');
end;

constructor TAvanceur.Create(TheOwnerPanel: TPanel; TheOwnerForm: TForm; cCaption : String; avcMax : Int64; thrDup : TAPublisherOrASubscriber);
const progressBarWidth = 300;
begin
  inherited Create(TheOwnerPanel);
  Parent := TheOwnerPanel;
  ownerForm := TheOwnerForm;

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

  prgRapidite := TProgressBar.Create(Self);
  prgRapidite.Parent := Self;
  prgRapidite.Width:= progressBarWidth;
  prgRapidite.Max:= 50;
  prgRapidite.BarShowText:=true;
  prgRapidite.Smooth:=true;
  prgRapidite.Align:=alLeft;
  prgRapidite.BorderSpacing.Around:=8;
  prgRapidite.Color:=clGreen;

  prgProgression := TProgressBar.Create(Self);
  prgProgression.Parent := Self;
  prgProgression.Width:= progressBarWidth;
  prgProgression.Max:= avcMax div ProgressBarAvancement_Diviser;
  prgProgression.BarShowText:=true;
  prgProgression.Smooth:=true;
  prgProgression.Align:=alLeft;
  prgProgression.BorderSpacing.Around:=8;


  btnAbandonner := TBitBtn.Create(Self);
  btnAbandonner.Parent := Self;
  btnAbandonner.OnClick := @BtnAbandonnerClick;
  btnAbandonner.Align:=alLeft;
  btnAbandonner.Kind:=bkCancel;
  btnAbandonner.Caption:='Stop';

  thrDuplicateur := thrDup;
  // thrDup est faiblement couplé à TAvanceur : celui qui passe cette variable est aussi responsable de sa gestion...
end;

procedure TAvanceur.Avancer(newpos : Integer); // Pour faire avancer les barres de progression...
var rapid, oldProgression : Integer;
begin
  oldProgression := prgProgression.Position; // On stocke l'ancienne valeur
  prgProgression.Position := newpos; // On avance le curseur
  rapid := newpos - oldProgression; // Et on calcule la dérivée pour avoir la vitesse
  rapid := rapid * 2; // le TimerProgression appelle "Avancer" deux fois par seconde.
  if rapid > prgRapidite.Max Then
  Begin
    // Changer la Rapidité Max pour *tous* les indicateurs d'avancement
    (ownerForm as TFormSDMultiCopier).ChangerRapidite(round(rapid * 1.05));
  end;
  prgRapidite.Position := rapid;
end;

procedure TAvanceur.SetRapiditeMax(newValue : Integer);
Begin
  prgRapidite.Max:= newValue;
end;

procedure TAvanceur.BtnAbandonnerClick(Sender: TObject);
begin
  Abort;
end;


procedure TAvanceur.Abort;
begin
  Color:=clRed;
  Font.StrikeThrough := True;
  prgRapidite.Visible := False;
  thrDuplicateur.Terminate;
  (ownerForm as TFormSDMultiCopier).AddAppLog('Arrêt demandé de l''entité ' + lblCible.Caption);
end;

{ TFormSDMultiCopier }

procedure StartThenWaitforProcessTermination(P : TProcess);
begin
  P.Active:=True;
  while P.Running do
    begin
      Application.ProcessMessages;
      sleep(250);
    end;
end;

procedure TFormSDMultiCopier.TimerProgressionTimer(Sender: TObject);
var i, avc, avcmax, avc_per20 : Integer;
  // avc = 250 [octets], avc max = 1000 octets, avc_per20 = 5 (car 5/20 = 25% = 250/1000
  unAvanceurEstVivant : boolean = false;
  DureeTotaleEstimee : TDateTime;//longint; // ms [longint = TDateTimeStamp.time]
  progressionStr : String;
begin
  for i := 0 to High(Avanceurs) do
  begin
    // On fait toujours avancer la barre, même si c'est fini, pour indiquer la fin.
    Avanceurs[i].Avancer(Avanceurs[i].thrDuplicateur.Progression * BUFF_LENGTH div ProgressBarAvancement_Diviser);
    if not Avanceurs[i].thrDuplicateur.Finished Then
      unAvanceurEstVivant := true;
  end;

  // La 1re fois que l'on détecte la fin de la duplication...
  If not unAvanceurEstVivant and (TimerProgression.Tag = 0) Then
  begin
    AddAppLog('Finalisation...');

    // On "désactive" cette section du timer pour ces opérations qui peuvent prendre du temps...
    TimerProgression.Tag := 1;

    // sync
    AddAppLog('Attente que les disques aient réellement écrits leurs données (vidange des caches)...');
    StartThenWaitforProcessTermination(ProcessSync);

    // éjecter systématiquement tous les disques cibles
    if Modeappli <> maDisk2Img then
    begin
      AddAppLog('Éjection des disques cibles...');
      for i := 0 to DiskTargets.Items.Count - 1 do
        if DiskTargets.Checked[i] Then
        Begin
          ProcessEject.Parameters.Text := DiskTargets.Items[i]; // Un seul paramètre!
          StartThenWaitforProcessTermination(ProcessEject);
        End;
      AddAppLog('Éjection des disques terminés. Opération de duplication terminée.');
    end;

    // La copie est terminée ou elle a été abandonnée
    // On remet l'affichage à l'état avant la copie+désactive le CopyInProgress
    SetModeAppli(Modeappli);
    TabSheetCopying.Caption := 'pas de copie en cours...';
    AddAppLog('Fin de la copie.');

    // On réactive cette section du timer
    TimerProgression.Tag := 0;
  end else begin
    // Faire tourner une icône en forme de CD pour indiquer l'avancement

    // on calcule la progression (%)
    avcmax := Avanceurs[0].prgProgression.Max;
    avc := avcmax;
    for i := 0 to High(Avanceurs) do
      if Avanceurs[i].prgProgression.Position < avc Then
        avc := Avanceurs[i].prgProgression.Position;
    avc_per20 := round((avc / avcmax) * 20);

    progressionStr := ' (' +  round((avc / avcmax) * 100).ToString + '%)';
    TabSheetCopying.Caption := 'Copie en cours... ' + progressionStr;

    // Permet d'avoir une progression continue = "sans fin"
    ImageListProgression.Tag:= (ImageListProgression.Tag + 1) mod 21;

    // A chaque tour, on calcule l'heure estimée de fin
    if ImageListProgression.Tag mod 21 = 0 Then
    Begin
      DureeTotaleEstimee := TimeStampToDateTime( MSecsToTimeStamp( round((
        (DateTimeToTimeStamp(Now).Time - DateTimeToTimeStamp(HeureDebutCopie).Time)
        /
        (avc / avcmax))) ) );
      AddAppLog(' Fin estimée à : '+TimeToStr(DureeTotaleEstimee - HeureDebutCopie)
        + ', soit dans '+TimeToStr(Time + DureeTotaleEstimee - HeureDebutCopie)
        + progressionStr);
    end;

    // Icône de l'application (barre des tâches)+de la fenêtre : progression (%)
    ImageListProgression.GetBitmap(avc_per20, ImageAppliIcon.Picture.Bitmap);
    Application.Icon.Assign(ImageAppliIcon.Picture.Graphic);

    // Icône de l'onglet : progression continue (à faire APRES l'Appli.Icon)
    TabSheetCopying.ImageIndex:= ImageListProgression.Tag;
  end;

end;


procedure TFormSDMultiCopier.DiskSourceSelect(Sender: TObject);
var idx : integer;
begin
  // Par défaut tous les disques trouvés sont des cibles potentielles
  for idx := 0 to DiskTargets.Items.Count -1 do
    DiskTargets.Checked[idx] := True;

  // Mais un disque sélectionné comme source ne peut être aussi la cible
  idx := DiskTargets.Items.IndexOf(DiskSource.Text);
  if idx >= 0 then
    DiskTargets.Checked[idx] := False;
end;

procedure TFormSDMultiCopier.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if CopyInProgress then
    BitBtnArreterLaCopieClick(Sender);

  CanClose := True;
end;


function IsRoot : boolean;
Begin
  Result := GetUserDir = '/root/';
end;

procedure TFormSDMultiCopier.FormCreate(Sender: TObject);
var Year, Month, Day : word;
begin
  CheckBoxDbgCopy.Visible:= CheckBoxDbgCopy.Checked;
  if (not IsRoot) and (not CheckBoxDbgCopy.Checked) then
    ShowMessage('Attention, cette application *DOIT* être lancée en super-utilisateur (root) '+
    'pour pouvoir copier les disques.');

  Modeappli:= maUnitialized;
  OpenDialog1.InitialDir:= GetUserDir() + 'Images';
  SaveDialog1.InitialDir:= OpenDialog1.InitialDir;
  If not DirectoryExists(OpenDialog1.InitialDir) Then
    mkdir(OpenDialog1.InitialDir);

  DecodeDate(Date, Year, Month, Day);
  ImgFilename.Text:= Format('%s/%d-%.2d-%.2d mon image.img',
    [OpenDialog1.InitialDir, Year, Month, Day ]);

  Avanceurs := nil;
  AppLog.Clear;
end;


procedure TFormSDMultiCopier.SetModeAppli(NewValue : TModeAppli);
begin
  FModeAppli:= NewValue;
  case NewValue of
  maUnitialized: begin
        GroupBoxDiskSrc.Visible:= False;
        GroupBoxImage.Visible:= False;
        GroupBoxdisks.Visible:= False;
    end;
  maDisk2Disks: begin
        GroupBoxDiskSrc.Visible:= True;
        GroupBoxImage.Visible:= False;
        GroupBoxdisks.Visible:= True;
    end;
  maDisk2Img: begin
        GroupBoxDiskSrc.Visible:= True;
        GroupBoxImage.Visible:= True;
        ImgSaveFile.Visible:= True;
        ImgLoadFile.Visible:= False;
        GroupBoxdisks.Visible:= False;
    end;
  maImg2Disk: begin
        GroupBoxDiskSrc.Visible:= False;
        GroupBoxImage.Visible:= True;
        ImgSaveFile.Visible:= False;
        ImgLoadFile.Visible:= True;
        GroupBoxdisks.Visible:= True;
    end;

  end;

  BitBtnLancerLaCopie.Visible := NewValue <> maUnitialized;

  // Met à jour aussi les composants suivant la var Copyinprogress
  CopyInProgress:=False;
end;


procedure TFormSDMultiCopier.BitBtnLancerLaCopieClick(Sender: TObject);
var Source, Cibles, PrefixDbg : String;
  i, idx, NbCibles : integer;
  SourceLength : Int64;
begin
  TimerProgression.Enabled := false;
  AddAppLog('Vérifications...');
  try
    // debug: utiliser /tmp/dev/sdX au lieu de /dev/sdX
    if CheckBoxDbgCopy.Checked Then
      PrefixDbg := '/tmp'
    else
      PrefixDbg := '';


    // Vérification de ce que l'utilisateur demande
    if Modeappli = maUnitialized then
      raise EInvalidOperation.Create('Impossible de lancer une copie sans avoir sélectionné un mode.');

    if Modeappli = maImg2Disk then
    begin
      // Source = une image locale
      Source := ImgFilename.Caption;
    end else begin
      // Source = un disque
      Source := DiskSource.Caption;
    end;

    If not FileExists(Source) Then
      raise EFileNotFoundException.Create('Le disque source <'+DiskSource.Caption+'> n''existe pas.');

    if Modeappli = maDisk2Img then
    begin
      // Cible = une image locale
      NbCibles := 1;
      Cibles := ImgFilename.Caption;
      if(FileExists(Cibles)) Then
      Begin
        if MessageDlg('Destruction du fichier cible',
          'Le fichier indiqué pour recevoir l''image existe déjà et sera donc écrasé intégralement.'+
          'Êtes-vous sûr de vouloir continuer et écraser le fichier '+Cibles+' ?',
          mtConfirmation, mbYesNoCancel, 0) = mrCancel Then
          Exit;
      end;

    end else begin
      // Cible = un/des disques
      NbCibles := 0;

      for idx := 0 to DiskTargets.Items.Count - 1 do
        if DiskTargets.Checked[idx] Then
        begin
          Inc(NbCibles);

          if not listDisks_linear.Contains(DiskTargets.Items[idx]) Then
            raise EFileNotFoundException.Create('Le disque '+DiskTargets.Items[idx]+' n''existe pas/plus...');

          if DiskTargets.Items[idx] = Source Then
            raise EInvalidOperation.Create('Impossible de sélectionner un disque comme Source ET Cible.');
        end;

      if NbCibles = 0 Then
        raise EInvalidOperation.Create('Il est nécessaire de cocher au moins un disque cible.');

    end;

    // Démarrage de la copie
    AddAppLog('Vérifications terminées, démarrage de la copie...');

    // S'il y a déjà eu une copie, on nettoie...
    if Avanceurs <> nil then
      for i := High(Avanceurs) downto 0 do
        Avanceurs[i].Free;

    // Il y aura 1 source + N cibles
    SetLength(Avanceurs, 1 + NbCibles);
    AddAppLog(IntToStr(Length(Avanceurs)) + ' entitées de copie');

    // Extraction de la taille de la source = qtt d'octets à copier
    SourceLength := getFileSize(PrefixDbg + Source);
    if( SourceLength = 0) then
      SourceLength := getDiskSize(PrefixDbg + Source);
    if( SourceLength = 0) then
      raise EInvalidOperation.Create('Impossible d''extraire la taille du fichier source : '+PrefixDbg + Source);
    AddAppLog('La source fait '+IntToStr(SourceLength) + ' (' + IntToStr(SourceLength>>30) + ' Gio) octets à dupliquer');


    // tout sera synchronisé par l'objet sync
    sync := TPublisherSubscribersSynchronization.Create();

    // La source, qu'elle soit un fichier ou un disque:
    Avanceurs[0] := TAvanceur.Create(
      PanelAvanceurs,
      Self,
      'Source: ' + PrefixDbg + Source,
      SourceLength,
      TPublisher.Create(PrefixDbg + Source, sync)
      );

    // Les cibles:
    if Modeappli = maDisk2Img Then
    begin
      // La cible est un fichier image
      Avanceurs[1] := TAvanceur.Create(
          PanelAvanceurs,
          Self,
          'Cible: ' + ImgFilename.Caption,
          SourceLength,
          TSubscriber.Create(ImgFilename.Caption, sync)
        );
      AddAppLog(Source + ' --> ' + ImgFilename.Caption);
    end else begin
      // Les cibles sont des disques
      idx := 0;
      Cibles := '';
      for  i := 0 to DiskTargets.Items.Count - 1 do
      if DiskTargets.Checked[i] Then
      Begin
        Inc(idx);
        Avanceurs[idx] := TAvanceur.Create(
          PanelAvanceurs,
          Self,
          'Cible: ' + PrefixDbg + DiskTargets.Items[i],
          SourceLength,
          TSubscriber.Create(PrefixDbg + DiskTargets.Items[i], sync)
        );
        Cibles := Cibles + PrefixDbg + DiskTargets.Items[i] + ' ';
      End;

      AddAppLog(Source + ' --> { ' + Cibles + '}');
    end;

    // Lancement du processus
    CopyInProgress:= TRUE; // Met à jour l'IHM
    Avanceurs[0].thrDuplicateur.Start;

    AddAppLog('Début de la copie. Les barres de progression sont en kibioctets et kio/s.');

  except
    // En cas d'erreur, on tente de restaurer un état "configuration"
    On E: Exception Do
    Begin
      TimerProgression.Enabled := false;
      TimerTrouverDisques.Enabled := Modeappli <> maUnitialized;
      Avanceurs := nil;
      AddAppLog('échec du démarrage de la copie : '+E.Message);
      ShowMessage(E.Message);
    End;
  end;
end;

procedure TFormSDMultiCopier.BitBtnArreterLaCopieClick(Sender: TObject);
var i : integer;
begin
  for i := 0 to High(Avanceurs) do
    Avanceurs[i].Abort;
  TabSheetCopying.Caption := 'Copie annulée !';
end;

procedure TFormSDMultiCopier.SetCopyInProgress(NewValue: boolean);
begin
  FCopyInProgress:=NewValue;

  TabSheetCopying.Visible:= CopyInProgress;
  GroupBoxMode.Enabled:= not CopyInProgress;
  GroupBoxDiskSrc.Enabled:= not CopyInProgress;
  GroupBoxImage.Enabled:= not CopyInProgress;
  GroupBoxdisks.Enabled:= not CopyInProgress;
  BitBtnLancerLaCopie.Enabled:=not CopyInProgress;
  BitBtnArreterLaCopie.Enabled:= CopyInProgress;

  If(CopyInProgress) Then
  begin
    PageControlMainForm.ActivePage := TabSheetCopying;
    TabSheetCopying.Caption := 'Copie en cours...';
    TabSheetCopying.TabVisible:=True; // On le rend visible à la première copie
    HeureDebutCopie := Now;
  end
  else
  begin
    PageControlMainForm.ActivePage := TabSheetconf;
  end;

  Self.Caption := 'SD Multi-Copieur (' + PageControlMainForm.ActivePage.Caption + ')';

  TimerProgression.Enabled := CopyInProgress;
  TimerTrouverDisques.Enabled:= (not CopyInProgress) and (FModeAppli <> maUnitialized);
end;

procedure TFormSDMultiCopier.ImgLoadFileClick(Sender: TObject);
begin
  If OpenDialog1.Execute Then
  Begin
    ImgFilename.Text:=OpenDialog1.FileName;
    If(FileExists(ImgFilename.Text + '.txt')) Then
      MemoImgNotes.Lines.LoadFromFile(ImgFilename.Text + '.txt');
  end;
end;

procedure TFormSDMultiCopier.ImgSaveFileClick(Sender: TObject);
begin
  If SaveDialog1.Execute Then
  begin
    ImgFilename.Text:=SaveDialog1.FileName;
    If(FileExists(ImgFilename.Text + '.txt')) Then
      MemoImgNotes.Lines.LoadFromFile(ImgFilename.Text + '.txt');
  end;
end;

procedure TFormSDMultiCopier.MemoImgNotesChange(Sender: TObject);
begin
  TimerSauverNotesPerso.Enabled:=False; //Init le timer| réinitialiser le délai
  TimerSauverNotesPerso.Enabled:=True;
end;

procedure TFormSDMultiCopier.SpeedButton1Click(Sender: TObject);
var SendBtn : TSpeedButton;
begin
  SendBtn := (Sender as TSpeedButton);
  AddAppLog(SendBtn.Caption);
  SendBtn.Down:= True;
  Case SendBtn.Tag of
    1: Modeappli:= maDisk2Disks;
    2: Modeappli:= maDisk2Img;
    3: Modeappli:= maImg2Disk;
  end;

end;

procedure TFormSDMultiCopier.ChangerRapidite(newValue : integer);
var i : integer;
begin
  for i := 0 to High(Avanceurs) do
    Avanceurs[i].SetRapiditeMax(newValue);
end;

procedure TFormSDMultiCopier.TimerSauverNotesPersoTimer(Sender: TObject);
begin
  // Le timer ne se redéclenchera pas jusqu'à la prochaine modif
  TimerSauverNotesPerso.Enabled:=False;

  try
    MemoImgNotes.Lines.SaveToFile(ImgFilename.Text + '.txt');
  except
    On E : Exception do
      ShowMessage('Impossible d''enregistrer les notes personnelles.'#13#10 + E.Message);
  end;
end;

procedure TFormSDMultiCopier.TimerTrouverDisquesTimer(Sender: TObject);
var i : Integer;
begin
  // Récupération des disques amovibles
  identifierDisquesAmovibles();

  // ajout des disques (si nécessaire) aux 2 listes
  if(listDisks <> nil) Then
    for i := 0 to High(listDisks) do // Ajout des disques branchés
    begin
      if DiskSource.Items.IndexOf(listDisks[i]) < 0 then
        DiskSource.Items.Add(listDisks[i]);;
      if DiskTargets.Items.IndexOf(listDisks[i]) < 0 then
      begin
        DiskTargets.Items.Add(listDisks[i]);;
        DiskTargets.Checked[DiskTargets.Items.Count-1] := true;
      end;
    end;

   // Suppression des disques débranchés
  for i := DiskSource.Items.Count-1 downto 0 do
    if not listDisks_linear.Contains(DiskSource.Items[i]) Then
      DiskSource.Items.Delete(i);
  for i := DiskTargets.Items.Count-1 downto 0 do
    if not listDisks_linear.Contains(DiskTargets.Items[i]) Then
      DiskTargets.Items.Delete(i);

end;


procedure TFormSDMultiCopier.AddAppLog(Msg : String);
var //A : Integer;
  Sab, Sbc : String;
Begin
  // Ajouter le texte
  //A := AppLog.GetTextLen;
  Sab := '[' + TimeToStr(Time) + '] ';
  //B := A + Sab.Length;
  Sbc := Msg;
  AppLog.Append(Sab + Sbc);

  // Colorer
  //AppLog.SetRangeColor(A, Sab.Length, clBlue);  //TODO à débuguer... ou passer sur du html !
  //AppLog.SetRangeColor(B, Msg.Length, clBlack);

  // Mettre le curseur à la fin
  AppLog.SelStart := AppLog.GetTextLen;
end;

{ /usr/share/icons/elementary/animations/48/  -> Avancement global (5%/5%, Brasero)

cd /usr/share/icons/elementary/animations/48/
for f in *.svg ; do
  inkscape $f -e ~/SuperSDCopier/SDMultiCopier/ImagesProgression/$   f/svg/png
done
cd -
}

end.

