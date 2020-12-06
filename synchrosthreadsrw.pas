unit SynchrosThreadsRW;

{ TODO:
 - Nettoyer un peu le code,
 - Mon algo prod/conso ne fonctionne pas => remplace par pub/subscr
 - des blocs de 1 octet ? de 4 Ko ? ...
 }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

const
  BUFF_BLOCKCOUNT = 2;
  BUFF_BLOCKSIZE  = 1;
  BUFF_LENGTH = BUFF_BLOCKCOUNT * BUFF_BLOCKSIZE;


{ Utilisé pour synchroniser les threads }
type TSemaphore = class
    sem : Pointer;
    sem_id : integer; // interne
    sem_tokens : integer; // debug only
  public
    constructor Create(Tokens : integer = 1);
    destructor Destroy; override;
    procedure Post(Tokens : integer = 1);
    procedure Wait(Tokens : integer = 1);
  end;

type TSyncDataItem = record
    TokensToWrite : TSemaphore; // Le Publisheur indique ainsi que les subscribers peuvent consommer (et combien ils peuvent = nb de jetons)
    TokensWritten : TSemaphore; // Les subscribers indiquent ainsi qu'ils ont consommé les données
  end;

type TDataItem = record
    Buffer : array [1..BUFF_LENGTH] of byte; // Les données lues et à écrire
    BufferLen : integer; // Nombre de données lues/à écrire. Mettre 0 pour indiquer EOF (la fin de fichier). -1 pour indiquer 'pas initialisé'
    BufferId : integer; // Pour le debug
    end;

{ Srtucture globale de synchro du Publisheur et des consommateurs. Doit être créé avant les Publishteurs/consommateurs. 1 publieur, N souscripteurs }
type TPublisherSubscribersSynchronization = class
  private
    //_NbSubscribers : integer; // == Length(syncData)
    _BufferCount : integer;
    protectRegistration : TCriticalSection;

  public
    syncData : array of TSyncDataItem; { Un tableau avec un couple de sémaphores pour chaque subscriber }
    dataItems : array of TDataItem;   { Une FIFO circulaire  }

    constructor Create(BufferCount : integer);
    destructor Destroy; override;

    function RegisterSubscriber : Integer; { Chaque subscriber doit s'enregistrer en appelant cette fonction qui retourne un id unique }

    { Les fonctions suivantes servent à la synchronisation (et à éviter les effets de bord:
       - *Publishtion->le Publishteur, *Subscribption->chaque consommateur.
       - no_item: une variable LOCALE au thread, doit être initialisée par le thread lui-même à 0. Pour index dans dataItems.
       - id_subscr: l'identifiant unique retourné par RegisterSubscriber, sert comme index dans syncData.

       - Begin* -> appeler avant de chaque opération de lecture/écriture de bloc
        ici, chaque consommateur/Publishteur peut utiliser en toute quiétude dataItems[no_item]
       - End* -> appeler à la fin de chaque opération de lecture/écriture de bloc
    }

    procedure BeginPublishtionAnItem(var no_item : integer);
    procedure EndPublishtionAnItem(var no_item : integer);

    procedure BeginSubscribptionAnItem(id_subscr : Integer; var no_item : integer);
    procedure EndSubscribptionAnItem(id_subscr : Integer; var no_item : integer);
  end;


  { Les Publishteurs et subscribers partagent un certain nombre de points communs }
type TAPublisherOrASubscriber = class(TThread)
  protected
    _synchro : TPublisherSubscribersSynchronization;
    _filename : string;

  protected
    // Les procédures et fonctions à instancier
    //OpenfileFunction : procedure(var f: File);
    //ReadorwriteFunction : procedure(f: File; var Buf; buflen : longint; var result : integer);

  public
    Progression : integer; // Variable initialisée à 0 et incrémentée à chaque bloc copié. Charge au MainThread de connaître le maximum pour faire avancer ses progressbar !

    constructor Create(filename : string; synchro : TPublisherSubscribersSynchronization; createSuspendend : boolean = True {publisher bloqué});
  end;


  { Publishteur }
type TPublisher = class(TAPublisherOrASubscriber)
    procedure Execute(); override;
  end;

  { Consommateur }
type TSubscriber  = class(TAPublisherOrASubscriber)
  protected
    subscriber_uid:Integer;
  public
    constructor Create(filename : string; synchro : TPublisherSubscribersSynchronization; createSuspendend : boolean = False {souscripteur autostart});
    procedure Execute(); override;
  end;

implementation

constructor TAPublisherOrASubscriber.Create(filename : string; synchro : TPublisherSubscribersSynchronization; createSuspendend : boolean = True);
begin
  _filename := filename;
  _synchro := synchro;
  Progression := 0;

  FreeOnTerminate := true;
  inherited Create(createSuspendend);
end;

constructor TSubscriber.Create(filename : string; synchro : TPublisherSubscribersSynchronization; createSuspendend : boolean = False);
begin
  inherited Create(filename, synchro, createSuspendend);

  subscriber_uid := synchro.RegisterSubscriber; // S'enregistrer pour la synchro
end;

procedure TPublisher.Execute();
var f : file;
  no, nodebug : integer;
  eof : boolean; // End of file
begin
  no := 0;
  nodebug := 0;

  writeln('Test du Publieur : ', GetThreadID);
  ///sleep(5000);

  // Ouverture du fichier en lecture
  AssignFile(f, _filename);
  Reset(f, BUFF_BLOCKSIZE);

  // et on lit bloc par bloc
  repeat
    _synchro.BeginPublishtionAnItem(no);
    BlockRead(f, _synchro.dataItems[no].Buffer, BUFF_BLOCKCOUNT, _synchro.dataItems[no].BufferLen);
    eof := (_synchro.dataItems[no].BufferLen = 0);
    ///sleep(random(2000));

    _synchro.dataItems[no].BufferId:= nodebug;
    writeln(' Published buffer #', nodebug, ' [', no, '] of size ', _synchro.dataItems[no].BufferLen);
    _synchro.EndPublishtionAnItem(no);

    Inc(Progression);
    Inc(nodebug);
  until eof;

  // Ici, fin du fichier atteinte => le dernier bloc transmis aux consommateurs a l'indication BufferLen = 0 et ils savent donc qu'il faut arrêter.

  // On ferme le fichier et on quitte.
  CloseFile(f);
end;

procedure TSubscriber.Execute();
var f : file;
  no, ResultBlockWrite : integer;
  eof : boolean; // End of file
begin
  no := 0;

  writeln('Test d''un souscripteur : ', GetThreadID);
  //sleep(8000);

  // Ouverture du fichier en écriture
  AssignFile(f, _filename);
  Rewrite(f, BUFF_BLOCKSIZE);

  // et on écrit bloc par bloc
  repeat
    _synchro.BeginSubscribptionAnItem(subscriber_uid, no);
    eof := _synchro.dataItems[no].BufferLen = 0; // A-t-on atteint la fin ?
    ResultBlockWrite := 0;

    ///sleep(random(2000));
    writeln(' Subscribed buffer #', _synchro.dataItems[no].BufferId, ' [', no, '] of size ', _synchro.dataItems[no].BufferLen);

    if not eof then
      BlockWrite(f, _synchro.dataItems[no].Buffer, _synchro.dataItems[no].BufferLen, ResultBlockWrite);
    _synchro.EndSubscribptionAnItem(subscriber_uid, no);
    //doCallbackNow(no);
    Inc(Progression);
  until eof;

  // On ferme le fichier et on quitte.
  CloseFile(f);
end;

constructor TPublisherSubscribersSynchronization.Create(BufferCount : integer);
var i : integer;
begin
  writeln('TPublisherSubscribersSynchronization.Create(BufferCount = ', BufferCount);

  // Initialiser syncData
  SetLength(dataItems, BufferCount);
  for i := 0 to BufferCount - 1 do
    With dataItems[i] do
    Begin
      BufferLen := -1;
      BufferId  := -1;
    End;

  protectRegistration := TCriticalSection.Create;
  syncData := nil;

  // Et garder en mémoire les infos utiles
  _BufferCount := BufferCount;
end;

function TPublisherSubscribersSynchronization.RegisterSubscriber : Integer; { Chaque subscriber doit s'enregistrer en appelant cette fonction qui retourne un id unique }
Begin
  protectRegistration.Acquire;
  Result := Length(syncData);
  SetLength(syncData, Result+1);
  With syncData[Result] do
  begin
    TokensToWrite := TSemaphore.Create(0);
    TokensWritten := TSemaphore.Create(1);
  end;
  protectRegistration.Release;
end;


destructor TPublisherSubscribersSynchronization.Destroy();
var i : integer;
begin
  // Libération des ressources allouées
  for i := High(syncData) downto 0 do
    With syncData[i] do
    Begin
      FreeAndNil(TokensToWrite);
      FreeAndNil(TokensWritten);
    end;
  SetLength(syncData, 0);

  SetLength(dataItems, 0);
  inherited ;
end;

procedure TPublisherSubscribersSynchronization.BeginPublishtionAnItem(var no_item : integer);
var id_subscr : integer;
begin
  writeln('TPublisherSubscribersSynchronization.BeginPublishtionAnItem(', no_item, ')');

  // Pour chaque souscripteur, P(1)
  for id_subscr := 0 to High(syncData) do
    syncData[id_subscr].TokensWritten.Wait;
end;

procedure TPublisherSubscribersSynchronization.EndPublishtionAnItem(var no_item : integer);
var id_subscr : integer;
begin
  writeln('TPublisherSubscribersSynchronization.EndPublishtionAnItem(', no_item, ')');

  //  Pour chaque souscripteur, V(1)
  for id_subscr := 0 to High(syncData) do
    syncData[id_subscr].TokensToWrite.Post;

  // Et préparer pour le bloc suivant
  no_item := (no_item + 1) mod _BufferCount;
end;

procedure TPublisherSubscribersSynchronization.BeginSubscribptionAnItem(id_subscr : Integer; var no_item : integer);
begin
  writeln('TPublisherSubscribersSynchronization.BeginSubscribptionAnItem(', no_item, ')');

  // P(1)
  syncData[id_subscr].TokensToWrite.Wait;
end;

procedure TPublisherSubscribersSynchronization.EndSubscribptionAnItem(id_subscr : Integer; var no_item : integer);
begin
  writeln('TPublisherSubscribersSynchronization.EndSubscribptionAnItem(', no_item, ')');

  // V(1)
  syncData[id_subscr].TokensWritten.Post;

  // Et préparer pour le bloc suivant
  no_item := (no_item + 1) mod _BufferCount;
end;



constructor TSemaphore.Create(Tokens : integer = 1);
begin
  sem := SemaphoreInit;
  if( sem = Pointer(-1) ) then
    raise EThreadExternalException.Create('Unable to init semaphore');

  sem_id := (QWord(sem) >> 4) mod 1024;
  writeln(' Thread ', GetCurrentThreadId, ' created sem ', QWord(sem), ' = ', sem_id, ' with ', Tokens, ' tokens');

  sem_tokens := 0;
  if(Tokens > 0) then
    Post(Tokens);
end;

destructor TSemaphore.Destroy;
begin
  SemaphoreDestroy(sem);
  inherited;
end;

procedure TSemaphore.Post(Tokens : integer = 1);
var i  : integer;
begin
  //writeln(' Thread ', GetCurrentThreadId, ' sem_id ', sem_id, ' posting ', Tokens, 'tokens : old val = ', sem_tokens, ' tokens');
  for i := 1 to Tokens do
    SemaphorePost(sem);
  sem_tokens := sem_tokens + Tokens;
  //writeln(' Thread ', GetCurrentThreadId, ' sem_id ', sem_id, ' posted tokens : new val = ', sem_tokens, ' tokens');
end;

procedure TSemaphore.Wait(Tokens : integer = 1);
var i  : integer;
begin
  //writeln(' Thread ', GetCurrentThreadId, ' sem_id ', sem_id, ' waiting ', Tokens, 'tokens : old val = ', sem_tokens, ' tokens');
  for i := 1 to Tokens do
    SemaphoreWait(sem);
  sem_tokens := sem_tokens - Tokens;
  //writeln(' Thread ', GetCurrentThreadId, ' sem_id ', sem_id, ' waited tokens : new val = ', sem_tokens, ' tokens');
end;


end.

