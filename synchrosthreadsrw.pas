unit SynchrosThreadsRW;

{ TODO:
 - Nettoyer un peu le code,
 - Mon algo prod/conso ne fonctionne pas => débugger
 - des blocs de 1 octet ? de 4 Ko ? ...
 }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

const
  BUFF_BLOCKCOUNT = 20;
  BUFF_BLOCKSIZE  = 1;
  BUFF_LENGTH = BUFF_BLOCKCOUNT * BUFF_BLOCKSIZE;


{ Utilisé pour synchroniser les threads }
type TSemaphore = class
    sem : Pointer;
    sem_id : integer; // interne
  public
    constructor Create(Tokens : integer = 1);
    destructor Destroy; override;
    procedure Post(Tokens : integer = 1);
    procedure Wait(Tokens : integer = 1);
  end;

type TSyncDataItem = record
    Buffer : array [1..BUFF_LENGTH] of byte; // Les données lues et à écrire
    BufferLen : integer; // Nombre de données lues/à écrire. Mettre 0 pour indiquer EOF (la fin de fichier). -1 pour indiquer 'pas initialisé'
    TokensToWrite : TSemaphore; // Le producteur indique ainsi que les consommateurs peuvent consommer
    TokensWritten : TSemaphore; // Les consommateurs indiquent ainsi qu'ils ont consommés les données
  end;

{ Srtucture globale de synchro du producteur et des consommateurs. Doit être créé avant les producteurs/consommateurs. }
type TProducerConsumersSynchronization = class
  private
    _NbConsumers : integer;
  public
    syncData : array of TSyncDataItem; { Une FIFO circulaire  }

    constructor Create(NbConsumers, BufferCount : integer);
    destructor Destroy; override;

    { Les fonctions :
       - *Production->le producteur, *Consumption->chaque consommateur.
       - no: une variable LOCALE au thread, doit être initialisée par le thread lui-même à 0.
       - Begin* -> appeler avant de chaque opération de lecture/écriture de bloc
        ici, chaque consommateur/producteur peut utiliser en toute quiétude syncData[no]
       - End* -> appeler à la fin de chaque opération de lecture/écriture de bloc
    }

    procedure BeginProductionAnItem(var no : integer);
    procedure EndProductionAnItem(var no : integer);

    procedure BeginConsumptionAnItem(var no : integer);
    procedure EndConsumptionAnItem(var no : integer);
  end;

  { La fonction de notification pour faire bouger les progressbar
type
  TPCCallBack = procedure (Sender: TObject; AData : Pointer) of object;  }


  { Les producteurs et consommateurs partagent un certain nombre de points communs }
type TAProducerOrAConsumer = class(TThread)
  protected
    _synchro : TProducerConsumersSynchronization;
//    _callback: TPCCallBack;
//    _callbackSender : TObject;
    _filename : string;
//    procedure doCallbackNow(n : integer); // Le callback n: un paramètre éventuellement transmis...

  protected
    // Les procédures et fonctions à instancier
    //OpenfileFunction : procedure(var f: File);
    //ReadorwriteFunction : procedure(f: File; var Buf; buflen : longint; var result : integer);

  public
    Progression : integer; // Variable initialisée à 0 et incrémentée à chaque bloc copié. Charge au MainThread de connaître le maximum pour faire avancer ses progressbar !

    constructor Create(filename : string; synchro : TProducerConsumersSynchronization{; callback: TPCCallBack; callbackSender : TObject});

    // synchro = L'objet commun de synchronisation, un objet de type TProducerConsumersSynchronization
    // callback = une fonction appelée à chaque tour de la boucle
    // callbackSender = le paramètre constant ASender pour la fonction callback

    // fonction effectant l'action globale de production, ou de consommation
    //procedure Execute(); override;
  end;


  { Producteur }
type TProducer = class(TAProducerOrAConsumer)
    procedure Execute(); override;
  end;

  { Consommateur }
type TConsumer  = class(TAProducerOrAConsumer)
    procedure Execute(); override;
  end;

implementation

constructor TAProducerOrAConsumer.Create(filename : string; synchro : TProducerConsumersSynchronization{; callback: TPCCallBack; callbackSender : TObject});
begin
  _filename := filename;
  _synchro := synchro;
//  _callback := callback;
//  _callbackSender := callbackSender;
  Progression := 0;

  FreeOnTerminate := true;
  inherited Create(False); // Create Not Suspended
end;

{procedure TAProducerOrAConsumer.doCallbackNow(n : integer);
begin
  _callback(_callbackSender, Pointer(n));
end;
}

procedure TProducer.Execute();
var f : file;
  no : integer;
  eof : boolean; // End of file
begin
  no := 0;

  writeln('Test du producteur : ', GetThreadID);
  sleep(5000);

  // Ouverture du fichier en lecture
  AssignFile(f, _filename);
  Reset(f, BUFF_BLOCKSIZE);

  // et on lit bloc par bloc
  repeat
    _synchro.BeginProductionAnItem(no);
    BlockRead(f, _synchro.syncData[no].Buffer, BUFF_BLOCKCOUNT, _synchro.syncData[no].BufferLen);
    _synchro.EndProductionAnItem(no);
    //doCallbackNow(no);
    eof := (_synchro.syncData[no].BufferLen = 0);
    Inc(Progression);
  until eof;

  // Ici, fin du fichier atteinte => le dernier bloc transmis aux consommateurs a l'indication BufferLen = 0 et ils savent donc qu'il faut arrêter.

  // On ferme le fichier et on quitte.
  CloseFile(f);
end;

procedure TConsumer.Execute();
var f : file;
  no, ResultBlockWrite : integer;
  eof : boolean; // End of file
begin
  no := 0;

  writeln('Test d''un consommateur : ', GetThreadID);
  sleep(8000);

  // Ouverture du fichier en écriture
  AssignFile(f, _filename);
  Rewrite(f, BUFF_BLOCKSIZE);

  // et on écrit bloc par bloc
  repeat
    _synchro.BeginConsumptionAnItem(no);
    eof := _synchro.syncData[no].BufferLen = 0; // A-t-on atteint la fin ?
    ResultBlockWrite := 0;

    if not eof then
      BlockWrite(f, _synchro.syncData[no].Buffer, _synchro.syncData[no].BufferLen, ResultBlockWrite);
    _synchro.EndConsumptionAnItem(no);
    //doCallbackNow(no);
    Inc(Progression);
  until eof;

  // On ferme le fichier et on quitte.
  CloseFile(f);
end;

constructor TProducerConsumersSynchronization.Create(NbConsumers, BufferCount : integer);
var i : integer;
begin
  // Initialiser syncData
  SetLength(syncData, BufferCount);
  for i := 0 to BufferCount - 1 do
    With syncData[i] do
    Begin
      BufferLen := -1;
      TokensToWrite := TSemaphore.Create(0);
      TokensWritten := TSemaphore.Create(NbConsumers);
    End;

  // Et garder en mémoire les infos utiles
  _NbConsumers := NbConsumers;
end;

destructor TProducerConsumersSynchronization.Destroy();
var i : integer;
begin
  for i := High(syncData) downto 0 do
    With syncData[i] do
    Begin
      FreeAndNil(TokensToWrite);
      FreeAndNil(TokensWritten);
    end;
  SetLength(syncData, 0);

  inherited ;
end;

procedure TProducerConsumersSynchronization.BeginProductionAnItem(var no : integer);
begin
  // P(Jetons écrits, N)
  syncData[no].TokensWritten.Wait(_NbConsumers);
end;

procedure TProducerConsumersSynchronization.EndProductionAnItem(var no : integer);
begin
  // V(Jetons à écrite, N)
  syncData[no].TokensToWrite.Post(_NbConsumers);

  // Et préparer pour le bloc suivant
  no := (no + 1) mod _NbConsumers;
end;

procedure TProducerConsumersSynchronization.BeginConsumptionAnItem(var no : integer);
begin
  // P(Jetons à écrire)
  syncData[no].TokensToWrite.Wait;
end;

procedure TProducerConsumersSynchronization.EndConsumptionAnItem(var no : integer);
begin
  // V(Jetons écrits)
  syncData[no].TokensWritten.Post;

  // Et préparer pour le bloc suivant
  no := (no + 1) mod _NbConsumers;
end;



constructor TSemaphore.Create(Tokens : integer = 1);
begin
  sem := SemaphoreInit;
  if( sem = Pointer(-1) ) then
    raise EThreadExternalException.Create('Unable to init semaphore');

  sem_id := (QWord(sem) >> 4) mod 1024;
  writeln(' Thread ', GetCurrentThreadId, ' created sem ', QWord(sem), ' = ', sem_id, ' with ', Tokens, ' tokens');
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
  for i := 1 to Tokens do
    SemaphorePost(sem);
end;

procedure TSemaphore.Wait(Tokens : integer = 1);
var i  : integer;
begin
  for i := 1 to Tokens do
    SemaphoreWait(sem);
end;


end.

