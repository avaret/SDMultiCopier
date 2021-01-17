unit detectRemovDisk;
{ Gère la détection et le maintient de la liste des disques amovibles }


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

var
  listDisks : array of string; // Un tableau avec "/dev/sde" "/dev/sdf" ...
  listDisks_linear : string; // Une chaîne de caractères où les disques sont séparés par des espaces

procedure identifierDisquesAmovibles;

implementation


procedure identifierDisquesAmovibles;
Var disks, devdisk : TUnicodeSearchRec;
  currentdisk : TextFile;
  isremov, removableFilename, diskname : String;
begin
{ Sous Linux, les disques ont l'identifiant "8" et sont des périph de type bloc =>
    Il existe à chaque fois une entrée de pseudo-dossier /sys/dev/block/8:XX
    Dans ce dossier, un pseudo-fichier removable contient "0" ou "1" selon que
    le disque soit amovible ou non.

   /sys/dev/block/8:XX/device/block contient un seul élément : un pseudo-dossier
     portant le nom du périph dans /dev }

  SetLength(listDisks, 0);
  listDisks_linear := '';

  if(FindFirst('/sys/dev/block/8:*', faDirectory, disks) = 0) then
  begin
    repeat
      isremov := '0';
      removableFilename := '/sys/dev/block/' + String(disks.Name) + '/removable';
      If(FileExists(removableFilename)) Then
      Begin
        // Lire 1 octet du fichier "removable" de ce dossier
        AssignFile(currentdisk, removableFilename);
        Reset(currentdisk);
        Readln(currentdisk, isremov);
        CloseFile(currentdisk);
      end;

      if isremov = '1' Then
      begin
        // récup le nom du périph et l'ajouter

        if(FindFirst('/sys/dev/block/' + disks.Name + '/device/block/sd*', faDirectory, devdisk) = 0) then
        begin
          diskname := '/dev/' + String(devdisk.Name);
          SetLength(listDisks, Length(listDisks)+1);
          listDisks[High(listDisks)] := diskname;
          listDisks_linear := listDisks_linear + ' ' + diskname;
          FindClose(devdisk);
        end;
      end;
    until FindNext(disks) <> 0;
    FindClose(disks);
  end;

end;

end.

