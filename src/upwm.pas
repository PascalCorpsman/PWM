(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PWM                                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit upwm;

{$MODE objfpc}{$H+}

Interface

Uses
  Forms, Classes, SysUtils, udatabase, DCPblowfish, DCPrijndael, DCPsha256, DCPcrypt2;

Const

  (*
   * Client ID für Filechecker ssl Interface
   *)
  ClientID = 'PWM';

  (*
   * Every Database has this text stored, it is used to validate the decryption process
   * So its content don't care but need to be fix.
   *)
  MagicHeader = 'I am the magic header bytes, do not change me!';

  (*
   * Historie: 1 = Initialversion
   *           2 = + Email
   *           3 = + comment
   *           4 = Umstellen von Blowfish auf Rijndael
   *           5 = + UserManagement
   *)
  PWM_FileVersion: UInt32 = 5; // Increase, when file format is changed

  (*
   * Liste der Möglichen Aktionen                                  Recht das man mindestens haben muss um die Aktion aus zu führen
   * - Anlegen eines Datensatzes                                   []
   * - Löschen eines Datensatzes der auch anderen gehört           [AdminRights]
   * - Löschen eines Datensatzes der nur dem Benutzer gehört       []
   * - Überschreiben eines Datensatzes der auch anderen gehört     [AdminRights]
   * - Überschreiben eines Datensatzes der nur dem Benutzer gehört []
   * - User Anlegen                                                [AdminRights]
   * - User Löschen                                                [AdminRights]
   * - Eigenem Benutzer Rechte wegnehmen                           []
   * - Anderem Benutzer Rechte wegnehmen                           [AdminRights]
   * - Anderem Benutzer Rechte Erteilen                            []
   * - Eigenes Passwort ändern                                     []
   *)

  urNoRights = 0;
  urAdminRights = 255;

Type

  TUser = Record
    UserName: String;
    Password: String;
  End;

  TUserContent = Record
    Rights: uint8; // Die Berechtigung die der User in der DB hat
    MasterKey: String; // Schlüssel der Master Datenbank -- Achtung dieser wird nicht zur Laufzeit gespeichert immer nur zum ver und entschlüsseln
  End;

  TUserData = Record
    UserName: String; // Der UserName -> Die dürfen nicht doppelt sein !!
    Content: TUserContent;
    EncryptedContent: TMemoryStream; // Der Verschlüsselte Content
  End;

  TDataSet = Object
  private
    // ---- Alles ab hier wird nicht gespeichert
    PrimaryKey: integer; // Wird von TWPM-Verwaltet
  public
    // ---- Alles ab hier wird gespeichert ----
    UserName: String;
    Password: String;
    PlattformURL: String;
    Email: String; // Die auf der Plattform Registrierte E-Mail Addresse
    Description: String;
    Comment: String;
    UserRights: Uint32; // BitKodiert die Rechte der User, 1 = User Darf den Datensatz Bearbeiten / Lesen, 0 = User ist nicht Berechtigt den Datensatz zu lesen
  End;

  TDataSetArray = Array Of TDataSet;

  { TPWM }

  TPWM = Class(specialize TDataBase < TDataSet > )
  private
    fConnected: Boolean;
    fUsers: Array Of TUserData;
    fAuser: String; // Der Gerade eingelogte User
    fAUserRights: uint8; // Die Userrechte des gerade eingeloggten Users - Das ist Redundant zu fusers[faUser].content.Rights !!
    fmodified: Boolean; // True, wenn die Datenbank sich geändert hat.
    fFileVersion: UInt32; // Die File Version der Gerade geladnenen Datei, für die Callbacks rausgeführt

    fPrimaryKeyCount: integer; // Zur Unterscheidung aller Datensätze

    Procedure WriteMagicHeader(Const Stream: TStream);
    Function ReadAndValidateMagicHeader(Const Stream: TStream): Boolean;

    Procedure SaveDataSetToStream(Const Data: TDataSet; Const Stream: TStream);
    Function LoadDataSetFromStream(Const Stream: TStream): TDataSet;
    Function OnDataEqual(Const A, B: TDataSet): Boolean;
    Function OnElementIsIn(Element: String; Const Data: TDataSet): Boolean;
    Procedure OnDataDeleteSet(Sender: TObject; Var Data: TDataSet);

    // Das Ist Absicht TPWM darf kein "einfaches" zugreifen auf Streams mehr haben
    Procedure SaveToStream(Const Stream: TStream) override;
    Function LoadFromStream(Const Stream: TStream): Boolean; override;
    Class Function UserDataFromStream(Const Stream: TStream): TUserData;
    Procedure UserDataToStream(Const Stream: TStream; Data: TUserData);
    Function GetRandomKey(): String;
    Function EnCryptContent(PassPhrase: String; Const Content: TUserContent): TMemoryStream;
    Function DecryptContent(Const Stream: Tstream; PassPhrase: String): TUserContent;
    Function GetUserMask(): uint32;
  public
    Property AktualUser: String read fAuser;
    Property AktualUserRights: uint8 read fAUserRights;

    Property LastError: String read FLastError;

    Property Modified: Boolean read fmodified;
    Property Connected: Boolean read fConnected; // True, wenn die Datenbank erfolgreich geladen wurde.

    Constructor Create(); override;
    Destructor Destroy(); override;

    Function LocateDataSets(Element: String): TDataSetArray override; // Get all Datasets that contain Element

    Procedure Clear(AdminPW: String) reintroduce;
    Procedure FlushData(); // Will man eine DB-Laden und als etwas anderes Speichern ohne sie zu ändern, geht das nicht, damit es geht muss 1 mal Flush aufgerufen werden

    Function AddDataSet(Var DataSet: TDataSet; ExpandIfExisting: Boolean): Boolean reintroduce; // Achtung, der Datensatz wird beim Add geändert (zumindest der Private Teil)
    Function DeleteDataSet(DataSet: TDataSet): Boolean override;
    Function OverwriteDataSet(DataSet: TDataSet): Boolean override;

    Function getUserList(): TStringlist; // Die Liste aller Aktuell in der Datenbank Registrierten Benutzer
    Function AddUser(ActualUserPassPhrase, NewUsername, NewUserPassPhrase: String; Rights: uint8): Boolean;
    Function DelUser(UserName: String; Force: Boolean): Boolean; // Wenn Force = True, dann werden die verweisten Datensätze einfach gelöscht

    Function getUsersFor(Const DataSet: TDataSet): TStringlist;
    Function RemoveUser(Const DataSet: TDataSet; User: String): Boolean;
    Procedure AddUser(Const DataSet: TDataSet; User: String);
    Function ChangePassword(Const User: TUser; NewPassword: String): Boolean;

    Function SaveToFile(Const Filename: String; Const User: TUser): Boolean;
    Function LoadFromFile(Const Filename: String; Const User: TUser): Boolean;

    (*
     * Liest den Unverschlüsselten Header der registrierten Benutzernamen in der DB aus.
     *)
    Class Function GetUsersFromDatabase(Const Filename: String): TStringList;
  End;

Function GetEmptyDataSet(): TDataSet;
Procedure ClearAndFree(Var Text: String);
Function Compare(Const a, b: TDataSet): Integer;
Procedure SenderFormWhereMouseIs(Const Form: TForm);

Implementation

Uses Controls, math;

Procedure Nop;
Begin
  // Nichts nur zum setzen von Haltepunkten
End;

(*
 * 0 => Komplett unterschiedlich
 * 1 => Der UserName und Description sind Gleich
 * 2 => Alles ist Gleich
 *)

Function Compare(Const a, b: TDataSet): Integer;
Begin
  result := 0;
  If (trim(a.UserName) = trim(b.UserName)) And
    (trim(a.Description) = trim(b.Description)) Then result := 1;
  If (trim(a.UserName) = trim(b.UserName)) And
    (trim(a.Password) = trim(b.Password)) And
    (trim(a.PlattformURL) = trim(b.PlattformURL)) And
    (trim(a.Email) = trim(b.Email)) And
    (trim(a.Description) = trim(b.Description)) And
    (trim(a.Comment) = trim(b.Comment)) Then result := 2;
End;

Function PointInRect(p: Tpoint; r: Trect): boolean;
Begin
  result :=
    (p.x >= min(r.Left, r.Right)) And
    (p.x <= max(r.Left, r.Right)) And
    (p.y >= min(r.Top, r.Bottom)) And
    (p.y <= max(r.Top, r.Bottom));
End;

Procedure SenderFormWhereMouseIs(Const Form: TForm);
Var
  i: Integer;
Begin
  For i := 0 To screen.MonitorCount - 1 Do Begin
    If PointInRect(Mouse.CursorPos, Screen.Monitors[i].BoundsRect) Then Begin
      Form.Left := (Screen.Monitors[i].BoundsRect.Left + Screen.Monitors[i].BoundsRect.Right - form.Width) Div 2;
      Form.Top := (Screen.Monitors[i].BoundsRect.Top + Screen.Monitors[i].BoundsRect.Bottom - form.Height) Div 2;
      exit;
    End;
  End;
End;

Function GetEmptyDataSet(): TDataSet;
Begin
  result.PrimaryKey := 0;
  result.UserName := '';
  result.Password := '';
  result.PlattformURL := '';
  result.Email := '';
  result.Description := '';
  result.Comment := '';
End;

Procedure ClearAndFree(Var Text: String);
Var
  i: Integer;
Begin
  For i := 1 To length(Text) Do Begin
    Text[i] := '*';
  End;
  text := '';
End;

{ TPWM }

Constructor TPWM.Create();
Begin
  Inherited Create();
  fConnected := false;
  OnSaveDataSetToStream := @SaveDataSetToStream;
  OnLoadDataSetFromStream := @LoadDataSetFromStream;
  OnDeleteDataSetEvent := @OnDataDeleteSet;
  ElementIsIn := @OnElementIsIn;
  DataEqual := @OnDataEqual;
  fFileVersion := 0;
  fmodified := false;
  fPrimaryKeyCount := 0;
  fUsers := Nil;
End;

Destructor TPWM.Destroy();
Var
  i: Integer;
Begin
  For i := 0 To high(fUsers) Do Begin
    fUsers[i].EncryptedContent.free;
  End;
  setlength(fUsers, 0);
  Inherited Destroy();
End;

Function TPWM.LocateDataSets(Element: String): TDataSetArray;
Var
  uMask: uint32;
  tmp: TDataSetArray;
  c, i: integer;
Begin
  tmp := Inherited LocateDataSets(Element);
  result := Nil;
  setlength(result, length(tmp));
  // Die Ergebnisse nach den Userrechten Filtern
  uMask := GetUserMask();
  If uMask = 0 Then Begin
    setlength(tmp, 0);
    exit;
  End;
  c := 0;
  For i := 0 To high(tmp) Do Begin
    If (tmp[i].UserRights And uMask) = uMask Then Begin
      result[c] := tmp[i];
      inc(c);
    End;
  End;
  setlength(result, c);
End;

Procedure TPWM.Clear(AdminPW: String);
Var
  i: Integer;
  ct: TUserContent;
Begin
  Inherited Clear();
  fConnected := false;
  fmodified := false;
  fPrimaryKeyCount := 0;
  For i := 0 To high(fUsers) Do Begin
    fusers[i].EncryptedContent.free;
  End;
  setlength(fUsers, 1);
  fUsers[0].UserName := 'Admin';
  ct.Rights := urAdminRights;
  ct.MasterKey := GetRandomKey();
  fUsers[0].EncryptedContent := EncryptContent(AdminPW, ct);
  fUsers[0].Content.Rights := urAdminRights;
  fAuser := 'Admin';
  fAUserRights := urAdminRights;
End;

Procedure TPWM.FlushData();
Begin
  fmodified := true;
End;

Function TPWM.AddDataSet(Var DataSet: TDataSet; ExpandIfExisting: Boolean): Boolean;
Var
  index, i: integer;
  uMask: UInt32;
Begin
  result := false;
  FLastError := '';
  // Da wir davon ausgehen, dass es den Datensatz nicht gibt darf hier nicht mit GetIndexof gearbeitet werden, denn das vergleicht die Primärschlüssel
  // index := GetIndexOfData(DataSet);
  index := -1;
  For i := 0 To high(fDataBase) Do Begin
    If Compare(fDataBase[i], DataSet) <> 0 Then Begin
      index := i;
      break;
    End;
  End;
  uMask := GetUserMask();
  If index = -1 Then Begin
    DataSet.UserRights := uMask; // Neu hinzugefügt Datensätze sind erst mal nur vom Aktuellen User einsehbar
    DataSet.PrimaryKey := fPrimaryKeyCount;
  End
  Else Begin
    If ExpandIfExisting Then Begin
      // Den Datensatz gibt es schon, der User hatte nur bisher keine Berechtigung darauf
      fDataBase[index].UserRights := fDataBase[index].UserRights Or uMask;
      DataSet.UserRights := fDataBase[index].UserRights;
      fmodified := true;
      flastError := 'Dataset "' + DataSet.Description + '" already existed, but user didn''t have access rights. Rights are now extended, but content is not updated! Please repeat merge process.';
    End
    Else Begin
      flastError := 'A Dataset with this information already exists, but you don''t have the rights to access to it.';
    End;
    exit;
  End;
  Result := Inherited AddDataSet(DataSet);
  If result Then Begin
    fmodified := true;
    fPrimaryKeyCount := fPrimaryKeyCount + 1;
  End;
End;

Function TPWM.DeleteDataSet(DataSet: TDataSet): Boolean;
Var
  index: integer;
  uMask: UInt32;
Begin
  // Der User darf einen Datensatz nur Löschen, wenn er ihm alleine gehört, oder er Admin ist
  If fAUserRights <> urAdminRights Then Begin
    FLastError := '';
    result := false;
    index := GetIndexOfData(DataSet);
    uMask := GetUserMask();
    If Index <> -1 Then Begin
      If (fDataBase[index].UserRights <> uMask) Then Begin
        FLastError := 'Missing permission to delete: ' + dataset.Description;
        exit;
      End;
    End;
  End;
  Result := Inherited DeleteDataSet(DataSet);
  If result Then Begin
    fmodified := true;
  End;
End;

Function TPWM.OverwriteDataSet(DataSet: TDataSet): Boolean;
Var
  index: integer;
  uMask: UInt32;
Begin
  result := false;
  index := GetIndexOfData(DataSet);
  If Index = -1 Then Begin
    FLastError := 'Error, could not locate dataset.';
    exit;
  End;
  // Der User darf einen Datensatz nur Löschen, wenn er ihm alleine gehört, oder er Admin ist
  If fAUserRights <> urAdminRights Then Begin
    FLastError := '';
    uMask := GetUserMask();
    If (fDataBase[index].UserRights <> uMask) Then Begin
      FLastError := 'Missing permission to overwrite: ' + dataset.Description;
      exit;
    End;
  End;
  DataSet.UserRights := fDataBase[index].UserRights; // Beim Überschreiben, werden die alten Userrechte Übernommen, das RechteManagement wird ja separat gemacht
  Result := Inherited OverwriteDataSet(DataSet);
  If result Then Begin
    fmodified := true;
  End;
End;

Function TPWM.getUserList(): TStringlist;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  For i := 0 To high(fUsers) Do Begin
    result.Add(fUsers[i].UserName);
  End;
End;

Function TPWM.AddUser(ActualUserPassPhrase, NewUsername,
  NewUserPassPhrase: String; Rights: uint8): Boolean;
Var
  fMasterKey: String;
  i: Integer;
  uMask, nMask: uint32;
  ct: TUserContent;
Begin
  result := false;
  FLastError := '';
  If fAUserRights <> urAdminRights Then Begin
    FLastError := 'Error, you do not have permission to add new users.';
    exit;
  End;
  If length(fUsers) >= 32 Then Begin
    FLastError := 'Error, reached maximum number of users allowed.';
    exit;
  End;
  NewUsername := trim(NewUsername);
  If NewUsername = '' Then Begin
    FLastError := 'Error, Invalid username';
    exit;
  End;
  NewUsername := trim(NewUsername);
  If NewUserPassPhrase = '' Then Begin
    FLastError := 'Error, Invalid password for: ' + NewUsername;
    exit;
  End;
  fMasterKey := '';
  uMask := 0;
  For i := 0 To high(fUsers) Do Begin
    If lowercase(fUsers[i].UserName) = lowercase(NewUsername) Then Begin
      FLastError := 'Error, User ' + NewUsername + ' already exists.';
      exit;
    End;
    If fUsers[i].UserName = fAuser Then Begin
      uMask := 1 Shl i;
      ct := DecryptContent(fUsers[i].EncryptedContent, ActualUserPassPhrase);
      fMasterKey := ct.MasterKey;
      If fMasterKey = '' Then Begin
        FLastError := 'Error, invalid password for: ' + fAuser;
        exit;
      End;
    End;
  End;
  If fMasterKey = '' Then Begin // Das hier tritt nie auf !
    FLastError := 'Error, could not find user: ' + fAuser;
    exit;
  End;
  If umask = 0 Then Begin // Das hier tritt nie auf !
    FLastError := 'Error, could not get userrights for: ' + fAuser;
    exit;
  End;
  // Alles gültig, nun kann der neue User erzeugt werden
  nMask := 1 Shl length(fUsers);
  setlength(fUsers, high(fUsers) + 2);
  fUsers[high(fUsers)].UserName := NewUsername;
  ct.Rights := Rights;
  ct.MasterKey := fMasterKey;
  fUsers[high(fUsers)].EncryptedContent := EnCryptContent(NewUserPassPhrase, ct);
  fUsers[high(fUsers)].Content.Rights := Rights;
  // Der neu Angelegte User erbt die Zugriffsrechte des erstellenden Users
  For i := 0 To high(fDataBase) Do Begin
    If (fDataBase[i].UserRights And uMask) = uMask Then Begin
      fDataBase[i].UserRights := fDataBase[i].UserRights Or nMask;
    End;
  End;
  fmodified := true;
  result := true;
End;

Function TPWM.DelUser(UserName: String; Force: Boolean): Boolean;

(*
 * Shiftet alle Bits > Index um 1 nach Rechts, alle drunter bleiben unverändert
 * Dabei wird effektif das Bit Index gelöscht
 *)
  Function shrabove(index: integer; value: Uint32): uint32;
  Var
    lMask: uint32;
  Begin
    // Sichern der Bits < Index
    lMask := (1 Shl index) - 1;
    result := value And lMask;
    // Right Shift der Bits > Index
    value := value Shr 1;
    // Anpassen der Maske
    lmask := Not lMask;
    // Adden der Bits >= index
    result := result Or (lMask And value);
  End;

Var
  dIndex, i: Integer;
  dmask: UInt32;
Begin
  result := false;
  FLastError := '';
  If fAUserRights <> urAdminRights Then Begin
    FLastError := 'Error, you do not have permission to delete other users.';
    exit;
  End;
  If UserName = fAuser Then Begin
    flasterror := 'Error, it is not allowed to delete the user that is actual logged in.';
    exit;
  End;
  dMask := 0;
  dindex := -1;
  For i := 0 To high(fUsers) Do Begin
    If fUsers[i].UserName = UserName Then Begin
      dIndex := i;
      dMask := 1 Shl i;
      break;
    End;
  End;
  If dmask = 0 Then Begin
    FLastError := 'Error, could not find user: ' + UserName;
    exit;
  End;
  For i := 0 To high(fDataBase) Do Begin
    // Prüfen ob der User einen "verwaisten" datensatz hinterlassen würde !
    If (fDataBase[i].userRights And (Not dMask) = 0) And (Not Force) Then Begin
      FLastError := 'Error, after deleting the user: "' + UserName + '" , the dataset: "' + fDataBase[i].Description + '" will not be accessable anymore.';
      exit;
    End;
  End;
  // Der user Kann gelöscht werden
  // 1. Löschen des Users
  fUsers[dIndex].EncryptedContent.free;
  For i := dIndex To high(fUsers) - 1 Do Begin
    fUsers[i] := fUsers[i + 1];
  End;
  setlength(fUsers, high(fUsers));
  // 2. Verschieben aller Zugriffsrechte Masken
  For i := high(fDataBase) Downto 0 Do Begin
    fDataBase[i].userRights := shrabove(dIndex, fDataBase[i].userRights);
    // Der Datensatz kann danach nie wieder zugegriffen werden
    If fDataBase[i].userRights = 0 Then Begin
      // das geht, weil wir die i schleife Rückwärts laufen lassen ;)
      DeleteDataSet(fDataBase[i]);
    End;
  End;
  fmodified := true;
  result := true;
End;

Function TPWM.getUsersFor(Const DataSet: TDataSet): TStringlist;
Var
  i, index: Integer;
Begin
  result := TStringList.Create;
  For i := 0 To high(fUsers) Do Begin
    index := 1 Shl i;
    If (dataset.UserRights And index = index) Then Begin
      result.Add(fUsers[i].UserName);
    End;
  End;
End;

Function TPWM.RemoveUser(Const DataSet: TDataSet; User: String): Boolean;
Var
  i: Integer;
  um: uint32;
Begin
  result := false;
  FLastError := '';
  // Ein User ohne Admin Rechte darf nur eigene Berechtigungen entfernen !
  If (fAUserRights <> urAdminRights) And (user <> fAuser) Then Begin
    exit;
  End;
  um := 0;
  For i := 0 To high(fUsers) Do Begin
    If fUsers[i].UserName = User Then Begin
      um := 1 Shl i;
    End;
  End;
  If um = 0 Then exit; // User nicht gefunden
  For i := 0 To high(fDataBase) Do Begin
    If fDataBase[i].PrimaryKey = DataSet.PrimaryKey Then Begin
      // Wenn Die Berechtigung gelöscht wird, muss mindestens 1 User Übrig Bleiben der Noch Zugriff auf den Datensatz hat.
      If (Not (um) And DataSet.UserRights = 0) Then Begin
        exit;
      End
      Else Begin
        fDataBase[i].UserRights := fDataBase[i].UserRights And Not um;
        fmodified := true;
      End;
      break;
    End;
  End;
  result := true;
End;

Procedure TPWM.AddUser(Const DataSet: TDataSet; User: String);
Var
  i: Integer;
  um: uint32;
Begin
  um := 0;
  For i := 0 To high(fUsers) Do Begin
    If fUsers[i].UserName = User Then Begin
      um := 1 Shl i;
    End;
  End;
  If um = 0 Then exit; // User nicht gefunden
  For i := 0 To high(fDataBase) Do Begin
    If fDataBase[i].PrimaryKey = DataSet.PrimaryKey Then Begin
      // Der User ist noch nicht Berechtigt auf diesen Datensatz
      If (um And DataSet.UserRights = 0) Then Begin
        fDataBase[i].UserRights := fDataBase[i].UserRights Or um;
        fmodified := true;
      End;
      break;
    End;
  End;
End;

Function TPWM.ChangePassword(Const User: TUser; NewPassword: String): Boolean;
Var
  ct: TUserContent;
  i: Integer;
Begin
  result := false;
  fLastError := 'Unknown error, while password change.';
  For i := 0 To high(fUsers) Do Begin
    If fUsers[i].UserName = User.UserName Then Begin
      // Den alten UserContent hohlen
      ct := DecryptContent(fUsers[i].EncryptedContent, User.Password);
      If ct.MasterKey = '' Then Begin // Plausicheck
        FLastError := 'Error, could not decrypt master database key, invalid password.';
        exit;
      End;
      // Den neuen Content erstellen
      fUsers[i].EncryptedContent.Free;
      fUsers[i].EncryptedContent := EnCryptContent(NewPassword, ct);
      // Fertig ;)
      fmodified := true;
      result := true;
      FLastError := '';
      exit;
    End;
  End;
  fLastError := 'Error, can not find user: ' + user.UserName;
End;

Function TPWM.SaveToFile(Const Filename: String; Const User: TUser): Boolean;
Var
  EncrytpStream: TDCP_rijndael;
  fs: TFileStream;
  fm: TMemoryStream;
  DataStream: TMemoryStream;
  fSalt: UInt32;
  i: Integer;
  fMasterKey: String;
  c: uint8;
  ct: TUserContent;
Begin
  result := false;
  If (Not fmodified) Then Begin
    result := true;
    exit; // Wenn Sich nichts geändert hat muss auch nichts gespeichert werden ..
  End;
  If (User.UserName = '') Then Begin
    FLastError := 'Error: invalid user, nothing is stored.';
    exit;
  End;
  If (fAuser <> User.UserName) Then Begin
    FLastError := 'Error: invalid user, nothing is stored.';
    exit;
  End;

  fm := TMemoryStream.Create;
  // Dateiversion
  fm.Write(PWM_FileVersion, SizeOf(PWM_FileVersion));
  // Salt
  fSalt := Random($FFFFFFFF);
  fm.Write(fSalt, SizeOf(fSalt));
  c := length(Fusers);
  fm.Write(c, SizeOf(c));
  fMasterKey := '';
  // Speichern der Ganzen User und dabei Extrahieren des Hauptschlüssels
  For i := 0 To high(fUsers) Do Begin
    // Aus dem Content des Users kann der Masterkey berechnet werden.
    If fUsers[i].UserName = User.UserName Then Begin
      ct := DecryptContent(fUsers[i].EncryptedContent, User.Password);
      fMasterKey := ct.MasterKey;
      If fMasterKey = '' Then Begin // Plausicheck
        fm.free;
        FLastError := 'Error could not decrypt master database key.';
        exit;
      End;
      // Wir haben den Masterkey dann können wir ihn beim User auch gleich noch mal neu
      // Verschlüsseln, => Würde man die Identische Datenbank wieder speichern
      //                   ändert sich so maximal viel ;)
      fUsers[i].EncryptedContent.free;
      fUsers[i].EncryptedContent := EnCryptContent(user.Password, ct);
    End;
    UserDataToStream(fm, fUsers[i]);
  End;
  If fMasterKey = '' Then Begin
    FLastError := 'Error could not decrypt master database key.';
    exit;
  End;
  (*
   * Die Idee ist, jedes Passwort zusätzlich zufällig zu "Salzen"
   * das Salz kann ganz einfach mit gespeichert werden.
   * => Das Bewirkt, dass bei jedem Speichern die Datenbank komplett neu und
   *    anders erzeugt wird. Selbst wenn sich eigentlich nichts ändert.
   *)
  // Speichern der Eigentlichen Datenbank
  DataStream := TMemoryStream.Create;
  WriteMagicHeader(DataStream); // Der Magic Header dient dazu zu erkennen ob die Entschlüsselte Datenbank gültige werte enthält =ist ein Constanter String
  SaveToStream(DataStream);
  DataStream.Seek(0, soBeginning);
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(inttostr(fSalt) + fMasterKey + inttostr(fSalt), TDCP_sha256);
  EncrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  EncrytpStream.EncryptStream(DataStream, fm, DataStream.Size);
  EncrytpStream.free;
  DataStream.Free;
  // Alles hat geklappt -> Tatsächlich Speichern
  fs := TFileStream.Create(Filename, fmCreate Or fmOpenWrite);
  fm.Seek(0, soBeginning);
  fs.CopyFrom(fm, fm.Size);
  fm.free;
  fs.Free;
  fmodified := false;
  result := true;
  fConnected := true;
End;

Function TPWM.LoadFromFile(Const Filename: String; Const User: TUser): Boolean;
Var
  DecrytpStream: TDCP_blockcipher;
  DataStream: TMemoryStream;
  fs: TFileStream;
  fSalt: UInt32;
  c: Uint8;
  i: Integer;
  fMasterKey: String;
  ct: TUserContent;
Begin
  result := false;
  FLastError := '';
  If Not FileExists(Filename) Then Begin
    FLastError := 'File not found: ' + Filename;
    exit;
  End;
  fs := TFileStream.Create(Filename, fmOpenRead);
  fFileVersion := PWM_FileVersion + 1;
  fs.read(fFileVersion, SizeOf(fFileVersion));
  If PWM_FileVersion < fFileVersion Then Begin // Die Geladene Datei ist neuer als die Programmversion => wir können das nicht laden
    FLastError := 'Invalid file version, pleas upgrade PWM to newest version.';
    result := false;
    fs.free;
    exit;
  End;
  fSalt := 0;
  fs.Read(fSalt, SizeOf(fSalt));
  fMasterKey := '';
  fConnected := false; // Scheiter das Laden ab hier ist die Alte Datenbank nicht mehr gültig -> nicht mehr Connected
  If fFileVersion >= 5 Then Begin
    c := 0;
    fs.Read(c, sizeof(c));
    setlength(fUsers, c);
    For i := 0 To high(fUsers) Do Begin
      fUsers[i] := UserDataFromStream(fs);
      If (fUsers[i].UserName = User.UserName) Then Begin
        ct := DecryptContent(fUsers[i].EncryptedContent, User.Password);
        fMasterKey := ct.MasterKey;
        If fMasterKey = '' Then Begin // Plausicheck
          fs.free;
          FLastError := 'Error could not decrypt master database key.';
          exit;
        End;
        fUsers[i].Content.Rights := ct.Rights;
        fAUserRights := ct.Rights;
      End;
    End;
  End
  Else Begin
    // Wir Portieren eine Alte Datei hoch auf das neue Userformat
    // Das Alte format hat nur einen User dessen Passwort die Datenbank verschlüsselt hatte
    // das Neue Format ein Masterpasswort das mittels des UserPasswortes Verschlüsselt gespeichert ist
    // Dieser Code steht so auch in "Clear"
    setlength(fUsers, 1);
    fUsers[0].UserName := 'Admin';
    ct.Rights := urAdminRights;
    ct.MasterKey := GetRandomKey();
    fUsers[0].EncryptedContent := EnCryptContent(User.Password, ct);
    fUsers[0].Content.Rights := urAdminRights;
    fMasterKey := User.Password;
    fAUserRights := urAdminRights;
  End;
  If fMasterKey = '' Then Begin
    fs.free;
    FLastError := 'Error could not decrypt master database key.';
    exit;
  End;

  If fFileVersion >= 4 Then Begin
    DecrytpStream := TDCP_rijndael.Create(Nil);
  End
  Else Begin
    DecrytpStream := TDCP_blowfish.Create(Nil);
  End;
  DataStream := TMemoryStream.Create;
  DecrytpStream.InitStr(inttostr(fSalt) + fMasterKey + inttostr(fSalt), TDCP_sha256);
  DecrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  DecrytpStream.DecryptStream(fs, DataStream, fs.Size - fs.Position);
  DecrytpStream.Burn;
  // Auslesen der Dateiversion der Datenbank
  DataStream.Seek(0, soBeginning);
  If Not ReadAndValidateMagicHeader(DataStream) Then Begin
    FLastError := 'Decryption failed.';
    DataStream.free;
    DecrytpStream.free;
    fs.free;
    exit;
  End;
  result := LoadFromStream(DataStream);
  DecrytpStream.Free;
  DataStream.free;
  fs.free;
  If Not result Then Begin
    FLastError := 'Database invalid';
    exit;
  End;
  fauser := User.UserName;
  fmodified := false;
  fConnected := true;
End;

Class Function TPWM.GetUsersFromDatabase(Const Filename: String): TStringList;
Var
  fs: TFileStream;
  c: uint8;
  FileVersion_: UInt32; // Die File Version der Gerade geladnenen Datei, für die Callbacks rausgeführt
  fSalt: UInt32;
  i: Integer;
  ud: TUserData;
Begin
  result := TStringList.Create;
  If Not FileExists(Filename) Then Begin
    result.add('Admin');
    exit;
  End;
  fs := TFileStream.Create(Filename, fmOpenRead);
  FileVersion_ := 0;
  fs.read(FileVersion_, SizeOf(FileVersion_));
  fSalt := 0;
  fs.Read(fSalt, sizeof(fSalt)); // Überlesen des Salzes
  If FileVersion_ >= 5 Then Begin
    (*
     * Ab Dateiversion 5 gibt es Multiuser
     *)
    // Auslesen der "Userliste"
    c := 0;
    fs.Read(c, sizeof(c));
    For i := 0 To c - 1 Do Begin
      ud := UserDataFromStream(fs);
      result.Add(ud.UserName);
    End;
  End
  Else Begin
    result.add('Admin');
  End;
  fs.free;
End;

Procedure TPWM.WriteMagicHeader(Const Stream: TStream);
Begin
  Stream.WriteAnsiString(MagicHeader);
End;

Function TPWM.ReadAndValidateMagicHeader(Const Stream: TStream): Boolean;
Var
  s: String;
Begin
  result := false;
  Try
    s := Stream.ReadAnsiString;
    If s <> MagicHeader Then Begin
      exit;
    End;
  Except
    exit;
  End;
  result := true;
End;

Procedure TPWM.SaveDataSetToStream(Const Data: TDataSet; Const Stream: TStream);
Begin
  stream.WriteAnsiString(data.UserName);
  stream.WriteAnsiString(data.Password);
  stream.WriteAnsiString(data.PlattformURL);
  stream.WriteAnsiString(data.Email);
  stream.WriteAnsiString(data.Comment);
  stream.WriteAnsiString(data.Description);
  stream.Write(data.UserRights, sizeof(data.UserRights));
End;

Function TPWM.LoadDataSetFromStream(Const Stream: TStream): TDataSet;
Begin
  result.UserName := stream.ReadAnsiString();
  result.Password := stream.ReadAnsiString();
  result.PlattformURL := stream.ReadAnsiString();
  If fFileVersion >= 2 Then Begin
    result.Email := stream.ReadAnsiString();
  End
  Else Begin
    result.Email := '';
  End;
  If fFileVersion >= 3 Then Begin
    result.Comment := stream.ReadAnsiString();
  End
  Else Begin
    result.Comment := '';
  End;
  result.Description := stream.ReadAnsiString();
  If fFileVersion >= 5 Then Begin
    result.UserRights := 0;
    stream.Read(result.UserRights, sizeof(result.UserRights));
  End
  Else Begin
    // Initialisieren einer Datenbank die noch kein Multiuser hatte, es gibt nur
    // Admin und den Berechtigen wir hiermit
    result.UserRights := 1;
  End;
  result.PrimaryKey := fPrimaryKeyCount;
  fPrimaryKeyCount := fPrimaryKeyCount + 1;
End;

Function TPWM.OnDataEqual(Const A, B: TDataSet): Boolean;
Begin
  result := (a.PrimaryKey = b.PrimaryKey);
End;

Function TPWM.OnElementIsIn(Element: String; Const Data: TDataSet): Boolean;
Begin
  element := LowerCase(Element);
  result :=
    (pos(Element, lowercase(Data.Description)) <> 0) Or
    (pos(Element, lowercase(Data.Password)) <> 0) Or
    (pos(Element, lowercase(Data.PlattformURL)) <> 0) Or
    (pos(Element, lowercase(Data.UserName)) <> 0) Or
    (pos(Element, lowercase(Data.Comment)) <> 0) Or
    (pos(Element, lowercase(Data.Email)) <> 0)
    ;
End;

Procedure TPWM.OnDataDeleteSet(Sender: TObject; Var Data: TDataSet);
Begin
  // Den Speicherbereich des Passwortes expliziet überschreiben
  ClearAndFree(Data.Password);
End;

Procedure TPWM.SaveToStream(Const Stream: TStream);
Begin
  Inherited SaveToStream(Stream);
End;

Function TPWM.LoadFromStream(Const Stream: TStream): Boolean;
Begin
  result := Inherited LoadFromStream(Stream);
End;

Class Function TPWM.UserDataFromStream(Const Stream: TStream): TUserData;
Var
  len: uint32;
Begin
  result.UserName := Stream.ReadAnsiString;
  len := 0;
  stream.read(len, sizeof(len));
  result.EncryptedContent := TMemoryStream.Create;
  result.EncryptedContent.CopyFrom(Stream, len);
End;

Procedure TPWM.UserDataToStream(Const Stream: TStream; Data: TUserData);
Var
  len: uint32;
Begin
  stream.WriteAnsiString(data.UserName);
  len := data.EncryptedContent.Size;
  stream.Write(len, sizeof(len));
  data.EncryptedContent.Seek(0, soBeginning);
  stream.CopyFrom(data.EncryptedContent, len);
End;

Function TPWM.GetRandomKey(): String;
Var
  i: Integer;
Begin
  Randomize;
  result := '';
  (*
   * Der Master Datenbankschlüssel ist eine
   * 4096-Bit Zahl
   * Diese wird gespeichert als Hex Werte
   *)
  For i := 0 To (4096 Div 8) - 1 Do Begin
    result := result + format('%0.2X', [random(256)]);
  End;
End;

Function TPWM.EnCryptContent(PassPhrase: String; Const Content: TUserContent
  ): TMemoryStream;
Var
  fSalt: UInt32;
  EncrytpStream: TDCP_rijndael;
  DataStream: TMemoryStream;
Begin
  result := TMemoryStream.Create;
  DataStream := TMemoryStream.Create;
  // Immer zuerst der Magig Header, dann der Content
  DataStream.WriteAnsiString(MagicHeader);

  DataStream.Write(Content.Rights, sizeof(Content.Rights));
  DataStream.WriteAnsiString(Content.MasterKey);

  DataStream.Seek(0, soBeginning);
  // Verschlüsselt den Content und gibt diesen als TMemorystream zurück
  (*
   * 1. 4 Byte Salt
   * 2. Verschlüsselter Text mit PassPhrase
   *)
  fSalt := Random($FFFFFFFF);
  result.Write(fSalt, SizeOf(fSalt));
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(inttostr(fSalt) + PassPhrase + inttostr(fSalt), TDCP_sha256);
  EncrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  EncrytpStream.EncryptStream(DataStream, result, DataStream.Size);
  EncrytpStream.Free;
  DataStream.Free;
End;

Function TPWM.DecryptContent(Const Stream: Tstream; PassPhrase: String
  ): TUserContent;
Var
  fSalt: UInt32;
  DecrytpStream: TDCP_blockcipher;
  m: TMemoryStream;
  s: String;
Begin
  m := TMemoryStream.create;
  DecrytpStream := TDCP_rijndael.Create(Nil);
  fSalt := 0;
  Stream.Seek(0, soBeginning);
  Stream.Read(fSalt, SizeOf(fSalt));
  DecrytpStream.InitStr(inttostr(fSalt) + PassPhrase + inttostr(fSalt), TDCP_sha256);
  DecrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  DecrytpStream.DecryptStream(Stream, m, Stream.Size - Stream.Position);
  DecrytpStream.Burn;
  DecrytpStream.Free;
  m.Seek(0, soBeginning);
  result.MasterKey := '';
  result.Rights := urNoRights;
  Try
    s := m.ReadAnsiString;
    If s <> MagicHeader Then Begin
      m.free;
      exit;
    End;
  Except
    m.free;
    exit;
  End;
  m.Read(result.Rights, sizeof(result.Rights));
  result.MasterKey := m.ReadAnsiString;
  m.free;
End;

Function TPWM.GetUserMask(): uint32;
Var
  i: Integer;
Begin
  result := 0;
  For i := 0 To high(fUsers) Do Begin
    If fAuser = fUsers[i].UserName Then Begin
      result := 1 Shl i;
    End;
  End;
End;

End.

