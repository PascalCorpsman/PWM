(******************************************************************************)
(* PWM                                                             ??.??.???? *)
(*                                                                            *)
(* Version     : 0.23                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : a simple multiuser Password manager                          *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 = Merge Databases                                       *)
(*               0.03 = Bessere Fehlermeldungen                               *)
(*                      Erkennen, wenn Dateiversion zu neu                    *)
(*                      Gui-Gimicks eingebaut                                 *)
(*               0.04 = Besserer Vergleich beim Merge                         *)
(*               0.05 = Speicher an dem Passwörter Abliegen, vor dem Freigeben*)
(*                      mit * überschreiben                                   *)
(*               0.06 = Copy Username spinnt manchmal                         *)
(*               0.07 = im Mergedialog haben diverse Visualisierungen nicht   *)
(*                      gestimmt                                              *)
(*                      Username und Description für "Überschreiben" Heuristik*)
(*               0.08 = Anzeigen PWM + Datenbankname in Application.Title     *)
(*               0.09 = 2. Abfrage beim Beenden ohne Speichern                *)
(*                      Fix Anchoring im Add Dialog                           *)
(*               0.10 = Automatisches anwählen eines Datensatzes nach dem     *)
(*                      Erstellen / Merge                                     *)
(*                      einführen MultiUser System                            *)
(*               0.11 = User können nun ihre eigenen Passwörter ändern        *)
(*                      Eine Fehlerhaft geöffnete Datenbank kann nicht mehr   *)
(*                      Editiert werden.                                      *)
(*               0.12 = Sonderzeichen Editierbar gemacht                      *)
(*                      Option "nur uppercase" für Passwort generator         *)
(*               0.13 = Add Option "Copy URL"                                 *)
(*               0.14 = Password Verification when adding new users           *)
(*               0.15 = Fix, non Admin user was not able to edit his own      *)
(*                      dataset when editing right after creation             *)
(*                      Fix, Beim Editieren eines Datensatzes wurden die Daten*)
(*                      in der Liste im Hauptformular nicht Aktualisiert      *)
(*               0.16 = Retry password entry when PW is invalid.              *)
(*               0.17 = Add "unhide" Password Button on startup dialog        *)
(*               0.18 = Add "unhide" Passwort to single user DB prompt        *)
(*               0.19 = ADD show password length in "ADD" dialog              *)
(*               0.20 = Show app on screen where the mouse is located         *)
(*               0.21 = Alphabetical sort in user management menu             *)
(*                      Show user in user manager list                        *)
(*               0.22 = Add Preferences                                       *)
(*                      Add Option ignore irritating symbols                  *)
(*                      Add Option autosize result window                     *)
(*               0.23 = Add Icons to Menu                                     *)
(*                      Add support for fileserver                            *)
(*                                                                            *)
(******************************************************************************)
(*  Silk icon set 1.3 used                                                    *)
(*  ----------------------                                                    *)
(*  Mark James                                                                *)
(*   https://peacocksoftware.com/silk                                         *)
(******************************************************************************)
(*  This work is licensed under a                                             *)
(*  Creative Commons Attribution 2.5 License.                                 *)
(*  [ http://creativecommons.org/licenses/by/2.5/ ]                           *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, Menus, Grids, ComCtrls, upwm, UniqueInstance;

Const
  PWM_Version = '0.23';

  IndexPassword = 0;
  IndexUrl = 1;
  IndexDescription = 2;
  IndexUserName = 3;

Type

  TResult = (rOK, rCancel, rIgnore);

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    UniqueInstance1: TUniqueInstance;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem18Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure StringGrid1ButtonClick(Sender: TObject; aCol, aRow: Integer);
    Procedure StringGrid1DblClick(Sender: TObject);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
    Procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; Const Parameters: Array Of String);
  private
    fUser: String;
    fDataBase: TPWM;
    fLoadedDataSets: TDataSetArray;

    (*
     * Prüft ob die Datenbank gespeichert ist
     * rOK = Alles Gut, so wie es sein soll
     * rCancel = Der User hat nicht gespeichert, will seine Aktuelle Aktion abbrechen
     * rIgnore = Der User hat nicht gespeichert, will mit seiner Aktuellen Aktion fortfahren -> Datenverlust
     *
     * MSG = Meldung die dem User beim Feststellen des "Nicht gespeichert" angezeigt wird.
     *       Die Meldung muss so formuliert werden, dass "Nein" = rCancel wird.
     *)
    Function SecureDBIsSaved(Msg: String): TResult;

    Procedure ClearLCL();
    Procedure LoadDataSets();
    Procedure RefreshStatusBar();
    Procedure MergeDataBase(Const DatabaseFilename: String);
  public
    Function PromptPassword(Const Database: String; UserToPreselect: String; EnableUserSelection: Boolean = True): TUser;
    Procedure OpenDatabase(Filename: String);
  End;

Var
  Form1: TForm1;
  SelectedRow: Integer;
  Form1ShowOnce: boolean = true;

Implementation

{$R *.lfm}

Uses LazFileUtils, LCLType, Clipbrd, lclintf, math
  , unit2 // Add / Compare dataset
  , unit3 // Merge DB Dialog
  , unit4 // MultiUser Password Prompt
  , unit5 // User Management
  // , unit6 Add User Dialog
  // , unit7 Password change dialog
  , unit8 // Options Dialog
  , unit9 // Select Database Dialog
  , usslconnector
  ;

{ TForm1 }

Procedure TForm1.MenuItem10Click(Sender: TObject);
Begin
  showmessage(
    'PWM - Password Manager ver. ' + PWM_Version + LineEnding + LineEnding +
    'Author: Corpsman' + LineEnding +
    'Homepage: http://www.Corpsman.de' + LineEnding + LineEnding +
    'License:' + LineEnding +
    '   This program is postcardware, see Homepage' + LineEnding +
    '   for further details.' + LineEnding + LineEnding +
    'Warranty: There is no warranty use on your own risk!' + LineEnding + LineEnding +
    'Encryption algorithm: Rijndael (4096-Bit Key)' + LineEnding + LineEnding +
    'Descrption:' + LineEnding +
    '   This is a simple password manager' + LineEnding +
    '   do not leave the programm open for long times' + LineEnding +
    '   its security is only existing when closed.');
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Application.Title := 'PWM';
  caption := 'No Database loaded.';
  Randomize;
  Edit1.Text := '';
  fDataBase := TPWM.Create();
  StringGrid1.RowCount := 1;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Begin
  // Merge Other Database into actual
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  If OpenDialog1.Execute Then Begin
    MergeDataBase(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  // Copy Username
  If (SelectedRow <= 0) And (StringGrid1.RowCount > 1) Then Begin
    SelectedRow := 1;
  End;
  If (SelectedRow > 0) And (SelectedRow < StringGrid1.RowCount) Then Begin
    Clipboard.AsText := StringGrid1.Cells[IndexUserName, SelectedRow];
  End
  Else Begin
    showmessage('Error, nothing selected.');
  End;
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Begin
  // User Management
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  form5.InitData(fDataBase);
  form5.ShowModal;
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Begin
  // Copy URL
  If (SelectedRow <= 0) And (StringGrid1.RowCount > 1) Then Begin
    SelectedRow := 1;
  End;
  If (SelectedRow > 0) And (SelectedRow < StringGrid1.RowCount) Then Begin
    Clipboard.AsText := fLoadedDataSets[SelectedRow - 1].PlattformURL;
  End
  Else Begin
    showmessage('Error, nothing selected.');
  End;
End;

Procedure TForm1.MenuItem17Click(Sender: TObject);
Begin
  // Options
  form8.CheckBox1.Checked := IniPropStorage1.ReadBoolean('AdjustWidthAfterSearch', true);
  form8.Edit6.text := IniPropStorage1.ReadString('URL', 'https://127.0.0.1');
  form8.Edit7.text := IniPropStorage1.ReadString('PORT', '8444');
  form8.Init(fUser);
  If form8.ShowModal = mrOK Then Begin
    IniPropStorage1.WriteBoolean('AdjustWidthAfterSearch', form8.CheckBox1.Checked);
    IniPropStorage1.WriteString('URL', form8.Edit6.text);
    IniPropStorage1.WriteString('PORT', form8.Edit7.text);
  End;
End;

Procedure TForm1.MenuItem18Click(Sender: TObject);
Var
  pw, url, Port: String;
  m: TMemoryStream;
Begin
  // Merge Other Database into actual
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  pw := PasswordBox('', 'Please enter password for server:');
  If trim(pw) = '' Then Begin
    showmessage('Error, invalid password.');
  End;
  url := IniPropStorage1.ReadString('URL', 'https://127.0.0.1');
  Port := IniPropStorage1.ReadString('PORT', '8444');
  If Not Login(url, Port, ClientID, fUser, pw) Then Begin
    showmessage('Failed to login as : ' + fUser);
  End;
  m := TMemoryStream.Create;
  m.LoadFromFile(IniPropStorage1.ReadString('LastDatabase', ''));
  If SendDB(m) Then Begin
    showmessage('Done.');
  End
  Else Begin
    showmessage('Failed to upload database.');
  End;
  m.free;
  logout;
End;

Procedure TForm1.MenuItem19Click(Sender: TObject);
Var
  pw, url, Port: String;
  m: TMemoryStream;
  sl: TStringList;
  tmpdbname: String;
Begin
  // Merge Other Database into actual
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  If (SecureDBIsSaved('Merge, without save can result in loosing changes. Do you really want to merge now ?') = rCancel) Then Begin
    // rOK und rIgnore führen dazu dass es weiter geht ..
    exit;
  End;
  // Download DB From Server
  pw := PasswordBox('', 'Please enter password for server:');
  If trim(pw) = '' Then Begin
    showmessage('Error, invalid password.');
    exit;
  End;
  url := IniPropStorage1.ReadString('URL', 'https://127.0.0.1');
  Port := IniPropStorage1.ReadString('PORT', '8444');
  If Not Login(url, Port, ClientID, fUser, pw) Then Begin
    showmessage('Failed to login as : ' + fUser);
    exit;
  End;
  sl := GetDBList();
  If Not assigned(sl) Then Begin
    showmessage('Error, unable to load database list.');
    Logout;
    sl.free;
    exit;
  End;
  If sl.Count = 0 Then Begin
    showmessage('Error, no databases on the server available.');
    Logout;
    sl.free;
    exit;
  End;
  form9.InitWith(sl);
  sl.free;
  If form9.showmodal <> mrOK Then exit;
  m := DownloadDB(form9.RadioGroup1.Items[form9.RadioGroup1.ItemIndex]);
  Logout;
  If Not assigned(m) Then Begin
    showmessage('Error, unable to download database.');
    m.free;
    exit;
  End;
  tmpdbname := GetTempFileName();
  m.SaveToFile(tmpdbname);
  m.free;
  MergeDataBase(tmpdbname);
  DeleteFile(tmpdbname);
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  fDataBase.free;
  fDataBase := Nil;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Var
  i: Integer;
Begin
  If (SecureDBIsSaved('Closing, without save will result in loosing all changes. Do you really want to close now ?') = rCancel) Then Begin
    CanClose := false;
    exit;
  End;
  Clipboard.AsText := ''; // Den Zwischenspeicher wieder Löschen
  For i := 0 To high(fLoadedDataSets) Do Begin
    ClearAndFree(fLoadedDataSets[i].Password);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  s: String;
Begin
  // Search
  s := Edit1.Text;
  fLoadedDataSets := fDataBase.LocateDataSets(s);
  ClearLCL();
  LoadDataSets();
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  s: String;
Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    s := IniPropStorage1.ReadString('LastDatabase', '');
    If s <> '' Then Begin
      OpenDataBase(s);
    End;
    SenderFormWhereMouseIs(self);
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Open Database
  If SecureDBIsSaved('Open a new Database without storing the old one will result in data loss, would you really want to proceed ?') = rCancel Then Begin
    exit;
  End;
  If OpenDialog1.Execute Then Begin
    OpenDataBase(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  pw: TUser;
Begin
  // New Database
  If SecureDBIsSaved('New Database without storing the old one will result in data loss, would you really want to proceed ?') = rCancel Then Begin
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    If FileExists(SaveDialog1.FileName) Then Begin
      showmessage('Error, file already exists, please choose a other filename.');
      exit;
    End;
    pw := PromptPassword(SaveDialog1.FileName, '');
    If pw.Password = '' Then Begin
      showmessage('You entered a invalid password.');
    End
    Else Begin
      fDataBase.Clear(pw.Password);
      fDataBase.FlushData(); // Sonst kann man eine Leere Datenbank nicht speichern
      If fDataBase.SaveToFile(SaveDialog1.FileName, pw) Then Begin
        ClearAndFree(pw.Password);
        IniPropStorage1.WriteString('LastDatabase', SaveDialog1.Filename);
        caption := 'Connected as ' + pw.UserName + ' to: ' + ExtractFileNameOnly(SaveDialog1.Filename);
        Application.Title := 'PWM: ' + ExtractFileNameOnly(SaveDialog1.Filename);
      End
      Else Begin
        ShowMessage(fDataBase.LastError);
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Var
  d: TDataSet;
  i: Integer;
Begin
  // Add
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  form2.Init('Add', GetEmptyDataSet(), false, false);
  If form2.Showmodal = mrOK Then Begin
    d := form2.GetData();
    If Not fDataBase.AddDataSet(d, false) Then Begin
      Showmessage('Error could not store dataset into database.');
    End;
    For i := 0 To high(fLoadedDataSets) Do Begin
      ClearAndFree(fLoadedDataSets[i].Password);
    End;
    // Auswahl des gerade erstellten Datensatzes anzeigen
    setlength(fLoadedDataSets, 1);
    fLoadedDataSets[0] := d;
    LoadDataSets();
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Var
  s: String;
  pw: TUser;
Begin
  // Save
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  s := IniPropStorage1.ReadString('LastDatabase', '');
  If FileExists(s) Then Begin
    pw := PromptPassword(s, fUser, false);
    If fDataBase.SaveToFile(S, pw) Then Begin
      showmessage('Save done.');
    End
    Else Begin
      ShowMessage(fDataBase.LastError);
    End;
    ClearAndFree(pw.Password);
  End
  Else Begin
    MenuItem7Click(Nil);
  End;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Var
  PW: TUser;
Begin
  // Save As
  If Not fDataBase.Connected Then Begin
    showmessage('Error, no valid database loaded.');
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    pw := PromptPassword(SaveDialog1.FileName, fUser, false);
    fDataBase.FlushData(); // Sicherstellen, das die DB auch wirklich gespeichert wird
    If fDataBase.SaveToFile(SaveDialog1.FileName, pw) Then Begin
      caption := 'Connected as ' + pw.UserName + ' to: ' + ExtractFileNameOnly(SaveDialog1.Filename);
      IniPropStorage1.WriteString('LastDatabase', SaveDialog1.FileName);
      ClearAndFree(PW.Password);
      showmessage('Save done.');
    End
    Else Begin
      ShowMessage(fDataBase.LastError);
    End;
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.StringGrid1ButtonClick(Sender: TObject; aCol, aRow: Integer);
Begin
  If acol = IndexPassword Then Begin // Copy PW to Clipboard
    Clipboard.AsText := fLoadedDataSets[aRow - 1].Password;
  End;
  If acol = IndexUrl Then Begin
    openurl(fLoadedDataSets[aRow - 1].PlattformURL);
  End;
End;

Procedure TForm1.StringGrid1DblClick(Sender: TObject);
Var
  d: TDataSet;
  i: integer;
Begin
  // Edit
  If SelectedRow <> -1 Then Begin
    form2.Init('Edit', fLoadedDataSets[SelectedRow - 1], true, false);
    Case form2.Showmodal Of
      mrOK: Begin
          d := form2.GetData();
          If fDataBase.OverwriteDataSet(d) Then Begin
            fLoadedDataSets[SelectedRow - 1] := d; // Im Puffer auch Aktualisieren
            StringGrid1.Cells[IndexDescription, SelectedRow] := d.Description;
            StringGrid1.Cells[IndexUserName, SelectedRow] := d.UserName;
          End
          Else Begin
            Showmessage(fDataBase.LastError);
          End;
        End;
      mrNo: Begin // Delete
          If fDataBase.DeleteDataSet(form2.GetData()) Then Begin
            ClearAndFree(fLoadedDataSets[SelectedRow - 1].Password);
            For i := SelectedRow - 1 To high(fLoadedDataSets) - 1 Do Begin
              fLoadedDataSets[i] := fLoadedDataSets[i + 1];
            End;
            setlength(fLoadedDataSets, high(fLoadedDataSets));
            StringGrid1.DeleteRow(SelectedRow);
            SelectedRow := min(SelectedRow - 1, StringGrid1.RowCount - 1);
            If SelectedRow = 0 Then SelectedRow := -1; // Es gibt keine Auswählbaren Zeilen mehr
            RefreshStatusBar();
          End
          Else Begin
            showmessage(fDataBase.LastError);
          End;
        End;
    End;
  End;
End;

Procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  SelectedRow := aRow;
End;

Procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; Const Parameters: Array Of String);
Begin
  // Die Andere instanz stirbt, wir bringen uns nach Vorne, dass der User das auch mitkriegt ;)
  Application.BringToFront;
End;

Function TForm1.SecureDBIsSaved(Msg: String): TResult;
Begin
  result := rOK;
  If fDataBase.Modified And fDataBase.Connected Then Begin
    Try
      MenuItem6Click(Nil);
    Except
      On av: exception Do Begin
        showmessage(av.Message);
      End;
    End;
    If fDataBase.Modified Then Begin
      If ID_NO = Application.MessageBox(Pchar(Msg), 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
        result := rCancel;
      End
      Else Begin
        result := rIgnore;
      End;
    End;
  End;
End;

Function TForm1.PromptPassword(Const Database: String; UserToPreselect: String;
  EnableUserSelection: Boolean): TUser;
Var
  users: TStringList;
  i: Integer;
  Retry: Boolean;
Begin
  result.Password := '';
  result.UserName := '';
  Retry := true;
  users := TPWM.GetUsersFromDatabase(Database);
  Repeat
    If users.Count = 1 Then Begin
      form4.ComboBox1.Text := users[0];
      form4.ComboBox1.Visible := false;
      form4.label1.Visible := false;
    End
    Else Begin
      form4.ComboBox1.Visible := true;
      form4.label1.Visible := true;
      form4.ComboBox1.Clear;
      form4.ComboBox1.Items.CommaText := users.CommaText;
      form4.ComboBox1.ItemIndex := 0;
      // Merken, welcher User sich als Letztes Angemeldet hatte und dann diesen auswählen
      If UserToPreselect <> '' Then Begin
        For i := 0 To Form4.ComboBox1.Items.Count - 1 Do Begin
          If Form4.ComboBox1.Items[i] = UserToPreselect Then Begin
            form4.ComboBox1.ItemIndex := i;
            break;
          End;
        End;
      End;
      form4.Edit1.Text := '';
      form4.ComboBox1.Enabled := EnableUserSelection;
    End;
    form4.caption := 'Please enter password for:' + ExtractFileNameOnly(Database);
    If Form4.ShowModal = mrOK Then Begin
      If trim(form4.Edit1.Text) <> '' Then retry := false;
      result.UserName := form4.ComboBox1.Text;
      result.Password := form4.Edit1.Text;
      form4.Edit1.Text := ''; // reicht das schon ?
      IniPropStorage1.WriteString('LastUser', form4.ComboBox1.Text);
    End
    Else Begin
      retry := false;
    End;
  Until Not Retry;
  users.free;
End;

Procedure TForm1.ClearLCL;
Begin
  StringGrid1.RowCount := 1;
  SelectedRow := -1;
End;

Procedure TForm1.LoadDataSets;
Var
  w, i, OSOffset: Integer;
Begin
  SelectedRow := -1;
  StringGrid1.RowCount := length(fLoadedDataSets) + 1;
  For i := 0 To high(fLoadedDataSets) Do Begin
    StringGrid1.Cells[IndexDescription, i + 1] := fLoadedDataSets[i].Description;
    StringGrid1.Cells[IndexUserName, i + 1] := fLoadedDataSets[i].UserName;
    StringGrid1.Cells[IndexPassword, i + 1] := 'Copy';
    StringGrid1.Cells[IndexUrl, i + 1] := 'Open';
  End;
  StringGrid1.AutoSizeColumns;
  If IniPropStorage1.ReadBoolean('AdjustWidthAfterSearch', true) Then Begin
    w := 0;
    For i := 0 To StringGrid1.ColCount - 1 Do Begin
      w := w + StringGrid1.ColWidths[i];
    End;
{$IFDEF Windows}
    OSOffset := Scale96ToForm(5); // TODO: rauskriegen
{$ELSE}
    OSOffset := Scale96ToForm(5);
{$ENDIF}
    form1.Width := max(form1.Width, w + OSOffset);
  End;
  RefreshStatusBar();
End;

Procedure TForm1.RefreshStatusBar;
Begin
  StatusBar1.Panels[0].Text := inttostr(StringGrid1.RowCount - 1) + ' entries';
End;

Procedure TForm1.MergeDataBase(Const DatabaseFilename: String);
Var
  i, c: Integer;
  msg: String;
Begin
  If form3.Merge(DatabaseFilename, fDataBase.LocateDataSets('')) = mrOK Then Begin
    msg := '';
    setlength(fLoadedDataSets, length(Form3.DataSetsToAdd) + length(form3.DataSetsToOverwrite));
    c := 0;
    For i := 0 To high(Form3.DataSetsToAdd) Do Begin
      If fDataBase.AddDataSet(Form3.DataSetsToAdd[i], fDataBase.AktualUserRights = urAdminRights) Then Begin
        fLoadedDataSets[c] := Form3.DataSetsToAdd[i];
        c := c + 1;
      End
      Else Begin
        msg := msg + LineEnding + Form3.DataSetsToAdd[i].Description;
      End;
    End;
    // TODO: Das gibt so noch eine komische Fehlermeldung, wenn der User kein Admin ist, evtl umstellen.
    If msg <> '' Then Begin
      msg := 'The Following datasets are alredy existed, but the user didn''t have access rights to it. The rights are now extended, but the content was not changed. Please repeat merge process.' + LineEnding + msg;
    End;
    setlength(Form3.DataSetsToAdd, 0);
    For i := 0 To high(form3.DataSetsToOverwrite) Do Begin
      If fDataBase.OverwriteDataSet(form3.DataSetsToOverwrite[i]) Then Begin
        fLoadedDataSets[c] := Form3.DataSetsToOverwrite[i];
        c := c + 1;
      End
      Else Begin
        msg := msg + LineEnding + fDataBase.LastError;
      End;
    End;
    setlength(Form3.DataSetsToOverwrite, 0);
    If msg <> '' Then Begin
      ShowMessage(msg);
    End;
    If c <> length(fLoadedDataSets) Then Begin
      setlength(fLoadedDataSets, c);
    End;
    LoadDataSets();
  End;
End;

Procedure TForm1.OpenDatabase(Filename: String);
Var
  PW: TUser;
  retry: Boolean;
Begin
  Application.Title := 'PWM';
  If Not FileExists(Filename) Then exit;
  retry := true;
  Repeat
    pw := PromptPassword(Filename, IniPropStorage1.ReadString('LastUser', ''));
    If pw.Password = '' Then Begin
      retry := false;
    End
    Else Begin
      fDataBase.Clear('');
      If fDataBase.LoadFromFile(FileName, pw) Then Begin
        fUser := PW.UserName;
        caption := 'Connected as ' + pw.UserName + ' to: ' + ExtractFileNameOnly(Filename);
        IniPropStorage1.WriteString('LastDatabase', Filename);
        ClearAndFree(pw.Password);
        Application.Title := 'PWM: ' + ExtractFileNameOnly(Filename);
        retry := false;
      End
      Else Begin
        fUser := ''; // Die DB ist nicht geladen
        If fDataBase.LastError <> '' Then Begin
          showmessage('Error, ' + fDataBase.LastError);
        End
        Else Begin
          showmessage('Error, could not load: ' + ExtractFileNameOnly(Filename));
        End;
      End;
    End;
  Until Not retry;
End;

End.

