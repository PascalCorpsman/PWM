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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Menus,
  upwm;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    StringGrid1: TStringGrid;
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure StringGrid1DblClick(Sender: TObject);
    Procedure StringGrid1HeaderSized(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    Procedure StringGrid1Resize(Sender: TObject);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
  private
    fNew, fAktual: TDataSetArray;
    fcol, fRow: integer;
    Function LoadMergeList(): integer;
    Procedure InitResultDataSets();
  public
    DataSetsToOverwrite, DataSetsToAdd: TDataSetArray;
    Function Merge(SourceDatabase: String; Aktual: TDataSetArray): TModalResult;
  End;

Var
  Form3: TForm3;

Implementation

Uses unit1, unit2, LazFileUtils;

{$R *.lfm}

Const
  Col_Old_Descr = 0;
  Col_Old_User = 1;
  Col_Dir = 2;
  Col_New_Descr = 3;
  Col_New_User = 4;
  Col_Old_index = 5;
  Col_New_index = 6;

{ TForm3 }

Procedure TForm3.StringGrid1Resize(Sender: TObject);
Begin
  Label2.Left := StringGrid1.Left + StringGrid1.Columns[0].Width + StringGrid1.Columns[1].Width + StringGrid1.Columns[2].Width;
End;

Procedure TForm3.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  fRow := aRow;
  fcol := aCol;
End;

Procedure TForm3.StringGrid1HeaderSized(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
Begin
  StringGrid1Resize(Nil);
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Merge overview';
End;

Procedure TForm3.MenuItem1Click(Sender: TObject);
Begin
  // Overwrite / Add
  If (fRow > 0) And (fRow < StringGrid1.RowCount) Then Begin
    StringGrid1.Cells[Col_Dir, fRow] := '<-'
  End;
End;

Procedure TForm3.MenuItem2Click(Sender: TObject);
Begin
  // Add
  If (fRow > 0) And (fRow < StringGrid1.RowCount) Then Begin
    StringGrid1.Cells[Col_Dir, fRow] := 'Add'
  End;
End;

Procedure TForm3.MenuItem3Click(Sender: TObject);
Begin
  // Ignore
  If (fRow > 0) And (fRow < StringGrid1.RowCount) Then Begin
    StringGrid1.Cells[Col_Dir, fRow] := 'X'
  End;
End;

Procedure TForm3.StringGrid1DblClick(Sender: TObject);
Begin
  If (StringGrid1.Cells[Col_Old_index, fRow] <> '') And (StringGrid1.Cells[Col_New_index, fRow] <> '') Then Begin
    // Ein Vergleichender Dialog
    form2.Init('View', fAktual[strtoint(StringGrid1.Cells[Col_Old_index, fRow])], false, true);
    form2.Init_second(fNew[strtoint(StringGrid1.Cells[Col_New_index, fRow])], true);
    form2.Showmodal;
  End
  Else Begin
    // Nur ein Datensatz vorhanden
    If StringGrid1.Cells[Col_Old_index, fRow] <> '' Then Begin
      form2.Init('View', fAktual[strtoint(StringGrid1.Cells[Col_Old_index, fRow])], false, true);
      form2.Showmodal;
    End;
    If StringGrid1.Cells[Col_New_index, fRow] <> '' Then Begin
      form2.Init('View', fNew[strtoint(StringGrid1.Cells[Col_New_index, fRow])], false, true);
      form2.Showmodal;
    End;
  End;
End;

Function TForm3.LoadMergeList(): integer;
Var
  y, i, j: Integer;
  found: Boolean;
Begin
  (*
   * Initialisiert die Gui und gibt zurück wie viele Datensätze aus fnew in
   *)
  result := 0;
  StringGrid1.RowCount := 1;
  fRow := -1;
  // Jeden in FNew prüfen ob es den in fAktual gibt
  For i := 0 To high(fnew) Do Begin
    found := false;
    For j := 0 To high(fAktual) Do Begin
      Case Compare(fNew[i], fAktual[j]) Of
        0: Begin
            // Unterschiedlich, wird am Schluss bearbeitet
          End;
        1: Begin
            // Überschreiben anbieten
            StringGrid1.RowCount := StringGrid1.RowCount + 1;
            y := StringGrid1.RowCount - 1;
            StringGrid1.Cells[Col_Old_Descr, y] := fAktual[j].Description;
            StringGrid1.Cells[Col_Old_User, y] := fAktual[j].UserName;
            StringGrid1.Cells[Col_Old_index, y] := inttostr(j);
            StringGrid1.Cells[Col_Dir, y] := '<-';
            StringGrid1.Cells[Col_New_Descr, y] := fnew[i].Description;
            StringGrid1.Cells[Col_new_User, y] := fnew[i].UserName;
            StringGrid1.Cells[Col_New_index, y] := inttostr(i);
            found := true;
            inc(result);
            break;
          End;
        2: Begin
            // identisch, nichts zu tun
            found := true;
            break;
          End;
      End;
    End;
    If Not found Then Begin // Den Datensatz gibt es nicht, also zum Übernehmen anbieten
      StringGrid1.RowCount := StringGrid1.RowCount + 1;
      y := StringGrid1.RowCount - 1;
      StringGrid1.Cells[Col_Old_Descr, y] := '';
      StringGrid1.Cells[Col_Old_User, y] := '';
      StringGrid1.Cells[Col_Old_index, y] := '';
      StringGrid1.Cells[Col_Dir, y] := 'Add';
      StringGrid1.Cells[Col_New_Descr, y] := fnew[i].Description;
      StringGrid1.Cells[Col_new_User, y] := fnew[i].UserName;
      StringGrid1.Cells[Col_New_index, y] := inttostr(i);
      found := true;
      inc(result);
    End;
  End;
  StringGrid1.AutoSizeColumns;
  StringGrid1Resize(Nil);
End;

Procedure TForm3.InitResultDataSets();
Var
  j: Integer;
  d_, dn: TDataSet;
Begin
  DataSetsToOverwrite := Nil;
  DataSetsToAdd := Nil;
  For j := 1 To StringGrid1.RowCount - 1 Do Begin
    // Die Die hinzugefügt werden sollen
    If StringGrid1.Cells[Col_Dir, j] = 'Add' Then Begin
      setlength(DataSetsToAdd, length(DataSetsToAdd) + 1);
      DataSetsToAdd[high(DataSetsToAdd)] := fNew[strtoint(StringGrid1.Cells[Col_New_index, j])];
    End;
    If StringGrid1.Cells[Col_Dir, j] = '<-' Then Begin
      If StringGrid1.Cells[Col_Old_index, j] = '' Then Begin
        // aus <- wird ein Add
        setlength(DataSetsToAdd, length(DataSetsToAdd) + 1);
        DataSetsToAdd[high(DataSetsToAdd)] := fNew[strtoint(StringGrid1.Cells[Col_New_index, j])];
      End
      Else Begin
        // ein echtes "Überschreiben"
        setlength(DataSetsToOverwrite, length(DataSetsToOverwrite) + 1);
        d_ := fAktual[strtoint(StringGrid1.Cells[Col_Old_index, j])]; // Erst mal mit dem alten initialisieren
        dn := fNew[strtoint(StringGrid1.Cells[Col_New_index, j])];
        d_.UserName := dn.UserName;
        d_.Description := dn.Description;
        d_.Comment := dn.Comment;
        d_.Email := dn.Email;
        d_.Password := dn.Password;
        d_.PlattformURL := dn.PlattformURL;
        DataSetsToOverwrite[high(DataSetsToOverwrite)] := d_;
      End;
    End;
  End;
End;

Function TForm3.Merge(SourceDatabase: String; Aktual: TDataSetArray): TModalResult;
Var
  src: TPWM;
  pw: TUser;
  cnt: integer;
Begin
  result := mrCancel;
  DataSetsToOverwrite := Nil;
  DataSetsToAdd := Nil;
  If Not FileExists(SourceDatabase) Then Begin
    showmessage('Error could not load source database.');
    exit;
  End;
  pw := Form1.PromptPassword(SourceDatabase, '');
  If pw.Password = '' Then Begin
    showmessage('Invalid password');
    exit;
  End;
  fAktual := Aktual;
  src := TPWM.Create();
  If Not src.LoadFromFile(SourceDatabase, pw) Then Begin
    If src.LastError <> '' Then Begin
      showmessage('Error, ' + src.LastError);
    End
    Else Begin
      showmessage('Error, could not load: ' + ExtractFileNameOnly(SourceDatabase));
    End;
    exit;
  End;
  fNew := src.LocateDataSets('');
  src.free;
  cnt := LoadMergeList();
  If cnt = 0 Then Begin
    showmessage('Database to merge is a subset or equal to actual database.' + LineEnding + 'Nothing to merge.');
  End
  Else Begin
    result := ShowModal;
    If result = mrOK Then Begin
      InitResultDataSets();
    End;
  End;
End;

End.

