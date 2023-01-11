(******************************************************************************)
(* udatabase.pas                                                   ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementierung einer höchst ineffizienten, aber dafür       *)
(*               generischen Datenbank                                        *)
(* Features    :                                                              *)
(*          - Laden / Speichern in Streams                                    *)
(*          - Add / Delete Dataset                                            *)
(*          - LocateDataSets (Man gibt ein Element eines Datensets an und     *)
(*                            bekommt die Menge der Treffer)                  *)
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
(*                                                                            *)
(******************************************************************************)
Unit udatabase;

{$MODE objfpc}{$H+}

Interface

(*
If you get a compile error, create the udatabase.inc file and copy the content
below into it.

-----------------Start Content of udatabase.inc

{$define USE_OOP_METHODS} // Set if you use Object Callbacks

- - - - - - - - - - - - - - - - - End Content Of udatabase.inc
*)

{$I udatabase.inc}

Uses
  Classes, SysUtils;

Const
  DataBaseVersion: UInt32 = 1;

Type

  { TDataBase }

  Generic TDataBase < T > = Class
  Type // -- Comment this out to auto codeformat with delforlaz
    TElementIsIn = Function(Element: String; Const Data: T): Boolean{$IFDEF USE_OOP_METHODS} Of Object{$ENDIF};
    TDataEqual = Function(Const A, B: T): Boolean{$IFDEF USE_OOP_METHODS} Of Object{$ENDIF};
    TOnDataSetEvent = Procedure(Sender: TObject; Var Data: T){$IFDEF USE_OOP_METHODS} Of Object{$ENDIF};
    TSaveDataSetToStream = Procedure(Const Data: T; Const Stream: TStream){$IFDEF USE_OOP_METHODS} Of Object{$ENDIF};
    TLoadDataSetFromStream = Function(Const Stream: TStream): T{$IFDEF USE_OOP_METHODS} Of Object{$ENDIF};

    TArrayOfT = Array Of T;

  protected
    fDataBase: Array Of T;
    FLastError: String; // Textual description of the last Error that occured.
    Function GetIndexOfData(Data: T): integer; // -1, if not found
  public
    (*
     * Operanden zum arbeiten
     *)
    ElementIsIn: TElementIsIn; // -- Callback to check if Element is in T, True if so.
    DataEqual: TDataEqual; // -- Callback to compare two T-Elements, True if "same"

    (*
     * Optionale Events
     *)
    OnDeleteDataSetEvent: TOnDataSetEvent; // -- Event that is called if a Dataset is deleted
    OnAddDataSetEvent: TOnDataSetEvent; // -- Event that is called if a Dataset is added to the database

    (*
     * Events die zur Nutzung von Außen auf jeden Fall definiert werden müssen
     *)
    OnLoadDataSetFromStream: TLoadDataSetFromStream;
    OnSaveDataSetToStream: TSaveDataSetToStream;

    Constructor Create(); virtual;
    Destructor Destroy; override;

    (*
     * Lesende Routinen
     *)
    Function LocateDataSets(Element: String): TArrayOfT virtual; // Get all Datasets that contain Element

    (*
     * Verändernde Routinen
     *)
    Procedure Clear() virtual;
    Function AddDataSet(DataSet: T): Boolean virtual; // Add a DataSet to Database, true if possible
    Function DeleteDataSet(DataSet: T): Boolean virtual; // Delete data from Database, true if something was deleted
    Function OverwriteDataSet(DataSet: T): Boolean virtual; // Overwrites data from Database, true if something was overwritten

    (*
     * Laden / Speichern
     *)
    Procedure SaveToStream(Const Stream: TStream) virtual;
    Function LoadFromStream(Const Stream: TStream): Boolean virtual;
  End;

Implementation

{ TDataBase }

Constructor TDataBase.Create();
Begin
  Inherited create;
  FLastError := '';
  fDataBase := Nil;

  ElementIsIn := Nil;
  DataEqual := Nil;

  OnDeleteDataSetEvent := Nil;
  OnAddDataSetEvent := Nil;
  OnLoadDataSetFromStream := Nil;
  OnSaveDataSetToStream := Nil;
End;

Destructor TDataBase.Destroy;
Begin
  Clear();
End;

Function TDataBase.GetIndexOfData(Data: T): integer;
Var
  i: Integer;
Begin
  result := -1;
  If Not assigned(DataEqual) Then Begin
    Raise Exception.Create('Error: TDataBase.DataEqual not defined.');
  End;
  For i := 0 To high(fDataBase) Do Begin
    If DataEqual(data, fDataBase[i]) Then Begin
      result := i;
      break;
    End;
  End;
End;

Function TDataBase.LocateDataSets(Element: String): TArrayOfT;
Var
  i: Integer;
Begin
  result := Nil;
  If trim(element) = '' Then Begin // Die Ganze Datenbank
    setlength(result, length(fDataBase));
    For i := 0 To high(fDataBase) Do Begin
      result[i] := fDataBase[i];
    End;
    exit;
  End;
  If Not assigned(ElementIsIn) Then Begin
    Raise Exception.Create('Error: TDataBase.ElementIsIn not defined.');
  End;
  For i := 0 To high(fDataBase) Do Begin
    If ElementIsIn(Element, fDataBase[i]) Then Begin
      setlength(result, length(result) + 1);
      result[high(result)] := fDataBase[i];
    End;
  End;
End;

Procedure TDataBase.Clear();
Var
  i: Integer;
Begin
  For i := 0 To high(fDataBase) Do Begin
    If assigned(OnDeleteDataSetEvent) Then OnDeleteDataSetEvent(self, fDataBase[i]);
  End;
  setlength(fDataBase, 0);
End;

Function TDataBase.AddDataSet(DataSet: T): Boolean;
Begin
  result := false;
  FLastError := '';
  If GetIndexOfData(DataSet) <> -1 Then Begin
    FLastError := 'Error, dataset already exists.';
    exit;
  End;
  result := true;
  If assigned(OnAddDataSetEvent) Then OnAddDataSetEvent(self, DataSet);
  setlength(fDataBase, length(fDataBase) + 1);
  fDataBase[high(fDataBase)] := DataSet;
End;

Function TDataBase.DeleteDataSet(DataSet: T): Boolean;
Var
  index, i: integer;
Begin
  FLastError := '';
  result := false;
  index := GetIndexOfData(DataSet);
  If index = -1 Then Begin
    FLastError := 'Error, unable to locate dataset.';
    exit; // Nichts zu löschen
  End;
  result := true;
  If assigned(OnDeleteDataSetEvent) Then OnDeleteDataSetEvent(self, fDataBase[index]);
  For i := index To high(fDataBase) - 1 Do Begin
    fDataBase[i] := fDataBase[i + 1];
  End;
  setlength(fDataBase, high(fDataBase));
End;

Function TDataBase.OverwriteDataSet(DataSet: T): Boolean;
Var
  index: integer;
Begin
  result := false;
  index := GetIndexOfData(DataSet);
  If index = -1 Then Begin
    FLastError := 'Error, could not locate dataset.';
    exit; // Nichts zu löschen
  End;
  result := true;
  fDataBase[index] := DataSet;
End;

Procedure TDataBase.SaveToStream(Const Stream: TStream);
Var
  l: UInt32;
  i: integer;
Begin
  If Not assigned(OnSaveDataSetToStream) Then Begin
    Raise Exception.Create('Error: TDataBase.OnSaveDataSetToStream not defined.');
  End;
  // 1. Datenbank Version schreiben
  stream.Write(DataBaseVersion, SizeOf(DataBaseVersion));

  // 2. Anzahl der Datensätze
  l := length(fDataBase);
  stream.Write(l, SizeOf(l));

  // 3. Speichern der Datensätze
  For i := 0 To high(fDataBase) Do Begin
    OnSaveDataSetToStream(fDataBase[i], Stream);
  End;
End;

Function TDataBase.LoadFromStream(Const Stream: TStream): Boolean;
Var
  l: UInt32;
  fv: UInt32;
  i: Integer;
Begin
  result := false;
  If Not assigned(OnLoadDataSetFromStream) Then Begin
    Raise Exception.Create('Error: TDataBase.OnLoadDataSetFromStream not defined.');
  End;
  Clear;
  // 1. Datenbank Version lesen
  fv := 0;
  stream.Read(fv, sizeof(fv)); // Die Datei ist abgespeichert als DB-Version "fv"
  // 2. Anzahl der Datensätze
  l := 0;
  stream.Read(l, sizeof(l)); // Die Datei ist abgespeichert als DB-Version "l"
  setlength(fDataBase, l);
  // 3. Laden der Datensätze
  For i := 0 To high(fDataBase) Do Begin
    fDataBase[i] := OnLoadDataSetFromStream(Stream);
  End;
  result := true;
End;

End.


