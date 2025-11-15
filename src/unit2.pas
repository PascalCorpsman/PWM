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
Unit Unit2;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, upwm;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit4Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fStoredData, fStoredData2: TDataSet;
    Procedure InitSpecialCharacters;
  public
    Procedure Init(aCaption: String; Const DataSet: TDataSet; ShowDelete, ReadOnly: Boolean);
    Procedure Init_second(Const DataSet: TDataSet; ReadOnly: Boolean);
    Function GetData(): TDataSet;
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Const
  SpecialCharacters: Set Of Char = ['(', ')', '[', ']', '!', '.', '?', '@', '#', '^', '&', '+', '-', '*', '%', '/', '\', '_', '=', '>', '<', '~', '$', '|'];

  { TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  edit5.text := '16';
  For i := 0 To CheckGroup1.Items.Count - 1 Do Begin
    CheckGroup1.Checked[i] := true;
  End;
  CheckGroup1.Checked[3] := false; // Wir wonnen so unterschiedliche wie möglich haben -> diese Einschränkung wieder weg nehmen.
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
End;

Procedure TForm2.InitSpecialCharacters;
Var
  i: Char;
Begin
  edit7.text := '';
  For i In SpecialCharacters Do Begin
    edit7.text := edit7.text + i;
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Const
  Letters: Set Of Char = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];
  Numbers: Set Of Char = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  AmbigiusChars: Set Of Char = [
  'l', '1', 'I' // Kleines L, Eins, Großes i
  , 'O', '0', 'o' // Großes o, Null, kleines O
  , '5', 'S' // Fünf, großes s
  , '2', 'Z' // Zwei, goßes z
  ];
Var
  c: Char;
  chars: Array Of Char;
  available: Set Of Char;
  res: String;
  charslen, i, len: integer;
Begin
  available := [];
  If CheckGroup1.Checked[0] Then available := available + Letters;
  If CheckGroup1.Checked[1] Then Begin
    For i := 1 To length(Edit7.Text) Do Begin
      available := available + [Edit7.Text[i]];
    End;
  End;
  If CheckGroup1.Checked[2] Then available := available + Numbers;
  chars := Nil;
  // Die Grundmenge in ein Array verwandeln
  For c In available Do Begin
    setlength(chars, length(chars) + 1);
    chars[high(chars)] := c;
  End;
  res := '';
  len := strtointdef(edit5.text, 1);
  charslen := length(chars);
  If length(chars) > 0 Then Begin
    // Theoretisch ist es möglich hier eine Endlosschleife zu bauen, wenn man nur Sonderzeichen zuläst und alle Sonderzeichen Ambigius sind,
    // aber wer das macht, dem geschieht es nicht anders ;)
    While length(res) < len Do Begin
      c := chars[random(charslen)];
      If CheckBox1.Checked And (c In AmbigiusChars) Then Begin
        c := #0;
      End;
      If c <> #0 Then
        res := res + c;
    End;
  End;
  If CheckGroup1.Checked[3] Then Begin
    res := uppercase(res);
  End;
  edit4.text := res;
  Edit4Change(Nil);
End;

Procedure TForm2.Edit4Change(Sender: TObject);
Begin
  label11.caption := format('[%d]', [length(edit4.text)]);
End;

Procedure TForm2.Init(aCaption: String; Const DataSet: TDataSet; ShowDelete,
  ReadOnly: Boolean);
Begin
  fStoredData := DataSet;
  caption := aCaption;
  edit1.text := fStoredData.Description;
  edit1.ReadOnly := ReadOnly;
  edit2.text := fStoredData.UserName;
  edit2.ReadOnly := ReadOnly;
  edit3.text := fStoredData.PlattformURL;
  edit3.ReadOnly := ReadOnly;
  edit4.Text := fStoredData.Password;
  edit4.ReadOnly := ReadOnly;
  Edit4Change(Nil);
  edit6.Text := fStoredData.Email;
  edit6.ReadOnly := ReadOnly;
  memo1.Text := fStoredData.Comment;
  memo1.ReadOnly := ReadOnly;
  Button1.Enabled := Not ReadOnly;
  Button2.Visible := Not ReadOnly;
  Button4.Visible := ShowDelete;
  Constraints.MinWidth := Memo1.Width + Memo1.Left + 10;
  Constraints.MaxWidth := Memo1.Width + Memo1.Left + 10;
  InitSpecialCharacters();
  SenderFormWhereMouseIs(self);
End;

Procedure TForm2.Init_second(Const DataSet: TDataSet; ReadOnly: Boolean);
Begin
  fStoredData2 := DataSet;
  edit8.text := fStoredData2.Description;
  edit8.ReadOnly := ReadOnly;
  edit9.text := fStoredData2.UserName;
  edit9.ReadOnly := ReadOnly;
  edit10.text := fStoredData2.PlattformURL;
  edit10.ReadOnly := ReadOnly;
  edit11.Text := fStoredData2.Password;
  edit11.ReadOnly := ReadOnly;
  edit12.Text := fStoredData2.Email;
  edit12.ReadOnly := ReadOnly;
  memo2.Text := fStoredData2.Comment;
  memo2.ReadOnly := ReadOnly;
  Constraints.MinWidth := Memo2.Width + Memo2.Left + 10;
  Constraints.MaxWidth := Memo2.Width + Memo2.Left + 10;
  SenderFormWhereMouseIs(self);
End;

Function TForm2.GetData(): TDataSet;
Begin
  result := fStoredData; // TDataSet hat Unsichtbare Felder die werden sonst nicht gesetzt ;)
  result.Description := Edit1.Text;
  result.UserName := Edit2.Text;
  result.PlattformURL := Edit3.Text;
  result.Password := Edit4.Text;
  result.Email := Edit6.Text;
  result.Comment := Memo1.Text;
End;

End.

