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
Unit unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure InitWith(Const list: TStringList);

  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Select database';
  RadioGroup1.Caption := '';
End;

Procedure TForm9.InitWith(Const list: TStringList);
Var
  i: Integer;
Begin
  RadioGroup1.Items.Clear;
  For i := 0 To list.Count - 1 Do Begin
    RadioGroup1.Items.Add(list[i]);
  End;
  If RadioGroup1.Items.Count <> 0 Then RadioGroup1.ItemIndex := 0;
End;

End.

