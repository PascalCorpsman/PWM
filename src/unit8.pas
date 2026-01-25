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
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit6: TEdit;
    Edit7: TEdit;
    GroupBox2: TGroupBox;
    Label6: TLabel;
    Label7: TLabel;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fUser: String;

  public
    Procedure Init(aUser: String);

  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

{ TForm8 }

Uses usslconnector, upwm;

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Options';
End;

Procedure TForm8.Init(aUser: String);
Begin
  fuser := aUser;
End;

Procedure TForm8.Button10Click(Sender: TObject);
Var
  pw: String;
Begin
  pw := PasswordBox('', 'Please enter password for server:');
  If trim(pw) = '' Then Begin
    showmessage('Error, invalid password.');
  End;
  If Login(edit6.text, edit7.text, ClientID, fuser, pw) Then Begin
    showmessage('Successfully logged in as: ' + fuser);
  End;
  logout;
End;

Procedure TForm8.Button11Click(Sender: TObject);
Var
  OldPW, NewPW: String;
Begin
  OldPW := PasswordBox('Old password', 'Please enter old password');
  If OldPW = '' Then exit;
  NewPW := PasswordBox('New password', 'Please enter New password');
  If NewPW = '' Then exit;
  If OldPW = NewPW Then Begin
    showmessage('Error, no change.');
    exit;
  End;
  If trim(NewPW) = '' Then Begin
    showmessage('Error, new password is only space empty.');
    exit;
  End;
  If Login(Edit6.text, edit7.text, ClientID, fuser, OldPW) Then Begin
    If SetPassword(NewPW) Then Begin
      showmessage('Password change succeed.');
    End
    Else Begin
      showmessage('Failed to set new password.');
    End;
  End
  Else Begin
    showmessage('Login failed.');
  End;
  Logout;
End;

End.

