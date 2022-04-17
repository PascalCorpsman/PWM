Unit Unit6;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, upwm;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Procedure Button2Click(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    fDatabase: TPWM;

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Create user';
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm6.Edit2Change(Sender: TObject);
Begin
  label3.caption := 'Enter password for ' + edit2.text + ':';
End;

Procedure TForm6.Button2Click(Sender: TObject);
Var
  ur: UInt8;
  pw2: String;
Begin
  If RadioButton1.Checked Then Begin
    ur := urNoRights;
  End
  Else Begin
    ur := urAdminRights;
  End;
  pw2 := PasswordBox('Password verification', 'Please repeat password for user: ' + Edit2.Text);
  If pw2 = Edit3.Text Then Begin
    If fDatabase.addUser(Edit1.Text, Edit2.Text, edit3.text, ur) Then Begin
      modalresult := mrOK;
    End
    Else Begin
      showmessage(fDatabase.LastError);
    End;
  End
  Else Begin
    Showmessage('Error, password verification differs from first given password, no change.');
  End;
End;

End.

