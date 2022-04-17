Unit Unit7;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Button1Click(Sender: TObject);
    procedure Edit3KeyPress(Sender: TObject; var Key: char);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  caption := 'Password change';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Begin
  If Edit2.Text = '' Then Begin
    showmessage('Error, no password set.');
    exit;
  End;
  If Edit2.Text <> edit3.text Then Begin
    showmessage('Error, new password and reentered password differ');
    exit;
  End;
  If Edit1.Text = Edit2.Text Then Begin
    showmessage('Error, old passord is the same as the new password.');
    exit;
  End;
  ModalResult := mrOK;
End;

procedure TForm7.Edit3KeyPress(Sender: TObject; var Key: char);
begin
  if key = #13 then Button1.Click;
end;

End.

