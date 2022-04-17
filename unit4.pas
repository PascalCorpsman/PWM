Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm4.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

End.

