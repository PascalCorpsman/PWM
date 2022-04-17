Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, CheckLst,
  upwm;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fDatabase: TPWM;
    fDatasets: TDataSetArray;
  public
    Procedure InitData(Const DataBase: TPWM);

  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses unit6, unit7, LCLType;

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Usermanagement';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  // Add User
  form6.Label1.Caption := 'Enter password for: ' + label3.caption;
  form6.Edit1.Text := '';
  form6.Edit2.Text := '';
  form6.Edit3.Text := '';
  form6.Edit2Change(Nil);
  form6.fDatabase := fDatabase;
  If form6.ShowModal = mrOK Then Begin
    InitData(fDatabase);
  End;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm5.Button3Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no user selected.');
    exit;
  End;
  If fDatabase.delUser(ListBox1.Items[ListBox1.ItemIndex], CheckBox1.Checked) Then Begin
    InitData(fDatabase);
  End
  Else Begin
    ShowMessage(fDatabase.LastError);
  End;
End;

Procedure TForm5.Button4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // All
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := true;
  End;
End;

Procedure TForm5.Button5Click(Sender: TObject);
Var
  i: Integer;
Begin
  // None
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    CheckListBox1.Checked[i] := false;
  End;
End;

Procedure TForm5.Button6Click(Sender: TObject);
Var
  c, i: integer;
  s: String;
Begin
  // Remove Rights
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no user selected.');
    exit;
  End;
  c := 0;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      inc(c);
      break;
    End;
  End;
  If c = 0 Then Begin
    showmessage('Error, no datasets to edit selected.');
    exit;
  End;
  If ListBox1.Items[ListBox1.ItemIndex] = Label3.Caption Then Begin
    If ID_no = application.MessageBox('Removing rights for current user.' + LineEnding + LineEnding + 'Datasets will instant not accessable anymore. Proceed ?', 'Warning', mb_yesno Or mb_iconwarning) Then Begin
      exit;
    End;
  End;
  s := '';
  For i := 0 To high(fDatasets) Do Begin
    If CheckListBox1.Checked[i] Then Begin
      If Not fDatabase.RemoveUser(fDatasets[i], ListBox1.Items[ListBox1.ItemIndex]) Then Begin
        If s <> '' Then Begin
          s := s + LineEnding;
        End;
        s := s + fDatasets[i].Description;
      End;
    End;
  End;
  If s <> '' Then Begin
    ShowMessage('The user access rights could not be removed from:' + LineEnding + s);
  End;
  i := ListBox1.ItemIndex;
  InitData(fDatabase);
  ListBox1.ItemIndex := i;
End;

Procedure TForm5.Button7Click(Sender: TObject);
Var
  c, i: Integer;
Begin
  // Add Rights
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no user selected.');
    exit;
  End;
  c := 0;
  For i := 0 To CheckListBox1.Items.Count - 1 Do Begin
    If CheckListBox1.Checked[i] Then Begin
      inc(c);
      break;
    End;
  End;
  If c = 0 Then Begin
    showmessage('Error, no datasets to edit selected.');
    exit;
  End;
  For i := 0 To high(fDatasets) Do Begin
    If CheckListBox1.Checked[i] Then Begin
      fDatabase.AddUser(fDatasets[i], ListBox1.Items[ListBox1.ItemIndex]);
    End;
  End;
  i := ListBox1.ItemIndex;
  InitData(fDatabase);
  ListBox1.ItemIndex := i;
End;

Procedure TForm5.Button8Click(Sender: TObject);
Var
  u: TUser;
Begin
  // Change Password
  form7.Edit1.Text := '';
  form7.Edit2.Text := '';
  form7.Edit3.Text := '';
  If Form7.ShowModal = mrOK Then Begin
    u.Password := form7.Edit1.Text;
    u.UserName := fDataBase.AktualUser;
    If fDatabase.ChangePassword(u, form7.Edit2.Text) Then Begin
      showmessage('Successfully changed password.');
    End
    Else Begin
      showmessage(fDatabase.LastError);
    End;
  End;
End;

Procedure TForm5.InitData(Const DataBase: TPWM);
Var
  sl, l: TStringList;
  i: Integer;
Begin
  fDatabase := DataBase;
  ListBox1.Clear;
  sl := fDataBase.getUserList;
  For i := 0 To sl.count - 1 Do Begin
    ListBox1.Items.Add(sl[i]);
  End;
  sl.free;
  label3.Caption := fDataBase.AktualUser;
  CheckListBox1.Clear;
  fDatasets := fDataBase.LocateDataSets('');
  For i := 0 To high(fDatasets) Do Begin
    l := fDataBase.getUsersFor(fDatasets[i]);
    CheckListBox1.Items.Add(fDatasets[i].Description + ' [' + l.CommaText + ']');
    l.free;
    CheckListBox1.Checked[CheckListBox1.Count - 1] := false;
  End;
  button1.Enabled := fDataBase.AktualUserRights = urAdminRights;
  Checkbox1.Enabled := fDataBase.AktualUserRights = urAdminRights;
  button3.Enabled := fDataBase.AktualUserRights = urAdminRights;
End;

End.

