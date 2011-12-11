function OnExit(Sender)
{
  ShowMessage('OnExit');
}

function main()
{
   var c = MainForm.Controls[0].Name;


   MainForm.Edit1.Text = 'Sample text';
   MainForm.Edit1.OnExit = OnExit;
   //MainForm.testjs();
   //MainForm[10000] = 'aaa';
   MainForm.Caption = 'Dummy cvapto '+c;

   App.testCall();
   ShowMessage('Test message');

}
