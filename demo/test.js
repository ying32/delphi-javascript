function main()
{
   var c = MainForm.Controls[0].Name;

   MainForm.Edit1.Text = 'Sample text';
   //MainForm.testjs();
   //MainForm[10000] = 'aaa';
   MainForm.Caption = 'Dummy cvapto '+c;

   App.testCall();
   ShowMessage('Test message');

}
