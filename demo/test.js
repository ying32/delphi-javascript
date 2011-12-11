const
   fmOpenRead = 0;
   fmCreate   = 0xFF00;

   fmOpenWrite = 1;
   fmOpenReadWrite =2;
   fmExclusive = 4;


function OnExit(Sender)
{
  ShowMessage('OnExit');
  Sender.Text  = 'OnExit fired...';
}

function main()
{
   var c = MainForm.Controls[0].Name;



   var file = new TFileStream('d:\\temp\\n.txt', fmOpenRead);
   delete file;

   MainForm.Edit1.Text = 'Sample text';
   MainForm.Edit1.OnExit = OnExit;
   MainForm.Caption = 'Dummy cvapto '+c;

   App.testCall();
   ShowMessage('Test message');

}
