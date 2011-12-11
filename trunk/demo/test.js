/* BE WARNED JAVASCRIPT IS CASE SENSITIVE */
const
   fmOpenRead = 0;
   fmCreate   = 0xFF00;

   fmOpenWrite = 1;
   fmOpenReadWrite =2;
   fmExclusive = 4;

   fileName = 'd:\\temp\\n.txt';

function OnExit(Sender)
{
  ShowMessage('OnExit');
  Sender.Text  = 'OnExit fired...';
}

function main()
{
   var c = MainForm.Controls[0].Name;


   if (FileExists(fileName))
   {
      var file = new TFileStream(fileName, fmOpenRead);
      delete file;

   }

   MainForm.Edit1.Text = 'Sample text';
   // OnExit event
   // Events supported is all TNotify events and keyevents
   MainForm.Edit1.OnExit = OnExit;
   MainForm.Caption = 'Dummy cvapto '+c;

   App.testCall();
   ShowMessage('Test message');

}
