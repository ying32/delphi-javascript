unit Unit12;

interface

uses
  Windows, Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, js15decl,jsintf, StdCtrls;

type
  [JSClassName('App')]
  TJSAppObject = class(TJSClass)
  public
    procedure testCall;

  end;

  TForm12 = class(TForm)
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FJSEngine: TJSEngine;
    FJSAppObject: TJSAppObject;
  public
    { Public declarations }
    procedure testjs;

    [JSExclude()]
    procedure testjsother;
  end;

  TJSGlobalFunctions = class
    class procedure ShowMessage(s: string);
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.FormCreate(Sender: TObject);
begin
  FJSEngine := TJSEngine.Create;
  FJSEngine.registerGlobalFunctions(TJSGlobalFunctions);
  FJSEngine.registerClasses([TEdit, TForm, TCheckBox]);
  FJSAppObject:= TJSAppObject.CreateJSObject(FJSEngine, 'App') ;
  TJSClass.CreateJSObject(Self, FJSEngine, 'MainForm', [cfaInheritedMethods, cfaInheritedProperties]);
//  FJSEngine.Evaluate(TJSScript.LoadScript('test.js'), ':ApplicationInitScript:');
  FJSEngine.EvaluateFile('test.js');
  FJSEngine.CallFunction('main');
end;

procedure TForm12.FormDestroy(Sender: TObject);
begin
  FJSEngine.free;
end;


procedure TForm12.testjs;
var s: string;
begin
  s := '';
  s := 'asdsda';
end;

procedure TForm12.testjsother;
begin
//
end;

{ TJSGlobalFunctions }

class procedure TJSGlobalFunctions.ShowMessage(s: string);
begin
  Dialogs.ShowMessage(s);
end;

{ TJSAppObject }

procedure TJSAppObject.testCall;
begin
  showMessage('testCall');
end;

end.
