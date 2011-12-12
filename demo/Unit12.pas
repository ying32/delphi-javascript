unit Unit12;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, js15decl,jsintf, StdCtrls;

type

  [JSClassName('App')]
  TJSAppObject = class(TJSClass)
  public
    procedure testCall;

  end;

  TForm12 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
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

  // This is been used for regsitering delphi procedures @ JS Global context
  // all class methods will be registered, anything else will be ignored

  TJSGlobalFunctions = class
    class procedure ShowMessage(s: string);
    class function FileExists(n: string): boolean;
  end;

var
  Form12: TForm12;

implementation

{$R *.dfm}

procedure TForm12.FormCreate(Sender: TObject);
begin
  FJSEngine := TJSEngine.Create;
  FJSEngine.registerGlobalFunctions(TJSGlobalFunctions);
  FJSEngine.registerClasses([TEdit, TForm, TLabel, TCheckBox, TFileStream]);
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

class function TJSGlobalFunctions.FileExists(n: string): boolean;
begin
  Result := SysUtils.FileExists(n)
end;

class procedure TJSGlobalFunctions.ShowMessage(s: string);
begin
  Dialogs.ShowMessage(s);
end;

{ TJSAppObject }

procedure TJSAppObject.testCall;
begin
  Form12.Memo1.Lines.Add('testCall');
end;

end.
