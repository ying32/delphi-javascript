{.$FINITEFLOAT OFF}
unit jsintf;
//{.$define MOZILLA_1_8_BRANCH}
{$ifdef ver230}
//    IMPLEMENT RTTI.GetIndexedProperties
{$endif}
interface

uses Classes, {ptrarray, namedarray,} TypInfo, js15decl, RTTI,
  Generics.Collections, SysUtils, Windows, Controls, syncObjs, JSDbgServer, forms,dialogs;

const
  global_class: JSClass = (name: 'global'; flags: JSCLASS_HAS_PRIVATE; addProperty: JS_PropertyStub;
    delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_PropertyStub;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub; finalize: JS_FinalizeStub);

type
  PWord = ^Word;
  PInteger = ^Integer;

  JSClassType = (ctDate, ctArray, ctString, ctNumber, ctBoolean, ctUnknownClass, ctUnknownType);

  PNativeMethod = ^TNativeMethod;

  TNativeMethod = record
    Name: String; // Method name
    Obj: TObject; // Object containing method
    JS: PJSObject; // Corresponding JS object
  end;

  PBridgeChar = PWideChar;
  PBridgeData = ^TBridgeData;

  TBridgeData = record
    container: Pointer;
    data: Pointer;
  end;

  TJSBase = class;
  TJSObject = class;
  TJSEngine = class;

  // Use from parameters for getting native jsparameters
  TJSNativeCallParams = record
    cx: PJSContext;
    jsobj: PJSObject;
    argc: uintN;
    argv: pjsval;
    // rval: pjsval;
  end;

  // JS RTTI Attributes
  JSClassNameAttribute = class(TCustomAttribute)
  private
    FClassName: string;
  public
    constructor Create(ClassName: string);
  end;

  // JS RTTI Attributes
  TJSClassFlagAttributes = set of (cfaInheritedMethods, cfaInheritedProperties, cfaOwnObject, cfaGlobalObject);

  JSClassFlagsAttribute = class(TCustomAttribute)
  private
    FClassFlags: TJSClassFlagAttributes;
  public
    constructor Create(flags: TJSClassFlagAttributes);
  end;

  // JS RTTI Attributes for method,properties names
  JSNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(Name: string);
  end;

  JSCtorAttribute = class(TCustomAttribute)
  end;

  JSExcludeAttribute = class(TCustomAttribute)
  end;

  TJSEventData = class
  protected
    fjsfuncobj: PJSObject;
    feventName: string;
    fobj: TObject;
    fcx: PJSContext;
  public
    constructor Create(ajsfuncobj: PJSObject; aeventname: string; aobj: TObject; acx: PJSContext);
  end;

  TJSClassProto = class(TObject)
  private
    function getJSClassName: string;
  protected
    FJSCtor: TRttiMethod;
    FJSClass: JSExtendedClass;
    FClass: TClass;
    Fclass_methods: array of JSFunctionSpec;
    Fclass_props: array of JSPropertySpec;
    FConsts: array of JSConstDoubleSpec;
    Fctx: TRttiContext;
    FRttiType: TRttiType;
    FJSClassProto: PJSObject;
    FMethodNamesMap: TStringList;
    FClassFlags: TJSClassFlagAttributes;
    FEventsCode: TObjectDictionary<string, TJSEventData>;

    procedure DefineJSClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    // Used from Engine.registerClass;
    procedure JSInitClass(AEngine: TJSEngine);
    function CreateNativeObject(AClass: TClass): TObject;
  public
    constructor Create(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    destructor Destroy; override;

    property JSClassName: string read getJSClassName;
    property JSClassProto: PJSObject read FJSClassProto;
  end;

  TJSClass = class
  private

    Fjsobj: PJSObject;
    FNativeObj: TObject;
    FJSObject: TJSObject;

    //class function PropWrite(Obj: TJSClass; cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; static;
  protected
    FClassProto: TJSClassProto;
    FJSEngine: TJSEngine;

    class function JSMethodCall(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
      static;
    class function JSPropWrite(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; static;
    class function JSPropRead(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; static;

    class function JSPropReadClass(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; static;

{$IF CompilerVersion >= 23}
    class function JSIndexedPropRead(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool; cdecl; static;
{$ifend}

    class function GetParamName(cx: PJSContext; id: jsval): string;

    class function JSObjectCtor(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
      static;
    class procedure JSObjectDestroy(cx: PJSContext; Obj: PJSObject); cdecl; static;

    // Events
    procedure JSNotifyEvent(Sender: TObject);
    procedure JSKeyEvent(Sender: TObject; var Key: Word;  Shift: TShiftState);
    procedure JSKeyPressEvent(Sender: TObject; var Key: Char);

  public
    constructor Create; virtual;
    constructor CreateJSObject(Instance: TObject; AEngine: TJSEngine; JSObjectName: string = '';
      AClassFlags: TJSClassFlagAttributes = []); overload; virtual;
    constructor CreateJSObject(AEngine: TJSEngine; JSObjectName: string = ''); overload; virtual;
    destructor Destroy; override;

    class function TValueToJSVal(cx: PJSContext; Value: TValue): jsval; overload;
    class function JSValToTValue(cx: PJSContext; t: PTypeInfo; vp: jsval): TValue; overload;
    class function JSArgsToTValues(params: TArray<TRttiParameter>; cx: PJSContext; jsobj: PJSObject; argc: uintN;
      argv: pjsval): TArray<TValue>; overload;

    class function JSDateToDateTime(JSEngine: TJSEngine; oDate: PJSObject; var dDate: TDateTime): boolean;
    class function JSEngine(cx: PJSContext): TJSEngine; overload;
    class function JSPrintf(JSEngine: TJSEngine; const fmt: String; argc: Integer; args: pjsval): String;

    procedure NewJSObject(Engine: TJSEngine; JSObjectName: string = ''; AInstance: TObject = nil;
      AClassFlags: TJSClassFlagAttributes = []); overload; virtual;
    procedure FreeJSObject(Engine: TJSEngine);
    function JSEngine: TJSEngine; overload;
    property JSObject: PJSObject read Fjsobj;

  end;

  TJSDebuggerScripts = TDictionary<string, string>;
  TJSEngine = class
  private
    type
      TJSMethod = record
         method_class: TClass;
         ctx: TRttiContext;
         RttiType: TRttiType;
         delphiName: string;
         method: TRttiMethod;
         params: TArray<TRttiParameter>;
         CodeAddress: Pointer;
      end;
  var
    farrayclass: PJSClass;
    fbooleanclass: PJSClass;
    fclasses: Array of JSClass;
    fcx: PJSContext;
    fdateclass: PJSClass;
    fglobal: PJSObject;
    fnativeglobal: TJSObject;
//    fnatives: TPtrArray;
    fnumberclass: PJSClass;
    fptrs: Array of Pointer;
    frt: PJSRuntime;
    fstackSize: Cardinal;
    fstringclass: PJSClass;
    fvarcount: Integer;
    fvarlist: Array of TJSBase;

    FMethodNamesMap: TDictionary<string, TJSMethod>;
    Fclass_methods: array of JSFunctionSpec;

    FDebugging: boolean;
    FDebugger: TJSDebugServer;
    FDebuggerScripts: TJSDebuggerScripts;

    procedure AddPtr(const Ptr: Pointer);
    procedure AddVar(Obj: TJSBase);
    procedure DeleteClasses;
    procedure DeletePtrs;
    procedure DeleteVarList;

    function Compile(const Code: String; const FileName: String = ''): PJSScript;
    function Execute(Script: PJSScript; Scope: TJSObject = nil): boolean; overload;

    procedure GetStandardClasses;
    procedure GrowVarList;
    function InternalCall(const Func: PJSFunction; Obj: PJSObject; var args: Array of TJSBase; rval: pjsval): boolean;
    function InternalCallName(const Func: String; Obj: PJSObject; var args: Array of TJSBase;
      rval: pjsval): boolean;
    function InternalGet(const name: String; Obj: PJSObject; var rval: jsval): boolean;
    procedure RemoveVar(Obj: TJSBase);
    function GetVersion: String;
    procedure SetDebugging(const Value: boolean);
  protected
    FDelphiClasses: TDictionary<string, TJSClassProto>;
    class function JSMethodCall(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool; cdecl;
      static;
  public

    constructor Create(MaxMemory: Cardinal = 64 * 1024 * 1024); virtual;
    destructor Destroy; override;


    procedure registerClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    procedure registerClasses(AClass: array of TClass; AClassFlags: TJSClassFlagAttributes = []);

    procedure registerGlobalFunctions(AClass: TClass);
    function Declare(val: Integer; const name: String ): boolean; overload;
    function Declare(val: Double; const name: String ): boolean; overload;
    function Declare(const val: String; const name: String): boolean; overload;
    function Declare(val: boolean; const name: String): boolean; overload;

    function EvaluateFile(const FileName: String; Scope: TJSObject = NIL): boolean; overload;

    function Evaluate(const Code: String; Scope: TJSObject; scriptFileName: AnsiString = ''): boolean; overload;
    function Evaluate(const Code: String; Scope: TJSObject; var rval: jsval; scriptFileName: AnsiString = ''): boolean; overload;
    function Evaluate(const Code: String; scriptFileName: AnsiString = ''): boolean; overload;
    function Evaluate(const Code: String; var rval: jsval; scriptFileName: AnsiString = ''): boolean; overload;

    function callFunction(functionName: AnsiString; var rval: jsval): boolean; overload;
    function callFunction(functionName: AnsiString): boolean; overload;

    procedure GarbageCollect;
    procedure lockObject(Obj: PJSObject; name: ansistring = ''); overload;
    procedure lockObject(Obj: TJSObject; name: ansistring = ''); overload;
    procedure unlockObject(Obj: TJSObject); overload;
    procedure unlockObject(Obj: PJSObject); overload;
    function IsExceptionRaised: boolean;
    function IsValidCode(const Code: String): boolean;

    function NewJSObject: TJSObject; overload;
    function NewJSObject(const name: String): TJSObject; overload;
    function NewJSObject(const name: String; parent: TJSObject): TJSObject; overload;

    procedure SetErrorReporter(proc: JSErrorReporter);

    property ArrayClass: PJSClass read farrayclass;
    property BooleanClass: PJSClass read fbooleanclass;
    property Context: PJSContext read fcx;
    property Runtime: PJSRuntime read frt;
    property DateClass: PJSClass read fdateclass;
    property Global: TJSObject read fnativeglobal;
    property NumberClass: PJSClass read fnumberclass;
    property StringClass: PJSClass read fstringclass;
    property Version: String read GetVersion;

    property Debugging: boolean read FDebugging write SetDebugging;
    property DebuggerScripts: TJSDebuggerScripts read FDebuggerScripts;
    property Debugger: TJSDebugServer read FDebugger;
  end;

  (*
    * These were initially set up to be ref-counted, but that may be more effort than its worth.
    * On the other hand, it may open up thread safety for a single TJSBase to be used within
    * multiple threads.  Need to do more reading to see if this is correct or not :)
    *)
  TJSBase = class
  protected
    FConnected: boolean;
    FDestroying: boolean;
    FEngine: TJSEngine;
    FJSVal: jsval;
    FName: String;
    FRefCnt: Integer;
    FScope: TJSObject;

    procedure AddRef;
    function CanGoLive: boolean; virtual;
    procedure InternalConnect; virtual;
    function IsLive: boolean; virtual;
    procedure SetConnected;
    procedure SetEngine(const Value: TJSEngine);
    procedure SetName(const Value: String);
    procedure SetScope(const Value: TJSObject);
  public
    constructor Create(AEngine: TJSEngine; AName: String); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Connect(AEngine: TJSEngine; AName: String; AParent: TJSObject); overload;
    procedure Connect(AEngine: TJSEngine; AName: String); overload;
    function ToString: String;

    property Connected: boolean read FConnected;
    property Destroying: boolean read FDestroying write FDestroying;
    property Engine: TJSEngine read FEngine write SetEngine;
    property JScriptVal: jsval read FJSVal;
    property JSName: String read FName write SetName;
    property parent: TJSObject read FScope write SetScope;

  end;

  TJSObject = class(TJSBase)
  protected
    Fjsobj: PJSObject;
//    fnatives: TPtrArray;

    procedure CheckConnection;
    procedure InternalConnect; override;
  public
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String); overload;
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String; AParent: TJSObject); overload;
    destructor Destroy; override;

    function AddMethods(var methods: TJSFunctionSpecArray): boolean;
    function AddNativeObject(Obj: TObject; const InstanceName: String): TJSObject;

    function AddProperties(var props: TJSPropertySpecArray): boolean;
    function Call(const Func: String; params: Array of TJSBase; var str: String): boolean; overload;
    function Call(const Func: String; params: Array of TJSBase; var int: Integer): boolean; overload;
    function Call(const Func: String; params: Array of TJSBase; var dbl: Double): boolean; overload;
    function Call(const Func: String; params: Array of TJSBase; var res: TJSObject): boolean; overload;
    function Call(const Func: String; params: Array of TJSBase; var bool: boolean): boolean; overload;
    function ClassType(const Name: String): JSClassType;
    function Declare(val: Double; const name: String ): boolean; overload;
    function Declare(val: Integer; const name: String ): boolean; overload;
    function Declare(const val: String; const name: String): boolean; overload;
    function Declare(val: boolean; const name: String ): boolean; overload;
    function enumerate: TArray<string>;
    function Evaluate(const Code: String; scriptFileName: AnsiString = ''): boolean; overload;
    function Evaluate(const Code: String; var rval: jsval; scriptFileName: AnsiString = ''): boolean; overload;
    function Compile(const Code: String; scriptFileName: AnsiString = ''): PJSScript;
    function Execute(script: PJSScript; rval: Pjsval = NIL): boolean;


    function getProperty(const name: String; var dbl: Double): boolean; overload;
    function getProperty(const name: String; var int: Integer): boolean; overload;
    function getProperty(const name: String; var ret: TJSObject): boolean; overload;
    function getProperty(const name: String; var str: String): boolean; overload;
    function getProperty(const name: String; var bool: boolean): boolean; overload;
    function HasProperty(const name: String): boolean;
    function IsFunction(const name: String): boolean;
    function IsInteger(const name: String): boolean;
    procedure RemoveObject(Obj: TJSBase);
    function setProperty(const name: String; val: TJSBase): boolean; overload;
    function setProperty(const name: String; val: TValue): boolean; overload;
    function TypeOf(const name: String): JSType;

    property JSObject: PJSObject read Fjsobj write Fjsobj;

  end;

  TJSScript = class
  private
    FCompiled: boolean;
    FScript: PJSScript;
    FEngine: TJSEngine;
    FCode: String;
    FFileName: string;
  protected
    procedure Compile(const AFileName: string = ''); virtual;
  public
    constructor Create; overload;virtual;
    constructor Create(AEngine: TJSEngine; const ACode: String; const AFileName: string = ''); overload; virtual;
    destructor Destroy; override;

    class function LoadScript(fileName: string): string; overload;
    class function LoadScript(Stream: TStream): string; overload;

    procedure Execute(AScope: TJSObject); overload;
    procedure Execute(); overload;
    // Streaming
    procedure LoadCompiled(const AFile: String);
    procedure LoadCompiledFromStream(AStream: TStream);
    procedure LoadRaw(const AFile: String);
    procedure SaveCompiled(const AFile: String);
    procedure SaveCompiledToStream(AStream: TStream);
    procedure SaveRaw(const AFile: String);

    property Code: String read FCode write FCode;
    property Compiled: boolean read FCompiled;
    property Script: PJSScript read FScript;
  end;

implementation

uses Math, ActiveX, DateUtils;

const
  NilMethod: TMethod = (Code: nil; data: nil);

Type
  pjsval_ptr = ^jsval_array;
  jsval_array = array [0 .. 256] of jsval;
  TJSIndexedProperty = class(TJSClass)
  public
    parentObj: TObject;
    propName: string;
  end;

function strdup(s: AnsiString): PAnsiChar;
begin
  GetMem(Result, Length(s) + 1);
  strCopy(Result, PAnsiChar(s));
  // move(PAnsiChar(s)^, Result^, Length(s)+1);
end;

function GetGUID: AnsiString;
Var
 GUID : TGUID;
 psz : array[0..256] of widechar;
begin
  CoCreateGuid(guid);
  StringFromGUID2(guid, @psz, 256);
  Result := POleStr(@psz);
end;

function generateScriptName: string;
begin
   Result := '(inline)' + GetGUID + '.js'
end;

procedure Debug(s: string);
begin
  OutputDebugString(PChar(s));
end;

procedure IntfBridge_ErrorReporter(cx: PJSContext; message: PAnsiChar; report: PJSErrorReport); cdecl;
var
  fileName, msg: String;
  o: integer;
begin
  // if (report^.flags and JSREPORT_EXCEPTION <> 0) then  // ignore js-catchable exceptions
  // exit;

  if report.filename <> nil then
     fileName := extractFileName(report.filename)
  else
     fileName := 'typein:';

  o := pos(report.uctokenptr, report.uclinebuf);

  msg := format('%s'#13#10'%s'#13#10'%s^'#13#10'Filename: %s'#13#10'Line: %d', [message, trim(report.uclinebuf), stringofchar(' ', o),fileName, report.lineno+1]);
  MessageBox(0, pchar(msg), pchar('Javascript error'), MB_ICONERROR or MB_OK);
{  msg := 'Notice type: ';
  if (report^.flags and JSREPORT_WARNING <> 0) then
    msg := msg + 'Warning'
  else
    msg := msg + 'Error';

  msg := msg + #10 + 'Message: ' + message + #10'Line: ' + IntToStr(report^.lineNo);
 }
end;

function GetParamName(cx: PJSContext; id: jsval): String;
begin
  Result := JS_GetStringChars(JS_ValueToString(cx, id));
end;

{ TJSEngine }

procedure TJSEngine.AddPtr(const Ptr: Pointer);
begin
  SetLength(fptrs, Length(fptrs) + 1);
  fptrs[Length(fptrs) - 1] := Ptr;
end;

procedure TJSEngine.AddVar(Obj: TJSBase);
begin
  if (fvarcount >= Length(fvarlist)) then
    GrowVarList;
  fvarlist[fvarcount] := Obj;
  Inc(fvarcount);
end;

function TJSEngine.callFunction(functionName: AnsiString): boolean;
var
  rval: jsval;
begin
  Result := callFunction(functionName, rval);

end;

function TJSEngine.callFunction(functionName: AnsiString; var rval: jsval): boolean;
var
  r: Integer;
  vp, argv: jsval;
begin

  Result := false;
  if JS_LookupProperty(context, Global.JSObject, PAnsiChar(functionName), @vp) = js_true then
  begin
    if (not JSValIsVoid(vp)) and (JSValIsObject(vp)) then
    begin
        r := JS_CallFunctionValue(context, Global.JSObject, vp, 0, @argv, @rval);
        Result := true;

    end;
  end;

end;

function TJSEngine.Compile(const Code: String; const FileName: String): PJSScript;
var
 name: UTF8String;
begin
  if FileName = '' then name := generateScriptName
  else name := FileName;

  // Register script source
  if FDebugging then
  begin
     FDebuggerScripts.AddOrSetValue(name, Code);
  end;

  Result := JS_CompileUCScript(fcx, fglobal, PWideChar(Code), Length(Code), PansiChar(Name), 0);
end;

constructor TJSEngine.Create(MaxMemory: Cardinal);
var
  d: word;
{$ifdef CPUX64}
//  em: TArithmeticExceptionMask;
{$endif}
begin
{$ifdef CPUX64}
//  ClearExceptions(false);
//  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
  //ClearExceptions(false);
  //em := GetExceptionMask();
//  mm := GetMasked8087Exceptions;
  fstackSize := 8192;
  frt := JS_NewRuntime(MaxMemory);
  //frt := JS_Init(MaxMemory);

  fcx := JS_NewContext(frt, fstackSize);
  //mm := GetMasked8087Exceptions;

//  JS_SetGCCallback(fcx, _JS_GCCallback);

  JS_SetRuntimePrivate(frt, self);
  fglobal := JS_NewObject(fcx, @global_class, nil, nil);
  JS_SetErrorReporter(fcx, IntfBridge_ErrorReporter);

//  fnatives := TPtrArray.Create;
//  fnatives.OwnsValues := true;

  //Set8087CW(524319);
  fnativeglobal := TJSObject.Create(fglobal, self, '');
  //ClearPending8087Exceptions;
  //d := Get8087ControlWord and $3F;
  //mm := GetMasked8087Exceptions;
  //SetMasked8087Exceptions([], true);
//  mm := GetMasked8087Exceptions;
  JS_InitStandardClasses(fcx, fglobal);

  GetStandardClasses;
  FDelphiClasses := TDictionary<string, TJSClassProto>.Create;
  FDebuggerScripts := TJSDebuggerScripts.Create;
  FMethodNamesMap:= TDictionary<string, TJSMethod>.create;
end;

function TJSEngine.Declare(val: Integer; const name: String): boolean;
begin
  result := Global.SetProperty(name, val);
end;

function TJSEngine.Declare(val: Double; const name: String ): boolean;
begin
  result := Global.SetProperty(name, val);
end;

function TJSEngine.Declare(const val: String; const name: String ): boolean;
begin
  result := Global.SetProperty(name, val);
end;

function TJSEngine.Declare(val: boolean; const name: String ): boolean;
begin
  result := Global.SetProperty(name, val);
end;

procedure TJSEngine.DeleteClasses;
begin
  SetLength(fclasses, 0);
end;

procedure TJSEngine.DeletePtrs;
var
  i: Integer;
begin
  for i := 0 to Length(fptrs) - 1 do
    Dispose(fptrs[i]);
  SetLength(fptrs, 0);
end;

procedure TJSEngine.DeleteVarList;
var
  i: Integer;
begin
  for i := 0 to Length(fvarlist) - 1 do
    if (fvarlist[i] <> nil) then
    begin
      fvarlist[i].Destroying := true;
      fvarlist[i].Free;
    end;
  SetLength(fvarlist, 0);
end;

destructor TJSEngine.Destroy;
begin
  JS_DestroyContext(fcx);
  JS_DestroyRuntime(frt);
//  JS_Finish(frt);

//  fnatives.Free;
  fnativeglobal.Free;

  DeletePtrs;
  DeleteClasses;
  DeleteVarList;
  FDelphiClasses.Free;
  FDebuggerScripts.Free;
  FMethodNamesMap.free;
  inherited;
end;

function TJSEngine.Evaluate(const Code: String; Scope: TJSObject; scriptFileName: AnsiString): boolean;
begin
  Result := Scope.Evaluate(Code, scriptFileName);
end;

function TJSEngine.Evaluate(const Code: String; scriptFileName: AnsiString): boolean;
begin
  Result := Evaluate(Code, fnativeglobal, scriptFileName);
end;

function TJSEngine.Evaluate(const Code: String; var rval: jsval; scriptFileName: AnsiString): boolean;
begin
  Result := Evaluate(Code, fnativeglobal, rval, scriptFileName);
end;


function TJSEngine.EvaluateFile(const FileName: String; Scope: TJSObject): boolean;
var
  filenameutf8: UTF8String;
  code: string;
  rval: jsval;
begin
  if Scope = nil then Scope := FNativeGlobal;

  filenameutf8 := FileName;
  code := TJSScript.LoadScript(fileName);
  if FDebugging then
     FDebuggerScripts.AddOrSetValue(FileName, Code);

  Result := JS_EvaluateUCScript(Context, FNativeGlobal.Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(filenameutf8), 0, @rval) = 1;
end;

function TJSEngine.Evaluate(const Code: String; Scope: TJSObject; var rval: jsval; scriptFileName: AnsiString): boolean;
begin
  Result := Scope.Evaluate(Code, rval, scriptFileName);

end;

function TJSEngine.Execute(Script: PJSScript; Scope: TJSObject): boolean;
var
  rval: jsval;
begin
  if (Scope = nil) then
      Scope := fnativeglobal;
  Result := (JS_ExecuteScript(fcx, Scope.JSObject, Script, @rval) = JS_TRUE);
end;

procedure TJSEngine.GarbageCollect;
begin
  JS_GC(fcx);
//  JS_MaybeGC(fcx);
end;

procedure TJSEngine.GetStandardClasses;
var
  Obj: PJSObject;
  v: jsval;

  function Eval(const str: String): jsval;
  var
    v : jsval;
  begin
    JS_EvaluateUCScript(fcx, fglobal, PWideChar(str), Length(str), nil, 0, @v);
    Result := v;
  end;

begin

  JS_EvaluateUCScript(fcx, fglobal, PWideChar('Date.prototype'), Length('Date.prototype'), nil, 0, @v);
  v := Eval('Date.prototype');
  Obj := JSValToObject(v);
  fdateclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Array.prototype'));
  farrayclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Boolean.prototype'));
  fbooleanclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('String.prototype'));
  fstringclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Number.prototype'));
  fnumberclass := JS_GetClass(Obj);
end;

function TJSEngine.GetVersion: String;
begin
  Result := JS_GetImplementationVersion;
end;

procedure TJSEngine.GrowVarList;
begin
  SetLength(fvarlist, Length(fvarlist) + 16);
end;

function TJSEngine.InternalCall(const Func: PJSFunction; Obj: PJSObject; var args: Array of TJSBase;
  rval: pjsval): boolean;
var
  myargs: TArray<jsval>;
  i: Integer;
begin
  if (Obj = nil) then
    Obj := fglobal;

  if (Length(args) = 0) then
    Result := (JS_CallFunction(fcx, Obj, Func, 0, nil, rval) = JS_TRUE)
  else
  begin
    SetLength(myargs, Length(args));
    for i := 0 to Length(args) - 1 do
      myargs[i] := args[i].JScriptVal;

    Result := (JS_CallFunction(fcx, Obj, Func, Length(myargs), @myargs[0], rval) = JS_TRUE);
    SetLength(myargs, 0);
  end;
end;

function TJSEngine.InternalCallName(const Func: String; Obj: PJSObject; var args: array of TJSBase;
  rval: pjsval): boolean;
var
  fval: jsval;
begin
  JS_GetUCProperty(fcx, Obj, PWideChar(Func), Length(Func), @fval);

  Result := InternalCall(JS_ValueToFunction(fcx, fval), Obj, args, rval);
end;

function TJSEngine.InternalGet(const name: String; Obj: PJSObject; var rval: jsval): boolean;
begin
  if (Obj = nil) then
      Obj := fglobal;
  Result := (JS_GetUCProperty(fcx, Obj, PWideChar(name), Length(name), @rval) = JS_TRUE);
end;

function TJSEngine.IsExceptionRaised: boolean;
begin
  Result := (JS_IsExceptionPending(fcx) = JS_TRUE);
end;

function TJSEngine.IsValidCode(const Code: String): boolean;
begin
  Result := (JS_BufferIsCompilableUnit(fcx, fglobal, PAnsiChar(AnsiString(Code)), Length(Code)) = JS_TRUE);
end;

class function TJSEngine.JSMethodCall(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool;
var
  methodName: string;
  eng : TJSEngine;
  method: TJSMethod;
  ctx: TRttiContext;
  RttiType: TRttiType;
  m: TRttiMethod;
  params:  TArray<TRttiParameter>;
  args:  TArray<TValue>;
  methodResult: TValue;
begin
{$POINTERMATH ON}
  Result := JS_FALSE;
  methodName := GetParamName(cx, argv[-2]);
  Delete(methodName, 1, Length('function '));
  Delete(methodName, pos('(', methodName), Length(methodName));
//  FMethodNamesMap.
  Eng := TJSClass.JSEngine(cx);
  if  eng.FMethodNamesMap.TryGetValue(methodName, method) then
  begin
     ctx := method.ctx;
     RttiType := method.RttiType;
     //RttiType.GetMethod(methodName);
//     method.method.Invoke(Method.Class, )
     //Ctx.Invoke(
      m := Method.Method;
      params := m.GetParameters;

      try
        args := TJSClass.JSArgsToTValues(params, cx, jsobj, argc, argv);
        methodResult := m.Invoke(Method.method_class, args);
        if methodResult.Kind <> tkUnknown then
          rval^ := TJSClass.TValueToJSVal(cx, methodResult);

      except
        on e: Exception do
        begin
          Result := JS_FALSE;
          JS_ReportError(cx, PAnsiChar(AnsiString('Exception: ' + e.message)), nil);
        end
      end;
      // Break since inherited/virtual methods will be called
      Result := JS_TRUE;
  end;
{$POINTERMATH OFF}
end;

procedure TJSEngine.lockObject(Obj: PJSObject; name: ansistring );
var
  r: JSBool;
begin
  {
   Do not pass a pointer to a JS double, string, or object—rp must be either a pointer to a pointer variable
   or a pointer to a jsval variable.
  }

  if name <> '' then
     r := JS_AddNamedRoot(Context, @Obj, PansiChar(name))
  else
     r := JS_AddRoot(Context, @Obj)
end;

procedure TJSEngine.lockObject(Obj: TJSObject; name: ansistring );
begin
  lockObject(Obj.JSObject, name);
end;

procedure TJSEngine.unlockObject(Obj: PJSObject);
var
  r: JSBool;
begin
  r := JS_RemoveRoot(Context, @Obj);

end;

procedure TJSEngine.unlockObject(Obj: TJSObject);
begin
  unlockObject(Obj.JSObject);
end;

function TJSEngine.NewJSObject: TJSObject;
begin
  Result := TJSObject.Create(nil, self, '');
end;

function TJSEngine.NewJSObject(const name: String): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name);
end;

function TJSEngine.NewJSObject(const name: String; parent: TJSObject): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name, parent);
end;

procedure TJSEngine.registerClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
var
  p: TJSClassProto;
begin

  if FDelphiClasses.ContainsKey(AClass.ClassName) then
     exit;

  p := TJSClassProto.Create(AClass, AClassFlags);
  p.JSInitClass(self);
  FDelphiClasses.Add(p.JSClassName, p);

end;

procedure TJSEngine.registerClasses(AClass: array of TClass; AClassFlags: TJSClassFlagAttributes);
var
  i: Integer;
begin
  for i := 0 to high(AClass) do
    registerClass(AClass[i], AClassFlags);

end;

procedure TJSEngine.registerGlobalFunctions(AClass: TClass);
var
  ctx: TRttiContext;
  RttiType: TRttiType;
  m: TRttiMethod;
  exclude: boolean;
  methodName: string;
  a: TCustomAttribute;
  methods: TJSFunctionSpecArray;
  method: TJSMethod;

begin
  // setter.Invoke(TClass(Instance), argsV)
  methods := nil;
  ctx := TRttiContext.Create;
  RttiType := ctx.GetType(AClass);
  for m in RttiType.GetMethods do
  begin
    methodName := m.name;
    method.Ctx := ctx;
    method.RttiType := RttiType;
    method.method_class := aclass;
    method.delphiName := methodName;
    method.method := m;
    method.params := m.GetParameters;

    // cfaInheritedMethods, cfaInheritedProperties
    if (m.Visibility < mvPublic) or (m.parent <> RttiType) or (not m.IsClassMethod) then continue;

    Exclude := false;
    for a in m.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
        Exclude := true;

      if (a is JSNameAttribute) then
         methodName := JSNameAttribute(a).FName;

    end;


    if Exclude then
      continue;

    FMethodNamesMap.Add(methodName, method);
    SetLength(methods, Length(methods) + 1);
    methods[ high(methods)].name := strdup(methodName);
    methods[ high(methods)].Call := @TJSEngine.JSMethodCall;
    methods[ high(methods)].nargs := Length(m.GetParameters);
    methods[ high(methods)].flags := 0;
    methods[ high(methods)].extra := 0;

  end;

  if methods <> nil then
  begin
    SetLength(methods, Length(methods) + 1);
    JS_DefineFunctions(Context, fglobal, @methods[0]);
  end;

end;

procedure TJSEngine.RemoveVar(Obj: TJSBase);
var
  i: Integer;
begin
  for i := 0 to Length(fvarlist) - 1 do
    if (fvarlist[i] = Obj) then
    begin
      fvarlist[i] := nil;
      exit;
    end;
end;

procedure TJSEngine.SetDebugging(const Value: boolean);
begin
  if FDebugging = Value then exit;

  if (not FDebugging) and Value then
  begin
     FDebugger := TJSDebugServer.Create(fcx);
  end
  else if not Value then
  begin
     if Assigned(FDebugger) then
     begin
        FDebugger.Free;
        FDebugger := NIL;
        FDebuggerScripts.Clear;
     end;

  end;

  FDebugging := Value;
end;

procedure TJSEngine.SetErrorReporter(proc: JSErrorReporter);
begin
  JS_SetErrorReporter(fcx, proc);
end;

{ TJSBase }

procedure TJSBase.AddRef;
begin
  Inc(FRefCnt);
end;

function TJSBase.CanGoLive: boolean;
begin
  Result := (FName <> '') and (FScope <> nil) and (FScope.Connected);
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: String; AParent: TJSObject);
begin
  Engine := AEngine;
  parent := AParent;
  JSName := AName;
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: String);
begin
  Engine := AEngine;
  parent := AEngine.Global;
  JSName := AName;
end;

constructor TJSBase.Create(AEngine: TJSEngine; AName: String);
begin
  Engine := AEngine;
  JSName := AName;
  parent := FEngine.Global;
end;

constructor TJSBase.Create;
begin
  FEngine := nil;
  FScope := nil;
end;

destructor TJSBase.Destroy;
var
  rval: jsval;
begin
  if (FEngine <> nil) and (not FDestroying) then
    try
      FEngine.RemoveVar(self); // only for ref-counting, and we'll probably move to something else soon
      if (FName <> '') and (FScope <> nil) then
        JS_DeleteUCProperty2(FEngine.Context, FScope.JSObject, PWideChar(FName), Length(FName), @rval);
    except
    end;
end;

procedure TJSBase.InternalConnect;
begin
end;

function TJSBase.IsLive: boolean;
begin
  (*
    * This may not be the fastest way to determine whether the property already exists in FScope.
    *)
  // Result := (FScope <> nil) and FScope.HasProperty(FName);
  Result := false;
end;

procedure TJSBase.SetConnected;
begin
  FConnected := (FEngine <> nil);
  if (FConnected) then
    InternalConnect;
end;

procedure TJSBase.SetEngine(const Value: TJSEngine);
begin
  FEngine := Value;
  SetConnected;
end;

procedure TJSBase.SetName(const Value: String);
begin
  FName := Value;
  SetConnected;
end;

procedure TJSBase.SetScope(const Value: TJSObject);
begin
  if (FEngine <> nil) and (Value = nil) then
    FScope := FEngine.Global
  else
    FScope := Value;
  SetConnected;
end;

function TJSBase.ToString: String;
begin
  Result := GetParamName(FEngine.Context, FJSVal);
end;

{ TJSObject }

function TJSObject.AddMethods(var methods: TJSFunctionSpecArray): boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |methods| must be blank *)
  len := Length(methods);
  SetLength(methods, len + 1);
  FillChar(methods[len], SizeOf(JSFunctionSpec), #0);

  Result := (JS_DefineFunctions(FEngine.Context, Fjsobj, @methods[0]) = JS_TRUE);
end;

function TJSObject.AddNativeObject(Obj: TObject; const InstanceName: String): TJSObject;
var
  p: TJSClassProto;
begin
  CheckConnection;

  p := TJSClassProto.Create(Obj.ClassType);
  // p.JSInitClass(FEngine);
  // FDelphiClasses.Add(p.JSClassName, p);
end;

(* function TJSObject.AddObject(var cls: JSClass; const AName: String): TJSObject;
  var
  jsobj: PJSObject;
  begin
  CheckConnection;
  jsobj := JS_DefineObject(FEngine.Context, FJSObj, CreateAnsiString(AName), @cls, nil, JSPROP_ENUMERATE);
  Result := TJSObject.Create(jsobj, FEngine, AName, self);
  end;
*)
function TJSObject.AddProperties(var props: TJSPropertySpecArray): boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |props| must be blank *)
  len := Length(props);
  SetLength(props, len + 1);
  FillChar(props[len], SizeOf(JSPropertySpec), #0);

  Result := (JS_DefineProperties(FEngine.Context, Fjsobj, @props[0]) = JS_TRUE);
end;

function TJSObject.Call(const Func: String; params: Array of TJSBase; var str: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, Fjsobj, params, @rval)) then
  begin
    str := '';
    exit;
  end;

  str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
  UniqueString(str);
end;

function TJSObject.Call(const Func: String; params: Array of TJSBase; var dbl: Double): boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, Fjsobj, params, @rval)) then
  begin
    dbl := JSVAL_NULL;
    exit;
  end;

  if (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl);
end;

function TJSObject.Call(const Func: String; params: Array of TJSBase; var int: Integer): boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, Fjsobj, params, @rval)) then
  begin
    int := JSVAL_NULL;
    exit;
  end;

  int := JSValToInt(rval);
end;

function TJSObject.Call(const Func: String; params: Array of TJSBase; var bool: boolean): boolean;
var
  rval: jsval;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, Fjsobj, params, @rval)) then
  begin
    bool := false;
    exit;
  end;

  bool := JSValToBoolean(rval);
end;

function TJSObject.Call(const Func: String; params: Array of TJSBase; var res: TJSObject): boolean;
var
  rval: jsval;
  p: PJSObject;
  t: Pointer;
begin
  CheckConnection;

  Result := false;
  if (not FEngine.InternalCallName(Func, Fjsobj, params, @rval)) then
  begin
    res := nil;
    exit;
  end;

  (* !!!
    * This isn't complete yet.  We need to query the object's parent and name to make this work.
    *)
  JS_ValueToObject(FEngine.Context, rval, p);
  t := JS_GetPrivate(FEngine.Context, p);
  if (t <> nil) then
    res := TJSObject(t)
  else
    res := TJSObject.Create(p, FEngine, '');
end;

procedure TJSObject.CheckConnection;
begin
  if (not FConnected) then
    raise Exception.Create('Connection to TJSEngine instance expected.  Assign Engine property of TJSObject instance.');
end;

function TJSObject.ClassType(const Name: String): JSClassType;
var
  rval: jsval;
  cls: PJSClass;
begin
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(Name), Length(Name), @rval);
  if (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_OBJECT) then
  begin
    cls := JS_GetClass(JSValToObject(rval));
    Result := ctUnknownClass;
  end
  else
  begin
    cls := nil;
    Result := ctUnknownType;
  end;

  if (cls = FEngine.farrayclass) then
    Result := ctArray
  else if (cls = FEngine.fdateclass) then
    Result := ctDate
  else if (cls = FEngine.fbooleanclass) then
    Result := ctBoolean
  else if (cls = FEngine.fnumberclass) then
    Result := ctNumber
  else if (cls = FEngine.fstringclass) then
    Result := ctString
  else
    case JS_TypeOfValue(FEngine.Context, rval) of
      JSTYPE_STRING:
        Result := ctString;
      JSTYPE_BOOLEAN:
        Result := ctBoolean;
      JSTYPE_NUMBER:
        Result := ctNumber;
    end;
end;

function TJSObject.Compile(const Code: String; scriptFileName: AnsiString ): PJSScript;
begin
  // Result := JS_EvaluateScript(FEngine.Context, FJSObj, PWideChar(code), Length(code), nil, 0, @rval) = 1;
  Result := JS_CompileUCScript(FEngine.Context, Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(scriptFileName), 0);
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String);
begin
  Fjsobj := AValue;
  FJSVal := JSObjectToJSVal(Fjsobj);

  Engine := AEngine;
  if (AEngine <> nil) then
    parent := FEngine.Global; // Set this before we
  JSName := AName;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String; AParent: TJSObject);
begin
  Fjsobj := AValue;
  FJSVal := JSObjectToJSVal(Fjsobj);

  Engine := AEngine;
  JSName := AName;
  parent := AParent;
end;

function TJSObject.Declare(const val: String; const name: String ): boolean;
begin
  result := SetProperty(name, val);
end;

function TJSObject.Declare(val: Integer; const name: String): boolean;
begin
  result := SetProperty(name, val);
end;

function TJSObject.Declare(val: Double; const name: String): boolean;
begin
  result := SetProperty(name, val);
end;

function TJSObject.Declare(val: boolean; const name: String ): boolean;
begin
  result := SetProperty(name, val);
end;

destructor TJSObject.Destroy;
begin
//  fnatives.Free;
  inherited;
end;

function TJSObject.enumerate: TArray<string>;
var
  list: PJSIdArray;
  curid: pjsid;
  val: jsval;
  i: Integer;
begin
  CheckConnection;
  list := JS_Enumerate(FEngine.Context, Fjsobj);
  curid := @list^.vector;

  SetLength(Result, list^.Length);
  for i := 0 to list^.Length - 1 do
  begin
    JS_IdToValue(FEngine.Context, curid^, @val);
    Result[i] := String(JS_GetStringChars(JS_ValueToString(FEngine.Context, val)));
    Inc(curid);
  end;
end;

function TJSObject.Evaluate(const Code: String; var rval: jsval; scriptFileName: AnsiString): boolean;
begin
  CheckConnection;
  // Result := false;

  if scriptFileName = '' then scriptFileName := generateScriptName;

  if FEngine.FDebugging then
  begin
     FEngine.FDebuggerScripts.AddOrSetValue(scriptFileName, Code);
  end;

  Result := JS_EvaluateUCScript(FEngine.Context, Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(scriptFileName), 0, @rval) = 1;

end;

function TJSObject.Execute(script: PJSScript; rval: Pjsval): boolean;
var
  val: jsval;
begin
 if rval = nil then rval := @val;
  
 Result := JS_ExecuteScript(FEngine.Context, Fjsobj, script, rval) = js_true;
end;

function TJSObject.Evaluate(const Code: String; scriptFileName: AnsiString): boolean;
var
  rval: jsval;
begin
  Result := Evaluate(Code, rval, scriptFileName);
end;

function TJSObject.getProperty(const name: String; var int: Integer): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToInt32(FEngine.Context, rval, @int)
  else
    int := JSVAL_NULL;
end;

function TJSObject.getProperty(const name: String; var dbl: Double): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl)
  else
    dbl := JSVAL_NULL;
end;

function TJSObject.getProperty(const name: String; var ret: TJSObject): boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    JS_ValueToObject(FEngine.Context, rval, p);
    (* !!!
      * This is wasteful.  We need to figure out how to find existing wrappers
      * for instance |p|.
      *)
    ret := TJSObject.Create(p, FEngine, name, self);
  end
  else
    ret := nil;
end;

function TJSObject.getProperty(const name: String; var bool: boolean): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
    bool := JSValToBoolean(rval)
  else
    bool := false;
end;

function TJSObject.getProperty(const name: String; var str: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (rval <> JSVAL_NULL) then
  begin
    str := JS_GetStringChars(JS_ValueToString(FEngine.Context, rval));
    UniqueString(str);
  end
  else
    str := '';
end;

function TJSObject.HasProperty(const name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  Result := (rval <> JSVAL_VOID);
end;

procedure TJSObject.InternalConnect;
begin
  if (not IsLive) and (CanGoLive) then
  begin
    JS_RemoveRoot(FEngine.Context, @FJSVal);
    FScope.setProperty(FName, self);
  end;
end;

function TJSObject.IsFunction(const name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  if (rval <> JSVAL_VOID) then
    Result := (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_FUNCTION)
  else
    Result := false;
end;

function TJSObject.IsInteger(const name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  if (rval <> JSVAL_VOID) then
    Result := JSValIsInt(rval)
  else
    Result := false;
end;

procedure TJSObject.RemoveObject(Obj: TJSBase);
var
  parent: PJSObject;
  rval: jsval;
begin
  CheckConnection;
  parent := Obj.parent.JSObject;
  JS_DeleteUCProperty2(FEngine.Context, parent, PWideChar(Obj.JSName), Length(Obj.JSName), @rval);
  Obj.Free;
end;

function TJSObject.setProperty(const name: String; val: TValue): boolean;
var
 v: jsval;
begin
  v := TJSClass.TValueToJSVal(FEngine.Context, val);
  if (HasProperty(name)) then
      Result := (JS_SetUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @v) = JS_TRUE)
  else
    Result := (JS_DefineUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), v, nil,
        nil, {JSPROP_READONLY or }JSPROP_ENUMERATE) = JS_TRUE);

end;

function TJSObject.setProperty(const name: String; val: TJSBase): boolean;
begin
  CheckConnection;
  if (HasProperty(name)) then
    Result := (JS_SetUCProperty(FEngine.Context, Fjsobj, CreateWideString(name), Length(name),
        @val.JScriptVal) = JS_TRUE)
  else
    Result := (JS_DefineUCProperty(FEngine.Context, Fjsobj, CreateWideString(name), Length(name), val.JScriptVal, nil,
        nil, JSPROP_ENUMERATE) = JS_TRUE);
end;

function TJSObject.TypeOf(const name: String): JSType;
var
  rval: jsval;
begin
  CheckConnection;
  if (FEngine.InternalGet(name, Fjsobj, rval)) then
    Result := JS_TypeOfValue(FEngine.Context, rval)
  else
    Result := JSTYPE_VOID;
end;

{ TJSScript }

procedure TJSScript.Compile(const AFileName: string );
begin
  FScript := FEngine.Compile(FCode, AFileName);
  FCompiled := FScript <> nil;
end;

constructor TJSScript.Create;
begin
  FCode := '';
  FScript := nil;
end;

constructor TJSScript.Create(AEngine: TJSEngine; const ACode: String; const AFileName: string);
begin
  FEngine := AEngine;
  FCode := ACode;
  FFileName := AFileName;
  Compile(FFileName);
end;

destructor TJSScript.Destroy;
begin
  if assigned(FEngine) and assigned(FScript) then
  begin
     JS_DestroyScript(FEngine.Context, FScript);
  end;
  inherited;
end;

procedure TJSScript.Execute;
begin
  Execute(FEngine.Global);
end;

procedure TJSScript.Execute(AScope: TJSObject);
var
  scriptObj: PJSObject;
begin
  if (not FCompiled) then
    Compile();

  if FScript <> nil then
  begin
     // // Create object based on it so we can root it
     //scriptObj := JS_NewScriptObject(FEngine.Context, fscript);
     //if JS_AddRoot(FEngine.Context, @scriptObj) = js_true then
     //begin
        FEngine.Execute(FScript, AScope);
        // Remove rooting, allowing GC to take place
     //   JS_RemoveRoot(FEngine.Context, scriptObj);
     //end;
  end;
end;

procedure TJSScript.LoadCompiled(const AFile: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmOpenRead);
  try
    LoadCompiledFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.LoadCompiledFromStream(AStream: TStream);
var
  ms: TMemoryStream;
  xdr: PJSXDRState;
  data: PWideChar;
  len: size_t;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(AStream);

    ms.Position := 0;
    data := ms.Memory;
    len := ms.Size;

    xdr := JS_XDRNewMem(FEngine.Context, JSXDR_DECODE);
    if (xdr <> nil) then
    begin
      JS_XDRMemSetData(xdr, data, len);
      JS_XDRScript(xdr, FScript);
    end;

    FCompiled := true;
    FCode := '';
  finally
    ms.Free;
  end;
end;

procedure TJSScript.LoadRaw(const AFile: String);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.LoadFromFile(AFile);
    FCode := s.Text;

    FCompiled := false;
    FScript := nil;
  finally
    s.Free;
  end;
end;

class function TJSScript.LoadScript(Stream: TStream): string;
begin
  with TStreamReader.Create(Stream, TEncoding.UTF8, true) do
    try
      Result := ReadToEnd;
    finally
      Free;
    end;
end;

class function TJSScript.LoadScript(fileName: string): string;
var
  Stream: TFileStream;
begin
  Stream:= TFileStream.Create(fileName, fmOpenRead  or fmShareDenyWrite);
  Result := LoadScript(Stream);
  Stream.Free;
{  with TStreamReader.Create(fileName, TEncoding.UTF8, true) do
    try
      Result := ReadToEnd;
    finally
      Free;
    end;
}
end;

procedure TJSScript.SaveCompiled(const AFile: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  try
    SaveCompiledToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.SaveCompiledToStream(AStream: TStream);
var
  xdr: PJSXDRState;
  data: Pointer;
  len: size_t;
begin
  if (not FCompiled) then
    Compile();

  xdr := JS_XDRNewMem(FEngine.Context, JSXDR_ENCODE);
  if (xdr <> nil) and (JS_XDRScript(xdr, FScript) = JS_TRUE) then
  begin
    data := JS_XDRMemGetData(xdr, @len);
    AStream.Write(data^, len);
  end
  else
    raise Exception.Create('The compiled script code may be corrupted; unable to save it to disk.');
end;

procedure TJSScript.SaveRaw(const AFile: String);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := FCode;
    s.SaveToFile(AFile);
  finally
    s.Free;
  end;
end;

{ JSClassNameAttribute }

constructor JSClassNameAttribute.Create(ClassName: string);
begin
  FClassName := ClassName;
end;

{ TJSClassProto }

function TJSClassProto.CreateNativeObject(AClass: TClass): TObject;
var
  t: TRttiType;
  m: TRttiMethod;
  methodResult: TValue;
  args: TArray<TValue>;
begin
  // constructor Create(AOwner: TComponent); override;
  t := Fctx.GetType(AClass);
  for m in t.GetMethods do
  begin
    if m.IsConstructor and (Length(m.GetParameters) = 0) then
    begin
      args := nil;
      methodResult := m.Invoke(AClass, args);
      exit(methodResult.AsObject);
    end;

  end;

  Result := AClass.Create;
end;

constructor TJSClassProto.Create(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
begin
  FClassFlags := AClassFlags;
  Fclass_methods := nil;
  Fclass_props := nil;
  FConsts := nil;
  FMethodNamesMap := TStringList.Create;
  FEventsCode := TObjectDictionary<string, TJSEventData>.Create([doOwnsValues]);

  DefineJSClass(AClass, AClassFlags);

end;

procedure TJSClassProto.DefineJSClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
var

  clName: AnsiString;
  methodName: string;
//  pt: TRttiType;
  p: TRttiProperty;
{$IF CompilerVersion >= 23}
  ip: TRttiIndexedProperty;
{$ifend}
  m: TRttiMethod;
  f: TRttiField;
  // a: TRttiParameter;

  jsobj: PJSObject;
  jsp: PJSObject;
  i: Integer;
  Enums: TDictionary<string, Integer>;
  a: TCustomAttribute;
  Exclude: boolean;
  clFlags: TJSClassFlagAttributes;
  defaultCtor: TRttiMethod;

  clctx: TRttiContext;
  clt: TRttiType;

  procedure defineEnums(pt: TRttiType);
  var
    i: Integer;
    st: TRttiSetType;
    ot: TRttiOrdinalType;
    ename: string;
    proc: TIntToIdent;
    procedure defineEnum(ename: string; v: Integer);
    begin

      if Enums.ContainsKey(ename) then
        exit;

      Enums.Add(ename, v);
      SetLength(FConsts, Length(FConsts) + 1);
      FConsts[ high(FConsts)].dval := v;
      FConsts[ high(FConsts)].name := strdup(ename);
      FConsts[ high(FConsts)].flags := JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;
    end;

  begin
    // FindIntToIdent(pt.handle);
    if pt.Handle^.Kind = tkEnumeration then
    begin
      ot := pt.AsOrdinal;
      for i := ot.MinValue to ot.MaxValue do
      begin
        defineEnum( { pt.Name + '_' + } GetEnumName(pt.Handle, i), i);
      end;
    end
    else if pt.IsSet then
    begin
      st := pt.AsSet;
      pt := st.ElementType;
      if pt.IsOrdinal then
      begin
        ot := pt.AsOrdinal;
        for i := ot.MinValue to ot.MaxValue do
        begin
          defineEnum(
            { pt.Name + '_' + } GetEnumName(pt.Handle, i), 1 shl i);
        end;
      end;

    end
    else if pt.Handle^.Kind = tkInteger then
    begin
      // proc := FindIntToIdent(pt.handle) ;
    end;
  end;

begin

  Enums := TDictionary<string, Integer>.Create;
  FClass := AClass;
  Fclass_methods := NIL;
  Fclass_props := NIL;
  FConsts := nil;

  Fctx := TRttiContext.Create;
  FRttiType := Fctx.GetType(AClass);
  // FRttiType := FRttiType;
  if FRttiType = NIL then
    raise Exception.Create('Fatal: RttiContext.getClass failed');

  clName := FRttiType.Name;
  clFlags := AClassFlags;

  for a in FRttiType.GetAttributes do
  begin
    if a is JSClassNameAttribute then
      clName := JSClassNameAttribute(a).FClassName
    else if a is JSClassFlagsAttribute then
      clFlags := JSClassFlagsAttribute(a).FClassFlags;
  end;

  for m in FRttiType.GetMethods do
  begin

    // m.GetAttributes
    Exclude := false;
    methodName := m.name;
    // cfaInheritedMethods, cfaInheritedProperties
    for a in m.GetAttributes do
    begin
      if (a is JSCtorAttribute) and m.IsConstructor then
      begin
        FJSCtor := m;
      end;

      if (a is JSExcludeAttribute) then
        Exclude := true;

      if (a is JSNameAttribute) then
      begin
        methodName := JSNameAttribute(a).FName;
        FMethodNamesMap.Values[JSNameAttribute(a).FName] := m.Name;
      end;
    end;

    // Default js ctor for tcomponent inherited objects
    if m.IsConstructor and (Length(m.GetParameters) = 1) and
      (m.GetParameters[0].ParamType.Handle = TypeInfo(TComponent)) then
    begin
      defaultCtor := m;
    end;

    if (m.parent <> FRttiType) and (not(cfaInheritedMethods in clFlags)) then
      Exclude := true;

    if Exclude or m.IsConstructor or m.IsDestructor or m.IsStatic or m.IsClassMethod or
      (not(m.MethodKind in [mkProcedure, mkFunction])) or (m.Visibility < mvPublic) then
      continue;

    SetLength(Fclass_methods, Length(Fclass_methods) + 1);
    Fclass_methods[ high(Fclass_methods)].name := strdup(methodName);
    Fclass_methods[ high(Fclass_methods)].Call := @TJSClass.JSMethodCall;
    Fclass_methods[ high(Fclass_methods)].nargs := Length(m.GetParameters);
    Fclass_methods[ high(Fclass_methods)].flags := 0;
    Fclass_methods[ high(Fclass_methods)].extra := 0;

  end;

  // Set default CTOR for TComponent parents
  if FJSCtor = nil then
     FJSCtor := defaultCtor;

  // Support only integer indexed properties
{$IF CompilerVersion >= 23}

  for ip in FRttiType.GetIndexedProperties do
  begin
    if (ip.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
      continue;

    Exclude := false;
    for a in ip.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        Exclude := true;
        break;
      end;
    end;

    if Exclude or (Length(ip.ReadMethod.GetParameters) = 0) or (ip.Visibility < mvPublic)then
      continue;

    if (ip.ReadMethod.GetParameters[0].ParamType.Handle <> TypeInfo(Integer))  then
       continue;

    { TODO : FIXME }
    if high(Fclass_props) = 255 then break;
    SetLength(Fclass_props, Length(Fclass_props) + 1);
    Fclass_props[ high(Fclass_props)].Flags := JSPROP_READONLY or JSPROP_ENUMERATE or JSPROP_PERMANENT;
    Fclass_props[ high(Fclass_props)].name := strdup(ip.Name);
    Fclass_props[ high(Fclass_props)].getter := @TJSClass.JSIndexedPropRead;
    Fclass_props[ high(Fclass_props)].tinyid := High(Fclass_props) - 127;
  end;

{$ifend}

  for f in FRttiType.GetFields do
  begin

    if (f.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
       continue;

    Exclude := false;
    for a in f.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        Exclude := true;
        break;
      end;
    end;

    if Exclude or (f.Visibility < mvPublic) then
      continue;

    defineEnums(f.FieldType);

    { TODO : FIXME }
    if high(Fclass_props) = 255 then break;

    SetLength(Fclass_props, Length(Fclass_props) + 1);
    Fclass_props[ high(Fclass_props)].Flags := JSPROP_ENUMERATE or JSPROP_PERMANENT  or JSPROP_READONLY;
    Fclass_props[ high(Fclass_props)].name := strdup(f.Name);
    Fclass_props[ high(Fclass_props)].getter := @TJSClass.JSPropRead;
    Fclass_props[ high(Fclass_props)].tinyid := High(Fclass_props) - 127;

  end;

  for p in FRttiType.GetProperties do
  begin

    if (p.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
      continue;

    Exclude := false;
    for a in p.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        Exclude := true;
        break;
      end;
    end;

    if Exclude or (p.Visibility < mvPublic) then
      continue;

    defineEnums(p.PropertyType);

    { TODO : FIXME }
    if high(Fclass_props) = 255 then break;
    SetLength(Fclass_props, Length(Fclass_props) + 1);
    Fclass_props[ high(Fclass_props)].Flags := JSPROP_ENUMERATE or JSPROP_PERMANENT;
    Fclass_props[ high(Fclass_props)].name := strdup(p.Name);
    if p.IsReadable then
      Fclass_props[ high(Fclass_props)].getter := @TJSClass.JSPropRead;

    if p.IsWritable then
      Fclass_props[ high(Fclass_props)].setter := @TJSClass.JSPropWrite
    else
      Fclass_props[ high(Fclass_props)].Flags := Fclass_props[ high(Fclass_props)].Flags or JSPROP_READONLY;

    Fclass_props[ high(Fclass_props)].tinyid := High(Fclass_props) - 127;

  end;

  // NULL terminate array
  SetLength(Fclass_props, Length(Fclass_props) + 1);
  SetLength(Fclass_methods, Length(Fclass_methods) + 1);
  SetLength(FConsts, Length(FConsts) + 1);

  FillChar(Fclass_props[Length(Fclass_props) - 1], SizeOf(JSPropertySpec), 0);
  FillChar(Fclass_methods[Length(Fclass_methods) - 1], SizeOf(JSFunctionSpec), 0);
  FillChar(FConsts[Length(FConsts) - 1], SizeOf(JSConstDoubleSpec), 0);

  FJSClass.Base.name := strdup(clName);
  FJSClass.Base.flags := JSCLASS_HAS_PRIVATE;
  FJSClass.Base.addProperty := JS_PropertyStub;
  FJSClass.Base.delProperty := JS_PropertyStub;
  FJSClass.Base.getProperty := TJSClass.JSPropReadClass;
  //FJSClass.Base.getProperty := JS_PropertyStub;
  FJSClass.Base.setProperty := JS_PropertyStub;
  FJSClass.Base.enumerate := JS_EnumerateStub;
  FJSClass.Base.resolve := JS_ResolveStub;
  FJSClass.Base.convert := JS_ConvertStub;
  FJSClass.Base.finalize := TJSClass.JSObjectDestroy;
  FreeAndNil(Enums);

end;

destructor TJSClassProto.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(Fclass_methods) - 1 do
    freeMem(Fclass_methods[i].name);

  for i := 0 to High(Fclass_props) - 1 do
    freeMem(Fclass_props[i].name);

  for i := 0 to High(FConsts) - 1 do
    freeMem(FConsts[i].name);

  if assigned(FJSClass.Base.name) then
    freeMem(FJSClass.Base.name);

  FMethodNamesMap.Free;
  Fctx.Free;
  FEventsCode.Free;
  inherited;
end;

function TJSClassProto.getJSClassName: string;
begin
  Result := FJSClass.Base.name;
end;

procedure TJSClassProto.JSInitClass(AEngine: TJSEngine);
var
  B: JSBool;
  ctorObj: PJSObject;
begin
  if FJSClassProto = nil then
  begin
    FJSClassProto := JS_InitClass(AEngine.Context, AEngine.Global.JSObject, nil, @FJSClass, @TJSClass.JSObjectCtor, 0,
      // @Fclass_props[0], @Fclass_methods[0], nil, nil);
      nil, nil, nil, nil);
    if FJSClassProto <> nil then
    begin
      JS_DefineProperties(AEngine.Context, FJSClassProto, @Fclass_props[0]);
      JS_DefineFunctions(AEngine.Context, FJSClassProto, @Fclass_methods[0]);

      JS_DefineConstDoubles(AEngine.Context, AEngine.Global.JSObject, @FConsts[0]);
      { ctorObj := JS_GetConstructor(AEngine.Context, FJSClassProto);
        if ctorObj <> nil then
        begin
        JS_DefineConstDoubles(AEngine.Context, ctorObj, @FConsts[0]);
        end;
        }
    end;
  end;

end;

{ TJSClass }

class function TJSClass.JSMethodCall(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool;
var
  Obj: TJSClass;
  p: Pointer;
  m: TRttiMethod;
  methodResult: TValue;
  methodName: string;
  t: TRttiType;
  args: TArray<TValue>;
  eng: TJSEngine;
  JSClassName: PAnsiChar;
  params: TArray<TRttiParameter>;
begin
{$POINTERMATH ON}
  methodName := GetParamName(cx, argv[-2]);
  Delete(methodName, 1, Length('function '));
  Delete(methodName, pos('(', methodName), Length(methodName));
  Result := JS_TRUE;

  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;

  eng := TJSClass.JSEngine(cx);

  Obj := TJSClass(p);
  // t := Obj.FClassProto.Fctx.GetType(Obj.FClassProto.FClass);
  t := Obj.FClassProto.FRttiType;

  if (Obj.FClassProto <> nil) and (Obj.FClassProto.FMethodNamesMap.Values[methodName] <> '') then
    methodName := Obj.FClassProto.FMethodNamesMap.Values[methodName];

  { TODO : Handle overloaded methods }
  for m in t.GetMethods(methodName) do
  begin
    params := m.GetParameters;

    try
      args := TJSClass.JSArgsToTValues(params, cx, jsobj, argc, argv);
      methodResult := m.Invoke(Obj.FNativeObj, args);
      if methodResult.Kind <> tkUnknown then
        rval^ := TValueToJSVal(cx, methodResult);

    except
      on e: Exception do
      begin
        Result := JS_FALSE;
        JS_ReportError(cx, PAnsiChar(AnsiString('Exception: ' + e.message)), nil);
      end
    end;
    // Break since inherited/virtual methods will be called
    break;

  end;
{$POINTERMATH OFF}
end;

procedure TJSClass.JSNotifyEvent(Sender: TObject);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: jsval;
begin
  eventData := TJSEventData(self);
  f_argv := JSObjectToJSVal(TJSClass(eventData.fobj).{FJSObject.}Fjsobj);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 1, @f_argv,
    @f_rval) = JS_TRUE then
  begin
    f_rval := 0;
  end;
  // showmessage(sender.ClassName);
  //
end;

class function TJSClass.JSPrintf(JSEngine: TJSEngine; const fmt: String; argc: Integer; args: pjsval): String;
var
  jsArgs: pjsval_ptr;
  pFmt, prev, fp, sp, p: PWideChar;
  specLen, len: Integer;
  wfmt, literal: WideString;
  nArg: Integer;
  nDecSep: wideChar;
  cFlags: wideChar;
  nPrecision, nWidth: Integer;
  padChar: ansichar;
  sepChar: AnsiString;
  FormatSettings: TFormatSettings;

  vDouble: Double;
  vInteger: Integer;
  JS: PJSString;

  function nextSpec(p: PWideChar): PWideChar;
  begin
    literal := '';

    while (p^ <> #0) and (p^ <> '%') do
    begin
      if (p^ = '\') and ((p + 1)^ in ['n', 'r', 't', 'b', '\', '%']) then
      begin
        Inc(p);
        case p^ of
          'n':
            p^ := #10;
          'r':
            p^ := #13;
          't':
            p^ := #8;
        end;
      end;

      literal := literal + p^;
      Inc(p);
    end;

    if p^ = #0 then
      Result := nil
    else
      Result := p;
  end;

begin

  jsArgs := pjsval_ptr(args);
  nArg := 0;
  Result := '';
  pFmt := PWideChar(fmt);
  prev := pFmt;
  p := nextSpec(prev);

  while (nArg < argc) and (p <> nil) do
  begin
    len := p - prev;
    { if len > 0 then
      setString(literal, prev, len)
      else
      literal := '';
      }
    // get format specifier
    Inc(p);
    nDecSep := #0;
    if p^ = ',' then
    begin
      Inc(p);
      nDecSep := p[0];
      Inc(p);
    end;
    sp := p;
    // locate first % or space character
    { repeat

      inc(p);
      until (p^ in [#0, '%', ' ', 'f', 'd', 'x', 's']) ; }
    while not(p^ in [#0, { '%', ' ', } 'f', 'd', 'x', 's']) do
    begin
      Inc(p);
    end;
    Inc(p);
    len := p - sp;
    setString(wfmt, sp, len);
    if len >= 0 then
    begin
      cFlags := #0;
      fp := sp;
      if fp[len - 1] in ['f', 'd'] then
      begin
        if fp^ in ['+', ' ', '#', '0'] then
        begin
          cFlags := fp^;
          Inc(fp);
        end;
      end;

      nWidth := 0;
      nPrecision := 0;

      // width
      sp := fp;
      while (fp^ in ['0' .. '9']) do
        Inc(fp);
      if fp <> sp then
      begin
        len := fp - sp;
        setString(wfmt, sp, len);
        nWidth := strtoIntDef(wfmt, 0);
      end;

      // precision
      if fp^ = '.' then
      begin
        Inc(fp);
        sp := fp;
        while (fp^ in ['0' .. '9']) do
          Inc(fp);
        if fp <> sp then
        begin
          len := fp - sp;
          setString(wfmt, sp, len);
          nPrecision := strtoIntDef(wfmt, 0);
        end;
      end;

      if fp^ in ['f', 'd'] then
      begin
        if cFlags = '0' then
          padChar := '0'
        else
          padChar := '#';
        sepChar := '';
        if nWidth = 0 then
          nWidth := 1;

        sepChar := ',';
        FormatSettings.ThousandSeparator := ',';
        FormatSettings.DecimalSeparator := '.';
        case nDecSep of
          '0':
            begin
              sepChar := ',';
              FormatSettings.ThousandSeparator := ',';
              FormatSettings.DecimalSeparator := '.';
            end;
          '1':
            begin
              sepChar := '';
              FormatSettings.ThousandSeparator := ',';
              FormatSettings.DecimalSeparator := '.';
            end;
          '2':
            begin
              sepChar := ',';
              FormatSettings.ThousandSeparator := '.';
              FormatSettings.DecimalSeparator := ',';
            end;
          '3':
            begin
              sepChar := '';
              FormatSettings.ThousandSeparator := '.';
              FormatSettings.DecimalSeparator := ',';
            end;
        end;

      end;

      case fp^ of
        'f', 'd':
          begin
            vDouble := JSValToDouble(JSEngine.Context, jsArgs[nArg]);

            if nPrecision > 0 then
              wfmt := sepChar + stringOfChar(padChar, nWidth) + '.' + stringOfChar('0', nPrecision)
            else
              wfmt := sepChar + stringOfChar(padChar, nWidth);

            wfmt := formatFloat(wfmt, vDouble, FormatSettings);
            if cFlags = '+' then
            begin
              if vDouble > 0 then
                wfmt := '+' + wfmt
              else
                wfmt := '-' + wfmt;
            end
            else if (cFlags = ' ') then
            begin
              if vDouble > 0 then
                wfmt := '+' + wfmt
              else
                wfmt := ' ' + wfmt;
            end;

            Result := Result + literal + wfmt;
            // Result := Result + literal + floatToStr(JSValToDouble(JSEngine.context, jsArgs[nArg]));
          end;
        { 'd':
          begin
          vInteger := JSValToInt(jsArgs[nArg]);
          Result := Result + literal + inttoStr(vInteger);
          end; }
        'x':
          begin
            Result := Result + literal + inttoHex(JSValToInt(jsArgs[nArg]), nWidth);
          end;
        's':
          begin
            JS := JS_ValueToString(JSEngine.Context, jsArgs[nArg]);
            { if JSValIsString(jsArgs[nArg]) then
              Result := Result + literal + JSStringToString(JSValToJSString(jsArgs[nArg]))
              else begin
              js := JS_ValueToString(JSEngine.Context, jsArgs[nArg]); }
            Result := Result + literal + JSStringToString(JS)
            // end;
          end;
      end;
    end;

    prev := p;
    p := nextSpec(prev);
    Inc(nArg);
  end;

  if literal <> '' then
    Result := Result + literal;

end;

class function TJSClass.JSPropRead(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiProperty;
  propIndex: Integer;
  f: TRttiField;
begin
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;
  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

  propName := Obj.FClassProto.Fclass_props[JSValToInt(id) + 127].name;
  prop := t.getProperty(propName);
  if prop <> nil then
    vp^ := TValueToJSVal(cx, prop.GetValue(Obj.FNativeObj))
  else begin
    f := t.getField(propName);
    if f <> nil then
       vp^ := TValueToJSVal(cx, f.GetValue(Obj.FNativeObj))
    else
       vp^ := JSVAL_NULL;
  end;
  Result := JS_TRUE;
end;

class function TJSClass.JSPropReadClass(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiIndexedProperty;
  propIndex: Integer;
  iObj: TJSIndexedProperty;
  v: TValue;
begin
  // propName := GetParamName(cx, id);
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;
  Obj := TJSClass(p);

  //if JSValIsString(id) then   // We have a method call
  //   propName := JSStringToString(JS_ValueToString(cx, id));

  if JSValIsint(id) and (Obj.fnativeobj is TJSIndexedProperty) then
  begin
     iObj:= TJSIndexedProperty(Obj.fnativeobj);
     obj := TJSCLASS(iObj.parentObj);
     t := Obj.FClassProto.FRttiType;
     prop := t.getIndexedProperty(iObj.propName);
     if prop <> nil then
     begin
         v := JSValToInt(id);
         v := prop.ReadMethod.Invoke(Obj.FNativeObj, [v]);
         vp^ := TValueToJSVal(cx, v);
     end;
  end;

  Result := JS_TRUE;

end;

class function TJSClass.JSPropWrite(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiProperty;
  v: TValue;
  jsfuncobj: PJSObject;
  NotifyMethod: TMethod;
  eventData: TJSEventData;
begin

  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;

  Obj := TJSClass(p);
  // t := Obj.FClassProto.Fctx.GetType(Obj.FClassProto.FClass);
  t := Obj.FClassProto.FRttiType;
  propName := Obj.FClassProto.Fclass_props[JSValToInt( id )+ 127].name;
  prop := t.getProperty(propName);
  if prop <> nil then
  begin
    if prop.PropertyType.Handle.Kind = tkMethod then
    begin
      SetMethodProp(Obj.FNativeObj, propName, NilMethod);

      if (not JSValIsNull(vp^) ) and JSValIsObject(vp^) then
      begin
        jsfuncobj := JSValToObject(vp^);
        if JS_ObjectIsFunction(cx, jsfuncobj) = JS_TRUE then
        begin

          if prop.PropertyType.Handle = TypeInfo(TNotifyEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, Obj, cx);
            NotifyMethod.Code := @TJSClass.JSNotifyEvent;
            NotifyMethod.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, NotifyMethod);
            Obj.FClassProto.FEventsCode.AddOrSetValue(propName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TKeyEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, Obj, cx);
            NotifyMethod.Code := @TJSClass.JSKeyEvent;
            NotifyMethod.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, NotifyMethod);
            Obj.FClassProto.FEventsCode.AddOrSetValue(propName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TKeyPressEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, Obj, cx);
            NotifyMethod.Code := @TJSClass.JSKeyPressEvent;
            NotifyMethod.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, NotifyMethod);
            Obj.FClassProto.FEventsCode.AddOrSetValue(propName, eventData);
          end;
        end;
      end;

    end
    else
    begin
      v := JSValToTValue(cx, prop.PropertyType.Handle, vp^);
      prop.SetValue(Obj.FNativeObj, v);
    end;
  end;

  Result := JS_TRUE;
end;

class function TJSClass.JSValToTValue(cx: PJSContext; t: PTypeInfo; vp: jsval): TValue;

var
  jsobj, jsarr: PJSObject;
  len: jsuint;
  i: Integer;
  eng: TJSEngine;
  elType: PTypeInfo;
  Values: array of TValue;
  v: TValue;
  typeData: PTypeData;
  dDate: TDateTime;
  st: TRttiSetType;
  pt: TRttiType;
  ot: TRttiOrdinalType;

  L: LongWord;
  W: Word;
  B: Byte;
  p: Pointer;
  Obj: TJSClass;
begin
  eng := TJSClass.JSEngine(cx);
  Result := TValue.Empty;
  case t^.Kind of
    tkEnumeration:
      if t = System.TypeInfo(boolean) then
        Result := JSValToBoolean(vp)
      else
      begin
        Result := Result.FromOrdinal(t, JSValToInt(vp));
      end;
    tkSet:
      begin

        case GetTypeData(t)^.OrdType of
          otSByte, otUByte:
            begin
              B := JSValToInt(vp);
              TValue.Make(@B, t, Result);
            end;
          otSWord, otUWord:
            begin
              W := JSValToInt(vp);
              TValue.Make(@W, t, Result);
            end;
          otSLong, otULong:
            begin
              L := JSValToInt(vp);
              TValue.Make(@L, t, Result);
            end;
        end;

      end;
    tkInt64, tkInteger:
      if JSValIsInt(vp) then
        Result := JSValToInt(vp);

    tkFloat:
      if JSValIsNumber(vp) then
        Result := JSValToDouble(cx, vp)
      else if (JSValIsObject(vp)) and (JS_InstanceOf(cx, JSValToObject(vp), eng.DateClass, nil) = JS_TRUE) then
      begin
        if not TJSClass.JSDateToDateTime(eng, JSValToObject(vp), dDate) then
          dDate := 0;
        Result := dDate;
      end;

    tkLString, tkWString, tkUString:
      if not JSValIsVoid(vp) then
        Result := JSStringToString(JS_ValueToString(cx, vp));

    tkClass:
      begin
        if (JSValIsObject(vp)) then
        begin
          jsobj := JSValToObject(vp);
          p := JS_GetPrivate(cx, jsobj);
          if TObject(p) is TJSClass then
          begin
            Obj := TJSClass(p);
            Result := Obj.FNativeObj;
          end;

        end;
      end;
    tkDynArray:
      begin
        if (JSValIsObject(vp)) and (JS_InstanceOf(cx, JSValToObject(vp), eng.ArrayClass, nil) = JS_TRUE) then
        begin
          typeData := GetTypeData(t);
          len := 0;
          jsarr := JSValToObject(vp);
          if JS_GetArrayLength(cx, jsarr, len) = JS_TRUE then
          begin
            SetLength(Values, len);
            for i := 0 to len - 1 do
            begin
              if JS_GetElement(cx, jsarr, i, @vp) = JS_TRUE then
              begin
                if not(JSValIsNull(vp) or JSValIsVoid(vp)) then
                begin
                  Values[i] := JSValToTValue(cx, typeData.eltype2^, vp);
                end;
              end;
            end;
            Result := TValue.FromArray(t, Values);
          end;

        end;
      end;
    tkRecord:
      begin
        if t = TypeInfo(TValue) then
        begin
          // result := TValueToJSVal(cx, Value.AsType<TValue>);
        end;

      end;
    tkMethod: // Events
      begin
        // const
        // NilMethod: TMethod = (Code: nil; Data: nil);
        TValue.Make(@NilMethod, t, Result);
        // TValue.From<TNotifyEvent>(notifyEvent);

      end;
  end;

end;

procedure TJSClass.NewJSObject(Engine: TJSEngine; JSObjectName: string; AInstance: TObject;
  AClassFlags: TJSClassFlagAttributes);
var
  B: JSBool;
  c: TClass;
begin

  if FClassProto <> NIL then
     raise Exception.Create('TJSClass.NewJSObject multiple calls');

  if AInstance = nil then
    AInstance := self;

  FJSEngine := Engine;
  c := AInstance.ClassType;

  FClassProto := TJSClassProto.Create(c, AClassFlags);

  if not TJSEngine(Engine).FDelphiClasses.ContainsKey(FClassProto.JSClassName) then
    TJSEngine(Engine).FDelphiClasses.Add(FClassProto.JSClassName, FClassProto);

  FNativeObj := AInstance;
  Fjsobj := JS_NewObject(Engine.Context, @FClassProto.FJSClass, nil, Engine.Global.JSObject);
  FJSObject := TJSObject.Create(Fjsobj, Engine, JSObjectName);
  JS_SetPrivate(Engine.Context, Fjsobj, Pointer(self));

  B := JS_DefineProperties(Engine.Context, fjsobj, @FClassProto.Fclass_props[0]);
  B := JS_DefineFunctions(Engine.Context, fjsobj, @FClassProto.Fclass_methods[0]);
  B := JS_DefineConstDoubles(Engine.Context, fjsobj, @FClassProto.FConsts[0]);

end;

class function TJSClass.TValueToJSVal(cx: PJSContext; Value: TValue): jsval;
var
  L: LongWord;
  B: Byte;
  W: Word;
  Obj: TObject;
  eng: TJSEngine;
  classProto: TJSClassProto;
  v: TValue;
  jsarr: PJSObject;
  val: jsval;

begin
  Result := JSVAL_NULL;

  if Value.IsEmpty then
    exit;

  case Value.Kind of
    tkSet:
      begin
        case Value.DataSize of
          1:
            begin
              Value.ExtractRawData(@B);
              L := B;
            end;
          2:
            begin
              Value.ExtractRawData(@W);
              L := W;
            end;
          4:
            begin
              Value.ExtractRawData(@L);
            end;
        end;
        Result := IntToJSVal(L);
      end;
    tkEnumeration:
      if Value.TypeInfo = System.TypeInfo(boolean) then
        Result := BoolToJSVal(Value.AsBoolean)
      else begin
        Result := IntToJSVal(Value.AsOrdinal);
        //Result := IntToJSVal(Value.AsInteger);
      end;

    tkInt64, tkInteger:
      Result := IntToJSVal(Value.AsInt64);
    tkFloat:
      Result := DoubleToJSVal(cx, Value.AsExtended);
    tkLString, tkWString, tkUString:
      Result := StringToJSVal(cx, Value.AsString);
    tkClass:
      begin
        Obj := Value.AsObject;
        eng := TJSClass.JSEngine(cx);
        Result := JSVAL_NULL;
        for classProto in eng.FDelphiClasses.Values do
        begin
          if classProto.FRttiType.Name = Obj.ClassName then
          begin
            with TJSClass.CreateJSObject(Obj, eng, '', classProto.FClassFlags) do
              Result := JSObjectToJSVal(Fjsobj);
            break;
          end;

        end;

        if Result = JSVAL_NULL then
        begin
          // Create JS Object
          with TJSClass.CreateJSObject(Obj, eng, '', [cfaInheritedMethods, cfaInheritedProperties]) do
            Result := JSObjectToJSVal(Fjsobj);
        end;

      end;
    tkDynArray:
      begin
        if Value.GetArrayLength = 0 then
        begin
          Result := JSVAL_FALSE;
        end
        else
        begin
          eng := TJSClass.JSEngine(cx);
          jsarr := JS_NewArrayObject(eng.Context, 0, nil);
          for L := 0 to Value.GetArrayLength - 1 do
          begin
            v := Value.GetArrayElement(L);
            val := TValueToJSVal(cx, v);
            JS_SetElement(eng.Context, jsarr, L, @val);
          end;
          Result := JSObjectToJSVal(jsarr);
        end;
      end;
    tkRecord:
      begin
        if Value.IsType(TypeInfo(TValue)) then
        begin
          Result := TValueToJSVal(cx, Value.AsType<TValue>);
          { end
            else if Value.IsType(TypeInfo(TJSReturnValue)) then
            begin
            Result := Value.AsType<TJSReturnValue>.Value; }
        end;
      end;
  end;

end;

constructor TJSClass.Create;
begin

end;

destructor TJSClass.Destroy;
var
  i: Integer;
begin
  if (cfaOwnObject in FClassProto.FClassFlags) and assigned(FNativeObj) and (FNativeObj <> self) then
     FNativeObj.Free;
  inherited;
end;

procedure TJSClass.FreeJSObject(Engine: TJSEngine);
begin
  // JS_DeleteUCProperty2(FEngine.Context, parent, PWideChar(Obj.JSName), Length(Obj.JSName), @rval);
  try
    JS_SetPrivate(Engine.Context, JSObject, NIL);
  except

  end;

end;

constructor TJSClass.CreateJSObject(Instance: TObject; AEngine: TJSEngine; JSObjectName: string;
  AClassFlags: TJSClassFlagAttributes);
begin
  Create;

  NewJSObject(AEngine, JSObjectName, Instance, AClassFlags);

end;

class function TJSClass.GetParamName(cx: PJSContext; id: jsval): string;
begin
  Result := JS_GetStringChars(JS_ValueToString(cx, id));

end;

class procedure TJSClass.JSObjectDestroy(cx: PJSContext; Obj: PJSObject);
var
  p: Pointer;
begin
  p := JS_GetPrivate(cx, Obj);
  if p <> nil then
  begin
    JS_SetPrivate(cx, Obj, nil);
    try
{$ifdef debug}
//     OutputDebugString(pchar('TJSClass.JSObjectDestroy: '+TObject(p).classname));
{$endif}

      TObject(p).Free;
    except

    end;
  end;

end;

class function TJSClass.JSObjectCtor(cx: PJSContext; jsobj: PJSObject; argc: uintN; argv, rval: pjsval): JSBool;
var
  eng: TJSEngine;
  defClass: TJSClassProto;
  Obj: TJSClass;
  JSClassName: string;
  args: TArray<TValue>;
  methodResult: TValue;
  t: TRttiType;
  m: TRttiMethod;
begin
  Result := JS_TRUE;
  if (JS_IsConstructing(cx) = JS_FALSE) then
  begin
    // EJS_THROW_ERROR(cx,obj,"not yet implemented");
    JS_ReportError(cx, 'JSCtor: Class Not yet implemented', nil);
  end;

  JSClassName := JS_GetClass(jsobj).name;
  eng := TJSClass.JSEngine(cx);
  if eng.FDelphiClasses.TryGetValue(JSClassName, defClass) then
  begin

    // Call objects javascript ctor methods
    if defClass.FJSCtor <> nil then
    begin
      args := TJSClass.JSArgsToTValues(defClass.FJSCtor.GetParameters, cx, jsobj, argc, argv);
      try
        methodResult := defClass.FJSCtor.Invoke(defClass.FClass, args);
        // Construct TJSClass if needed
        if defClass.FClass.InheritsFrom(TJSClass) then
        begin
          Obj := methodResult.AsObject as TJSClass;
          Obj.FNativeObj := Obj;
        end
        else
        begin
          Obj := TJSClass.Create;
          Obj.FNativeObj := methodResult.AsObject;
        end;
        Obj.FClassProto := defClass;

        // Call JSAfterConstruction
        (* t := obj.FClassProto.Fctx.GetType(obj.FClassProto.FClass);
          for m in t.GetMethods('JSAfterConstruction') do
          begin
          if Obj.FNativeObj is TJSClass then
          TJSClass(Obj.FNativeObj).FJSEngine := eng;

          m.Invoke(obj.FNativeObj, []);
          end; *)

      except
        on e: Exception do
        begin
          Result := JS_FALSE;
          JS_ReportError(cx, PAnsiChar(AnsiString('Exception: ' + e.message)), nil);
          exit;
        end
      end;
    end
    else
    begin
      if defClass.FClass.InheritsFrom(TJSClass) then
      begin
        Obj := defClass.CreateNativeObject(defClass.FClass) as TJSClass;
        Obj.FNativeObj := Obj;
        TJSClass(Obj).FJSEngine := eng;
      end
      else
      begin
        Obj := TJSClass.Create;
        Obj.FNativeObj := defClass.CreateNativeObject(defClass.FClass);
      end;
      Obj.FClassProto := defClass;
    end;

    Obj.FJSObj := jsObj;
    Obj.FJSObject := TJSObject.Create(jsobj, eng, '');
    Result := JS_SetPrivate(cx, jsobj, Pointer(Obj));

  end;

end;

class function TJSClass.JSArgsToTValues(params: TArray<TRttiParameter>; cx: PJSContext; jsobj: PJSObject; argc: uintN;
  argv: pjsval): TArray<TValue>;
var
  i: Integer;
  pObj: PJSObject;
  param: TRttiParameter;
  vp: jsval;
  nativeParams: TJSNativeCallParams;

  function getDefaultValue(t: PTypeInfo): TValue;
  var
    typeData: PTypeData;

  begin
    case t^.Kind of
      tkEnumeration:
        Result := false;
      tkFloat:
        Result := 0.0;
      tkInt64, tkInteger:
        Result := 0;
      tkLString, tkWString, tkUString:
        Result := '';
      tkDynArray:
        Result := TValue.FromArray(t, []);
      tkClass:
        Result := nil;
    end;

  end;

begin
{$POINTERMATH ON}
  SetLength(Result, Length(params));
  if Length(params) = 0 then
    exit;

  (*
    // Check if this is a registered object or javascript parameters  object
    var annot = this.addAnnot({
    page: 0,
    type: "Stamp",
    author: "A. C. Robat",
    name: "myStamp",
    rect: [400, 400, 550, 500],
    contents: "Try it again, this time with order and method!",
    AP: "NotApproved"
    }
    *)
  if (argc > 0) and (JSValIsObject(argv^) and (JS_GetClass(JSValToObject(argv^)).name = 'Object')) then
  begin

    pObj := JSValToObject(argv^);

    for i := 0 to High(params) do
    begin
      param := params[i];
      if JS_GetProperty(cx, pObj, PAnsiChar(AnsiString(param.name)), @vp) = 1 then
      begin
        Result[i] := JSValToTValue(cx, param.ParamType.Handle, vp);
      end;
    end;

  end
  else
  begin

    for i := 0 to High(params) do
    begin

      param := params[i];

      if param.ParamType.Handle = TypeInfo(TJSNativeCallParams) then
      begin
        nativeParams.cx := cx;
        nativeParams.jsobj := jsobj;
        nativeParams.argc := argc;
        nativeParams.argv := argv;
        // nativeParams.rval := nil;//rval;

        // Args[0] := TValue.From<TJSNativeCallParams>(nativeParams);
        TValue.Make(@nativeParams, TypeInfo(TJSNativeCallParams), Result[i]);
      end
      else
      begin
        if (argc = 0) or (i > argc - 1) then
        begin
          Result[i] := getDefaultValue(param.ParamType.Handle);
        end
        else
        begin
          Result[i] := JSValToTValue(cx, param.ParamType.Handle, argv[i]);
        end;
      end;

    end;

  end;
{$POINTERMATH OFF}
end;

class function TJSClass.JSDateToDateTime(JSEngine: TJSEngine; oDate: PJSObject; var dDate: TDateTime): boolean;
var
  vp: jsval;
  fval: jsval;

  d, m, y, h, mn, s, ml: Integer;
  cx: PJSContext;
begin

  Result := false;
  cx := JSEngine.Context;
  if (JS_InstanceOf(cx, oDate, JSEngine.DateClass, nil) = JS_FALSE) then
    exit;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getDate')), Length('getDate'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      d := JSValToInt(vp);
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getMonth')), Length('getMonth'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      m := JSValToInt(vp) + 1;
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getFullYear')), Length('getFullYear'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      y := JSValToInt(vp);
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getHours')), Length('getHours'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      h := JSValToInt(vp);
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getMinutes')), Length('getMinutes'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      mn := JSValToInt(vp);
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getSeconds')), Length('getSeconds'), @fval) = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      s := JSValToInt(vp);
  end;

  if JS_GetUCProperty(cx, oDate, PWideChar(WideString('getMilliseconds')), Length('getMilliseconds'), @fval)
    = JS_TRUE then
  begin
    if JS_CallFunction(cx, oDate, JS_ValueToFunction(cx, fval), 0, nil, @vp) = JS_FALSE then
      exit;
    if JSValIsInt(vp) then
      ml := JSValToInt(vp);
  end;

  dDate := encodeDateTime(y, m, d, h, mn, s, ml);
  Result := true;

end;

function TJSClass.JSEngine: TJSEngine;
begin
  Result := FJSEngine;
end;

{$IF CompilerVersion >= 23}
class function TJSClass.JSIndexedPropRead(cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiIndexedProperty;
  propIndex: Integer;
  jsarr: PJSObject;
  V: TValue;
  iObj: TJSIndexedProperty;
begin
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
     exit;
  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

  propName := Obj.FClassProto.Fclass_props[JSValToInt(id) + 127].name;
  prop := t.GetIndexedProperty(propName);
  if prop <> nil then
  begin

     iObj := TJSIndexedProperty.Create;
     iObj.parentObj := obj;
     iObj.propName := propName;
     vp^ := TValueToJSVal(cx, iObj)
  end
  else
    vp^ := JSVAL_NULL;

  Result := JS_TRUE;

end;
{$ifend}
procedure TJSClass.JSKeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array[0..2] of jsval;
  shiftI: Integer;
begin
//  i := Byte(Shift);
  eventData := TJSEventData(self);

  shiftI := 0;
  if ssShift in Shift then shiftI := shiftI and Integer(ssShift);
  if ssAlt in Shift then shiftI := shiftI and Integer(ssAlt);
  if ssCtrl in Shift then shiftI := shiftI and Integer(ssCtrl);
  if ssLeft in Shift then shiftI := shiftI and Integer(ssLeft);
  if ssRight in Shift then shiftI := shiftI and Integer(ssRight);
  if ssMiddle in Shift then shiftI := shiftI and Integer(ssMiddle);
  if ssDouble in Shift then shiftI := shiftI and Integer(ssDouble);

  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).{FJSObject.}Fjsobj);
  f_argv[1] := IntToJSVal(Key);
  f_argv[2] := IntToJSVal(shiftI);


  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 3, @f_argv,
    @f_rval) = JS_TRUE then
  begin
    if JSValIsInt(f_rval) then
    begin
       Key := JSValToInt(f_rval);
    end;
  end;

end;

procedure TJSClass.JSKeyPressEvent(Sender: TObject; var Key: Char);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array [0..1] of jsval;
begin
  eventData := TJSEventData(self);
  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).{FJSObject.}Fjsobj);
  f_argv[1] := StringToJSVal(eventData.fcx,Key);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 1, @f_argv,
    @f_rval) = JS_TRUE then
  begin
    f_rval := 0;
  end;

end;

class function TJSClass.JSEngine(cx: PJSContext): TJSEngine;
begin
  Result := TJSEngine(JS_GetRuntimePrivate(JS_GetRuntime(cx)));

end;

constructor TJSClass.CreateJSObject(AEngine: TJSEngine; JSObjectName: string);
begin
  Create;
  NewJSObject(AEngine, JSObjectName);
end;

{ JSNameAttribute }

constructor JSNameAttribute.Create(Name: string);
begin
  FName := Name;
end;

{ JSClassFlagsAttribute }

constructor JSClassFlagsAttribute.Create(flags: TJSClassFlagAttributes);
begin
  FClassFlags := flags;
end;

{ TJSEvent }

constructor TJSEventData.Create(ajsfuncobj: PJSObject; aeventname: string; aobj: TObject; acx: PJSContext);
begin
  fjsfuncobj := ajsfuncobj;
  feventName := aeventname;
  fobj := aobj;
  fcx := acx;

end;

(*
{ TJSDebuggedScript }

function TJSDebuggedScript.ClearBreakPoint(lineNo: uintN): boolean;
var
  pc: pjsbytecode;
  t: JSTrapHandler;
  p: pointer;
begin
  pc := JS_LineNumberToPC(FEngine.Context, FScript, lineNo);
  if pc = nil then
    exit(false);

  JS_ClearTrap(FEngine.Context, FScript, pc, t, p);

end;

procedure TJSDebuggedScript.ClearBreakPoints;
begin
  JS_ClearAllTraps(FEngine.Context);
end;

procedure TJSDebuggedScript.Compile(const AFileName: string );
begin
  inherited ;//Compile(FFilename);
//  Result := JS_CompileUCScript(fcx, fglobal, PWideChar(Code), Length(Code), 'inline', 0);

end;

constructor TJSDebuggedScript.Create(AEngine: TJSEngine; const ACode: String; const AFileName: String);
begin
  inherited Create;

  FEngine := AEngine;
  FCode := ACode;
  JS_SetNewScriptHook(FEngine.frt, _JSNewScriptHook, self);
  JS_SetDestroyScriptHook(FEngine.frt, _JSDestroyScriptHook, Self);
  JS_SetDebugErrorHook(FEngine.frt, _JSDebugErrorHook, self);
  JS_SetExecuteHook(FEngine.frt, _JSInterpreterHook, self);
  FMode := dmStepping;
  FStarted := False;
  FFrameStop := nil;
  FFilename := AFilename;

  //FEvent := TEvent.Create(nil, true, false, 'js_debug_event');
  Compile(AFileName);

end;

function TJSDebuggedScript.Debug(cx: PJSContext; rval: pjsval; fp: PJSStackFrame): JSTrapStatus;
var
  Action: TJSDebuggerAction;
begin
  DoDebug(Action);

  case Action of
    daStepInto:
      begin
        JS_SetInterrupt(FEngine.frt,  _JSTrapHandler, self);
        Fmode := dmStepping;
        Result := JSTRAP_CONTINUE;
      end;
    daRun:
      begin
        JS_SetInterrupt(FEngine.frt, nil, nil);
        Fmode := dmRun;
        exit(JSTRAP_CONTINUE);
      end;
    daStepOver:
      begin
        FframeStop := fp;
        Fmode := dmStepover;
        exit(JSTRAP_CONTINUE);
      end;
    daStepOut:
      begin
        if (fp <> nil) and ( JS_IsNativeFrame(cx,fp) = js_false) then
           FframeStop := JS_GetScriptedCaller(cx, fp);
        fmode := dmStepout;
        exit(JSTRAP_CONTINUE);

      end;
  end;
  // test run

  // test step
end;

destructor TJSDebuggedScript.Destroy;
begin
  JS_SetNewScriptHook(FEngine.Context, NIL, NIL);
  JS_SetDestroyScriptHook(FEngine.Context, NIL, NIL);
  JS_SetDebugErrorHook(FEngine.frt, NIL, NIL);
  JS_SetExecuteHook(FEngine.frt, NIL, NIL);
  //FEvent.Free;
  inherited;
end;

procedure TJSDebuggedScript.DoDebug(var Action: TJSDebuggerAction);
begin


  Action := daRun;
  if Assigned(FOnDebug) then
     FOnDebug(Self, Action);
  {while FEvent.WaitFor(500) = wrTimeout do
  begin
    Application.ProcessMessages;
  end;
  }
end;

function TJSDebuggedScript.SetBreakPoint(lineNo: uintN): boolean;
var
  pc: pjsbytecode;
begin
  pc := JS_LineNumberToPC(FEngine.Context, FScript, lineNo);
  if pc = nil then
    exit(false);

  Result := JS_SetTrap(FEngine.Context, FScript, pc, _JSBreakHandler, self) = JS_TRUE;

end;

class function TJSDebuggedScript._JSBreakHandler(cx: PJSContext; Script: PJSScript; pc: pjsbytecode; rval: pjsval;
  closure: pointer): JSTrapStatus;
var
  scr: TJSDebuggedScript;
  line: uintN;
begin
  scr := TJSDebuggedScript(closure);
  line := JS_PCToLineNumber(cx, script, pc);

  scr.Fmode := dmBreak;
  // traphandler will be called at the beginning of the js_Execute loop
  JS_SetInterrupt(scr.FEngine.frt, _JSTrapHandler, closure);
  JS_SetExecuteHook(scr.FEngine.frt, _JSInterpreterHook, closure);

  result := JSTRAP_CONTINUE;
end;

class function TJSDebuggedScript._JSDebugErrorHook(cx: pJSContext; const _message: pansichar; report: PJSErrorReport;
  closure: pointer): JSBool;
var
  scr: TJSDebuggedScript;
begin
  scr := TJSDebuggedScript(closure);
  if scr = NIL then
     exit(JS_TRUE);


end;

class procedure TJSDebuggedScript._JSDestroyScriptHook(cx: pJSContext; script: pjsscript; callerdata: pointer);
var
  scr: TJSDebuggedScript;
begin
  scr := TJSDebuggedScript(callerdata);
  if scr = nil then exit;

  JS_ClearScriptTraps(cx, script);
  

end;

class function TJSDebuggedScript._JSInterpreterHook(cx: pJSContext; fp: pJSStackFrame; before: JSBool; ok: PJSBool;
  closure: pointer): pointer;
var
  scr: TJSDebuggedScript;
  s: PJSScript;
  pc: pjsbytecode;
  line: Integer;
begin

  scr := TJSDebuggedScript(closure);
  if scr.FMode = dmRun then exit(nil);


  if (scr.FframeStop <> fp )and (
   (scr.Fmode = dmStepout )or( scr.Fmode = dmStepover))
   then exit(nil);

  s := JS_GetFrameScript(cx, fp);
  pc := JS_GetFramePC(cx, fp);
  line := 0;
  if (pc <> nil) then
     line := JS_PCToLineNumber(cx,s,pc);


  scr.FlastStop := s;
  scr.FlastLine := line;

  if (scr.Debug(cx, nil, fp) = JSTRAP_ERROR) then
  begin
     //exit if possible
     if (ok <> nil) then ok^ := JS_FALSE;
     exit(nil);
  end;


  if (before = js_true) then
  begin
     if (scr.Fmode = dmStepover )or( scr.Fmode = dmStepout) then
     begin
        JS_SetExecuteHook(scr.FEngine.Frt, nil, nil);
        JS_SetInterrupt(scr.FEngine.Frt, nil, nil);
        exit(closure); //call when done to restore debugger state.
     end;
     //running or stepping? no need to call again for this script.
     exit(nil);
  end else
  begin
     scr.Fmode := dmStepping;
     JS_SetExecuteHook(scr.FEngine.Frt, _JSInterpreterHook, closure);
     JS_SetInterrupt(scr.FEngine.Frt, _JSTrapHandler, closure);
     exit(nil);
  end;

end;

class procedure TJSDebuggedScript._JSNewScriptHook(cx: pJSContext; filename: PAnsiChar; lineno: uintN;
  script: Pjsscript; fun: PJSFunction; callerdata: pointer);
var
  scr: TJSDebuggedScript;
begin

  scr := TJSDebuggedScript(callerdata);
end;

class function TJSDebuggedScript._JSTrapHandler(cx: PJSContext; Script: PJSScript; pc: pjsbytecode; rval: pjsval;
  closure: pointer): JSTrapStatus; cdecl;
var
  str: PJSString;
  caller: PJSStackFrame;
  scr: TJSDebuggedScript;
  line: uintN;
begin
 // JS_GC(cx);
  scr := TJSDebuggedScript(closure);

  if (pc <> nil) and (pc^ <> 0) and (pc^ <> 125) then
    scr.FStarted := true;

  if (not scr.FStarted) then
     exit(JSTRAP_CONTINUE);


  if (scr.Fmode = dmRun) then exit(JSTRAP_CONTINUE);
//  caller := JS_GetScriptedCaller(cx, nil);
  line := JS_PCToLineNumber(cx, Script, pc);

  if (
      (
       (Scr.Fmode = dmStepping)
       or (Scr.Fmode = dmStepover)
       or (Scr.Fmode = dmStepout)
      )
      and (script = scr.FlastStop)and (line = scr.FlastLine)
     ) then
   exit(JSTRAP_CONTINUE);

   scr.FlastStop := script;
   scr.FlastLine := line;

   if (scr.Fmode = dmBreak) or (scr.Fmode = dmStepping ) then
   begin
      //scr.FlastLine := line;
   end;

  //if assigned(scr.FOnDebug) then scr.FOnDebug(scr);

  // test step
  //JS_SetInterrupt(rt,jsdb_TrapHandler,this);
  //mode = Stepping;

  //return JSTRAP_CONTINUE;
  Result := scr.Debug(cx, rval, nil);
//  result := JSTRAP_CONTINUE;
end;
*)

end.
