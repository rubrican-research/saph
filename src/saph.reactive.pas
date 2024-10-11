unit saph.reactive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, fgl;
type
    	{ TReactive }

    TReactive = class
    private
		myEnableHistory: boolean;
        myKey: string;
		myLocked: boolean;
        myManaged : boolean; // Determines if this unit handles freeing the objects after use.
        myName: string;
		mySilentLock: boolean;
        function getName: string; overload;
		procedure setEnableHistory(AValue: boolean);
        procedure setName(const _n: string); overload;
    protected
        function makeKey(): string;
    public
        const SGNAME =  'sig_r_name';
        const SGREAD =  'sig_r_read';
        const SGWRITE = 'sig_r_write';
    public
        constructor Create;
        destructor Destroy; override;
    public
        function value: variant; overload; virtual;       // getter
        procedure value(_v: variant); overload; virtual;   // setter
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Adding read listener
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add write listener
        function lock(): string; // locks the value and returns a key. Returns empty string if already locked;
        function unlock(_key: string): boolean;
        function undo: TReactive;
        function redo: TReactive;
    published
        property name: string read getName write setName;
        property locked: boolean read myLocked;
        // silentLock
        //      true: raises an exception when assigning a value to locked object
        //      false: does not change value. exits silently.
        property silentLock: boolean read mySilentLock write mySilentLock;
        // Enables undo and redo
        property enableHistory: boolean read myEnableHistory write setEnableHistory;
	end;

	{ TRInt }

    TRInt = class(TReactive)
    protected
        myValue: integer;
    public
        function value: integer; overload; reintroduce;
        procedure value(_v: integer); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRInt; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRInt; reintroduce;
    published
        property val: integer read value write value;
    end;

	{ TRInt64 }

    TRInt64 = class(TReactive)
    protected
        myValue: int64;
    public
        function value: int64; overload; reintroduce;
        procedure value(_v: int64); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRInt64; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRInt64; reintroduce;
    published
            property val: int64 read value write value;
    end;

	{ TRDWord }

    TRDWord = class(TReactive)
    protected
        myValue: DWord;
    public
        function value: DWord; overload; reintroduce;
        procedure value(_v: DWord); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRDWord; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRDWord; reintroduce;
    published
        property val: DWord read value write value;
    end;

	{ TRQWord }

    TRQWord = class(TReactive)
    protected
        myValue: QWord;
    public
        function value: QWord; overload; reintroduce;
        procedure value(_v: QWord); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRQWord; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRQWord; reintroduce;
    published
        property val: QWord read value write value;
    end;


	{ TRFloat }

    TRFloat = class(TReactive)
    protected
        myValue: double;
    public
        function value: double; overload; reintroduce;
        procedure value(_v: double); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRFloat; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRFloat; reintroduce;
    published
        property val: double read value write value;
    end;

	{ TRStr }

    TRStr = class(TReactive)
    protected
        myValue: string;
    public
        function value: string; overload; reintroduce;
        procedure value(_v: string); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRStr; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRStr; reintroduce;
    published
        property val: string read value write value;
    end;

	{ TRBool }

    TRBool = class(TReactive)
    protected
        myValue: boolean;
    public
        function value: boolean; overload; reintroduce;
        procedure value(_v: boolean); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRBool; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRBool; reintroduce;
    published
        property val: boolean read value write value;
    end;

	{ TRDateTime }

    TRDateTime = class(TReactive)
    protected
        myValue: TDateTime;
    public
        function value: TDateTime; overload; reintroduce;
        procedure value(_v: TDateTime); overload; reintroduce;
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TRDateTime; reintroduce;
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TRDateTime; reintroduce;
    published
        property val: TDateTime read value write value;
    end;
    // Factory functions
    function RInt : TRInt; overload;
    function RInt(_default: integer; _name: string = '') : TRInt; overload;

    function RInt64 : TRInt64;overload;
    function RInt64(_default: int64; _name: string = '') : TRInt64;overload;

    function RDWord : TRDWord;overload;
    function RDWord(_default: DWord; _name: string = '') : TRDWord;overload;

    function RQWord : TRQWord;overload;
    function RQWord(_default: QWord; _name: string = '') : TRQWord;overload;

	function RFloat : TRFloat;overload;
	function RFloat(_default: Double; _name: string = ''): TRFloat;overload;

	function RStr : TRStr;overload;
	function RStr(_default: string; _name: string = '') : TRStr;overload;

	function RBool : TRBool;overload;
	function RBool(_default: boolean; _name: string = '') : TRBool;overload;

	function RDateTime  : TRDateTime;overload;
	function RDateTime(_default: TDateTime; _name: string = '') : TRDateTime;overload;

    // Destructors
    function rFree(var _r: TReactive): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRInt): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRInt64): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRDWord): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRQWord): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRFloat): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRStr): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRBool): integer; // syntax sugar for rmFromStore()
    function rFree(var _r: TRDateTime): integer; // syntax sugar for rmFromStore()

    // Manage
    function addToStore(_r: TReactive): TReactive;
    function rmFromStore(_r: TReactive): integer; // Index where it was located

{========= OPERATOR OVERLOADING ============================}
{TRStr}
    operator :=(v: TRStr) : string;
    operator =(v: TRStr; a: string): boolean;
    operator +(b: TRStr; a: string): TRStr;
    operator +(a: string; b: TRStr): TRStr;
    operator +(a: TRStr; b: TRStr): TRStr;


implementation
uses
    obj.Listener, strutils;

{============ MANAGED VARIABLES =========================}
type
    TRStore = class(specialize TFPGMapObject<string, TReactive>);

var
    rStore : TRStore; // initialization, finalization

    function pointerAsHex(_obj: pointer): string;
    begin
        Result := PtrUInt(_obj).ToHexString(16);
    end;

    function hexStrAsPointer(_hex: string): Pointer;
    begin
        Result := pointer(Hex2Dec64(_hex));
    end;

{=========================================================}

function addToStore(_r: TReactive): TReactive;
begin
    Result := _r;
    Result.myManaged := true;
    rStore.Add(pointerAsHex(Result), Result);
end;

function rmFromStore(_r: TReactive): integer; // Index where it was located
begin
    Result := rStore.IndexOf(pointerAsHex(_r));
    if Result > -1 then
        rStore.Delete(Result);
end;

operator:=(v: TRStr): string;
begin
    Result := v.val;
end;

operator=(v: TRStr; a: string): boolean;
begin
    Result := (v.val = a);
end;

operator+(b: TRStr; a: string): TRStr;
begin
    Result := RStr();
    Result.Val := b.Val + a;
end;

operator+(a: string; b: TRStr): TRStr;
begin
    Result := RStr();
    Result.Val := b.Val + a;
end;

operator+(a: TRStr; b: TRStr): TRStr;
begin
    Result := RStr(a.val + b.val);
end;


function RInt: TRInt;
begin
    Result := TRInt.Create;
    addToStore(Result);
end;

function RInt(_default: integer; _name: string): TRInt;
begin
    Result := RInt();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RInt64: TRInt64;
begin
    Result := TRInt64.Create;
    addToStore(Result);
end;

function RInt64(_default: int64; _name: string): TRInt64;
begin
    Result := RInt64();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);


end;

function RDWord: TRDWord;
begin
    Result := TRDWord.Create;
    addToStore(Result);
end;

function RDWord(_default: DWord; _name: string): TRDWord;
begin
    Result := RDWord();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RQWord: TRQWord;
begin
    Result := TRQWord.Create;
    addToStore(Result);
end;

function RQWord(_default: QWord; _name: string): TRQWord;
begin
    Result := RQWord();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RFloat: TRFloat;
begin
    Result := TRFloat.Create;
    addToStore(Result);
end;

function RFloat(_default: Double; _name: string): TRFloat;
begin
    Result := RFloat();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RStr: TRStr;
begin
    Result := TRStr.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RStr(_default: string; _name: string): TRStr;
begin
    Result := RStr();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RBool: TRBool;
begin
    Result := TRBool.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RBool(_default: boolean; _name: string): TRBool;
begin
    Result := RBool();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RDateTime: TRDateTime;
begin
    Result := TRDateTime.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RDateTime(_default: TDateTime; _name: string): TRDateTime;
begin
    Result := RDateTime();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function rFree(var _r: TReactive): integer;
begin
    Result := rmFromStore(_r);
    _r := nil;
end;

function rFree(var _r: TRInt): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRInt64): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRDWord): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRQWord): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRFloat): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRStr): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRBool): integer;
begin
    Result := rFree(TReactive(_r));
end;

function rFree(var _r: TRDateTime): integer;
begin
    Result := rFree(TReactive(_r));
end;

{ TReactive }

function TReactive.getName: string;
begin
    Result := myName;
end;

procedure TReactive.setEnableHistory(AValue: boolean);
begin
	if myEnableHistory=AValue then Exit;
	myEnableHistory:=AValue;
end;

procedure TReactive.setName(const _n: string);
begin
    if myName.isEmpty then begin
        myName := _n;
        signal(SGNAME);
	end
	else
        raise Exception.Create('Name can only be set once. ' + sLinebreak
        + 'Right now Name = "' + myName+'"');
end;

function TReactive.makeKey(): string;
begin
    Result := IntToStr(getTickCount64());
end;

constructor TReactive.Create;
begin
    inherited;
    myName:= '';
    myManaged := false;
    mySilentLock:= false; // Raise exception if value is being changed after locking
end;

destructor TReactive.Destroy;
begin
    stopListening;
    inherited Destroy;
end;

function TReactive.value: variant;
begin
    //signal(SGREAD);
    Result := nil;
end;

procedure TReactive.value(_v: variant);
begin
    signal(SGWRITE);
end;

function TReactive.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGREAD, _subscriber, _e);
end;

function TReactive.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGWRITE, _subscriber, _e);
end;

function TReactive.lock(): string;
begin
    if myKey.isEmpty then begin
        myLocked:= true;
        myKey   := makeKey();
	end
    else
        Result := '';
end;

function TReactive.unlock(_key: string): boolean;
begin
    if UnicodeSameStr(_key, myKey) then begin
       myKey:='';
       myLocked:=False;
       Result := true;
	end
    else
        Result := false;
end;

{ TRInt }

function TRInt.value: integer;
begin
    inherited;
    Result := myValue;
end;

procedure TRInt.value(_v: integer);
begin
    if myLocked then begin

        exit;
    end;

    if (myValue <> _v) then begin
        myValue := _v;
        signal(SGWRITE);
	end;
end;

function TRInt.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRInt;
begin
    Result := TRInt(inherited);
end;

function TRInt.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRInt;
begin
    Result := TRInt(inherited);
end;

{ TRInt64 }

function TRInt64.value: int64;
begin
    inherited;
    Result := myValue;
end;

procedure TRInt64.value(_v: int64);
begin
    if (myValue <> _v) then begin
          myValue := _v;
          signal(SGWRITE);
  	end;
end;

function TRInt64.listenRead(constref _subscriber: TObject; _e: TNotifyEvent): TRInt64;
begin
    Result := TRInt64(inherited);
end;

function TRInt64.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent): TRInt64;
begin
    Result := TRInt64(inherited);
end;

{ TRDWord }

function TRDWord.value: DWord;
begin
    inherited;
    Result := myValue;
end;

procedure TRDWord.value(_v: DWord);
begin
    if (myValue <> _v) then begin
          myValue := _v;
          signal(SGWRITE);
  	end;
end;

function TRDWord.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDWord;
begin
    Result := TRDWord(inherited);
end;

function TRDWord.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDWord;
begin
    Result := TRDWord(inherited);
end;

{ TRQWord }

function TRQWord.value: QWord;
begin
    inherited;
    Result := myValue;
end;

procedure TRQWord.value(_v: QWord);
begin
    if (myValue <> _v) then begin
          myValue := _v;
          signal(SGWRITE);
  	end;
end;

function TRQWord.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRQWord;
begin
    Result := TRQWord(inherited);
end;

function TRQWord.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRQWord;
begin
    Result := TRQWord(inherited);
end;

{ TRFloat }

function TRFloat.value: double;
begin
    inherited;
    Result := myValue;
end;

procedure TRFloat.value(_v: double);
begin
    if (myValue <> _v) then begin
        myValue := _v;
        signal(SGWRITE);
	end;
end;

function TRFloat.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRFloat;
begin
    Result:= TRFloat(inherited);
end;

function TRFloat.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRFloat;
begin
    Result:= TRFloat(inherited);
end;

{ TRStr }

function TRStr.value: string;
begin
    inherited;
    Result := myValue;
end;

procedure TRStr.value(_v: string);
begin
    if (myValue <> _v) then begin
        myValue := _v;
        signal(SGWRITE);
	end;
end;

function TRStr.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRStr;
begin
    Result := TRStr(inherited);
end;

function TRStr.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRStr;
begin
    Result := TRStr(inherited);
end;

{ TRBool }

function TRBool.value: boolean;
begin
    inherited;
    Result := myValue;
end;

procedure TRBool.value(_v: boolean);
begin
    if (myValue <> _v) then begin
        myValue := _v;
        signal(SGWRITE);
	end;
end;

function TRBool.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRBool;
begin
    Result := TRBool(inherited);
end;

function TRBool.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRBool;
begin
    Result := TRBool(inherited);
end;

{ TRDateTime }
function TRDateTime.value: TDateTime;
begin
    inherited;
    Result := myValue;
end;

procedure TRDateTime.value(_v: TDateTime);
begin
    if (myValue <> _v) then begin
          myValue := _v;
          signal(SGWRITE);
  	end;
end;

function TRDateTime.listenRead(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDateTime;
begin
    Result := TRDateTime(inherited);
end;

function TRDateTime.listenWrite(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDateTime;
begin
    Result := TRDateTime(inherited);
end;

initialization
    RStore := TRStore.Create(true);
    RStore.Sorted := True;
    RStore.Duplicates := TDuplicates.dupAccept;

finalization
    RStore.Free;

end.

