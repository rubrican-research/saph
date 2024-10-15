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
        myManaged : boolean; // Determines if this unit handles freeing the objects after use.
        myName: string;
        {locking}
        myLockExclusive: boolean;
        myLockProcessID: SizeUInt;
        myLockThreadID: TThreadID;
        myKey: string;
		mySilentLock: boolean;
        function getName: string; overload;
		procedure setEnableHistory(AValue: boolean);
        procedure setName(const _n: string); overload;
        function getLocked: boolean;
        function setLock: string;
        procedure clearLock;
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
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Adding read listener
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add write listener
        function undo: TReactive;
        function redo: TReactive;

        // Crtical section
        procedure enterCS;
        procedure leaveCS;

        function  value: variant; overload; virtual;     // getter
        procedure value(_v: variant); overload; virtual; // setter
        procedure value(_v: variant; _req: string; _key: string); overload; virtual;// writes a value without unlocking

        {lock function}
        function lock(): string; // locks the value and returns a key. Returns empty string if already locked;
        function lockEx(): string; // locks the value exclusively. cannot be borrowed.
        function unlock(_key: string): boolean;
        function borrow: string; // Forces change of lock
        function canChangeValue: boolean;
        function isMyLock: boolean; // returns true if locked by the process and thread.


    published
        property name: string read getName write setName;
        property locked: boolean read getLocked;
        // silentLock
        //      true: raises an exception when assigning a value to locked object
        //      false: does not change value. exits silently.
        property silentLock: boolean read mySilentLock write mySilentLock;
        // Enables undo and redo
        property enableHistory: boolean read myEnableHistory write setEnableHistory;
    public
        property val: variant read value write value;
	end;

	{ GReactive }

    generic GReactive<T> = class(TReactive)
	private
		myValue: T;
    public
        function value: T; overload; reintroduce;      // getter
        procedure value(_v: T); overload; reintroduce; // setter
        procedure value(_v: T; _req: string; _key: string); // writes a value without unlocking
    public
        property val: T read value write value;

	end;


	{ TRInt }

    TRInt = class(specialize GReactive<integer>);

	{ TRInt64 }

    TRInt64 = class(specialize GReactive<int64>);

	{ TRDWord }

    TRDWord = class(specialize GReactive<DWord>);

	{ TRQWord }

    TRQWord = class(specialize GReactive<QWord>);


	{ TRFloat }

    TRFloat = class(specialize GReactive<double>);

	{ TRStr }

    TRStr = class(specialize GReactive<string>);

	{ TRBool }

    TRBool = class(specialize GReactive<boolean>);

	{ TRDateTime }

    TRDateTime = class(specialize GReactive<TDateTime>);


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

var
    rStoreCS: TRTLCriticalSection;

implementation
uses
    obj.Listener, strutils, sugar.logger;

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

function rmFromStore(_r: TReactive): integer;
begin
    log('rmFromStore of ' + _r.ClassName);
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

generic function rFree(var _r: TReactive): integer; // syntax sugar for rmFromStore()
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

function TReactive.getLocked: boolean;
begin
    result := not myKey.IsEmpty;
end;

function TReactive.setLock: string;
begin
    EnterCriticalSection(rStoreCS);
    myLockProcessID := GetProcessID;
    myLockThreadID  := ThreadID;
    myLockExclusive := false;
    myKey           := makeKey();
    LeaveCriticalSection(rStoreCS);
    Result          := myKey;
end;

procedure TReactive.clearLock;
begin
    EnterCriticalSection(rStoreCS);
    myLockProcessID := 0;
    myLockThreadID  := 0;
    myLockExclusive := false;
    myKey           := '';
    LeaveCriticalSection(rStoreCS);
end;

procedure TReactive.enterCS;
begin
    EnterCriticalSection(rStoreCS);
end;

procedure TReactive.leaveCS;
begin
    LeaveCriticalSection(rStoreCS);
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
    clearLock;
end;

destructor TReactive.Destroy;
begin
    inherited Destroy;
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
    addListener(SGWRITE, _subscriber, _e, qSerial);
end;

function TReactive.lock: string;
begin
    if myKey.isEmpty then begin
        Result := setLock;
	end
    else
        Result := '';
end;

function TReactive.lockEx(): string;
begin
    if myKey.isEmpty then begin
        Result          := setLock;

        //EnterCriticalSection(myLockCS);
        myLockExclusive := true;
        //LeaveCriticalSection(myLockCS);
	end
    else
        Result := '';
end;

function TReactive.unlock(_key: string): boolean;
begin
    if
        (UnicodeSameStr(_key, myKey))
        and (myLockProcessID = GetProcessID)
        and (myLockThreadID = ThreadID) then
    begin
        clearLock;
        Result := true;
	end
    else begin
        log('FAIL UNLOCK::: same key: %s; processID: %s, thread id: %s',[ BoolToStr(UnicodeSameStr(_key, myKey)), BoolToStr((myLockProcessID = GetProcessID)), BoolToStr((myLockThreadID = ThreadID))]);
        log('FAIL UNLOCK::: _key: "%s"; myKey: "%s"',[ _key, myKey]);
        log('FAIL UNLOCK::: myLockPID: "%d"; getProcessID: "%d"',[ myLockProcessID ,GetProcessID]);
        log('FAIL UNLOCK::: myLockThreadID: "%d"; ThreadID: "%d"',[ myLockThreadID,ThreadID]);
        Result := false;
	end;
end;

function TReactive.borrow: string;
begin
    if not myLockExclusive then begin
        result := setLock;
	end
    else if not mySilentLock then begin
        raise Exception.Create(Format('TReactive.borrow() %s "%s" is exclusively locked.', [ClassName, Name]));
	end;
end;

function TReactive.canChangeValue: boolean;
begin
    Result :=  myKey.isEmpty; // yes, change because it is not locked.
    if not Result then // it is locked. Allow changes to be made by the process and thread that has locked this
    begin
        Result := isMyLock;
	end;
end;

function TReactive.isMyLock: boolean;
begin
    Result := (myLockProcessID = GetProcessID) and (myLockThreadID = ThreadID);
end;

function TReactive.undo: TReactive;
begin

end;

function TReactive.redo: TReactive;
begin

end;

function TReactive.value: variant;
begin
    raise Exception.Create('TReactive:: Value should not be called from TReactive');
    Result := nil;
end;

procedure TReactive.value(_v: variant);
begin
    raise Exception.Create('TReactive:: Value(_v) should not be called from TReactive');
end;

procedure TReactive.value(_v: variant; _req: string; _key: string);
begin
    raise Exception.Create('TReactive:: Value(_v, _req, _key) should not be called from TReactive');
end;


{ GReactive }

function GReactive.value: T;
begin
    Result := myValue;
end;

procedure GReactive.value(_v: T);
begin
    if canChangeValue then begin
        if _v <> myValue then begin
            enterCS;
            myValue := _v;
            signal(SGWRITE);
            leaveCS;
	    end;
	end
    else if not mySilentLock then
        raise Exception.Create(Format('Reactive variable(%s) "%s" is locked. Value cannot be written', [ClassName, Name]));
end;

procedure GReactive.value(_v: T; _req: string; _key: string);
begin

end;


initialization
    InitCriticalSection(rStoreCS);
    RStore := TRStore.Create(true);
    RStore.Sorted := True;
    RStore.Duplicates := TDuplicates.dupAccept;

finalization
    RStore.Free;
    DoneCriticalSection(rStoreCS);

end.

