unit saph.reactive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, fgl, saph.undo;
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
        const SGNAME  = 'sig_r_name';
        const SGREAD  = 'sig_r_read';
        const SGWRITE = 'sig_r_write';
        const SGUNDO  = 'sig_r_undo';
        const SGREDO  = 'sig_r_redo';
    public
        constructor Create; virtual;
        destructor Destroy; override;

    public
        function listenRead(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Adding read listener
        function listenWrite(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add write listener
        function listenUndo(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add Undo listener
        function listenRedo(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add Redo listener

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
    private type SUndoHistory = class(specialize GUndoHistory<T>);
	private
		myValue: T;
        myHistory : SUndoHistory;

    public
        function value: T; overload; reintroduce;      // getter
        procedure value(_v: T); overload; reintroduce; // setter
        procedure value(_v: T; _req: string; _key: string); // writes a value without unlocking
        function memdump: string;

        function histVal(_pos: integer): T;
        function undo(_count: integer = 1): T; reintroduce;
        function redo(_count: integer = 1): T; reintroduce;

    public
        property val: T read value write value;
    public
        constructor Create; override;
        destructor Destroy; override;
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

    TRStr = class(specialize GReactive<string>)

    end;

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


    // Cloner
    function rClone(var _r: TRInt): TRInt;
    function rClone(var _r: TRInt64): TRInt64;
    function rClone(var _r: TRDWord): TRDWord;
    function rClone(var _r: TRQWord): TRQWord;
    function rClone(var _r: TRFloat): TRFloat;
    function rClone(var _r: TRStr): TRStr;
    function rClone(var _r: TRBool): TRBool;
    function rClone(var _r: TRDateTime): TRDateTime;


{========= OPERATOR OVERLOADING ============================}
{TRInt      }
    operator :=(v: TRInt) : integer;
    operator =(v: TRInt; a: integer): boolean;
    operator =(a: integer; v: TRInt): boolean;
    {ADDITION}
    operator +(a: TRInt): TRInt;        // unary operator. Returns a new object
    operator +(b: TRInt; a: integer): TRInt;
    operator +(a: integer; b: TRInt): TRInt;
    operator +(a: TRInt; b: TRInt): TRInt;
    {SUBTRACTION}
    operator -(b: TRInt; a: integer): TRInt;
    operator -(a: integer; b: TRInt): TRInt;
    operator -(a: TRInt; b: TRInt): TRInt;
    operator -(a: TRInt): TRInt;        // unary operator. Returns a new object
    {MULTIPLICATION}
    operator *(b: TRInt; a: integer): TRInt;
    operator *(a: integer; b: TRInt): TRInt;
    operator *(a: TRInt; b: TRInt): TRInt;
    {DIVISION}
    operator /(b: TRInt; a: integer): TRInt; // integer division. for normal division, use a.val / b.val
    operator /(a: integer; b: TRInt): TRInt;
    operator /(a: TRInt; b: TRInt): TRInt;


{TRInt64    }
    operator :=(v: TRInt64) : int64;
    operator =(v: TRInt64; a: int64): boolean;
    operator =( a: int64; v: TRInt64): boolean;
    {ADDITION}
    operator +(b: TRInt64; a: int64): TRInt64;
    operator +(a: int64; b: TRInt64): TRInt64;
    operator +(a: TRInt64; b: TRInt64): TRInt64;
    {SUBTRACTION}
    operator -(b: TRInt64; a: int64): TRInt64;
    operator -(a: int64; b: TRInt64): TRInt64;
    operator -(a: TRInt64; b: TRInt64): TRInt64;
    operator -(a: TRInt64): TRInt64;
    {MULTIPLICATION}
    operator *(b: TRInt64; a: int64): TRInt64;
    operator *(a: int64; b: TRInt64): TRInt64;
    operator *(a: TRInt64; b: TRInt64): TRInt64;
    {DIVISION}
    operator /(b: TRInt64; a: int64): TRInt64; // integer division. for normal division, use a.val / b.val
    operator /(a: int64; b: TRInt64): TRInt64;
    operator /(a: TRInt64; b: TRInt64): TRInt64;

{TRDWord    }
    operator :=(v: TRDWord) : DWord;
    operator =(v: TRDWord; a: DWord): boolean;
    operator =(a: DWord; v: TRDWord): boolean;
    {ADDITION}
    operator +(b: TRDWord; a: DWord): TRDWord;
    operator +(a: DWord; b: TRDWord): TRDWord;
    operator +(a: TRDWord; b: TRDWord): TRDWord;
    {SUBTRACTION}
    operator -(b: TRDWord; a: DWord): TRDWord;
    operator -(a: DWord; b: TRDWord): TRDWord;
    operator -(a: TRDWord; b: TRDWord): TRDWord;
    operator -(a: TRDWord): TRDWord;
    {MULTIPLICATION}
    operator *(b: TRDWord; a: DWord): TRDWord;
    operator *(a: DWord; b: TRDWord): TRDWord;
    operator *(a: TRDWord; b: TRDWord): TRDWord;
    {DIVISION}
    operator /(b: TRDWord; a: DWord): TRDWord; // integer division. for normal division, use a.val / b.val
    operator /(a: DWord; b: TRDWord): TRDWord;
    operator /(a: TRDWord; b: TRDWord): TRDWord;

{TRQWord    }
    operator :=(v: TRQWord) : QWord;
    operator =(v: TRQWord; a: QWord): boolean;
    operator =(a: QWord; v: TRQWord): boolean;
    {ADDITION}
    operator +(b: TRQWord; a: QWord): TRQWord;
    operator +(a: QWord; b: TRQWord): TRQWord;
    operator +(a: TRQWord; b: TRQWord): TRQWord;
    {SUBTRACTION}
    operator -(b: TRQWord; a: QWord): TRQWord;
    operator -(a: QWord; b: TRQWord): TRQWord;
    operator -(a: TRQWord; b: TRQWord): TRQWord;
    {MULTIPLICATION}
    operator *(b: TRQWord; a: QWord): TRQWord;
    operator *(a: QWord; b: TRQWord): TRQWord;
    operator *(a: TRQWord; b: TRQWord): TRQWord;
    {DIVISION}
    operator /(b: TRQWord; a: QWord): TRQWord; // integer division. for normal division, use a.val / b.val
    operator /(a: QWord; b: TRQWord): TRQWord;
    operator /(a: TRQWord; b: TRQWord): TRQWord;

{TRFloat    }
    operator :=(v: TRFloat) : Double;
    operator =(v: TRFloat; a: Double): boolean;
    operator =(a: Double; v: TRFloat): boolean;
    {ADDITION}
    operator +(b: TRFloat; a: Double): TRFloat;
    operator +(a: Double; b: TRFloat): TRFloat;
    operator +(a: TRFloat; b: TRFloat): TRFloat;
    {SUBTRACTION}
    operator -(b: TRFloat; a: Double): TRFloat;
    operator -(a: Double; b: TRFloat): TRFloat;
    operator -(a: TRFloat; b: TRFloat): TRFloat;
    operator -(a: TRFloat): TRFloat;
    {MULTIPLICATION}
    operator *(b: TRFloat; a: Double): TRFloat;
    operator *(a: Double; b: TRFloat): TRFloat;
    operator *(a: TRFloat; b: TRFloat): TRFloat;
    {DIVISION}
    operator /(b: TRFloat; a: Double): TRFloat; // integer division. for normal division, use a.val / b.val
    operator /(a: Double; b: TRFloat): TRFloat;
    operator /(a: TRFloat; b: TRFloat): TRFloat;

{TRStr}
    operator :=(v: TRStr) : string;
    operator =(v: TRStr; a: string): boolean;
    operator =(a: string; v: TRStr): boolean;
    operator +(b: TRStr; a: string): TRStr;
    operator +(a: string; b: TRStr): TRStr;
    operator +(a: TRStr; b: TRStr): TRStr;

{TRBool}
    operator :=(v: TRBool) : Boolean;
    operator =(v: TRBool; a: Boolean): boolean;
    operator =(a: Boolean; v: TRBool): boolean;

{TRDateTime}
    operator :=(v: TRDateTime) : TDateTime;
    operator =(v: TRDateTime; a: TDateTime): boolean;
    operator =(a: TDateTime; v: TRDateTime): boolean;
    {ADDITION}
    operator +(b: TRDateTime; a: TDateTime): TRDateTime;
    operator +(a: TDateTime; b: TRDateTime): TRDateTime;
    operator +(a: TRDateTime; b: TRDateTime): TRDateTime;
    {SUBTRACTION}
    operator -(b: TRDateTime; a: TDateTime): TRDateTime;
    operator -(a: TDateTime; b: TRDateTime): TRDateTime;
    operator -(a: TRDateTime; b: TRDateTime): TRDateTime;


var
    rStoreCS: TRTLCriticalSection;

implementation
uses
    obj.Listener, strutils, sugar.logger, Math;

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

function rClone(var _r: TRInt): TRInt;
begin
    Result := RInt();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;
end;

function rClone(var _r: TRInt64): TRInt64;
begin
    Result := RInt64();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;
end;

function rClone(var _r: TRDWord): TRDWord;
begin
    Result := RDWord();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

function rClone(var _r: TRQWord): TRQWord;
begin
    Result := RQWord();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

function rClone(var _r: TRFloat): TRFloat;
begin
    Result := RFloat();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

function rClone(var _r: TRStr): TRStr;
begin
    Result := RStr();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

function rClone(var _r: TRBool): TRBool;
begin
    Result := RBool();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

function rClone(var _r: TRDateTime): TRDateTime;
begin
    Result := RDateTime();
	Result.myEnableHistory := _r.myEnableHistory  ;
    Result.myManaged       := _r.myManaged        ;
    Result.myName          := _r.myName           ;
    Result.myLockExclusive := _r.myLockExclusive  ;
    Result.myLockProcessID := _r.myLockProcessID  ;
    Result.myLockThreadID  := _r.myLockThreadID   ;
    Result.myKey           := _r.myKey            ;
	Result.mySilentLock    := _r.mySilentLock     ;
	Result.myValue         := _r.myValue          ;
	Result.myHistory       := _r.myHistory        ;

end;

operator:=(v: TRInt): integer;
begin
    Result := v.value();
end;

operator=(v: TRInt; a: integer): boolean;
begin
    Result := (v.value = a)
end;

operator=(a: integer; v: TRInt): boolean;
begin
    Result := a = v.value();
end;

operator+(a: TRInt): TRInt;
begin
    Result := RInt(a.val);
end;

operator+(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.Value + a);
end;

operator+(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a + b.Value);
end;

operator+(a: TRInt; b: TRInt): TRInt;
begin
    Result:= RInt(a.value + b.value);
end;

operator-(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val - a);
end;

operator-(a: integer; b: TRInt): TRInt;
begin
    Result:= RInt(a - b.val);
end;

operator-(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val - b.val);
end;

operator-(a: TRInt): TRInt;
begin
    Result := RInt(-1 * (a.val));
end;

operator*(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val * a);
end;

operator*(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a * b.Val);
end;

operator*(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val * b.val);
end;

operator/(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val div a);
end;

operator/(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a div b.val);
end;

operator/(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val div b.val);
end;


operator:=(v: TRInt64): int64;
begin
    Result := v.value;
end;

operator=(v: TRInt64; a: int64): boolean;
begin
    Result := v.value = a;
end;

operator=(a: int64; v: TRInt64): boolean;
begin
    Result := a = v.value;
end;

operator+(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.value + a);
end;

operator+(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a + b.value);
end;

operator+(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.value + b.value);
end;

operator-(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val - a);
end;

operator-(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a - b.val);
end;

operator-(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val - b.val);
end;

operator-(a: TRInt64): TRInt64;
begin
    Result := RInt64(-1 * a.val);
end;

operator*(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val * a);
end;

operator*(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a * b.val);
end;

operator*(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val * b.val);
end;

operator/(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val div a);
end;

operator/(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a div b.val);
end;

operator/(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val div b.val);
end;

operator:=(v: TRDWord): DWord;
begin
    Result := v.value;
end;

operator=(v: TRDWord; a: DWord): boolean;
begin
    Result := v.value = a;
end;

operator=(a: DWord; v: TRDWord): boolean;
begin
    Result := a = v.value;
end;

operator+(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.value + a);
end;

operator+(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a + b.value);
end;

operator+(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.value + b.value);
end;

operator-(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val - a);
end;

operator-(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a - b.val);
end;

operator-(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val - b.val);
end;

operator-(a: TRDWord): TRDWord;
begin
    Result := RDWord(-1 * a.val);
end;

operator*(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val * a);
end;

operator*(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a * b.val);
end;

operator*(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val * b.val);
end;

operator/(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val div a);
end;

operator/(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a div b.val);
end;

operator/(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val div b.val);
end;

operator:=(v: TRQWord): QWord;
begin
    Result := v.value;
end;

operator=(v: TRQWord; a: QWord): boolean;
begin
    Result := v.value = a;
end;

operator=(a: QWord; v: TRQWord): boolean;
begin
    Result := a = v.value;
end;

operator+(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.value + a);
end;

operator+(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a + b.value);
end;

operator+(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.value + b.value);
end;

operator-(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val - a);
end;

operator-(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a - b.val);
end;

operator-(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val - b.val);
end;


operator*(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val * a);
end;

operator*(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a * b.val);
end;

operator*(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val * b.val);
end;

operator/(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val div a);
end;

operator/(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a div b.val);
end;

operator/(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val div b.val);
end;

operator:=(v: TRFloat): Double;
begin
    Result := v.value;
end;

operator=(v: TRFloat; a: Double): boolean;
begin
    Result := v.value  = a;
end;

operator=(a: Double; v: TRFloat): boolean;
begin
    Result := a = v.value;
end;

operator+(b: TRFloat; a: Double): TRFloat;
begin
    Result := RFloat(b.value + a);
end;

operator+(a: Double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a + b.value);
end;

operator+(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.value + b.value);
end;

operator-(b: TRFloat; a: Double): TRFloat;
begin
    Result := RFloat(b.val - a);
end;

operator-(a: Double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a - b.val);
end;

operator-(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val - b.val);
end;

operator-(a: TRFloat): TRFloat;
begin
    Result := RFloat(-1 * a.val);
end;

operator*(b: TRFloat; a: Double): TRFloat;
begin
    Result := RFloat(b.val * a);
end;

operator*(a: Double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a * b.val);
end;

operator*(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val * b.val);
end;

operator/(b: TRFloat; a: Double): TRFloat;
begin
    Result := RFloat(b.val/a);
end;

operator/(a: Double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a/b.val);
end;

operator/(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val/b.val);
end;

operator:=(v: TRStr): string;
begin
    Result := v.val;
end;

operator=(v: TRStr; a: string): boolean;
begin
    Result := (v.val = a);
end;

operator=(a: string; v: TRStr): boolean;
begin
    Result := a = v.Value;
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

operator:=(v: TRBool): Boolean;
begin
    Result:= v.value;
end;

operator=(v: TRBool; a: Boolean): boolean;
begin
    Result := v.value = a;
end;

operator=(a: Boolean; v: TRBool): boolean;
begin
    Result := a = v.value;
end;

operator:=(v: TRDateTime): TDateTime;
begin
    Result := v.value;
end;

operator=(v: TRDateTime; a: TDateTime): boolean;
begin
    result := v.value = a;
end;

operator=(a: TDateTime; v: TRDateTime): boolean;
begin
    Result := a = v.value;
end;

operator+(b: TRDateTime; a: TDateTime): TRDateTime;
begin
    Result := RDateTime(b.value + a);
end;

operator+(a: TDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a + b.value);
end;

operator+(a: TRDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a.value + b.value);
end;

operator-(b: TRDateTime; a: TDateTime): TRDateTime;
begin
    Result := RDateTime(b.val - a);
end;

operator-(a: TDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a - b.val);
end;

operator-(a: TRDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a.val - b.val);
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

function TReactive.listenUndo(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGUNDO, _subscriber, _e, qSerial);
end;

function TReactive.listenRedo(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGREDO, _subscriber, _e, qSerial);
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
            myHistory.add(_v);
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

function GReactive.memdump: string;
begin
    Result := '';
end;

function GReactive.histVal(_pos: integer): T;
begin
    Result := myHistory.histVal(_pos);
end;

function GReactive.undo(_count: integer): T;
begin
    EnterCS;
    myHistory.Undo(_count);
    myValue := myHistory.currVal;
    signal(SGUNDO);
    signal(SGWRITE);
    leaveCS;
    Result := myValue;
end;

function GReactive.redo(_count: integer): T;
begin
    EnterCS;
    myHistory.redo(_count);
    myValue := myHistory.currVal;
    signal(SGREDO);
    signal(SGWRITE);
    leaveCS;
    Result := myValue;
end;


constructor GReactive.Create;
begin
	inherited Create;
    myHistory := SUndoHistory.Create;
end;

destructor GReactive.Destroy;
begin
    myHistory.Free;
	inherited Destroy;
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

