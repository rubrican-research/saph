unit saph.reactive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, fgl, saph.undo;

type
    { TReactiveStore }
    TReactiveStore = class;
    TProcReactiveOpAllowed = function (_rstore: TReactiveStore): boolean;

    TReactiveStore = class
    private
        myEnableHistory: boolean;
        myManaged: boolean;
        // Determines if this unit handles freeing the objects after use.
        myName: string;
        {locking}
        myLockExclusive: boolean;
        myLockProcessID: SizeUInt;
        myLockThreadID: TThreadID;
        myKey: string;
		myonCanread: TProcReactiveOpAllowed;
		myonCanRedo: TProcReactiveOpAllowed;
		myonCanUndo: TProcReactiveOpAllowed;
		myonCanWrite: TProcReactiveOpAllowed;
        mySilentLock: boolean;
        function getName: string; overload;
        procedure setEnableHistory(AValue: boolean);
        procedure setName(const _n: string); overload;
        function getLocked: boolean;
        function setLock: string;
        procedure clearLock;
		procedure setonCanread(const _value: TProcReactiveOpAllowed);
		procedure setonCanRedo(const _value: TProcReactiveOpAllowed);
		procedure setonCanUndo(const _value: TProcReactiveOpAllowed);
		procedure setonCanWrite(const _value: TProcReactiveOpAllowed);
    protected
        function makeKey(): string;
    public
    const
        SGNAME  = 'sig_r_name';
        SGREAD  = 'sig_r_read';
        SGWRITE = 'sig_r_write';
        SGUNDO  = 'sig_r_undo';
        SGREDO  = 'sig_r_redo';
    public
        constructor Create; virtual;
        destructor Destroy; override;
    public
        property onCanread: TProcReactiveOpAllowed read myonCanread write setonCanread;
        property onCanWrite: TProcReactiveOpAllowed read myonCanWrite write setonCanWrite;
        property onCanUndo: TProcReactiveOpAllowed read myonCanUndo write setonCanUndo;
        property onCanRedo: TProcReactiveOpAllowed read myonCanRedo write setonCanRedo;
        function onRead(constref _subscriber: TObject; _e: TNotifyEvent): TReactiveStore;
            virtual; // Adding read listener
        function onWrite(constref _subscriber: TObject; _e: TNotifyEvent): TReactiveStore;
            virtual; // Add write listener
        function onUndo(constref _subscriber: TObject; _e: TNotifyEvent): TReactiveStore;
            virtual; // Add Undo listener
        function onRedo(constref _subscriber: TObject; _e: TNotifyEvent): TReactiveStore;
            virtual; // Add Redo listener

        // Crtical section
        procedure enterCS;
        procedure leaveCS;

        function Value: variant; overload; virtual;     // getter
        procedure Value(_v: variant); overload; virtual; // setter
        procedure Value(_v: variant; _req: string; _key: string);
            overload; virtual;// writes a value without unlocking

        {lock function}
        function lock(): string;   // locks the value and returns a key. Returns empty string if already locked;
        function lockEx(): string; // locks the value exclusively. cannot be borrowed.
        function unlock(_key: string): boolean;
        function borrow: string; // Forces change of lock
        function canChangeValue: boolean;
        function isMyLock: boolean; // returns true if locked by the process and thread.

    published
        property Name: string read getName write setName;
        property locked: boolean read getLocked;
        // silentLock
        //      true: raises an exception when assigning a value to locked object
        //      false: does not change value. exits silently.
        property silentLock: boolean read mySilentLock write mySilentLock;
        // Enables undo and redo
        property enableHistory: boolean read myEnableHistory write setEnableHistory;
    public
        property val: variant read Value write Value;
    end;

    { GReactive }

    generic GReactiveStore <T> = class(TReactiveStore)
    private
    type SUndoHistory = class(specialize GUndoHistory<T>);
    private
        myValue: T;
        myHistory: SUndoHistory;

    public
        function Value: T; overload; reintroduce;      // getter
        procedure Value(_v: T); overload; reintroduce; // setter
        procedure Value(_v: T; _req: string; _key: string);
        // writes a value without unlocking
        function memdump: string;

        function histVal(_pos: integer): T;
        function undo(_count: integer = 1): T; reintroduce;
        function redo(_count: integer = 1): T; reintroduce;
    public
        property val: T read Value write Value;
    public
        constructor Create; override;
        destructor Destroy; override;
    end;




    { TRInt }

    TRInt = class(specialize GReactiveStore<integer>);

    { TRInt64 }

    TRInt64 = class(specialize GReactiveStore<int64>);

    { TRDWord }

    TRDWord = class(specialize GReactiveStore<DWord>);

    { TRQWord }

    TRQWord = class(specialize GReactiveStore<QWord>);


    { TRFloat }

    TRFloat = class(specialize GReactiveStore<double>);

    { TRString }

    TRString = class(specialize GReactiveStore<string>)

    end;

    { TRBool }

    TRBool = class(specialize GReactiveStore<boolean>);

    { TRDateTime }

    TRDateTime = class(specialize GReactiveStore<TDateTime>);


// Factory functions
function RInt: TRInt; overload;
function RInt(_default: integer; _name: string = ''): TRInt; overload;

function RInt64: TRInt64; overload;
function RInt64(_default: int64; _name: string = ''): TRInt64; overload;

function RDWord: TRDWord; overload;
function RDWord(_default: DWord; _name: string = ''): TRDWord; overload;

function RQWord: TRQWord; overload;
function RQWord(_default: QWord; _name: string = ''): TRQWord; overload;

function RFloat: TRFloat; overload;
function RFloat(_default: double; _name: string = ''): TRFloat; overload;

function RStr: TRString; overload;
function RStr(_default: string; _name: string = ''): TRString; overload;

function RBool: TRBool; overload;
function RBool(_default: boolean; _name: string = ''): TRBool; overload;

function RDateTime: TRDateTime; overload;
function RDateTime(_default: TDateTime; _name: string = ''): TRDateTime; overload;

// Destructors
function rFree(var _r: TReactiveStore): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRInt): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRInt64): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRDWord): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRQWord): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRFloat): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRString): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRBool): integer; // syntax sugar for rmFromStore()
function rFree(var _r: TRDateTime): integer; // syntax sugar for rmFromStore()

// Manage
function addToStore(_r: TReactiveStore): TReactiveStore;
function rmFromStore(_r: TReactiveStore): integer; // Index where it was located


// Cloner
function rClone(var _r: TRInt): TRInt;
function rClone(var _r: TRInt64): TRInt64;
function rClone(var _r: TRDWord): TRDWord;
function rClone(var _r: TRQWord): TRQWord;
function rClone(var _r: TRFloat): TRFloat;
function rClone(var _r: TRString): TRString;
function rClone(var _r: TRBool): TRBool;
function rClone(var _r: TRDateTime): TRDateTime;


{========= OPERATOR OVERLOADING ============================}
{TRInt      }
operator := (v: TRInt): integer;
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
operator /(b: TRInt; a: integer): TRInt;
// integer division. for normal division, use a.val / b.val
operator /(a: integer; b: TRInt): TRInt;
operator /(a: TRInt; b: TRInt): TRInt;
{GREATER THAN}
operator >(b: TRInt; a: integer): boolean;
operator >(a: integer; b: TRInt): boolean;
operator >(a: TRInt; b: TRInt): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRInt; a: integer): boolean;
operator >=(a: integer; b: TRInt): boolean;
operator >=(a: TRInt; b: TRInt): boolean;
{LESS THAN}
operator <(b: TRInt; a: integer): boolean;
operator <(a: integer; b: TRInt): boolean;
operator <(a: TRInt; b: TRInt): boolean;
{LESS THAN EQUAL}
operator <=(b: TRInt; a: integer): boolean;
operator <=(a: integer; b: TRInt): boolean;
operator <=(a: TRInt; b: TRInt): boolean;
{NOT EQUAL}
operator <>(b: TRInt; a: integer): boolean;
operator <>(a: integer; b: TRInt): boolean;

{TRInt64    }
operator := (v: TRInt64): int64;
operator =(v: TRInt64; a: int64): boolean;
operator =(a: int64; v: TRInt64): boolean;
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
operator /(b: TRInt64; a: int64): TRInt64;
// integer division. for normal division, use a.val / b.val
operator /(a: int64; b: TRInt64): TRInt64;
operator /(a: TRInt64; b: TRInt64): TRInt64;
{GREATER THAN}
operator >(b: TRInt64; a: int64): boolean;
operator >(a: int64; b: TRInt64): boolean;
operator >(a: TRInt64; b: TRInt64): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRInt64; a: int64): boolean;
operator >=(a: int64; b: TRInt64): boolean;
operator >=(a: TRInt64; b: TRInt64): boolean;
{LESS THAN}
operator <(b: TRInt64; a: int64): boolean;
operator <(a: int64; b: TRInt64): boolean;
operator <(a: TRInt64; b: TRInt64): boolean;
{LESS THAN EQUAL}
operator <=(b: TRInt64; a: int64): boolean;
operator <=(a: int64; b: TRInt64): boolean;
operator <=(a: TRInt64; b: TRInt64): boolean;
{NOT EQUAL}
operator <>(b: TRInt64; a: int64): boolean;
operator <>(a: int64; b: TRInt64): boolean;

{TRDWord    }
operator := (v: TRDWord): DWord;
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
operator /(b: TRDWord; a: DWord): TRDWord;
// integer division. for normal division, use a.val / b.val
operator /(a: DWord; b: TRDWord): TRDWord;
operator /(a: TRDWord; b: TRDWord): TRDWord;
{GREATER THAN}
operator >(b: TRDWord; a: DWord): boolean;
operator >(a: DWord; b: TRDWord): boolean;
operator >(a: TRDWord; b: TRDWord): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRDWord; a: DWord): boolean;
operator >=(a: DWord; b: TRDWord): boolean;
operator >=(a: TRDWord; b: TRDWord): boolean;
{LESS THAN}
operator <(b: TRDWord; a: DWord): boolean;
operator <(a: DWord; b: TRDWord): boolean;
operator <(a: TRDWord; b: TRDWord): boolean;
{LESS THAN EQUAL}
operator <=(b: TRDWord; a: DWord): boolean;
operator <=(a: DWord; b: TRDWord): boolean;
operator <=(a: TRDWord; b: TRDWord): boolean;
{NOT EQUAL}
operator <>(b: TRDWord; a: DWord): boolean;
operator <>(a: DWord; b: TRDWord): boolean;

{TRQWord    }
operator := (v: TRQWord): QWord;
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
operator /(b: TRQWord; a: QWord): TRQWord;
// integer division. for normal division, use a.val / b.val
operator /(a: QWord; b: TRQWord): TRQWord;
operator /(a: TRQWord; b: TRQWord): TRQWord;
{GREATER THAN}
operator >(b: TRQWord; a: QWord): boolean;
operator >(a: QWord; b: TRQWord): boolean;
operator >(a: TRQWord; b: TRQWord): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRQWord; a: QWord): boolean;
operator >=(a: QWord; b: TRQWord): boolean;
operator >=(a: TRQWord; b: TRQWord): boolean;
{LESS THAN}
operator <(b: TRQWord; a: QWord): boolean;
operator <(a: QWord; b: TRQWord): boolean;
operator <(a: TRQWord; b: TRQWord): boolean;
{LESS THAN EQUAL}
operator <=(b: TRQWord; a: QWord): boolean;
operator <=(a: QWord; b: TRQWord): boolean;
operator <=(a: TRQWord; b: TRQWord): boolean;
{NOT EQUAL}
operator <>(b: TRQWord; a: QWord): boolean;
operator <>(a: QWord; b: TRQWord): boolean;

{TRFloat    }
operator := (v: TRFloat): double;
operator =(v: TRFloat; a: double): boolean;
operator =(a: double; v: TRFloat): boolean;
{ADDITION}
operator +(b: TRFloat; a: double): TRFloat;
operator +(a: double; b: TRFloat): TRFloat;
operator +(a: TRFloat; b: TRFloat): TRFloat;
{SUBTRACTION}
operator -(b: TRFloat; a: double): TRFloat;
operator -(a: double; b: TRFloat): TRFloat;
operator -(a: TRFloat; b: TRFloat): TRFloat;
operator -(a: TRFloat): TRFloat;
{MULTIPLICATION}
operator *(b: TRFloat; a: double): TRFloat;
operator *(a: double; b: TRFloat): TRFloat;
operator *(a: TRFloat; b: TRFloat): TRFloat;
{DIVISION}
operator /(b: TRFloat; a: double): TRFloat;
// integer division. for normal division, use a.val / b.val
operator /(a: double; b: TRFloat): TRFloat;
operator /(a: TRFloat; b: TRFloat): TRFloat;
{GREATER THAN}
operator >(b: TRFloat; a: double): boolean;
operator >(a: double; b: TRFloat): boolean;
operator >(a: TRFloat; b: TRFloat): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRFloat; a: double): boolean;
operator >=(a: double; b: TRFloat): boolean;
operator >=(a: TRFloat; b: TRFloat): boolean;
{LESS THAN}
operator <(b: TRFloat; a: double): boolean;
operator <(a: double; b: TRFloat): boolean;
operator <(a: TRFloat; b: TRFloat): boolean;
{LESS THAN EQUAL}
operator <=(b: TRFloat; a: double): boolean;
operator <=(a: double; b: TRFloat): boolean;
operator <=(a: TRFloat; b: TRFloat): boolean;
{NOT EQUAL}
operator <>(b: TRFloat; a: double): boolean;
operator <>(a: double; b: TRFloat): boolean;

{TRStr}
operator := (v: TRString): string;
operator =(v: TRString; a: string): boolean;
operator =(a: string; v: TRString): boolean;
operator +(b: TRString; a: string): TRString;
operator +(a: string; b: TRString): TRString;
operator +(a: TRString; b: TRString): TRString;
{NOT EQUAL}
operator <>(b: TRString; a: string): boolean;
operator <>(a: string; b: TRString): boolean;

{TRBool}
operator := (v: TRBool): boolean;
operator =(v: TRBool; a: boolean): boolean;
operator =(a: boolean; v: TRBool): boolean;
{NOT EQUAL}
operator <>(b: TRBool; a: boolean): boolean;
operator <>(a: boolean; b: TRBool): boolean;
{TRDateTime}
operator := (v: TRDateTime): TDateTime;
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

{GREATER THAN}
operator >(b: TRDateTime; a: TDateTime): boolean;
operator >(a: TDateTime; b: TRDateTime): boolean;
operator >(a: TRDateTime; b: TRDateTime): boolean;
{GREATER THAN EQUAL}
operator >=(b: TRDateTime; a: TDateTime): boolean;
operator >=(a: TDateTime; b: TRDateTime): boolean;
operator >=(a: TRDateTime; b: TRDateTime): boolean;
{LESS THAN}
operator <(b: TRDateTime; a: TDateTime): boolean;
operator <(a: TDateTime; b: TRDateTime): boolean;
operator <(a: TRDateTime; b: TRDateTime): boolean;
{LESS THAN EQUAL}
operator <=(b: TRDateTime; a: TDateTime): boolean;
operator <=(a: TDateTime; b: TRDateTime): boolean;
operator <=(a: TRDateTime; b: TRDateTime): boolean;
{NOT EQUAL}
operator <>(b: TRDateTime; a: TDateTime): boolean;
operator <>(a: TDateTime; b: TRDateTime): boolean;


var
    rStoreCS: TRTLCriticalSection;

implementation

uses
    obj.Listener, strutils, sugar.logger, Math;

    {============ MANAGED VARIABLES =========================}
type
    TRStore = class(specialize TFPGMapObject<string, TReactiveStore>);

var
    rStore: TRStore; // initialization, finalization


function pointerAsHex(_obj: pointer): string;
begin
    Result := PtrUInt(_obj).ToHexString(16);
end;

function hexStrAsPointer(_hex: string): Pointer;
begin
    Result := pointer(Hex2Dec64(_hex));
end;

{=========================================================}

function addToStore(_r: TReactiveStore): TReactiveStore;
begin
    Result := _r;
    Result.myManaged := True;
    rStore.Add(pointerAsHex(Result), Result);
end;

function rmFromStore(_r: TReactiveStore): integer;
begin
    log('rmFromStore of ' + _r.ClassName);
    Result := rStore.IndexOf(pointerAsHex(_r));
    if Result > -1 then
        rStore.Delete(Result);
end;

procedure rCopyVars(constref _source: TReactiveStore; constref _dest: TReactiveStore);
begin
    _dest.myEnableHistory := _source.myEnableHistory;
    _dest.myManaged := _source.myManaged;
    _dest.myName := _source.myName;
    _dest.myLockExclusive := _source.myLockExclusive;
    _dest.myLockProcessID := _source.myLockProcessID;
    _dest.myLockThreadID := _source.myLockThreadID;
    _dest.myKey := _source.myKey;
    _dest.mySilentLock := _source.mySilentLock;
end;

function rClone(var _r: TRInt): TRInt;
begin
    Result := RInt();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRInt64): TRInt64;
begin
    Result := RInt64();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRDWord): TRDWord;
begin
    Result := RDWord();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRQWord): TRQWord;
begin
    Result := RQWord();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRFloat): TRFloat;
begin
    Result := RFloat();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRString): TRString;
begin
    Result := RStr();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRBool): TRBool;
begin
    Result := RBool();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

function rClone(var _r: TRDateTime): TRDateTime;
begin
    Result := RDateTime();
    rCopyVars(_r, Result);
    Result.myValue := _r.myValue;
    Result.myHistory := _r.myHistory;
end;

operator := (v: TRInt): integer;
begin
    Result := v.Value();
end;

operator =(v: TRInt; a: integer): boolean;
begin
    Result := (v.Value = a);
end;

operator =(a: integer; v: TRInt): boolean;
begin
    Result := a = v.Value();
end;

operator +(a: TRInt): TRInt;
begin
    Result := RInt(a.val);
end;

operator +(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.Value + a);
end;

operator +(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a + b.Value);
end;

operator +(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.Value + b.Value);
end;

operator -(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val - a);
end;

operator -(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a - b.val);
end;

operator -(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val - b.val);
end;

operator -(a: TRInt): TRInt;
begin
    Result := RInt(-1 * (a.val));
end;

operator *(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val * a);
end;

operator *(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a * b.Val);
end;

operator *(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val * b.val);
end;

operator /(b: TRInt; a: integer): TRInt;
begin
    Result := RInt(b.val div a);
end;

operator /(a: integer; b: TRInt): TRInt;
begin
    Result := RInt(a div b.val);
end;

operator /(a: TRInt; b: TRInt): TRInt;
begin
    Result := RInt(a.val div b.val);
end;

operator>(b: TRInt; a: integer): boolean;
begin
    Result := b.val > a;
end;

operator>(a: integer; b: TRInt): boolean;
begin
    Result := a > b.val;
end;

operator>(a: TRInt; b: TRInt): boolean;
begin
    Result := a.val > b.val;
end;

operator>=(b: TRInt; a: integer): boolean;
begin
    Result := b.val >= a;
end;

operator>=(a: integer; b: TRInt): boolean;
begin
    Result := a >= b.val;
end;

operator>=(a: TRInt; b: TRInt): boolean;
begin
    Result := a.val >= b.val;
end;

operator<(b: TRInt; a: integer): boolean;
begin
    Result := b.val < a;
end;

operator<(a: integer; b: TRInt): boolean;
begin
    Result := a < b.val;
end;

operator<(a: TRInt; b: TRInt): boolean;
begin
    Result := a.val < b.val;
end;

operator<=(b: TRInt; a: integer): boolean;
begin
    Result := b.val <= a;
end;

operator<=(a: integer; b: TRInt): boolean;
begin
    Result := a <= b.val;
end;

operator<=(a: TRInt; b: TRInt): boolean;
begin
    Result := a.val <= b.val;
end;

operator<>(b: TRInt; a: integer): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: integer; b: TRInt): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRInt64): int64;
begin
    Result := v.Value;
end;

operator =(v: TRInt64; a: int64): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: int64; v: TRInt64): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.Value + a);
end;

operator +(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a + b.Value);
end;

operator +(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.Value + b.Value);
end;

operator -(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val - a);
end;

operator -(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a - b.val);
end;

operator -(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val - b.val);
end;

operator -(a: TRInt64): TRInt64;
begin
    Result := RInt64(-1 * a.val);
end;

operator *(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val * a);
end;

operator *(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a * b.val);
end;

operator *(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val * b.val);
end;

operator /(b: TRInt64; a: int64): TRInt64;
begin
    Result := RInt64(b.val div a);
end;

operator /(a: int64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a div b.val);
end;

operator /(a: TRInt64; b: TRInt64): TRInt64;
begin
    Result := RInt64(a.val div b.val);
end;

operator>(b: TRInt64; a: int64): boolean;
begin
    Result := b.val > a;
end;

operator>(a: int64; b: TRInt64): boolean;
begin
    Result := a <> b.val ;
end;

operator>(a: TRInt64; b: TRInt64): boolean;
begin
    Result := a.val <> b.val;
end;

operator>=(b: TRInt64; a: int64): boolean;
begin
    Result := b.val >= a;
end;

operator>=(a: int64; b: TRInt64): boolean;
begin
    Result := a >= b.val;
end;

operator>=(a: TRInt64; b: TRInt64): boolean;
begin
    Result := a.val >= b.val;
end;

operator<(b: TRInt64; a: int64): boolean;
begin
    Result := b.val < a;
end;

operator<(a: int64; b: TRInt64): boolean;
begin
    Result := a < b.val;
end;

operator<(a: TRInt64; b: TRInt64): boolean;
begin
    Result := a.val < b.val;
end;

operator<=(b: TRInt64; a: int64): boolean;
begin
    Result := b.val <= a;
end;

operator<=(a: int64; b: TRInt64): boolean;
begin
    Result := a <= b.val;
end;

operator<=(a: TRInt64; b: TRInt64): boolean;
begin
    Result := a.val <= b.val;
end;

operator<>(b: TRInt64; a: int64): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: int64; b: TRInt64): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRDWord): DWord;
begin
    Result := v.Value;
end;

operator =(v: TRDWord; a: DWord): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: DWord; v: TRDWord): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.Value + a);
end;

operator +(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a + b.Value);
end;

operator +(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.Value + b.Value);
end;

operator -(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val - a);
end;

operator -(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a - b.val);
end;

operator -(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val - b.val);
end;

operator -(a: TRDWord): TRDWord;
begin
    Result := RDWord(-1 * a.val);
end;

operator *(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val * a);
end;

operator *(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a * b.val);
end;

operator *(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val * b.val);
end;

operator /(b: TRDWord; a: DWord): TRDWord;
begin
    Result := RDWord(b.val div a);
end;

operator /(a: DWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a div b.val);
end;

operator /(a: TRDWord; b: TRDWord): TRDWord;
begin
    Result := RDWord(a.val div b.val);
end;

operator>(b: TRDWord; a: DWord): boolean;
begin
    Result := b.val > a;
end;

operator>(a: DWord; b: TRDWord): boolean;
begin
    Result := a > b.val;
end;

operator>(a: TRDWord; b: TRDWord): boolean;
begin
    Result := a.val > b.val;
end;

operator>=(b: TRDWord; a: DWord): boolean;
begin
    Result := b.val >= a ;
end;

operator>=(a: DWord; b: TRDWord): boolean;
begin
    Result := a >= b.val;
end;

operator>=(a: TRDWord; b: TRDWord): boolean;
begin
    Result := a.val >= b.val;
end;

operator<(b: TRDWord; a: DWord): boolean;
begin
    Result := b.val < a;
end;

operator<(a: DWord; b: TRDWord): boolean;
begin
    Result := a >= b.val;
end;

operator<(a: TRDWord; b: TRDWord): boolean;
begin
    Result := a.val >= b.val;
end;

operator<=(b: TRDWord; a: DWord): boolean;
begin
    Result :=  b.val <= a;
end;

operator<=(a: DWord; b: TRDWord): boolean;
begin
    Result := a <= b.val;
end;

operator<=(a: TRDWord; b: TRDWord): boolean;
begin
    Result := a.val <= b.val;
end;

operator<>(b: TRDWord; a: DWord): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: DWord; b: TRDWord): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRQWord): QWord;
begin
    Result := v.Value;
end;

operator =(v: TRQWord; a: QWord): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: QWord; v: TRQWord): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.Value + a);
end;

operator +(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a + b.Value);
end;

operator +(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.Value + b.Value);
end;

operator -(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val - a);
end;

operator -(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a - b.val);
end;

operator -(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val - b.val);
end;


operator *(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val * a);
end;

operator *(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a * b.val);
end;

operator *(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val * b.val);
end;

operator /(b: TRQWord; a: QWord): TRQWord;
begin
    Result := RQWord(b.val div a);
end;

operator /(a: QWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a div b.val);
end;

operator /(a: TRQWord; b: TRQWord): TRQWord;
begin
    Result := RQWord(a.val div b.val);
end;

operator>(b: TRQWord; a: QWord): boolean;
begin
    Result := b.val > a;
end;

operator>(a: QWord; b: TRQWord): boolean;
begin
    Result := a > b.val;
end;

operator>(a: TRQWord; b: TRQWord): boolean;
begin
    Result := a.val > b.val;
end;

operator>=(b: TRQWord; a: QWord): boolean;
begin
    Result := b.val >= a ;
end;

operator>=(a: QWord; b: TRQWord): boolean;
begin
    Result := a >= b.val;
end;

operator>=(a: TRQWord; b: TRQWord): boolean;
begin
    Result := a.val >= b.val;
end;

operator<(b: TRQWord; a: QWord): boolean;
begin
    Result := b.val < a;
end;

operator<(a: QWord; b: TRQWord): boolean;
begin
    Result := a >= b.val;
end;

operator<(a: TRQWord; b: TRQWord): boolean;
begin
    Result := a.val >= b.val;
end;

operator<=(b: TRQWord; a: QWord): boolean;
begin
    Result :=  b.val <= a;
end;

operator<=(a: QWord; b: TRQWord): boolean;
begin
    Result := a <= b.val;
end;

operator<=(a: TRQWord; b: TRQWord): boolean;
begin
    Result := a.val <= b.val;
end;

operator<>(b: TRQWord; a: QWord): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: QWord; b: TRQWord): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRFloat): double;
begin
    Result := v.Value;
end;

operator =(v: TRFloat; a: double): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: double; v: TRFloat): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRFloat; a: double): TRFloat;
begin
    Result := RFloat(b.Value + a);
end;

operator +(a: double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a + b.Value);
end;

operator +(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.Value + b.Value);
end;

operator -(b: TRFloat; a: double): TRFloat;
begin
    Result := RFloat(b.val - a);
end;

operator -(a: double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a - b.val);
end;

operator -(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val - b.val);
end;

operator -(a: TRFloat): TRFloat;
begin
    Result := RFloat(-1 * a.val);
end;

operator *(b: TRFloat; a: double): TRFloat;
begin
    Result := RFloat(b.val * a);
end;

operator *(a: double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a * b.val);
end;

operator *(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val * b.val);
end;

operator /(b: TRFloat; a: double): TRFloat;
begin
    Result := RFloat(b.val / a);
end;

operator /(a: double; b: TRFloat): TRFloat;
begin
    Result := RFloat(a / b.val);
end;

operator /(a: TRFloat; b: TRFloat): TRFloat;
begin
    Result := RFloat(a.val / b.val);
end;

operator>(b: TRFloat; a: double): boolean;
begin
    Result := b.val > a;
end;

operator>(a: double; b: TRFloat): boolean;
begin
    Result := a > b.val;
end;

operator>(a: TRFloat; b: TRFloat): boolean;
begin
    Result := a.val > b.val;
end;

operator>=(b: TRFloat; a: double): boolean;
begin
    Result := b.val >= a ;
end;

operator>=(a: double; b: TRFloat): boolean;
begin
    Result := a >= b.val;
end;

operator>=(a: TRFloat; b: TRFloat): boolean;
begin
    Result := a.val >= b.val;
end;

operator<(b: TRFloat; a: double): boolean;
begin
    Result := b.val < a;
end;

operator<(a: double; b: TRFloat): boolean;
begin
    Result := a >= b.val;
end;

operator<(a: TRFloat; b: TRFloat): boolean;
begin
    Result := a.val >= b.val;
end;

operator<=(b: TRFloat; a: double): boolean;
begin
    Result :=  b.val <= a;
end;

operator<=(a: double; b: TRFloat): boolean;
begin
    Result := a <= b.val;
end;

operator<=(a: TRFloat; b: TRFloat): boolean;
begin
    Result := a.val <= b.val;
end;

operator<>(b: TRFloat; a: double): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: double; b: TRFloat): boolean;
begin
    Result := a <> b.val;
end;


operator := (v: TRString): string;
begin
    Result := v.val;
end;

operator =(v: TRString; a: string): boolean;
begin
    Result := (v.val = a);
end;

operator =(a: string; v: TRString): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRString; a: string): TRString;
begin
    Result := RStr();
    Result.Val := b.Val + a;
end;

operator +(a: string; b: TRString): TRString;
begin
    Result := RStr();
    Result.Val := b.Val + a;
end;

operator +(a: TRString; b: TRString): TRString;
begin
    Result := RStr(a.val + b.val);
end;

operator<>(b: TRString; a: string): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: string; b: TRString): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRBool): boolean;
begin
    Result := v.Value;
end;

operator =(v: TRBool; a: boolean): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: boolean; v: TRBool): boolean;
begin
    Result := a = v.Value;
end;

operator<>(b: TRBool; a: boolean): boolean;
begin
    Result := b.val <> a;
end;

operator<>(a: boolean; b: TRBool): boolean;
begin
    Result := a <> b.val;
end;

operator := (v: TRDateTime): TDateTime;
begin
    Result := v.Value;
end;

operator =(v: TRDateTime; a: TDateTime): boolean;
begin
    Result := v.Value = a;
end;

operator =(a: TDateTime; v: TRDateTime): boolean;
begin
    Result := a = v.Value;
end;

operator +(b: TRDateTime; a: TDateTime): TRDateTime;
begin
    Result := RDateTime(b.Value + a);
end;

operator +(a: TDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a + b.Value);
end;

operator +(a: TRDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a.Value + b.Value);
end;

operator -(b: TRDateTime; a: TDateTime): TRDateTime;
begin
    Result := RDateTime(b.val - a);
end;

operator -(a: TDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a - b.val);
end;

operator -(a: TRDateTime; b: TRDateTime): TRDateTime;
begin
    Result := RDateTime(a.val - b.val);
end;


operator>(b: TRDateTime; a: TDateTime): boolean;
begin

end;

operator>(a: TDateTime; b: TRDateTime): boolean;
begin

end;

operator>(a: TRDateTime; b: TRDateTime): boolean;
begin

end;

operator>=(b: TRDateTime; a: TDateTime): boolean;
begin

end;

operator>=(a: TDateTime; b: TRDateTime): boolean;
begin

end;

operator>=(a: TRDateTime; b: TRDateTime): boolean;
begin

end;

operator<(b: TRDateTime; a: TDateTime): boolean;
begin

end;

operator<(a: TDateTime; b: TRDateTime): boolean;
begin

end;

operator<(a: TRDateTime; b: TRDateTime): boolean;
begin

end;

operator<=(b: TRDateTime; a: TDateTime): boolean;
begin

end;

operator<=(a: TDateTime; b: TRDateTime): boolean;
begin

end;

operator<=(a: TRDateTime; b: TRDateTime): boolean;
begin

end;

operator<>(b: TRDateTime; a: TDateTime): boolean;
begin

end;

operator<>(a: TDateTime; b: TRDateTime): boolean;
begin

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

function RFloat(_default: double; _name: string): TRFloat;
begin
    Result := RFloat();
    Result.val := _default;
    if not _name.isEmpty then Result.setName(_name);
end;

function RStr: TRString;
begin
    Result := TRString.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RStr(_default: string; _name: string): TRString;
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

generic function rFree(var _r: TReactiveStore): integer; // syntax sugar for rmFromStore()
begin
    Result := rmFromStore(_r);
    _r := nil;
end;

function rFree(var _r: TRInt): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRInt64): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRDWord): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRQWord): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRFloat): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRString): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRBool): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

function rFree(var _r: TRDateTime): integer;
begin
    Result := rFree(TReactiveStore(_r));
end;

{ TReactiveStore }

function TReactiveStore.getName: string;
begin
    Result := myName;
end;

procedure TReactiveStore.setEnableHistory(AValue: boolean);
begin
    if myEnableHistory = AValue then Exit;
    myEnableHistory := AValue;
end;

procedure TReactiveStore.setName(const _n: string);
begin
    if myName.isEmpty then
    begin
        myName := _n;
        signal(SGNAME);
    end
    else
        raise Exception.Create('Name can only be set once. ' +
            sLinebreak + 'Right now Name = "' + myName + '"');
end;

function TReactiveStore.getLocked: boolean;
begin
    Result := not myKey.IsEmpty;
end;

function TReactiveStore.setLock: string;
begin
    EnterCriticalSection(rStoreCS);
    myLockProcessID := GetProcessID;
    myLockThreadID := ThreadID;
    myLockExclusive := False;
    myKey := makeKey();
    LeaveCriticalSection(rStoreCS);
    Result := myKey;
end;

procedure TReactiveStore.clearLock;
begin
    EnterCriticalSection(rStoreCS);
    myLockProcessID := 0;
    myLockThreadID := 0;
    myLockExclusive := False;
    myKey := '';
    LeaveCriticalSection(rStoreCS);
end;

procedure TReactiveStore.setonCanread(const _value: TProcReactiveOpAllowed);
begin
	if myonCanread=_value then Exit;
	myonCanread:=_value;
end;

procedure TReactiveStore.setonCanRedo(const _value: TProcReactiveOpAllowed);
begin
	if myonCanRedo=_value then Exit;
	myonCanRedo:=_value;
end;

procedure TReactiveStore.setonCanUndo(const _value: TProcReactiveOpAllowed);
begin
	if myonCanUndo=_value then Exit;
	myonCanUndo:=_value;
end;

procedure TReactiveStore.setonCanWrite(const _value: TProcReactiveOpAllowed);
begin
	if myonCanWrite=_value then Exit;
	myonCanWrite:=_value;
end;

procedure TReactiveStore.enterCS;
begin
    EnterCriticalSection(rStoreCS);
end;

procedure TReactiveStore.leaveCS;
begin
    LeaveCriticalSection(rStoreCS);
end;

function TReactiveStore.makeKey(): string;
begin
    Result := IntToStr(getTickCount64());
end;

constructor TReactiveStore.Create;
begin
    inherited;
    myName := '';
    myManaged := False;
    mySilentLock := False; // Raise exception if value is being changed after locking
    clearLock;
end;

destructor TReactiveStore.Destroy;
begin
    inherited Destroy;
end;

function TReactiveStore.onRead(constref _subscriber: TObject;
    _e: TNotifyEvent): TReactiveStore;
begin
    Result := self;
    addListener(SGREAD, _subscriber, _e);
end;

function TReactiveStore.onWrite(constref _subscriber: TObject;
    _e: TNotifyEvent): TReactiveStore;
begin
    Result := self;
    addListener(SGWRITE, _subscriber, _e, qSerial);
end;

function TReactiveStore.onUndo(constref _subscriber: TObject;
    _e: TNotifyEvent): TReactiveStore;
begin
    Result := self;
    addListener(SGUNDO, _subscriber, _e, qSerial);
end;

function TReactiveStore.onRedo(constref _subscriber: TObject;
    _e: TNotifyEvent): TReactiveStore;
begin
    Result := self;
    addListener(SGREDO, _subscriber, _e, qSerial);
end;

function TReactiveStore.lock: string;
begin
    if myKey.isEmpty then
    begin
        Result := setLock;
    end
    else
        Result := '';
end;

function TReactiveStore.lockEx(): string;
begin
    if myKey.isEmpty then
    begin
        Result := setLock;

        //EnterCriticalSection(myLockCS);
        myLockExclusive := True;
        //LeaveCriticalSection(myLockCS);
    end
    else
        Result := '';
end;

function TReactiveStore.unlock(_key: string): boolean;
begin
    if (UnicodeSameStr(_key, myKey)) and
        (myLockProcessID = GetProcessID) and (myLockThreadID = ThreadID) then
    begin
        clearLock;
        Result := True;
    end
    else
    begin
        log('FAIL UNLOCK::: same key: %s; processID: %s, thread id: %s', [
            BoolToStr(UnicodeSameStr(_key, myKey)), BoolToStr((myLockProcessID = GetProcessID)),
            BoolToStr((myLockThreadID = ThreadID))]);
        log('FAIL UNLOCK::: _key: "%s"; myKey: "%s"', [_key, myKey]);
        log('FAIL UNLOCK::: myLockPID: "%d"; getProcessID: "%d"', [
            myLockProcessID, GetProcessID]);
        log('FAIL UNLOCK::: myLockThreadID: "%d"; ThreadID: "%d"', [
            myLockThreadID, ThreadID]);
        Result := False;
    end;
end;

function TReactiveStore.borrow: string;
begin
    if not myLockExclusive then
    begin
        Result := setLock;
    end
    else if not mySilentLock then
    begin
        raise Exception.Create(
            Format('TReactive.borrow() %s "%s" is exclusively locked.', [ClassName, Name]));
    end;
end;

function TReactiveStore.canChangeValue: boolean;
begin
    Result := myKey.isEmpty; // yes, change because it is not locked.
    if not Result then
        // it is locked. Allow changes to be made by the process and thread that has locked this
    begin
        Result := isMyLock;
    end;
end;

function TReactiveStore.isMyLock: boolean;
begin
    Result := (myLockProcessID = GetProcessID) and (myLockThreadID = ThreadID);
end;

function TReactiveStore.Value: variant;
begin
    raise Exception.Create('TReactive:: Value should not be called from TReactive');
    Result := nil;
end;

procedure TReactiveStore.Value(_v: variant);
begin
    raise Exception.Create('TReactive:: Value(_v) should not be called from TReactive');
end;

procedure TReactiveStore.Value(_v: variant; _req: string; _key: string);
begin
    raise Exception.Create(
        'TReactive:: Value(_v, _req, _key) should not be called from TReactive');
end;




{ GReactiveStore }


function GReactiveStore.Value: T;
begin
    Result := myValue;
end;

procedure GReactiveStore.Value(_v: T);
begin
    if canChangeValue then
    begin
        if _v <> myValue then
        begin
            enterCS;
            myValue := _v;
            myHistory.add(_v);
            signal(SGWRITE);
            leaveCS;
        end;
    end
    else if not mySilentLock then
        raise Exception.Create(
            Format('Reactive variable(%s) "%s" is locked. Value cannot be written',
            [ClassName, Name]));
end;

procedure GReactiveStore.Value(_v: T; _req: string; _key: string);
begin

end;

function GReactiveStore.memdump: string;
begin
    Result := '';
end;

function GReactiveStore.histVal(_pos: integer): T;
begin
    Result := myHistory.histVal(_pos);
end;

function GReactiveStore.undo(_count: integer): T;
begin
    EnterCS;
    myHistory.Undo(_count);
    myValue := myHistory.currVal;
    signal(SGUNDO);
    signal(SGWRITE);
    leaveCS;
    Result := myValue;
end;

function GReactiveStore.redo(_count: integer): T;
begin
    EnterCS;
    myHistory.redo(_count);
    myValue := myHistory.currVal;
    signal(SGREDO);
    signal(SGWRITE);
    leaveCS;
    Result := myValue;
end;


constructor GReactiveStore.Create;
begin
    inherited Create;
    myHistory := SUndoHistory.Create;
end;

destructor GReactiveStore.Destroy;
begin
    myHistory.Free;
    inherited Destroy;
end;

initialization
    InitCriticalSection(rStoreCS);
    RStore := TRStore.Create(True);
    RStore.Sorted := True;
    RStore.Duplicates := TDuplicates.dupAccept;

finalization
    RStore.Free;
    DoneCriticalSection(rStoreCS);

end.
