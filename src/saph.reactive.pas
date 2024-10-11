unit saph.reactive;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, fgl;
type
    	{ TReactive }

    TReactive = class
    private
        myManaged : boolean; // Determines if this unit handles freeing the objects after use.
        myName: string;
        function getName: string; overload;
        procedure setName(const _n: string); overload;
    public
        const SGREAD = 'sig_read';
        const SGWRITE = 'sig_write';
    public

        constructor Create;
        destructor Destroy; override;
    public
        function value: variant; overload; virtual;       // getter
        procedure value(_v: variant); overload; virtual;   // setter
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Adding read listener
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TReactive; virtual; // Add write listener
    published
        property name: string read getName write setName;
	end;

	{ TRInt }

    TRInt = class(TReactive)
    protected
        myValue: integer;
    public
        function value: integer; overload; reintroduce;
        procedure value(_v: integer); overload; reintroduce;
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRInt; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRInt; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRInt64; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRInt64; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRDWord; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRDWord; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRQWord; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRQWord; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRFloat; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRFloat; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRStr; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRStr; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRBool; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRBool; reintroduce;
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
        function reader(constref _subscriber: TObject; _e: TNotifyEvent):TRDateTime; reintroduce;
        function writer(constref _subscriber: TObject; _e: TNotifyEvent):TRDateTime; reintroduce;
    published
        property val: TDateTime read value write value;
    end;
    // Factory functions
    function RInt       : TRInt;
    function RInt64     : TRInt64;
    function RDWord     : TRDWord;
    function RQWord     : TRQWord;
	function RFloat     : TRFloat;
	function RStr       : TRStr;
	function RBool      : TRBool;
	function RDateTime  : TRDateTime;

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

function RInt: TRInt;
begin
    Result := TRInt.Create;
    addToStore(Result);
end;

function RInt64: TRInt64;
begin
    Result := TRInt64.Create;
    addToStore(Result);
end;

function RDWord: TRDWord;
begin
    Result := TRDWord.Create;
    addToStore(Result);
end;

function RQWord: TRQWord;
begin
    Result := TRQWord.Create;
    addToStore(Result);
end;

function RFloat: TRFloat;
begin
    Result := TRFloat.Create;
    addToStore(Result);
end;

function RStr: TRStr;
begin
    Result := TRStr.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RBool: TRBool;
begin
    Result := TRBool.Create;
    rStore.Add(pointerAsHex(Result), Result);
end;

function RDateTime: TRDateTime;
begin
    Result := TRDateTime.Create;
    rStore.Add(pointerAsHex(Result), Result);
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

procedure TReactive.setName(const _n: string);
begin
    if myName.isEmpty then
        myName := _n
    else
        raise Exception.Create('Name can only be set once. ' + sLinebreak
        + 'Right now Name = "' + myName+'"');
end;

constructor TReactive.Create;
begin
    inherited;
    myName:= '';
    myManaged := false;
end;

destructor TReactive.Destroy;
begin
    stopListening;
    inherited Destroy;
end;

function TReactive.value: variant;
begin
    //signal(SGREAD);
end;

procedure TReactive.value(_v: variant);
begin
    signal(SGWRITE);
end;

function TReactive.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGREAD, _subscriber, _e);
end;

function TReactive.writer(constref _subscriber: TObject; _e: TNotifyEvent
	): TReactive;
begin
    Result := self;
    addListener(SGWRITE, _subscriber, _e);
end;

{ TRInt }

function TRInt.value: integer;
begin
    inherited;
    Result := myValue;
end;

procedure TRInt.value(_v: integer);
begin
    if (myValue <> _v) then begin
        myValue := _v;
        signal(SGWRITE);
	end;
end;

function TRInt.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRInt;
begin
    Result := TRInt(inherited);
end;

function TRInt.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRInt64.reader(constref _subscriber: TObject; _e: TNotifyEvent): TRInt64;
begin
    Result := TRInt64(inherited);
end;

function TRInt64.writer(constref _subscriber: TObject; _e: TNotifyEvent): TRInt64;
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

function TRDWord.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDWord;
begin
    Result := TRDWord(inherited);
end;

function TRDWord.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRQWord.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRQWord;
begin
    Result := TRQWord(inherited);
end;

function TRQWord.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRFloat.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRFloat;
begin
    Result:= TRFloat(inherited);
end;

function TRFloat.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRStr.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRStr;
begin
    Result := TRStr(inherited);
end;

function TRStr.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRBool.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRBool;
begin
    Result := TRBool(inherited);
end;

function TRBool.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

function TRDateTime.reader(constref _subscriber: TObject; _e: TNotifyEvent
	): TRDateTime;
begin
    Result := TRDateTime(inherited);
end;

function TRDateTime.writer(constref _subscriber: TObject; _e: TNotifyEvent
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

