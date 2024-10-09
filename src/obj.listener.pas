unit Obj.Listener;
{
(c) Rubrican Research.
https://github.com/rubrican-research/saph

This library is released under the MIT License.

to freely define event listeners - which is text based, case-sensitive - on any object.
This is implemented as a Type Helper on TObject.

Implement event listeners in your units with the following signature
    procedure (const _sender: TObject; const _event: string; constref _params: TJSONObject);

Also an event listener can be TNotify

Then, for any control in that unit (form, button, editbox etc.) you can now assign an event listener as follows:
    edtName.addListener('change', @FormChange);   // Where FormChange is a general listener for all changes to data on the from
    edtDOB.addListener('change', @FormChange);    // EditBox for Date of Birth - change is listened by FormChange
    edtDOB.addListener('change', @CalculateAge);  // Same Date of Birth edit box, the change will be listened by CalculateAge
}



{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, Controls, Forms, fpjson, fgl;

type
    // These are Event Listeners.
    // Implement procedures with this signaturs in the main program and then assign them to the event
    // using addListener.
    // In the implementation of the procedure, you have access to:
    //      _sender:    This is the Control that sent the signal - the control on which addListener was called
    //      _event:     The event. Text.   You can implement a case structure to handle multiple events.
    //      _params:    Parameters as a JSONObject. DO NOT free the object inside your listener procedure!!
    //                  The runner with free it after the procedure is called.
    TObjectListenerProc        = procedure (const _sender: TObject; const _event: string; constref _params: TJSONObject);
    TObjectListenerMethod      = procedure (const _sender: TObject; const _event: string; constref _params: TJSONObject) of object;

    // Syntax sugar. To assign multiple listeners in one go.
    TArrayObjectListenerProc   = array of TObjectListenerProc;
    TArrayObjectListenerMethod = array of TObjectListenerMethod;



    TInvokeType = (
                            qAsync,     // Queues the listeners to Application.QueueAsyncCall();
                            qThreads,   // Runs the methods in individual threads;
                            qSerial     // Runs in a blocking loop
                          );

	{ TObjectlListener }
    TObjectlListener = class
	private
	  myEnabled: boolean;
      sender: TObject;
      event: string;
	  proc: TObjectListenerProc;
	  meth: TObjectListenerMethod;
      notify: TNotifyEvent;
      params: TJSONObject;
      freeParams: boolean;
      sigType: TInvokeType;

      function getAsString: string;
	public
      constructor Create;
	  procedure add(constref _proc: TObjectListenerProc; const _sigType: TInvokeType = qAsync); overload;
	  procedure add(constref _meth: TObjectListenerMethod; const _sigType: TInvokeType = qAsync); overload;
      procedure add(constref _notify: TNotifyEvent; const _sigType: TInvokeType = qAsync); overload;
	  procedure do_(constref _sender: TObject; const _event: string; constref _params: TJSONObject; const _freeParams: Boolean = true);
    public
      beforeDo: function : integer of object;
      afterDo : function : integer of object;
      property enabled: boolean read myEnabled write myEnabled;
      property asString : string read getAsString;

      function getAsJSON: TJSONObject;
	end;


    // List of listeners
	{ TObjectListenerProcList }

    TObjectListenerProcList       = class(specialize TFPGObjectList<TObjectlListener>)
    public
        beforeDo: function : integer of object;
        afterDo : function : integer of object;

        function Add(const Item: TObjectlListener): Integer; inline;
	end;

    // Map of Event and List of listener procedures
	{ TObjectListenerMap }

    TObjectListenerMap = class(specialize TFPGMap<string, TObjectListenerProcList>)
    private
        myActiveCallCount : integer; // count
        myCriticalSection: TRTLCriticalSection;
    public
        constructor Create;
        destructor Destroy; override;
        function Add(const AKey: string; const AData: TObjectListenerProcList): Integer; inline;

        function activeCallCount: integer;
        function incActiveCall: integer; // Returns new count;
        function decActiveCall: integer; // Returns new count;
        procedure wrapUp;
	end;


     // Map of Control ID and List of Event Listeners
	{ TObjectListenerList }
    TObjectListenerList = class(specialize TFPGMap<string, TObjectListenerMap>)
        procedure Clear; reintroduce;
        destructor Destroy; override;
	end;

	{ TObjectListenerHelper }

    TObjectListenerHelper = class helper for TObject

        function myListeners: TObjectListenerMap;                     // Returns the list of listeners for this particular object!
        function myListener(const _event: string): TObjectListenerProcList;  // Returns the proc list for this particular event

        function addListener(
                        const _event: string;
                        const _handler: TObjectListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;
        {Array of events -> single listener Method}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TObjectListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;
        {Array of Listener Methods}
        function addListener(
                        const _event: string;
                        const _handlers: TArrayObjectListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;

        function addListener(
                        const _event: string;
                        const _handler: TObjectListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;

        {Array of events -> single listener Proc}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TObjectListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;
        {Array of Listener Procs}
        function addListener(
                        const _event: string;
                        const _handlers: TArrayObjectListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;

        function addListener(
                        const _event: string;
                        const _handler: TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;

        {Array of events -> single listener Notify Event}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;

        {Array of Notify Events}
        function addListener(
                        const _event: string;
                        const _handlers: array of TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TObject; overload;
        // TODO - not implemented
        procedure rmListeners; // Removes all listeners
        procedure rmListeners(const _event: string); unimplemented;
        procedure rmListener(const _event: string; const _handler: TObjectListenerMethod); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: TArrayObjectListenerMethod); overload; unimplemented;
        procedure rmListener(const _event: string; const _handler: TObjectListenerProc); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: TArrayObjectListenerProc); overload; unimplemented;
        procedure rmListener(const _event: string; const _handler: TNotifyEvent); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: array of TNotifyEvent); overload; unimplemented;

        //function isListener(const _event: string; const _handler: TObjectListenerMethod): boolean; overload;
        //function isListener(const _event: string; const _handler: TObjectListenerProc): boolean; overload;
        //function isListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;
        //
        //function isEnabledListeners(const _event: string): boolean;
        //function isEnabledListener(const _event: string; const _handler: TObjectListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayObjectListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TObjectListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayObjectListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: array of TNotifyEvent): boolean; overload;
        //
        //procedure enableListeners(const _event: string);
        //procedure enableListener(const _event: string; const _handler: TObjectListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayObjectListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handler: TObjectListenerProc); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayObjectListenerProc); overload;
        //procedure enableListener(const _event: string; const _handler: TNotifyEvent); overload;
        //procedure enableListener(const _event: string; const _handlers: array of TNotifyEvent); overload;
        //
        //procedure disableListeners(const _event: string);
        //procedure disableListener(const _event: string; const _handler: TObjectListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayObjectListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handler: TObjectListenerProc); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayObjectListenerProc); overload;
        //procedure disableListener(const _event: string; const _handler: TNotifyEvent); overload;
        //procedure disableListener(const _event: string; const _handlers: array of TNotifyEvent); overload;

        // Invokes listeners
        procedure signal(const _event: string; constref _params: TJSONObject=nil; _freeParams: Boolean = true);


        // Returns a list of signal names that have been registered
        function signals: TStringArray;
        function memdump: TJSONArray;

        // TODO -- still evaluating if this is a good idea.
        //-------------------------------------------------
        //function async(_p: TProcedure; _invoke: TInvokeType = qAsync): int64;
        //function async(_p: TProcedureOfObject; _invoke: TInvokeType = qAsync): int64;
        //function async(_p: TDataEvent; _invoke: TInvokeType = qAsync): int64;
        //function async(_p: TNotifyEvent; _invoke: TInvokeType = qAsync): int64;
        //function async(_p: TNotifyCallBack; _invoke: TInvokeType = qAsync): int64;
        //
        //function await(_p: TProcedure; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TProcedureOfObject; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TDataEvent; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TNotifyEvent; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TNotifyCallBack; _invoke: TInvokeType = qAsync): int64;

        function activeSignalCount: integer; // Returns the number of signals active for this particular object.
        function objectAlive: boolean;

        procedure stopListening; // Removes this object from the list of listeners.

	end;

    TListenerSignalMode = (lmSingleton, lmDynamic);

    // Hack to check if an object address points to a valid object.
    function isObjectAlive(_obj: TObject) : boolean;


var
    // ListenerSignalMode: TListenerSignalMode = lmSingleton;
    ListenerSignalMode: TListenerSignalMode = lmDynamic;

implementation

type

	{ TObjectListenerProcRunner }

    // IMPORTANT.
    // This class frees itself after running
    // See doRun();
    TObjectListenerProcRunner = class
    private
        myFreeOnDone: boolean;
        listener: TObjectlListener;
        procedure doRun;                        // Can be called in a thread as well.
        procedure doRunAsync(_param: PtrInt);   // For Application.QueueAsyncCall()
    public
        beforeDo: function : integer of object;
        afterDo : function : integer of object;
        Enabled: boolean;
        procedure runAsync (_listener: TObjectlListener);
        procedure runThread(_listener: TObjectlListener);
        procedure runSerial(_listener: TObjectlListener);
        constructor Create(_freeOnDone: boolean = false);

	end;

 var
     // See Initialization section
    globalObjectListenerList : TObjectListenerList;
    globalProcRunner         : TObjectListenerProcRunner;


 function AddressAsHex(_obj: pointer): string;
 begin
    Result:= PtrUInt(_obj).ToHexString(16);
 end;

 procedure clearListenerObject(_index: integer);
 begin
     while globalObjectListenerList.Data[_index].Count > 0 do begin // Loop of event listeners
         globalObjectListenerList.Data[_index].Data[_index].Free;        // TObjectListenerProcList
         globalObjectListenerList.Data[_index].Delete(_index);
     end;
     globalObjectListenerList.Data[_index].Free;
     globalObjectListenerList.Delete(_index);
 end;

procedure clearMyListenerList;
begin
    while globalObjectListenerList.Count>0 do begin               // Loop of control event listeners
        clearListenerObject(0);
    end;
end;

function isObjectAlive(_obj: TObject): boolean;
begin
    try
        if _obj is TObject then
            Result := true;
	except
        Result := false;
	end;
end;

function TObjectListenerProcList.Add(const Item: TObjectlListener): Integer;
begin
    inherited add(Item);
    Item.beforeDo := Self.beforeDo;
    Item.afterDo  := Self.afterDo;
end;

{ TObjectListenerMap }

constructor TObjectListenerMap.Create;
begin
    inherited Create;
    InitCriticalSection(myCriticalSection);
    myActiveCallCount:=0;
end;

destructor TObjectListenerMap.Destroy;
begin
    wrapUp;
    DoneCriticalSection(myCriticalSection);
	inherited Destroy;
end;

function TObjectListenerMap.Add(const AKey: string;
	const AData: TObjectListenerProcList): Integer;
begin
    inherited add(Akey, AData);
    AData.beforeDo := @Self.incActiveCall;
    AData.afterDo  := @Self.decActiveCall;
end;

function TObjectListenerMap.activeCallCount: integer;
begin
    Result:= myActiveCallCount;
end;

function TObjectListenerMap.incActiveCall: integer;
begin
    EnterCriticalSection(myCriticalSection);
    inc(myActiveCallCount);
    LeaveCriticalSection(myCriticalSection);
    Result := myActiveCallCount;
end;

function TObjectListenerMap.decActiveCall: integer;
begin
    if myActiveCallCount > 0 then begin
        EnterCriticalSection(myCriticalSection);
        dec(myActiveCallCount);
        LeaveCriticalSection(myCriticalSection);
	end;
	Result := myActiveCallCount;
end;

procedure TObjectListenerMap.wrapUp;
begin
    while activeCallCount > 0 do begin
        sleep(20);
        Application.ProcessMessages;
	end;
end;

{ TObjectListenerList }

procedure TObjectListenerList.Clear;
begin
    while Count> 0 do // Loop of control event myListeners
    begin
        while Data[0].Count > 0 do begin // Loop of event myListeners
            Data[0].Data[0].Free;        // TObjectListenerProcList
            Data[0].Delete(0);
        end;
    	Data[0].Free;
        Delete(0);
	end;
end;

destructor TObjectListenerList.Destroy;
begin
    Clear;
	inherited Destroy;
end;



{ TListenerProcRunner }

procedure TObjectListenerProcRunner.runAsync(_listener: TObjectlListener);
begin
    Application.QueueAsyncCall(@doRunAsync, PtrInt(_listener));
end;

procedure TObjectListenerProcRunner.runThread(_listener: TObjectlListener);
begin
    listener := _listener;
    TThread.ExecuteInThread(@doRun);
end;

procedure TObjectListenerProcRunner.runSerial(_listener: TObjectlListener);
begin
    listener := _listener;
    doRun;
end;

constructor TObjectListenerProcRunner.Create(_freeOnDone: boolean);
begin
    inherited Create;
    myFreeOnDone := _freeOnDone;
    Enabled      := true;
end;

procedure TObjectListenerProcRunner.doRunAsync(_param: PtrInt);
begin
    listener := TObjectlListener(_param);
    doRun;
end;

procedure TObjectListenerProcRunner.doRun;
var
	a: String;
begin
    {
        Because this version has not implemented freeing myListeners when the object is freed
        which then causes the proc address to point to invalid memory
        catch the exception and then disable this listener.
    }

    if enabled then begin
	    if assigned(listener) then begin
            if isObjectAlive(listener) then
            begin
                with listener do begin
			        try
		                if assigned(beforeDo) then beforeDo();

				        if assigned(meth) then
			                try
					            meth(sender, event, params)
							except
			                    ; //meth := nil;
							end

					    else if assigned(proc) then
		                    try
					            proc(sender, event, params);
							except
		                        ; // proc is nil
							end

					    else if assigned(notify) then
		                    try
					            notify(sender);
							except
		                        ; //
							end;

                        if freeParams then params.Free;
		                if assigned(AfterDo) then AfterDo();
					except
			            on E:Exception do begin
		                    // procedure is probably pointing to freed memory
		                    // Don't run this listener anymore.
			                // Enabled := false;
			            end;
					end;

	            end;
			end
            else
                ;
		end;
	end;


    if myFreeOnDone then Free; // Destroy itself
end;



{ TObjectListenerHelper }


function TObjectListenerHelper.myListeners: TObjectListenerMap;
var
   _i: integer;
begin
    {
        myListeners returns a map of
        Each control has its own myListeners collection.
        Because type helpers cannot have fields, the list is stored in
        the implementation global variable as a key map.

        Key is the hex representation of the "self" pointer.
    }

    // _i := globalObjectListenerList.IndexOf(Self.Name);
    _i := globalObjectListenerList.IndexOf(AddressAsHex(Self));
    if _i >= 0 then
        Result:= globalObjectListenerList.Data[_i]
    else begin
        Result:= TObjectListenerMap.Create;
        globalObjectListenerList.Add(AddressAsHex(Self), Result);
	end;
end;

function TObjectListenerHelper.myListener(const _event: string
	): TObjectListenerProcList;
var
   _i: integer;
begin
    _i := myListeners.IndexOf(_event);
    if _i >= 0 then begin
        Result:= myListeners.Data[_i];
    end
    else begin
        Result:= TObjectListenerProcList.Create;
        myListeners.Add(_event, Result);
    end;
end;

//Listener Method
function TObjectListenerHelper.addListener(const _event: string;
	const _handler: TObjectListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
   _L : TObjectlListener;
begin
    if assigned(_handler) then begin
        _L := TObjectlListener.Create;
        _L.sender := self;
        _L.add(_handler, _sigType);
        myListener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	const _handler: TObjectListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_event: String;
begin
    for _event in _events do
            addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

//Listener Method
function TObjectListenerHelper.addListener(const _event: string;
	const _handlers: TArrayObjectListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_handler: TObjectListenerMethod;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

//Listener Proc
function TObjectListenerHelper.addListener(const _event: string;
	const _handler: TObjectListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
   _L : TObjectlListener;
begin
    if assigned(_handler) then begin
        _L := TObjectlListener.Create;
        _L.add(_handler, _sigType);
        myListener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	const _handler: TObjectListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_event: String;
begin
    for _event in _events do
            addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

//Listener Proc
function TObjectListenerHelper.addListener(const _event: string;
	const _handlers: TArrayObjectListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_handler: TObjectListenerProc;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

// TNotifyEvent
function TObjectListenerHelper.addListener(const _event: string;
	const _handler: TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
   _L : TObjectlListener;
begin
    if assigned(_handler) then begin
        _L := TObjectlListener.Create;
        _L.add(_handler, _sigType);
        myListener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	const _handler: TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_event: String;
begin
    for _event in _events do
        addListener(_event, _handler, _sigType, _ignoreduplicates);

    Result := Self;
end;

// TNotifyEvent
function TObjectListenerHelper.addListener(const _event: string;
	const _handlers: array of TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TObject;
var
	_handler: TNotifyEvent;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

procedure TObjectListenerHelper.rmListeners;
var
	_i: Integer;
begin
    {WE ALSO NEED A WAY
        To remove proc addresses from other objects whose signals we are listening to
        when we are being destroyed.
        Need more indexes.
    }
    _i := globalObjectListenerList.IndexOf(AddressAsHex(Self));
    if _i > -1 then clearListenerObject(_i);
end;

procedure TObjectListenerHelper.rmListeners(const _event: string);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handler: TObjectListenerMethod);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handlers: TArrayObjectListenerMethod);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handler: TObjectListenerProc);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handlers: TArrayObjectListenerProc);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handler: TNotifyEvent);
begin

end;

procedure TObjectListenerHelper.rmListener(const _event: string;
	const _handlers: array of TNotifyEvent);
begin

end;

procedure TObjectListenerHelper.signal(
            const _event: string;
            constref _params: TJSONObject; _freeParams: Boolean
	);
var
   i : integer;
   _l : TObjectListenerProcList;
   _tmpParams: TJSONObject = nil;
begin

    if not objectAlive then exit;  // Check if this object is still in scope.

    _l := myListener(_event);
    for i := 0 to pred(_l.Count) do begin
        if assigned(_params) then begin
	        _tmpParams:= _params.Clone as TJSONObject // Always call the myListener procedure with a cloned param object (memory safety).
        end;
        _l.Items[i].do_(self, _event, _tmpParams, true {free params because it will be cloned before next call})
	end;
    // Always free parameters, irrespective of whether there were event handlers or not
    if _freeParams then _params.Free;
end;

function TObjectListenerHelper.signals: TStringArray;
var
	i: Integer;
begin
    SetLength(Result, myListeners.Count);
    for i := 0 to High(Result) do begin
        Result[i] := myListeners.Keys[i];
	end;
end;

function TObjectListenerHelper.memdump: TJSONArray;
var
    _signals: TStringArray;
	_signal: String;
	_procList: TObjectListenerProcList;
	_obj: TJSONObject;
	_i: Integer;
begin
    Result:= TJSONArray.Create;
    _signals := signals;

    for _signal in _signals do begin
        _procList :=  myListener(_signal);

        _obj := TJSONObject.Create([
            'control', AddressAsHex(Self),
            'signal', _signal,
            'listeners', TJSONArray.Create()
        ]);

        Result.Add(_obj);
        for _i := 0 to pred(_procList.Count) do begin
            _obj.arrays['listeners'].add (_proclist.Items[_i].getAsJSON);
        end;
	end;

end;

function TObjectListenerHelper.activeSignalCount: integer;
begin
    Result := myListeners.activeCallCount;
end;

function TObjectListenerHelper.objectAlive: boolean;
begin
    Result := isObjectAlive(self);
end;

procedure TObjectListenerHelper.stopListening;
begin
    rmListeners;
end;


{ TObjectlListener }

function TObjectlListener.getAsString: string;
begin
    with getAsJSON() do begin
        Result:= AsJSON;
        Free;
	end;
end;

function TObjectlListener.getAsJSON: TJSONObject;
var
    _s: string = '';
begin
    Result:= TJSONObject.Create;
    Result.Add('enabled', myEnabled);
    Result.Add('sender', AddressAsHex(sender));
    Result.Add('event', event);
    if assigned(proc) then
        Result.Add('proc', 'proc: ' + AddressAsHex(@proc))
    else if assigned(meth) then
        Result.Add('proc', 'meth: ' + AddressAsHex(@meth))
    else if assigned(notify) then
        Result.Add('proc', 'notify: ' + AddressAsHex(@notify))
    else
        Result.add('proc', 'nil');

    if assigned(params) then
        Result.Add('params', params.AsJSON)
    else
        Result.Add('params', 'nil');

    Result.Add('freeParams', freeParams);

    Str(sigType, _s);
    Result.Add('sigType', _s);

end;

constructor TObjectlListener.Create;
begin
    inherited Create;
    myEnabled:= true;
end;

procedure TObjectlListener.add(constref _proc: TObjectListenerProc; const _sigType: TInvokeType = qAsync);
begin
    proc:= _proc;
    meth:= nil;
    notify:= nil;
end;

procedure TObjectlListener.add(constref _meth: TObjectListenerMethod; const _sigType: TInvokeType = qAsync);
begin
    meth:= _meth;
    proc:=  nil;
    notify:= nil;
end;

procedure TObjectlListener.add(constref _notify: TNotifyEvent;
	const _sigType: TInvokeType);
begin
    notify:= _notify;
    proc:= nil;
    meth:= nil;
end;


procedure TObjectlListener.do_(constref _sender: TObject; const _event: string;
	constref _params: TJSONObject; const _freeParams: Boolean);
var
	_runner: TObjectListenerProcRunner;

begin

    if not enabled then exit;

    sender      := _sender;
    event       := _event;
    params      := _params;
    freeParams  := _freeParams;

    case ListenerSignalMode of
    	lmSingleton: _runner     := globalProcRunner;
        lmDynamic:   _runner     := TObjectListenerProcRunner.Create(true); // Free on done.
    end;

    _runner.beforeDo := Self.beforeDo;
    _runner.afterDo  := Self.afterDo;

    case sigType of
        qAsync:     _runner.runAsync(self);
        qThreads:   _runner.runThread(self);
        qSerial:    _runner.runSerial(self);
    end;
    if assigned(AfterDo) then AfterDo();

end;

initialization
    globalObjectListenerList:= TObjectListenerList.Create;
    globalProcRunner        := TObjectListenerProcRunner.Create; // Don't free on done;

finalization
    globalObjectListenerList.Free;
    globalProcRunner.Free;
end.

