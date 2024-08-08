unit Controls.Listener;
{
(c) Rubrican Research.
https://github.com/rubrican-research/saph

This library is released under the MIT License.

to freely define event listeners - which is text based, case-sensitive - on any control.
This is implemented as a Type Helper on TControl.

Implement event listeners in your units with the following signature
    procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject);

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
    TControlListenerProc        = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject);
    TControlListenerMethod      = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject) of object;

    // Syntax sugar. To assign multiple listeners in one go.
    TArrayControlListenerProc   = array of TControlListenerProc;
    TArrayControlListenerMethod = array of TControlListenerMethod;



    TInvokeType = (
                            qAsync,     // Queues the listeners to Application.QueueAsyncCall();
                            qThreads,   // Runs the methods in individual threads;
                            qSerial     // Runs in a blocking loop
                          );

	{ TControlListener }
    TControlListener = class
	private
	  myEnabled: boolean;
      sender: TControl;
      event: string;
	  proc: TControlListenerProc;
	  meth: TControlListenerMethod;
      notify: TNotifyEvent;
      params: TJSONObject;
      freeParams: boolean;
      sigType: TInvokeType;

      function getAsString: string;
	public
      constructor Create;
	  procedure add(constref _proc: TControlListenerProc; const _sigType: TInvokeType = qAsync); overload;
	  procedure add(constref _meth: TControlListenerMethod; const _sigType: TInvokeType = qAsync); overload;
      procedure add(constref _notify: TNotifyEvent; const _sigType: TInvokeType = qAsync); overload;
	  procedure do_(constref _sender: TControl; const _event: string; constref _params: TJSONObject; const _freeParams: Boolean = true);
    public
      beforeDo: function : integer of object;
      afterDo : function : integer of object;
      property enabled: boolean read myEnabled write myEnabled;
      property asString : string read getAsString;

      function getAsJSON: TJSONObject;
	end;

	TControlListenerProcListBase   = specialize TFPGObjectList<TControlListener>;             // List of listeners

	{ TControlListenerProcList }

    TControlListenerProcList       = class(TControlListenerProcListBase)
    public
        beforeDo: function : integer of object;
        afterDo : function : integer of object;

        function Add(const Item: TControlListener): Integer; inline;
	end;

    TControlListenerCollectionBase = specialize TFPGMap<string, TControlListenerProcList>;// Map of Event and List of listener procedures

	{ TControlListenerCollection }

    TControlListenerCollection = class(TControlListenerCollectionBase)
    private
        myActiveCallCount : integer; // count
        myCriticalSection: TRTLCriticalSection;
    public
        constructor Create;
        destructor Destroy; override;
        function Add(const AKey: string; const AData: TControlListenerProcList): Integer; inline;

        function activeCallCount: integer;
        function incActiveCall: integer; // Returns new count;
        function decActiveCall: integer; // Returns new count;
        procedure wrapUp;
	end;

    TListenerListBase          = specialize TFPGMap<string, TControlListenerCollection>;  // Map of Control ID and List of Event Listeners

	{ TListenerList }

    TListenerList = class(TListenerListBase)
        procedure Clear; reintroduce;
        destructor Destroy; override;
	end;

	{ TControlListenerHelper }

    TControlListenerHelper = class helper for TControl

        function listeners: TControlListenerCollection;                     // Returns the list of listeners for this particular object!
        function listener(const _event: string): TControlListenerProcList;  // Returns the proc list for this particular event

        function addListener(
                        const _event: string;
                        const _handler: TControlListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;
        {Array of events -> single listener Method}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TControlListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;
        {Array of Listener Methods}
        function addListener(
                        const _event: string;
                        const _handlers: TArrayControlListenerMethod;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        function addListener(
                        const _event: string;
                        const _handler: TControlListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        {Array of events -> single listener Proc}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TControlListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;
        {Array of Listener Procs}
        function addListener(
                        const _event: string;
                        const _handlers: TArrayControlListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        function addListener(
                        const _event: string;
                        const _handler: TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        {Array of events -> single listener Notify Event}
        function addListener(
                        const _events: TStringArray;
                        const _handler: TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        {Array of Notify Events}
        function addListener(
                        const _event: string;
                        const _handlers: array of TNotifyEvent;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;
        // TODO - not implemented
        procedure rmListeners; // Removes all listeners
        procedure rmListeners(const _event: string); unimplemented;
        procedure rmListener(const _event: string; const _handler: TControlListenerMethod); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: TArrayControlListenerMethod); overload; unimplemented;
        procedure rmListener(const _event: string; const _handler: TControlListenerProc); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: TArrayControlListenerProc); overload; unimplemented;
        procedure rmListener(const _event: string; const _handler: TNotifyEvent); overload; unimplemented;
        procedure rmListener(const _event: string; const _handlers: array of TNotifyEvent); overload; unimplemented;

        //function isListener(const _event: string; const _handler: TControlListenerMethod): boolean; overload;
        //function isListener(const _event: string; const _handler: TControlListenerProc): boolean; overload;
        //function isListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;
        //
        //function isEnabledListeners(const _event: string): boolean;
        //function isEnabledListener(const _event: string; const _handler: TControlListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayControlListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TControlListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayControlListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: array of TNotifyEvent): boolean; overload;
        //
        //procedure enableListeners(const _event: string);
        //procedure enableListener(const _event: string; const _handler: TControlListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayControlListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handler: TControlListenerProc); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayControlListenerProc); overload;
        //procedure enableListener(const _event: string; const _handler: TNotifyEvent); overload;
        //procedure enableListener(const _event: string; const _handlers: array of TNotifyEvent); overload;
        //
        //procedure disableListeners(const _event: string);
        //procedure disableListener(const _event: string; const _handler: TControlListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayControlListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handler: TControlListenerProc); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayControlListenerProc); overload;
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

	end;

    TListenerSignalMode = (lmSingleton, lmDynamic);

    // Hack to check if an object address points to a valid object.
    function isObjectAlive(_obj: TObject) : boolean;


var
    // ListenerSignalMode: TListenerSignalMode = lmSingleton;
    ListenerSignalMode: TListenerSignalMode = lmDynamic;

implementation

type

	{ TListenerProcRunner }

    // IMPORTANT.
    // This class frees itself after running
    // See doRun();
    TListenerProcRunner = class
    private
        myFreeOnDone: boolean;
        listener: TControlListener;
        procedure doRun;                        // Can be called in a thread as well.
        procedure doRunAsync(_param: PtrInt);   // For Application.QueueAsyncCall()
    public
        beforeDo: function : integer of object;
        afterDo : function : integer of object;
        Enabled: boolean;
        procedure runAsync (_listener: TControlListener);
        procedure runThread(_listener: TControlListener);
        procedure runSerial(_listener: TControlListener);
        constructor Create(_freeOnDone: boolean = false);

	end;

 var
     // See Initialization section
    myListenerList : TListenerList;
    myRunner: TListenerProcRunner;


 function ObjAddressAsHex(_obj: pointer): string;
 begin
    Result:= PtrUInt(_obj).ToHexString(16);
 end;

procedure clearMyListenerList;
begin
    while myListenerList.Count>0 do begin               // Loop of control event listeners
        while myListenerList.Data[0].Count > 0 do begin // Loop of event listeners
            myListenerList.Data[0].Data[0].Free;        // TControlListenerProcList
            myListenerList.Data[0].Delete(0);
        end;
        myListenerList.Data[0].Free;
        myListenerList.Delete(0);
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

function TControlListenerProcList.Add(const Item: TControlListener): Integer;
begin
    inherited add(Item);
    Item.beforeDo := Self.beforeDo;
    Item.afterDo  := Self.afterDo;
end;

{ TControlListenerCollection }

constructor TControlListenerCollection.Create;
begin
    inherited Create;
    InitCriticalSection(myCriticalSection);
    myActiveCallCount:=0;
end;

destructor TControlListenerCollection.Destroy;
begin
    wrapUp;
    DoneCriticalSection(myCriticalSection);
	inherited Destroy;
end;

function TControlListenerCollection.Add(const AKey: string;
	const AData: TControlListenerProcList): Integer;
begin
    inherited add(Akey, AData);
    AData.beforeDo := @Self.incActiveCall;
    AData.afterDo  := @Self.decActiveCall;
end;

function TControlListenerCollection.activeCallCount: integer;
begin
    Result:= myActiveCallCount;
end;

function TControlListenerCollection.incActiveCall: integer;
begin
    EnterCriticalSection(myCriticalSection);
    inc(myActiveCallCount);
    LeaveCriticalSection(myCriticalSection);
    Result := myActiveCallCount;
end;

function TControlListenerCollection.decActiveCall: integer;
begin
    if myActiveCallCount > 0 then begin
        EnterCriticalSection(myCriticalSection);
        dec(myActiveCallCount);
        LeaveCriticalSection(myCriticalSection);
	end;
	Result := myActiveCallCount;
end;

procedure TControlListenerCollection.wrapUp;
begin
    while activeCallCount > 0 do begin
        sleep(10);
        Application.ProcessMessages;
	end;
end;

{ TListenerList }

procedure TListenerList.Clear;
begin
    while Count> 0 do // Loop of control event listeners
    begin
        while Data[0].Count > 0 do begin // Loop of event listeners
            Data[0].Data[0].Free;        // TControlListenerProcList
            Data[0].Delete(0);
        end;
    	Data[0].Free;
        Delete(0);
	end;
end;

destructor TListenerList.Destroy;
begin
    Clear;
	inherited Destroy;
end;



{ TListenerProcRunner }

procedure TListenerProcRunner.runAsync(_listener: TControlListener);
begin
    Application.QueueAsyncCall(@doRunAsync, PtrInt(_listener));
end;

procedure TListenerProcRunner.runThread(_listener: TControlListener);
begin
    listener := _listener;
    TThread.ExecuteInThread(@doRun);
end;

procedure TListenerProcRunner.runSerial(_listener: TControlListener);
begin
    listener := _listener;
    doRun;
end;

constructor TListenerProcRunner.Create(_freeOnDone: boolean);
begin
    inherited Create;
    myFreeOnDone := _freeOnDone;
    Enabled      := true;
end;

procedure TListenerProcRunner.doRunAsync(_param: PtrInt);
begin
    listener := TControlListener(_param);
    doRun;
end;

procedure TListenerProcRunner.doRun;
var
	a: String;
begin
    {
        Because this version has not implemented freeing listeners when the object is freed
        which then causes the proc address to point to invalid memory
        catch the exception and then disable this listener.
    }

    if enabled then
	    if assigned(listener) then
            if isObjectAlive(listener) then
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
		                        ;
							end

					    else if assigned(notify) then
		                    try
					            notify(sender);
							except
		                        ;
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

    if myFreeOnDone then Free; // Destroy itself
end;



{ TControlListenerHelper }


function TControlListenerHelper.listeners: TControlListenerCollection;
var
   _i: integer;
begin
    {
        Listeners is a list of listener objects.
        Each control has its own Listeners collection.
        Because type helpers cannot have fields, the list is stored in
        the implementation global variable as a key map.

        Key is the hex representation of the "self" pointer.
    }

    // _i := myListenerList.IndexOf(Self.Name);
    _i := myListenerList.IndexOf(ObjAddressAsHex(Self));
    if _i >= 0 then
        Result:= myListenerList.Data[_i]
    else begin
        Result:= TControlListenerCollection.Create;
        myListenerList.Add(ObjAddressAsHex(Self), Result);
	end;
end;

function TControlListenerHelper.listener(const _event: string
	): TControlListenerProcList;
var
   _i: integer;
begin
    _i := listeners.IndexOf(_event);
    if _i >= 0 then begin
        Result:= listeners.Data[_i];
    end
    else begin
        Result:= TControlListenerProcList.Create;
        listeners.Add(_event, Result);
    end;
end;

//Listener Method
function TControlListenerHelper.addListener(
                const _event: string;
                const _handler: TControlListenerMethod;
                const _sigType: TInvokeType;
                const _ignoreduplicates: boolean
	): TControl;
var
   _L : TControlListener;
begin
    if assigned(_handler) then begin
        _L := TControlListener.Create;
        _L.add(_handler, _sigType);
        listener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TControlListenerHelper.addListener(const _events: TStringArray;
	const _handler: TControlListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_event: String;
begin
    for _event in _events do
            addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

//Listener Method
function TControlListenerHelper.addListener(const _event: string;
	const _handlers: TArrayControlListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_handler: TControlListenerMethod;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

//Listener Proc
function TControlListenerHelper.addListener(
        const _event: string;
	    const _handler: TControlListenerProc;
        const _sigType: TInvokeType;
        const _ignoreduplicates: boolean
	): TControl;
var
   _L : TControlListener;
begin
    if assigned(_handler) then begin
        _L := TControlListener.Create;
        _L.add(_handler, _sigType);
        listener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TControlListenerHelper.addListener(const _events: TStringArray;
	const _handler: TControlListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_event: String;
begin
    for _event in _events do
            addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

//Listener Proc
function TControlListenerHelper.addListener(const _event: string;
	const _handlers: TArrayControlListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_handler: TControlListenerProc;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

// TNotifyEvent
function TControlListenerHelper.addListener(const _event: string;
	const _handler: TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
   _L : TControlListener;
begin
    if assigned(_handler) then begin
        _L := TControlListener.Create;
        _L.add(_handler, _sigType);
        listener(_event).Add(_L);
	end;
	Result:= Self;
end;

function TControlListenerHelper.addListener(const _events: TStringArray;
	const _handler: TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_event: String;
begin
    for _event in _events do
        addListener(_event, _handler, _sigType, _ignoreduplicates);

    Result := Self;
end;

// TNotifyEvent
function TControlListenerHelper.addListener(const _event: string;
	const _handlers: array of TNotifyEvent; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_handler: TNotifyEvent;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
    Result := Self;
end;

procedure TControlListenerHelper.rmListeners;
var
	_i: Integer;
begin
    {WE ALSO NEED A WAY
        To remove proc addresses from other objects whose signals we are listening to
        when we are being destroyed.
        Need more indexes.
    }
    _i := myListenerList.IndexOf(ObjAddressAsHex(Self));
    if _i > -1 then begin
	    while myListenerList.Data[_i].Count > 0 do begin // Loop of event listeners
	        myListenerList.Data[_i].Data[0].Free;        // TControlListenerProcList
	        myListenerList.Data[_i].Delete(0);
	    end;
	    myListenerList.Data[_i].Free;
	    myListenerList.Delete(_i);
	end;
end;

procedure TControlListenerHelper.rmListeners(const _event: string);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handler: TControlListenerMethod);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handlers: TArrayControlListenerMethod);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handler: TControlListenerProc);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handlers: TArrayControlListenerProc);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handler: TNotifyEvent);
begin

end;

procedure TControlListenerHelper.rmListener(const _event: string;
	const _handlers: array of TNotifyEvent);
begin

end;

procedure TControlListenerHelper.signal(
            const _event: string;
            constref _params: TJSONObject; _freeParams: Boolean
	);
var
   i : integer;
   _l : TControlListenerProcList;
   _tmpParams: TJSONObject = nil;
begin

    if not objectAlive then exit;  // Check if this object is still in scope.

    _l := listener(_event);
    for i := 0 to pred(_l.Count) do begin
        if assigned(_params) then begin
	        _tmpParams:= _params.Clone as TJSONObject // Always call the listener procedure with a cloned param object (memory safety).
        end;
        _l.Items[i].do_(self, _event, _tmpParams, true {free params because it will be cloned before next call})
	end;
    // Always free parameters, irrespective of whether there were event handlers or not
    if _freeParams then _params.Free;
end;

function TControlListenerHelper.signals: TStringArray;
var
	i: Integer;
begin
    SetLength(Result, listeners.Count);
    for i := 0 to High(Result) do begin
        Result[i] := listeners.Keys[i];
	end;
end;

function TControlListenerHelper.memdump: TJSONArray;
var
    _signals: TStringArray;
	_signal: String;
	_procList: TControlListenerProcList;
	_obj: TJSONObject;
	_i: Integer;
begin
    Result:= TJSONArray.Create;
    _signals := signals;

    for _signal in _signals do begin
        _procList :=  listener(_signal);

        _obj := TJSONObject.Create([
            'control', ObjAddressAsHex(Self),
            'signal', _signal,
            'listeners', TJSONArray.Create()
        ]);

        Result.Add(_obj);
        for _i := 0 to pred(_procList.Count) do begin
            _obj.arrays['listeners'].add (_proclist.Items[_i].getAsJSON);
        end;
	end;

end;

function TControlListenerHelper.activeSignalCount: integer;
begin
    Result := listeners.activeCallCount;
end;

function TControlListenerHelper.objectAlive: boolean;
begin
    Result := isObjectAlive(self);
end;

{ TControlListener }

function TControlListener.getAsString: string;
begin
    with getAsJSON() do begin
        Result:= AsJSON;
        Free;
	end;
end;

function TControlListener.getAsJSON: TJSONObject;
var
    _s: string = '';
begin
    Result:= TJSONObject.Create;
    Result.Add('enabled', myEnabled);
    Result.Add('sender', ObjAddressAsHex(sender));
    Result.Add('event', event);
    if assigned(proc) then
        Result.Add('proc', 'proc: ' + ObjAddressAsHex(@proc))
    else if assigned(meth) then
        Result.Add('proc', 'meth: ' + ObjAddressAsHex(@meth))
    else if assigned(notify) then
        Result.Add('proc', 'notify: ' + ObjAddressAsHex(@notify))
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

constructor TControlListener.Create;
begin
    inherited Create;
    myEnabled:= true;
end;

procedure TControlListener.add(constref _proc: TControlListenerProc; const _sigType: TInvokeType = qAsync);
begin
    proc:= _proc;
    meth:= nil;
    notify:= nil;
end;

procedure TControlListener.add(constref _meth: TControlListenerMethod; const _sigType: TInvokeType = qAsync);
begin
    meth:= _meth;
    proc:=  nil;
    notify:= nil;
end;

procedure TControlListener.add(constref _notify: TNotifyEvent;
	const _sigType: TInvokeType);
begin
    notify:= _notify;
    proc:= nil;
    meth:= nil;
end;


procedure TControlListener.do_(constref _sender: TControl;
	const _event: string;
    constref _params: TJSONObject;
    const _freeParams: Boolean
	);
var
	_runner: TListenerProcRunner;

begin

    if not enabled then exit;

    sender      := _sender;
    event       := _event;
    params      := _params;
    freeParams  := _freeParams;

    case ListenerSignalMode of
    	lmSingleton: _runner     := myRunner;
        lmDynamic:   _runner     := TListenerProcRunner.Create(true); // Free on done.
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
    myListenerList  := TListenerList.Create;
    myRunner        := TListenerProcRunner.Create; // Don't free on done;

finalization
    myListenerList.Free;
    myRunner.Free;
end.

