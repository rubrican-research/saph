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
    TListenerProc = procedure(const _sender: TObject;
        const _event: string; constref _params: TJSONObject);
    TListenerMethod = procedure(const _sender: TObject;
        const _event: string; constref _params: TJSONObject) of object;

    // Syntax sugar. To assign multiple listeners in one go.
    TArrayListenerProc = array of TListenerProc;
    TArrayListenerMethod = array of TListenerMethod;



    TInvokeType = (
        qAsync,
        // Queues the listeners to Application.QueueAsyncCall();
        qThreads,   // Runs the methods in individual threads;
        qSerial     // Runs in a blocking loop
        );

    { TListener }
    TListener = class
    private
        myEnabled: boolean;
        Sender: TObject;
        Subscriber: TObject;
        //Subscriber: pointer;
        event: string;
        proc: TListenerProc;
        meth: TListenerMethod;
        notify: TNotifyEvent;
        params: TJSONObject;
        freeParams: boolean;
        sigType: TInvokeType;

        function getAsString: string;
    public
        constructor Create;
        procedure add(constref _proc: TListenerProc;
            const _sigType: TInvokeType = qAsync); overload;
        procedure add(constref _subscriber: TObject; constref _meth: TListenerMethod;
            const _sigType: TInvokeType = qAsync); overload;
        procedure add(constref _subscriber: TObject; constref _notify: TNotifyEvent;
            const _sigType: TInvokeType = qAsync); overload;

        procedure do_(constref _sender: TObject; const _event: string;
            constref _params: TJSONObject; const _freeParams: boolean = True);
    public
        beforeDo: function: integer of object;
        afterDo: function: integer of object;
        property Enabled: boolean read myEnabled write myEnabled;
        property AsString: string read getAsString;

        function getAsJSON: TJSONObject;
    end;


    // List of listeners
    { TListenerProcList }

    TListenerProcList = class(specialize TFPGObjectList<TListener>)
    public
        key: string;
        beforeDo: function: integer of object;
        afterDo: function: integer of object;

        function Add(const Item: TListener): integer; inline;
        destructor Destroy; override;
    end;

    // Map of Event and List of listener procedures
    { TEventListenerMap }

    TEventListenerMap = class(specialize TFPGMapObject<string, TListenerProcList>)
    private
        myActiveCallCount: integer; // count
        myCriticalSection: TRTLCriticalSection;
    public
        constructor Create(AFreeObjects: Boolean = true);
        destructor Destroy; override;
        function Add(const AKey: string; const AData: TListenerProcList): integer; inline;
        function activeCallCount: integer;
        function incActiveCall: integer; // Returns new count;
        function decActiveCall: integer; // Returns new count;
        procedure wrapUp;
    end;


    // Map of Control ID and List of Event Listeners
    { TObjectEventList }
    TObjectEventList = class(specialize TFPGMapObject<string, TEventListenerMap>)
        destructor Destroy; override;
    end;

    { TObjectListenerHelper }

    TObjectListenerHelper = class helper for TObject

        function Listeners: TEventListenerMap;
        // Returns the list of listeners for this particular object!
        function Listener(const _event: string): TListenerProcList;
        // Returns the proc list for this particular event

        {================================================================}
        {ADD TListenerMethod}
        {================================================================}
        function addListener(const _event: string;
            constref _subscriber: TObject;
            constref _handler: TListenerMethod;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;
        {Array of events -> single listener Method}
        function addListener(const _events: TStringArray;
            constref _subscriber: TObject;
            constref _handler: TListenerMethod;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;
        {Array of Listener Methods}
        function addListener(const _event: string;
            constref _subscriber: TObject;
            constref _handlers: TArrayListenerMethod;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;

        {================================================================}
        {ADD TListenerProc}
        {================================================================}
        function addListener(const _event: string;
            constref _handler: TListenerProc;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;

        {Array of events -> single listener Proc}
        function addListener(const _events: TStringArray;
            constref _handler: TListenerProc;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;
        {Array of Listener Procs}
        function addListener(const _event: string;
            constref _handlers: TArrayListenerProc;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;

        {================================================================}
        {ADD TNotifyEvent}
        {================================================================}
        function addListener(const _event: string;
            constref _subscriber: TObject;
            constref _handler: TNotifyEvent;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;

        {Array of events -> single listener Notify Event}
        function addListener(const _events: TStringArray;
            constref _subscriber: TObject;
            constref _handler: TNotifyEvent;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;

        {Array of Notify Events}
        function addListener(const _event: string;
            constref _subscriber: TObject;
            constref _handlers: array of TNotifyEvent;
            constref _sigType: TInvokeType = qAsync;
            constref _ignoreduplicates: boolean = True): TObject; overload;
        {================================================================}

        // TODO - not implemented
        procedure rmListeners; // Removes all listeners on this object
        procedure rmListeners(const _event: string); // Removes listeners on this event
        procedure rmListeners(constref _subscriber: TObject); // Removes listeners that belong to this subscriber

        //procedure rmListener(const _event: string; const _handler: TListenerMethod); overload; unimplemented;
        //procedure rmListener(const _event: string; const _handlers: TArrayListenerMethod); overload; unimplemented;
        //procedure rmListener(const _event: string; const _handler: TListenerProc); overload; unimplemented;
        //procedure rmListener(const _event: string; const _handlers: TArrayListenerProc); overload; unimplemented;
        //procedure rmListener(const _event: string; const _handler: TNotifyEvent); overload; unimplemented;
        //procedure rmListener(const _event: string; const _handlers: array of TNotifyEvent); overload; unimplemented;

        {TODO - still evaluating if this is necessary}
        {============================================}
        //function isListener(const _event: string; const _handler: TListenerMethod): boolean; overload;
        //function isListener(const _event: string; const _handler: TListenerProc): boolean; overload;
        //function isListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;

        //function isEnabledListeners(const _event: string): boolean;
        //function isEnabledListener(const _event: string; const _handler: TListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayListenerMethod): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: TArrayListenerProc): boolean; overload;
        //function isEnabledListener(const _event: string; const _handler: TNotifyEvent): boolean; overload;
        //function isEnabledListener(const _event: string; const _handlers: array of TNotifyEvent): boolean; overload;

        //procedure enableListeners(const _event: string);
        //procedure enableListener(const _event: string; const _handler: TListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayListenerMethod); overload;
        //procedure enableListener(const _event: string; const _handler: TListenerProc); overload;
        //procedure enableListener(const _event: string; const _handlers: TArrayListenerProc); overload;
        //procedure enableListener(const _event: string; const _handler: TNotifyEvent); overload;
        //procedure enableListener(const _event: string; const _handlers: array of TNotifyEvent); overload;

        //procedure disableListeners(const _event: string);
        //procedure disableListener(const _event: string; const _handler: TListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayListenerMethod); overload;
        //procedure disableListener(const _event: string; const _handler: TListenerProc); overload;
        //procedure disableListener(const _event: string; const _handlers: TArrayListenerProc); overload;
        //procedure disableListener(const _event: string; const _handler: TNotifyEvent); overload;
        //procedure disableListener(const _event: string; const _handlers: array of TNotifyEvent); overload;

        // Invokes listeners
        procedure signal(const _event: string; constref _params: TJSONObject = nil; _freeParams: boolean = True);


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

        //function await(_p: TProcedure; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TProcedureOfObject; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TDataEvent; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TNotifyEvent; _invoke: TInvokeType = qAsync): int64;
        //function await(_p: TNotifyCallBack; _invoke: TInvokeType = qAsync): int64;

        function activeSignalCount: integer;

        // Returns the number of signals active for this particular object.
        function objectAlive: boolean;

        procedure stopListening; // Removes to signals from other objects
    end;

    TListenerSignalMode = (lmSingleton, lmDynamic);

// Hack to check if an object address points to a valid object.
function isObjectAlive(constref _obj: TObject): boolean;


var
    // ListenerSignalMode: TListenerSignalMode = lmSingleton;
    ListenerSignalMode: TListenerSignalMode = lmDynamic;

implementation

uses
    StrUtils, sugar.logger;

type

    { TProcRunner }

    // IMPORTANT.
    // This class frees itself after running
    // See doRun();
    TProcRunner = class
    private
        myCs: TRTLCriticalSection;
        myFreeOnDone: boolean;
        myListener: TListener;
        procedure doRun;                        // Can be called in a thread as well.
        procedure doRunAsync(_param: PtrInt);   // For Application.QueueAsyncCall()
    public
        beforeDo: function: integer of object;
        afterDo: function: integer of object;
        Enabled: boolean;
        procedure runAsync(_listener: TListener);
        procedure runThread(_listener: TListener);
        procedure runSerial(_listener: TListener);
        constructor Create(_freeOnDone: boolean = False);
        destructor Destroy; override;

    end;

    THexstringMap = class(specialize TFPGMap<string, string>);
    TSubscriberObjectMap = class(specialize TFPGMapObject<string, THexstringMap>);

var
    // See Initialization section
    objectListeners: TObjectEventList;
    procRunner: TProcRunner;
    subscriberObjectMap: TSubscriberObjectMap;


function pointerAsHex(_obj: pointer): string;
begin
    Result := PtrUInt(_obj).ToHexString(16);
end;

function hexStrAsPointer(_hex: string): Pointer;
begin
    Result := pointer(Hex2Dec64(_hex));
end;

procedure clearObjectListener(_index: integer);
begin
    while objectListeners.Data[_index].Count > 0 do
    begin // Loop of event listeners
        objectListeners.Data[_index].Delete(_index);
    end;
    objectListeners.Delete(_index);
end;

procedure clearMyListenerList;
begin
    while objectListeners.Count > 0 do
    begin               // Loop of control event listeners
        clearObjectListener(0);
    end;
end;

{=============IMPORTANT=========================================}
// Hack to check if an object address points to a valid object.
// If during debugging, an exception is raised here
// while an object is being destoryed, it would not
// cause no memory leaks. To be sure, please check memory leaks
// with heaprtc turned on in the debugging options.
// The exception is unavoidable in this design
function isObjectAlive(constref _obj: TObject): boolean;
begin
    try
        if _obj is TObject then
            Result := True;
    except
        Result := False;
    end;
end;

function addSubscriber(_subscriber: TObject; _signaler: TObject): integer;
var
    _subscribers: THexstringMap;
	_i: Integer;
begin
	// subscriber => object 1:many relationship
	if subscriberObjectMap.Find(pointerAsHex(_subscriber), _i) then
	begin
	    _subscribers := subscriberObjectMap.Data[_i];
        Result := _i;
	end
	else
	begin
	    _subscribers := THexstringMap.Create;
	    _subscribers.Duplicates:=dupIgnore;
	    subscriberObjectMap.Add(pointerAsHex(_subscriber), _subscribers);
        Result := pred(subscriberObjectMap.count)
	end;
	_subscribers.add(pointerAsHex(_signaler));
end;



procedure clearGlobalListenerToObjectMap;
begin
    while subscriberObjectMap.Count > 0 do
        subscriberObjectMap.Delete(0);
end;

function TListenerProcList.Add(const Item: TListener): integer;
begin
    Result := inherited add(Item);
    if key.isEmpty then
        key := Item.event
    else
        if not key.Contains(Item.event) then
            key := key + ', ' + Item.Event;
    Item.beforeDo := Self.beforeDo;
    Item.afterDo := Self.afterDo;
end;

destructor TListenerProcList.Destroy;
begin
    log('TListenerProcList.Destroy() %s', [key] );
	inherited Destroy;
end;

{ TEventListenerMap }

constructor TEventListenerMap.Create(AFreeObjects: Boolean);
begin
    inherited;
    InitCriticalSection(myCriticalSection);
    myActiveCallCount := 0;
end;

destructor TEventListenerMap.Destroy;
var
	i: Integer;
begin
    wrapUp;
    DoneCriticalSection(myCriticalSection);
    log('TEventListenerMap.Destroy::');
    for i := 0 to pred(count) do
        log ('  -> key=%s', [Keys[i]]);
    inherited Destroy;
end;

function TEventListenerMap.Add(const AKey: string;
    const AData: TListenerProcList): integer;
begin
    Result := inherited add(Akey, AData);
    AData.beforeDo := @Self.incActiveCall;
    AData.afterDo  := @Self.decActiveCall;
end;

function TEventListenerMap.activeCallCount: integer;
begin
    Result := myActiveCallCount;
end;

function TEventListenerMap.incActiveCall: integer;
begin
    EnterCriticalSection(myCriticalSection);
    Inc(myActiveCallCount);
    LeaveCriticalSection(myCriticalSection);
    Result := myActiveCallCount;
end;

function TEventListenerMap.decActiveCall: integer;
begin
    if myActiveCallCount > 0 then
    begin
        EnterCriticalSection(myCriticalSection);
        Dec(myActiveCallCount);
        LeaveCriticalSection(myCriticalSection);
    end;
    Result := myActiveCallCount;
end;

procedure TEventListenerMap.wrapUp;
begin
    while activeCallCount > 0 do
    begin
        sleep(20);
        Application.ProcessMessages;
    end;
end;

{ TObjectEventList }
destructor TObjectEventList.Destroy;
begin
    inherited Destroy;
    log ('----------------------------');
    log ('TObjectEventList.destroyed()');
    log ('----------------------------');
end;



{ TListenerProcRunner }

procedure TProcRunner.runAsync(_listener: TListener);
begin
    //log3('TProcRunner.runAsync:: -->');
    myListener := _listener;
    Application.QueueAsyncCall(@doRunAsync, 0);
end;

procedure TProcRunner.runThread(_listener: TListener);
begin
    //log3('TProcRunner.runThread:: -->');
    myListener := _listener;
    TThread.ExecuteInThread(@doRun);
end;

procedure TProcRunner.runSerial(_listener: TListener);
begin
    //log3('TProcRunner.runSerial:: -->');
    myListener := _listener;
    doRun;
end;

constructor TProcRunner.Create(_freeOnDone: boolean);
begin
    inherited Create;
    myFreeOnDone := _freeOnDone;
    Enabled := True;
    InitCriticalSection(myCs);
end;

destructor TProcRunner.Destroy;
begin
    DoneCriticalSection(myCs);
	inherited Destroy;
end;

procedure TProcRunner.doRunAsync(_param: PtrInt);
begin
    doRun;
end;

procedure TProcRunner.doRun;
var
    a, _objAdd: string;
    _i: integer;
begin
    {
        Because this version has not implemented freeing Listeners when the object is freed
        which then causes the proc address to point to invalid memory
        catch the exception and then disable this myListener.
    }

    log3('   TProcRunner.doRun:: -->');
    try
        if Enabled then
        begin
            if assigned(myListener) then
            begin
                if isObjectAlive(myListener) then
                begin
                    with myListener do
                    begin
                        try
                            if sigType = qThreads then EnterCriticalSection(myCS);
                            if assigned(beforeDo) then beforeDo();

                            if assigned(meth) then
                            try
                                if isObjectAlive(TObject(subscriber)) then
                                    meth(Sender, event, params)
                            except
                                log3('       meth Exception'); //meth := nil;
                            end

                            else if assigned(proc) then
                            try
                                proc(Sender, event, params);
                            except
                                log3('       proc Exception');; // proc is nil
                            end

                            else if assigned(notify) then
                            try
                                if isObjectAlive(TObject(subscriber)) then
                                    notify(Sender);
                            except
                                log3('       notify Exception');;
                            end;

                            try
                                if freeParams then params.Free;
                            except
                                log3('       params.Free Exception');;
                            end;
							if assigned(AfterDo) then AfterDo();
                            if sigType = qThreads then LeaveCriticalSection(myCS);
                        except
                            on E: Exception do
                            begin
                                // procedure is probably pointing to freed memory
                                // Don't run this listener anymore.
                                // Enabled := false;
                                //log3('   doRun: Exception');
                            end;
                        end;
					end;
                end
                else
                    log3('   isObjectAlive(listener) is false');
            end;
        end;

    finally
        if myFreeOnDone then Free; // Destroy itself
    end;
end;



{ TObjectListenerHelper }


function TObjectListenerHelper.Listeners: TEventListenerMap;
var
    _i: integer;
begin
    {
        Listeners returns a map of
        Each control has its own Listeners collection.
        Because type helpers cannot have fields, the list is stored in
        the implementation global variable as a key map.

        Key is the hex representation of the "self" pointer.
    }

    // _i := objectListeners.IndexOf(Self.Name);
    _i := objectListeners.IndexOf(pointerAsHex(Self));
    if _i >= 0 then
        Result := objectListeners.Data[_i]
    else
    begin
        Result := TEventListenerMap.Create;
        objectListeners.Add(pointerAsHex(Self), Result);
    end;
end;

function TObjectListenerHelper.Listener(
    const _event: string): TListenerProcList;
var
    _i: integer;
begin
    _i := Listeners.IndexOf(_event);
    if _i >= 0 then
    begin
        //log2('!!%s Listener found', [_event]);
        Result := Listeners.Data[_i];
    end
    else
    begin
        Result := TListenerProcList.Create;
        Listeners.Add(_event, Result); // return an empty Proclist
        //log2('>> %s added listener', [_event]);
    end;
end;

{==========================================================================}
{  TListenerMethod                                                   }
{==========================================================================}
function TObjectListenerHelper.addListener(const _event: string; constref
	_subscriber: TObject; constref _handler: TListenerMethod; constref
	_sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _L: TListener;
begin
    if assigned(_handler) then
    begin
        _L := TListener.Create;
        _L.Sender := self;
        _L.Subscriber := _subscriber;
        _L.add(_subscriber, _handler, _sigType);
        Listener(_event).Add(_L);
        addSubscriber(_subscriber, self);
    end;
    Result := Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	constref _subscriber: TObject; constref _handler: TListenerMethod;
	constref _sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _event: string;
begin
    for _event in _events do
        addListener(_event, _subscriber, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

function TObjectListenerHelper.addListener(const _event: string; constref
	_subscriber: TObject; constref _handlers: TArrayListenerMethod; constref
	_sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _handler: TListenerMethod;
begin
    for _handler in _handlers do
        addListener(_event, _subscriber, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;


{==========================================================================}
{  Listener Proc                                                           }
{==========================================================================}
function TObjectListenerHelper.addListener(const _event: string; constref
	_handler: TListenerProc; constref _sigType: TInvokeType; constref
	_ignoreduplicates: boolean): TObject;
var
    _L: TListener;
begin
    if assigned(_handler) then
    begin
        _L := TListener.Create;
        _L.add(_handler, _sigType);
        Listener(_event).Add(_L);
    end;
    Result := Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	constref _handler: TListenerProc; constref _sigType: TInvokeType;
	constref _ignoreduplicates: boolean): TObject;
var
    _event: string;
begin
    for _event in _events do
        addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

function TObjectListenerHelper.addListener(const _event: string; constref
	_handlers: TArrayListenerProc; constref _sigType: TInvokeType; constref
	_ignoreduplicates: boolean): TObject;
var
    _handler: TListenerProc;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;


{==========================================================================}
{  TNotifyEvent                                                            }
{==========================================================================}
function TObjectListenerHelper.addListener(const _event: string; constref
	_subscriber: TObject; constref _handler: TNotifyEvent; constref
	_sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _L: TListener;
begin
    if assigned(_handler) then
    begin
        _L := TListener.Create;
        _L.add(_subscriber, _handler, _sigType);
        Listener(_event).Add(_L);
        addSubscriber(_subscriber, Self);
    end;
    Result := Self;
end;

function TObjectListenerHelper.addListener(const _events: TStringArray;
	constref _subscriber: TObject; constref _handler: TNotifyEvent; constref
	_sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _event: string;
begin
    for _event in _events do
        addListener(_event, _subscriber, _handler, _sigType, _ignoreduplicates);

    Result := Self;
end;
function TObjectListenerHelper.addListener(const _event: string; constref
	_subscriber: TObject; constref _handlers: array of TNotifyEvent; constref
	_sigType: TInvokeType; constref _ignoreduplicates: boolean): TObject;
var
    _handler: TNotifyEvent;
begin
    for _handler in _handlers do
        addListener(_event, _subscriber, _handler, _sigType, _ignoreduplicates);
    Result := Self;
end;

procedure TObjectListenerHelper.rmListeners;
var
    _i: integer;
begin
    {WE ALSO NEED A WAY
        To remove proc addresses from other objects whose signals we are listening to
        when we are being destroyed.
        Need more indexes.
    }
    if not objectAlive then exit;
    _i := objectListeners.IndexOf(pointerAsHex(Self));
    if _i > -1 then clearObjectListener(_i);
end;

procedure TObjectListenerHelper.rmListeners(const _event: string);
var
    _i, _j: integer;
begin
    if not objectAlive then exit;
    begin
        _j := listeners.indexOf(_event);
        if _j > -1 then begin
            listeners.Delete(_j);
            log('rmListeners(%s): deleted %d',[_event, _j])
		end
        else
            log('rmListeners(%s): event listener map not found',[_event]);
	end;
end;

procedure TObjectListenerHelper.rmListeners(constref _subscriber: TObject);
var
    _i, _j, _k, _x, _y: integer;
    _signalerList : THexstringMap;
	_listenerMap: TEventListenerMap;
    _procList : TListenerProcList;
    _listener : TListener;
begin
    if not objectAlive then exit;
    //log3('=============== TObjectListenerHelper.rmListeners() ================');
    _y := subscriberObjectMap.IndexOf(pointerAsHex(_subscriber));
    if _y > -1 then
    begin
        //log3('subscriber %s @%s', [_subscriber.ClassName, pointerAsHex(_subscriber)]);

        _signalerList :=subscriberObjectMap.Data[_y];
        for _i := 0 to pred(_signalerList.Count) do begin

            _x := objectListeners.IndexOf(_signalerList.Keys[_i]);
            if _x = -1 then continue;

	        _listenerMap := objectListeners.Data[_x]; // TEventListenerMap(objectListeners.Items[_i]);
	        for _j := 0 to pred(_listenerMap.Count) do begin
	            _procList := _listenerMap.Data[_j];
	            _k := 0;
	            while _k < _procList.Count do begin
	                _listener := _procList.Items[_k];
                    if isObjectAlive(_listener) and isObjectAlive(_listener.subscriber)then begin
                        if _listener.subscriber.equals(_subscriber) then begin
                            //log3('   deleting %s, %s [%d]', [_listener.event, _listener.Subscriber.ClassName, _k]);
                            _procList.Delete(_k);
					    end
                        else
                            inc(_k);
    				end
                    else
                        inc(_k);
				end;
			end;

		end;
	end;
    //log3('=============== DONE ================');
end;


//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handler: TListenerMethod);
//begin

//end;

//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handlers: TArrayListenerMethod);
//begin

//end;

//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handler: TListenerProc);
//begin

//end;

//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handlers: TArrayListenerProc);
//begin

//end;

//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handler: TNotifyEvent);
//begin

//end;

//procedure TObjectListenerHelper.rmListener(const _event: string;
//    const _handlers: array of TNotifyEvent);
//begin

//end;

procedure TObjectListenerHelper.signal(const _event: string;
    constref _params: TJSONObject; _freeParams: boolean);
var
    i: integer;
    _l: TListenerProcList;
    _tmpParams: TJSONObject = nil;
begin
    try
	    if not objectAlive then exit;  // Check if this object is still in scope.
	    _l := Listener(_event);
	    try
	        //log3('TObjectListenerHelper.signal:: --> start (%s, %s)', [_event, _params.FormatJSON()]);
	        for i := 0 to pred(_l.Count) do
	        begin
	            if assigned(_params) then
	            begin
	                // Always call the Listener procedure with
	                // a cloned param object (memory safety).
	                _tmpParams := _params.Clone as TJSONObject;
	            end;
	            _l.Items[i].do_(self, _event, _tmpParams, True {free params because it will be cloned before next call});
	        end;
	    finally

	    end;
    finally
        if _freeParams then _params.Free; // Always free parameters, irrespective of whether there were event handlers or not
	end;
end;

function TObjectListenerHelper.signals: TStringArray;
var
    i: integer;
begin
    Result := [];
    SetLength(Result, Listeners.Count);
    for i := 0 to High(Result) do
    begin
        Result[i] := Listeners.Keys[i];
    end;
end;

function TObjectListenerHelper.memdump: TJSONArray;
var
    _signals: TStringArray;
    _signal: string;
    _procList: TListenerProcList;
    _obj: TJSONObject;
    _i: integer;
begin
    Result := TJSONArray.Create;
    _signals := signals;

    for _signal in _signals do
    begin
        _procList := Listener(_signal);

        _obj := TJSONObject.Create(['control', pointerAsHex(Self),
            'signal', _signal, 'listeners', TJSONArray.Create()]);

        Result.Add(_obj);
        for _i := 0 to pred(_procList.Count) do
        begin
            _obj.arrays['listeners'].add(_proclist.Items[_i].getAsJSON);
        end;
    end;

end;

function TObjectListenerHelper.activeSignalCount: integer;
begin
    Result := Listeners.activeCallCount;
end;

function TObjectListenerHelper.objectAlive: boolean;
begin
    Result := isObjectAlive(self);
end;

procedure TObjectListenerHelper.stopListening;
var
    _signalerList: THexstringMap;
    _signaler : TObject;
    _i, _j: integer;
begin

    if Application.Terminated then
    begin
        log('Exiting stopListening because application is terminated');
        exit;
	end;

    _i := subscriberObjectMap.IndexOf(pointerAsHex(self));
    if _i = -1 then exit;

    _signalerList := subscriberObjectMap.Data[_i];
    for _j := 0 to pred(_signalerList.Count) do
    begin
        _signaler := TObject(hexStrAsPointer(_signalerList.Keys[_j]));
        _signaler.rmListeners(self);
    end;
    subscriberObjectMap.delete(_i);
end;


{ TListener }

function TListener.getAsString: string;
begin
    with getAsJSON() do
    begin
        Result := AsJSON;
        Free;
    end;
end;

function TListener.getAsJSON: TJSONObject;
var
    _s: string = '';
begin
    Result := TJSONObject.Create;
    Result.Add('enabled', myEnabled);
    Result.Add('sender', pointerAsHex(Sender));
    Result.Add('event', event);
    if assigned(proc) then
        Result.Add('proc', 'proc: ' + pointerAsHex(@proc))
    else if assigned(meth) then
        Result.Add('proc', 'meth: ' + pointerAsHex(@meth))
    else if assigned(notify) then
        Result.Add('proc', 'notify: ' + pointerAsHex(@notify))
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

constructor TListener.Create;
begin
    inherited Create;
    myEnabled := True;
end;

procedure TListener.add(constref _proc: TListenerProc;
    const _sigType: TInvokeType = qAsync);
begin
    proc := _proc;
    meth := nil;
    notify := nil;
    sigType := _sigType;
    subscriber := nil;
end;

procedure TListener.add(constref _subscriber: TObject; constref
	_meth: TListenerMethod; const _sigType: TInvokeType);
begin
    meth := _meth;
    proc := nil;
    notify := nil;
    sigType := _sigType;
    subscriber:= _subscriber;
end;

procedure TListener.add(constref _subscriber: TObject; constref
	_notify: TNotifyEvent; const _sigType: TInvokeType);
begin
    notify := _notify;
    proc := nil;
    meth := nil;
    sigType := _sigType;
    subscriber := _subscriber;
end;


procedure TListener.do_(constref _sender: TObject; const _event: string;
    constref _params: TJSONObject; const _freeParams: boolean);
var
    _runner: TProcRunner;
begin
    //log3('TObjectListenerHelper.do_:: -->');
    if not Enabled then exit;

    Sender  := _sender;
    event   := _event;
    params  := _params;
    freeParams := _freeParams;

    //if isObjectAlive(TObject(subscriber)) then
    //    log ('  ## subscriber is %s @%s', [TObject(subscriber).ClassName, pointerAsHex(subscriber)])
    //else
    //    log ('  ## subscriber is dead @%s', [TObject(subscriber).ClassName, pointerAsHex(subscriber)]);

    case ListenerSignalMode of
        lmSingleton:    _runner := procRunner;
        lmDynamic:      _runner := TProcRunner.Create(True); // Free on done.
    end;

    _runner.beforeDo := Self.beforeDo;
    _runner.afterDo  := Self.afterDo;

    //log3('TObjectListenerHelper.do_:: starting runners');
    case sigType of
        qAsync:     _runner.runAsync(self);
        qThreads:   _runner.runThread(self);
        qSerial:    _runner.runSerial(self);
    end;

    if assigned(AfterDo) then AfterDo();
    //log3('<------- TObjectListenerHelper.do_:: -->');
end;

initialization
    objectListeners := TObjectEventList.Create;
    procRunner := TProcRunner.Create; // Don't free on done;
    subscriberObjectMap := TSubscriberObjectMap.Create;
    subscriberObjectMap.Sorted:=True;

finalization
    objectListeners.Free;
    procRunner.Free;
    subscriberObjectMap.Free;

end.
