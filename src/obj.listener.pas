unit Obj.Listener;
{
(c) 2024.  Rubrican Research.
https://github.com/rubrican-research/saph

This library is released under the MIT License.

to freely define event listeners - which is text based, case-sensitive - on any object.
This is implemented as a Type Helper on TObject.

Implement event listeners in your units with the following signature

    procedure (const _sender: TObject; const _event: string; constref _params: TJSONObject);

Also an event listener can be TNotify

Then, for any control in that unit (form, button, editbox etc.) you can now assign an event listener as follows:
    edtName.addListener('change', @FormChange);   // Where FormChange is a general listener for all changes to data on the from
    edtDOB.addListener('change',  @FormChange);    // EditBox for Date of Birth - change is listened by FormChange
    edtDOB.addListener('change',  @CalculateAge);  // Same Date of Birth edit box, the change will be listened by CalculateAge

About Signals
    This library provides a mechanism to implement complex signalling systems on objects.
    When a signal is designed, there are three things to consider:
        - points of invocation:
                        Clarity is required to list all the points in the logic where an object
                        will send a signal. Avoid using string literals when adding listeners or sending signals
                        as any typos would be very hard to find when signals don't behave as expected. It is best
                        to define all signals as constants in the unit where an object is intended to send signals

                        Listeners use the constants in the listener procs (_event) if they need to know what signal
                        was sent (except the TNotify listener, which does not send any event or parameters)

        - parameters:
                        because the parameters of a signal are implemented as a JSONObject, it is important
                        to define each field in the parameter list and denote its purpose. It may prove to be
                        useful to create a separate unit that contains factory methods to instantiate the
                        parameter object of a signal, which provides a good place in the the code to document
                        how a signal may be consumed.

        - avoid recursion:
                        Because of the inherent async nature of signals, we can have a situation akin to callback hell,
                        where it can become impossible to keep track of when signals being fired. This could make it
                        extremely difficult to debug the code.

                        One hack is to use a global search by signal name

                        Object Pascal currently does not support anonymous functions yet (which would make for readable
                        code to know what a listener would do at the place where it is being added) so one would have to
                        jump around in the code quite a bit to locate different listenenrs

        ** CALL stopListening() in your destructors **
                        when an object consumes signals, its address is registerd in the library. When the object is
                        freed, the library does not implicitly know that the object has been freed. So, in the destructor
                        of the object, call "stopListening()", it will delete the entries for this object.

                        Exceptions are handled in the library if the listener object is not alive anymore, but it makes debugging
                        very cumbersome because every call to a deleted object will raise an exception in the debugger
                        (not in the final executable).
        thread safety:
                        One of the listener types is qThreads, which calls each listner method inside its own thread.
                        The library has a dedicated variable
                            runnerCS: TRTLCriticalSection;
                        which MUST be used inside your listener methods if you have opted to consume the signal as  qThread.

                        NOTE: Threaded listeners are not stable as of this release. They work most of the time
                        as long a listener method listens to only one signal. It leads to unstable conditions if you do
                        something like this
                            obj.addListener('sigA', self, @listener1, qThreads);
                            obj.addListener('sigB', self, @listener1, qThreads);

                        two signals "sigA" and "sigB" are calling the same method "listener1". This works well for aAsync and qSerial. But
                        for qThread, it can cause unexpected exceptions if the method accesses any global variables.

Best Practices
    Ideally, there should be a dedicated method in every unit or class that adds listeners. This way it is easy to see at a
    glance how many listeners are assigned to an object OR all the different objects that are being listened to within a
    listener implementation.

    Factory functions that instantiate objects are a good way to keep the listener definitions in a single place. If a class
    consumes objects from other classes and also manages their lifecycles, this style is better suited, for it guarantees that
    every instance of a particular class has the exact same set of listeners

    The  listener code can make decisions based on:
    - _sender (which tells you which object has signalled)
    - _event (the string literal of the event)

    NOTE: the lifecycle of the param object is managed by the listener library. It will be freed as soon as the listener method
    is exited. So, if you need for it to persist outside of the method, you would have to clone it.
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

    TListenerProc = procedure(const _sender: TObject; const _event: string; constref _params: TJSONObject);
    TListenerMethod = procedure(const _sender: TObject; const _event: string; constref _params: TJSONObject) of object;

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
    public
        constructor Create(AFreeObjects: Boolean = true);
        destructor Destroy; override;
        function Add(const AKey: string; const AData: TListenerProcList): integer; inline;
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


        procedure rmListeners; // Removes all listeners on this object
        procedure rmListeners(const _event: string); // Removes listeners on this event of this object
        procedure rmListeners(constref _subscriber: TObject); // Removes listeners that belong to this subscriber

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

        function activeSignalCount: integer; // Returns total number of all signals that are active at the present moment.
                                             // This is not the count of active signals of this object.

        // Is this object currenly alive ?
        // hack to handle cases where the memory address will be accessed
        // but we don't know if the object has been freed.
        function objectAlive: boolean;

        {IMPORTANT}
        // You should call this in the destructor of your objects that
        // are listening to signals.
        // This is for listener objects, not signalling.
        // It removes all methods that were added as listeners across all
        // objects in the library.
        procedure stopListening;
    end;

    TListenerSignalMode = (lmSingleton, lmDynamic);

// Hack to check if an object address points to a valid object.
function isObjectAlive(constref _obj: TObject): boolean;

// calls Application.ProcessMessages if _hold milliseconds have passed since last "breath"
procedure Breathe(_hold : Qword = 0);

var
    //ListenerSignalMode: TListenerSignalMode = lmSingleton;
    ListenerSignalMode: TListenerSignalMode = lmDynamic;
    runnerCS: TRTLCriticalSection;

implementation

uses
    StrUtils
    {, sugar.logger}
    ;

type

    { TProcRunner }

    // IMPORTANT.
    // This class frees itself after running
    // See doRun();
    TProcRunner = class
    private
        myFreeOnDone: boolean;
        myListener: TListener;
        procedure doRun;                        // Can be called in a thread as well.
        procedure doRunAsync(_param: PtrInt);   // For Application.QueueAsyncCall()
    public
        beforeDo: function: integer of object;
        afterDo: function: integer of object;
        Enabled: boolean;
        procedure runAsync(constref  _listener: TListener);
        procedure runThread(constref _listener: TListener);
        procedure runSerial(constref _listener: TListener);
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

{================================================================================}
{                       MANAGE SIGNAL PENDING                                    }
{================================================================================}
    activeSignals: integer;         // count of active signals
    signalCountCS: TRTLCriticalSection;


    function incSignalCount: integer;
    begin
        EnterCriticalSection(signalCountCS);
        InterlockedIncrement(activeSignals);
        LeaveCriticalSection(signalCountCS);
        Result := activeSignals;
    end;

    function decSignalCount: integer;
    begin
        if activeSignals > 0 then
        begin
            EnterCriticalSection(signalCountCS);
            InterlockedDecrement(activeSignals);
            LeaveCriticalSection(signalCountCS);
        end;
        Result := activeSignals;
    end;

    procedure wrapUpSignals;
    begin
        //log('wrapUpSignals()--->');
        while activeSignals > 0 do
        begin
            sleep(10);
            Application.ProcessMessages;
		end;
        //log('<----- wrapUpSignals() done.');
	end;

{================================================================================}


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



{================ BREATHE =====================================}
var
    lastBreath: QWord = 0;
procedure Breathe(_hold : Qword = 0);
begin
    //log('Breath : %s', [ IntToStr(getTickCount64() - lastBreath)]);
    if (getTickCount64() - lastBreath) >= _hold then begin
        Application.ProcessMessages;
        lastBreath := getTickCount64();
        //log('...... whew() .......');
	end;
end;
//============================================================

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
    //log('TListenerProcList.Destroy() %s', [key] );
	inherited Destroy;
end;

{ TEventListenerMap }

constructor TEventListenerMap.Create(AFreeObjects: Boolean);
begin
    inherited;
end;

destructor TEventListenerMap.Destroy;
//var
//	i: Integer;
begin
    //log('TEventListenerMap.Destroy start::');
    //for i := 0 to pred(count) do
        //log ('  -> key=%s', [Keys[i]]);
    //log('TEventListenerMap.Destroy done::');

    inherited Destroy;
end;

function TEventListenerMap.Add(const AKey: string;
    const AData: TListenerProcList): integer;
begin
    Result := inherited add(Akey, AData);
    AData.beforeDo := nil;
    AData.afterDo  := nil;
end;

{ TObjectEventList }
destructor TObjectEventList.Destroy;
begin
    inherited Destroy;
    //log ('----------------------------');
    //log ('TObjectEventList.destroyed()');
    //log ('----------------------------');
end;

{ TListenerProcRunner }

procedure TProcRunner.runAsync(constref _listener: TListener);
begin
    //log('TProcRunner.runAsync:: -->');
    myListener := _listener;
    Application.QueueAsyncCall(@doRunAsync, 0);
    breathe;
end;

procedure TProcRunner.runThread(constref _listener: TListener);
begin
    //log3('TProcRunner.runThread:: -->');
    myListener := _listener;
    TThread.ExecuteInThread(@Self.doRun);

end;

procedure TProcRunner.runSerial(constref _listener: TListener);
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

end;

destructor TProcRunner.Destroy;
begin
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

    //log3('   TProcRunner.doRun:: -->');
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
                            if assigned(beforeDo) then beforeDo();

                            if assigned(meth) then
	                            try
	                                if isObjectAlive(subscriber) then
	                                    meth(Sender, event, params)
	                            except
	                                //log('       meth Exception'); //meth := nil;
	                            end

                            else if assigned(proc) then
	                            try
	                                proc(Sender, event, params);
	                            except
	                                //log('       proc Exception'); // proc is nil
	                            end

                            else if assigned(notify) then
	                            try
	                                if isObjectAlive(subscriber) then
	                                    notify(Sender);
	                            except
	                                //log('       notify Exception');
	                            end;

                            try
                                if sigType = qThreads then
                                    EnterCriticalSection(runnerCS);
                                if freeParams then params.Free;
                            except
                                //log('       params.Free Exception');;
                            end;
                            if sigType = qThreads then begin
                                LeaveCriticalSection(runnerCS);
                                sleep(300);
							end;
							if assigned(AfterDo) then AfterDo();

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
                    //log('   isObjectAlive(listener) is false');
            end;
        end;

    finally
        decSignalCount;
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
            //log('rmListeners(%s): deleted %d',[_event, _j])
		end
        else
            //log('rmListeners(%s): event listener map not found',[_event]);
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

{---- thread safety hack -------- }
{see below after EnterCriticalSection(runnerCS)}
var
    _j: TJSONObject;
{-----------------------------------}

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

                    EnterCriticalSection(runnerCS);
                    _j := _params.Clone as TJSONObject;
                    LeaveCriticalSection(runnerCS);

                    //log('signal() params clone : %s', [_j.formatJSON(AsCompactJSON)]);
	                _l.Items[i].do_(self, _event, _j, True {free params because it will be cloned before next call});
	            end
                else
                    _l.Items[i].do_(self, _event, nil, false);
	        end;
	    except
            On E:Exception do begin
                //log('signal(): Exception ' + E.Message);
			end;
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
    Result := activeSignals;
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
        //log('Exiting stopListening because application is terminated');
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

    //if assigned(params) then
    //    log('TListener.do_ params: %s', [params.formatJSON(AsCompressedJSON)]);

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

    incSignalCount;
    //log3('TObjectListenerHelper.do_:: starting runners');
    case sigType of
        qAsync:     _runner.runAsync(self);
        qThreads:   _runner.runThread(self);
        qSerial:    _runner.runSerial(self);
    end;

    //log3('<------- TObjectListenerHelper.do_:: -->');
end;

initialization
    InitCriticalSection(signalCountCS);
    InitCriticalSection(runnerCS);
    objectListeners := TObjectEventList.Create;
    procRunner := TProcRunner.Create; // Singleton runner: don't free when method is done;
    subscriberObjectMap := TSubscriberObjectMap.Create;
    subscriberObjectMap.Sorted:=True;

finalization
    wrapUpSignals;
    objectListeners.Free;
    procRunner.Free;
    subscriberObjectMap.Free;
    DoneCriticalSection(signalCountCS);
    DoneCriticalSection(runnerCS);
end.
