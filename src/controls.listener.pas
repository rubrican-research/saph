unit Controls.Listener;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, Controls, Forms, fpjson, fgl;

type
    TControlListenerProc   = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject);
    TControlListenerMethod = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject) of object;

    TListenerSignalType = (
                            qAsync,     // Queues the listeners to Application.QueueAsyncCall();
                            qThreads,   // Runs the methods in individual threads;
                            qSerial     // Runs in a blocking loop
                          );

	{ TControlListener }
    TControlListener = class
	private
      sender: TControl;
      event: string;
	  proc: TControlListenerProc;
	  meth: TControlListenerMethod;
      params: TJSONObject;
      freeParams: boolean;
      sigType: TListenerSignalType;
	public
	  procedure add(constref _proc: TControlListenerProc; const _sigType: TListenerSignalType = qAsync); overload;
	  procedure add(constref _meth: TControlListenerMethod; const _sigType: TListenerSignalType = qAsync); overload;
	  procedure do_(constref _sender: TControl; const _event: string; constref _params: TJSONObject; const _freeParams: Boolean = true);
	end;

	TControlListenerProcList   = specialize TFPGObjectList<TControlListener>;             // List of listeners
    TControlListenerCollection = specialize TFPGMap<string, TControlListenerProcList>;    // Map of Event and List of listener procedures
    TListenerList              = specialize TFPGMap<string, TControlListenerCollection>;  // Map of Control Name and List of Event Listeners

	{ TControlListenerHelper }

    TControlListenerHelper = class helper for TControl
        function listeners: TControlListenerCollection;
        function listener(const _event: string): TControlListenerProcList;

        function addListener(
                        const _event: string;
                        const _handler: TControlListenerMethod;
                        const _sigType: TListenerSignalType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        function addListener(
                        const _event: string;
                        const _handler: TControlListenerProc;
                        const _sigType: TListenerSignalType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        procedure signal(const _event: string; constref _params: TJSONObject=nil; _freeParams: Boolean = true);
        function signals: TStringArray;
	end;

    TListenerSignalMode = (lmSingleton, lmDynamic);

var
    ListenerSignalMode: TListenerSignalMode = lmSingleton;
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
        procedure doRun;    // Can be called in a thread as well.
        procedure doRunAsync(_param: PtrInt);
    public
        procedure runAsync (_listener: TControlListener);
        procedure runThread(_listener: TControlListener);
        procedure runSerial(_listener: TControlListener);
        constructor Create(_freeOnDone: boolean = false);
	end;

 var
     // See Initialization section
    myListenerList : TListenerList;
    myRunner: TListenerProcRunner;

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
end;

procedure TListenerProcRunner.doRunAsync(_param: PtrInt);
begin
    listener := TControlListener(_param);
    doRun;
end;

procedure TListenerProcRunner.doRun;
begin
    if assigned(listener) then with listener do begin
	    if assigned(meth) then
	        meth(sender, event, params)
	    else if assigned(proc) then
	        proc(sender, event, params);

	    if freeParams then
            params.Free;
    end;

    if myFreeOnDone then
        Free; // Destroy itself
end;



{ TControlListenerHelper }

function TControlListenerHelper.listeners: TControlListenerCollection;
var
   _i: integer;
begin
    _i := myListenerList.IndexOf(Self.Name);
    if _i >= 0 then
        Result:= myListenerList.Data[_i]
    else begin
        Result:= TControlListenerCollection.Create;
        myListenerList.Add(Self.Name, Result);
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

function TControlListenerHelper.addListener(
                const _event: string;
                const _handler: TControlListenerMethod;
                const _sigType: TListenerSignalType;
                const _ignoreduplicates: boolean
	): TControl;
var
   _L : TControlListener;
begin
    _L := TControlListener.Create;
    _L.add(_handler);
    listener(_event).Add(_L);
    Result:= Self;
end;

function TControlListenerHelper.addListener(
        const _event: string;
	    const _handler: TControlListenerProc;
        const _sigType: TListenerSignalType;
        const _ignoreduplicates: boolean
	): TControl;
var
   _L : TControlListener;
begin
    _L := TControlListener.Create;
    _L.add(_handler);
    listener(_event).Add(_L);
    Result:= Self;
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
    _l := listener(_event);


    if _l.Count >0 then begin // Do this only if there are listeners
        if assigned(_params) then begin
            _tmpParams:= _params.Clone as TJSONObject // Always call the listener procedure with a cloned param object (memory safety).
        end;

        for i := 0 to pred(_l.Count) do begin
            _l.Items[i].do_(self, _event, _tmpParams, true {force params to be freed})
    	end;
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

{ TControlListener }


procedure TControlListener.add(constref _proc: TControlListenerProc; const _sigType: TListenerSignalType = qAsync);
begin
    proc:= _proc;
    meth:= nil;
end;

procedure TControlListener.add(constref _meth: TControlListenerMethod; const _sigType: TListenerSignalType = qAsync);
begin
    meth:= _meth;
    proc:=  nil;
end;


procedure TControlListener.do_(constref _sender: TControl;
	const _event: string;
    constref _params: TJSONObject;
    const _freeParams: Boolean
	);
var
	_runner: TListenerProcRunner;

begin
    sender      := _sender;
    event       := _event;
    params      := _params;
    freeParams  := _freeParams;

    case ListenerSignalMode of
    	lmSingleton: _runner     := myRunner;
        lmDynamic:   _runner     := TListenerProcRunner.Create(true); // Free on done.
    end;

    case sigType of
        qAsync:     _runner.runAsync(self);
        qThreads:   _runner.runThread(self);
        qSerial:    _runner.runSerial(self);
    end;
end;

initialization
    myListenerList  := TListenerList.Create;
    myRunner        := TListenerProcRunner.Create; // Don't free on done;

finalization

    while myListenerList.Count>0 do begin               // Loop of control event listeners
        while myListenerList.Data[0].Count > 0 do begin // Loop of event listeners
            myListenerList.Data[0].Data[0].Free;        // TControlListenerProcList
            myListenerList.Data[0].Delete(0);
        end;
        myListenerList.Data[0].Free;
        myListenerList.Delete(0);
    end;

    myListenerList.Free;
    myRunner.Free;

end.
