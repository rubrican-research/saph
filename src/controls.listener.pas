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
      sender: TControl;
      event: string;
	  proc: TControlListenerProc;
	  meth: TControlListenerMethod;
      params: TJSONObject;
      freeParams: boolean;
      sigType: TInvokeType;
	public
	  procedure add(constref _proc: TControlListenerProc; const _sigType: TInvokeType = qAsync); overload;
	  procedure add(constref _meth: TControlListenerMethod; const _sigType: TInvokeType = qAsync); overload;
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
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;
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

        function addListener(
                        const _event: string;
                        const _handlers: TArrayControlListenerProc;
                        const _sigType: TInvokeType = qAsync;
                        const _ignoreduplicates: boolean = true) : TControl; overload;

        procedure signal(const _event: string; constref _params: TJSONObject=nil; _freeParams: Boolean = true);
        function signals: TStringArray;

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

	    if freeParams then params.Free;

    end;

    if myFreeOnDone then Free; // Destroy itself
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
                const _sigType: TInvokeType;
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

function TControlListenerHelper.addListener(const _event: string;
	const _handlers: TArrayControlListenerMethod; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_handler: TControlListenerMethod;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
end;

function TControlListenerHelper.addListener(
        const _event: string;
	    const _handler: TControlListenerProc;
        const _sigType: TInvokeType;
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

function TControlListenerHelper.addListener(const _event: string;
	const _handlers: TArrayControlListenerProc; const _sigType: TInvokeType;
	const _ignoreduplicates: boolean): TControl;
var
	_handler: TControlListenerProc;
begin
    for _handler in _handlers do
        addListener(_event, _handler, _sigType,_ignoreduplicates);
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

{ TControlListener }


procedure TControlListener.add(constref _proc: TControlListenerProc; const _sigType: TInvokeType = qAsync);
begin
    proc:= _proc;
    meth:= nil;
end;

procedure TControlListener.add(constref _meth: TControlListenerMethod; const _sigType: TInvokeType = qAsync);
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

