unit Controls.Listener;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch typehelpers}

interface

uses
    Classes, SysUtils, Controls, Forms, fpjson, fgl;

type
    TControlListenerProc   = procedure (_sender: TControl; _event: string; _params: TJSONObject);
    TControlListenerMethod = procedure (_sender: TControl; _event: string; _params: TJSONObject) of object;

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
      procedure runInThread;
      procedure runAsync(_param: PtrInt);
	public
	  procedure add(_proc: TControlListenerProc); overload;
	  procedure add(_meth: TControlListenerMethod); overload;
	  procedure do_(_sender: TControl; _event: string; _params: TJSONObject; _freeParams: Boolean = true; _sigType: TListenerSignalType = qAsync);
	end;

	TControlListenerProcList   = specialize TFPGObjectList<TControlListener>;             // List of listeners
    TControlListenerCollection = specialize TFPGMap<string, TControlListenerProcList>;    // Map of Event and List of listener procedures
    TListenerList              = specialize TFPGMap<string, TControlListenerCollection>;  // Map of Control Name and List of Event Listeners

	{ TControlListenerHelper }

    TControlListenerHelper = class helper for TControl
        function listeners: TControlListenerCollection;
        function listener(_event: string): TControlListenerProcList;

        function addListener(_event: string; _handler: TControlListenerMethod; _ignoreduplicates: boolean = true) : TControl; overload;
        function addListener(_event: string; _handler: TControlListenerProc; _ignoreduplicates: boolean = true) : TControl; overload;

        procedure signal(_event: string; _params: TJSONObject=nil; _sigType: TListenerSignalType = qAsync; _freeParams: Boolean = true);
        function signals: TStringArray;
	end;

implementation

 var
    myListenerList : TListenerList;


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

function TControlListenerHelper.listener(_event: string): TControlListenerProcList;
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

function TControlListenerHelper.addListener(_event: string;
	_handler: TControlListenerMethod; _ignoreduplicates: boolean): TControl;
var
   _L : TControlListener;
begin
    _L := TControlListener.Create;
    _L.add(_handler);
    listener(_event).Add(_L);
    Result:= Self;
end;

function TControlListenerHelper.addListener(_event: string;
	_handler: TControlListenerProc; _ignoreduplicates: boolean): TControl;
var
   _L : TControlListener;
begin
    _L := TControlListener.Create;
    _L.add(_handler);
    listener(_event).Add(_L);
    Result:= Self;
end;

procedure TControlListenerHelper.signal(_event: string; _params: TJSONObject;
	_sigType: TListenerSignalType; _freeParams: Boolean);
var
   i : integer;
   _l : TControlListenerProcList;
   _tmpParams: TJSONObject = nil;
begin
    _l := listener(_event);

    if assigned(_params) then begin
        if _freeParams then
            _tmpParams:= _params.Clone as TJSONObject
        else
            _tmpParams := _params;
    end;

    for i := 0 to pred(_l.Count) do begin
        _l.Items[i].do_(self, _event, _tmpParams, _freeParams, _sigType)
	end;

    if _freeParams then
        _params.Free;
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

procedure TControlListener.runInThread;
begin
  if assigned(meth) then
      meth(sender, event, params)
  else if assigned(proc) then
      proc(sender, event, params);
  if freeParams then params.Free;
end;

procedure TControlListener.add(_proc: TControlListenerProc);
begin
    proc:= _proc;
    meth:= nil;
end;

procedure TControlListener.add(_meth: TControlListenerMethod);
begin
    meth:= _meth;
    proc:=  nil;
end;

procedure TControlListener.runAsync(_param: PtrInt);
var
   _listener: TControlListener;
begin
    _listener := TControlListener(_param);
    with _listener do begin
        if assigned(meth) then
            meth(sender, event, params)
        else if assigned(proc) then
            proc(sender, event, params);

        if freeParams then params.Free;
	end;
end;

procedure TControlListener.do_(_sender: TControl; _event: string;
	_params: TJSONObject; _freeParams: Boolean; _sigType: TListenerSignalType);
begin
    sender      := _sender;
    event       := _event;
    params      := _params;
    freeParams  := _freeParams;
    sigType     := _sigType;

    case sigType of
        qAsync:     Application.QueueAsyncCall( @RunAsync, PtrInt(self));
        qThreads:   TThread.ExecuteInThread(@runInThread);
    end;

end;

initialization
    myListenerList := TListenerList.Create;

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

end.

