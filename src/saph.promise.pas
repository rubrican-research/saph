unit saph.promise;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl;
type

    TPromise = class; // forward declaration
    TPromiseClass = class of TPromise;

	{ TPromiseCallBack }

    TPromiseCallBack = class
        owner: TPromise;
        constructor Create;
        constructor Create(constref _owner: TPromise);
        procedure execute; virtual; abstract;
	end;

	{ TPResolve }
    TPromiseError = class;
    TPromiseErrorClass = class of TPromiseError;

    TPResolve = class(TPromiseCallBack)
		procedure execute; override;
    end;
    TPResolveClass = class of TPResolve;

	{ TPReject }

    TPReject  = class(TPromiseCallBack)
        reason: string;
        errClass: TPromiseErrorClass;
		procedure execute; override;
    end;
    TPRejectClass = class of TPReject;

	{ TPromiseError }

    TPromiseError = class(TPromiseCallBack)
	protected
		myReject: TPReject;
		procedure setReject(AValue: TPReject); virtual; // implement additional validation to evaluate the reject object
    public
        reason: string;
	    procedure execute; override;
        property reject: TPReject read myReject write setReject;
    end;





    TPromiseClassArray = array of TPromiseClass;
    TPromiseArray = array of TPromise;
    TPromiseList = class(specialize TFPGObjectList<TPromise>);


    TPromiseResult = (  promiseUnknown,
                        promiseInit,                // Init value
                        promiseRunning,             // The action is still running
                        promiseResolved,            // The action is resolved
                        promiseRejected,            // The action is rejected
                        promiseException,           // The action resulted in an error
                        promiseTimeOut,
                        promiseKilled
                      );
{
RTLEventCreate: Create a new RTL event
RTLEventDestroy:  Destroy a RTL Event
RTLEventSetEvent: Notify threads of the event.
RTLEventReSetEvent: Reset an event
RTLEventWaitFor: Wait for an event.
}

	{ TPromiseAction }
    TPromiseActionClass = class of TPromiseAction;
    TPromiseAction = class
    private
        const __waitLoopSleep = 50; // milliseconds
    private
        //myEventResolve: PRTLEvent;    // unused
        //myEventReject : PRTLEvent;    // unused
        //myEventException : PRTLEvent; // unused
        //myEventPromiseDone: PRTLEvent; // Announces that the promise is finished

        // Don't call until object is being destroyed
        procedure destroyEvents;
		function getStatus: TPromiseResult;
		procedure setStatus(AValue: TPromiseResult);
        function isKilled: boolean;

    protected
        myOwnder : TPromise;
        myRunning: boolean;
        myResult : TPromiseResult;

    public
        constructor Create(constref _owner: TPromise);
        destructor Destroy; override;
    public
        procedure run(constref _resolve: TPResolve; const _reject: TPReject; const ownObjects: boolean = true); overload; virtual; abstract;
        procedure run(const _reject: TPReject; const ownObjects: boolean = true); overload; virtual; abstract;
        function wait(_timeOut: word = 2000): TPromiseResult;
        function waitForever: TPromiseResult;
        function isRunning: boolean; virtual;


        procedure kill;
    published
        property status: TPromiseResult read getStatus write setStatus;
	end;

    RPromise = record
        resolve: TPResolve;
        reject : TPReject;
        catch  : TPromiseError;
	end;

    TPromiseExecFunction    = function (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false): TPromiseResult of object;
    TPromiseResolvedMeth    = procedure(constref _resolve: TPResolve) of object;
    TPromiseRejectedMeth    = procedure(constref _reject: TPReject) of object;
    TPromiseCatchMeth       = procedure(constref _catch:  TPromiseError) of object;

    TPromise = class
    private type

        // For chaining then calls
        TPromiseFuncCaller = class
            resolveClass: TPResolveClass;
            rejectClass : TPRejectClass;
            execFunc  : TPromiseExecFunction;
        end;
        TExecFuncList = class(specialize TFPGObjectList<TPromiseFuncCaller>);

    private
        myOwnObjects    : boolean;
        myExecFuncList  : TExecFuncList;

        myResolveClass: TPResolveClass;
        myRejectClass : TPRejectClass;
        myErrorClass  : TPromiseErrorClass;
        myExecFunc    : TPromiseExecFunction;
        myFinalMeth   : TNotifyEvent;

        myStatus      : TPromiseResult;

    protected
        procedure Init;
        {Call these functions within the action. Set results to }

        function doResolve(): TPResolve;
        function doReject() : TPReject;
        function doCatch()  : TPromiseError;
        function doFinally(): TPromise;

    public
        class function all(constref _promises: TPromiseList)    : TPromise;overload;
        class function all(const _promises: TPromiseArray)      : TPromise;overload;
        class function all(const _promises: TPromiseClassArray) : TPromise;overload;

        class function allSettled(constref _promises: TPromiseList)   : TPromise;overload;
        class function allSettled(const _promises: TPromiseArray)     : TPromise;overload;
        class function allSettled(const _promises: TPromiseClassArray): TPromise;overload;

        class function any(constref _promises: TPromiseList)    : TPromise;overload;
        class function any(const _promises: TPromiseArray)      : TPromise;overload;
        class function any(const _promises: TPromiseClassArray) : TPromise;overload;

        class function race(constref _promises: TPromiseList)   : TPromise;overload;
        class function race(const _promises: TPromiseArray)     : TPromise;overload;
        class function race(const _promises: TPromiseClassArray): TPromise;overload;

        class function withResolvers(): TPromise;
        class function reject() : TPReject;
        class function resolve(): TPResolve;

    public

        constructor Create(_execFunc: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil); overload;
        destructor Destroy; override;

    public
        function then_ (_action: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil): TPromise; overload;
        function catch_(_errClass: TPromiseErrorClass): TPromise; overload;
        function finally_(_finalMeth : TNotifyEvent): TPromise;

    public
        procedure run;
        procedure runAsync;
        procedure runThread;
    public
        OnPromiseResoved    : TPromiseResolvedMeth;
        OnPromiseRejected   : TPromiseRejectedMeth;
        OnPromiseException  : TPromiseCatchMeth;
        OnPromiseTimeOut    : TPromiseCatchMeth;
        OnPromiseKilled     : TPromiseCatchMeth;
    end;

    function Promise(_action: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil): TPromise;


implementation

function Promise(_action: TPromiseExecFunction; _resolveClass: TPResolveClass;
	_rejectClass: TPRejectClass): TPromise;
begin
    Result := TPromise.Create(_action, _resolveClass, _rejectClass);
end;



{ TPromiseAction }

procedure TPromiseAction.destroyEvents;
begin
    //RTLEventDestroy(myEventPromiseDone);
    //RTLEventDestroy(myEventResolve);
    //RTLEventDestroy(myEventReject);
    //RTLEventDestroy(myEventException);
end;

function TPromiseAction.getStatus: TPromiseResult;
begin
    Result := myResult;

end;

procedure TPromiseAction.setStatus(AValue: TPromiseResult);
begin
    if myResult = AValue then exit;

    // Don't allow change of status after promise has been killed
    if myResult <> promiseKilled then begin
        myResult := AValue;
	end;
end;

function TPromiseAction.isKilled: boolean;
begin
    Result := (myResult = promiseKilled);
end;

constructor TPromiseAction.Create(constref _owner: TPromise);
begin
    inherited Create;
    myOwnder := _owner;
    //myEventPromiseDone  := RTLEventCreate();
    //myEventResolve      := RTLEventCreate();
    //myEventReject       := RTLEventCreate();
    //myEventException    := RTLEventCreate();
end;

destructor TPromiseAction.Destroy;
begin
    destroyEvents;
	inherited Destroy;
end;

procedure TPromiseAction.kill;
begin
    myResult := promiseKilled;
end;

function TPromiseAction.wait(_timeOut: word): TPromiseResult;
var
	_start: QWord;
begin
    _start := GetTickCount64();
    while myResult = promiseRunning do begin
        sleep(__waitLoopSleep);
        if GetTickCount64() - _start > _timeOut then begin
            myResult := promiseTimeOut;
		end;
	end;
    Result := myResult;
end;

function TPromiseAction.waitForever: TPromiseResult;
begin
    while (myResult = promiseRunning) and not isKilled do sleep(__waitLoopSleep);
end;

function TPromiseAction.isRunning: boolean;
begin
    Result := (myResult = promiseRunning);
end;

function TPromise.doReject(): TPReject;
begin

end;

procedure TPromise.Init;
begin
    myExecFuncList  := TExecFuncList.Create();
end;

function TPromise.doResolve(): TPResolve;
begin

end;

function TPromise.doCatch(): TPromiseError;
begin

end;

function TPromise.doFinally(): TPromise;
begin

end;

class function TPromise.all(constref _promises: TPromiseList): TPromise;
begin

end;

class function TPromise.all(const _promises: TPromiseArray): TPromise;
begin

end;

class function TPromise.all(const _promises: TPromiseClassArray): TPromise;
begin

end;

class function TPromise.allSettled(constref _promises: TPromiseList): TPromise;
begin

end;

class function TPromise.allSettled(const _promises: TPromiseArray): TPromise;
begin

end;

class function TPromise.allSettled(const _promises: TPromiseClassArray
	): TPromise;
begin

end;

class function TPromise.any(constref _promises: TPromiseList): TPromise;
begin

end;

class function TPromise.any(const _promises: TPromiseArray): TPromise;
begin

end;

class function TPromise.any(const _promises: TPromiseClassArray): TPromise;
begin

end;

class function TPromise.race(constref _promises: TPromiseList): TPromise;
begin

end;

class function TPromise.race(const _promises: TPromiseArray): TPromise;
begin

end;

class function TPromise.race(const _promises: TPromiseClassArray): TPromise;
begin

end;

class function TPromise.reject: TPReject;
begin

end;

class function TPromise.resolve: TPResolve;
begin

end;

constructor TPromise.Create(_execFunc: TPromiseExecFunction;
	_resolveClass: TPResolveClass; _rejectClass: TPRejectClass);
begin
    inherited Create;
    myExecFuncList := TExecFuncList.Create(true);

    myExecFunc := _execFunc;
    if assigned(_resolveClass) then
        myResolveClass  := _resolveClass
    else
        myResolveClass := TPResolve;

    if assigned(_rejectClass) then
        myRejectClass   := _rejectClass
    else
        myRejectClass := TPReject;

    myErrorClass := TPromiseError;
    myStatus := promiseInit;
    then_(myExecFunc, myResolveClass, myRejectClass); // Put this as the first item in the list

end;

procedure TPromise.run;
var
	i: Integer;
	_execFuncCaller: TPromiseFuncCaller;
    _resolve: TPResolve;
    _reject: TPReject;
    _catch : TPromiseError;
    _resolveCount: integer = 0;

    function createErrorObject(_reject: TPReject): TPromiseError;
    begin
        if assigned(_reject.errClass) then
            Result := _reject.errClass.Create(Self)
        else
            Result := myErrorClass.Create(Self);
        Result.reject := _reject;
        Result.reason := _reject.reason;
	end;

begin
    try
	   	for i := 0 to pred(myExecFuncList.Count) do begin
	   	    _execFuncCaller := myExecFuncList.Items[i];
	        _resolve := _execFuncCaller.resolveClass.Create(Self);
	        _reject  := _execFuncCaller.rejectClass.Create(Self);
            try
    		    {Call the default action}

                case _execFuncCaller.execFunc(_resolve, _reject) of
                    promiseUnknown:     begin
                        myStatus := promiseUnknown;
                        _catch := myErrorClass.Create(Self);
                        _catch.reject := _reject;
                        _catch.reason := ClassName + ' execFunc returned promiseUnknown';
                        raise _catch;
                    end;

                    promiseRunning:  begin // The action is still running
                        myStatus := promiseException;
                        _catch := myErrorClass.Create(Self);
                        _catch.reject := _reject;
                        _catch.reason := ClassName + ' execFunc returned promiseRunning';
                        raise _catch;
                    end;

                    promiseResolved: begin // The action is resolved
                        inc(_resolveCount);
                        _resolve.execute;
                    end;

                    promiseRejected: begin       // The action is rejected
                        myStatus := promiseRejected;
                        raise createErrorObject(_reject);
    				end;

                    promiseException: begin
                        myStatus := promiseException;
                        raise createErrorObject(_reject);
    				end;

                    promiseTimeOut: begin
                        myStatus := promiseTimeOut;
                        raise createErrorObject(_reject);
    				end;

                    promiseKilled: begin
                        myStatus := promiseKilled;
                        raise createErrorObject(_reject);
    				end;
                end;

    		except
                on _e: TPromiseError do begin
                    {Call the catch function}
                    _e.execute;
                    break;
				end;
			end;
   	    end;// For loop
	finally

        if _resolveCount = myExecFuncList.Count then
             myStatus := promiseResolved;

        {Call the finally}
        try
            if assigned(myFinalMeth) then
                myFinalMeth(Self);
		except
            ;
		end;
	end;

end;

procedure TPromise.runAsync;
begin

end;

procedure TPromise.runThread;
begin

end;

class function TPromise.withResolvers(): TPromise;
begin

end;


destructor TPromise.Destroy;
begin
    myExecFuncList.Free;
    inherited;
end;

function TPromise.then_(_action: TPromiseExecFunction;
	_resolveClass: TPResolveClass; _rejectClass: TPRejectClass): TPromise;
var
    _execFuncCaller: TPromiseFuncCaller;
begin

    if not assigneD(_action) then exit; // Don't add anything if there is no function

    _execFuncCaller := TPromiseFuncCaller.Create;
    with myExecFuncList do begin
        _execFuncCaller.execFunc:=_action;

        if assigned(_resolveClass) then
            _execFuncCaller.resolveClass := _resolveClass
        else
            _execFuncCaller.resolveClass :=myResolveClass;

        if assigned(_rejectClass) then
            _execFuncCaller.rejectClass := _rejectClass
        else
            _execFuncCaller.rejectClass := myRejectClass;

        Add(_execFuncCaller);
	end;

end;




function TPromise.catch_(_errClass: TPromiseErrorClass): TPromise;
begin
    if assigned(_errClass) then
        myErrorClass := _errClass;
end;


function TPromise.finally_(_finalMeth: TNotifyEvent): TPromise;
begin
    myFinalMeth := _finalMeth;
end;

{ TPromiseCallBack }

constructor TPromiseCallBack.Create;
begin
    inherited;
end;

constructor TPromiseCallBack.Create(constref _owner: TPromise);
begin
    Create;
    Owner := _owner;
end;

{ TPResolve }

procedure TPResolve.execute;
begin

end;

{ TPReject }

procedure TPReject.execute;
begin

end;

{ TPromiseError }

procedure TPromiseError.execute;
begin

end;

procedure TPromiseError.setReject(AValue: TPReject);
begin
	if myReject=AValue then Exit;
	myReject:=AValue;
end;

end.

