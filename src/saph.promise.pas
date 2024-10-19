unit saph.promise;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, fgl, fpjson;
type

    TPromise = class; // forward declaration
    TPromiseClass = class of TPromise;

	{ TPromiseCallBack }

    TPromiseCallBack = class
    private
        myOwner: TPromise;
        myExecCount: integer;
    public
        execOnce: boolean; // Will execute this only once in the lifecycle of this object
        json: TJSONObject;
        constructor Create;
        constructor Create(constref _owner: TPromise);
        destructor Destroy; override;
        procedure setData(_jsonData: TJSONObject);
        function shouldExecute: boolean;
        procedure execute; virtual;
    public
        property Owner: TPromise read myOwner;


	end;

	{ TPResolve }

	{ TPromiseError }

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

    TPromiseState = (   psCreated,
                        psRunning,
                        psDone
                    );

    NPromiseResult = (promiseUnknown,
                      promiseInit,                // Init value
                      promiseRunning,             // The action is still running
                      promiseResolved,            // The action is resolved
                      promiseRejected,            // The action is rejected
                      promiseException,           // The action resulted in an error
                      promiseTimeOut,
                      promiseKilled
                    );

	{ TPromiseResult }
    TPromiseExecFuncResult = class
        theResult : NPromiseResult;
        json      : TJSONObject;
        constructor Create;
        destructor Destroy; override;
	end;

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
		function getStatus: NPromiseResult;
		procedure setStatus(AValue: NPromiseResult);
        function isKilled: boolean;

    protected
        myOwnder : TPromise;
        myRunning: boolean;
        myResult : NPromiseResult;

    public
        constructor Create(constref _owner: TPromise);
        destructor Destroy; override;
    public
        procedure run(constref _resolve: TPResolve; const _reject: TPReject; const ownObjects: boolean = true); overload; virtual; abstract;
        procedure run(const _reject: TPReject; const ownObjects: boolean = true); overload; virtual; abstract;
        function wait(_timeOut: word = 2000): NPromiseResult;
        function waitForever: NPromiseResult;
        function isRunning: boolean; virtual;

        procedure kill;
    published
        property status: NPromiseResult read getStatus write setStatus;
	end;

    RPromise = record
        resolve: TPResolve;
        reject : TPReject;
        catch  : TPromiseError;
	end;

    TPromiseExecFunction    = function (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false): TPromiseExecFuncResult of object;
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
        myStatus        : TPromiseState; // Indicates that the promise has started running
        myOwnObjects    : boolean;
        myExecFuncList  : TExecFuncList;

        myResolveClass: TPResolveClass;
        myRejectClass : TPRejectClass;
        myErrorClass  : TPromiseErrorClass;
        myExecFunc    : TPromiseExecFunction;
        myFinalMeth   : TNotifyEvent;

        myPromiseResult : NPromiseResult;

    protected
        procedure doInit;

    public
        {
            This is a first draft. A copy of the static functions of the Javascript Promise class
            Subject to change
        }
        class function all(constref _promises: TPromiseList)    : TPromise;overload; unimplemented;
        class function all(const _promises: TPromiseArray)      : TPromise;overload; unimplemented;
        class function all(const _promises: TPromiseClassArray) : TPromise;overload;

        class function allSettled(constref _promises: TPromiseList)   : TPromise;overload; unimplemented;
        class function allSettled(const _promises: TPromiseArray)     : TPromise;overload; unimplemented;
        class function allSettled(const _promises: TPromiseClassArray): TPromise;overload; unimplemented;

        class function any(constref _promises: TPromiseList)    : TPromise;overload; unimplemented;
        class function any(const _promises: TPromiseArray)      : TPromise;overload; unimplemented;
        class function any(const _promises: TPromiseClassArray) : TPromise;overload; unimplemented;

        class function race(constref _promises: TPromiseList)   : TPromise;overload; unimplemented;
        class function race(const _promises: TPromiseArray)     : TPromise;overload; unimplemented;
        class function race(const _promises: TPromiseClassArray): TPromise;overload; unimplemented;

        class function withResolvers(): TPromise; unimplemented;
        class function reject() : TPReject; unimplemented;
        class function resolve(): TPResolve; unimplemented;

    public
        constructor Create(_execFunc: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil); overload;
        destructor Destroy; override;

    public { To use inside a ExecFunction}
        function init  (_initFunc: TNotifyEvent): TPromise;
        function then_ (_action: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil): TPromise; overload;
        function catch_(_errClass: TPromiseErrorClass): TPromise; overload;
        function finally_(_finalMeth : TNotifyEvent): TPromise;

        procedure run;
        procedure runAsync; unimplemented; // to be implemented in Application.QueueAsyncCall();
        procedure runThread;unimplemented; // to run inside a thread/threads?

        function promiseResult: NPromiseResult;

    public {Event handlers}
        OnInit       : TNotifyEvent;
        OnResolved   : TPromiseResolvedMeth;
        OnRejected   : TPromiseRejectedMeth;
        OnException  : TPromiseCatchMeth;
        OnTimeOut    : TPromiseCatchMeth;
        OnKilled     : TPromiseCatchMeth;
        OnFinally    : TNotifyEvent;            // Will called only if finally_() was not previously set.

    protected
        procedure doOnInit;
        procedure doOnResolved(constref _resolve: TPResolve);
        procedure doOnRejected(constref _reject: TPReject);
        procedure doOnException(constref _catch:  TPromiseError);
        procedure doOnTimeOut(constref _catch:  TPromiseError);
        procedure doOnKilled(constref _catch:  TPromiseError);
        procedure doOnFinally;

    end;

    {EXCEPTIONS}
    EPromiseInProgress = class(Exception); // Raised when you try to call run on a promise that is already running
    EPromiseCallBack   = class(Exception); // Exception raised when a promise callback fails.

    {factory function}
    function Promise(_action: TPromiseExecFunction; _resolveClass : TPResolveClass = nil; _rejectClass  : TPRejectClass = nil): TPromise;

    {Result helpers}
    function PromiseResult(_result: NPromiseResult = promiseUnknown; _json: TJSONObject = nil): TPromiseExecFuncResult;


implementation

function Promise(_action: TPromiseExecFunction; _resolveClass: TPResolveClass;
	_rejectClass: TPRejectClass): TPromise;
begin
    Result := TPromise.Create(_action, _resolveClass, _rejectClass);
end;

function PromiseResult(_result: NPromiseResult; _json: TJSONObject
	): TPromiseExecFuncResult;
begin
    Result := TPromiseExecFuncResult.Create;
    Result.theResult := _result;
    Result.json := _json;
end;



{ TPromiseAction }

procedure TPromiseAction.destroyEvents;
begin
    //RTLEventDestroy(myEventPromiseDone);
    //RTLEventDestroy(myEventResolve);
    //RTLEventDestroy(myEventReject);
    //RTLEventDestroy(myEventException);
end;

function TPromiseAction.getStatus: NPromiseResult;
begin
    Result := myResult;

end;

procedure TPromiseAction.setStatus(AValue: NPromiseResult);
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

function TPromiseAction.wait(_timeOut: word): NPromiseResult;
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

function TPromiseAction.waitForever: NPromiseResult;
begin
    while (myResult = promiseRunning) and not isKilled do sleep(__waitLoopSleep);
end;

function TPromiseAction.isRunning: boolean;
begin
    Result := (myResult = promiseRunning);
end;

procedure TPromise.doInit;
begin
    myExecFuncList  := TExecFuncList.Create(true);
    myStatus        := psCreated;
    myPromiseResult := promiseInit;
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
    doInit;

    myExecFunc := _execFunc;
    if assigned(_resolveClass) then
        myResolveClass  := _resolveClass
    else
        myResolveClass := TPResolve;

    if assigned(_rejectClass) then
        myRejectClass   := _rejectClass
    else
        myRejectClass := TPReject;

    myErrorClass    := TPromiseError;
    myPromiseResult := promiseInit;

    then_(myExecFunc, myResolveClass, myRejectClass); // Put this as the first item in the list

end;



procedure TPromise.run;
var
	i, c: Integer;
	_execFuncCaller: TPromiseFuncCaller;
    _resolve: TPResolve;
    _reject: TPReject;
    _catch , e: TPromiseError;
    _resolveCount: integer = 0;
	_funcResult: TPromiseExecFuncResult = nil;

    function createErrorObject(constref _reject: TPReject): TPromiseError;
    begin
        if assigned(_reject.errClass) then
            Result := _reject.errClass.Create(Self)
        else
            Result := myErrorClass.Create(Self);
        Result.reject := _reject;
        Result.reason := _reject.reason;
	end;

begin

    if myStatus = psRunning then raise EPromiseInProgress.Create('This promise is already running');

    try
        doOnInit;
        myStatus := psRunning;
        c := myExecFuncList.Count;
	   	for i := 0 to pred(myExecFuncList.Count) do begin
	   	    try
	            _execFuncCaller := myExecFuncList.Items[i];
                {RESOLVE INSTANCE}
	            if assigned(_execFuncCaller.resolveClass) then
		            _resolve := _execFuncCaller.resolveClass.Create(Self)
				else
	                _resolve := TPResolve.Create(Self);

                if assigned(_funcResult) then begin
                    if assigned(_funcResult.json) then begin
                        _resolve.json :=
                            _funcResult.json.Clone as TJSONObject;
                        freeAndNil(_funcResult);
                    end;
                end;

                {REJECT INSTANCE}
	            if assigned(_execFuncCaller.rejectClass) then
		            _reject  := _execFuncCaller.rejectClass.Create(Self)
	            else
                    _reject := TPReject.Create(Self);

                {EXECUTE}
	            try
	    		    {Call the default action}
                    try
                        _funcResult := _execFuncCaller.execFunc(_resolve, _reject);
                        if not assigned(_funcResult) then
                            raise Exception.Create('Promise execFunction returned nil');
					except
                         _funcResult := TPromiseExecFuncResult.Create;
                         _funcResult.theResult := promiseException;
					end;
                    {If you have an access violation here, it means that the execFunc()
                     did not return a TPromiseExecFuncResult object. }
					case _funcResult.theResult of
	                    promiseRunning:  begin // The action is still running
	                        myPromiseResult := promiseException;
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
	                        myPromiseResult := promiseRejected;
                            _reject.execute;
	                        e := createErrorObject(_reject);
	                        raise e;
	    				end;

	                    promiseException: begin
	                        myPromiseResult := promiseException;
	                        e := createErrorObject(_reject);
                            doOnException(e);
	                        raise e;
	    				end;

	                    promiseTimeOut: begin
	                        myPromiseResult := promiseTimeOut;
	                        e := createErrorObject(_reject);
	                        doOnTimeOut(e);
	                        raise e;
	    				end;

	                    promiseKilled: begin
	                        myPromiseResult := promiseKilled;
	                        e := createErrorObject(_reject);
	                        doOnKilled(e);
	                        raise e;
	    				end;

	                    else     begin
	                        myPromiseResult := promiseUnknown;
	                        _catch := myErrorClass.Create(Self);
	                        _catch.reject := _reject;
	                        _catch.reason := ClassName + ':: ExecFunc responded with an unexpected result.';
	                        raise _catch;
	                    end;

	                end;

	    		except
	                on _e: TPromiseError do begin
	                    {Call the catch function}
	                    _e.execute;
	                    break;
					end;
				end;
			finally
                _resolve.Free;
                _reject.Free;
			end;
		end;// For loop
	finally
        myStatus := psDone;

        {If you have an access violation here, it means that the execFunc()
        did not return a TPromiseExecFuncResult object. }
        freeAndNil(_funcResult);

        if _resolveCount = myExecFuncList.Count then
             myPromiseResult := promiseResolved;

        {Call the finally}
        try
            doOnFinally;
		except
            ;
		end;

        Free; // The promise;
	end;

end;

procedure TPromise.runAsync;
begin

end;

procedure TPromise.runThread;
begin

end;

function TPromise.promiseResult: NPromiseResult;
begin
    Result := myPromiseResult;
end;

procedure TPromise.doOnInit;
begin
    if assigned(OnInit) then OnInit(Self);
end;

procedure TPromise.doOnResolved(constref _resolve: TPResolve);
begin
    if assigned(OnResolved) then OnResolved(_resolve);
end;

procedure TPromise.doOnRejected(constref _reject: TPReject);
begin
    if assigned(OnRejected) then OnRejected(_reject);
end;

procedure TPromise.doOnException(constref _catch: TPromiseError);
begin
    if assigned(OnException) then OnException(_catch);
end;

procedure TPromise.doOnTimeOut(constref _catch: TPromiseError);
begin
    if assigned(OnTimeOut) then OnTimeOut(_catch);
end;

procedure TPromise.doOnKilled(constref _catch: TPromiseError);
begin
    if assigned(OnKilled) then OnKilled(_catch);
end;

procedure TPromise.doOnFinally;
begin
    if assigned(myFinalMeth) then
        myFinalMeth(Self)
    else if assigned(OnFinally) then
        OnFinally(Self)
end;

class function TPromise.withResolvers(): TPromise;
begin

end;


destructor TPromise.Destroy;
begin
    myExecFuncList.Free;
    inherited;
end;

function TPromise.init(_initFunc: TNotifyEvent): TPromise;
begin
    OnInit := _initFunc;
    Result := self;
end;

function TPromise.then_(_action: TPromiseExecFunction;
	_resolveClass: TPResolveClass; _rejectClass: TPRejectClass): TPromise;
var
    _execFuncCaller: TPromiseFuncCaller;
begin
    Result := Self;

    if myStatus > psCreated then begin
        raise EPromiseInProgress.Create('TPromise:: then_() not allowed when promise.run() has been called');
    end;

    if not assigned(_action) then exit; // Don't add anything if there is no function

    _execFuncCaller := TPromiseFuncCaller.Create;

    _execFuncCaller.execFunc:=_action;

    if assigned(_resolveClass) then
        _execFuncCaller.resolveClass := _resolveClass
    else
        _execFuncCaller.resolveClass :=myResolveClass;

    if assigned(_rejectClass) then
        _execFuncCaller.rejectClass := _rejectClass
    else
        _execFuncCaller.rejectClass := myRejectClass;

    myExecFuncList.Add(_execFuncCaller);

end;




function TPromise.catch_(_errClass: TPromiseErrorClass): TPromise;
begin
    if myStatus > psCreated then begin
        raise EPromiseInProgress.Create('TPromise:: catch_() not allowed when promise.run() has been called');
    end;

    if assigned(_errClass) then
        myErrorClass := _errClass;
end;


function TPromise.finally_(_finalMeth: TNotifyEvent): TPromise;
begin
    if myStatus > psCreated then begin
        raise EPromiseInProgress.Create('TPromise:: finally_() not allowed when promise.run() has been called');
    end;

    myFinalMeth := _finalMeth;
end;

{ TPromiseCallBack }


constructor TPromiseCallBack.Create;
begin
    inherited;
    json:= nil;
    myExecCount := 0;
    execOnce := true;
end;

constructor TPromiseCallBack.Create(constref _owner: TPromise);
begin
    Create;
    myOwner := _owner;
end;

destructor TPromiseCallBack.Destroy;
begin
    json.Free;
	inherited Destroy;
end;

procedure TPromiseCallBack.setData(_jsonData: TJSONObject);
begin
    if assigned(json) then json.Free;
    json := _jsonData;
end;

function TPromiseCallBack.shouldExecute: boolean;
begin
    Result := execOnce and (myExecCount > 1);
end;

procedure TPromiseCallBack.execute;
begin
    inc(myExecCount);
end;


{ TPResolve }

procedure TPResolve.execute;
begin
    inherited;
    if shouldExecute then
        if assigned(Owner) then
            if assigned(owner.OnResolved) then
                owner.OnResolved(self);

end;

{ TPReject }

procedure TPReject.execute;
begin
    inherited;
    if shouldExecute then
        if assigned(Owner) then
            if assigned(owner.OnRejected) then
                owner.OnRejected(self);
end;

{ TPromiseError }

procedure TPromiseError.execute;
begin
    inherited;
    if shouldExecute then
        if assigned(Owner) then
            if assigned(owner.OnException) then
                owner.OnException(self);
end;

procedure TPromiseError.setReject(AValue: TPReject);
begin
	if myReject=AValue then Exit;
	myReject:=AValue;
end;

{ TPromiseResult }

constructor TPromiseExecFuncResult.Create;
begin
    inherited Create;
    json := nil;
end;

destructor TPromiseExecFuncResult.Destroy;
begin
    json.Free;
	inherited Destroy;
end;

end.

