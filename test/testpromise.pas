unit testPromise;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    saph.promise;

type

	{ TForm3 }

    TForm3 = class(TForm)
		Button1: TButton;
		Button10: TButton;
		Button11: TButton;
		Button12: TButton;
		Button2: TButton;
		Button3: TButton;
		Button4: TButton;
		Button5: TButton;
		Button6: TButton;
		Button7: TButton;
		Button8: TButton;
		Button9: TButton;
		Memo1: TMemo;
		Panel1: TPanel;
		StaticText1: TStaticText;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
    private

    public
        function loadAppointments   (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseExecFuncResult;
        function sortAppointments   (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseExecFuncResult;
        function filterAppointments (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseExecFuncResult;
        function displayAppointments(constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseExecFuncResult;

        procedure OnResolved(constref _resolve: TPResolve);
        procedure OnRejected(constref _reject: TPReject);
        procedure loadingFailed(constref _error: TPromiseError);
        procedure doneLoadingAppointments(Sender: TObject);
        procedure errorLoadingAppointments(constref E: TPromiseError);
    end;

    TFetchAppointments = class(TPromise)

	end;

	{ TAppointmentsResolve }

    TAppointmentsResolve = class(TPResolve)
        procedure execute; override;
	end;

    TAppointmentsReject = class(TPReject)

	end;

    TAppointmentError = class(TPromiseError)

	end;


var
    Form3: TForm3;

implementation

{$R *.lfm}
uses
    fpjson;

{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
var
    _promise: TPromise;
begin
    {Tests all the different ways in which we can define a promise}
    _promise := Promise(@loadAppointments, TPResolve, TPReject);
    _promise.then_(@sortAppointments, TAppointmentsResolve)
            .then_(@filterAppointments)
            .then_(@displayAppointments)

            .catch_(TPromiseError)
            .finally_(@doneLoadingAppointments);

    _promise.OnResolved := @OnResolved;
    _promise.OnRejected := @OnRejected;
    _promise.OnException:= @loadingFailed;

    _promise.run;

end;

procedure TForm3.Button2Click(Sender: TObject);
var
    _promise: TPromise;
begin
    {Tests all the different ways in which we can define a promise}
    _promise := Promise(@loadAppointments, TAppointmentsResolve, TAppointmentsReject);
    _promise.run;
end;

function TForm3.loadAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseExecFuncResult;
begin
    Memo1.lines.add('loadAppointments');
    _resolve.execute;

    Result := PromiseResult(promiseResolved, TJSONObject.Create(['appointments', TJSONArray.Create(['Amber', 'Amba', 'Aadira'])]));
end;

function TForm3.sortAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseExecFuncResult;
begin
    Memo1.lines.add('sortAppointments');
    if assigned(_resolve.json) then
    Memo1.Lines.Add(_resolve.json.AsJSON);
    _resolve.Execute;

    Result := PromiseResult(promiseResolved, TJSONObject.Create(['sorted_appointments', TJSONArray.Create(['A', 'AB', 'AC'])]));

end;

function TForm3.filterAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseExecFuncResult;
begin
    Memo1.lines.add('filterAppointments');

    if assigned(_resolve.json) then
    Memo1.Lines.Add(_resolve.json.AsJSON);

    _reject.reason := 'Not a good reason';
    _reject.execute;

    Result := PromiseResult(promiseResolved, TJSONObject.Create(['testing', 'to see if the memory is freed properly']));
end;

function TForm3.displayAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseExecFuncResult;
begin
    Memo1.lines.add('displayAppointments');
    Result := PromiseResult(promiseResolved);
end;

procedure TForm3.OnResolved(constref _resolve: TPResolve);
begin
    Memo1.lines.add('OnPromiseResolved --');
end;

procedure TForm3.OnRejected(constref _reject: TPReject);
begin
    Memo1.lines.add('OnPromiseRejected --');
end;

procedure TForm3.loadingFailed(constref _error: TPromiseError);
begin
    Memo1.lines.add('loadFailed');
end;

procedure TForm3.doneLoadingAppointments(Sender: TObject);
begin
    Memo1.lines.add('doneLoadingAppointments');
    case TPromise(Sender).promiseResult of
    	promiseUnknown:     Memo1.lines.add('Result unknown');
        promiseInit:        Memo1.lines.add('Result Init');
        promiseRunning:     Memo1.lines.add('Result Running');
        promiseResolved:    Memo1.lines.add('Result Resolved');
        promiseRejected:    Memo1.lines.add('Result Rejected');
        promiseException:   Memo1.lines.add('Result Exception');
        promiseTimeOut:     Memo1.lines.add('Result TimeOut');
        promiseKilled:      Memo1.lines.add('Result Killed');
    end;
end;

procedure TForm3.errorLoadingAppointments(constref E: TPromiseError);
begin
    Memo1.lines.add('errorLoadingAppointments because ' + E.reason);
end;

{ TAppointmentsResolve }

procedure TAppointmentsResolve.execute;
begin
	inherited execute;
    if shouldExecute then
        form3.Memo1.lines.add('TAppointmentsResolve.execute;');
end;

end.

