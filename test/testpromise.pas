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
        function loadAppointments   (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseResult;
        function sortAppointments   (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseResult;
        function filterAppointments (constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseResult;
        function displayAppointments(constref _resolve: TPResolve; _reject: TPReject; const ownObjects: boolean = false):TPromiseResult;

        procedure loadingFailed(const _error: TPromiseError);
        procedure doneLoadingAppointments(Sender: TObject);
        procedure errorLoadingAppointments(constref E: TPromiseError);
    end;

var
    Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.Button1Click(Sender: TObject);
var
    _promise: TPromise;
begin
    {Tests all the different ways in which we can define a promise}
    _promise := Promise(@loadAppointments, TPResolve, TPReject);
    _promise.then_(@sortAppointments)
            .then_(@filterAppointments)
            .then_(@displayAppointments)

            .catch_(TPromiseError)
            .finally_(@doneLoadingAppointments);

    _promise.OnPromiseException:= @errorLoadingAppointments;
    _promise.run;
end;

Type
TResolve = class(TStringList);
TReject = class(TStringList);
TCatch = class(TStringList);

procedure TForm3.Button2Click(Sender: TObject);
var
	i: Integer;
begin
        for i := 0 to 30 do begin
            try
	            if i mod 3 = 0 then
                    raise TResolve.Create
	            else if i mod 5 = 0 then
                     raise TReject.Create
	            else if i mod 7 = 0 then
                    raise TCatch.Create;
            except
                on a: TResolve do begin
                    Memo1.Lines.Add('resolved ' + inttostr(i));
				end;

                on b: TReject do begin
                    Memo1.Lines.Add('rejected ' + inttostr(i));
				end;

                on c: TCatch do begin
                    Memo1.Lines.Add('error ' + inttostr(i));
				end;
			end;
        end
end;

function TForm3.loadAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseResult;
begin
    Memo1.lines.add('loadAppointments');
    _resolve.execute;
    Result := promiseResolved;
end;

function TForm3.sortAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseResult;
begin
    Memo1.lines.add('sortAppointments');
    _reject.reason := 'Not in a mood';
    _reject.execute;
    Result := promiseRejected;
end;

function TForm3.filterAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseResult;
begin
    Memo1.lines.add('filterAppointments');
end;

function TForm3.displayAppointments(constref _resolve: TPResolve;
	_reject: TPReject; const ownObjects: boolean): TPromiseResult;
begin
    Memo1.lines.add('displayAppointments');
end;

procedure TForm3.loadingFailed(const _error: TPromiseError);
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

end.

