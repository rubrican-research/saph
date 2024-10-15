unit threadtestform;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

	{ TReactiveThreadTest }

    TReactiveThreadTest = class (TThread)
        formCaption: string;
        procedure Execute; override;
        constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt=
			DefaultStackSize);
        destructor Destroy; override;
        procedure SayBye(Sender: TObject);
	end;

	{ TTestForm }

    TTestForm = class(TForm)
		Terminate: TButton;
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure TerminateClick(Sender: TObject);
    private
        T: TReactiveThreadTest;
    public

    end;

var
    TestForm: TTestForm;

implementation

{$R *.lfm}
uses
    reactive, math;

var
    myCS: TRTLCriticalSection;
{ TReactiveThreadTest }

procedure TReactiveThreadTest.Execute;
var
    testForm: TTestForm;
    _key: string = '';
    _count: integer = 0;
    _stime: integer;
begin
    while not Terminated do begin
        _key := sB.lock();
        if sB.isMyLock() then begin
            sb.EnterCS;
            sB.val := Format('Form: %s; ThreadID: %d => %s; key: %s; count = %d', [formCaption, ThreadID, DateTimeToStr(Now()), _key, _count]);
            sB.unlock(_key);
            sb.LeaveCS;
		end;
        _sTime := Min(500, Random(1000));
		sleep(_sTime);
        yield;
	end;
    if sB.isMyLock then sB.Unlock(_key);
end;

constructor TReactiveThreadTest.Create(CreateSuspended: Boolean;
	const StackSize: SizeUInt);
begin
    inherited;
    FreeOnTerminate:=True;
    OnTerminate:= @SayBye;
end;

destructor TReactiveThreadTest.Destroy;
begin
	inherited Destroy;
end;

procedure TReactiveThreadTest.SayBye(Sender: TObject);
begin
    sB.val := Format('PID: %d; ThreadID: %d => Bye Bye', [GetProcessID, ThreadID, DateTimeToStr(Now())]);
end;

{ TTestForm }

procedure TTestForm.FormShow(Sender: TObject);
begin
    T:= TReactiveThreadTest.Create(false);
    T.formCaption:= Caption;
end;

procedure TTestForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    CloseAction := caFree;
end;

procedure TTestForm.TerminateClick(Sender: TObject);
begin
    T.Terminate;
    Close;
end;

initialization
    InitCriticalSection(myCS);
    Randomize;

finalization
    DoneCriticalSection(myCS);
end.

