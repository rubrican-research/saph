unit main_multi;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Obj.Listener, fgl, fpjson;

type

	{ TStore }

    TStore = class
        subscriber: TObject;
        param: TJSONObject;
        name: string;
        procedure do_;

        constructor Create(constref _sub: TObject; constref _param: TJSONObject);
        destructor Destroy; override;
    end;

    TStores = class(specialize TFPGObjectList<TStore>)

    end;

    { TForm1 }

    TForm1 = class(TForm)
        addL1: TButton;
		Button1: TButton;
		Button2: TButton;
		Label10: TLabel;
		Label6: TLabel;
		Label7: TLabel;
		Label8: TLabel;
		Label9: TLabel;
		Memo1: TMemo;
		rb1: TRadioButton;
		rb2: TRadioButton;
		rb3: TRadioButton;
        stopL1: TButton;
        delObj: TButton;
        addL2: TButton;
        stopL2: TButton;
        Edit1: TEdit;
        Edit2: TEdit;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
		Label5: TLabel;
        procedure addL1Click(Sender: TObject);
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
        procedure stopL1Click(Sender: TObject);
        procedure delObjClick(Sender: TObject);
        procedure addL2Click(Sender: TObject);
        procedure stopL2Click(Sender: TObject);
        procedure Edit1Change(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    private
        procedure L1Change(const _sender: TObject; const _event: string;
            constref _params: TJSONObject);
        procedure L2Change(const _sender: TObject; const _event: string;
            constref _params: TJSONObject);
        procedure L3Change(const _sender: TObject; const _event: string;
            constref _params: TJSONObject);
        procedure L4Change(const _sender: TObject; const _event: string;
            constref _params: TJSONObject);
        procedure MChange(const _sender: TObject; const _event: string;
            constref _params: TJSONObject);

        procedure X1Change(const _sender: TObject; const _event: string; constref _params: TJSONObject);
        procedure doneSpike(const _sender: TObject; const _event: string; constref _params: TJSONObject);

    public
        myStores : TStores;
    end;

    { TTest }

    TTest = class
        val: string;
        procedure X1Change(const _sender: TObject; const _event: string;  constref _params: TJSONObject);
        destructor Destroy; override;
    end;

    function X1: TTest;

var
    Form1: TForm1;
    myX1: TTest;

implementation

{$R *.lfm}

uses
    sugar.logger, sugar.utils;

function X1: TTest;
begin
    if not assigned(myX1) then
        myX1 := TTest.Create;

    Result := myX1;
end;

{ TForm1 }


procedure TForm1.Edit1Change(Sender: TObject);
begin

    Sender.signal('change', TJSONObject.Create(['text', TEdit(Sender).Text]));
    Sender.signal('pearl',  TJSONObject.Create(['text', TEdit(Sender).Text]));
    Sender.signal('dance',  TJSONObject.Create(['text', TEdit(Sender).Text]));

    //Sender.signal('change', TJSONObject.Create(['text', '#']));
    //Sender.signal('pearl',  TJSONObject.Create(['text', '$']));
    //Sender.signal('dance',  TJSONObject.Create(['text', '%']));

    //Sender.signal('change');
    //Sender.signal('pearl');
    //Sender.signal('dance');


end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    myStores.Free;
end;

procedure TForm1.stopL1Click(Sender: TObject);
begin
    Edit1.rmListeners();
end;

procedure TForm1.delObjClick(Sender: TObject);
begin
    FreeAndNil(myX1);
end;

procedure TForm1.addL2Click(Sender: TObject);
var
    _sigType: TInvokeType = qAsync;
begin
    if rb1.Checked then
        _sigType := qAsync
    else if rb2.checked then
        _sigType := qThreads
    else if rb3.checked then
        _sigType := qSerial;

    with Edit2 do
    begin
        //addListener('change', X1, @X1.X1Change, _sigType);

         addListener('change', X1, @X1.X1Change, _sigType);
         addListener('pearl',  X1, @X1.X1Change, _sigType);
         addListener('dance',  X1, @X1.X1Change, _sigType);
         addListener('done',   Self, @doneSpike, _sigType);

         addListener('change', Self, @L3Change, _sigType);
         addListener('pearl',  Self, @L4Change, _sigType);
         addListener('dance',  Self, @MChange, _sigType);

    end;
end;

procedure TForm1.stopL2Click(Sender: TObject);
begin
    Edit2.rmListeners('change');
end;

procedure TForm1.addL1Click(Sender: TObject);
begin
    with Edit1 do
    begin
        addListener('change', Self, [@L1Change, @L2Change], qAsync);
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
	i: Integer;
begin
    Memo1.Lines.Clear;
    log('======================================================');
    log('============ SPIKE TEST BEGIN ========================');
    log('======================================================');
    Edit2.Text := '';
    //for i := 0 to 1024 do begin
    for i := 0 to 512 do begin
        Edit2.Text :=Edit2.Text + intToStr(i);
        sleep(10);
        ThreadSwitch;
        //Application.ProcessMessages;
	end;
    log('============ SPIKE TEST END ========================');
    log('======================================================');
    //Edit2.signal('done');
    //Close;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    Memo1.Text:= X1.val;
end;

procedure _a(constref _l: TLabel; constref _p: TJSONObject);
begin
    if assigned(_p) then
    begin
        _l.Caption := _p.get('text', '');
    end;
end;

procedure TForm1.L1Change(const _sender: TObject; const _event: string;
    constref _params: TJSONObject);
begin
    _a(label1, _params);
end;

procedure TForm1.L2Change(const _sender: TObject; const _event: string;
    constref _params: TJSONObject);
begin
    _a(label2, _params);
end;

procedure TForm1.L3Change(const _sender: TObject; const _event: string;
    constref _params: TJSONObject);
begin
    _a(label3, _params);
end;

procedure TForm1.L4Change(const _sender: TObject; const _event: string;
    constref _params: TJSONObject);
begin
    _a(label4, _params);
end;

procedure TForm1.MChange(const _sender: TObject; const _event: string; constref
	_params: TJSONObject);
begin
    if assigned(_params) then
    begin
        Memo1.Append(_params.get('text', ''));
    end;

end;

procedure TForm1.X1Change(const _sender: TObject; const _event: string;
	constref _params: TJSONObject);
begin

end;

procedure TForm1.doneSpike(const _sender: TObject; const _event: string;
	constref _params: TJSONObject);
begin
    log(X1.val);
end;

{ TStore }

procedure TStore.do_;
begin
    if isObjectAlive(TObject(subscriber)) then
        log ('  TStore.do_ %s ## subscriber is %s @%s', [Name, TObject(subscriber).ClassName, ObjAddressAsHex(subscriber)])
    else
        log ('  TStore.do_ %s ## subscriber is dead @%s', [Name, ObjAddressAsHex(subscriber)]);

end;

constructor TStore.Create(constref _sub: TObject; constref _param: TJSONObject);
begin
    inherited Create;
    subscriber := _sub;
    param := _param;
    name  := IntToStr(GetTickCount64);
end;

destructor TStore.Destroy;
begin
    param.Free;
    inherited;
end;

{ TTest }

procedure TTest.X1Change(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
    //writeToLog(getLogFileName(), format('X1Change:: (%s) %s', [_event, 'nothing']));
    EnterCriticalSection(runnerCS);
    try
	    if assigned(_params) then begin
		    val := val + DateTimeToStr(Now()) + ':: ' + _event + ':: ' + _params.get('text', '') + sLineBreak;
            log('X1Change:: (%s) %s', [_event, _params.get('text', '')]);
		end
	    else
	    begin
	        //log('X1Change:: (%s) %s', [_event, 'no params']);
		end;

	except
        on E:Exception do
            log('TTest.X1Change exception: ' + E.Message);
	end;
	LeaveCriticalSection(runnerCS);
    //log('X1Change:: (%s) %s', [_event, 'received']);
	//sleep(45);
end;

destructor TTest.Destroy;
begin
    stopListening;
    inherited Destroy;
end;

finalization
    X1.Free;

end.
