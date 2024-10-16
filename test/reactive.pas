unit reactive;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fgl, saph.reactive, fpjson;

type
	{ TForm2 }

	{ TReactiveThreadTest }

    TForm2 = class(TForm)
		Button1: TButton;
		undo: TButton;
		Button2: TButton;
		Button3: TButton;
		Button4: TButton;
		Button5: TButton;
		Button6: TButton;
		Button7: TButton;
		Button8: TButton;
		Button9: TButton;
		Edit1: TEdit;
		Label1: TLabel;
		Memo1: TMemo;
		redo: TButton;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure Button4Click(Sender: TObject);
		procedure Button5Click(Sender: TObject);
		procedure Button6Click(Sender: TObject);
		procedure Button7Click(Sender: TObject);
		procedure Button8Click(Sender: TObject);
		procedure Button9Click(Sender: TObject);
		procedure Edit1EditingDone(Sender: TObject);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure redoClick(Sender: TObject);
		procedure undoClick(Sender: TObject);
    private
        procedure iARead (sender:TObject);
        procedure iAWrite(sender:TObject);

        procedure iBRead (sender:TObject);
        procedure iBWrite(sender:TObject);

        procedure iCRead (sender:TObject);
        procedure iCWrite(sender:TObject);

        procedure sARead (sender:TObject);
        procedure sAWrite(sender:TObject);

        procedure sBRead (sender:TObject);
        procedure sBWrite(sender:TObject);

        procedure sCRead (sender:TObject);
        procedure sCWrite(sender:TObject);
        procedure appListner(const _sender: TObject; const _event: string; constref _params: TJSONObject);

    public

    end;

var
    Form2: TForm2;
    iA, iB, iC: TRInt;
    sA, sB, sC: TRStr;

implementation

{$R *.lfm}
uses
    sugar.logger, obj.Listener, threadtestform;


{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
    iA := RInt();
    iA .listenRead(self, @iARead).listenWrite(self, @iAWrite);

    iB := RInt();
    iB.listenRead(self, @iBRead).listenWrite(self, @iBWrite);

    iC := RInt();
    iC.listenRead(self, @iCRead).listenWrite(self, @iCWrite);

    sA := RStr();
    sA.listenRead(self, @sARead).listenWrite(self, @sAWrite);

    sB := RStr();
    sB.silentLock:=True;
    sB.listenRead(self, @sBRead).listenWrite(self, @sBWrite);

    sC := RStr();
    sC.listenRead(self, @sCRead).listenWrite(self, @sCWrite);

    application.addListener('wow', self, @appListner);
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
    //iA.Free;
    //iB.Free;
    //iC.Free;
    //sA.Free;
    //sB.Free;
    //sc.Free;
end;

procedure TForm2.redoClick(Sender: TObject);
begin
    sC.Redo;
    Edit1.Text := sC.val;
end;

procedure TForm2.undoClick(Sender: TObject);
begin
    sC.undo();
    Edit1.Text := sC.Val;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
    iA.value(0);
    while iA.value < 32 do iA.Val := iA.Val + 1;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
    rFree(iC);
    try
        iC.val := 999;
    except
        Memo1.Lines.Add('iC is not alive');
	end;
    iB.val := 2993;
    sA.val := 'Oh thinking about the younger years';
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
    with RStr() do begin
        Name := 'My Name';
		Name := 'This should not work';
	end;
end;

procedure TForm2.Button4Click(Sender: TObject);
var
    _r : TReactive;
    _s : TRStr;
begin
    _s := RStr('This is cool');
    _r := _s;
    Memo1.Lines.Add('-------------');
    Memo1.Lines.Add(_r.Value);
    Memo1.Lines.Add('-------------');
    Memo1.Lines.Add(_s.Value);
end;

procedure TForm2.Button5Click(Sender: TObject);
var
    a, b, c, d: TRStr;
begin
    a := RStr() + 'This '; // + ' is' + ' a test';
    c := 'is it ' + a + ' somewhere is nowhere ' + 'and anywhere';
    d := a + c;

    Memo1.Lines.Add(a);
    Memo1.Lines.Add(c);
    Memo1.Lines.Add(d);
    if (a = 'This ') then
        Memo1.Lines.Add('comparison worked');
    Memo1.Lines.Add('There is something quite nice about c = "%s"', [string(c)]);

end;

procedure TForm2.Button6Click(Sender: TObject);
begin
    Application.signal('wow', TJSONObject.Create(['sender', 'me', 'data', 'something']));
end;

procedure TForm2.Button7Click(Sender: TObject);
begin
    with TTestForm.Create(Application) do begin
        Caption := 'Test 1';
        Show;
	end;

	with TTestForm.Create(Application) do begin
        Caption := 'Test 2';
        Show;
	end;

	with TTestForm.Create(Application) do begin
        Caption := 'Test 3';
        Show;
    end;
end;

procedure TForm2.Button8Click(Sender: TObject);
const
    MAX = 6;
type
    SS = array[0..MAX] of string;
    strptr = ^string;
var
    source : SS = ('1', '2', '3', '4', '5', '6', '7');
    dest: SS;
	a: String;
	i: Integer;

    _start, _curr, _end : strptr;

begin
    dest := source;

    _start := @source[0];
    _end   := @source[MAX];
    _curr  := _start;

    for i := 0 to 6 do begin
        source[i] := '--';

	end;

    for a in dest do begin
        Memo1.Lines.Add(a);
	end;

    for a in source do begin
        Memo1.Lines.Add(a);
	end;

    Memo1.Lines.Add('=================================');
    for i := 0 to MAX do begin
        _curr := @dest[i];
        Memo1.Lines.Add('>> ' + _curr^);
	end;

end;

procedure TForm2.Button9Click(Sender: TObject);
var
    _r: TRStr;
	x: TRInt;
begin
    _r := rClone(sB);
    Memo1.lines.Add('cloned');
    Memo1.lines.Add(_r);
    rFree(_r);
    x := -iA;
    Memo1.Lines.Add(Format('Pred of -5 is %d', [pred(-5)]));
end;

procedure TForm2.Edit1EditingDone(Sender: TObject);
begin
    sC.val := edit1.Text;
end;

procedure TForm2.iARead(sender: TObject);
begin

end;

procedure TForm2.iAWrite(sender: TObject);
begin
    with Sender as TRInt do begin
        Memo1.Lines.Add(IntToStr(value));
	end;
end;

procedure TForm2.iBRead(sender: TObject);
begin

end;

procedure TForm2.iBWrite(sender: TObject);
begin

end;

procedure TForm2.iCRead(sender: TObject);
begin

end;

procedure TForm2.iCWrite(sender: TObject);
begin

end;

procedure TForm2.sARead(sender: TObject);
begin

end;

procedure TForm2.sAWrite(sender: TObject);
begin

end;

procedure TForm2.sBRead(sender: TObject);
begin

end;

procedure TForm2.sBWrite(sender: TObject);
begin
    if sender is TRStr then
        memo1.lines.Add(TRStr(Sender).val);
end;

procedure TForm2.sCRead(sender: TObject);
begin

end;

procedure TForm2.sCWrite(sender: TObject);
begin
    Memo1.Lines.Add(TRStr(Sender).val);
end;

procedure TForm2.appListner(const _sender: TObject; const _event: string;
	constref _params: TJSONObject);
begin
    Memo1.Lines.Add(Format('event: %s; data: %s', [_event, _params.FormatJSON(AsCompactJSON)]));
end;


end.

