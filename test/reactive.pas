unit reactive;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, fgl, saph.reactive;

type
	{ TForm2 }

    TForm2 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Button3: TButton;
		Button4: TButton;
		Button5: TButton;
		Memo1: TMemo;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
		procedure Button4Click(Sender: TObject);
		procedure Button5Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    private
        iA, iB, iC: TRInt;
        sA, sB, sC: TRStr;

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
    public

    end;

var
    Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
    iA := RInt().listenRead(self, @iARead).listenWrite(self, @iAWrite);
    iB := RInt().listenRead(self, @iBRead).listenWrite(self, @iBWrite);
    iC := RInt().listenRead(self, @iCRead).listenWrite(self, @iCWrite);
    sA := RStr().listenRead(self, @sARead).listenWrite(self, @sAWrite);
    sB := RStr().listenRead(self, @sBRead).listenWrite(self, @sBWrite);
    sc := RStr().listenRead(self, @sCRead).listenWrite(self, @sCWrite);
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

procedure TForm2.Button1Click(Sender: TObject);
begin
    iA.value(0);
    while iA.value < 32 do iA.Value(iA.Value + 1)
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

end;

procedure TForm2.sCRead(sender: TObject);
begin

end;

procedure TForm2.sCWrite(sender: TObject);
begin

end;

end.

