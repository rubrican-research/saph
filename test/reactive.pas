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
		Memo1: TMemo;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
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
    iA := RInt().reader(self, @iARead).writer(self, @iAWrite);
    iB := RInt().reader(self, @iBRead).writer(self, @iBWrite);
    iC := RInt().reader(self, @iCRead).writer(self, @iCWrite);
    sA := RStr().reader(self, @sARead).writer(self, @sAWrite);
    sB := RStr().reader(self, @sBRead).writer(self, @sBWrite);
    sc := RStr().reader(self, @sCRead).writer(self, @sCWrite);
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

