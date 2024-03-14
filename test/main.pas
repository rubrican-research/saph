unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
	Controls.Listener, fpjson;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Edit1: TEdit;
		Edit2: TEdit;
		Label1: TLabel;
		Memo1: TMemo;
		procedure Edit1Change(Sender: TObject);
		procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
			);
		procedure Edit1MouseEnter(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure Label1MouseEnter(Sender: TObject);
    private
        procedure log(_s: string);
    public
      procedure A (_sender: TControl; _event: string; _params: TJSONObject);
      procedure B (_sender: TControl; _event: string; _params: TJSONObject);
      procedure C (_sender: TControl; _event: string; _params: TJSONObject);
      procedure D (_sender: TControl; _event: string; _params: TJSONObject);
    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
    _s: TStringArray;
	i: Integer;
begin
  Edit1.addListener('change', @A);
  Edit1.addListener('keyup', @B);
  Edit1.addListener('enter', @C);

  Edit1.addListener('change', @A);
  Edit1.addListener('keyup', @B);
  Edit1.addListener('enter', @C);

  Edit1.addListener('change', @A);
  Edit1.addListener('keyup', @B);
  Edit1.addListener('enter', @C);


  Edit2.addListener('change', @A);
  Edit2.addListener('enter', @C);

  Label1.addListener('enter', @C);

  _s := Edit1.signals;
  Memo1.Lines.Append('EDIT 1 -->');
  for i := 0 to high(_s) do Memo1.Lines.Append('>>   ' + _s[i]);

  _s := Edit2.signals;
  Memo1.Lines.Append('EDIT 2 -->');
  for i := 0 to high(_s) do Memo1.Lines.Append('>>   ' + _s[i]);

   _s := Memo1.signals;
  Memo1.Lines.Append('MEMO 1 -->');
  for i := 0 to high(_s) do Memo1.Lines.Append('>>   ' + _s[i]);

  _s := Label1.signals;
 Memo1.Lines.Append('LABEL 1 -->');
 for i := 0 to high(_s) do Memo1.Lines.Append('>>   ' + _s[i]);

  Memo1.Lines.Append('<-- Done');
end;

procedure TForm1.Label1MouseEnter(Sender: TObject);
begin
    TControl(Sender).signal('enter')
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
    TControl(Sender).signal('change', nil, qThreads);
    ;
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TControl(Sender).signal('keyup', nil);
end;

procedure TForm1.Edit1MouseEnter(Sender: TObject);
begin
    TControl(Sender).signal('enter', nil);
end;

procedure TForm1.log(_s: string);
begin
    Memo1.Lines.Append(Format('%d:: %s',[GetTickCount64, _s]));
end;

procedure TForm1.A(_sender: TControl; _event: string; _params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['A', _sender.Name, _event]));
end;

procedure TForm1.B(_sender: TControl; _event: string; _params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['B', _sender.Name, _event]));
end;

procedure TForm1.C(_sender: TControl; _event: string; _params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['C', _sender.Name, _event]));
end;

procedure TForm1.D(_sender: TControl; _event: string; _params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['D', _sender.Name, _event]));
end;

end.

