unit main;

{$mode objfpc}{$H+}
{$modeswitch FUNCTIONREFERENCES}
{$modeswitch ANONYMOUSFUNCTIONS}

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
      procedure A (const _sender: TControl; const _event: string; constref _params: TJSONObject);
      procedure B (const _sender: TControl; const _event: string; constref _params: TJSONObject);
      procedure C (const _sender: TControl; const _event: string; constref _params: TJSONObject);
      procedure D (const _sender: TControl; const _event: string; constref _params: TJSONObject);
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
  Edit1.addListener('change', @A);
  Edit1.addListener('change', @A);

  Edit1.addListener('enter', @C);
  Edit1.addListener('enter', @C);
  Edit1.addListener('enter', @C);

  Edit2.addListener('change', @A,qThreads);
  Edit2.addListener('enter', @C, qSerial);
  Edit2.addListener('keyup', @B);

  Label1.addListener('enter', @C, qThreads);

end;

procedure TForm1.Label1MouseEnter(Sender: TObject);
begin
    TControl(Sender).signal('enter')
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
    TControl(Sender).signal('change');
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    TControl(Sender).signal(
            'keyup',
            TJSONObject.Create(['key', Key])
    );
end;

procedure TForm1.Edit1MouseEnter(Sender: TObject);
begin
    TControl(Sender).signal('enter');
end;

procedure TForm1.log(_s: string);
begin
    Memo1.Lines.Append(Format('%d:: %s',[GetTickCount64, _s]));
end;

procedure TForm1.A(const _sender: TControl; const _event: string; constref
	_params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['A', _sender.Name, _event]));
end;

procedure TForm1.B(const _sender: TControl; const _event: string; constref
	_params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['B', _sender.Name, _event]));
    if assigned(_params) then
        log(Format('    key = %d', [_params.get('key', -1)]));
end;

procedure TForm1.C(const _sender: TControl; const _event: string; constref
	_params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['C', _sender.Name, _event]));
end;

procedure TForm1.D(const _sender: TControl; const _event: string; constref
	_params: TJSONObject);
begin
    log(Format('%s:: %s -> %s', ['D', _sender.Name, _event]));
end;

end.

