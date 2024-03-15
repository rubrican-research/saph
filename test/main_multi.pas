unit main_multi;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Controls.Listener, fpjson;

type

	{ TForm1 }

    TForm1 = class(TForm)
		Edit1: TEdit;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
		procedure Edit1Change(Sender: TObject);
  procedure FormCreate(Sender: TObject);
    private
        procedure L1Change(const _sender: TControl; const _event: string; constref _params: TJSONObject);
        procedure L2Change(const _sender: TControl; const _event: string; constref _params: TJSONObject);
        procedure L3Change(const _sender: TControl; const _event: string; constref _params: TJSONObject);
        procedure L4Change(const _sender: TControl; const _event: string; constref _params: TJSONObject);
    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    with Edit1 do begin
        addListener('change', @L1Change, qThreads);
        addListener('change', @L2Change, qThreads);
        addListener('change', @L3Change, qThreads);
        addListener('change', @L4Change, qThreads);
	end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
    edit1.signal('change', TJSONObject.Create(['text', edit1.text]));
end;

procedure _a(constref _l: TLabel; constref _p: TJSONObject);
begin
    if assigned(_p) then begin
        _l.Caption := _p.get('text', '');
    end;
end;

procedure TForm1.L1Change(const _sender: TControl; const _event: string;
	constref _params: TJSONObject);
begin
    _a(label1, _params);
end;

procedure TForm1.L2Change(const _sender: TControl; const _event: string;
	constref _params: TJSONObject);
begin
  _a(label2, _params);
end;

procedure TForm1.L3Change(const _sender: TControl; const _event: string;
	constref _params: TJSONObject);
begin
  _a(label3, _params);
end;

procedure TForm1.L4Change(const _sender: TControl; const _event: string;
	constref _params: TJSONObject);
begin
  _a(label4, _params);
end;

end.

