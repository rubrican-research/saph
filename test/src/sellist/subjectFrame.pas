unit subjectFrame;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, saph.lists;

type

	{ TFRSubject }

    TFRSubject = class(TFrame)
		edtName: TEdit;
		Label2: TLabel;
		lblDesc: TLabel;
		lblSubNum: TLabel;
		Memo1: TMemo;
		procedure FrameEnter(Sender: TObject);
		procedure FrameExit(Sender: TObject);
    private
        count: integer; static;
        i: integer;
		function getDescription: string;
		function getSelected: boolean;
		function getSubName: string;
        function getSubNum: integer;
		procedure setDescription(const _value: string);
		procedure setSelected(const _value: boolean);
		procedure setSubName(const _value: string);
		procedure setSubNum(const _value: integer);

    public
        constructor Create(TheOwner: TComponent); override;
    public
        property subNum:        integer read getSubNum      write setSubNum;
        property subName:       string  read getSubName     write setSubName;
        property Description:   string  read getDescription write setDescription;
        property selected:      boolean read getSelected    write setSelected;
    end;

implementation

{$R *.lfm}

uses
    Graphics;

{ TFRSubject }

function TFRSubject.getSubNum: integer;
begin
    Result:= i;
end;

procedure TFRSubject.FrameEnter(Sender: TObject);
begin
    Selected:= True;
end;

procedure TFRSubject.FrameExit(Sender: TObject);
begin
    Selected:= False;
end;

function TFRSubject.getDescription: string;
begin
    Result:= Memo1.Text;
end;

function TFRSubject.getSelected: boolean;
begin

end;

function TFRSubject.getSubName: string;
begin
    Result:= edtName.Text;
end;

procedure TFRSubject.setDescription(const _value: string);
begin
    Memo1.Text:= _value;
end;

procedure TFRSubject.setSelected(const _value: boolean);
begin
    case _value of
    	True:  Color := clGradientActiveCaption;
        False: Color := clDefault;
    end;
end;

procedure TFRSubject.setSubName(const _value: string);
begin
    edtName.Text:= _value;
end;

procedure TFRSubject.setSubNum(const _value: integer);
begin
    i := _value;
    lblSubNum.Caption := 'Subject #' + intToStr(i);
end;

constructor TFRSubject.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    inc(Count);
    subNum := Count;
    Name:= '';
    Constraints.MinHeight:= Height;
end;

end.

