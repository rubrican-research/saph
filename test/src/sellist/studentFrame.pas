unit studentFrame;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, subjectForm;

type

	{ TFRStudent }

    TFRStudent = class(TFrame)
		edtName: TEdit;
		lblStudentNumber: TLabel;
		Label2: TLabel;
		rgGender: TRadioGroup;
		procedure FrameEnter(Sender: TObject);
		procedure FrameExit(Sender: TObject);
    private
        count: integer; static;
        i: integer;
        mySubjectList: TSubjectList;
		function getGender: string;
		function getSelected: boolean;
		function getStudName: string;
		function getStudNum: integer;
		procedure setGender(const _value: string);
        procedure setLabels;
		procedure setSelected(const _value: boolean);
		procedure setStudName(const _value: string);
		procedure setStudNum(const _value: integer);
    public
        constructor Create(TheOwner: TComponent); override;
        destructor Destroy; override;

    public
        property StudNum: integer read getStudNum write setStudNum;
        property StudName: string read getStudName write setStudName;
        property Gender: string read getGender write setGender;
        property subjectList: TSubjectList read mySubjectList;
        property selected: boolean read getSelected write setSelected;
    end;

implementation

{$R *.lfm}
uses
    graphics, Controls.Listener;

{ TFRStudent }

procedure TFRStudent.setLabels;
begin

end;

procedure TFRStudent.setSelected(const _value: boolean);
begin
    case _value of
    	True:  Color := clGradientActiveCaption;
        False: Color := clDefault;
    end;
end;

procedure TFRStudent.FrameEnter(Sender: TObject);
begin
    signal('selected');
end;

procedure TFRStudent.FrameExit(Sender: TObject);
begin
    signal('unselected');
end;

function TFRStudent.getGender: string;
begin
    Result:= rgGender.Items[rgGender.ItemIndex];
end;

function TFRStudent.getSelected: boolean;
begin
    Result:= (Color <> clDefault);
end;

function TFRStudent.getStudName: string;
begin
    Result:= edtName.Text;
end;

function TFRStudent.getStudNum: integer;
begin
    Result:= i;
end;

procedure TFRStudent.setGender(const _value: string);
begin
    with rgGender do begin
      ItemIndex := Items.IndexOf(_value);
	end;
end;

procedure TFRStudent.setStudName(const _value: string);
begin
    edtName.Text := _value;
end;

procedure TFRStudent.setStudNum(const _value: integer);
begin
    i := _value;
    lblStudentNumber.Caption := 'Student #' + intToStr(i);
end;

constructor TFRStudent.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    inc(Count);
    studNum := Count;
    Name:= '';
    Constraints.MinHeight:= Height;
    mySubjectList := newSubjectList();
end;

destructor TFRStudent.Destroy;
begin
    mySubjectList.Free;
	inherited Destroy;
end;

end.

