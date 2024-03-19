unit mainSellist;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    Controls.Listener, fpjson, saph.lists, sugar.utils, studentFrame, studsubFrame;

type

	{ TMyItem }


    TFRStudSubList = specialize TSelectList<TFRStudSub>;

	{ TForm1 }

    TForm1 = class(TForm)
		Button1: TButton;
		Button2: TButton;
		Button3: TButton;
		Panel1: TPanel;
		Panel2: TPanel;
		Panel3: TPanel;
		sbStudents: TScrollBox;
		sbSub: TScrollBox;
		procedure Button1Click(Sender: TObject);
		procedure Button2Click(Sender: TObject);
		procedure Button3Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    private
        selStudentFrame: TFRStudent;
        myStudSubList: TFRStudSubList;
		function newFRStudent: TFRStudent;
  function newSubSelect: TFRStudSub;
        procedure selectStudent(const _sender: TControl; const _event: string; constref _params: TJSONObject);
        procedure clearStudent(_sender: TObject);

    public

    end;

var
    Form1: TForm1;

implementation

{$R *.lfm}

uses
    subjectForm;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    myStudSubList := TFRStudSubList.Create(True);
end;

function TForm1.newFRStudent: TFRStudent;
begin
    Result:= TFRStudent.Create(Self);
    with Result do begin
        Parent:= sbStudents;
        addListener('selected',     @selectStudent);
        addListener('unselected',   @clearStudent, qSerial);
	end;

end;
procedure TForm1.Button1Click(Sender: TObject);
begin
    newFRStudent.SetFocus;
//
//    if assigned(selStudentFrame) then
//        selStudentFrame.selected:= false;
//
//    selStudentFrame := ;
//    selStudentFrame.selected:=True;
//    sbStudents.ScrollInView(selStudentFrame);

end;
function  TForm1.newSubSelect: TFRStudSub;
begin
    Result:= TFRStudSub.Create(Self);
    with Result do begin
        if assigned(selStudentFrame) then
            subjectList := selStudentFrame.subjectList;
	end;
    sbSub.InsertControl(Result);
    myStudSubList.add(Result);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    // Select Subject
    if assigned(selStudentFrame) then
        sbSub.ScrollInView(newSubSelect);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    FSubject.ShowModal;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    myStudSubList.Free;
end;

procedure TForm1.selectStudent(const _sender: TControl; const _event: string;
	constref _params: TJSONObject);
var
	_subSel: TFRStudSub;
	_i: Integer;
	_c: TControl;
begin

    if (selStudentFrame <> TFRStudent(_sender)) then begin

        if assigned(selStudentFrame) then selStudentFrame.selected:=False;
        selStudentFrame := TFRStudent(_sender);
        selStudentFrame.selected:=True;

        sbStudents.ScrollInView(selStudentFrame);

        myStudSubList.Clear(true);
	    with selStudentFrame do begin
	        if assigned(subjectList) then begin
	            for _i := 0 to pred(subjectList.selCount) do begin
	                _subSel :=  newSubSelect;
	                _subSel.subject:= subjectList.selItems[_i].subName;
			    end;
	        end;
		end;
        selStudentFrame.SetFocus;
	end;
end;

procedure TForm1.clearStudent(_sender: TObject);
begin

end;

end.

