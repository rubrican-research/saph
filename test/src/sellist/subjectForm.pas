unit subjectForm;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, saph.lists
    , subjectFrame;

type

    TSubjectList = specialize TSelectList<TFRSubject>;

	{ TFSubject }

    TFSubject = class(TForm)
		Button1: TButton;
		Panel1: TPanel;
		Panel2: TPanel;
		sbSubjects: TScrollBox;
		procedure Button1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
    private

    public
        function newSubject:TFRSubject;
    end;

    function newSubjectList: TSubjectList;
var
    FSubject: TFSubject;


implementation

{$R *.lfm}

var
    SubjectList: TSubjectList;

function newSubjectList: TSubjectList;
var
	i: Integer;
begin
    Result:= TSubjectList.Create(false);
    for i := 0 to pred(SubjectList.allCount) do
    begin
        Result.add(SubjectList.all[i]);
	end;
end;

{ TFSubject }

procedure TFSubject.Button1Click(Sender: TObject);
begin
    newSubject;
end;

procedure TFSubject.FormCreate(Sender: TObject);
begin
    newSubject.subName:='English';
    newSubject.subName:='Hindi';
    newSubject.subName:='Sanskrit';
    newSubject.subName:='Mathematics';
    newSubject.subName:='Science';
end;

function TFSubject.newSubject: TFRSubject;
begin
    Result := TFRSubject.Create(Self);

    with Result do
    begin
        Parent := sbSubjects;
    end;

    if Visible then Result.SetFocus;
    sbSubjects.ScrollInView(Result);
    SubjectList.add(Result);

end;


initialization
    SubjectList := TSubjectList.Create(False); // frames are owned by the form

finalization
    SubjectList.Free;
end.

