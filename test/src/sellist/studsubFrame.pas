unit studsubFrame;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, StdCtrls, Menus, Controls.Listener, subjectForm,
	subjectFrame;

type

	{ TFRStudSub }

    TFRStudSub = class(TFrame)
		cmbSubjects: TComboBox;
		Label2: TLabel;
		lblStudSub: TLabel;
		MenuItem1: TMenuItem;
		PopupMenu1: TPopupMenu;
		procedure cmbSubjectsChange(Sender: TObject);
		procedure cmbSubjectsEnter(Sender: TObject);
		procedure MenuItem1Click(Sender: TObject);
    private
      count: integer; static;
      mySubjectList: TSubjectList;
      mySelSubject: TFRSubject;
      i: integer;

	  function getSubjectList: TSubjectList;
      function getStudSubNum: integer;
	  function getSubject: string;
	  procedure RefreshSubList;
	  procedure setSubjectList(const _value: TSubjectList);
	  procedure setStudSubNum(const _value: integer);
	  procedure setSubject(const _value: string);

    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;

      property studSubNum  : integer read getStudSubNum write setStudSubNum;
      property subjectList : TSubjectList read getSubjectList write setSubjectList;
      property subject     : string read getSubject write setSubject;
      property subjectObj  : TFRSubject read mySelSubject;

    end;

implementation

{$R *.lfm}

function TFRStudSub.getStudSubNum: integer;
begin
    Result:= i;
end;

procedure TFRStudSub.cmbSubjectsChange(Sender: TObject);
var
    _currSubject: TFRSubject;
    _i : integer;
begin
    _i := cmbSubjects.ItemIndex;
    _currSubject := subjectList.unselItems[_i];

    if assigned(mySelSubject) and (_currSubject <> mySelSubject)then
        subjectList.selected[mySelSubject] := false;

     //if assigned(_currSubject) then
        subjectList.Selected[_currSubject] := true;

    mySelSubject:= _currSubject;
    if _i < 0 then RefreshSubList;
end;

procedure TFRStudSub.cmbSubjectsEnter(Sender: TObject);
begin
    RefreshSubList;
end;

procedure TFRStudSub.MenuItem1Click(Sender: TObject);
begin
    signal('delete');
end;

function TFRStudSub.getSubjectList: TSubjectList;
begin
    Result := mySubjectList;
end;

function TFRStudSub.getSubject: string;
begin
    Result := cmbSubjects.Text;
end;

procedure TFRStudSub.RefreshSubList;
var
	a, _i: Integer;

begin
    with cmbSubjects.Items do begin
        Clear;
        for _i := 0 to pred(mySubjectList.unselCount) do begin
            Add(mySubjectList.unselItems[_i].subName);
		end;
	end;
end;

procedure TFRStudSub.setSubjectList(const _value: TSubjectList);
begin
    mySubjectList := _value;
    RefreshSubList;
end;

procedure TFRStudSub.setStudSubNum(const _value: integer);
begin
    i := _value;
    lblStudSub.Caption := 'Subject #' + intToStr(i);
end;

procedure TFRStudSub.setSubject(const _value: string);
begin
    cmbSubjects.Text := _value;
end;

constructor TFRStudSub.Create(TheOwner: TComponent);
begin
	inherited Create(TheOwner);
    inc(Count);
    studSubNum := Count;
    Name:= '';
    Constraints.MinHeight:= Height;
    TStringList(cmbSubjects.Items).OwnsObjects:=False;
end;

destructor TFRStudSub.Destroy;
begin
	inherited Destroy;
end;

end.

