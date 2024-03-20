program sellist2_12;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, mainSellist, studentFrame, subjectForm, subjectFrame, studsubFrame

    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.CreateForm(TFSubject, FSubject);
    Application.Run;
end.

