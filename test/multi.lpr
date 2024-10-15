program multi;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, main_multi, reactive, sugar.logger, threadtestform
    { you can add units after this };

{$R *.res}

begin
    startLog();
    log('-------------- START  -----------------------');
    log('');

    RequireDerivedFormResource:=True;
	Application.Scaled:=True;
    Application.Initialize;
	//Application.CreateForm(TForm1, Form1);
	Application.CreateForm(TForm2, Form2);

    Application.Run;

    log('');
    log('-------------- STOP  -----------------------');

end.

