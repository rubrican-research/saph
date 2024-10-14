unit saph.winman;
{ WINDOW MANAGER }


{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls ;
type
    TFrameClass = class of TFrame;

	{ TWinManager }
    // Removes a form from the formlist and form maps when it is destroyed by the user.
    TWinManager = class(TComponent)
        procedure Notification(AComponent: TComponent;Operation: TOperation); override;
	end;

function winManager: TWinManager;
procedure registerWind(_FC: TFormClass);
function isWindRegistered(_FC: TFormClass): boolean;
function isWindRegistered(_className: string): boolean;

function getForm(const _className: string; _name: string): TForm;

implementation
uses
    strutils, fgl, obj.listener, sugar.logger;
type
    TFormClassMap = class(specialize TFPGMap<string, TFormClass>);
    TFormMap  = class(specialize TFPGMap<string, TForm>);    // List by forms by Form.Name
    TFormList = class(specialize TFPGMapObject<string, TFormMap>); // map of form.name=>form by ClassName
var
    formClassMap    : TFormClassMap;
    formList        : TFormList; //
    myWinManager    : TWinManager;

function pointerAsHex(_obj: pointer): string;
begin
    Result := PtrUInt(_obj).ToHexString(16);
end;

function sanitize(const _str: string): string;
var
    i : integer;
begin
    Result := _str;
    if not (Result[1] in ['A'..'Z', 'a'..'z', '_']) then
        Result[1] := '_';
    for i := 2 to length(Result) do
        if not (Result[i] in ['0'..'9', 'a'..'z', 'A'..'Z', '_']) then
            Result[i] := '_';

end;
function hexStrAsPointer(_hex: string): Pointer;
begin
    Result := pointer(Hex2Dec64(_hex));
end;

function genFormName(constref f: TForm; _name: string = '') : string;
begin
    if _name = '' then
        _name := '_'
    else
        _name := sanitize(_name);
    Result := _name + pointerAsHex(f)
end;

function genFormCaption(constref f: TForm; _name: string = '') : string;
begin
    if _name = '' then
    begin
        _name := ReplaceStr(f.ClassName, 'T', '');
	end;
	Result := _name;
end;

function createForm(_className: string): TForm;
var
    _fc: TFormClass;
begin
    Result := nil;
    if formClassMap.TryGetData(_className, _fc) then begin
        Result := _fc.Create(Application);
        Result.Name := '';
        Result.Position := poScreenCenter;
        winManager.FreeNotification(Result)
	end;
end;

function newFormMap(): TFormMap;
begin
    Result := TFormMap.Create;
    Result.Sorted := true;
    Result.duplicates := dupIgnore;
end;

function winManager: TWinManager;
begin
    Result := myWinManager;
end;

procedure registerWind(_FC: TFormClass);
begin
     formClassMap.Add(_FC.ClassName, _FC);
end;

function isWindRegistered(_FC: TFormClass): boolean;
begin
    Result := isWindRegistered(_FC.ClassName);
end;

function isWindRegistered(_className: string): boolean;
begin
    Result:= formClassMap.IndexOf(_className) > -1;
end;

function getForm(const _className: string; _name: string): TForm;
var
	_formMap: TFormMap;

    procedure addNewFormToMap (constref _fm: TFormMap);
    begin
        Result := createForm(_className);
        Result.Name := genFormName(Result, _name);
        Result.Caption := genFormCaption(Result, _name) + ' ' + IntToStr(succ(_fm.Count));
        _fm.Add(Result.Name, Result);
    end;

begin
    Result := nil;
    if formList.TryGetData(_className, _formMap) then begin
        if not _formMap.TryGetData(_name, Result) then begin
            addNewFormToMap(_formMap);
        end
    end
    else begin
        _formMap := newFormMap();
        formList.Add(_className, _formMap);
        addNewFormToMap(_formMap);
	end;
end;

{ TWinManager }

procedure TWinManager.Notification(AComponent: TComponent; Operation: TOperation
	);
var
	_i: Integer;
	_form: TForm;
	_formMap: TFormMap;
begin
    if AComponent is TForm then begin
        case Operation of
        	opInsert: ;
            opRemove: begin
                log('TWinManager notified.%s is destroyed ', [AComponent.ClassName]);
                _form := TForm(AComponent);
                if formList.TryGetData(_form.ClassName, _formMap) then begin
                    _i :=_formMap.IndexOf(_form.Name);
                    if _i > -1 then
                        _formMap.Delete(_i);
				end;
			end;
		end;
	end;
	inherited Notification(AComponent, Operation);
end;



initialization
    myWinManager := TWinManager.Create(Application); // will be freed when application is freed

    formClassMap := TFormClassMap.Create;
    formClassMap.sorted := true;
    formClassMap.duplicates := dupIgnore;

    formList := TFormList.Create;
    formList.Sorted := true;
    formList.duplicates := dupIgnore;

finalization
    formClassMap.Free;
    formList.Free;

end.

