unit saph.viewmanager;

{$mode ObjFPC}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, generics.Collections;
type
    {To embed frames or panels or other targets into another container}
    // Classes that implement this interface must take care of all the housekeeping
    IEmbeddableView = interface
        ['{13D18245-EDD5-4CDC-B799-67C6AD26101E}']
        function viewKind: byte; // Return the ord value of the enum
        procedure embed(constref _container: TWinControl);
        function isEmbedded: boolean;

        function canRemove: boolean;
        procedure remove;

        function destruct: boolean; // To call the destructor and do other cleanup

	end;

    EEmbeddableException = Class(Exception); // Raise this exception when you need to inform the host about errors during embedding or removing


type
    TFrameclass = class of TFrame;
    TViewFactory = function : IEmbeddableView;

    NViewKind = (
        vkForm,
        vkFrame
    );

    TViewRefrence = record
        alias   : string;
        instance: IEmbeddableView;
        factory : TViewFactory;
        viewKind: NViewKind;
        case NViewKind of
            vkForm    : (formClass  : TFormClass);
            vkFrame   : (frameClass : TFrameClass);

	end;

    // All the configured views
    NView =(
        viewUndefined,
        viewDashboard,
        viewUsers,
        viewAnweisungen,
        viewMachines,
        viewAufgaben,
        viewBerichte,
        viewConfig,
        viewHelp,
        viewDataEx
    );

    TDictViewReference = class(specialize TDictionary<NView, TViewRefrence>);
    TDictAliasView = class(specialize TDictionary<string, NView>);
    TDictClassView = class(specialize TDictionary<TClass, NView>);

	{ TViewRegistry }

    TViewRegistry = class
    private
        myCriticalSection: TRTLCriticalSection;
        myViewByEnum : TDictViewReference;
        myViewByAlias: TDictAliasView;
        myViewByType : TDictClassView;
    public
        constructor Create;
        destructor Destroy; override;

        procedure registerView(const _nview: NView; constref _class: TFormClass;  constref _factory: TViewFactory; _alias: string=''); overload;
        procedure registerView(const _nview: NView; constref _class: TFrameClass; constref _factory: TViewFactory; _alias: string=''); overload;

        function getView(const _nview: NView;  _singleton: boolean = true): IEmbeddableView;  overload;
        function getView(const _alias: string; _singleton: boolean = true): IEmbeddableView; overload;

        function isRegistered(const _nview: NView): boolean; overload;
        function isRegistered(const _alias: string): boolean; overload;
        function isRegistered(constref _class: TFormClass): boolean; overload;
        function isRegistered(constref _class: TFrameClass): boolean; overload;

        function unRegister(const _nview: NView): boolean; overload;
        function unRegister(const _alias: string): boolean; overload;
        function unRegister(constref _class: TFormClass): boolean; overload;
        function unRegister(constref _class: TFrameClass): boolean; overload;

      end;


    procedure registerView(const _nview: NView; constref _view: TFrameClass; constref _factory: TViewFactory; _alias: string=''); overload;
    procedure registerView(const _nview: NView; constref _view: TFormClass;  constref _factory: TViewFactory; _alias: string=''); overload;

    function getView(const _nview: NView): IEmbeddableView; overload;
    function getView(const _alias: string): IEmbeddableView; overload;

    function isEmbeddableView(const _form: TForm): boolean; overload;
    function isEmbeddableView(const _frame: TFrame): boolean; overload;


implementation
uses
    TypInfo;

var
    viewRegistry : TViewRegistry;

procedure registerView(const _nview: NView; constref _view: TFrameClass;
	constref _factory: TViewFactory; _alias: string);
begin
    viewRegistry.registerView(_nView, _view, _factory, _alias);
end;

procedure registerView(const _nview: NView; constref _view: TFormClass;
	constref _factory: TViewFactory; _alias: string);
begin
    viewRegistry.registerView(_nView, _view, _factory, _alias);
end;

function getView(const _nview: NView): IEmbeddableView;
begin
    Result := viewRegistry.getView(_nview);
end;

function getView(const _alias: string): IEmbeddableView;
begin
    Result := viewRegistry.getView(_alias);
end;


function isEmbeddableView(const _form: TForm): boolean;
var
    v: IEmbeddableView;
begin
    Result := supports(_form, IEmbeddableView, v);
end;

function isEmbeddableView(const _frame: TFrame): boolean;
var
    v: IEmbeddableView;
begin
    Result := supports(_frame, IEmbeddableView, v);
end;

{ TViewRegistry }

constructor TViewRegistry.Create;
begin
    inherited;
    InitCriticalSection(myCriticalSection);
    myViewByEnum := TDictViewReference.Create;
    myViewByAlias:= TDictAliasView.Create;
    myViewByType := TDictClassView.Create;
end;

destructor TViewRegistry.Destroy;
begin
    myViewByEnum .Free;
    myViewByAlias.Free;
    myViewByType .Free;
    DoneCriticalSection(myCriticalSection);
	inherited Destroy;
end;

procedure TViewRegistry.registerView(const _nview: NView; constref
	_class: TFormClass; constref _factory: TViewFactory; _alias: string);
var
    _viewRef: TViewRefrence;
begin
    EnterCriticalSection(myCriticalSection);
    try
	    if not assigned(_factory) then
	        raise Exception.Create('registerView():: We need a valid view factory (TViewFactory) to register this view');

	    _viewRef.viewKind := vkForm;
	    _viewRef.formClass:= _class;
	    _viewRef.factory  := _factory;
	    if _alias = '' then _alias := _class.ClassName;
        _viewRef.alias:= _alias;

	    myViewByEnum.AddOrSetValue(_nview, _viewRef);
	    myViewByAlias.AddOrSetValue(_alias, _nView);
	    myViewByType.AddOrSetValue(_class, _nView);

	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

procedure TViewRegistry.registerView(const _nview: NView; constref
	_class: TFrameClass; constref _factory: TViewFactory; _alias: string);
var
    _viewRef: TViewRefrence;
begin
    EnterCriticalSection(myCriticalSection);
    try
        if not assigned(_factory) then
            raise Exception.Create('registerView():: We need a valid view factory (TViewFactory) to register this view');

	    _viewRef.factory   := _factory;
	    _viewRef.viewKind  := vkFrame;
	    _viewRef.frameClass:= _class;
	    if _alias = '' then _alias := _class.ClassName;

	    myViewByEnum.AddOrSetValue(_nview, _viewRef);
	    myViewByAlias.AddOrSetValue(_alias, _nView);
	    myViewByType.AddOrSetValue(_class, _nView);
	finally
        LeaveCriticalSection(myCriticalSection);
	end;

end;

function TViewRegistry.getView(const _nview: NView; _singleton: boolean
	): IEmbeddableView;
var
    _s: string = '';
	_viewRef: TViewRefrence;
begin
    EnterCriticalSection(myCriticalSection);
    try
    	if not myViewByEnum.ContainsKey(_nview) then begin
            _s := GetEnumName(TypeInfo(NView), ord(_nview));
            raise Exception.Create('TViewRegistry.getView:: View '+  _s  + ' not registered');
        end;
        case _singleton of
        	True    : begin
                Result := myViewByEnum.Items[_nView].instance;
                if not assigned(Result) then begin
                    Result := myViewByEnum.Items[_nView].factory();
                    _viewRef := myViewByEnum.Items[_nView];
                    _viewRef.instance := Result;
                end;
    		end;
            False   : begin
                Result := myViewByEnum.Items[_nView].factory();
    		end;
    	end;

	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.getView(const _alias: string; _singleton: boolean
	): IEmbeddableView;
begin
    EnterCriticalSection(myCriticalSection);
    try
    	if not myViewByAlias.ContainsKey(_alias) then begin
            raise Exception.Create('TViewRegistry.getView:: View '+  _alias  + ' not registered');
    	end;
        result := getView(myViewByAlias.Items[_alias], _singleton);
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.isRegistered(const _nview: NView): boolean;
begin
    EnterCriticalSection(myCriticalSection);
    try
        Result:= myViewByEnum.ContainsKey(_nview)
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.isRegistered(const _alias: string): boolean;
begin
    EnterCriticalSection(myCriticalSection);
    try
        Result := myViewByAlias.ContainsKey(_alias);
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.isRegistered(constref _class: TFormClass): boolean;
begin
    EnterCriticalSection(myCriticalSection);
    try
        Result := myViewByType.ContainsKey(_class);
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.isRegistered(constref _class: TFrameClass): boolean;
begin
    EnterCriticalSection(myCriticalSection);
    try
        Result := myViewByType.ContainsKey(_class);
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.unRegister(const _nview: NView): boolean;
var
	_ref: TViewRefrence;
begin
    Result := false;
    EnterCriticalSection(myCriticalSection);
    try
        if myViewByEnum.ContainsKey(_nview) then begin
            _ref := myViewByEnum.Items[_nView];
            myViewByAlias.Remove(_ref.alias);
            case _ref.viewKind of
            	vkForm:  myViewByType.Remove(_ref.formClass);
                vkFrame: myViewByType.Remove(_ref.frameClass);
            end;
            myViewByEnum.Remove(_nView);
		end;
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.unRegister(const _alias: string): boolean;
var
	_nview: NView;
	_ref: TViewRefrence;
begin
    Result := false;
    EnterCriticalSection(myCriticalSection);
    try
        if myViewByAlias.ContainsKey(_alias) then begin
            _nview := myViewByAlias.Items[_alias];
            _ref := myViewByEnum.Items[_nView];
            case _ref.viewKind of
                vkForm:  myViewByType.Remove(_ref.formClass);
                vkFrame: myViewByType.Remove(_ref.frameClass);
            end;
            myViewByEnum.Remove(_nView);
            myViewByAlias.Remove(_ref.alias);
            Result := true;
    	end;
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.unRegister(constref _class: TFormClass): boolean;
var
	_nview: NView;
	_ref: TViewRefrence;
begin
    Result := false;
    EnterCriticalSection(myCriticalSection);
    try
        if myViewByType.ContainsKey(_class) then begin
            _nview := myViewByType.Items[_class];
            _ref := myViewByEnum.Items[_nView];
            myViewByEnum.Remove(_nView);
            myViewByAlias.Remove(_ref.alias);
            myViewByType.Remove(_class);
            Result := true;
    	end;
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

function TViewRegistry.unRegister(constref _class: TFrameClass): boolean;
var
	_nview: NView;
	_ref: TViewRefrence;
begin
    Result := false;
    EnterCriticalSection(myCriticalSection);
    try
        if myViewByType.ContainsKey(_class) then begin
            _nview := myViewByType.Items[_class];
            _ref := myViewByEnum.Items[_nView];
            myViewByEnum.Remove(_nView);
            myViewByAlias.Remove(_ref.alias);
            myViewByType.Remove(_class);
            Result := true;
    	end;
	finally
        LeaveCriticalSection(myCriticalSection);
	end;
end;

initialization
    viewRegistry := TViewRegistry.Create;

finalization
    viewRegistry.Free;
end.

