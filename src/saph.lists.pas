unit saph.lists;
{(c) Rubrican Research.
https://github.com/rubrican-research/saph

A little library to manage selection lists. This is specifically to handle
ui cases where you want to select unassigned items from a list. This requires
the list to keep track of which items are previously selected and offer a dedicated
list of unassigned items.

Implementation notes:

TSelectList is a generic class. To use, specalize this with the object that you need store in the list.

Adding and removing objects are done with "add(_item)" and "remove(_item)";

PROPERTIES
- selected[_itemVariable] : boolean. to check if an object is selected or not.
                            you can also set the status of an object to selected = true with this property.

- allCount  gives the count of total items
- all[_index] retrieves the item from the full list

- selCount gives the count of selected items
- selItems[_index] retrieves the item from the list of selected items

- unselCount gives the count of unselected items
- unselItems [_index] retrieves the item from the list of unselected items


METHODS
// Returns a GItemList object that is a specialized TFPGObjectList object with the items in the same
// order as the main list. The list object does not own the items.
// You have to free the list object after you are done.

- listAll:       GItemList;
- selectedList:  GItemList;
- unselectedList:GItemList;

}

{$mode objfpc}{$H+}



interface

uses
    Classes, SysUtils, fgl, Contnrs;


type

	{ TSelectList }

    generic TSelectList<GItem: TObject> = class
    public type
        // To return lists from this class.
        //  To use, you have to declare variables as:
        //  <SpecializedClassName>.GItemList
        GItemList = specialize TFPGObjectList<GItem>;

    private
        // The hash values are the Hex representations of
        // the address of the item. This is guaranteed to be unique
        // Also, allows you to quickly find retrieve the obhect from this list with the object reference.
        masterList  : TFPHashObjectList; // Stores GItem with the address as the key.
        selectList  : TFPHashObjectList;
        unselList   : TFPHashObjectList;

        // comparitor function for TAvgLvTree objects;
        // TODO: to keep selectList and unselList sorted by the same order of
        // items in masterList.
        // function itemSort(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;

		function getAllCount: integer;
		function getKeyItemFromAll(_index: integer): string;
		function getKeyItemFromSelected(_index: integer): string;
		function getKeyItemFromUnselected(_index: integer): string;
		function getSelected(constref _item: GItem): boolean;
		function getItemFromAll(_index: integer): GItem;
		function getItemFromSelected(_index: integer): GItem;
		function getItemFromUnselected(_index: integer): GItem;
		function getSelCount: integer;
		function getUnselCount: integer;
		procedure setSelected(constref _item: GItem; const _value: boolean);

        // Generates a key value from the address of the item object
        // This is the hexadecimal represenation of the address of this object.
        function key(constref _item: GItem): string;

    public
        property selected[constref _item: GItem] : boolean read getSelected write setSelected;

        property all        [_index: integer]: GItem read getItemFromAll;           // retrieves the item from the full list
        property selItems   [_index: integer]: GItem read getItemFromSelected;      // retrieves the item from the list of selected items
        property unselItems [_index: integer] : GItem read getItemFromUnselected;   // retrieves the item from the list of unselected items.

        //For debug. I presume that this won't be needed in production
        //property keyall        [_index: integer]: string read getKeyItemFromAll;    //
        //property keyselItems   [_index: integer]: string read getKeyItemFromSelected;
        //property keyunselItems [_index: integer]: string read getKeyItemFromUnselected;


        property allCount: integer read getAllCount;        // Count of total items
        property selCount: integer read getSelCount;        // Count of selected items
        property unselCount: integer read getUnselCount;    // Count of unselected items

     public
        constructor Create(_OwnObjects: boolean = true);
        destructor Destroy; override;

        procedure add(_item: GItem);            // Add an Item to the list
        procedure remove(_item: GItem);         // Remove an item from the list. Requires the item object as a parameter

        // Returns a specialized TFPGObjectList object with the items in the same
        // order as the main list. The list object does not own the items.
        // You have to free the list object after you are done.
        function listAll:       GItemList;
        function listSelected:  GItemList;
        function listUnselected:GItemList;

        procedure resetSelection; // Unselects all selected items
        procedure clear(_freeObj: boolean = true);
	end;


implementation

{ TSelectList }
uses
    Math, Dialogs;

//function TSelectList.itemSort(Tree: TAvgLvlTree; Data1, Data2: Pointer
//	): integer;
//begin
//    Result := GItemContainer(Data1).index - GItemContainer(Data2).index;
//
//    if Result <0 then
//        Result:= -1
//    else if Result > 0 then
//        Result := 1
//    else
//        Result := 0;
//end;

function TSelectList.getAllCount: integer;
begin
    Result:= masterList.Count;
end;

function TSelectList.getKeyItemFromAll(_index: integer): string;
begin
    if InRange(_index, 0, pred(allCount)) then
        Result := masterList.NameOfIndex(_index)
    else
        Result := '-NF-';
end;

function TSelectList.getKeyItemFromSelected(_index: integer): string;
begin
    if InRange(_index, 0, pred(selCount)) then
        Result := selectList.NameOfIndex(_index)
    else
        Result := '-NF-';
end;

function TSelectList.getKeyItemFromUnselected(_index: integer): string;
begin
    if InRange(_index, 0, pred(unselCount)) then
        Result := unselList.NameOfIndex(_index)
    else
        Result := '-NF-';
end;

function TSelectList.getSelected(constref _item: GItem): boolean;
begin
    Result:= Assigned(selectList.Find(key(_item)));
end;

function TSelectList.getItemFromAll(_index: integer): GItem;
begin
    if InRange(_index, 0, pred(allCount)) then
        Result := GItem(masterList.Items[_index])
    else
        Result := TObject(nil);
end;

function TSelectList.getItemFromSelected(_index: integer): GItem;
begin
    if InRange(_index, 0, pred(selCount)) then
        Result := GItem(selectList.Items[_index])
    else
        Result := TObject(nil);
end;

function TSelectList.getItemFromUnselected(_index: integer): GItem;
begin
    if InRange(_index, 0, pred(unselCount)) then
        Result := GItem(unselList.Items[_index])
    else
        Result := TObject(nil);

end;

function TSelectList.getSelCount: integer;
begin
    Result:= selectList.Count;
end;

function TSelectList.getUnselCount: integer;
begin
    Result := unselList.Count;
end;

procedure TSelectList.setSelected(constref _item: GItem; const _value: boolean);
var
  _foundItem: GItem;
begin

    if not assigned(_item) then exit;

    _foundItem := GItem(masterList.Find(key(_item)));
    if not assigned(_foundItem) then exit; //add(_item);

    case _value of

        True: begin
            if not selected[_item] then
            begin
                _foundItem := GItem(unselList.Find(key(_item)));
                if assigned(_foundItem) then
                begin
                    selectList.Add(key(_item), _item);
                    unselList.Remove(_foundItem);
				end;
			end;
		end;

        False: begin
            if selected[_item] then
            begin
                selectList.Remove(_item);
                unselList.Add(key(_item), _item);
			end;
		end;

    end;
end;

function TSelectList.key(constref _item: GItem): string;
begin
    //Result:= IntToHex(QWord(@_item), 1);
    Result:= PtrUint(_item).ToHexString(16)
end;

constructor TSelectList.Create(_OwnObjects: boolean);
begin
    inherited Create;
    masterList := TFPHashObjectList.Create(_OwnObjects);

    selectList := TFPHashObjectList.Create(False);
    unselList  := TFPHashObjectList.Create(False);

    //selectList := TAvgLvlTree.CreateObjectCompare(@itemSort);
    //selectList.OwnsObjects :=False;
    //
    //unselList  := TAvgLvlTree.CreateObjectCompare(@itemSort);
    //unselList.OwnsObjects := False
end;

destructor TSelectList.Destroy;
begin
    unselList.Free;
    selectList.Free;
    masterList.Free;
    inherited;
end;

procedure TSelectList.add(_item: GItem);
var
	_key: String;
begin
    _key := key(_item);
    if not assigned(masterList.Find(_key)) then
    begin
        masterList.Add(_key, _item);
        unselList.add(_key, _item);
	end;
end;

procedure TSelectList.remove(_item: GItem);
var
	_key: String;
	_i: Integer;
begin
    _key := key(_item);
    if assigned(masterList.Find(_key)) then
    begin
        if selected[_item] then begin
            _i := selectList.FindIndexOf(_key);
            selectList.Delete(_i);
		end
		else begin
            _i := unselList.FindIndexOf(_key);
            unselList.Delete(_i);
		end;
		masterList.Remove(_item);
	end;
end;

function TSelectList.listAll: GItemList;
var
	_i: Integer;
begin
    Result:= GItemList.Create(false);
    for _i := 0 to pred(allCount) do begin
        Result.Add(all[_i]);
	end;
end;

function TSelectList.listSelected: GItemList;
var
	_item: GItem;
	_i: Integer;
begin
    Result:= GItemList.Create(false);
    for _i := 0 to pred(allCount) do
    begin
        _item := all[_i];
        if selected[_item] then Result.Add(_item);
	end;
end;

function TSelectList.listUnselected: GItemList;
var
	_item: GItem;
	_i: Integer;
begin
    Result:= GItemList.Create(false);
    for _i := 0 to pred(allCount) do
    begin
        _item := all[_i];
        if not selected[_item] then Result.Add(_item);
	end;
end;

procedure TSelectList.resetSelection;
var
	_i: Integer;
begin
    for _i := 0 to pred (selCount) do
        selected[selItems[_i]] := false;
end;

procedure TSelectList.clear(_freeObj: boolean);
var
    _prevOwnsObj : boolean;
begin
    selectList.Clear;
    unselList.Clear;
    _prevOwnsObj := masterList.OwnsObjects;
    if _freeObj then begin
        masterList.OwnsObjects := True;
    end;
    masterList.Clear;
    masterList.OwnsObjects := _prevOwnsObj;
end;

end.

