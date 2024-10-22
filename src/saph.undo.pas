unit saph.undo;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;
const
    UNDOSIZE  = 32;

type

    {GUndoHistory                                                                           }
    {DESIGN NOTE                                                                            }
    {=======================================================================================}


    generic GUndoHistory<T> = class
	public
	    const MAXUNDO   = UNDOSIZE-1 ;
	    const UPPER_LIM = 2 * UNDOSIZE;

    private type
        RHistoryItem = record
            timestamp: TDateTime;
            val: T
        end;

	private
        myHistory : array[0..MAXUNDO] of RHistoryItem; // list of history values
	    myHistHead: integer; // position to the latest value
	    myHistCurr: integer; // position of the current pointer that changed during undo/redo
	    function posToIndex (_step: integer): integer; // Maps the current position to the index array
        function upperPos: integer; // Max position value possible
        function lowerPos: integer; // lowest position value possible
        function nextHeadPos: integer; // moves the head forward and returns the current position

	public
	    function histCount: integer; // count of history;
	    function histVal(_pos: integer = 0): T; virtual;

	    function undo   (_count: integer = 1): integer; // returns the current position after undo
	    function redo   (_count: integer = 1): integer; // returns the current position after redo
        function restore: integer; // cancels the undo and redo operations, points the current value to the currHead position.

        function add(_val: T): integer; // returns the current position after addition

        function currVal    : T;
        function currPos    : integer;
        function currHead   : integer;

    public
        constructor Create;
    end;

implementation
uses
    Math;

{ GUndoHistory }

function GUndoHistory.posToIndex(_step: integer): integer;
begin
    Result := _step mod UNDOSIZE;
end;

function GUndoHistory.upperPos: integer;
begin
    Result := myHistHead;
end;

function GUndoHistory.lowerPos: integer;
var
    _t: integer;
begin
    _t := upperPos - MAXUNDO;
    Result := Max(0, _t);
end;

function GUndoHistory.nextHeadPos: integer;
begin
    {Our current position is not at the head position:
        this means that undo/redo has been performed
        and the current value is not the latest value.

        so, when we move our history forward, we must start a  new history
        branch from the current position }
    if myHistCurr < myHistHead then
        myHistHead := myHistCurr;

    {Move our history forward}
    inc(myHistHead);

    {If we are moving past 2 x UNDOSIZE, then
        set it back to the UNDOSIZE position.

    REASON:
        This is built on a static array. We simulate
        a undo/redo queue by moving the indexes.

        To handle the queue when old items must be dropped from
        from the array, we use the mod operator to cycle past
        the UNDOSIZE position. This way, the mod operator points to the
        correct position in the array (overwriting the first items)
        but our position values indicate an asscending order of numbers.

        SIZE = 5

        ARRAY: [0] [1] [2] [3] [4]
        POS  :                  ^
        POS  :                      ^  <-- past the size of the array this translates to

        ARRAY: [0] [1] [2] [3] [4] [0]
        POS  :                      ^
        POS  :  0   1   2   3   4   5  <-- the new value is written to [0].

        Position 5 points to [0], overriting the first value.
        When you undo, you get position (5-1) which is 4 and it points to item [4]

        With this scheme, we keep incrementing head until we reach 2 x SIZE, which also points to item[0] after mod operation.
        At ths point, we reset head to SIZE, we can keepy cycling through as shown previously.    }
    if myHistHead = UPPER_LIM then
        myHistHead := UNDOSIZE;

    {Synchronize head and current postion, because the history now starts from here}
    myHistCurr := myHistHead;
    Result := myHistCurr;
end;

function GUndoHistory.histCount: integer;
begin
    Result := Min(UNDOSIZE, succ(myHistHead));
end;

function GUndoHistory.histVal(_pos: integer): T;
begin
    Result := myHistory[posToIndex(_pos)].val;
end;

function GUndoHistory.undo(_count: integer): integer;
begin
    myHistCurr := max(lowerPos,myHistCurr - _count);
    Result := myHistCurr;
end;

function GUndoHistory.redo(_count: integer): integer;
begin
    myHistCurr := min(myHistHead, myHistCurr + _count);
    Result := myHistCurr;
end;

function GUndoHistory.restore: integer;
begin
    myHistCurr := myHistHead;
    Result := myHistHead;
end;

function GUndoHistory.add(_val: T): integer;
begin
    if currVal = _val then exit;

    Result := nextHeadPos;
    with myHistory[posToIndex(Result)] do begin
        timestamp:=Now();
        val := _val;
    end;
end;

function GUndoHistory.currVal: T;
begin
    if myHistCurr > -1 then
        Result := myHistory[posToIndex(myHistCurr)].val
    else
        Result := default(T);
end;

function GUndoHistory.currPos: integer;
begin
    Result:= myHistCurr;
end;

function GUndoHistory.currHead: integer;
begin
    Result:= myHistHead;
end;

constructor GUndoHistory.Create;
begin
    inherited Create;
    // To init so that nextHeadPos points correctly
    myHistHead:=-1;
    myHistCurr:=-1;
end;

end.

