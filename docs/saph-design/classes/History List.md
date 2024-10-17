***unit saph.undo***

This class provides undo/redo features.

### Usage
```
TStringHistory = class(specialize GUndoHistory<string>);

var
s : TStringHistory;

begin
	s := TStringHistory.Create;
	s.add('One');
	s.add('Two');
	s.add('Three');
	writeln(s.value); // outputs "Three"
	
	s.undo
	writeln(s.value); // outputs "Two";
	
	s.undo;
	writeln(s.value); // outputs "One";
	
	s.redo
	writeln(s.value); // outputs "Two";
end;

```
### Implementation
History is implemented as a fixed array. The size of the array is also specified as a constant. 

A distinction is made between the term "position" and "index".
- **position** refers to the rank of the history item. This value ranges from 0 to 2 x UNDOSIZE (maximum size of the history array)
- **index** refers to the array index. This value must be between 0 and UNDOSIZE - 1 (because it is a zero-indexed array)

**Reasoning:** Round-robin list functionality is implemented by mapping position value to the array index. 
1. Let the maximum size of the history array be 7.
2. Adding 7 items to the history stores each item in the next position of the array, i.e. from 0 up to 6. 
3. When an 8th item is added, there is no array index 7, so it cycles over to position 0 to store that value. However because 0 is the start of the array, it becomes necessary to map the position, i.e. 7 to the array index, i.e. 0. This is done with the **mod** operator using:
	- `index := position mod UNDOSIZE`
	- `7 mod 7 = 0`
4. Now when we want to undo, i.e. get the previous value, which is at position 6, the mapping gives us the array index 6.

## generic GUndoHistory

#### `function histCount: integer; // count of history;`
Returns how many items are currently in history. Once it exceeds the library limit, this function will always return the limit value.
#### `function histVal( _pos: integer = 0): T; virtual;`
Returns the value stored at the position.
#### `function undo   (_count: integer = 1): integer;`
Sets the current position to currPos - count, retrieves the history value and stores it in currValue. Returns the current position after undo
#### `function redo   (_count: integer = 1): integer;`
 Sets the current position to currPos + count. If this value exceeds the current "head" position (which is the start of the history), currPos is reset to equal "head" positition. the function retrieves the history value and stores it in currValue and returns the current position after redo.
#### `function restore: integer;`
Cancels all undo/redo operations and points the current position to the head value (latest value in the history). This works only if no add() was called. 
#### `function add(_val: T): integer;`
Returns the current position after adding the value. The "head" position is advanced with each call to add. 
1. if at the time of add() the currPosition is less than head, which is the case when undo was called, the head is reset to equal currPosition and then advanced to add the new value. This effectively breaks the history and points sets the head position to the one immediately after the current value in currPos.
#### `function currVal    : T`;
Returns the value that is being pointed to by currPos, that may have changed by calling undo() or redo().
#### `function currPos    : integer;`
Returns the current position in history (not index)
#### `function currHead   : integer;`
Returns the current head position in history (not index)


