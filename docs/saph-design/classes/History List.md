This class provides undo/redo features.

### Usage
```
TStringHistory = class(specialize TUndoHistory<string>);

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


## generic TUndoHistory

### constructor **Create(size)**
Creates a histrory object with a maximum undo size

#### function **add(value)** -> position number
Adds a value to the list and returns the current position number. When the position is max size, then it removes position 0 from the list and adds the current value to the max position.
#### function **value**
Returns the current value.

####  function **values(index)** -> value
Returns the value stored at position index. 
Index from 0 to (max size -1)

#### function **undo(number)** -> position number
You can undo a number of steps. returns the current position number. After the position reaches 0, calling undo returns the 
#### function **redo(number)** -> position number
You can undo a number of steps. returns the position number of the undone 

#### function **current** -> position number
Returns the current position in the list.

#### function **setCurrent(position)** -> position number
Sets the current position. 
Checks for boundaries, number of items and only returns the valid position.
If position is out of bounds, then this function raises an exception without changing position.
**Boundary conditions**
```
// list has 0 items
writeln(s.setCurrPosition(10)); // outputs -1. There are no items

// list has 5 items, max number of items = 15
writeln(s.setCurrPosition(3)); // outputs 3. sets the position to 3.
writeln(s.setCurrPosition(0)); // outputs 0. sets the position to 0.
writeln(s.setCurrPosition(-1)); // raises exception. current position is 0.
writeln(s.setCurrPosition(10)); // raises exception. current position is 0.
```


