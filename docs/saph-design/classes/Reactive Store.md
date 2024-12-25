This library was built specifically to have native support of undo/redo at a "variable" level. It is primarily to improve developer experience. When you also use the [obj.listener](https://github.com/rubrican-research/saph/wiki/Object-Listeners) library, you can take advantage of change notifications.

# Design considerations
## Generic Class
Reactive store variables are instances of specialized the generic class "GReactive" which is a class of TReactive. This strategy makes it possible to create instances of any datatype, with minimal coding overhead to create a specialized class.

Creating a reactive integer class is as simple as 
` TRInt = class(specialize GReactive<integer>);`
## Factory methods
The unit offers factory methods to instantiate a reactive store object. 
```
function RInt: TRInt; overload;
function RInt(_default: integer; _name: string = ''): TRInt; overload;
```
This function instantiates a reactive integer object. 
### Automatic destruction
Using factory methods to instantiate reactive store objects will automatically destroy the object when the program terminates. The factory method adds the object to a master list , which is maintained by the unit `saph.reactive.pas`. In the finalization section of the unit, all objects created by the factory methods are freed.
```
var
	_i: TRInt;
begin
	_i := RInt(5, 'index'); // Creates a variable named 'index' initialized to 5
end;
```
### Manual destruction
To free an object created by a factory method, use `rFree();` In addition to destroying the object, it removes the reference to the object from the master list.
## Operator overloading
Basic operators such as 

## Undo/Redo
