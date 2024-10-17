# Saph: Lazarus package for complex GUI
This is a library to make coding complex GUIs in Lazarus easier. Broad functional goals:

### [Object Listeners](https://github.com/rubrican-research/saph/wiki/Object-Listeners)
The ability to add multiple event listeners to any LCL control (objects will be supported in a future upgrade) and signal the event as needed.
### [Reactive store](https://github.com/rubrican-research/saph/wiki/Reactive-variables-TRInt,-TRStr-etc)
Data fields (variables) that signal when value has changed. Includes undo/redo capabilty baked in. Major data types .
- integer
- int64
- dword
- qword
- float (double)
- TDateTime
- string
- boolean

### [Select List](https://github.com/rubrican-research/saph/wiki/Select-List)
The library makes it easy to implement the gui to support a use cases similar to selecting subjects for a student from a list of available courses. With each course that is selected, the next list of available selections should only show the remaining, unselected subjects. If you are implementing this with dynamically created panels that are displayed in a scroll box, it becomes tricky to filter out the previously selected items (in another panel object).

### [Undo History](https://github.com/rubrican-research/saph/wiki/Undo-History)
A library to add undo/redo functionality to a data type. This is implemented as a generic class.

### [Win Manager](https://github.com/rubrican-research/saph/wiki/Win-Manager)
Simplified API to dynamically instantiate a form by just giving its class name as a string literal. Easy to register a Form Class in the initialization section of each form unit.. Show a form from anywhere in the application without having to include it in the uses clause.

`getForm('TForm1').show`
