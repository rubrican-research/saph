# Saph: Lazarus package for complex GUI
This is a library to make coding complex GUIs in Lazarus easier. Broad functional goals:

### [Event listeners](https://github.com/rubrican-research/saph/wiki/Event-Listeners)
The ability to add multiple event listeners to any LCL control (objects will be supported in a future upgrade)and signal the event as needed.

### Manage multiple selections
The library makes it easy to implement the gui to support a use cases similar to selecting subjects for a student from a list of available courses. With each course that is selected, the next list of available selections should only show the remaining, unselected subjects. If you are implementing this with dynamically created panels that are displayed in a scroll box, it becomes tricky to filter out the previously selected items (in another panel object).
