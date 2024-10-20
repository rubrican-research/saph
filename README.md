![saph-logo](https://github.com/user-attachments/assets/53aa2749-83ee-4c2e-847f-222cffe61fe4)

**Note**: This library depends on "sugar" (https://github.com/stanley643212/sugar). You will need to clone this repository as well.
The plan is to remove the dependency before release.

# Saph: Lazarus package for complex GUI
This is a library to make coding complex GUIs in Lazarus easier. Broad functional goals:

### [Object listeners](https://github.com/rubrican-research/saph/wiki/Object-Listeners)
Implemented as a type helper on TObject. To include the signal/listen functionality, just add **obj.listener** to your uses clause. 
Add a listener to any object by simply calling 
```
objvar.addListener('event string case sensitive', @listenProc/listenMethod); // psuedo code :-)
```
Whenever you call 
```
objvar.signal('event string case sensitive'),
```
the object calls all the attached listener procs/methods. You can also send event parameters to the listening function (if it does not use TNotifyEvent) as a JSON Object like so:
```
objvar.signal('file downloaded', TJSONObject.Create(['filepath', '\this\is\the\file\path.txt', 'fingerprint', '773kkdh88344jd']));
```
**Sending signals**
You can control how the signals are sent when  you add a listener. You can mix up the modes as you add listeners. Each listener gets called by invocation type that was specified when the listener was defined. The invocation types are:
  - _qAsynch_:(default). signal() returns without waiting for the procs/methods to complete. This queues the listener proc/method in the Application object's asynch queue. This way is best for GUI applications where you want to signal user actions to multiple subscribers.
  - _qThreads_: signal() returns without waiting for the procs/methods to complete. This runs each of the attached listener procs/methods in a separate thread. The method runner uses a critical section **runnerCS** (global variable that is managed in the initialization and finalization sections of the unit) when running in _qThread_ mode. If needed, you can use this in your own methods that will be called by the signal.
  - _qSerial_: This runs each of the attached listener procs/methods in a non breaking loop. signal() returns after all the listener methods have been executed.

### [Control listeners (deprecated)](https://github.com/rubrican-research/saph/wiki/Event-Listeners)
Control Listeners was the first version of the signal/listen library. It is no longer used. All functionality has been implemented in the obj.listener library.

### [Selection Manager](https://github.com/rubrican-research/saph/wiki/Select-List)
This is a generic object list that allows you to implement complex select/unselect behaviours in Gui. A good usecase for this library is selecting different subjects (object) for a student from a list of available courses. With each course that is selected, the next list of available selections should only show the remaining, unselected subjects. If you are implementing this with dynamically created panels that are displayed in a scroll box, it becomes tricky to filter out the previously selected items (in another panel object).

### [Undo history](https://github.com/rubrican-research/saph/wiki/Undo-History)
A generic class that implements undo/redo for basic data types. Object level undo/redo is not supported.

### [Reactive store](https://github.com/rubrican-research/saph/wiki/Reactive-variables-TRInt,-TRStr-etc)
Imagine a variable that you can use like a normal integer, string, boolean, float or string in code but is actually an object that can:
  - signal when the value is changed
  - supports undo/redo, with signaling
    
This library allows you to create classes where each field signals their change state and supports undo/redo. Also implemented is a locking mechanism that allows you to lock the variable such that you can only write to it if you are the owner. (the first version is working. still to be refined).

### [Window Manager](https://github.com/rubrican-research/saph/wiki/Win-Manager)
This library was built to hel build a multi-window app, similar to the Lazarus IDE. Each form in the app can be registered in the initialization section of its unit **procedure registerWind(_FC: TFormClass);** and then you can call **getForm('TForm1').Show:** to instantiate the form. You can also manage multiple instances of the same form class to dynamically build a "show windows" menu to retrieve a window that got buried in the backgroud.

Using the obj.listener library, you can use Application.addListener() to listen to signals anywhere in your app. And you can call Application.signal() from anywhere in your app to inform other listeners of an event in your form/logic. This allows you to be able to update a variable change (together with Reactive Store) across multiple windows in an asynchronous manner.

### [Promise](https://github.com/rubrican-research/saph/wiki/Promises)
This implementation of Promise in Object Pascal is an attempt to understand how the Javascript Promise object is a solution to the problem of managing async calls to the server. 

The JavaScript implementation allows you to define callback functions as one pleases, with parameters that one can decide at will. However, I continued to find it hard to understand the  underlying mechanism - it is entirely possible that my difficulty stemmed from being a hard-core fan of "strict-typing", especially the Object Pascal flavor of it.

