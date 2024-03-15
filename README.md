# saph
 A Lazarus package to simplify messaging, signalling and IPC
(Work in progress)

# Inspiration
I wanted to have a simple way to implement the kind of refresh capabilities of Excel and React/VueJs/AlpineJS inside a Lazarus form. This library draws inspiration from that paradigm. I am building complex UI in Lazarus by dynamically intantiating frames inside a scroll box, which then means that I would need an easy way to listen for events from child controls in order to made decisions at the form level. For instance, in a list of 20 frames inside a scroll box, I want to delete frame 14. The main form needs to know that the user has clicked on the delete button that is placed inside the frame. The best way would be to just call `delButton.signal('delete'); ` which will inform the containing form. The scroll box can also listen for the "delete' signal and do specific things - similar to how browser applications work with modern JS frameworks.

# Control.Listener
This library allows you to freely define event listeners - which is text based, case-sensitive - on any control. This is implemented as a Type Helper on TControl.

## Usage 
1. Compile the package in your Lazarus installation. There are two package files - saph30.lpk compiles on Lazarus 3.0. saph2_12.lpk compiles on Lazarus 2.0.12
2. Include Controls.Listener into your uses clause.
3. Implement the Listener procedure. You can use either an object method or a normal unit level procedure. The procedure should have the following signature:
4.
```
   TControlListenerProc   = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject);
   TControlListenerMethod = procedure (const _sender: TControl; const _event: string; constref _params: TJSONObject) of object;
```
* _sender: this is the control which sent the signal
* _event: this is the event string that was signalled
* _params: if the sender wants to send additional parameters, a JSON Object can be used. This allows you to send complex values with each event signal
  
5. For any control in that unit (form, button, editbox etc.) you can now assign an event listener as follows:
```
edtName.addListener('change', @FormChange);   // Where FormChange is a general listener for all changes to data on the from
edtDOB.addListener('change', @FormChange);    // EditBox for Date of Birth - change is listened by FormChange
edtDOB.addListener('change', @CalculateAge);  // Same Date of Birth edit box, the change will be listened by CalculateAge
```
6. Next step: You have to signal the event. You have to implement the OnChange event handler for all controls - to match the previous example - and simply include the following line:
```
procedure TMyForm.Edit2Change(Sender: TObject);
begin
   TControl(Sender).signal('change');
end;  
```
7. You can select ALL controls on your form and implement the same event handler to signal the "change" event. Now depending on how you have setup your eventListeners, you have a central place to respond to changes AND you can define multiple event listeners to the same control. 
8. Remember that the events can be named as you like, so you have to option of designing a UI system where you can assign multiple listeners for complex events like "remember-to-reorder", "birthday-this-week" etc. The flexibility is yours to choose.

## Async. Threads. Serial
The invocation of the event listeners can be selected. You can choose between:
```
qAsync - This means that each event listener is called via an Application.QueueAsyncCall()
qThreads - This will call each event listener inside a separate thread.
qSerial - Each event listener will be called inside a loop (running inside the main Application thread.
```

This is done by supplying the parameter in the addListener() method as follows:
```
edtName.addListener('change', @FormChange, qAsync{default});   // Where FormChange is a general listener for all changes to data on the from
edtDOB.addListener('change', @FormChange, qThreads);    // EditBox for Date of Birth - change is listened by FormChange
edtDOB.addListener('change', @CalculateAge, qThreads);  // Same Date of Birth edit box, the change will be listened by CalculateAge()
edtDOB.addListener('change', @HighlightSiblings, qThreads);  // Same Date of Birth edit box, the change will be listened by HighlightSiblings()
edtDOB.addListener('change', @QueueFreeBirthdayCake, qThreads);  // Same Date of Birth edit box, the change will be listened by QueueFreeBirthdayCake()
```
## Multiple listeners in one line
For the case where you need to assign multiple listeners for the same event you can now use the following procedure call:
```
    with Edit1 do begin
        addListener('change', [@L1Change, @L2Change, @L3Change, @L4Change], qThreads);
	end;
```
## Parameters for Event Listeners
Parameters are sent via a JSONObject when signaling the event. This means that you can design complex logic around event signals.
The signal procedure has the following signature:
```
    procedure signal(const _event: string; constref _params: TJSONObject=nil; _freeParams: Boolean = true);
````
### Usage
```
edit1.signal('new student', TJSONObject.Create([
     'name', 'John Dear',
     'dob', '2010-07-09'
     'gender', '',
     'subjects, TJSONArray.Create(['english', 'pottery', 'history', 'chemistry']);
]));
```

The JSON Object is automatically freed once the event has finished processing. You can prevent the automatic free mechanism by supplying 
```
student:= getStudentJSON(_id);
studentForm.signal('new student', student, false); // Do not automatically free the student JSON object because it is needed in the application
```

## Singleton Runner
There are two implementations of how the event procedures are invoked. This is right now an experiment to see which is more effective.
There is a global variable in `Controls.Listeners` called `ListenerSignalMode: TListenerSignalMode = lmSingleton;` The default method is that all event listener procedures are run from within the same runner object (which decides whether to call Application.QueueAsyncCall(). or TThread.ExecuteInThread() or to directly invoke the event listener (inside the main loop). If `ListenerSignalMode := lmDynamic` then for each event listener proc that needs to be executed, a new Runner object is instantiated. My test have not shown an advantage to using lmDynamic. However it is there for experimentation


