### Purpose:
To freely define event listeners - which is text based, case-sensitive - on any object.
This is implemented as a **class helper** on TObject.

Implement event listeners in your units with the following signature

```
procedure (const _sender: TObject; const _event: string; constref _params: TJSONObject);
```

An event listener can be a TNotifyEvent procedure.

```
procedure Listener(Sender:TObject);
```

Then, for any control in that unit (form, button, editbox etc.) you can now assign an event listener as follows:
```
edtName.addListener('change', @FormChange);   // Where FormChange is a general listener for all changes to data on the form.

edtDOB.addListener('change',  @FormChange);    // EditBox for Date of Birth - change is listened by FormChange

edtDOB.addListener('change',  @CalculateAge);  // Same Date of Birth edit box, the change will be listened by CalculateAge
```
### About Signals

This library provides a mechanism to implement complex signaling systems on objects. When planning to use signals, it would help to start with a *sequence diagram* so that you have a blue-print of how the system will behave.  

When a signal is designed, there are three things to consider:

- **points of invocation**: This library uses string literals as signal identifiers. They are case sensitive. Some of the ways to manage signals in your code-base include:
	- declaring string constants for signal names. This ensures that you always invoke the intended signal in your code. If you further organize your code where all related signal constants are located in a single unit, you could also document how the signals are intended to be used.
    - use "search across files" by the signal constant name to locate all the places in code where the signal is invoked, and also where listeners are added. Since there are no restrictions on when a signal listener may be added, it is technically possible to add listeners in any unit where the object may be reference, scattering the listeners all over the code base. 

- **invoking signals**: You can control how the signals are sent when  you add a listener. You can mix up the modes as you add listeners. Each listener gets called by invocation type that was specified when the listener was defined. The invocation types are:
	  - _qAsynch_:(default). signal() returns without waiting for the procs/methods to complete. This queues the listener proc/method in the Application object's asynch queue. This way is best for GUI applications where you want to signal user actions to multiple subscribers.
	  - _qThreads_: signal() returns without waiting for the procs/methods to complete. This runs each of the attached listener procs/methods in a separate thread. The method runner uses a critical section **runnerCS** (global variable that is managed in the initialization and finalization sections of the unit) when running in _qThread_ mode. If needed, you can use this in your own methods that will be called by the signal.
	  - _qSerial_: This runs each of the attached listener procs/methods in a non breaking loop. signal() returns after all the listener methods have been executed.

- **parameters:** because the parameters of a signal are implemented as a JSONObject, it is important to define each field in the parameter list and denote its purpose. It may prove to be useful to create a separate unit that contains factory methods to instantiate the parameter object of a signal, which provides a good place in the the code to document how a signal may be consumed.

- **avoid recursion:** When signals are invoked with qAsync or qThreads, there is potential for creating "callback hell". It can become extremely difficult to keep track of the signals being fired, especially when your design is based on the same object.signal being invoked from multiple place. This could make debugging very difficult. The problem is exacerbated when listeners also send signals. 

- **call stopListening() in destructors:** When an object consumes signals, its address is registerd in the library. When the object is freed, the library does not implicitly know that the object has been freed. So, in the destructor of the object, call "stopListening()", it will delete the entries for this object. Exceptions are handled in the library if the listener object is not alive anymore, but it makes debugging very cumbersome because every call to a deleted object will raise an exception in the debugger (not in the final executable).

	**NOTE:** As an experiment, the procedure BeforeDestruction is "hidden" in the object listener (class helper). Technically, you can't override or reintroduce methods in a type helper, however I was able to include BeforeDestruction() so that I can call stopListening() automatically when an object is being destroyed. However, **TControls** override BeforeDestruction and it does not call the inherited method. So, whenever you are manually freeing a control, it is best to also call the stopListening() on that control. 
	
	There is no harm done if stopListening() is not called. When the application is terminated, all memory used up by the object listener is freed. However, it can lead to annoying access violation exceptions during debugging (as described above).

- **thread safety:** One of the listener types is *qThreads*, which calls each listener method inside its own thread. The library has a dedicated variable **runnerCS**: TRTLCriticalSection; which can be used inside your listener methods if you have opted to consume the signal as  qThread. 

**NOTE:**  Thread listeners have shown some instability in this release when:
- multiple signals use the same listener procedure as below:
```
obj.addListener('sigA', self, @listener1, qThreads); 
obj.addListener('sigB', self, @listener1, qThreads);
```
Two signals "sigA" and "sigB" are calling the same object and method "listener1". This works well for aAsync and qSerial. But for qThread, it can cause unexpected exceptions if the method accesses any global variables.

- the signal is sent with  params (TJSON Object).  Use the cticial section **runnerCS** in the unit obj.listener inside your thread-listener procedures if you experience problems.

### Best Practices
Ideally, there should be a dedicated method in every unit or class that adds listeners. This way it is easy to see at a glance how many listeners are assigned to an object OR all the different objects that are being listened to within a listener implementation.

Factory functions that instantiate objects are a good way to keep the listener definitions in a single place. If a class consumes objects from other classes and also manages their lifecycles, this style is better suited, for it guarantees that every instance of a particular class has the exact same set of listeners

The  listener code can make decisions based on:

- **_sender** (which tells you which object has signaled)
- **_event** (the string literal of the event)

*NOTE:* the lifecycle of the param object is managed by the listener library. It will be freed as soon as the listener method is exited. So, if you need for it to persist outside of the method, you would have to clone it before calling signal().