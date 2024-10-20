***unit saph.promise***
# Motivation
This implementation of Promise in Object Pascal is an attempt to understand how the Javascript Promise object is a solution to the problem of managing async calls to the server. 

The JavaScript implementation allows you to define callback functions as one pleases, with parameters that one can decide at will. However, I continued to find it hard to understand the  underlying mechanism - it is entirely possible that my difficulty stemmed from being a hard-core fan of "strict-typing", especially the Object Pascal flavor of it.

This library implements the "freedom" of choosing any kind of response and reject callbacks by using a clever feature of Object Pascal - passing class types as variables.

If we have a class like this
```
type
	TPromiseCallback = class
		procedure Execute; virtual;
	end;
	
	TResolve = class(TPromiseCallBack);	
	TResolveClass = class of TResolve;
	
	TReject = class(TPromiseCallBack);
	TRejectClass = class of TReject;
	
```

Then we can have a function like this that returns a promise object

```
function promise(
		_execFunc: TPromiseExecFunc; 
		_resolveClass: TResolveClass;
		_rejectClass: TRejectClass
		): TPromise;
```

Further we can define specific implementations of the resolve class and use that to invoke `promise()`

*Note: This is representative. It is not an actual example *
```
type
	TResolveMakeAppointment = class(TResolve)
		appointmentID: string;
		personID: string;
		timeSlot: string;
		function updateOfficeCalendar: integer; // returns code
		function updatePersonCalendar: integer;
		
		procedure Execute; override;
	end;
	
	TRejectMakeAppointment = class(TReject)
		appointmentID: string;
		personID: string;
		timeSlot: string;
		function generateErrorMessage: string;	
				
		procedure Execute; override;
	end;
```

Inside the promise function we can decide whether we are going to resolve the promise or reject the promise and then assign the values to the object. To instantiate a promise object we do something like:

```
var
	appointmentPromise: TPromise;
begin
...
...
	appointmentPromise := Promise(
					@makeAppointment,
					TResolveMakeAppointment,    //Class. not object
					TRejectMakeAppointment      //Class. not object
			 	);
							
	appointmentPromise.run;
	
end;
```


# Design considerations
I discovered many things while trying to recreate this functionality but mostly I recognized some of the core differences of trying to achieve invocation freedom within a strictly typed language that is quite attractive about untyped scripting languages. One would have to approach the problem differently. There can be no one-to-one comparison between the two language paradigms (this is probably a redundant statement from my side).
## Self-explanatory code
First off, before using a  Promise in your application, you would have to derive the necessary sub-classes for resolve and reject in order to handle the logic specific to the operation you are implementing. Although this seems to be quite tedious at first, the most obvious advantage to this approach is that the added tedium automatically prepares your code to be documented. You know immediately when you instantiate your promise, exactly what logic will be executed. Your resolve and reject classes can long and complex but they are all together in their own module (unit). When you come back to your code a year later, you can immediately find your way around the business rule. You can easily derive from your  Resolve class to implement a new or different business rule. 

The main point here is that such code makes it highly readable and maintainable. I have found this to not be the case when examining JavaScript code-bases that were not put together thoughtfully. Creating a level of self-explanatory code is a matter of personal discipline. Object Pascal, on the other hand offers you an intrinsic module structure that encourages  that personal discipline. (imho).

## Resolving and Rejecting based on return value
I realized that I would need to use function return values to inform the calling function what I intended to do with  - whether I want to resolve or reject.  My first iterations of this used resolve and reject as function pointers that I call from the promise function. But, because function pointers would have fixed parameters, achieving invocation freedom would then required parameters to be passed as pointers, which in turn would complicated the function body. 

The choice to send reject and resolve as object instances (which are instantiated from the ClassType variables passed to the Promise factory) make the code easier to follow and maintain.

So, you in addition to setting the required data to the resolve or reject object, you have to indicate in your function return value whether you have resolved the promise or rejected it. You can also raise an exception, which is caught in the Promise.run function, which in turn calls "catch".

Because the design required the use of function return values, I moved the invocation of the `resolve.execute` or `reject.execute` to the `Promise.run` function.  You can call resolve.execute within the promise function. There is no programmatic way to prevent that. However in order to guarantee that `resolve.execute` will be called even when you forget to call it explicitly, I implemented a call counter in the `TPromiseCallback` function that executes the `.execute` procedure only once in the lifetime of the object. So, if you call `.execute` inside your promise method, it won't be called again. If you forget to call it, it **will be called** in the `Promise.Run() `method.

## Chainablility
Javascript promises are chainable with `then()`.  This posed somewhat of a challenge to implement in Object Pascal. 

The current iteration stores each promise call in a list. Calling `then()` adds a call to the list. The `run()` method iterates through the list and calls the promise methods in the order they were added.

Unlike the Javascript implementation, `then_()` does not return a new object. It returns the same object for method chaining.

Chaining execFunctions is done by calling `then_()`.
***Note:*** The underscore in `then_()` is necessary because "then" is an Object Pascal reserved word.
```
        function then_ (
			        _execFunc: TPromiseExecFunction; 
			        _resolveClass : TPResolveClass = nil; 
			        _rejectClass  : TPRejectClass = nil): TPromise; overload;
			    
        function then_ (
		        _execFunc: TPromiseExecFunction;
		        _resolveFactory: TPResolveFactory; 
		        _rejectFactory: TPRejectFactory;
		        _errorFactory: TPromiseErrorFactory): TPromise; overload;

```

When you do not provide resolve or reject instance generators, TPromise assigns the  default TPResolve and TPReject instance generators, both for class types and factory methods. 

Default values are not provided in  second `then_() `call in order to differentiate this call from the previous one. having all nil default parameters makes it impossible for the compiler to decide which overloaded then_() is being invoked.

# Usage
After you add saph30 package to your project, include `saph.promise` into your uses clause.  It is best to make use of the factory functions defined in the unit to instantiate the needed objects.

## Free after run
It is important to note that all objects that are instantiated within the promise are freed by the promise. You don't have to explicitly free any object instance that is part of the Promise structure. 

Also, every Promise object is automatically freed after `run()` is completed. You should never call `_promise.Free();` 

This is core part of the design. The rationale is that a promise action is atomic and you should not be able to re-run a promise without reinitializing all the necessary state variables. 

## Promise object
Use the `Promise()` function to instantiate a promise object. It just makes the code cleaner to read. There are two distinct ways to use promises:
1. ***Using factory methods***: You don't need to derive any classes from TPromise, TPResolve or TPReject.
2. ***Using classes***: Defining descendants of TPromise, TPResolve, TPReject.
Regardless of how you choose to use Promises, the common element is the Promise Execute Function pointer that is required by every instance of a Promise, which contains the code that needs to execute inside a Promise structure.
### Using factory methods
You can instantiate a TPromise object by providing factory methods that instantiate TPResolve and TPReject objects respectively. Factory methods give you the flexibility to initialize the objects, inject dependencies and so on before they are used inside the Promise.  

```
function Promise(
		_execFunc: TPromiseExecFunction; 
		_resolveFactory : TPResolveFactory; 
		_rejectFactory  : TPRejectFactory = nil; 
		_errorFactory: TPromiseErrorFactory = nil): TPromise;
```

### Using classes
You need to provide 3 parameters
1. Pointer to the Execute function (see below)
2. Class reference of the TPResolve class that should be instantiated and passed to the promise execute function.
3. Class reference to the TPReject class that should be instantiated and passed to the promise execute function.
```
var
    _promise: TPromise;
begin
    {Tests all the different ways in which we can define a promise}
    _promise := Promise(@loadAppointments, TPResolve, TPReject);
    _promise.then_(@sortAppointments, TAppointmentsResolve)
            .then_(@filterAppointments)
            .then_(@displayAppointments)

            .catch_(TPromiseError)
            .finally_(@doneLoadingAppointments);

    _promise.OnResolved := @OnResolved;
    _promise.OnRejected := @OnRejected;
    _promise.OnException:= @loadingFailed;

    _promise.run;

end;

```

You can chain promise execute methods with `then_()`
Once the promise has been instantiated, you can call the `run()` method to run the promise.
## Execute function
This is the method that implements the actions you want to perform inside a promise construct.
The signature of the method has to match: 
```
    TPromiseExecFunction    = function (
					constref _resolve: TPResolve;
					constref _reject: TPReject
					): TPromiseExecFuncResult of object;
```

When this function is called by the promise, it sends an instance of the configured Resolve object and the Reject object. Note that for each execFunc that is chained to the Promise, you can provide a different implementation of Resolve and Reject when you

You have to return a TPromiseExecFuncResult object from your method. If you don't, it raises an Access Violation. Use a the factory function (below) to instantiate a proper return value.
### Returning data
As with the Javascript version of Promise, you can send data as a TJSONObject instance with the result. When you send data, this data is made available to the **next** promise Execute function that is chained to the promise with `then()`. The data is sent in with resolve object instance.  

All data objects are freed by the Promise.

#### Example
```
function TForm3.sortAppointments(constref _resolve: TPResolve; constref
	_reject: TPReject): TPromiseExecFuncResult;
begin
    Memo1.lines.add('sortAppointments');

    {Data available from previous Promise execute function}
    if assigned(_resolve.json) then
        Memo1.Lines.Add(_resolve.json.AsJSON);

    _resolve.Execute; // explicit call to resolve. Can be omitted if you return the required result (below)

    // Returning data as the second parameter
    Result := PromiseResult(promiseResolved,
					 TJSONObject.Create([
						    'sorted_appointments', 
						    TJSONArray.Create(['A', 'AB', 'AC'])
						    ])
			   );
end;
```

### Return values from an execFunc
The Enum NPromise result is a field of the TPromiseResult object. The value returned is handled inside the `run()` function of TPromise

```
    NPromiseResult = (promiseUnknown,
                      promiseInit,          // Init value
                      promiseRunning,       // The action is still running
                      promiseResolved,      // The action is resolved
                      promiseRejected,      // The action is rejected
                      promiseException,     // The action resulted in an error
                      promiseTimeOut,
                      promiseKilled
                    );
```

The chain of promise calls is broken at the first instance of the promise result being equal to `promiseRejected` and above.

An exception is raised for promise results being equal to `promiseException` and above. There is no exception raised on `promiseReject` even though the chain of promise functions is broken.

```
									    
    TPromiseResolvedMeth    = procedure(constref _resolve: TPResolve) of object;
    
    TPromiseRejectedMeth    = procedure(constref _reject: TPReject) of object;
    TPromiseCatchMeth       = procedure(constref _catch:  TPromiseError) of object;

```

# Exception
Like the resolve and reject classes, the library also contains a class that handles the errors: `TPromiseError`. You can customize the error class by declaring a derived class and chain it to the the Promise object using
```
function catch_(_errClass: TPromiseErrorClass): TPromise; overload;
```

Whenever the Promise encounters an exception, an instance of the TPromiseErrorClass is used to handle the exception.

# Event Handlers
In addition to resolve, reject and error classes, Promises also expose event handlers that you can bind to the Promise object to send control to the respective methods depending on the result of your promise execFunc.
```
        OnInit       : TNotifyEvent;
        OnResolved   : TPromiseResolvedMeth;
        OnRejected   : TPromiseRejectedMeth;
        OnException  : TPromiseCatchMeth;
        OnTimeOut    : TPromiseCatchMeth;
        OnKilled     : TPromiseCatchMeth;
        OnFinally    : TNotifyEvent; // Will be called only 
							         // if finally_() was not previously set.
```
Using these Event handlers, you can use  a promise object without declaring any additional classes or supplying additional parameters. You simply assign the necessary event handler methods (that match the parameter list as defined by the the method pointer type). Your code could look like this.
```
var
	_promise: TPromise;
begin
	_promise := Promise(@loadCustomers); // use default reject, resolve objects
	_promise.OnResoved := @customersLoaded;
	_promise.OnRejected:= @loadingError;
	_promise.OnException:= @loadingException;
	
	_promise.run;
end;
```

# Async and Threaded runs
It is planned to implement the `run()` method to execute inside an `Application.QueueAsyncCall()` method or to run inside a thread. This would make the invocation of a Promise a non-blocking operation.

*To be done*