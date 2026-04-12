# `obj.listener`

**A lightweight, Lazarus-native event signaling library for Object Pascal**
**Built for dynamic UI composition, decoupled communication, and low-boilerplate architecture**

---

## Table of Contents

1. [Overview](#overview)
2. [Why this library exists](#why-this-library-exists)
3. [Design philosophy](#design-philosophy)
4. [Core concepts](#core-concepts)
5. [When to use `obj.listener`](#when-to-use-objlistener)
6. [When *not* to use `obj.listener`](#when-not-to-use-objlistener)
7. [Quick start](#quick-start)
8. [Basic usage](#basic-usage)

   * [Method listeners](#method-listeners)
   * [Proc listeners](#proc-listeners)
   * [Notify listeners](#notify-listeners)
9. [Signals and parameters](#signals-and-parameters)
10. [Dispatch modes (`qAsync`, `qSerial`, `qThreads`)](#dispatch-modes-qasync-qserial-qthreads)
11. [Dynamic UI patterns](#dynamic-ui-patterns)
12. [Lifecycle management](#lifecycle-management)
13. [Thread safety](#thread-safety)
14. [Best practices](#best-practices)
15. [Common pitfalls](#common-pitfalls)
16. [Recommended architectural patterns](#recommended-architectural-patterns)
17. [API reference (practical)](#api-reference-practical)
18. [Example patterns](#example-patterns)
19. [Design tradeoffs and limitations](#design-tradeoffs-and-limitations)
20. [Suggested signal conventions](#suggested-signal-conventions)
21. [FAQ](#faq)
22. [Final notes](#final-notes)

---

# Overview

`obj.listener` is a **lightweight event signaling library** for **Lazarus / Free Pascal / Object Pascal** that allows **any `TObject`** to:

* **emit named signals**
* **subscribe to named signals**
* **receive structured parameters**
* **dispatch listeners asynchronously, serially, or in dedicated threads**

It is implemented as a **type helper on `TObject`**, which means the API feels natural and low-friction:

```pascal
SomeObject.addListener('changed', Self, @OnChanged);
SomeObject.signal('changed');
```

The library was designed specifically to solve a common pain point in Lazarus GUI applications:

> **How do dynamically created frames, controls, or child views communicate state changes upward (or sideways) without becoming tightly coupled to their container hierarchy?**

It is particularly effective for:

* dynamically inserted frames
* reusable configuration panels
* scrollbox / flowpanel child components
* parent-child UI orchestration
* local pub/sub patterns within a visual tree
* decoupled messaging between non-visual helper objects

---

# Why this library exists

Object Pascal already has traditional event properties like:

* `OnClick`
* `OnChange`
* `OnClose`

Those are excellent for **fixed, explicit, 1-to-1 event relationships**.

But they become awkward when:

* child controls are created dynamically
* multiple listeners should react to the same event
* the parent should not be hardcoded into the child
* several different objects should respond to the same state change
* you want to avoid custom event type declarations everywhere
* you want to pass structured metadata without defining a new callback type each time

Without a signaling layer, these situations often degrade into:

* passing parent references into child constructors
* casting `Owner` / `Parent` to expected types
* manual callback plumbing for each frame class
* duplicated glue code
* tight coupling between UI layers
* brittle assumptions about container structure

`obj.listener` exists to solve that problem with a **small, ergonomic, Pascal-native abstraction**.

---

# Design philosophy

## 1. **Any object should be able to participate**

If a pattern is useful across the application, it should not be limited to:

* forms
* controls
* specialized event-bus classes

That is why `obj.listener` is built as a **helper for `TObject`**.

This means:

* any visual control can signal
* any frame can subscribe
* any non-visual service object can participate
* the same mental model applies everywhere

---

## 2. **Low ceremony beats “framework purity”**

Many event-bus or messaging systems become unpleasant in Object Pascal because they require:

* RTTI-heavy registration
* attributes
* central container objects
* explicit bus injection
* class-based message hierarchies
* too much setup for simple UI interactions

This library intentionally avoids that.

The goal is:

```pascal
ChildFrame.addListener('changed', Self, @OnChildChanged);
ChildFrame.signal('changed');
```

—not:

```pascal
GlobalEventBus.Subscribe<TChildChangedMessage>(...);
GlobalEventBus.Publish(TChildChangedMessage.Create(...));
```

The latter may be more “formal.”
The former is often better for **real Lazarus UI work**.

---

## 3. **Signals are semantic, not merely technical**

A signal should represent a **meaningful event in the domain of the sender**, not just a low-level callback.

Good signal names:

* `change`
* `saved`
* `selection_changed`
* `request_remove`
* `sig_queue_guh_import`
* `sig_config_ipc_config_change`

Bad signal names:

* `do1`
* `event2`
* `callback`
* `x`

Signals should communicate **intent**.

---

## 4. **Dynamic UI composition is a first-class use case**

This library is especially designed for UIs composed of:

* child frames inserted into a scrollbox
* embeddable panels
* repeated editor components
* dynamic visual collections
* reusable subviews

In these scenarios, `obj.listener` allows child objects to be:

* reusable
* self-contained
* independent of specific parent types

The child emits signals.
The parent decides whether to listen.

That is the correct separation of concerns.

---

## 5. **Parameters should be flexible, structured, and safe**

Signal parameters are passed as `TJSONObject`.

This provides:

* flexible structured payloads
* named fields
* extensibility over time
* self-describing signal contracts

Crucially:

> Each listener receives a **clone** of the parameter object.

This avoids the classic shared-payload problem where:

* one listener mutates the object
* another listener sees stale or corrupted data
* asynchronous dispatch introduces lifetime bugs

This cloning strategy is one of the library’s core safety features.

---

## 6. **Async is useful — but explicit**

The library supports multiple dispatch modes:

* `qAsync`
* `qSerial`
* `qThreads`

This is powerful, but should be used intentionally.

The default (`qAsync`) is often ideal for GUI work because it:

* avoids deep re-entrancy
* defers execution safely
* feels responsive
* decouples sender and receiver timing

---

# Core concepts

There are four key concepts:

---

## 1. **Sender**

The object that emits a signal.

Example:

```pascal
MyFrame.signal('saved');
```

Here, `MyFrame` is the sender.

---

## 2. **Signal**

A **string name** that identifies an event.

Example:

```pascal
'saved'
'change'
'request_remove'
SIG_QUEUE_GUH_IMPORT
```

Signals are **case-sensitive**.

---

## 3. **Subscriber / Listener**

The object (or proc) that reacts to a signal.

Example:

```pascal
ChildFrame.addListener('change', Self, @OnChildChange);
```

Here, `Self` is the subscriber.

---

## 4. **Parameters**

Optional `TJSONObject` data passed with the signal.

Example:

```pascal
ChildFrame.signal('change', TJSONObject.Create([
  'field', 'PumpNr',
  'value', '12'
]));
```

Listeners receive the payload as:

```pascal
procedure OnChildChange(const _sender: TObject; const _event: string; constref _params: TJSONObject);
```

---

# When to use `obj.listener`

Use `obj.listener` when:

* you are building **dynamic UIs**
* child frames need to notify parents
* multiple objects may respond to the same event
* you want to avoid direct parent references
* you want a clean local pub/sub pattern
* you want structured parameters without defining a new event type every time
* you want semantic signals that read clearly in code

Ideal use cases:

* rows in a scrollbox notifying the container of changes
* editor frames requesting deletion/removal
* child views notifying parent about validation state
* service objects signaling completion to interested consumers
* decoupled coordination between sibling-like objects through a common signaler

---

# When *not* to use `obj.listener`

Do **not** use `obj.listener` for everything.

It is not intended to replace all normal events.

Prefer normal event properties when:

* there is exactly one natural consumer
* the relationship is fixed and explicit
* a built-in control event already exists
* a typed callback is simpler and clearer
* you need compile-time enforcement of a very specific signature

Examples where native events are often better:

* `Button.OnClick`
* `Edit.OnChange`
* `Thread.OnTerminate`
* a custom component with one explicit `OnDataLoaded` event

A good rule:

> Use native events for **tight, explicit contracts**.
> Use `obj.listener` for **decoupled semantic signaling**.

---

# Quick start

## 1. Add a listener

```pascal
ChildFrame.addListener('changed', Self, @OnChildChanged);
```

## 2. Implement the listener method

```pascal
procedure TMainForm.OnChildChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  // react to the signal
end;
```

## 3. Emit a signal

```pascal
ChildFrame.signal('changed');
```

## 4. Clean up in destructor

```pascal
destructor TMyFrame.Destroy;
begin
  stopListening;  // important
  inherited Destroy;
end;
```

---

# Basic usage

---

## Method listeners

This is the most common and recommended usage.

### Register a method listener

```pascal
ChildFrame.addListener('changed', Self, @OnChildChanged);
```

### Listener signature

```pascal
procedure TParentFrame.OnChildChanged(
  const _sender: TObject;
  const _event: string;
  constref _params: TJSONObject
);
begin
  // _sender = object that emitted the signal
  // _event  = signal name
  // _params = optional JSON payload
end;
```

### Best use case

Use method listeners when:

* a frame/form/service object should handle the signal
* you need access to `Self`
* you want to maintain state in the subscriber

---

## Proc listeners

You can also attach plain procedures.

```pascal
procedure GlobalListener(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  // handle event
end;

SomeObject.addListener('saved', @GlobalListener);
```

### Best use case

Use proc listeners when:

* you want stateless handling
* the logic is global / utility-style
* you do not need instance state

---

## Notify listeners

You can also attach `TNotifyEvent` style handlers.

```pascal
SomeObject.addListener('refresh', Self, @OnRefreshRequested);
```

Handler:

```pascal
procedure TMyForm.OnRefreshRequested(Sender: TObject);
begin
  // no _event or _params here
end;
```

### Best use case

Use notify listeners when:

* you only need the sender
* the signal name is already obvious from registration context
* you do not need parameters

---

# Signals and parameters

## Signal naming

Signals are strings and are **case-sensitive**.

### Strong recommendation

Always define signals as constants.

**Good:**

```pascal
const
  SIG_CHANGE = 'change';
  SIG_REMOVE = 'request_remove';
  SIG_QUEUE_GUH_IMPORT = 'sig_queue_guh_import';
```

**Avoid:**

```pascal
ChildFrame.signal('chnage'); // typo = silent failure
```

Because string signals are flexible but not compile-time checked, constants are important.

---

## Sending a signal without parameters

```pascal
signal('saved');
```

---

## Sending a signal with parameters

```pascal
signal('changed', TJSONObject.Create([
  'field', 'PumpNr',
  'value', '12'
]));
```

---

## Parameter lifecycle

**Important:** The parameter object passed to `signal(...)` is **owned by the library** by default.

The library will:

1. clone the JSON for each listener
2. deliver the clone to that listener
3. free the original payload after dispatch setup
4. free each clone after the listener finishes

### Therefore:

* **Do not free `_params` in the listener**
* **Do not store `_params` for later use unless you clone it yourself**

This is one of the most important rules in the library.

---

## If you need parameters later

Clone the object inside your listener:

```pascal
procedure TMainForm.OnSomething(const _sender: TObject; const _event: string; constref _params: TJSONObject);
var
  _copy: TJSONObject;
begin
  if Assigned(_params) then
  begin
    _copy := _params.Clone as TJSONObject;
    try
      // use _copy later in this method or pass ownership onward
    finally
      _copy.Free;
    end;
  end;
end;
```

---

# Dispatch modes (`qAsync`, `qSerial`, `qThreads`)

The library supports three dispatch modes via `TInvokeType`.

```pascal
type
  TInvokeType = (
    qAsync,
    qThreads,
    qSerial
  );
```

---

## `qAsync` (default)

**Queues the listener via `Application.QueueAsyncCall()`**

This is the default and usually the best choice for GUI work.

### Characteristics

* asynchronous
* non-blocking for the sender
* generally main-thread friendly (via queued application callback)
* helps avoid re-entrancy issues
* excellent for UI orchestration

### Recommended for

* child frame → parent notifications
* deferred UI updates
* general app signaling
* most normal usage

### Example

```pascal
ChildFrame.addListener(SIG_CHANGE, Self, @OnChildChanged, qAsync);
```

---

## `qSerial`

**Runs the listener immediately in a blocking loop**

### Characteristics

* synchronous
* caller waits until listener completes
* deterministic ordering
* simplest execution model

### Recommended for

* situations where order matters immediately
* internal coordination where async deferral is undesirable
* debugging or tracing signal flow

### Example

```pascal
MyObject.addListener(SIG_VALIDATE, Self, @OnValidate, qSerial);
```

---

## `qThreads`

**Runs each listener inside its own thread**

### Characteristics

* concurrent
* potentially useful for heavy work
* introduces real threading concerns
* **not recommended for normal UI work**
* explicitly documented as less stable in complex shared-state scenarios

### Important warning

The library documentation itself notes that `qThreads` is **not fully stable in all cases**, especially when:

* the same listener method handles multiple threaded signals
* global/shared state is accessed
* multiple signal sources hit the same subscriber method concurrently

### If you use `qThreads`

You **must** understand:

* thread safety
* object lifetime
* synchronization
* shared state protection
* UI thread restrictions

### Recommendation

Use `qThreads` sparingly and only when you truly need it.

---

# Dynamic UI patterns

This is where the library shines.

---

## Pattern 1: Child frame notifies parent of internal state changes

### Child frame

```pascal
const
  SIG_ROW_CHANGED = 'row_changed';

procedure TRowFrame.edtValueChange(Sender: TObject);
begin
  signal(SIG_ROW_CHANGED, TJSONObject.Create([
    'field', 'Value',
    'value', edtValue.Text
  ]));
end;
```

### Parent frame

```pascal
procedure TParentFrame.AddRow;
var
  _row: TRowFrame;
begin
  _row := TRowFrame.Create(Self);
  _row.Parent := ScrollBox1;
  _row.addListener(SIG_ROW_CHANGED, Self, @OnRowChanged);
end;

procedure TParentFrame.OnRowChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  // React to any child row change
end;
```

### Why this is good

* child knows nothing about parent class
* child is reusable anywhere
* parent can attach or not attach behavior
* no hardcoded owner/parent casting

---

## Pattern 2: Child requests removal from container

### Child

```pascal
const
  SIG_REQUEST_REMOVE = 'request_remove';

procedure TRowFrame.btnDeleteClick(Sender: TObject);
begin
  signal(SIG_REQUEST_REMOVE, TJSONObject.Create([
    'id', myRowID
  ]));
end;
```

### Parent

```pascal
procedure TParentFrame.AddRow;
begin
  _row.addListener(SIG_REQUEST_REMOVE, Self, @OnRowRemoveRequested);
end;

procedure TParentFrame.OnRowRemoveRequested(const _sender: TObject; const _event: string; constref _params: TJSONObject);
var
  _row: TRowFrame;
begin
  if _sender is TRowFrame then
  begin
    _row := TRowFrame(_sender);
    _row.Parent.RemoveControl(_row);
    _row.Free;
  end;
end;
```

---

## Pattern 3: Parent broadcasts state to many children

You can also reverse the direction.

### Parent signals

```pascal
signal('theme_changed', TJSONObject.Create([
  'darkMode', True
]));
```

### Children subscribe

```pascal
ParentFrame.addListener('theme_changed', Self, @OnThemeChanged);
```

This can be useful when:

* parent state changes affect many embedded subviews
* you want a simple local message fabric

---

# Lifecycle management

This is one of the most important sections.

---

## **IMPORTANT: Call `stopListening()` in destructors**

If an object subscribes to signals, it should explicitly unregister itself when being destroyed.

### Recommended destructor pattern

```pascal
destructor TMyFrame.Destroy;
begin
  stopListening;
  inherited Destroy;
end;
```

### Why?

The library stores listener registrations in global structures.
If a subscriber is destroyed but still registered, later signal dispatch may try to invoke a dead method pointer.

The library includes safeguards and object-aliveness checks, but:

> **Explicit cleanup is the correct and recommended usage.**

---

## Also consider `rmListeners`

If the object itself is a signal source and you want to remove listeners attached *to it*:

```pascal
destructor TMyFrame.Destroy;
begin
  rmListeners;     // remove listeners registered on this object as sender
  stopListening;   // remove this object as subscriber elsewhere
  inherited Destroy;
end;
```

### Practical rule

For frames/forms that both:

* **emit signals**
* **subscribe to signals**

…it is often best to do both:

```pascal
destructor TMyFrame.Destroy;
begin
  rmListeners;
  stopListening;
  inherited Destroy;
end;
```

---

## About automatic cleanup

The library includes mechanisms such as:

* `beforeDestruction`
* observer hooks for some `TPersistent` descendants
* finalization cleanup

These help, but they should be treated as **safety nets**, not your primary lifecycle strategy.

### Recommended stance

Always clean up explicitly in destructors.

---

# Thread safety

## General rule

`obj.listener` is **primarily a UI and application signaling library**, not a hardened concurrent message broker.

That does **not** mean it is unsafe.
It means it should be used with **appropriate expectations**.

---

## Safe defaults

### `qAsync`

Usually the safest and most useful for Lazarus GUI work.

### `qSerial`

Safe and deterministic, but blocking.

### `qThreads`

Advanced usage only.

---

## Important thread rules

### 1. Do not touch UI from background threads

If a listener may run on a background thread (`qThreads`), then:

* do **not** update controls directly
* do **not** mutate GUI state directly
* marshal back to the main thread if needed

---

### 2. Protect shared mutable state

If multiple threaded listeners can access the same shared data:

* use critical sections
* or confine ownership to one thread
* or marshal all mutations through the main thread

---

### 3. `qThreads` is not for casual convenience

If you need parallel work:

* understand the lifecycle of subscriber objects
* understand parameter lifetime
* understand shared-state hazards

If you are unsure, use `qAsync` instead.

---

# Best practices

---

## 1. Always define signal names as constants

```pascal
const
  SIG_CHANGE = 'change';
  SIG_SAVE = 'save';
  SIG_REQUEST_REMOVE = 'request_remove';
```

This prevents silent typos.

---

## 2. Document signal contracts

For every non-trivial signal, document:

* when it is emitted
* what `_sender` will be
* what fields exist in `_params`

Example:

```pascal
// OnSignalQueueGUHImport()
// _params = session.asJSON()
// Required fields:
//   host      : string
//   sessionID : string
// Optional:
//   stelle    : string
//   stufe     : string
```

This is extremely valuable for maintainability.

---

## 3. Keep signals semantic

Signals should describe **what happened**, not how to react.

Prefer:

* `changed`
* `saved`
* `selection_changed`
* `request_remove`

Avoid:

* `do_parent_refresh`
* `call_parent_delete`
* `set_parent_state`

Signals should not encode implementation coupling.

---

## 4. Keep payloads minimal but meaningful

Only include what consumers need.

Not every signal needs a giant JSON payload.

Good:

```pascal
signal(SIG_ROW_CHANGED, TJSONObject.Create([
  'field', 'Pressure',
  'value', edtPressure.Text
]));
```

Overkill:

```pascal
signal(SIG_ROW_CHANGED, DumpEntireFormAsJSON());
```

---

## 5. Clone `_params` yourself if you need persistence

Remember:

* `_params` is temporary
* clone it if needed beyond the method

---

## 6. Prefer `qAsync` for UI orchestration

It is usually the sweet spot.

---

## 7. Reserve `qThreads` for carefully designed cases

Do not use `qThreads` as a “fast mode.”

It is a concurrency tool, not a convenience flag.

---

# Common pitfalls

---

## Pitfall 1: Forgetting `stopListening`

**Symptom:** weird exceptions later, especially in debug mode.

**Fix:** call `stopListening` in destructors.

---

## Pitfall 2: Using string literals everywhere

**Symptom:** typo silently breaks behavior.

**Fix:** use constants.

---

## Pitfall 3: Freeing `_params` manually

**Symptom:** double-free or random access violations.

**Fix:** never free `_params` in listeners.

---

## Pitfall 4: Storing `_params` without cloning

**Symptom:** dangling pointer after listener exits.

**Fix:** clone if you need it later.

---

## Pitfall 5: Using `qThreads` for UI code

**Symptom:** crashes, invalid control access, random instability.

**Fix:** use `qAsync` or marshal to main thread.

---

## Pitfall 6: Using signals where normal events are simpler

**Symptom:** over-abstracted code.

**Fix:** use `obj.listener` intentionally, not universally.

---

# Recommended architectural patterns

---

## 1. Child emits → parent listens

This is the canonical pattern.

Use when:

* child should remain reusable
* parent owns orchestration

---

## 2. Local signal fabric per composite UI

A parent frame can act as a local hub where:

* child rows signal up
* parent aggregates state
* parent emits higher-level signals outward

This creates a nice layered design:

* **child signal:** `row_changed`
* **parent signal:** `config_changed`
* **form signal:** `dirty_state_changed`

---

## 3. Service object emits completion / status

Non-visual helpers can also participate.

Example:

* importer signals `import_done`
* validator signals `validation_failed`
* transport layer signals `connection_lost`

---

## 4. Semantic escalation

A lower-level event can be translated into a higher-level one.

Example:

* EditBox change → child frame emits `field_changed`
* parent frame emits `row_changed`
* container emits `dataset_changed`

This is often cleaner than wiring low-level control events across multiple layers.

---

# API reference (practical)

Below is the **practical mental model**, not every overload in exhaustive detail.

---

## `addListener(...)`

Registers a listener for a named signal.

Common forms:

### Method listener

```pascal
addListener(const _event: string;
  constref _subscriber: TObject;
  constref _handler: TListenerMethod;
  constref _sigType: TInvokeType = qAsync);
```

### Proc listener

```pascal
addListener(const _event: string;
  constref _handler: TListenerProc;
  constref _sigType: TInvokeType = qAsync);
```

### Notify listener

```pascal
addListener(const _event: string;
  constref _subscriber: TObject;
  constref _handler: TNotifyEvent;
  const _sigType: TInvokeType = qAsync);
```

---

## `signal(...)`

Emits a named signal.

```pascal
signal(const _event: string; constref _params: TJSONObject = nil; _freeParams: boolean = True);
```

### Notes

* `_params` may be nil
* `_params` is usually owned/freed by the library
* each listener receives a clone

---

## `rmListeners`

Removes listeners associated with the sender object.

Forms:

```pascal
rmListeners;                  // remove all listeners on this object
rmListeners(const _event);    // remove listeners for one signal on this object
rmListeners(constref _subscriber: TObject); // remove listeners belonging to a subscriber
```

---

## `stopListening`

Removes this object as a subscriber from all signal sources.

```pascal
stopListening;
```

**Highly recommended in destructors.**

---

## `signals`

Returns all registered signal names on the object.

```pascal
function signals: TStringArray;
```

Useful for diagnostics / debugging.

---

## `memdump`

Returns a JSON snapshot of registered listeners.

Useful for debugging or tooling.

---

# Example patterns

---

## Example 1: A reusable editor row

```pascal
const
  SIG_VALUE_CHANGED = 'value_changed';

procedure TEditorRow.edtValueChange(Sender: TObject);
begin
  signal(SIG_VALUE_CHANGED, TJSONObject.Create([
    'value', edtValue.Text
  ]));
end;
```

Parent:

```pascal
procedure TEditorPanel.AddRow;
var
  _row: TEditorRow;
begin
  _row := TEditorRow.Create(Self);
  _row.Parent := ScrollBox1;
  _row.addListener(SIG_VALUE_CHANGED, Self, @OnRowValueChanged);
end;

procedure TEditorPanel.OnRowValueChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  signal('editor_changed'); // escalate semantic meaning upward
end;
```

---

## Example 2: Global-ish application signal

```pascal
Application.addListener(SIG_REFRESH_DATA, Self, @OnRefreshData);
Application.signal(SIG_REFRESH_DATA);
```

This can be useful, but should be used carefully and intentionally.

---

## Example 3: Validation state propagation

Child:

```pascal
signal('validation_state_changed', TJSONObject.Create([
  'valid', isValid
]));
```

Parent:

```pascal
procedure TContainer.OnValidationStateChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  RecalculateOverallValidity;
end;
```

---

# Design tradeoffs and limitations

This library intentionally chooses simplicity and ergonomics over maximal formalism.

That means there are tradeoffs.

---

## Tradeoff 1: String signals are flexible but not compile-time checked

**Benefit:** low ceremony, semantic clarity, no extra type declarations
**Cost:** typos are possible

**Mitigation:** always use constants

---

## Tradeoff 2: JSON payloads are flexible but not strongly typed

**Benefit:** extensible, self-describing, easy to evolve
**Cost:** field names are runtime contracts, not compile-time contracts

**Mitigation:**

* document signal payloads
* use helper/factory methods for constructing parameter objects
* use `.Get(...)` or defensive checks in listeners

---

## Tradeoff 3: Global listener maps require lifecycle discipline

**Benefit:** no central bus object needs to be passed around
**Cost:** subscribers should clean up explicitly

**Mitigation:** call `stopListening` in destructors

---

## Tradeoff 4: `qThreads` increases complexity sharply

**Benefit:** parallel execution available
**Cost:** thread safety, UI restrictions, lifecycle hazards

**Mitigation:** use `qAsync` by default

---

# Suggested signal conventions

A codebase benefits greatly from consistent signal naming.

---

## Recommended naming styles

### Generic local UI signals

```pascal
'change'
'saved'
'removed'
'selected'
'validated'
```

### More explicit component-level signals

```pascal
'row_changed'
'selection_changed'
'request_remove'
'validation_state_changed'
```

### Application / domain-level signals

```pascal
SIG_QUEUE_GUH_IMPORT
SIG_CONFIG_IPC_CONFIG_CHANGE
SIG_START_SERVER
SIG_STOP_SERVER
```

---

## Recommended style rules

* use lowercase with underscores for string literals
* use `SIG_` prefix for constants
* reserve “request_*” for action requests
* reserve past-tense or stateful names for facts/events:

  * `saved`
  * `validated`
  * `selection_changed`

This creates semantic clarity.

---

# FAQ

---

## “Should I use this instead of normal component events?”

No. Use it where decoupling is beneficial.

---

## “Can I use it for non-visual classes?”

Yes. It is a `TObject` helper, so any object can participate.

---

## “Can I store `_params`?”

Not directly. Clone it first.

---

## “Do I need to call `stopListening()` if the app is shutting down anyway?”

Probably not strictly necessary in all cases, but **yes, you should still call it**. It is the correct lifecycle discipline.

---

## “Is `qThreads` safe?”

It is powerful, but should be treated as advanced usage. For most UI work, prefer `qAsync`.

---

## “Why JSON instead of custom record/class messages?”

Because the goal is:

* flexibility
* low ceremony
* easy evolution of signal payloads
* no proliferation of tiny message classes

For this library’s intended use case, that tradeoff is often worth it.

---

# Final notes

`obj.listener` is not trying to be:

* a heavyweight enterprise event bus
* a full reactive programming framework
* a formal message broker
* a replacement for all normal events

It is something more focused and, in many Lazarus applications, more useful:

> **A lightweight, Pascal-native signal fabric that makes dynamic UI composition and decoupled object communication pleasant.**

If your application contains:

* many reusable frames
* dynamically inserted visual components
* layered UI containers
* non-trivial orchestration between child and parent views

…then `obj.listener` can dramatically reduce boilerplate while improving architectural clarity.

Used with discipline—especially around:

* signal constants
* parameter contracts
* destructor cleanup
* thread awareness

—it becomes a powerful foundation utility in a Lazarus codebase.

---

# Minimal recommended usage template

If you only remember one pattern, use this:

```pascal
const
  SIG_CHANGED = 'changed';

type
  TChildFrame = class(TFrame)
  public
    destructor Destroy; override;
  end;

  TParentFrame = class(TFrame)
  private
    procedure OnChildChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
  end;

destructor TChildFrame.Destroy;
begin
  rmListeners;
  stopListening;
  inherited Destroy;
end;

procedure TParentFrame.OnChildChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  // react
end;

// Somewhere during setup:
ChildFrame.addListener(SIG_CHANGED, Self, @OnChildChanged);

// Somewhere in child:
signal(SIG_CHANGED, TJSONObject.Create(['value', '42']));
```

Absolutely — and this section is important enough that it should be part of the **core README**, not a footnote.

Below is a **drop-in README section** you can insert after **“Core concepts”** or after **“Dynamic UI patterns”** (I would personally place it **after “Core concepts”**, because it explains one of the library’s defining strengths).

---

# Messaging scopes: local, private, and global buses

One of the most powerful design features of `obj.listener` is this:

> **Any `TObject` can act as a signal hub.**

Because the library is implemented as a `TObject` helper, you are **not forced into a single global event bus**.

Instead, you can choose the **scope** of your messaging architecture based on what makes the most sense for the objects involved.

This creates a very flexible and very Pascal-native model of communication:

* a **child object** can emit its own local signals
* a **parent object** can act as a private message hub for a related cluster of objects
* the **`Application` object** can act as a global message bus for cross-unit or cross-subsystem communication

The key advantage is:

> **All of these use the same API and the same mental model.**

That means you do **not** need one pattern for local callbacks and a completely different framework for global events.

---

## Why this matters

In many GUI applications, developers end up with two completely separate communication styles:

### Local communication

* direct event handlers
* custom callback types
* parent references passed into child constructors
* `Owner as TMyParentFrame` casting
* brittle component hierarchy assumptions

### Global communication

* singleton event bus
* service locator
* ad hoc global procedures
* manual unit dependencies between unrelated modules

`obj.listener` removes that split.

Instead, **any object can be the communication center for its own scope**.

This gives you a clean architecture with **graduated scopes of messaging**.

---

## The three natural scopes of messaging

`obj.listener` naturally supports three messaging scopes.

---

## 1. **Object-local signaling**

This is the simplest form.

An object emits a signal about its own internal state.

### Example

A child frame signals that its name changed:

```pascal id="1cc3s6"
const
  SIG_NAME_CHANGED = 'name_changed';

procedure TMapItemFrame.edtNameEditingDone(Sender: TObject);
begin
  signal(SIG_NAME_CHANGED, TJSONObject.Create([
    'oldName', myOldName,
    'newName', edtName.Text
  ]));
end;
```

Here:

* the **child frame** is the signal source
* the signal is **local to the meaning of that child**
* the child does **not** know who is listening

This is the most direct and most common use.

---

## 2. **Private / local message bus between related objects**

This is where `obj.listener` becomes especially powerful.

A parent, manager, or container object can act as a **private bus** for a small cluster of related objects.

This is ideal for:

* parent ↔ child communication
* container ↔ item communication
* manager ↔ managed objects
* dynamic frame collections
* reusable editor rows inside a scrollbox
* specialized map/dictionary-backed child lists

### Why this is useful

Instead of the child needing to know:

* what parent type owns it
* how the parent stores it
* what additional bookkeeping is required

…the child simply emits a semantic signal.

The parent listens and performs the necessary orchestration.

---

### Example: map-backed child list where rename affects the key

Suppose a parent stores child items in a map keyed by name:

```pascal id="7d8n2h"
FItemsByName: specialize TFPGMap<string, TMapItemFrame>;
```

Each child can be renamed by the user.

If the child changes its name, the parent must:

* remove the old key
* insert the new key
* validate uniqueness
* maybe update related UI or persistence state

The child should **not** know any of that.

### Child frame

```pascal id="5h6w1u"
const
  SIG_NAME_CHANGED = 'name_changed';

procedure TMapItemFrame.edtNameEditingDone(Sender: TObject);
begin
  signal(SIG_NAME_CHANGED, TJSONObject.Create([
    'oldName', myName,
    'newName', edtName.Text
  ]));
  myName := edtName.Text;
end;
```

### Parent frame

```pascal id="0u7q2z"
procedure TMapListFrame.AddItem(const AName: string);
var
  _item: TMapItemFrame;
begin
  _item := TMapItemFrame.Create(Self);
  _item.Parent := ScrollBox1;
  _item.NameValue := AName;

  FItemsByName.Add(AName, _item);

  _item.addListener(SIG_NAME_CHANGED, Self, @OnItemNameChanged);
end;

procedure TMapListFrame.OnItemNameChanged(
  const _sender: TObject;
  const _event: string;
  constref _params: TJSONObject
);
var
  _oldName, _newName: string;
  _item: TMapItemFrame;
  _idx: Integer;
begin
  if not (_sender is TMapItemFrame) then Exit;

  _item := TMapItemFrame(_sender);
  _oldName := _params.Get('oldName', '');
  _newName := _params.Get('newName', '');

  if (_oldName = '') or (_newName = '') or (_oldName = _newName) then Exit;

  // Example uniqueness check
  if FItemsByName.IndexOf(_newName) >= 0 then
  begin
    // reject / restore / notify user as needed
    Exit;
  end;

  _idx := FItemsByName.IndexOf(_oldName);
  if _idx >= 0 then
    FItemsByName.Delete(_idx);

  FItemsByName.Add(_newName, _item);

  signal('map_changed'); // optional higher-level semantic escalation
end;
```

### Why this is good design

The child only knows:

> **“My name changed.”**

The parent decides:

* whether the new name is valid
* whether the key must be updated
* whether secondary indexes must be rebuilt
* whether the change affects persistence
* whether the whole structure is now “dirty”

This is exactly the kind of decoupling `obj.listener` is designed for.

---

## 3. **Application-wide / global message bus**

For broader application communication, the `Application` object can act as a **global signal hub**.

This is especially useful when:

* the sender and receiver are in unrelated units
* you want to avoid direct `uses` dependencies
* the event is application-level or cross-subsystem
* the sender should not know about the receiver at all

### Example

```pascal id="3p9u8f"
Application.signal(SIG_QUEUE_GUH_IMPORT, asJSON(_session));
```

And elsewhere:

```pascal id="i0x8h6"
Application.addListener(SIG_QUEUE_GUH_IMPORT, Self, @OnSignalQueueGUHImport);
```

This allows:

* one unit to emit a semantic request
* another unit to react
* no direct dependency between them
* no need to reference the sender class

---

## Why `Application` is a good global bus in Lazarus

Using `Application` as the global signal hub is not a hack — it is actually a very natural fit in Lazarus GUI applications.

`Application` is already:

* globally accessible
* long-lived
* central to the app lifecycle
* available across units
* conceptually “the app-level coordinator”

So if you need a global message bus, `Application` is often the simplest and cleanest host object.

This avoids creating a separate singleton or service just to host signals.

---

## Architectural benefit: reducing unit dependencies

One of the most practical benefits of using `Application` as a global signal bus is **dependency reduction**.

Without a global semantic signaling mechanism, code often becomes tightly coupled:

* Unit A needs to call a method in Unit B
* Unit B now appears in `uses`
* Unit B may already depend on Unit A (or on Unit C that depends on A)
* circular dependency pressure increases
* “common” units start accumulating unrelated types just to break cycles

With `Application.signal(...)`:

* sender only needs access to `Application`
* listener only needs access to `Application`
* neither side needs a direct reference to the other

In Object Pascal, where `uses` relationships matter a great deal, this is a significant architectural advantage.

---

# Recommended rule: use the narrowest scope that makes sense

One of the easiest ways to keep a signaling architecture clean is to prefer the **smallest appropriate message scope**.

---

## Prefer a **private/local bus** when:

* communication is between parent and child
* communication is between a manager and its managed objects
* the relationship is naturally local to a composite UI
* the signal is not meaningful outside that local structure
* you want strong locality and easier debugging

Examples:

* row frame notifies parent of value change
* item frame requests removal from container
* child frame signals rename, parent updates map key
* validation state bubbles upward within a settings panel

---

## Prefer the **global `Application` bus** when:

* the signal crosses subsystem boundaries
* the sender and receiver should not know about each other
* you want to avoid unit dependencies
* the event is truly application-level
* multiple unrelated listeners may care

Examples:

* queue import request
* configuration changed
* server started / stopped
* global refresh requested
* theme changed
* transport disconnected
* domain-level workflow event

---

# A practical mental model

A useful way to think about `obj.listener` is:

> **Every object can be a scoped semantic message hub.**

That means:

* **child objects** can emit their own semantic state changes
* **parent objects** can act as local buses for a cluster of related objects
* **`Application`** can act as the app-wide bus for decoupled cross-unit events

This is more flexible than a traditional “one global event bus” architecture.

---

# Why this is better than a single global singleton bus

Many event-bus systems push developers toward a single global bus.

That can work, but it often leads to:

* hidden coupling
* difficult provenance tracking
* “everything talks to everything”
* debugging becoming harder over time

`obj.listener` avoids this by allowing you to **choose scope intentionally**.

This is one of the library’s most important design strengths.

---

# Important caution: do not overuse the global bus

Because `Application.signal(...)` is convenient, it is tempting to use it for everything.

Resist that temptation.

A global bus should be reserved for:

* true cross-subsystem communication
* app-wide lifecycle events
* infrastructure-level notifications
* semantic events that genuinely matter beyond a local object cluster

If you use the global bus for every child/parent interaction, you lose the architectural clarity that makes the system powerful.

### Bad example

A child frame inside a local editor signals everything through `Application`:

```pascal id="h4q0k8"
Application.signal('row_changed', ...);
Application.signal('field_changed', ...);
Application.signal('delete_requested', ...);
```

This is usually too broad.

### Better example

Use the child itself (or the parent) as the local signal hub:

```pascal id="f8v2k1"
signal(SIG_ROW_CHANGED, ...);         // child-local semantic event
ParentFrame.signal(SIG_EDITOR_DIRTY); // escalated parent-level semantic event
```

Then reserve `Application.signal(...)` for things that truly matter application-wide.

---

# Pattern: semantic escalation across scopes

A very elegant pattern is to let signals **escalate semantically** as they move outward.

For example:

### Child-level signal

```pascal id="nd5o6m"
signal('name_changed', TJSONObject.Create([
  'oldName', _oldName,
  'newName', _newName
]));
```

### Parent reacts and emits a broader signal

```pascal id="w7g9k3"
signal('map_changed');
```

### Higher-level form reacts and emits an application-level signal

```pascal id="pv1m4z"
Application.signal(SIG_CONFIG_CHANGED);
```

This creates a layered, very readable architecture:

* **low-level local facts**
* **mid-level structural state changes**
* **high-level application/domain events**

That is often cleaner than wiring low-level events directly across multiple layers.

---

# Example: local + global combined

This is a strong real-world pattern.

### Child row emits local signal

```pascal id="1n7s4b"
signal('row_changed', TJSONObject.Create([
  'field', 'PumpNr',
  'value', edtPumpNr.Text
]));
```

### Parent editor listens, updates state, emits broader local signal

```pascal id="6j3k2p"
procedure TEditorFrame.OnRowChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  RecalculateValidity;
  signal('editor_changed');
end;
```

### Main form listens, marks document dirty, optionally broadcasts app-wide

```pascal id="y4r9u2"
procedure TMainForm.OnEditorChanged(const _sender: TObject; const _event: string; constref _params: TJSONObject);
begin
  SetDirty(True);
  Application.signal(SIG_DOCUMENT_DIRTY_CHANGED);
end;
```

This layered messaging is one of the most natural and maintainable ways to structure a complex Lazarus UI.

---

# Recommended best practice summary

Use these guidelines:

### Local object signal

Use when:

* the event belongs to the sender itself

### Parent/private bus

Use when:

* the event belongs to a related object cluster
* parent/manager orchestration is required

### `Application` global bus

Use when:

* the event crosses subsystem boundaries
* you want to avoid unit dependencies
* the event is meaningfully app-wide

### Golden rule

> **Prefer the narrowest scope that still expresses the event naturally.**

That keeps your signal topology clean, understandable, and maintainable.


# License

MIT License (or project license)

---

# Credits / Rationale

This library was created to provide a **simple, ergonomic, Lazarus-native way** to implement decoupled signaling between ordinary objects—especially in **dynamic UI composition scenarios** where traditional event wiring becomes cumbersome.

It is inspired by classic software patterns such as:

* Observer
* Publish/Subscribe
* Event Bus
* Mediator

…but intentionally re-expressed in a way that feels natural in **Object Pascal**, especially with the advent of **object helpers**.
