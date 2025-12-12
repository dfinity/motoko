# core/WeakReference
Module that implements a weak reference to an object.

ATTENTION: This functionality does not work with classical persistence (`--legacy-persistence` moc flag).

Usage example:
Import from the core package to use this module.
```motoko name=import
import WeakReference "mo:core/WeakReference";
```

## Type `WeakReference`
``` motoko no-repl
type WeakReference<T> = { ref : weakT }
```


## Function `allocate`
``` motoko no-repl
func allocate<T>(obj : T) : WeakReference<T>
```

Allocate a new weak reference to the given object.

The `obj` parameter is the object to allocate a weak reference for.
Returns a new weak reference pointingto the given object.
```motoko include=import
let obj = { x = 1 };
let weakRef = WeakReference.allocate(obj);
```

## Function `get`
``` motoko no-repl
func get<T>(self : WeakReference<T>) : ?T
```

Get the value that the weak reference is pointing to.

The `self` parameter is the weak reference pointing to the value the function returns.
The function returns the value that the weak reference is pointing to,
or `null` if the value has been collected by the garbage collector.
```motoko include=import
let obj = { x = 1 };
let weakRef = WeakReference.allocate(obj);
let value = weakRef.get();
```

## Function `isLive`
``` motoko no-repl
func isLive<T>(self : WeakReference<T>) : Bool
```

Check if the weak reference is still alive.

The `self` parameter is the weak reference to check whether it is still alive.
Returns `true` if the weak reference is still alive, `false` otherwise.
False means that the value has been collected by the garbage collector.
```motoko include=import
let obj = { x = 1 };
let weakRef = WeakReference.allocate(obj);
let isLive = weakRef.isLive();
assert isLive == true;
```
