# core/WeakReference
Module that implements a weak reference to an object.

ATTENTION: This functionality does not work with classical persistence (`--legacy-persistence` moc flag).

Usage example:
Import from the core package to use this module.
```motoko name=import
import WeakReference "mo:core/WeakReference";
```

## Function `allocate`
``` motoko no-repl
func allocate<T>(obj : T) : WeakReference<T>
```


## Function `get`
``` motoko no-repl
func get<T>(weakRef : WeakReference<T>) : ?T
```


## Function `isLive`
``` motoko no-repl
func isLive<T>(weakRef : WeakReference<T>) : Bool
```

