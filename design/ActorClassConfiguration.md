# Configuring Actor Installation

Imports of actor classes are now supported. An actor class is imported
as a module containing a type definition an asynchronous actor class constructor.

The constructor abstracts a call the System's `ManagementCanister`,
passing the quoted code of the imported actor class and actual
argument as arguments, using the managment canister methods:

```
  create_canister : () -> (record {canister_id : canister_id});
  install_code : (record {
    mode : variant {install; reinstall; upgrade};
    canister_id : canister_id;
    wasm_module : wasm_module;
    arg : blob;
    compute_allocation : opt nat;
    memory_allocation : opt nat;
  }) -> ();
```

By default, motoko constructores create a fresh canister id and use
`#install` (or `mode`) and `null` for `compute_allocation` and
`memory_alloction`. The latter is unfortunate as it leads to a default
allocation of 4GB.

While this is sufficient for basic programmatic installation of
canisters, it fails to expose some the additional configuration
arguments of canister installation, choosing sensible defaults
instead.

Programmers will soon wish to tweak canister installation, so we need
some mechanism to determine non-default values.

Solutions:

1. Use global state, set prior to an actor constructor call using
   configuration functions, and reset to default on call.
   This raises the question of whether the state should be shared or individual to
   each actor class.
   In the latter case, it's not clear where to store
   the state since modules (the imported form of actor classes) are
   meant to be pure (it the former case, it can be stored in the prelude).

2. Recognize and utilize certain actor class parameters to configure
   state. While this might appeal to authors of actor classes, it has
   the serious problem of requiring dfx (and foreign) users to
   duplicate the special arguments as real canister arguments and
   separate configuration arguments, leading to confusion when these
   do not agree. To avoid this, one could strip the arguments at IDL
   export, and bind them on constructor entry using appropriate system calls (assuming
   these are available - probably not yet since they are expessed as Candid types).

   This approach seems too much of hack.

3. Extend actor class definition to introduce a second value, with a
   derived name `Configure<class-name>`.  This would be a curried constructor
   that take the explicit installation arguments and returns a
   specialzed constructor (with the usual signature) for instances of
   that class.
   Naive users (and simple examples) would continue to use the default class constructor;
   advanced users could resort to the `Configure<class-name>` constructor when necessary.

   For example, the declaration:

```
   class Counter(step : Nat) {
     var state = 0;
	 public shared inc() { state += step; };
   }
```
   would introduce one type and two values:

```
   type Counter = actor { inc: () -> () };
   func ConfigureCounter(
     mode: { #install;
             #upgrade : Principal;   // use actor {} instead?
			 #reinstall : Principal; // use Counter instead?
     },
     compute_allocation: ?Nat;
     memory_allocation: ?Nat)
	 (step : Nat) :
	 async Counter { ... }
   func Counter(step : Nat) : async Counter; // = ConfigureCounter(#install, null, null);
```

The `Principal` argument to `#upgrade` and `#reinstall` (passed
separately in the System API) allows the programmer to at least do (unsafe)
programmatic upgrades of the given principals.

Remark:

Attempting to make this interface safer, avoiding raw principals or `actor {}`, seems to require *lower-bounds* on type parameters (which we don't have, but might be able to encode).

Moreover, even if we had lower bounds, we would still not be verifying
compatibility of _stable_ state, which we don't yet track
in actor types and wouldn't be never be able to check for foreign actors
anyway.

```
   type Counter = actor { inc: () -> () };
   func InstallCounter(
     compute_allocation: ?Nat;
     memory_allocation: ?Nat)
	 (step : Nat) :
   fun ReinstallCounter(
     actor: Counter;
     compute_allocation: ?Nat;
     memory_allocation: ?Nat)
	 (step : Nat) :
	 async Counter;
   func Upgrade<A :> Counter> { // note the upper bound
     actor: A;
     compute_allocation: ?Nat;
     memory_allocation: ?Nat)
	 (step : Nat) :
	 async Counter;
   func Counter(step : Nat) : async Counter; // = #InstallCounter(null, null);
```


