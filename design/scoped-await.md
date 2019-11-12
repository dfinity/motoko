# Structured awaits using indexed async types and parametric polymorphism


# Abstract Syntax

```bnf
<typ> := ...
    | async[T]U          // T is an index, typically a type parameter

<exp> :=
    | async<X> e <U>                                  // index abstraction plus application in one
	| shared f<X>(x:T) : async[X]U = async<Y> e <X>;  // requests
	| shared f<X>(x:T) : async[X]U { e; }             // sugar (for the above)
	| f <T> e                                         // (indexed) application
```

### Parameterized async expressions

```
E,X; async[X] |- e : T
--------------------------------------
E; async[U] |- async<X> e <T>: async[U]T
```
An `async` expression at index `X` provides the scoped ability to await asyncs with index `X`.

The body 'e' of an async must be parametric in the index but the index parameter is immediately eliminated at any index `U`.

### (restricted) await expressions
```
E; async[T] |- e : async[T]U
------------------------------
E; async[T] |- await e : U
```

We can only await things of the current index `T`, recorded in the context as `_ ; async[T]`.

### Application

```
E; _ |- f : shared <X> U -> async[X] V
E; _ |- e': [T/X] U
-------------------------------------
E; _ |-  f <T> e' : async[T]([T/X]V)
```

Application must provide an index, typically the nearest enclosing index parameter if want to await the result, but I guess
it could be any enclosing index parameter.


### Abstraction

Rule for (desugared) shared functions:

```
E, X, x : T, Y;  async<Y> |- e : U
----------------------------------------------------
E; _ |- shared f<X>(x:T) : async[X]U = async<Y> e <X>;
```
Every shared function introduce a new index parameter, immediately supplied to the inner async expression.

Derived rule for sugar:

```
E, X, x : T; async<X> |- e : U
--------------------------------------------
E; _ |- shared f<X>(x:T) : async[X] U { e; }
```

## Examples:

Assuming the following requests:

```
shared Request<X>() : async[X](){
}

shared Echo<X>(i : Int) : async[X] Int { return i; }
```


### Static parralel waiting:
```
async<X> {
  let a1 = Request<X>();
  let a2 = Request<X>()
  await(a1);
  await(a2);
}<Any>;
```

### Dynamic parallel waiting for acknowledgements

```
async<X> {
  let as = Array_tabulate<Async<X>()>(func _ { Request<X>(); });
  for (a in as.key) {
   await(a2);
  };
}<Any>;
```

### Dynamic parallel waiting (with results)

```
async<X> {
  let as = Array_tabulate<Async<X>()>(func _ { Request<X>(); });
  let res = Array_init<>(as.len,-1);
  rs = for (i in as.keys()) {
    res[i] := await(a2);
  };
  res;
}<Any>;
```

### Recursive parallel waiting

```
shared func waitN<X>(n:Nat) : async[X]() {
  if (n = 0)
    ()
  else {
    let a = Request<X>();
    await waitN<X>(n-1);
	await<X>(a);
  };
}<Any>;
```
### Recursive parallel waiting (with results)

```
shared func waitN<X>(n:Nat) : async[X](List<Int>) {
  if (n = 0)
    List.null<Int>
  else {
    let a = Echo<X>(n);
    let tl = await waitN<X>(n-1);
	List.cons<Int>(await<X>(a),tl);
  };
}<Any>;
```
### Deadlock Prevention:

### Immediate deadlock

```
let t:async[T]U = async<X>{ await t;}<T>; // bad await since t : Async[Any]U  </: Async[X]U
```
Ruled out by index scoping (`X != T`, any `T`)

###

Indirect deadlock

```
async<X> {
  let a1 = async<Y>{ await a2; }<X>; // bad await since a1 : Async[X]() </: Async<Y>()
  let a2 = async<Z>{ await a1; }<X>; // bad await since a2 : Async[X]() </: Async<Z>()
  await(a1);
}<Any>;
```
Ruled out by index scoping (`X != Y,Z`, any `Y,Z`)
