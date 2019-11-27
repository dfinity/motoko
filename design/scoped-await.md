# Structured awaits using indexed async types and parametric polymorphism

Inspired by Launchbury's RunST trick and Rust's lifetimes.

*Basic idea:*

Use type indexing and parametricity to ensure
that a function can only await async
values it has created, hopefully ruling out:

* deadlock, and
* reply to wrong sender

while still allowing local uses of first-class async values.

Deadlock is (hopefully) prevented because an async can't await itself, since
it can only await asyncs it has created. Similarly, a sequence of
asyncs can only be awaited by the async that created them, not by each other.
That's the hope anyway.

The idea behind ruling out *reply-to-wrong-sender* is that an async
value cannot be stored in a non-local var or other mutable
datastructure, due to the fresh index, so it is only accessible from
the current message and thus can only receive continuations (via `await`) from the
current message.  It can't receive continuations from an inner
function by escaping into that function, since the inner function can at
most await asyncs with its inner index, not the outer one. Thus the only
continations that are stored (including the ultimate reply
continuation), must be from the current function.

_Please break it_


# Abstract Syntax

```bnf
<typ> := ...
    | async<T>U          // T is an index, typically a type parameter

<exp> :=
    | async<X> e <U>                                  // index abstraction plus application in one (parameter X free in e, but not in instantiation U)
    | shared f<X>(x:T) : async<X>U = async<Y> e <X>;  // requests
    | shared f<X>(x:T) : async<X>U { e; }             // sugar (for the above)
    | f <T> e                                         // (indexed) application

Ctxt := E; async<T>   // async context with index U
    |  E; -           // non-async context

Env :=                // the usual suspects
    | E, x : T
    | E, X <: T
    | <emp>
```

# Concrete Syntax

TBD, but hopefully the explicit parameterization/instantiation can be supressed, much as Rust *lifetimes* are largely implicit.
Here, the simplist might be to use the function name itself for the implicit index parameter
(although not all motoko functions are named) and always instantiate with the ambient index (or `Any` if none).

### Parameterized async expressions

```
E, X; async<X> |- e : T   E |- U :: *   (X fresh)
-------------------------------------------------
E; - |- async<X> e <U>: async[U]T
```
An `async` expression at index `X` provides the scoped ability to await asyncs with index `X`.

The body `e` of an async must be parametric in the index but the index
parameter is immediately eliminated at some index `U` (not mentioning `X`).

### (restricted) await expressions

```
E; async<T> |- e : async<T>U
------------------------------
E; async<T> |- await e : U
```

We can only await things of the current index `T`, recorded in the context as `_ ; async<T>`.

### Application

```
E; _ |- f : shared <X> U -> async<X> V
E; _ |- e': [T/X] U
-------------------------------------
E; _ |-  f <T> e' : async<T>([T/X]V)
```

Application must provide an index, typically the nearest enclosing index parameter if want to await the result, but I guess
it could be any enclosing index parameter.


### Abstraction

Rule for (desugared) shared functions (ignoring recursion).

```
E, X, x : T, Y;  async<Y> |- e : U  (X, Y fresh)
----------------------------------------------------
E; _ |- shared f<X>(x:T) : async<X>U = async<Y> e <X>;
```
Every shared function introduce a new index parameter, immediately supplied to the inner async expression.

Derived rule for sugar (ignoring recursion):

```
E, X, x : T; async<X> |- e : U
--------------------------------------------
E; _ |- shared f<X>(x:T) : async<X> U { e; }
```

## Examples:

Assuming the following requests:

```
shared Ack<X>() : async<X>(){ };

shared Request<X>(i : Int) : async<X> Int { return i; }
```

### Static parralel waiting:
```
async<X> {
  let a1 = Ack<X>();
  let a2 = Ack<X>()
  await(a1);
  await(a2);
}<Any>;
```

### Dynamic parallel waiting for acknowledgements

```
async<X> {
  let as = Array_tabulate<Async<X>()>(func _ { Ack<X>(); });
  for (a in as.key) {
    await(a2);
  };
}<Any>;
```

### Dynamic parallel waiting (with results)

```
async<X> {
  let as = Array_tabulate<Async<X>()>(func _ { Ack<X>(); });
  let res = Array_init<>(as.len,-1);
  rs = for (i in as.keys()) {
    res[i] := await(a2);
  };
  res;
}<Any>;
```

### Recursive parallel waiting

```
shared func waitN<X>(n:Nat) : async<X>() {
  if (n = 0)
    ()
  else {
    let a = Ack<X>();
    await waitN<X>(n-1); // recurse
    await<X>(a);
  };
}<Any>;
```

### Recursive parallel waiting (with results)

```
shared func waitN<X>(n:Nat) : async<X>(List<Int>) {
  if (n = 0)
    List.null<Int>();
  else {
    let a = Request<X>(n);
    let tl = await waitN<X>(n-1);
    List.cons<Int>(await<X>(a),tl);
  };
}<Any>;
```

### Deadlock Prevention:

#### Immediate deadlock


```
let t:async<T>U = async<X>{ await t;}<T>; // bad await since t : Async[Any]U  </: Async<X>U
```

Ruled out by index scoping (`X != T`, any `T`)

#### Indirect deadlock

```
async<X> {
  let a1 = async<Y>{ await a2; }<X>; // bad await since a1 : Async<X>() </: Async<Y>()
  let a2 = async<Z>{ await a1; }<X>; // bad await since a2 : Async<X>() </: Async<Z>()
  await(a1);
}<Any>;
```

Ruled out by index scoping (`X != Y,Z`, any `Y,Z`)

### Imperative deadlock

The informal example:
```
shared func f() : async () {
  var x : async Nat = async 0;
  x := async {
    await x
  };
}
```

is rejected by this system:

Explicitly, the shared function and nested async would have distinct indices,so the await for type `async<S>Nat` on `x` (of type async<R>Nat (with the outer parameter) would actually be illegal:

```
shared func f<R>() : async () {
  var x : async<R> Nat = async<_> 0 <R>;
  x := async<S>{
    await x // illegal: await _ : async<S>T -> T (not async<R> T -> T) (any T)
  } <R>;
}
```


syntactic sugar:

```
async typ := async<@> typ

(typ,...) -> async<@> typ -> :=
<@>(typ,...) -> async<@> typ

func f(<pat>) : async<@> t = e
func<@>f() : async<@>t = e

func f(<pat>) : async<@> t { e } :-
func<@>f() : async<@>t { e } :-

x(pat) : async t {e}  = x<@>(pat): async<@> t
x(pat) : async t = e  = x<@>(pat): async<@> t

async e := async<@> e <@>
```

static sugar
```
f e = f<@>e when f: sort <X>(typ,...) -> async<X> t
```
