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
continuations that are stored (including the ultimate reply
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
E; - |- async<X> e <U>: async<U> T[U/X]
```

An `async` expression at index `X` provides the scoped ability to await asyncs with index `X`.

The body `e` of an async must be parametric in the index but the index
parameter is immediately eliminated at some index `U` (not mentioning `X`).

### (restricted) await expressions

```
E; async<U> |- e : async<U> T
------------------------------
E; async<U> |- await e : T
```

We can only await things of the current index `U`, recorded in the context as `_ ; async<U>`.

(For *closure under type substitution* of contexts, note that we need the annotation in the context to be a type, not just a type parameter).


### Application (Derived Rule)

In the explicit system, the rules for function abstraction and application are *unchanged*. But
to illustrate how this works, we consider the derived rules:


```
E; _ |- f : shared <X> U -> async<U> V
E; _ |- e': [T/X] U
-------------------------------------
E; _ |-  f <T> e' : async<T>([T/X]V)
```

Application must provide an instantiation, typically the nearest enclosing index parameter if want to await the result, but it
could be any type.

In the implicit system, we change the rule for application to (if necessary) insert a single missing scope parameter (`@`)
amongst the remaining explicit type parameters, driven by the expected async returning function type
and the 1-deficit of type arguments. This is a simple check to see if the index of the return type is a type parameter and the number of arguments is 1 less than the number of parameters,
and then inserting '@' at the correct index in the type arguments (otherwise the arguments are as given).


### Abstraction (Derived Rule)

Derivation for (desugared) shared functions (ignoring recursion).


Consider the desugared async function:

```shared f<X>(x:T) : async<X>U = async<Y> e <X>;```

For reusability in different async contexts,
every shared function should introduce a new index parameter, immediately supplied to the inner async expression.
Using non-generic shared functions is ok, but less useful (typically only locally useful): the result type needs to
be generic if we want to await it from other async contexts.

Using the above rules and ordinary lambda abstraction we get:

```
E, X, x : T, Y;  async<Y> |- e : U[Y]  Y fresh
-------------------------------------------------------------
E, X, x : T |- async <Y> e <X> : U[Y] : (U[Y])[X/Y]  X fresh
-------------------------------------------------------------
E; _ |- shared f<X>(x:T) : async<X>U[X] = async<Y> e <X>;
        shared <X>(x:T) : async<X>U[X]
```


If we explicitly pun `X` and `Y` in our *implicit* syntax desugaring we get:

```
shared f(x:T) : async U :=
  shared f<X>(x:T) : async<X>U = async<X> e <X>;

```

Then we get the derived rule:

```
E, X, x : T; async<X> |- e : U  (X fresh)
----------------------------------------------------------------------
----------------------------------------------------------------------
E; _ |- shared f(x:T) : async U { e; } : shared <X> T -> async<X> U
```

## Examples:

(These need fixing for typos below but are all coded in

* [general_await.mo](../test/run-stub/general_await.mo) (no-sugar)
* [general_await_implicit.mo](../test/run-stub/general_await_implicit.mo) (with sugar)
)

Assuming the following requests:

```
shared Ack<X>() : async<X>(){ };

shared Request<X>(i : Int) : async<X> Int { return i; }
```

### Static parralel waiting:

```
async<X> {
  let a1 = Ack<X>();
  let a2 = Ack<X>();
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

(These need fixing but are all correctly coded in [illegal-await.mo](../test/fail/illegal-await.mo)

#### Immediate deadlock

```
let t:async<T>U = async<X>{ await t;}<T>; // bad await since t : Async<T>U  </: Async<X>U
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

## Sugar

Principle: Desugaring should be:

* simple and unambiguous
* expressible in the syntax (as explicit binders and instantiations).
* avoidable (by supplying explicit binders and instantations).

### Basic idea:

Prelude:

Defines default scope @ = Any (non-awaitable in any async context) and instantiation:

```
type @ = Any
```

(`@` is a newly legal scope identifier)

Parsing:

* inserts `<@>` type instantiations and binders for missing `async` binders and instantiations (in types and terms) and `async` binders (in terms)
* adds missing `<@>` bindings to async returning functions with absent quantifiers (note we distinguish missing type parameters from empty parameters `<>`)

Elaboration:

* Elaboration ensures `@` is bound to appropriate constructor, shadowing any previous `@`-binding to ensure structured scoping.
* Elaboration adds missing unary instantiations to function applications that require them, guided by the synthesized function type.

syntactic sugar (during parse, applied bottom up as we construct types and terms)


```
async T := async<@> T
T1 -> async<@> T2 := <@>T1 -> async<@> T1

func f(<pat>) : async<@> T = e :=
func<@>f(<pat>) : async<@> T = e

func f(<pat>) : async<@> T { e } :=
func<@>f(<pat>) : async<@> T = async<@> e <@>

func f<X>(<pat>) : async<@> T { e } :=
func<X>f(<pat>) : async<@> T = async <X> e <@>

f(<pat>) : async<@> T {e} := f<@>(<pat>): async<@> T = async <@> e <@>
f<X>(<pat>) : async<@> T := e = f<@>(<pat>): async<@> T = e

async e := async<@> e <@>

```

static sugar (during elaboration)

```
<X<:T0>T1 -> async<@> T2 ~~> <X<:T0> T1[X/@] -> async <X> T[X/@]
<XsTs>T1 -> async<U> T2 ~~> LHS (|tbs| <> 1)


async <X> e <U> ~~> async <X> e[X/@] <U>
f e = f<@>e when f: sort <X>(typ,...) ~~> async<X> t

func<X>f() : async<@> T = async <X> e <@> ~~>
func<X>f() : async<@> T[X/@] = (async<X> e <@>) [X/@]
````

(basically, we rebind `@` to the current scope during elaboration, so references inserted during parsing elaborate to the nearest appropiate binding, and default missing scope instantiations to the current meaning of `@`).

Note that in a function type or definition with n>1 type parameters, `<@>` either shadows one of those eponymous type parameters or it retains its outer meaning. In the latter case (outer binding), we might either warn appropriately or reject as ambiguous, requiring the user to
give the missing instantiation of the `async T` return type.

(The implementation is currently silent and defaults `@` to the enclosing interpretation.)

### Sugaring types (for pretty printing)

During pretty printing of types, we suppress a unary type binding in a function type if it only occurs as an `async` type instantiation at DeBruijn level 0, in which case we elide all those `async` type instantiations too. Binders with async instantiations at DeBruijn levels higher than 0 must be shown to avoid ambiguity.
