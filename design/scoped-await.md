# Structured awaits using indexed async types and parametric polymorphism

Inspired by Launchbury's RunST trick and Rust's lifetimes.

*Basic idea:*

Use type indexing and parametricity to ensure that a function can only
await async values it has created, hopefully ruling out:

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

Application must provide an instantiation, typically the nearest
enclosing index parameter if want to await the result, but it could be
any type.

In the implicit system, we change the rule for application to (if
necessary) insert a single missing scope parameter (`@`) amongst the
remaining explicit type parameters, driven by the expected async
returning function type and the 1-deficit of type arguments. This is a
simple check to see if the index of the return type is a type
parameter and the number of arguments is 1 less than the number of
parameters, and then inserting '@' at the correct index in the type
arguments (otherwise the arguments are as given).


### Abstraction (Derived Rule)

Derivation for (desugared) shared functions (ignoring recursion).


Consider the desugared async function:

```
shared f<X>(x:T) : async<X>U = async<Y> e <X>;
```

For reusability in different async contexts, every shared function
should introduce a new index parameter, immediately supplied to the
inner async expression.  Using non-generic shared functions is ok, but
less useful (typically only locally useful): the result type needs to
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

(These examples and more are all coded in

* [general_await.mo](../test/run-stub/general_await.mo) (no-sugar)
* [general_await_implicit.mo](../test/run-stub/general_await_implicit.mo) (with sugar)
)

Assuming the following requests:

```
  public shared func Ack<@>() : async<@> (){
    debugPrint "Ack"
  };

  public shared func Request<@>(i : Int) : async<@> Int {
    debugPrint("Request(" # debug_show i # ")");
    return i;
  };
```

### Static parralel waiting:

```
  public shared func PA<@>() : async<@> () {
    let a1 = Ack<@>();
    let a2 = Ack<@>();
    await a1;
    await a2;
  };
```

### Dynamic parallel waiting for acknowledgements

```
  // Dynamic parallel waiting for acknowledgements

  public shared func DPA<@>() : async<@>() {
    let as: [async()]  = Array_tabulate<async ()>(N, func (_) { Ack<@>(); });
    for (a in as.vals()) {
      await a;
    };
  };
```

### Dynamic parallel waiting (with results)

```
  // Dynamic parallel waiting (with results)

  public shared func DPR<@>() : async<@>[Int] {
    func f<>(i:Nat) : async Int = Request<@>(i);
    let as = Array_tabulate<async Int>(N, f);
    let res = Array_init<Int>(as.len(),-1);
    for (i in as.keys()) {
      res[i] := (await as[i]);
    };
    Array_tabulate<Int>(as.len(),func i = res[i])
  };
```

### Recursive parallel waiting

```
  public shared func RPA<@>(n:Nat) : async<@>() {
    if (n == 0) ()
    else {
      let a = Ack<@>();
      await RPA<@>(n-1); // recurse
      await a;
    };
  };
```

### Recursive parallel waiting (with results)

```
  public type List<Int> = ?(Int,List<Int>);

  public shared func RPR<@>(n:Nat) : async<@> List<Int> {
    if (n == 0) null
    else {
      let a = Request<@>(n);
      let tl = await RPR<@>(n-1); // recurse
      ?(await a,tl)
    }
  };
```

### Deadlock Prevention:

(These examples are all coded in [illegal-await.mo](../test/fail/illegal-await.mo)

#### Immediate deadlock

```
  let t : async () = async { await t};
```
is rejected because, once annotated:

```
  let t:async<@>U = async<X>{ await t;} <@>; // bad await since t : Async<@>U  </: Async<X>U
```

Ruled out by index scoping (`X != @`, any `@`)

#### Indirect deadlock

```
  async {
    let a1 : async () = async { await a2; }; // illegal await since a1 : Async<X>() </: Async<Y>()
    let a2 : async () = async { await a1; }; // illegal await since a2 : Async<X>() </: Async<Z>()
  };
```

is rejected because, once annotated:

```
  async<X> {
    let a1 : async<X> = async<Y> { await a2; }<X>; // bad await since a1 : Async<X>() </: Async<Y>()
    let a2 : async<X> = async<Z> { await a1; }<X>; // bad await since a2 : Async<X>() </: Async<Z>()
  }<@>
```

since `X != Y,Z`, any `Y,Z`.

### Imperative deadlock

The informal example:

```
  async {
    var x = async { 0 };
    x := (async {
      await x // illegal: await _ : async<S>T -> T (not async<R> T -> T) (any T))
    });
  }
```

that attempts to tie an imperative knot, is rejected by this system.

Explicitly, the outer and nested async would have distinct parameters `<R>` and `<S>`, so the await for type `async<S>Nat` on `x` (of type async<R>Nat (with the outer parameter) would actually be illegal:

```
async<R> {
  var x : async<R> Nat = async<_> 0 <R>;
  x := async<S>{
    await x // illegal: await _ : async<S>T -> T (not async<R> T -> T) (any T)
  } <R>;
}<@>
```

Note that simply renaming `S` to `R` would not circumvent the error.)

## Sugar

Principle: Desugaring should be:

* simple and unambiguous
* expressible in the syntax (as explicit binders and instantiations).
* avoidable (by supplying explicit binders and instantations).


### Basic idea:

Prelude:

Defines default scope `@ = Any` (non-awaitable in any async context) and instantiation:

```
type @ = Any
```

(`@` is a newly legal scope identifier)

Parsing:

* inserts `<@>` type binders for missing `async` binders and instantiations (in types and terms) and `async` binders (in terms);
* adds missing `<@>` bindings to async returning functions with missing async indices.

Elaboration:

* Elaboration ensures `@` is bound to an appropriate constructor, shadowing any previous `@`-binding to ensure lexical scoping.
* Elaboration adds missing scope instantiations to function applications that require them, guided by the synthesized function type.

Syntactic sugar (during parse, applied bottom up as we construct types and terms)

```
<...>T1 -> async T2 :=                         (@ not in ...)
  <@,...>T1 -> async T1

func f<...>(<pat>) : async T = e :=           (@ not in ...)
  func f<@,...> f(<pat>) : async T = e

func f<...>(<pat>) : async T { e } :=
  func f<@,...>(<pat>) : async T = async<@> e <@>

func f<...>(<pat>) : async<X> T { e } :=
  func f<...>(<pat>) : async<X> T = async <X> e <X>

  (binds inner scope as X, not @, to pun X in e)

func f<...>(<pat>) : async<U> T { e } :=      (U <> X)
  func f<...>(<pat>) : async<U> T = async <@> e <U>

async e :=
  async<@> e <@>

```

Syntax elaboration (applied during type checking)

```
Types:

@* ~~>
  @*

  (* interprets @ at its current binding (which may be some X) *)

(async t)* ~~>
  async<@*> t*

(async<t1> t1)*
  ~~> async<t1*> t2*

<...>T1 -> async T2 ~~>                        (* syntax sugaring ensures @ bound in ... *)
  <...>T1* -> async<@*> T2*

<...,X,...>T1 -> async<X> T2 ~~>
  <...*,X,...*>T1*[X/@] -> async<X> T2*[X/@],  (* rebind @ to X *)

<...>T1 -> async<U> T2 ~~>  (* U <> X in ... *)
  <...*>T1* -> async<U*> T2*

Terms:

async <X> e <U> ~~> async <X> e[X/@] <U>

f <Ts1,Ts2> e ~~>
 f <Ts1*,@,Ts2*> e when f : <...,Xi,...> T1 ~~> async<Xi> T1

 (note inference can infer a missing scope parameter at any position, not just position 0)

func<...>f() : async T = e ~~>               (* syntax sugaring ensures @ bound in ... *)
 func<...*>f() : (async T)* = e*

func<...,X,...>f() : async<X> T = e          (* rebind @ to X *)
 func<...*,X,...*>f() : (async<X> T)*[X/@] = e*[X/@]

func<...>f() : async<U> T = e ~~>            (* U <> X in ... *)
 func<...*>f() : async<U*> T* = e*

```

Basically, during elaboration, we rebind `@` to the current scope
identifier (aliasing `@` with some type parameter `X` if necessary) so
that:
 * references inserted during parsing elaborate to the nearest appropiate binding
 * missing scope instantiations default to the current binding of `@`.

Note that in a function type or definition with n>1 type parameters,
`@` either shadows one of those eponymous type parameters or it
retains its outer meaning.  In the latter case (outer binding), we
might either warn appropriately or reject as ambiguous, requiring the
user to give the missing instantiation of the `async T` return type.

(The implementation is currently silent and defaults `@` to the enclosing interpretation.)

### Sugaring types (for pretty printing)

During pretty printing of types, we suppress a unary type binding in a
function type if it only occurs as an `async` type instantiation at
DeBruijn level 0, in which case we elide all those `async` type
instantiations too.

Binders with async instantiations at DeBruijn levels other than 0 must
be shown to avoid ambiguity, in particular so that the user knows
which parameter is the scope parameter if explicit (not implicit)
instantiation is desired.

### Refinements

Since users may find it odd that we can instantiate the index at any
type, it might be better to define "type @ = Non" and always bound
index-parameters by `Non`. Then the top-level choice really is unique
since `Non` (and any `Non`-bounded type parameter) ares the only type
bounded by `Non` and thus suitable for uses as index instantiations.
