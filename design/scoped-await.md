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
    | async<T>U            // T is an index, typically a scope parameter X
    | shared? <Xs> T -> U  // function types (with optional scope parameter X)


Xs := X,Ys
   | Ys

Ys := Y, Ys

(type parameters are an optional scope parameter X followed by ordinary type parameters Y(s))

<exp> :=
    | async<X> e                                         // async (scope X bound in e)
	| shared? f<Xs>(x:T) : U = e                         // functions
    | f <Ts> e                                           // application
    | ...

Ctxt := E; Cap        // Environment and capability C

// capabilites

Cap :=
    | NullCap            // no async capability (top-level, in constructors, some local functions)
    | AsyncCap<T>        // capability to async/send at T
	| AwaitCap<T>        // capability to async/send at T and await an async<T>

Env :=                // the usual suspects
    | E, x : T          // term va
    | E, X              // scope var
	| E, Y              // type var
    | <emp>

scope(AsyncCap<T>) = Some T
scope(AwaitCap<T>) = Some T
scope(NullCap) = None

cap(@ X,Ys) = AsyncCap<X>
cap(Ys) = NullCap
```

### Parameterized async expressions

```
E, X; AwaitCap<X> |- e : U  (X not in E,cap)
scope(cap) = Some T
----------------------------------------------------------
E; cap |- async<X> e: async<T> U[T/X]
```

An `async` expression at index `X` provides the scoped capability, `AwaitCap<X>`, to await asyncs with index `X`, provided
the current capability is `AwaitCap<T>` or `AsyncCap<T>`. That is, async expressions are only allowed in async or await contexts,
and the body of async expressions can await, spawn an async or send a message (at scope `X`).

The body `e` of an async must be parametric in the index but the index parameter is immediately eliminated at the current scope `T`.

Async expressions are illegal if the current capability is `NullCap` (e.g. at top-level, or in a constructor or vanilla function).

### (restricted) Await expressions

```
E; AwaitCap<T> |- e : async<T> U
------------------------------
E; AwaitCap<T> |- await e : U
```

We can only await things of the current index `T`, recorded in the context as `_ ; AwaitCap<T>`.

In particular, you cannot await anything unless you've entered an async expression.

(For *closure under type substitution* of contexts, note that we need the annotation in the context to be a type, not just a type parameter).

### Formation

Function types are only well-formed if async and oneway functions have an initial scope type parameter (our sugar will ensure this).

```
E,Xs; cap |- T   E,Xs; cap(Xs) |- U
shared? = shared implies Xs = X,Ys and (U = () or U = async<V>W) ...
Xs = X,Ys and U = async<V>W implies V = X and
----------------------------------------------------[func ok]
E; cap |- shared? <Xs> T -> U
```
The first side condition ensures that a shared function has a scope parameter `X` (invokation requires a capability and scope instantiation).
The second side condition ensures that an async return is parametric in `X` (local or shared).

Note that a local function may or may not take a scope parameter.


### Abstraction

The rules for async/oneway function abstraction are as follows:

```
E,Xs,x:T; cap(Xs) |- e : U
E, cap |- shared? <Xs> T -> U
shared? = shared implies U = () or e == async<Y>e' (some e')
-----------------------------------------------------------------------------------------[func]
E; cap |- shared? f<Xs> (x:T) : U = e : shared? <X::Xs>T -> U
```

The second premise ensure the initial parameter of a oneway or async function must be a scope parameter `X` (see formation [func ok]).
In this way, regardless, of the current capability, the body of an (shared/local) async function or shared oneway
is granted the capability to enter an async expression and send messages.

Without a scope parameter in `Xs` (`Xs` = `Ys`), the abstraction rule simple introduces the null capability, `NullCap`, to prevent sends and async expressions in the body.

Not that a local function is may or may not introduce a scope parameter, affecting the capabilities of its body (and the ability to invoke that function).
This means that a local function *can* be used to abstract out async and sends, provided it has a scope parameter that is supplied when called.

### Application

Async and oneway functions:

Application of an async (or, in the full system, shared
oneway) uses the current scope `T` as the instantiation of the scope
parameter and is rejected when no such parameter exists (`scope(cap) = None`).

One cannot specify the instantiation of the initial scope parameter, it
is determined by the context if at all.


```
E; cap |- f : shared? <Xs> U -> V
Xs = X,Ys
scope(cap) = Some T
E; cap |- e': U[T/X,Ts/Ys]
-------------------------------------------------------
E; cap |-  f <Ts> e' : V[T/X,Ts/Ys]
```

(Local) Functions with no scope parameters are instantiated as usual and can be invoked with any capability.

```
E; cap |- f : <Ys> U -> V
E; cap |- e': U[Ts/Ys]
----------------------------------------------
E; cap |-  f <Ts> e' : V[Ts/Ys]
```

By construction, shared functions must have a scope parameter and can never be invoked in a `NullCap` context.

For local functions, it depends on the type of the function (i.e. whether it has a scope parameter).


### Abstraction (Derived Rule)

Derivation for (desugared) shared functions (ignoring recursion).


Consider the desugared async function:

```
shared? f<Ys>(X:T) : async U { e } :=
shared? f<X,Ys>(x:T) : async<X>U = async<X> e;
```

For reusability in different async contexts, every shared function
should introduce a new index parameter, implicitly supplied to the
inner async expression.  The result type needs to
be generic if we want to invoke and await it from other async contexts.

Using the above rules we get:

```
E, X, Ys, x : T, Y;  AwaitCap<X> |- e : U
-------------------------------------------------------------
E, X, Ys, x : T; AsyncCap<X> |- async <X> e
-------------------------------------------------------------
E; _ |- shared? f<X,Ys>(x:T) : async<X>U = async<Y> e :
          shared? <X,Ys>(x:T) : async<X>U
```
Notice that the context transitions from `AsyncCap<X>` to `AwaitCap<X>` to allow awaits from within `e`.

Applying the sugar below, which implicitly introduces scope parameters in types and terms we get the more manageable:

```
E; _ |- shared? f<Ys>(x:T) : async U { e } :
         shared? <Ys>(x:T) : async U
```

(in which the scope parameters `X` are completely elided).



### DON'T READ TOO CLOSELY BELOW HERE, needs revising

## Examples:

(These examples and more are all coded in

* [general_await.mo](../test/run-drun/general_await.mo) (no-sugar)
* [general_await_implicit.mo](../test/run-drun/general_await_implicit.mo) (with sugar)
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

### Static parallel waiting:

```
  public shared func PA<@>() : async<@> () {
    let a1 = Ack();
    let a2 = Ack();
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
* expressible in the syntax (as explicit binders).
* avoidable (by supplying explicit binders).


### Basic idea:

(`@` is a new type identifier, used for scopes)

Parsing:

* inserts `<@,...>` type binders for missing scope binders (in types and terms);
* adds missing `<@>` bindings to async returning functions with missing async indices.

Elaboration:

* Elaboration ensures `@` is bound to an appropriate constructor, shadowing any previous `@`-binding to ensure lexical scoping.

Syntactic sugar (during parse, applied bottom up as we construct types and terms)

```
(async T)^ := async<@> (T^)

(<...>T1 -> async T2)^ :=                           (@ not in ...)
  <@,...^>T1^ -> (async T2)^

(shared? f<...>(<pat>) : async T = e)^ :=           (@ not in ...)
  shared? f<@,...^>(<pat>^) : (async T)^ = e^

(shared? f<...>(<pat>) : async T { e })^ :=         (@ not in ...)
  shared? f<@,...^>(<pat>^) : (async T)^ = async<@> e^

(shared? f<...>(<pat>) : async<X> T { e }) :=
  shared? f<...^>(<pat>^) : async<X> T^ = async<X> e ^

  (binds inner scope as X, not @, to pun X in e)

(shared f<...>(<pat>) : () = e)^ :=           (@ not in ...)
  shared f<@,...^>(<pat>^) : () = e^

(shared f<...>(<pat>) { e })^ :=              (@ not in ...)
  shared f<@,...^>(<pat>^) : () =  e^


(async e)^ :=
  async<@> e^

```

Syntax elaboration (applied during type checking)

```
Types:

@* ~~>
  @*

  (* interprets @ at its current binding (which may be some X) *)

(async t)* ~~>
  async<@*> t*

(async<t1> t2)*
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
index-parameters by `Non`. Then the top-level choice of index really is unique
since `Non`, and any `Non`-bounded type parameter, are the only types
bounded by `Non` and thus suitable for uses as index instantiations.
