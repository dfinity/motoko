# Structured awaits using indexed async types and parametric polymorphism

Inspired by Launchbury's RunST trick and Rust's lifetimes.

*Basic idea:*

Use type indexing and parametricity to ensure that a function can only
await async values it has created, hopefully ruling out:

* deadlock, and
* reply to wrong sender

while still allowing local uses of first-class async values.

Deadlock is prevented because an async can't await itself, since
it can only await asyncs it has created. Similarly, a sequence of
asyncs can only be awaited by the async that created them, not by each other.

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

cap(X,Ys) = AsyncCap<X>
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

Note that a local function may or may not introduce a scope parameter, affecting the capabilities of its body (and the ability to invoke that function).
This means that a local function *can* be used to abstract out async and sends, provided it has a scope parameter that is supplied when called.

### Application

Async and oneway functions:

Application of an async (or, in the full system, shared
oneway) uses the current scope `T` as the instantiation of the scope
parameter and is rejected when no such parameter exists (`scope(cap) = None`).



```
E; cap |- f : shared? <Xs> U -> V
Xs = X,Ys
scope(cap) = Some T
E; cap |- e': U[T/X,Ts/Ys]
-------------------------------------------------------
E; cap |-  f <Ts> e' : V[T/X,Ts/Ys]
```
One cannot specify the instantiation of an initial scope parameter, it
is determined by the context, if at all (as per async expressions).


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

Applying the sugar above, which implicitly introduces scope parameters in types and terms we get the more manageable:

```
E; _ |- shared? f<Ys>(x:T) : async U { e } :
         shared? <Ys>(x:T) : async U
```

(in which the scope parameters `X` are completely elided).



### DON'T READ TOO CLOSELY BELOW HERE, needs revising

## Examples:

(These examples and more are all coded in

* [general_await.mo](../test/run-drun/general_await.mo) (annotated with desugaring)
* [general_await_implicit.mo](../test/run-drun/general_await_implicit.mo) (sugar only)
)

Assuming the following requests:

```
  public shared func Ack() : async(){
    Prim.debugPrint "Ack"
  };

  public shared func Request(i : Int) : async Int {
    Prim.debugPrintInt(i);
    return i
  };

```

### Static parallel waiting:

```
  public shared func PA() : async () {
    let a1 = Ack();
    let a2 = Ack();
    await a1;
    await a2;
  };

  public shared func PR() : async (Int,Int) {
    let a1 = Request(1);
    let a2 = Request(2);
    (await a1, await a2)
  };
```

### Dynamic parallel waiting for acknowledgements

```
  // Dynamic parallel waiting for acknowledgements

  public shared func DPA() : async() {
   let os = Prim.Array_init<?(async ())>(10, null);
   for (i in os.keys()) {
     os[i] := ? (Ack());
   };
   for (o in os.vals()) {
     switch o {
      case (? a) await a;
      case null (assert false);
     };
   };
  };
```

### Dynamic parallel waiting (with results)

```
  public shared func DPR() : async [Int] {
    let os = Prim.Array_init<?(async Int)>(10, null);
    for (i in os.keys()) {
      os[i] := ? (Request(i));
    };
    let res = Prim.Array_init<Int>(os.len(),-1);
    for (i in os.keys()) {
      switch (os[i]) {
        case (? a) res[i] := await a;
        case null (assert false);
      };
    };
    Prim.Array_tabulate<Int>(res.len(),func i { res[i] })
  };
```

### Recursive parallel waiting

```
  public shared func RPA(n:Nat) : async () {
    if (n == 0) ()
    else {
      let a = Ack();
      await RPA(n-1); // recurse
      await a;
    };
  };

```

### Recursive parallel waiting (with results)

```
  public type List<Int> = ?(Int,List<Int>);

  public shared func RPR(n:Nat) : async List<Int> {
    if (n == 0) null
    else {
      let a = Request(n);
      let tl = await RPR(n-1); // recurse
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
  let t:async<$>U = async<$1>{ await t;}; // bad await since t : Async<$>U  </: Async<$1>U
```

Ruled out by index scoping (`$1 <> $ `)

#### Indirect deadlock

```
  async {
    let a1 : async () = async { await a2; };
    let a2 : async () = async { await a1; };
  };
```

is rejected because, once annotated:

```
  async<$> {
    let a1 : async<$> = async<$1> { await a2; }; // bad await since a2 : Async<$>() </: Async<$1>()
	let a2 : async<$> = async<$2> { await a1; }; // bad await since a1 : Async<$>() </: Async<$2>()
  }
```

since `$1 <> $` and `$2` \<\> `$`.

### Imperative deadlock

The informal example:

```
  async {
    var x = async { 0 };
    x := async {
      await x
    };
  }
```

that attempts to tie an imperative knot, is rejected by this system.

Explicitly, the outer and nested async would have distinct parameters `<$>` and `<$2>`, so the await at type `async<$2>Nat` on `x`
(of type `async<$>Nat` (with the outer parameter) would actually be illegal:

```
async<$> {
  var x : async<$> Nat = async<$1> 0;
  x := async<$2>{
    await x // illegal: this await requires async<$2>Nat (not async<$>Nat)
  };
}
```

## Sugar

Principle: Desugaring should be:

* simple and unambiguous
* expressible in the syntax (as explicit binders).
* avoidable (by supplying explicit binders).


### Basic idea:

(`$` is a new type identifier, reserved for scopes only, intially defined as 'Any')

Parsing:

* inserts `<$,...>` type binders for missing scope binders (in types and terms);
* adds missing `<$>` bindings to async returning functions with missing async indices.

Elaboration:

* Elaboration ensures `$` is bound to an appropriate constructor, shadowing any previous `$`-binding to ensure lexical scoping.

Syntactic sugar (during parse, applied bottom up as we construct types and terms)

```
(async T)^ := async<$> (T^)

(<...>T1 -> async T2)^ :=                           ($ not in ...)
  <$,...^>T1^ -> (async T2)^

(shared? f<...>(<pat>) : async T = e)^ :=           ($ not in ...)
  shared? f<$,...^>(<pat>^) : (async T)^ = e^

(shared? f<...>(<pat>) : async T { e })^ :=         ($ not in ...)
  shared? f<$,...^>(<pat>^) : (async T)^ = async<$> e^

(shared f<...>(<pat>) : () = e)^ :=           ($ not in ...)
  shared f<$,...^>(<pat>^) : () = e^

(shared f<...>(<pat>) { e })^ :=              ($ not in ...)
  shared f<$,...^>(<pat>^) : () =  e^


(async e)^ :=
  async<$> e^

```

### Elaboration

During elaboration, we rebind `$` to the current scope
identifier (aliasing `$` with some type parameter `X` if necessary) so
that:
 * references inserted during parsing elaborate to the nearest appropiate binding

Note that in a function type or definition with type parameters, `$`
either shadows one of those eponymous type parameters (if introduced by
de-sugaring) or it retains its outer meaning.

### Sugaring types (for pretty printing)

During pretty printing of types, we suppress a unary type binding in a
function type if it only occurs as an `async` type instantiation at
DeBruijn level 0, in which case we elide all those `async` type
instantiations too.

Binders with async instantiations at DeBruijn levels other than 0
cannot arise by construction (this is an invariant of desugaring and
the fact that we don't support explicit binding).


### Initial Context

For compiled programs we restrict the inital capability to `NullCap`,
so that sends and async can only occur in shared functions.

For interpreted programs we use the initial capability `Async $` so
that programs can `async` and `send` at top-level (but not `await`).

### Queries

In Motoko, expressions that `await` can also `throw/try-catch`, but
`query` functions are not allowed to spawn async expressions or send
message (but can return errors).

Query functions that may `throw/try/catch` but not send or `async` are
easily accomodated by:

* refining the notion of capabilities, addinq `QueryCap<c>` and
  `ErrorCap`, where `AwaitCap<C>` entails `ErrorCap`.
* making helper `cap` depend on the query modifer, returning
  `QueryCap<c>` for a query.
* conditionally transitioning the current capability accordingly when
  entering an `async` expression (from `QueryCap<T>` to `ErrorCap`
  rather than `AsyncCap<T>` to `AwaitCap<U>`)

See the code for details.


### Refinements

Since users may find it odd that we can instantiate the index at any
type, it might be better to define "type $ = Non" and always bound
index-parameters by `Non`. Then the top-level choice of index really
is unique since `Non`, and any `Non`-bounded type parameter, are the
only types bounded by `Non` and thus suitable for uses as index
instantiations.
