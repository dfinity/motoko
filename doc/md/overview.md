# Concise overview of Motoko

This is terse, slide-like introduction to Motoko and its features.

(For a gentler introduction, visit the other sections on this site.)

## Motivation and Goals

A simple, useful language for the Internet Computer (IC)

-   Familiar syntax

-   Safe by default

-   Incorporating **actor** model for canister smart contracts

-   Seamless integration of IC features

-   Making most of present and future WebAssembly

## Key Design Points

-   Object-oriented, functional & imperative

-   Objects as records of functions

-   `async`/`await` for sequential programming of asynchronous messaging

-   Structural typing with simple generics and subtyping

-   Safe arithmetic (both unbounded and checked)

-   Non-nullable types by default

-   Garbage collected (no manual memory management)

-   JavaScript-like syntax but statically typed & sane

Inspirations: Java, JavaScript, C#, Swift, Pony, ML, Haskell

## Semantics

-   call-by-value (like Java, C, JS, ML; unlike Haskell)

-   declarations are locally mutually recursive

-   parametric, bounded polymorphism

-   subtyping as zero-cost subsumption, not coercion

-   no dynamic casts

-   no inheritance

## Implementation(s)

-   implemented in OCaml (leverages `wasm` libary)

-   simple reference interpreter

-   less simple compiler to WebAssembly

    -   multipass with typed IR in each pass.

    -   uniform representation, unboxed arithmetic

    -   two-space GC or mark-compact GC, invoked after messages (for now)

-   polymorphism by erasure

# The language

## Expressions

-   Identifiers:  
    `x`, `foo_bar`, `test123`, `List`, `Map`

-   Parentheses `( … )` for grouping

-   Braces `{ … }` for scoping (and records)

-   `;` for sequencing

-   Type annotations (to help type inference):  
    `(42 : Int)`  
    (zero cost)

## Libraries

``` motoko name=impDebugInt
  import Debug "mo:base/Debug";
  import Int "mo:base/Int";
```

(`import MyLib "src/MyLib"` imports a library from the local file system.)

Specific bindings can be imported from the module using object patterns

      import { cons; nil } = "mo:base/List";

## Libraries

``` motoko
  import Debug "mo:base/Debug";
  import Int "mo:base/Int";
  import Trie "mo:base/Trie";

  type Users = Trie.Trie<Text, Nat>; // reference types

  Debug.print(Int.toText(7)); // reference functions/values
```

## Blocks and declarations

``` motoko include=impDebugInt
  type Delta = Nat;
  func print() {
    Debug.print(Int.toText(counter));
  };
  let d : Delta = 42;
  var counter = 1;
  counter := counter + d;
  print();
```

-   Semicolon after each declaration!

-   Mutually recursive

-   Mutable variables marked explicitly

## Control flow

The usual suspects…​

-   `do { … }`

-   `if b …`

-   `if b … else …`

-   `switch e { case pat1 e1; …; case _ en }`

-   `while b …`

-   `loop …`

-   `loop …  while b`

-   `for (pat in e) …`

-   `return`, `return e`

-   `label l e`, `break l e`

-   `do ? { … e! … }`

-   `async e`, `await e` *(restricted)*

-   `throw`, `try … catch x { … }` *(restricted)*

# Primitive types

## Unbounded integers

`Int`

`{ …​, -2, 1, 0, 1, 2, …​ }`

Inferred by default for negative literals.

Literals: `13`, `0xf4`, `-20`, `+1`, `1_000_000`

## Unbounded naturals

`Nat`

`{ 0, 1, 2, …​ }`

Non-negative, trap on underflow.

Inferred by default for non-negative literals

Literals: `13`, `0xf4`, `1_000_000`

`Nat <: Int`

`Nat` is a *subtype* of `Int`

(you can supply a `Nat` wherever an `Int` is expected)

## Bounded numbers (trapping)

`Nat8`, `Nat16`, `Nat32`, `Nat64`, `Int8`, `Int16`, `Int32`, `Int64`

Trap on over- and underflow; wrap-around and bit-manipulating operations available separately

Needs type annotations (somewhere)

Literals: `13`, `0xf4`, `-20`, `1_000_000`

## Floating point numbers

`Float`

IEEE 754 double precision (64 bit) semantics, normalized NaN

Inferred for fractional literals

Literals: 0, -10, `2.71`, `-0.3e+15`, `3.141_592_653_589_793_12`

## Numeric operations

No surprises here

`- x`  
`a + b`  
`a % b`  
`a & b`  
`a << b`  
…

`a +% b, a -% b, …` for wrapping, modular arithmetic (where appropriate)

## Characters and Text

`Char`, `Text`

Unicode! Character = Unicode scalar value; no random access on text

-   `'x'`, `'\u{6a}'`, `'☃'`,

-   `"boo"`, `"foo \u{62}ar ☃"`

-   `"Concat" # "enation"`

## Booleans

`Bool`

Literals: `true`, `false`

`a or b`  
`a and b`  
`not b`  
`if (b) e1 else e2`

# Functions

## Function types

-   Simple functions:

    ``` motoko no-repl
    Int.toText : Int -> Text
    ```

-   multiple arguments and return values

    ``` motoko no-repl
    divRem : (Int, Int) -> (Int, Int)
    ```

-   can be generic/polymorphic

    ``` motoko no-repl
    Option.unwrapOr : <T>(?T, default : T) -> T
    ```

-   first-class (can be passed around, stored)

    ``` motoko no-repl
    map : <A, B>(f : A -> B, xs : [A]) -> [B]
    let funcs : [<T>(T) -> T] = …
    ```

## Function Declarations & Use

``` motoko include=impDebugInt
func add(x : Int, y : Int) : Int = x + y;

func applyNTimes<T>(n : Int, x : T, f : T -> ()) {
  if (n <= 0) return;
  f(x);
  applyNTimes(n-1, x, f);
};

applyNTimes<Text>(3, "Hello!", func(x) { Debug.print(x) } );
```

-   `func() { … }` short for `func() : () = { … }`

-   Parametric functions

-   Type instantiations may sometimes be omitted

-   Anonymous functions (a.k.a. lambdas)

# Composite types

## Tuples

`(Bool, Float, Text)`

immutable, heterogeneous, fixed size

``` motoko name=tuple
let tuple = (true or false, 0.6 * 2.0, "foo" # "bar");
```

``` motoko include=tuple
tuple.1;
```

``` motoko include=tuple
let (_,_,t) = tuple;
t
```

## Options

`?Text`

is either a value of that type, e.g. `?"hello"`, or `null`.

``` motoko name=display
func display(x : ?Text) : Text {
  switch x {
    case (null) { "No value" };
    case (?y) { "Value: " # y };
  };
};
```

``` motoko include=display
(display(null), display(?"Test"))
```

## Option blocks

Switching on every option value can be inconvenient …​  

The *option block*, `do ? { … }`, allow you to safely access option values with a postfix *null break* `!` expression.

Within `do ? { … }`, which returns an option, the expression `e!` immediately exits the block with `null` when the value of option `e` is `null` or continues with the option’s contents.

``` motoko
func add(x : ?Nat, y: ?Nat) : ?Nat {
  do ? { x! + y! };
};

(add(null, null), add (?1,null), add (?1,?2), add (null,?2));
```

## Arrays (immutable)

`[Text]`

``` motoko include=impDebugInt
let days = [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ];

assert(days.size() == 7);

assert(days[1] == "Tue");

// days[7] will trap (fixed size)

for (d in days.vals()) { Debug.print(d) };
```

## Arrays (mutable)

`[var Nat]`

``` motoko
let counters = [var 1, 2, 3];

assert(counters.size() == 3);

counters[1] := counters[1] + 1;

// counters[3] will trap (fixed size)

counters;
```

## Records

`{first : Text; last : Text; salary : var Nat}`

``` motoko include=impDebugInt
let employee = {first = "John"; last = "Doe"; var salary = 81_932};

Debug.print(
  employee.first # " " # employee.last # " earns " #
    Int.toText(employee.salary) # " pounds."
);

employee.salary += 79_496;

employee;
```

## Objects

`{first : Text; last : Text; get : () → Nat; add : Nat → ()}`

``` motoko
object self {
  public let first = "John";
  public let last = "Doe";
  var salary : Nat = 81_932; // private by default
  public func get() : Nat = salary;
  public func add(bump : Nat) { salary += bump };
}
```

## Classes

``` motoko
class Employee(fst : Text, lst : Text) {
  public let first = fst;
  public let last = lst;
  var salary : Nat = 0;
  public func get() : Nat = salary;
  public func add(bump : Nat) { salary += bump };
}
```

Classes are factories for constructing objects.  
A class introduces a type and a function (for constructing instances).

Just sugar for:

``` motoko no-repl
type Employee = {first : Text; last : Text; get : () -> Nat; add : Nat -> ()};

func Employee(fst : Text, lst : Text) : Employee = object { … }
```

## Variants

`{#Sun; #Mon; #Tue; #Wed; #Thu; #Fri; #Sat}`

``` motoko
type Day = {#Sun; #Mon; #Tue; #Wed; #Thu; #Fri; #Sat};

func toText(d : Day) : Text {
  switch d {
     case (#Sun) "Sunday";
     case (#Mon) "Monday";
     case (#Tue) "Tuesday";
     case (#Wed) "Wednesday";
     case (#Thu) "Thursday";
     case (#Fri) "Friday";
     case (#Sat) "Saturday";
   };
};

func sort(d : Day) : { #WeekDay; #WeekEnd } {
  switch d {
    case (#Sun or #Sat) #WeekEnd;  // or pattern
    case _ #WeekDay;  // wildcard pattern
  };
};
```

## Recursive Types

``` motoko name=Lists
type List = {
  #item : {head : Text; tail : List}; // variant with payload!
  #empty                     // ^^^^ recursion!
};

func reverse(l : List) : List {
  func rev(l : List, r : List) : List {
    switch l {
      case (#empty) { r };
      case (#item { head; tail }) { // nested patterns
        rev(tail, #item {head; tail = r})
      }
    }
  };
  rev(l, #empty);
};

let l = reverse(#item {head = "A"; tail = #item {head = "B"; tail = #empty}});
```

## Generic types

``` motoko
type List<T> = {
  #item : {head : T; tail : List<T>};
  #empty
};

func reverse<T>(l : List<T>) : List<T> {
  func rev(l : List<T>, r : List<T>) : List<T> {
    switch l {
      case (#empty) { r };
      case (#item { head; tail }) { // a nested pattern
        rev(tail, #item {head; tail = r})
      }
    }
  };
  rev(l, #empty);
};

let s : List<Text> =
  reverse(#item {head = "A"; tail = #item {head = "B"; tail = #empty}});

let ns : List<Nat> =
  reverse(#item {head = 0; tail = #item {head = 1; tail = #empty}})
```

# Packages and modules

## Modules

``` motoko no-repl
// the type of base/Int.mo
module {
  type Int = Prim.Types.Int;
  toText : Int -> Text;
  abs : Int -> Nat;
  // ...
}
```

modules contain named types and values (like objects),  
but are restricted to *static* content (pure, no state, …)

## Module imports

``` motoko no-repl
import Debug "mo:base/Debug";  // import from package
import Int "mo:base/Int";
import MyLib "lib/MyLib";  // import from local file MyLib.mo
```

`base` package provides basic features as separate modules.

More libraries popping up!

`MyLib.mo` *must* contain a module or actor class, eg:

``` motoko no-repl
module {
  public type List<T> = …;

  public func reverse<T>(l : List<T>) : List<T> { … };
}
```

# Platform features

## Actor types

Like object types, but marked as `actor`:

``` motoko name=actorTypes
type Broadcast = actor {
  register : Receiver -> ();
  send : Text -> async Nat;
};

type Receiver = actor {
  recv : query Text -> async Nat
};
```

*sharable* arguments and *no* or *async* result type.

-   `register` is a *oneway* IC method (unawaitable).

-   `send` is an IC *update* method

-   `recv` is IC *query* method

IC canister with Candid interface ≈ Motoko actor

## sharable ≈ serializable

**Sharable:**

-   all primitive types

-   records, tuples, arrays, variants, options  
    with immutable sharable components

-   `actor` types

-   `shared` function type

**Not sharable:**

-   mutable things

-   local functions

-   objects (with methods)

## A complete actor

``` motoko
import Array "mo:base/Array";

actor Broadcast {
  type Receiver = actor {recv : query Text -> async Nat};

  var r : [Receiver] = [];

  public func register(a : Receiver) {
    r := Array.append(r, [a]);
  };

  public func send(t : Text) : async Nat {
    var sum = 0;
    for (a in r.vals()) {
      sum += await a.recv(t);
    };
    return sum;
  };
}
```

a typical actor/canister main file

## Async/await

`async T`

asychronous future or promise

introduced by `async { … }`  
(implicit in async function declaration)

`await e`  
suspends computation pending `e`’s result:  
if the result is a value, continues with that value,  
if the result is an `Error`, `throw`s the error.

``` motoko no-repl
  public func send(t : Text) : async Nat {
    var sum = 0;
    for (a in r.vals()) {
      sum += await a.recv(t); // may return Nat or `throw` error
    };
    return sum;
  };
```

(Errors can be handled using `try … catch …`)

## Concurrency Hazards

Functions that `await` are *not* atomic.  
Suspension introduces *concurrency hazards*.

A bad implementation of `send`:

``` motoko no-repl
  var sum = 0; // shared state!
  public func send(t : Text) : async Nat {
    sum := 0;
    for (a in r.vals()) {
      sum += await a.recv(t);
    };
    return sum;
  };
```

(Concurrent `send`s will share and clobber `sum`.)

Beware of race conditions!

## Actor import

``` motoko
import Broadcast "canister:Broadcast";
/* or
import Broadcast "ic:r7inp-6aaaa-aaaaa-aaabq-cai";
*/
actor Self {

  var count = 0;

  public func go() {
    Broadcast.register(Self);
  };

  public query func recv(msg : Text) : async Nat {
    return count;
  }
}
```

(assumes there is a Candid file describing the interface of the import)

## A Candid interface file

`Broadcast`'s Candid file (produced by `moc --idl Broadcast.mo` compiler).

Broadcast.did:

``` candid
type Receiver =
 service {
   recv: (text) -> (nat) query;
 };
service : {
  register: (Receiver) -> () oneway;
  send: (text) -> (nat);
}
```

A language independent interface definition.

Could just as easily describe a Rust implementation of `Broadcast`.

## Principal and caller

``` motoko
import Principal "mo:base/Principal";

actor Self {

  public shared(context) func hello() : async Text {
    let myself : Principal = Principal.fromActor(Self);
    if (context.caller == myself) {
      "Talking to yourself is the first sign of madness";
    } else {
      "Hello, nice to see you";
    };
  };

}
```

## Errors

``` motoko no-repl
import Principal "mo:base/Principal";
import Error "mo:base/Error";

actor Self {

  public shared(context) func hello() : async Text {
    let myself : Principal = Principal.fromActor(Self);
    if (context.caller == myself) {
      throw Error.reject("Talking to yourself is the first sign of madness");
    } else {
      "Hello, nice to see you";
    };
  };

};

async {
  let t = try Self.hello() catch (e) { Error.message(e); }
};
```

Similar to exceptions in other languages,  
but *only* available in async contexts, e.g. shared functions; async blocks

## Stable variables

If we upgrade the `Broadcast` actor, all current registrations are lost.  
To preserve them, declare the state variable `r` as `stable`.

``` motoko no-repl
import Array "mo:base/Array";

actor Broadcast {

  type Receiver = actor {recv : query Text -> async Nat};

  stable var r : [Receiver] = []; // declare r `stable`

  public func register(a : Receiver) { … }
  public func send(t : Text) : async Nat { … }

  // optional pre-upgrade action
  system func preupgrade() { Debug.print("saving receivers"); }

  // optional post-upgrade action
  system func postupgrade() {  Debug.print("restoring receivers"); }
}
```

stable variables must have *stable* types (see manual)  
`system` hooks can’t send messages

# Type system

## Structural

``` motoko include=Lists
/*
type List = {
  #item : {head : Text; tail : List};
  #empty
};

func reverse(l : List) : List { //... };
*/
type Stack = {
   #empty;
   #item : {tail : Stack; head : Text};
};

let stack : Stack = #empty;

let revStack = reverse(stack); // works though reverse defined on List (not Stack)
```

Type definitions  
do not create types,  
but name existing types

Despite their different names, `Stack` and `List` are equivalent types.

## Subtyping (Variants)

`WeekDay <: Day`

``` motoko
type WeekDay = {#Mon; #Tue; #Wed; #Thu; #Fri};

type Day = {#Sun; #Mon; #Tue; #Wed; #Thu; #Fri; #Sat};

func toText(d : Day) : Text {
  switch d
   { case (#Sun) "Sunday";
     case (#Mon) "Monday";
     //...
   };
};

let mon : WeekDay = #Mon;
let t = toText(mon); // also works, since WeekDay <: Day
```

`t1 <: t2`: `t1` can be used wherever `t2` is expected

`Employee <: Person`

``` motoko
type Employee = {first : Text; last : Text; var salary : Nat};
type Person = {first : Text; last : Text};

func toText(p : Person) : Text {
  p.last # "," # p.first;
};

let employee : Employee =
  { first = "John"; last = "Doe"; var salary = 161_401};

let t = toText(employee); // also works, since Employee <: Person
```

# Fin

## Not covered

-   Polymorphic functions with type bounds

-   User defined iterator objects, supporting `for` loops.

-   Actor classes

-   `debug_show` for conversion of almost any value to text.

-   `debug e` expressions for debug-only compilation

-   `do ? { … e! …  }` blocks for handling/propagating option values.

-   `assert e` expressions for conditional traps

-   tools:

    -   `mo_doc` (generates doc from doc comments),

    -   `vessel` (package manager)

    -   `mo_ide` (LSP language server for VSCode, emacs etc)
