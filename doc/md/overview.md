# Concise overview of Motoko {#_concise_overview_of_motoko}

This is terse, slide-like introduction to Motoko and its features.

(For a gentler introduction, visit the other sections on this site.)

## Motivation and Goals {#_motivation_and_goals}

A simple, useful language for the Internet Computer (IC)

-   Familiar syntax

-   Safe by default

-   Incorporating **actor** model for canister smart contracts

-   Seamless integration of IC features

-   Making most of present and future WebAssembly

## Key Design Points {#_key_design_points}

-   Object-oriented, functional & imperative

-   Objects as records of functions

-   `async`/`await` for sequential programming of asynchronous messaging

-   Structural typing with simple generics and subtyping

-   Safe arithmetic (both unbounded and checked)

-   Non-nullable types by default

-   Garbage collected (no manual memory management)

-   JavaScript-like syntax but statically typed & sane

Inspirations: Java, JavaScript, C#, Swift, Pony, ML, Haskell

## Semantics {#_semantics}

-   call-by-value (like Java, C, JS, ML; unlike Haskell)

-   declarations are locally mutually recursive

-   parametric, bounded polymorphism

-   subtyping as zero-cost subsumption, not coercion

-   no dynamic casts

-   no inheritance

## Implementation(s) {#_implementations}

-   implemented in OCaml (leverages `wasm` libary)

-   simple reference interpreter

-   less simple compiler to WebAssembly

    -   multipass with typed IR in each pass.

    -   uniform representation, unboxed arithmetic

    -   two-space GC or mark-compact GC, invoked after messages (for now)

-   polymorphism by erasure

# The language {#_the_language}

## Expressions {#_expressions}

-   Identifiers:  
    `x`, `foo_bar`, `test123`, `List`, `Map`

-   Parentheses `( … )` for grouping

-   Braces `{ … }` for scoping (and records)

-   `;` for sequencing

-   Type annotations (to help type inference):  
    `(42 : Int)`  
    (zero cost)

## Libraries {#_libraries}

``` {#impDebugInt .motoko}
  import Debug "mo:base/Debug";
  import Int "mo:base/Int";
```

(`import MyLib "src/MyLib"` imports a library from the local file system.)

Specific bindings can be imported from the module using object patterns

      import { cons; nil } = "mo:base/List";

## Libraries {#_libraries_2}

``` motoko
  import Debug "mo:base/Debug";
  import Int "mo:base/Int";
  import Trie "mo:base/Trie";

  type Users = Trie.Trie<Text, Nat>; // reference types

  Debug.print(Int.toText(7)); // reference functions/values
```

## Blocks and declarations {#_blocks_and_declarations}

``` motoko
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

## Control flow {#_control_flow}

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

# Primitive types {#_primitive_types}

## Unbounded integers {#_unbounded_integers}

`Int`

`{ …​, -2, 1, 0, 1, 2, …​ }`

Inferred by default for negative literals.

Literals: `13`, `0xf4`, `-20`, `+1`, `1_000_000`

## Unbounded naturals {#_unbounded_naturals}

`Nat`

`{ 0, 1, 2, …​ }`

Non-negative, trap on underflow.

Inferred by default for non-negative literals

Literals: `13`, `0xf4`, `1_000_000`

`Nat <: Int`

`Nat` is a *subtype* of `Int`

(you can supply a `Nat` wherever an `Int` is expected)

## Bounded numbers (trapping) {#_bounded_numbers_trapping}

`Nat8`, `Nat16`, `Nat32`, `Nat64`, `Int8`, `Int16`, `Int32`, `Int64`

Trap on over- and underflow; wrap-around and bit-manipulating operations available separately

Needs type annotations (somewhere)

Literals: `13`, `0xf4`, `-20`, `1_000_000`

## Floating point numbers {#_floating_point_numbers}

`Float`

IEEE 754 double precision (64 bit) semantics, normalized NaN

Inferred for fractional literals

Literals: 0, -10, `2.71`, `-0.3e+15`, `3.141_592_653_589_793_12`

## Numeric operations {#_numeric_operations}

No surprises here

`- x`  
`a + b`  
`a % b`  
`a & b`  
`a << b`  
…

`a +% b, a -% b, …` for wrapping, modular arithmetic (where appropriate)

## Characters and Text {#_characters_and_text}

`Char`, `Text`

Unicode! Character = Unicode scalar value; no random access on text

-   `'x'`, `'\u{6a}'`, `'☃'`,

-   `"boo"`, `"foo \u{62}ar ☃"`

-   `"Concat" # "enation"`

## Booleans {#_booleans}

`Bool`

Literals: `true`, `false`

`a or b`  
`a and b`  
`not b`  
`if (b) e1 else e2`

# Functions {#_functions}

## Function types {#_function_types}

-   Simple functions:

    ``` motoko
    Int.toText : Int -> Text
    ```

-   multiple arguments and return values

    ``` motoko
    divRem : (Int, Int) -> (Int, Int)
    ```

-   can be generic/polymorphic

    ``` motoko
    Option.unwrapOr : <T>(?T, default : T) -> T
    ```

-   first-class (can be passed around, stored)

    ``` motoko
    map : <A, B>(f : A -> B, xs : [A]) -> [B]
    let funcs : [<T>(T) -> T] = …
    ```

## Function Declarations & Use {#_function_declarations_use}

``` motoko
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

# Composite types {#_composite_types}

## Tuples {#_tuples}

`(Bool, Float, Text)`

immutable, heterogeneous, fixed size

``` {#tuple .motoko}
let tuple = (true or false, 0.6 * 2.0, "foo" # "bar");
```

``` motoko
tuple.1;
```

``` motoko
let (_,_,t) = tuple;
t
```

## Options {#_options}

`?Text`

is either a value of that type, e.g. `?"hello"`, or `null`.

``` {#display .motoko}
func display(x : ?Text) : Text {
  switch x {
    case (null) { "No value" };
    case (?y) { "Value: " # y };
  };
};
```

``` motoko
(display(null), display(?"Test"))
```

## Option blocks {#_option_blocks}

Switching on every option value can be inconvenient …​  

The *option block*, `do ? { … }`, allow you to safely access option values with a postfix *null break* `!` expression.

Within `do ? { … }`, which returns an option, the expression `e!` immediately exits the block with `null` when the value of option `e` is `null` or continues with the option’s contents.

``` motoko
func add(x : ?Nat, y: ?Nat) : ?Nat {
  do ? { x! + y! };
};

(add(null, null), add (?1,null), add (?1,?2), add (null,?2));
```

## Arrays (immutable) {#_arrays_immutable}

`[Text]`

``` motoko
let days = [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ];

assert(days.size() == 7);

assert(days[1] == "Tue");

// days[7] will trap (fixed size)

for (d in days.vals()) { Debug.print(d) };
```

## Arrays (mutable) {#_arrays_mutable}

`[var Nat]`

``` motoko
let counters = [var 1, 2, 3];

assert(counters.size() == 3);

counters[1] := counters[1] + 1;

// counters[3] will trap (fixed size)

counters;
```

## Records {#_records}

`{first : Text; last : Text; salary : var Nat}`

``` motoko
let employee = {first = "John"; last = "Doe"; var salary = 81_932};

Debug.print(
  employee.first # " " # employee.last # " earns " #
    Int.toText(employee.salary) # " pounds."
);

employee.salary += 79_496;

employee;
```

## Objects {#_objects}

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

## Classes {#_classes}

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

``` motoko
type Employee = {first : Text; last : Text; get : () -> Nat; add : Nat -> ()};

func Employee(fst : Text, lst : Text) : Employee = object { … }
```

## Variants {#_variants}

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

## Recursive Types {#_recursive_types}

``` {#Lists .motoko}
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

## Generic types {#_generic_types}

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

# Packages and modules {#_packages_and_modules}

## Modules {#_modules}

``` motoko
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

## Module imports {#_module_imports}

``` motoko
import Debug "mo:base/Debug";  // import from package
import Int "mo:base/Int";
import MyLib "lib/MyLib";  // import from local file MyLib.mo
```

`base` package provides basic features as separate modules.

More libraries popping up!

`MyLib.mo` *must* contain a module or actor class, eg:

``` motoko
module {
  public type List<T> = …;

  public func reverse<T>(l : List<T>) : List<T> { … };
}
```

# Platform features {#_platform_features}

## Actor types {#_actor_types}

Like object types, but marked as `actor`:

``` {#actorTypes .motoko}
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

## sharable ≈ serializable {#_sharable_serializable}

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

## A complete actor {#_a_complete_actor}

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

## Async/await {#_asyncawait}

`async T`

asychronous future or promise

introduced by `async { … }`  
(implicit in async function declaration)

`await e`  
suspends computation pending `e`’s result:  
if the result is a value, continues with that value,  
if the result is an `Error`, `throw`s the error.

``` motoko
  public func send(t : Text) : async Nat {
    var sum = 0;
    for (a in r.vals()) {
      sum += await a.recv(t); // may return Nat or `throw` error
    };
    return sum;
  };
```

(Errors can be handled using `try … catch …`)

## Concurrency Hazards {#_concurrency_hazards}

Functions that `await` are *not* atomic.  
Suspension introduces *concurrency hazards*.

A bad implementation of `send`:

``` motoko
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

## Actor import {#_actor_import}

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

## A Candid interface file {#_a_candid_interface_file}

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

## Principal and caller {#_principal_and_caller}

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

## Errors {#_errors}

``` motoko
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

## Stable variables {#_stable_variables}

If we upgrade the `Broadcast` actor, all current registrations are lost.  
To preserve them, declare the state variable `r` as `stable`.

``` motoko
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

# Type system {#_type_system}

## Structural {#_structural}

``` motoko
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

## Subtyping (Variants) {#_subtyping_variants}

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

# Fin {#_fin}

## Not covered {#_not_covered}

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
