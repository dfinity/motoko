# Motoko Style Guide

To increase readability and uniformity of Motoko source code, this guide provides suggestions for formatting Motoko sources and other basic conventions.


## Layout

### Line Breaks

* Pick a fixed right margin for lines and break definitions or expessions that are longer than that
  (80 still is considered a good limit by many).

Example:
```
let sum = a + b + 2*c + d +
  e + f + g + h + i + k +
  l + m + n + o + p;

// Or:
let sum =
  a + b + 2*c + d + e +
  f + g + h + i + k + l +
  m + n + o + p;
```

Rationale: Among other reasons, this
(1) avoids code being hidden to the right in a window;
(2) avoids random line breaks in side-by-side diffs (e.g., as shown by GitHub or similar code review tools);
(3) allows prettier display on paper, web sites, or other material.

* Break lines _after_ an operator.

Example:
```
a + b + c +
  d + f;

foo(bar, baz).
  boo();
```

* When breaking function definitions or calls with long argument lists, put each argument on a separate line.
  (Also, consider using records for long parameter lists, see [Picking Types](#picking-types))

Example:
```
func someFunction(
  arg1 : FirstType,
  arg2 : SecondType,
  anotherArg : Nat,
  yetAnother : [Type],
  func : Nat -> Nat,
) {
  ...
};

someFunction(
  veryLongArgumentExpression,
  anotherVeryLongArgumentExpression,
  3,
  aNestedFunctionCall(
    alsoWithLongArguments,
    andMoreSuchArguments),
  moreLongishArgument,
);
```

Rationale: This prevents overlooking an argument when reading code and avoids re-breaking lines when changing one of the expressions.


### Indentation

* Each level of indentation should be 2 spaces.

Example:
```
actor A {
  public func f() {
    return;
  }
}
```
Rationale: There may be a lot of nesting. Using only 2 spaces avoids wasting screen estate.

* Indentation should not depend on the lexical contents of previous lines.
  In particular, do not vertically align indentation with inner characters from previous lines.

Counter-Example:
```
let x = someFunction(arg1, arg2, arg3,
                     arg4, arg5);        // DON'T DO THIS!
```
Example:
```
let x = someFunction(
  arg1, arg2, arg3, arg4, arg5);         // Do this instead.

let x = someFunction(arg1, arg2, arg3,
  arg4, arg5);                           // Or this.

let x =
  someFunction(arg1, arg2, arg3, arg4, arg5);     // Or this.

let x = someFunction(   // Or this.
  arg1,
  arg2,
  arg3,
  arg4,
  arg5,
);
```

Rationale: There are many problems with vertical alignment, for example:
(1) it wastes a lot of horizontal space;
(2) it creates wildly inconsistent indentation levels that obfuscate the structure of the code;
(3) it can produce realignment churn when changing a line, which (even when automated by editors) inflates and obfuscates diffs for changes;
(4) it completely breaks with non-proportional fonts.

* Do not use tabs.

Rationale: The interpretation of tabs varies wildly across tools and they get lost or are displayed incorrectly in many contexts, such as web pages, diffs, etc.


### Comments

* Put short comments explaining a single line at the end of the line, separated by at least 2 spaces.

Example:
```
paBoom(getSnibble()));  // create new snibble
```

* Put multi-line comments before a line of code, with the same indentation as the code it is describing.

Example:
```
func f() {
  // Try to invoke the current pallaboom with
  // the previous snibble. If that succeeds,
  // we have the new plexus; if not, complain.
  let plexusHandle = paBoom(getSnibble()));
}
```

* Capitalize comments that are on separate lines.
  Use a proper full stop for sentences.


### Grouping

* Separate complex multi-line definitions with empty lines.
  One-liners can be put on consecutive lines.

Example:
```
func foo() {
  // This function does a lot of interesting stuff.
  // It's definition takes multiple lines.
}

func boo() {
  // This is another complicated function.
  // It's definition also takes multiple lines.
}

func add(x : Nat, y : Nat) { return x + y };
func mul(x : Nat, y : Nat) { return x * y };
```

* Separate logic groups of definitions with two empty lines.
  Add a one-line comment as a "section header" for each group.

Example:
```
// A very large class
class MuffleMiff(n : Nat) {

  // Accessors

  public func miffMuff() : Text {
    ...
  }

  public func sniffMiff() : Nat {
    ...
  }


  // Mutators

  public func clearMurk() {
    ...
  }

  public func addMuff(name : Text) {
    ...
  }


  // Processing
  
  public func murkMuffle(param : List<Gnobble>) {
    ...
  }

  public func transformSneezler() {
    ...
  }


  // Internal State

  var miffCount = 0;
  var mabbleMap = Map<Nat, Text>();

}
```

### Spacing

* Put spaces around arithmetic operators, except to visually group sub-expressions of more tightly binding operators.

Example:
```
let z = 2*x + 3*y + 4*(x*x + y*y);
```

* Put spaces around comparison operators, Boolean operators, and assignment operators.

Examples:
```
4 + 5 <= 5 + 4;
not (a or b and not c);
v := 0;
v += 1;
```

* Put spaces around '='.

Example:
```
var v = 0;
let r = { a = 1; b = 2 };
```

* Analogously, put spaces around `:`.

Example:
```
var v : Nat = 0;
func foo(x : Nat, y : Nat) : Nat { x + y }
func bar((x, y) : (Nat, Nat)) : Nat { x + y }
let w = -1 ^ 0xff : Word16;
```

Rationale: ':' is to declarations what '=' is to definitions.
Moreover, the left-hand of a type annotation may generally be an arbitrary complex expression or pattern.

* Put a space after a comma or semicolon (but not before).

Example:
```
let tuple = (1, 2, 3);
let record = { a = 1; b = 2; c = 3 };
```

* Put spaces inside braces, unless they are a simple variant or record.

Example:
```
func f() { 0 };
f({ a = 1; b = 2; c = 3 });
f({a = 1; b = 2});  // okay as well

type Vec3D = { x : Float; y : Float; y : Float };
type Order = { #less; #equal; #more };

type Order = {#less; #equal; #more};  // okay as well
type Proc = {h : Nat; w : Nat} -> {#ok; #fail};
```

* Put spaces inside regular parentheses or square brackets if they stretch multiple lines.

Example:
```
foo(
  firstArgument,
  ( longTupleComponent, anotherLongExpression,
    moreLongExpression
  ),
  [ 1, 2, 3,
    4, 5, 6,
  ]);
```

* Put a space between statement keywords and their operands.

Examples:
```
if (f()) A else B;
for (x in xs.vals()) { ... };
switch (compare(x, y)) {
  case (#less) { A };
  case (_) { B };
}

assert (x < 100);
await (async (0));
```

* Do _not_ put a space between a function or variant tag and its argument tuple or around a generic type parameter list.

Example:
```
type Pair<X> = (X, X);
type Id = <X>(X) -> X;

let ok = #ok(5);

func id<X>(x : X) : X { x };
id<Nat>(5);
```

* However, put a space between a function and its argument if it is _not_ a tuple or parenthesized expression (see [parenless style](#parentheses)) or a record used as a named parameter list (see [Picking Type](#picking-types)).

Example:
```
sin 0.0;
g [1, 2, 3];
f{arg1 = 0; arg2 = 0};
```
Rationale

* Do _not_ put a space around access operators like `.`, `?`, `!`, or index brackets.

Example:
```
foo(bar).baz[5]().boo;
foom(?(bam()! + 1));
```


## Punctuation

### Semicolons

* Motoko uniformly requires a semicolon to separate expressions or local declarations in a block, regardless of whether the preceding declaration ends in a closing '}'.

Rationale: This is unlike other C-style languages, which tend to have rather ad-hoc rules.

* Put a semicolon after the last expression in a block, unless the whole block is written on a single line.
  Similarly for types.

Example:
```
// No ; needed before closing } on same line

type Vec3D = {x : Float; y : Float; z : Float};
type Result<A> = {#ok : A; #error : Text};

func add(x : Nat, y : Nat) : Nat { return x + y };


// End last case with ;

type Address = {
  first : Text;
  middle : Text;
  last : Text;
  street : Text;
  nr : Nat;
  zip : Nat;
  city : Text;
  country : Text;
};

type Expr = {
  #const : Float;
  #add : (Expr, Expr);
  #mul : (Expr, Expr);
};

func eval(e : Expr) : Float {
  switch (e) {
    case (#const(x)) { x };
    case (#add(e1, e2)) { eval(e1) + eval(e2) };
    case (#mul(e1, e2)) { eval(e1) * eval(e2) };
  };
}
```

Rationale: Consistently ending lines with semicolon simplifies adding, removing, or swapping lines.


### Braces

* Put braces around function bodies, `if` or `case` branches, and loop bodies,
  unless they appear nested as an expression and only contain a single expression.

Example:
```
if (cond) {
  foo();
} else {
  bar();
};

switch (opt) {
  case (?x) { f(x) };
  case (null) {};
};

func f(x) { f1(x); f2(x) };

let abs = if (v >= 0) v else -v;
let val = switch (f()) { case (#ok(x)) x; case (_) 0 };
func succ(x : Nat) : Nat = x + 1;
```


### Parentheses

* Motoko supports "parenless" style, meaning that parentheses are optional in most places, such as function parameter lists, or statement operands, when they enclose an expression that either is bracketed already (e.g., a tuple, object, or array) or is a simple constant or identifier.

Example:
```
type Op = Nat -> Nat;
let a2 = Array.map(a, func x { x + 1 });

let y = f x;
let z = f {}
let choice = if flag { f1() } else { f2() };

switch opt {
  case null { tryAgain() };
  case _ { proceed() };
};
```

* Avoid overuse of parenless style.
  In particular, do not omit parentheses and braces on statements at the same time.

Counter Examples:
```
let choice = if flag f1() else f2();  // DON'T DO THIS!

switch opt {
  case null tryAgain();  // DON'T DO THIS!
  case _ proceed();
};
```

Rationale: Omitting both at the same time makes the code harder to read, since there is less visual clue.

* Omit parentheses around argument types of a function types with a single argument and no type parameters.
  But use parentheses around the arguments when functions or classes have type parameters.

Examples:
```
type Inv = Nat -> Nat;
type Id = <T>(T) -> T;

func add<T>({})
```


## Naming

### Style

* Use `UpperCamelCase` for type names (including classes or type parameters), module names, and actor names.

* Use `lowerCamelCase` for all other names, including constants and variant fields.

Examples:
```
module MoreMuff {
  type FileSize = Nat;
  type Weekday = {#monday; #tuesday; #wednesday};
  type Pair<X> = (X, X);

  class Container<X, Y>() { ... };

  func getValue<Name>(name : Name) : Pair<Name> { ... };

  let zero = 0;
  let pair = getValue<Text>("opus");
  var nifty : Nat = 0;

  object obj { ... };

  actor ServerProxy { ... };
};
```

Rationale: The general convention is upper case for types and lower case for values. Modules and actors can export types, so are considered "bigger". Objects usually don't export types.

* Spell acronyms as regular words.

Examples:
```
type HttpHeader = ...;
func getUrl() { ... };
let urlDigest = ...;
```

* Do not use identifier names that start with an underscore `_`, except to document that a variable in a pattern is intentionally unused.

Example:
```
let (width, _color, name) = rumpler();
...  // _color is not used here


func foo(x : Nat, _futureFlag : Bool) { ... };
```

Rationale: A type checker can warn about unused identifiers, which can be circumvented by explicitly prepending `_` to its name to document intention.
This aligns with the use of the keyword `_` for pattern wildcards.


### Conventions

* The name of functions returning a value should describe that value (as a noun).
  Avoid redundant `get` prefixes.

Examples:
```
dict.size();
list.first();
sum(array);
```

* The name of functions performing side effects or complex operations should describe that operation (as a verb).

Examples:
```
dict.clear();
dict.set(key, value);
let result = traverse(graph);
```

* The name of predicate functions returning `Bool` should use an `is` or `has` prefix or a similar name.

Examples:
```
class Set<X>() {
  public func size() : Nat { ... };

  public func add(x : X) { ... };
  public func remove(x : X) { ... };

  public func isEmpty() : Bool { ... };
  public func contains(x : X) : Bool { ... };
};
```

* In classes or objects, use a name ending with `_` to distinguish private variables from getters.

Example:
```
class Cart(length_ : Nat) {
  var width_ = 0;

  public func length() : Nat { return length_ };
  public func width() : Nat { return width_ };
}
```

Rationale: In Motoko, functions are first-class values, so functions and other value identifiers share the same name space.
Identifiers with a leading `_` should _not_ be used for private state, since that indicates an unused name (see [#style]).

* Use longer, more descriptive names for global or public identifier or ones with large scope, and short names for local ones with small scope.
  It is fine to use single character identifiers when there is nothing interesting to say, especially when using the same naming scheme consistently.

Example:
```
func map(x : Nat, y : Nat) : Nat { x + y };

func eval(e : Expr) : Nat {
  let n =
    switch (e) {
      case (#neg(e1)) { - eval(e1) };
      case (#add(e1, e2)) { eval(e1) + eval(e2) };
      case (#mul(e1, e2)) { eval(e1) * eval(e2) };
    };
  Debug.print(n);
  return n;
};
```

Rationale: Contrary to popular belief, overly chatty local names can decrease readability instad of increasing it, by increasing the noise level.

* In suitable cases, use plural form for describing a collection of items, such as a list or array.
  This also works for short names.

Example:
```
func foreach<X>(xs : [X], f : X -> ()) {
  for (x in xs.vals()) { f(x) }
}
```


## Types

### Type Annotations

* Put type annotations on mutable variables, unless the type is obvious.

Example:
```
var name = "Motoko";
var count = 0;

func f(i : Int) {
  var j = i;
};

var balance : Int = 0;
var obj : Class = foo();
```

Rationale: Due to subtyping, inferring the type from the initialisation would not necessarily deduce the intended type. For example, `balance` would have type `Nat` without the annotation, ruling out assignments of integers.

* Put type annotations on all public fields in a class.

Example:
```
class C(init_ : Nat) {
  public let init : Nat = init_;
  public var count : Nat = 0;
}
```

* Omit return type annotations of functions when the type is `()`.

Example:
```
func twiceF() { f(); f() };  // no need to write ": ()"
```

* Omit type annotations on functions when they are passed as arguments.

Example:
```
Array.map<Nat>(a, func n {n + 1});
```


### Picking Types

* Use return type `()` for functions whose primary purpose is to mutate state or cause other side effects.

Example:
```
class Set<X>() {
  public func add(x : X) { ... };
  public func remove(x : X) { ... };
  ...
};
```

* Use a record (an object with just data) as argument for long parameter lists that have no obvious ordering.

Example:
```
func process{seed : Float; delta : Float; data : [Record]; config : Config} : Thing {
  ...
};

process{config = Config(); data = read(); delta = 0.01; seed = 1.0};
```
Rationale: This expresses named paremeters. This way, arguments can be freely reordered at the call site and callers are prevented from accidentally passing them in the wrong order.



## Features

TODO...

* Return

* Mutable State

* What else...

