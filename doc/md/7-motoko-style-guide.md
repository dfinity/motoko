---
sidebar_position: 7
---

# Motoko style guidelines

To increase the readability and uniformity of Motoko source code, the style guide provides suggestions for formatting Motoko sources and other basic conventions.

## Layout

### Spacing

- Put spaces around arithmetic operators, except when visually grouping sub-expressions of more tightly binding operators.

    ``` motoko no-repl
    let z = - 2*x + 3*y + 4*(x*x + y*y);
    ```

- Put spaces around comparison operators, Boolean operators, and assignment operators.

    ``` motoko no-repl
    4 + 5 <= 5 + 4;
    not (a or b and not c);
    v := 0;
    v += 1;
    ```

- Put spaces around '='.

    ``` motoko no-repl
    var v = 0;
    let r = { a = 1; b = 2 };
    ```

- Analogously, put spaces around `:`.

    ``` motoko no-repl
    var v : Nat = 0;
    func foo(x : Nat, y : Nat) : Nat { x + y }
    func bar((x, y) : (Nat, Nat)) : Nat { x + y }
    let w = 1 ^ 0xff : Nat16;
    ```

    Rationale: ':' is to declarations what '=' is to definitions. Moreover, the left-hand of a type annotation may generally be an arbitrary complex expression or pattern.

- Put a space after a comma or semicolon, but not before.

    ``` motoko no-repl
    let tuple = (1, 2, 3);
    let record = { a = 1; b = 2; c = 3 };
    ```

- Put spaces inside braces, unless they are a simple variant or record.

    ``` motoko no-repl
    func f() { 0 };
    f({ a = 1; b = 2; c = 3 });
    f({a = 1; b = 2});  // okay as well

    type Vec3D = { x : Float; y : Float; y : Float };
    type Order = { #less; #equal; #more };

    type Order = {#less; #equal; #more};  // okay as well
    type Proc = {h : Nat; w : Nat} -> {#ok; #fail};
    ```

- Put spaces inside brackets if they stretch multiple lines.

    ``` motoko no-repl
    foo(
      firstArgument,
      ( longTupleComponent, anotherLongExpression,
        moreLongExpression
      ),
      [ 1, 2, 3,
        4, 5, 6,
      ],
      { field1 = 4; field2 = 5;
        field3 = 6;
      }
    );
    ```

- Put a space between statement keywords and their operands.

    ``` motoko no-repl
    if (f()) A else B;
    for (x in xs.values()) { ... };
    switch (compare(x, y)) {
      case (#less) { A };
      case (_) { B };
    }

    assert (x < 100);
    await (async (0));
    ```

- Do not put a space between a function or variant tag and its argument tuple or around a generic type parameter list.

    ``` motoko no-repl
    type Pair<X> = (X, X);
    type Id = <X>(X) -> X;

    let ok = #ok(5);

    func id<X>(x : X) : X { x };
    id<Nat>(5);
    ```

- Put a space between a function and its argument if it is not a tuple or parenthesized expression (see [parentheses](#parentheses)) or a record used as a named parameter list (see [picking types](#picking-types)).

    ``` motoko no-repl
    sin 0.0;
    g [1, 2, 3];
    f{arg1 = 0; arg2 = 0};
    ```

Rationale: `g[1]` in particular will be misparsed as an indexing operation.

- Do not put a space around access operators like `.`, `?`, `!`, or index brackets.

    ``` motoko no-repl
    foo(bar).baz[5]().boo;
    foom(?(bam()! + 1));
    ```

### Line breaks

- Pick a fixed right margin for lines and break definitions or expressions accordingly. A limit of 80 characters is considered a good standard as recommended by style guides such as [PEP 8](https://peps.python.org/pep-0008/#maximum-line-length), the [GNU Coding Standards](https://www.gnu.org/prep/standards/standards.html#Long-Lines), and the [Linux Kernel Coding Style](https://www.kernel.org/doc/html/latest/process/coding-style.html#breaking-long-lines-and-strings).

    ``` motoko no-repl
    let sum = a + b + 2*c + d +
      e + f + g + h + i + k +
      l + m + n + o + p;

    // Or:
    let sum =
      a + b + 2*c + d + e +
      f + g + h + i + k + l +
      m + n + o + p;
    ```

    Rationale: Among other reasons, this style of formatting:

    1. Avoids code being hidden to the right in a window.

    2. Avoids random line breaks in side-by-side diffs. For example, as shown by GitHub or similar code review tools.

    3. Allows prettier display on paper, web sites, or other media.

- Break lines after an operator.

    ``` motoko no-repl
    a + b + c +
      d + f;

    foo(bar, baz).
      boo();
    ```

- When breaking function definitions or calls with long argument lists, put each argument on a separate line.

    Also, consider using records for long parameter lists, see [picking types](#picking-types).

    ``` motoko no-repl
    func someFunction(
      arg1 : FirstType,
      arg2 : SecondType,
      anotherArg : Nat,
      yetAnother : [Type],
      func : Nat -> Nat,
    ) : Nat {
      ...
    };

    someFunction(
      veryLongArgumentExpression,
      anotherVeryLongArgumentExpression,
      3,
      aNestedFunctionCall(
        alsoWithLongArguments,
        andMoreSuchArguments,
      ),
      moreLongishArgument,
    );
    ```

    Rationale: This prevents overlooking an argument when reading code and avoids re-breaking lines when changing one of the expressions.

### Indentation

- Each level of indentation should be 2 spaces.

    ``` motoko no-repl
    actor A {
      public func f() {
        return;
      }
    }
    ```

    Rationale: There may be a lot of nesting. Using only 2 spaces avoids wasting screen estate.

- Indentation should not depend on the lexical contents of previous lines.

    In particular, do not vertically align indentation with inner characters from previous lines.

    ``` motoko no-repl
    let x = someFunction(
      arg1, arg2, arg3, arg4, arg5);               // Do this.

    let x = someFunction(arg1, arg2, arg3,
      arg4, arg5);                                 // Or this.

    let x =
      someFunction(arg1, arg2, arg3, arg4, arg5);  // Or this.

    let x = someFunction(                          // Or this.
      longArg1,
      longArg2,
      longArg3,
      longArg4,
      longArg5
      );

    // COUNTER EXAMPLE!
    let x = someFunction(arg1, arg2, arg3,
                         arg4, arg5);              // DO NOT DO THIS!
    ```

    Rationale: There are many problems with vertical alignment, for example:

    1. It wastes a lot of horizontal space.

    2. It creates wildly inconsistent indentation levels that obfuscate the structure of the code.

    3. It can produce realignment churn when changing a line, which, even when automated by editors, inflates and obfuscates diffs.

    4. It completely breaks with variable-width fonts.

    Rule of thumb: there should be no indentation that is not a multiple of 2.

- Do not use tabs.

    Rationale: The interpretation of tabs varies wildly across tools and they get lost or are displayed incorrectly in many contexts, such as web pages, diffs, etc.

### Grouping

- Separate complex multi-line definitions with empty lines. One-liners can be put on consecutive lines.

    ``` motoko no-repl
    func foo() {
      // This function does a lot of interesting stuff.
      // It's definition takes multiple lines.
    }

    func boo() {
      // This is another complicated function.
      // It's definition also takes multiple lines.
    }

    func add(x : Nat, y : Nat) { x + y };
    func mul(x : Nat, y : Nat) { x * y };
    ```

- Separate logic groups of definitions with two empty lines. Add a one-line comment as a "section header" for each group.

    ``` motoko no-repl
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

### Comments

- Use line comments (`//…​`). Use block comments (`/* …​ */`) only when commenting in the middle of a line or for commenting out pieces of code during development.

    ``` motoko no-repl
    // The following function runs the current
    // pallaboom on a given snibble. It returns
    // suitable plexus if it can.
    func paBoom(s : Snibble) : Handle<Plexus> {
      let puglet = initPugs(s.crick, 0 /* size */, #local);
    /* Don't do the odd stuff yet...
      ...
      ...
    */
      return polyfillNexus(puglet);  // for now
    }
    ```

    Rationale: Line comments make it easier to insert, remove or swap individual lines.

- Put short comments explaining a single line at the end of the line, separated by at least 2 spaces.

    ``` motoko no-repl
    paBoom(getSnibble()));  // create new snibble
    ```

- Put multi-line comments before a line of code, with the same indentation as the code it is describing.

    ``` motoko no-repl
    func f() {
      // Try to invoke the current pallaboom with
      // the previous snibble. If that succeeds,
      // we have the new plexus; if not, complain.
      let plexusHandle = paBoom(getSnibble()));
    }
    ```

- Capitalize comments that are on separate lines. Use a proper full stop for sentences.

## Punctuation

### Semicolons

- Motoko uniformly requires a semicolon to separate expressions or local declarations in a block, regardless of whether the preceding declaration ends in a closing '}'.

    Rationale: This is unlike other C-style languages, which tend to have rather ad-hoc rules.

- Put a semicolon after the last expression in a block, unless the whole block is written on a single line.

    Similarly for types.

    ``` motoko no-repl
    // No ; needed before closing } on same line

    type Vec3D = {x : Float; y : Float; z : Float};
    type Result<A> = {#ok : A; #error : Text};

    func add(x : Nat, y : Nat) : Nat { return x + y };

    // End last case with ;

    type Address = {
      first : Text;
      last : Text;
      street : Text;
      nr : Nat;
      zip : Nat;
      city : Text;
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

- Put braces around function bodies, `if` or `case` branches, and loop bodies, unless they appear nested as an expression and only contain a single expression.

    ``` motoko no-repl
    func f(x) { f1(x); f2(x) };

    let abs = if (v >= 0) v else -v;
    let val = switch (f()) { case (#ok(x)) x; case (_) 0 };
    func succ(x : Nat) : Nat = x + 1;
    ```

- Use "C-style" layout for braced sub-expressions stretching multiple lines.

    ``` motoko no-repl
    func f() {
      return;
    };

    if (cond) {
      foo();
    } else {
      bar();
    };

    switch (opt) {
      case (?x) {
        f(x);
      };
      case (null) {};
    };
    ```

### Parentheses

- Motoko supports optional parentheses in most places, such as function parameter lists, or statement operands, when they enclose an expression that either is bracketed already. For example, a tuple, object, array, or a simple constant or identifier.

    ``` motoko no-repl
    type Op = Nat -> Nat;
    let a2 = Array.map<Nat, Nat>(func x { x + 1 }, a);

    let y = f x;
    let z = f {};
    let choice = if flag { f1() } else { f2() };

    switch opt {
      case null { tryAgain() };
      case _ { proceed() };
    };
    ```

- Avoid overuse of omitting parentheses.

    In particular, do not omit parentheses and braces on statements at the same time.

    ``` motoko no-repl
    // COUNTER EXAMPLES!
    let choice = if flag x + y else z;  // DO NOT DO THIS!

    switch val {
      case 0 f();    // DO NOT DO THIS!
      case n n + 1;  // OR THIS!
    };
    ```

    Rationale: Omitting both at the same time makes the code harder to read, since there are less visual clues as to how it is grouped.

- Similarly, do not omit parentheses around function parameters if the function also has type parameters.

    ``` motoko no-repl
    // COUNTER EXAMPLE!
    foo<Nat> 0;   // DO NOT DO THIS!
    ```

- Omit parentheses around argument types of a function type with a single argument and no type parameters.

    But do not omit them when functions or classes also have type parameters.

    ``` motoko no-repl
    type Inv = Nat -> Nat;
    type Id = <T>(T) -> T;
    type Get = <X>(C<X>) -> X;

    // COUNTER EXAMPLE!
    type Get = <X>C<X> -> X;   // DO NOT DO THIS!
    ```

### Miscellaneous

- Use `_` to group digits in numbers.

    Group by 3 digits in decimal numbers and by 4 in hexadecimal notation.

    ``` motoko no-repl
    let billion = 1_000_000_000;
    let pi = 3.141_592_653_589_793_12;
    let mask : Nat32 = 0xff00_ff0f;
    ```

## Naming

### Style

- Use `UpperCamelCase` for type names (including classes or type parameters), module names, and actor names.

- Use `lowerCamelCase` for all other names, including constants and variant fields.

    ``` motoko no-repl
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

    Rationale: The general convention is upper case for "static" entities like types and lower case for "dynamic" values. Modules and actors are fairly static and can export types. Objects usually don’t export types and tend to be used mostly as dynamic values.

- Capitalize acronyms as regular words.

    ``` motoko no-repl
    type HttpHeader = ...;
    func getUrl() { ... };
    let urlDigest = ...;
    ```

- Do not use identifier names that start with an underscore `_`, except to document that a variable in a pattern is intentionally unused.

    ``` motoko no-repl
    let (width, _color, name) = rumpler();
    ...  // _color is not used here

    func foo(x : Nat, _futureFlag : Bool) { ... };
    ```

    Rationale: A type checker can warn about unused identifiers, which can be suppressed by explicitly prepending `_` to its name to document intention.

    This aligns with the use of the keyword `_` for pattern wildcards.

### Conventions

- The name of functions returning a value should describe that value.

    Avoid redundant `get` prefixes.

    ``` motoko no-repl
    dict.size();
    list.first();
    sum(array);
    ```

- The name of functions performing side effects or complex operations should describe that operation.

    ``` motoko no-repl
    dict.clear();
    dict.set(key, value);
    let result = traverse(graph);
    ```

- The name of predicate functions returning [`Bool`](./base/Bool.md) should use an `is` or `has` prefix or a similar description of the tested property.

    ``` motoko no-repl
    class Set<X>() {
      public func size() : Nat { ... };

      public func add(x : X) { ... };
      public func remove(x : X) { ... };

      public func isEmpty() : Bool { ... };
      public func contains(x : X) : Bool { ... };
    };
    ```

- Functions converting to or from a type `X` are named `toX` and `fromX`, respectively, if the source, resp. target, is either the object the function is a method of, or the primary type of the module this function appears in.

- In classes or objects, use a name ending with `_` to distinguish private variables from getters.

    ``` motoko no-repl
    class Cart(length_ : Nat) {
      var width_ = 0;

      public func length() : Nat { length_ };
      public func width() : Nat { width_ };
    }
    ```

    Rationale: In Motoko, functions are first-class values, so functions and other value identifiers share the same name space.

    Identifiers with a leading `_` should *not* be used for private state, since that indicates an unused name (see [style](#style)).

- Use longer, more descriptive names for global or public identifier or ones with large scope, and short names for local ones with small scope.

    It is fine to use single character identifiers when there is nothing interesting to say, especially when using the same naming scheme consistently.

    ``` motoko no-repl
    func map(x : Nat, y : Nat) : Nat { x + y };

    func eval(e : Expr) : Nat {
      let n =
        switch (e) {
          case (#neg(e1)) { - eval(e1) };
          case (#add(e1, e2)) { eval(e1) + eval(e2) };
          case (#mul(e1, e2)) { eval(e1) * eval(e2) };
        };
      Debug.print(n);
      n;
    };
    ```

    Rationale: Contrary to popular belief, overly chatty local names can decrease readability instead of increasing it.

- In suitable cases, use plural form for describing a collection of items, such as a list or array.

    This also works for short names.

    ``` motoko no-repl
    func foreach<X>(xs : [X], f : X -> ()) {
      for (x in xs.values()) { f(x) }
    }
    ```

## Types

### Type annotations

- Put type annotations on definitions that involve fixed-width numeric types to disambiguate the type of overloaded arithmetic operators and constants.

    ``` motoko no-repl
    let mask : Nat32 = 0xfc03_ff00;
    let pivot : Nat32 = (size + 1)/2;
    let vec : [Int16] = [1, 3, -4, 0];
    ```

    :::note

    Use floating point constants to enforce type `Float` without an extra annotation. Similarly, use an explicit `+` sign to produce a positive value of type [`Int`](./base/Int.md) instead of [`Nat`](./base/Nat.md), if desired.

    :::

    ``` motoko no-repl
    let zero = 1.0;    // type Float
    let offset = +1;   // type Int
    ```

- Similarly, put inline type annotations on arithmetic expressions with types other than [`Nat`](./base/Nat.md) or [`Int`](./base/Int.md).

    ``` motoko no-repl
    if (x & mask == (1 : Nat32)) { ... };
    ```

    :::note

    The need to annotate constants in cases like this is a short-coming of Motoko’s type system.

    :::

    An annotation is not needed on function arguments since their type is usually inferred from the function. The only exception is when that argument has a generic type and the type arguments have been omitted.

    ``` motoko no-repl
    func foo(len : Nat32, vec : [Nat16]) { ... };
    func bar<X>(x : X) { ... };

    foo(3, [0, 1, 2]);
    bar<Nat16>(0);
    bar(0 : Nat16);
    ```

- Put type annotations on mutable variables.

    ``` motoko no-repl
    var name = "Motoko";
    var balance = 0;

    func f(i : Int) {
      var j = i;
    };

    var balance : Int = 0;
    var obj : Class = foo();
    ```

    Rationale: Due to subtyping, inferring the type from the initialization would not necessarily deduce the intended type. For example, `balance` would have type [`Nat`](../motoko/base/Nat.md) without the annotation, ruling out assignments of integers.

- Put type annotations on all public fields in a class.

    ``` motoko no-repl
    class C(init_ : Nat) {
      public let init : Nat = init_;
      public var count : Nat = 0;
    }
    ```

- Omit return type annotations of functions when the type is `()`.

    ``` motoko no-repl
    func twiceF() { f(); f() };  // no need to write ": ()"
    ```

- Omit type annotations on functions when they are passed as arguments.

    ``` motoko no-repl
    Array.map<Nat, Nat>(func n {n + 1}, a);
    ```

- Put type annotations on definitions that involve numeric types other than [`Nat`](./base/Nat.md) or [`Int`](./base/Int.md), to resolve the overloading between arithmetic operators and constants.

    ``` motoko no-repl
    let mask : Nat32 = 0xfc03_ff00;
    let offset : Nat32 = size + 1;
    ```

### Picking types

- Use [`Nat`](./base/Nat.md) for any integral value that cannot be negative.

- Use fixed-width `NatN` or `IntN` only when storing many values and space usage matters, when bit-fiddling requires the low-level interpretation of a number as a vector of bits, or when matching types imposed by external requirements such as other canisters.

- Avoid proliferation of option types, and therefore `null`.

    Limit their use to as small a scope as possible. Rule out the `null` case and use non-option types wherever possible.

- Consider using records instead of tuples when there are more than 2 or 3 components. Records are just simple objects with named fields.

    Note that record types can be used in place and do not need to be declared.

    ``` motoko no-repl
      func nodeInfo(node : Node) : {parent : Node; left : Node; right : Node} { ... }
    ```

- Consider using variants instead of [`Bool`](./base/Bool.md) to represent binary choices.

    Note that variant types can be used in place and do not need to be declared.

    ``` motoko no-repl
    func capitalization(word : Text) : {#upper; #lower} { ... }
    ```

- Where possible, use return type `()` for functions whose primary purpose is to mutate state or cause other side effects.

    ``` motoko no-repl
    class Set<X>() {
      public func add(x : X) { ... };
      public func remove(x : X) { ... };
      ...
    };
    ```

- Consider using a record (an object with just data) as argument for long parameter lists.

    ``` motoko no-repl
    func process({seed : Float; delta : Float; data : [Record]; config : Config}) : Thing {
      ...
    };

    process{config = Config(); data = read(); delta = 0.01; seed = 1.0};
    ```

    Rationale: This expresses named parameters. This way, arguments can be freely reordered at the call site and callers are prevented from accidentally passing them in the wrong order.

- Higher-order functions, such as functions that take a callback argument, should put the function parameter last.

    Rationale: This makes call sites more readable, and in the absence of currying, there is no point in putting the function first, like you often would in functional languages.

- Do not use sentinel values, such as `-1`, to represent invalid values.

    Use the option type instead.

    ``` motoko no-repl
    func lookup(x : key) : ?Nat { ... }
    ```

- Data is immutable in Motoko unless explicitly stated otherwise.

    Use mutability types and definitions (`var`) with care and only where needed.

    Rationale: Mutable data cannot be communicated or share across actors. It is more error-prone and much more difficult to formally reason about, especially when concurrency is involved.

## Features

### Statements

- Use `for` loops instead of `while` loops for iterating over a numeric range or a container.

    ``` motoko no-repl
    for (i in Iter.range(1, 10)) { ... };
    for (x in array.values()) { ... };
    ```

    Rationale: For loops are less error-prone and easier to read.

- Use `if` or `switch` as expressions where appropriate.

    ``` motoko no-repl
    func abs(i : Int) : Int { if (i < 0) -i else i };

    let delta = switch mode { case (#up) +1; case (#dn) -1 };
    ```

- Motoko requires that all expressions in a block have type `()` to prevent accidentally dropped results.

    Use `ignore` to explicitly drop results. Do *not* use `ignore` when it’s not needed.

    ``` motoko no-repl
    ignore async f();  // fire of a computation
    ```

- Motoko allows omitting the `return` at the end of a function because a block evaluates to its last expression.

    Omit `return` when a function is short and in "functional" style, that is, the function does not contain complex control flow or side effects.

    Use explicit `return` at the end when the function contains other `return` statements or imperative control flow.

    ``` motoko no-repl
    func add(i : Nat, j : Nat) : Nat { i + j };

    func foo(a : Float, b : Float) : Float {
      let c = a*a + b*b;
      c + 2*c*c;
    };

    func gcd(i : Nat, j : Nat) : Nat {
      if (j == 0) i else gcd(j, i % j);
    };

    func gcd2(i : Nat, j : Nat) : Nat {
      var a = i;
      var b = j;
      while (b > 0) {
        let c = a;
        a := b;
        b := c % b;
      };
      a;
    };
    ```

### Objects and records

- Use the short-hand object syntax `{x1 = e1; …​ ; xN = eN}` when using objects as simple records, i.e., data structures with no private state and no methods.

- Use `object` when creating singleton objects.

- Limit the use of objects to records where possible.

    Rationale: Only records can be sent as message parameters or results and can be stored in stable variables. Objects with methods are also more expensive to create and represent in memory.

- Use full objects only as a means for encapsulating state or behavior.

### Classes

- Use `class` to create multiple objects of the same shape.

- Name classes after their conceptual functionality, not their implementation, except when having to distinguish multiple different implementations of the same concept. For example, `OrderedMap` vs `HashMap`.

- Classes are both type definitions and factory functions for objects.

    Do not use classes unless both these roles are intended; use plain type aliases or functions returning an object in other cases.

- Do not overuse classes.

    Use a module defining a plain type and functions on it where appropriate. Use classes only as a means for encapsulating state or behavior.

    Rationale: Objects with methods have disadvantages over simple record types with separate functions (see above).

- If values of a class are meant to be shared, the class needs to provide a pair of `share`/`unshare` methods that convert to/from a sharable representation, for example, as a record.

    :::note

    For immutable classes it may seem more natural to make `unshare` a kind of static function. However, even for immutable ones it may depend on constructor arguments (such as an ordering function), so that the a pattern like `Map(compareInt).unshare(x)` seems appropriate.

    :::

- Avoid overloading classes with too many methods, since that is expensive.

    Restrict to a sufficiently small set of canonical methods. Make less essential methods that can be implemented on top of the canonical methods into functions in the enclosing module.

- Use modules for "static" classes or methods.

### Modules

- Use `module` to group definitions, including types, and create a name space for them.

- Where applicable, name modules after the main type or class they implement or provide functions for.

- Limit each module to a single main concept/type/class or closely entangled family of concepts/types/classes.

### Error handling

- Use `Result` for error handling where applicable.
  - Use `Result<T, E>` rather than exceptions for recoverable errors.
  - Use meaningful error types instead of generic `Text`.

    ```motoko no-repl
    type MyError = { #NotFound; #InvalidInput; #PermissionDenied };

    func safeDivide(a : Int, b : Int) : Result.Result<Int, MyError> {
      if (b == 0) { return #err (#InvalidInput) };
      #ok (a / b);
    };
    ```

- Use `assert` only for programming errors.
  - `assert` should be reserved for conditions that should never fail if the program is correct.

    ```motoko no-repl
    assert(x > 0);  // Only use if x should never be <= 0
    ```

- Use `debug assert` for development-time checks.
  - `debug assert` is like `assert`, but only runs in non-release builds.
  - Useful for checking internal assumptions or invariants without impacting production performance.
  - All `debug` expressions are ignored when compiled with `moc --release`.

    ```motoko no-repl
    debug assert(balance >= 0);  // Ensure balance is never negative during development
    ```

### Concurrency

- Use `await` judiciously.
  - Minimize the number of `await` calls in a function to avoid unnecessary suspensions.

    ```motoko no-repl
    let user = await db.getUser(id); // Good ~4s
    let orders = await db.getOrders(user); // Bad: sequential calls introduce high latency ~6s
    ```

- Use composite queries where applicable.

Example:

```motoko no-repl
// This is an unecessary update call :
public func couldBeQuery() : async () {
  await queryFunction();
}
```

This can be optimized with a composite query:

```motoko no-repl
// Composite queries can await other queries without becoming an update call
public composite query func betterQuery() : async () {
  await queryFunction();
}
```

### Pattern matching

- Use `switch` instead of manual conditionals for variants.
  - Prefer exhaustive pattern matching to avoid missing cases.
  - Exhaust all cases before using the wildcard pattern.

    ```motoko no-repl
    switch (response) {
      case (#possiblity1) { processPossibility1() };
      case (#possiblity2) { processPossibility2() }
      case (#possiblity2) { processPossibility3() };
      case (_) { handleExecption(err) };;
    };
    ```

### Data structures

- Prefer `List` over dynamic array resizing.
  - `Array` in Motoko is immutable, so if dynamic resizing is needed, `List` is a better choice.

    ```motoko no-repl
    let list = List.empty<Nat>()
    List.add(list, 1);;
    let arr = List.toArray(list);
    ```

## Motoko grammar

This section describes the concrete syntax (grammar) of Motoko. The specification is auto-generated with a tool. This grammar is in the [Extended Backus–Naur form(EBNF)](https://tomassetti.me/ebnf/).

```EBNF
<list(X, SEP)> ::=
    <empty>
    X
    X SEP <list(X, SEP)>

<list1(X, SEP)> ::=
    X
    X SEP <list(X, SEP)>

<typ_obj_sort> ::=
    'object'
    'actor'
    'module'

<obj_sort> ::=
    'object'
    'persistent'? 'actor'
    'module'

<query> ::=
    'query'
    'composite' 'query'

<func_sort_opt> ::=
    <empty>
    'shared' <query>?
    <query>

<shared_pat_opt> ::=
    <empty>
    'shared' <query>? <pat_plain>?
    <query> <pat_plain>?

<typ_obj> ::=
    '{' <list(<typ_field>, ';')> '}'

<typ_variant> ::=
    '{' '#' '}'
    '{' <list1(<typ_tag>, ';')> '}'

<typ_nullary> ::=
    '(' <list(<typ_item>, ',')> ')'
    <id> ('.' <id>)* <typ_args>?
    '[' 'var'? <typ> ']'
    <typ_obj>
    <typ_variant>

<typ_un> ::=
    <typ_nullary>
    '?' <typ_un>

<typ_pre> ::=
    <typ_un>
    'async' <typ_pre>
    'async*' <typ_pre>
    <typ_obj_sort> <typ_obj>

<typ_nobin> ::=
    <typ_pre>
    <func_sort_opt> <typ_params_opt> <typ_un> '->' <typ_nobin>

<typ> ::=
    <typ_nobin>
    <typ> 'and' <typ>
    <typ> 'or' <typ>

<typ_item> ::=
    <id> ':' <typ>
    <typ>

<typ_args> ::=
    '<' <list(<typ>, ',')> '>'

<inst> ::=
    <empty>
    '<' <list(<typ>, ',')> '>'
    '<' 'system' (',' <typ>)* '>'

<typ_params_opt> ::=
    ('<' <list(<typ_bind>, ',')> '>')?
    '<' 'system' (',' <typ_bind>)* '>'

<typ_field> ::=
    'type' <id> ('<' <list(<typ_bind>, ',')> '>')? '=' <typ>
    'var'? <id> ':' <typ>
    <id> <typ_params_opt> <typ_nullary> ':' <typ>

<typ_tag> ::=
    '#' <id> (':' <typ>)?

<typ_bind> ::=
    <id> '<:' <typ>
    <id>

<lit> ::=
    'null'
    <bool>
    <nat>
    <float>
    <char>
    <text>

<unop> ::=
    '+'
    '-'
    '^'

<binop> ::=
    '+'
    '-'
    '*'
    '/'
    '%'
    '**'
    '+%'
    '-%'
    '*%'
    '**%'
    '&'
    '|'
    '^'
    '<<'
    ' >>'
    '<<>'
    '<>>'
    '#'

<relop> ::=
    '=='
    '!='
    ' < '
    '<='
    ' > '
    '>='

<unassign> ::=
    '+='
    '-='
    '^='

<binassign> ::=
    '+='
    '-='
    '*='
    '/='
    '%='
    '**-'
    '+%='
    '-%='
    '*%='
    '**%='
    '&='
    '|='
    '^='
    '<<='
    '>>='
    '<<>='
    '<>>='
    '@='

<exp_obj> ::=
    '{' <list(<exp_field>, ';')> '}'
    '{' <exp_post> 'and' <exp_post> ('and' <exp_post>)* '}'
    '{' <exp_post> ('and' <exp_post>)* 'with' <list1(<exp_field>, ';')> '}'

<exp_plain> ::=
    <lit>
    '(' <list(<exp>, ',')> ')'

<exp_nullary> ::=
    <exp_obj>
    <exp_plain>
    <id>
    '_'

<exp_post> ::=
    <exp_nullary>
    '[' 'var'? <list(<exp_nonvar>, ',')> ']'
    <exp_post> '[' <exp> ']'
    <exp_post> '.'<nat>
    <exp_post> '.' <id>
    <exp_post> <inst> <exp_nullary>
    <exp_post> '!'
    '(' 'system' <exp_post> '.' <id> ')'

<exp_un> ::=
    <exp_post>
    '#' <id>
    '#' <id> <exp_nullary>
    '?' <exp_un>
    <unop> <exp_un>
    <unassign> <exp_un>
    'actor' <exp_plain>
    'not' <exp_un>
    'debug_show' <exp_un>
    'to_candid' '(' <list(<exp>, ',')> ')'
    'from_candid' <exp_un>

<exp_bin> ::=
    <exp_un>
    <exp_bin> <binop> <exp_bin>
    <exp_bin> <relop> <exp_bin>
    <exp_bin> 'and' <exp_bin>
    <exp_bin> 'or' <exp_bin>
    <exp_bin> ':' <typ_nobin>
    <exp_bin> '|>' <exp_bin>

<exp_nondec> ::=
    <exp_bin>
    <exp_bin> ':=' <exp>
    <exp_bin> <binassign> <exp>
    'return' <exp>?
    'async' <exp_nest>
    'async*' <exp_nest>
    'await' <exp_nest>
    'await*' <exp_nest>
    'assert' <exp_nest>
    'label' <id> (':' <typ>)? <exp_nest>
    'break' <id> <exp_nullary>?
    'continue' <id>
    'debug' <exp_nest>
    'if' <exp_nullary> <exp_nest>
    'if' <exp_nullary> <exp_nest> 'else' <exp_nest>
    'try' <exp_nest> <catch>
    'try' <exp_nest> <catch> 'finally' <exp_nest>
    'try' <exp_nest> 'finally' <exp_nest>
    'throw' <exp_nest>
    'switch' <exp_nullary> '{' <list(<case>, ';')> '}'
    'while' <exp_nullary> <exp_nest>
    'loop' <exp_nest>
    'loop' <exp_nest> 'while' <exp_nest>
    'for' '(' <pat> 'in' <exp> ')' <exp_nest>
    'ignore' <exp_nest>
    'do' <block>
    'do' '?' <block>

<exp_nonvar> ::=
    <exp_nondec>
    <dec_nonvar>

<exp> ::=
    <exp_nonvar>
    <dec_var>

<exp_nest> ::=
    <block>
    <exp>

<block> ::=
    '{' <list(<dec>, ';')> '}'

<case> ::=
    'case' <pat_nullary> <exp_nest>

<catch> ::=
    'catch' <pat_nullary> <exp_nest>

<exp_field> ::=
    'var'? <id> (':' <typ>)?
    'var'? <id> (':' <typ>)? '=' <exp>

<dec_field> ::=
    <vis> <stab> <dec>

<vis> ::=
    <empty>
    'private'
    'public'
    'system'

<stab> ::=
    <empty>
    'flexible'
    'stable'
    'transient'

<pat_plain> ::=
    '_'
    <id>
    <lit>
    '(' <list(<pat_bin>, ',')> ')'

<pat_nullary> ::=
    <pat_plain>
    '{' <list(<pat_field>, ';')> '}'

<pat_un> ::=
    <pat_nullary>
    '#' <id>
    '#' <id> <pat_nullary>
    '?' <pat_un>
    <unop> <lit>

<pat_bin> ::=
    <pat_un>
    <pat_bin> 'or' <pat_bin>
    <pat_bin> ':' <typ>

<pat> ::=
    <pat_bin>

<pat_field> ::=
    <id> (':' <typ>)?
    <id> (':' <typ>)? '=' <pat>

<dec_var> ::=
    'var' <id> (':' <typ>)? '=' <exp>

<dec_nonvar> ::=
    'let' <pat> '=' <exp>
    'type' <id> ('<' <list(<typ_bind>, ',')> '>')? '=' <typ>
    <obj_sort> <id>? (':' <typ>)? '='? <obj_body>
    <shared_pat_opt> 'func' <id>? <typ_params_opt> <pat_plain> (':' <typ>)? <func_body>
    <shared_pat_opt> <obj_sort>? 'class' <id>? <typ_params_opt> <pat_plain> (':' <typ>)? <class_body>

<dec> ::=
    <dec_var>
    <dec_nonvar>
    <exp_nondec>
    'let' <pat> '=' <exp> 'else' <exp_nest>

<func_body> ::=
    '=' <exp>
    <block>

<obj_body> ::=
    '{' <list(<dec_field>, ';')> '}'

<class_body> ::=
    '=' <id>? <obj_body>
    <obj_body>

<imp> ::=
    'import' <pat_nullary> '='? <text>

<prog> ::=
    <list(<imp>, ';')> <list(<dec>, ';')>
```
