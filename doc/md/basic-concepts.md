# Basic concepts and terms

Motoko is designed for distributed programming with actors.

When programming on the Internet Computer in Motoko, each **actor** represents an **Internet Computer canister smart contract** with a Candid interface, whether written in Motoko, Rust, Wasm or some other language that compiles to Wasm. Within Motoko, we use the term **actor** to refer to any canister, authored in any language that deploys to the Internet Computer. The role of Motoko is to make these actors easy to author, and easy to use programmatically, once deployed.

Before you begin writing distributed applications using actors, you should be familiar with a few of the basic building blocks of any programming language and with Motoko in particular. To get you started, this section introduces the following key concepts and terms that are used throughout the remainder of the documentation and that are essential to learning to program in Motoko:

-   program

-   declaration

-   expression

-   value

-   variable

-   type

If you have experience programming in other languages or are familiar with modern programming language theory, you are probably already comfortable with these terms and how they are used. There’s nothing unique in how these terms are used in Motoko. If you are new to programming, however, this guide introduces each of these terms gradually and by using simplified example programs that eschew any use of actors or distributed programming. After you have the basic terminology as a foundation to build on, you can explore more advanced aspects of the language. More advanced features are illustrated with correspondingly more complex examples.

The following topics are covered in the section:

-   [Motoko program syntax](#motoko-program-syntax)

-   [Printing values](#printing-values)

-   [Using the base library](#the-motoko-base-library)

-   [Declarations versus expressions](#declarations-versus-expressions)

-   [Lexical scoping of variables](#declarations-follow-lexical-scoping)

-   [Values and evaluation](#values-and-evaluation)

-   [Type annotations and variables](#type-annotations-and-variables)

-   [Type soundness and type-safe evaluation](#type-soundness)

## Motoko program syntax

Each Motoko *program* is a free mix of declarations and expressions, whose syntactic classes are distinct, but related (see the [language quick reference](language-manual.md) for precise program syntax).

For programs that we deploy on the Internet Computer, a valid program consists of an *actor expression*, introduced with specific syntax (keyword `actor`) that we discuss in [Actors and async data](actors-async.md).

In preparing for that discussion, this section and Section [Mutable state](mutable-state.md) begin by discussing programs that are not meant to be Internet Computer services. Rather, these tiny programs illustrate snippets of Motoko for writing those services, and each can (usually) be run on its own as a (non-service) Motoko program, possibly with some printed terminal output.

The examples in this section illustrate basic principles using simple expressions, such as arithmetic. For an overview of the full expression syntax of Motoko, see the [Language quick reference](language-manual.md).

As a starting point, the following code snippet consists of two declarations — for the variables `x` and `y` — followed by an expression to form a single program:

``` motoko
let x = 1;
let y = x + 1;
x * y + x;
```

We will use variations of this small program in our discussion below.

First, this program’s type is `Nat` (natural number), and when run, it evaluates to the (natural number) value of `3`.

Introducing a block with enclosing braces (`do {` and `}`) and another variable (`z`), we can amend our original program as follows:

``` motoko
let z = do {
  let x = 1;
  let y = x + 1;
  x * y + x
};
```

## Declarations and expressions

Declarations introduce immutable variables, mutable state, actors, objects, classes and other types. Expressions describe computations that involve these notions.

For now, we use example programs that declare immutable variables, and compute simple arithmetic.

### Declarations versus expressions

[Recall](#motoko-program-syntax) that each Motoko *program* is a free mix of declarations and expressions, whose syntactic classes are distinct, but related. In this section, we use examples to illustrate their distinctions and accommodate their intermixing.

Recall our example program, first introduced above:

``` motoko
let x = 1;
let y = x + 1;
x * y + x;
```

In reality, this program is a *declaration list* that consists of *three* declarations:

1.  immutable variable `x`, via declaration `let x = 1;`,

2.  immutable variable `y`, via declaration `let y = x + 1;`,

3.  and an *unnamed, implicit variable* holding the final expression’s value, `x * y + x`.

This expression `x * y + x` illustrates a more general principle: Each expression can be thought of as a declaration where necessary since the language implicitly declares an unnamed variable with that expression’s result value.

When the expression appears as the final declaration, this expression may have any type. Here, the expression `x * y + x` has type `Nat`.

Expressions that do not appear at the end, but rather *within* the list of declarations must have unit type `()`.

### Ignoring non-unit-typed expressions in declaration lists

We can always overcome this unit-type restriction by explicitly using `ignore` to ignore any unused result values. For example:

``` motoko
let x = 1;
ignore(x + 42);
let y = x + 1;
ignore(y * 42);
x * y + x;
```

### Declarations and variable substitution

Declarations can be mutually recursive, but in cases where they are not, they permit substitution semantics. (that is, replacing equals for equals, as familiar from high-school algebraic simplification).

Recall our original example:

``` motoko
let x = 1;
let y = x + 1;
x * y + x;
```

We can manually rewrite the program above by *substituting* the variables' declared values for each of their respective occurrences.

In so doing, we produce the following expression, which is also a program:

``` motoko
1 * (1 + 1) + 1
```

This is also a valid program — of the same type and with the same behavior (result value `3`) — as the original program.

We can also form a single expression using a block.

### From declarations to block expressions

Many of the programs above each consist of a list of declarations, as with this example, just above:

``` motoko
let x = 1;
let y = x + 1;
x * y + x
```

A declaration list is not itself (immediately) an *expression*, so we cannot (immediately) declare another variable with its final value (`3`).

**Block expressions.** We can form a *block expression* from this list of declarations by enclosing it with matching *curly braces*. Blocks are only allowed as sub-expressions of control flow expressions like `if`, `loop`, `case`, etc. In all other places, we use `do { …​ }` to represent block expression, to distinguish blocks from object literals. For example, `do {}` is the empty block of type `()`, while `{}` is an empty record of record type `{}`.

``` motoko
do {
  let x = 1;
  let y = x + 1;
  x * y + x
}
```

This is also program, but one where the declared variables `x` and `y` are privately scoped to the block we introduced.

This block form preserves the autonomy of the declaration list and its *choice of variable names*.

A block expression produces a value and, when enclosed in parentheses, can occur within some larger, compound expression. For example:

``` motoko
100 +
  (do {
     let x = 1;
     let y = x + 1;
     x * y + x
   })
```

### Declarations follow **lexical scoping**

Above, we saw that nesting blocks preserves the autonomy of each separate declaration list and its *choice of variable names*. Language theorists call this idea *lexical scoping*. It means that variables' scopes may nest, but they may not interfere as they nest.

For instance, the following (larger, enclosing) program evaluates to `42`, *not* `2`, since the final occurrences of `x` and `y`, on the final line, refer to the *very first* definitions, *not* the later ones within the enclosed block:

``` motoko
let x = 40; let y = 2;
ignore do {
  let x = 1;
  let y = x + 1;
  x * y + x
};
x + y
```

Other languages that lack lexical scoping may give a different meaning to this program. However, modern languages universally favor lexical scoping, the meaning given here.

Aside from mathematical clarity, the chief practical benefit of lexical scoping is *security*, and its use in building compositionally-secure systems. Specifically, Motoko gives very strong composition properties. For example, nesting your program within a program you do not trust cannot arbitrarily redefine your variables with different meanings.

## Values and evaluation

Once a Motoko expression receives the program’s (single) thread of control, it evaluates eagerly until it reduces to a *result value*.

In so doing, it will generally pass control to sub-expressions, and to sub-routines before it gives up control from the *ambient control stack*.

If this expression never reaches a value form, the expression evaluates indefinitely. Later we introduce recursive functions and imperative control flow, which each permit non-termination. For now, we only consider terminating programs that result in values.

In the material above, we focused on expressions that produced natural numbers. As a broader language overview, however, we briefly summarize the other value forms below:

### Primitive values

Motoko permits the following primitive value forms:

-   Boolean values (`true` and `false`).

-   Integers (…​,`-2`, `-1`, `0`, `1`, `2`, …​) - bounded and *unbounded* variants.

-   Natural numbers (`0`, `1`, `2`, …​) - bounded and *unbounded* variants.

-   Text values - strings of unicode characters.

By default, **integers** and **natural numbers** are *unbounded* and do not overflow. Instead, they use representations that grow to accommodate any finite number.

For practical reasons, Motoko also includes *bounded* types for integers and natural numbers, distinct from the default versions. Each bounded variant has a fixed bit-width (one of `8`, `16`, `32`, `64`) that determines the range of representable values and each carries the potential for *overflow*. Exceeding a bound is a run-time fault that causes the program to [trap](#traps-due-to-faults).

There are no unchecked, uncaught overflows in Motoko, except in well-defined situations, for explicitly *wrapping* operations (indicated by a conventional `%` character in the operator). The language provides primitive built-ins to convert between these various number representations.

The [language quick reference](language-manual.md) contains a complete list of [primitive types](language-manual.md#primitive-types).

### Non-primitive values

Building on the primitive values and types above, the language permits user-defined types, and each of the following non-primitive value forms and associated types:

-   [Tuples](language-manual.md#exp-tuple), including the unit value (the "empty tuple");

-   [Arrays](language-manual.md#exp-arrays), with both *immutable* and *mutable* variants;

-   [Objects](language-manual.md#exp-object), with named, unordered fields and methods;

-   [Variants](language-manual.md#variant-types), with named constructors and optional payload values;

-   [Function values](language-manual.md#exp-func), including [shareable functions](sharing.md);

-   [Async values](language-manual.md#exp-async), also known as *promises* or *futures*;

-   [Error values](language-manual.md#type-Error) carry the payload of exceptions and system failures.

We discuss the use of these forms in the next sections.

For precise language definitions of primitive and non-primitive values, see the [language quick reference](language-manual.md).

### The *unit* type `()`

Motoko has no type named `void`. In many cases where readers may think of return types being “void” from using languages like Java or C++, we encourage them to think instead of the *unit type*, written `()`.

In practical terms, like `void`, the unit value usually carries zero representation cost.

Unlike the `void` type, there *is* a unit value, but like the `void` return value, the unit value carries no values internally, and as such, it always carries zero *information*.

Another mathematical way to think of the unit value is as a tuple with no elements - the nullary (“zero-ary”) tuple. There is only one value with these properties, so it is mathematically unique, and thus need not be represented at runtime.

### Natural numbers

The members of this type consist of the usual values - `0`, `1`, `2`, …​ - but, as in mathematics, the members of `Nat` are not bound to a special maximum size. Rather, the runtime representation of these values accommodates arbitrary-sized numbers, making their "overflow" (nearly) impossible. (*nearly* because it is the same event as running out of program memory, which can always happen for some programs in extreme situations).

Motoko permits the usual arithmetic operations one would expect. As an illustrative example, consider the following program:

``` motoko
let x = 42 + (1 * 37) / 12: Nat
```

This program evaluates to the value `45`, also of type `Nat`.

## Type soundness

Each Motoko expression that type-checks we call *well-typed*. The *type* of a Motoko expression serves as a promise from the language to the developer about the future behavior of the program, if executed.

First, each well-typed program will evaluate without undefined behavior. That is, the phrase **“well-typed programs don’t go wrong”** applies here. For those unfamiliar with the deeper implications of that phrase, it means that there is a precise space of meaningful (unambiguous) programs, and the type system enforces that we stay within it, and that all well-typed programs have a precise (unambiguous) meaning.

Furthermore, the types make a precise prediction over the program’s result. If it yields control, the program will generate a *result value* that agrees with that of the original program.

In either case, the static and dynamic views of the program are linked by and agree with the static type system. This agreement is the central principle of a static type system, and is delivered by Motoko as a core aspect of its design.

The same type system also enforces that asynchronous interactions agree between static and dynamic views of the program, and that the resulting messages generated "under the hood" never mismatch at runtime. This agreement is similar in spirit to the caller/callee argument type and return type agreements that one ordinarily expects in a typed language.

## Type annotations and variables

Variables relate (static) names and (static) types with (dynamic) values that are present only at runtime.

In this sense, Motoko types provide a form of *trusted, compiler-verified documentation* in the program source code.

Consider this very short program:

``` motoko
let x : Nat = 1
```

In this example, the compiler infers that the expression `1` has type `Nat`, and that `x` has the same type.

In this case, we can omit this annotation without changing the meaning of the program:

``` motoko
let x = 1
```

Except for some esoteric situations involving operator overloading, type annotations do not (typically) affect the meaning of the program as it runs.

If they are omitted and the compiler accepts the program, as is the case above, the program has the same meaning (same *behavior*) as it did originally.

However, sometimes type annotations are required by the compiler to infer other assumptions, and to check the program as a whole.

When they are added and the compiler still accepts the program, we know that the added annotations are *consistent* with the existing ones.

For instance, we can add additional (not required) annotations, and the compiler checks that all annotations and other inferred facts agree as a whole:

``` motoko
let x : Nat = 1 : Nat
```

If we were to try to do something *inconsistent* with our annotation type, however, the type checker will signal an error.

Consider this program, which is not well-typed:

``` motoko run
let x : Text = 1 + 1
```

The type annotation `Text` does not agree with the rest of the program, since the type of `1 + 1` is `Nat` and not `Text`, and these types are unrelated by subtyping. Consequently, this program is not well-typed, and the compiler will signal an error (with a message and location) and will not compile or execute it.

## Type errors and messages

Mathematically, the type system of Motoko is *declarative*, meaning that it exists independently of any implementation, as a concept entirely in formal logic. Likewise, the other key aspects of the language definition (for example, its execution semantics) exist outside of an implementation.

However, to design this logical definition, to experiment with it, and to practice making mistakes, we want to interact with this type system, and to make lots of harmless mistakes along the way.

The error messages of the *type checker* attempt to help the developer when they misunderstand or otherwise misapply the logic of the type system, which is explained indirectly in this book.

These error messages will evolve over time, and for this reason, we will not include particular error messages in this text. Instead, we will attempt to explain each code example in its surrounding prose.

### The Motoko base library

For various practical language engineering reasons, the design of Motoko strives to minimize builtin types and operations.

Instead, whenever possible, the Motoko base library provides the types and operations that make the language feel complete. ***However**, this base library is still under development, and is still incomplete*.

The [Motoko Base Library](../../../../references/motoko-ref/stdlib-intro.md) lists a *selection* of modules from the Motoko base library, focusing on core features used in the examples that are unlikely to change radically. However, all of these base library APIs will certainly change over time (to varying degrees), and in particular, they will grow in size and number.

To import from the base library, use the `import` keyword. Give a local module name to introduce, in this example `D` for “**D**ebug”, and a URL where the `import` declaration may locate the imported module:

``` motoko
import D "mo:base/Debug";
D.print("hello world");
```

In this case, we import Motoko code (not some other module form) with the `mo:` prefix. We specify the `base/` path, followed by the module’s file name `Debug.mo` minus its extension.

### Printing values

Above, we print the text string using the function `print` in library `Debug.mo`:

``` motoko no-repl
print: Text -> ()
```

The function `print` accepts a text string (of type `Text`) as input, and produces the *unit value* (of *unit type*, or `()`) as its output.

Because unit values carry no information, all values of type unit are identical, so the `print` function doesn’t actually produce an interesting result. Instead of a result, it has a *side effect*. The function `print` has the effect of emitting the text string in a human-readable form to the output terminal. Functions that have side effects, such as emitting output, or modifying state, are often called *impure*. Functions that just return values, without further side-effects, are called *pure*. We discuss the return value (the unit value) [in detail below](#the-unit-type), and relate it to the `void` type for readers more familiar with that concept.

Finally, we can transform most Motoko values into human-readable text strings for debugging purposes, *without* having to write those transformations by hand.

The `debug_show` primitive permits converting a large class of values into values of type `Text`.

For instance, we can convert a triple (of type `(Text, Nat, Text)`) into debugging text without writing a custom conversion function ourselves:

``` motoko
import D "mo:base/Debug";
D.print(debug_show(("hello", 42, "world")))
```

Using these text transformations, we can print most Motoko data as we experiment with our programs.

### Accommodating incomplete code

Sometimes, in the midst of writing a program, we want to run an incomplete version, or a version where one or more execution paths are either missing or simply invalid.

To accommodate these situations, we use the `xxx`, `nyi` and `unreachable` functions from the base `Prelude` library, explained below. Each is a simple wrapper around a more general trap mechanism [general trap mechanism](#explicit-traps), explained further below.

### Use short-term holes

Short-term holes are never committed to a source repository, and only ever exist in a single development session, for a developer that is still writing the program.

Assuming that earlier, one has imported the prelude as follows:

``` motoko name=prelude
import P "mo:base/Prelude";
```

The developer can fill *any missing expression* with the following one:

``` motoko include=prelude
P.xxx()
```

The result will *always* type check at compile time, and *will always* trap at run time, if and when this expression ever executes.

### Document longer-term holes

By convention, longer-term holes can be considered "not yet implemented" (`nyi`) features, and marked as such with a similar function from the Prelude module:

``` motoko include=prelude
P.nyi()
```

### Document `unreachable` code paths

In contrast to the situations above, sometimes code will *never* be filled, since it will *never* be evaluated, assuming the coherence of the internal logic of the programs' invariants.

To document a code path as logically impossible, or *unreachable*, use the base library function `unreachable`:

``` motoko include=prelude
P.unreachable()
```

As in the situations above, this function type-checks in all contexts, and when evaluated, traps in all contexts.

### Traps due to faults

Some errors, such as division by zero, out-of-bounds array indexing, and pattern match failure are (by design) not prevented by the type system, but instead cause dynamic faults called *traps*.

``` motoko
1/0; // traps due to division by 0
```

``` motoko
let a = ["hello", "world"];
a[2]; // traps due to out-of-bounds indexing
```

``` motoko
let true = false; // pattern match failure
```

We say that code *traps* when its exection causes a *trap*.

Because the meaning of execution is ill-defined after a faulting trap, execution of the code ends by aborting at the trap.

:::note

Traps that occur within actor messages are more subtle: they don’t abort the entire actor, but prevent that particular message from proceeding, rolling back any yet uncommitted state changes. Other messages on the actor will continue execution.

:::

### Explicit traps

Occasionally it can be useful to force an unconditional trap, with a user-defined message.

The `Debug` library provides the function `trap(t)` for this purpose, which can be used in any context.

``` motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

``` motoko
import Debug "mo:base/Debug";

let swear : Text = Debug.trap("oh my!");
```

(The `Prelude` functions `nyi()`, `unreachable()` and `xxx()` discussed above are simple wrappers around `Debug.trap`.)

### Assertions

Assertions allow you to conditionally trap when some Boolean test fails to hold, but continue execution otherwise. For example,

``` motoko
let n = 65535;
assert n % 2 == 0; // traps when n not even
```

``` motoko
assert false; // unconditionally traps
```

``` motoko
import Debug "mo:base/Debug";

assert 1 > 0; // never traps
Debug.print "bingo!";
```

Because an assertion may succeed, and thus proceed with execution, it may only be used in context where a value of type `()` is expected.
