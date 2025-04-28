---
sidebar_position: 2
---

# Basic concepts and terms



Motoko is designed for distributed programming with **actors**. When programming on ICP in Motoko, each actor represents an ICP canister smart contract with a Candid interface.
Within Motoko, the term actor is used to refer to any canister authored in any language that deploys to ICP. The role of Motoko is to make these actors easy to author and use programmatically once deployed.

Before you begin writing canisters in Motoko, you should be familiar with a few of the basic concepts and terms specific to Motoko.

## Values

### Primitive values

Motoko supports the following primitive types and values:

-   [`Bool`](../base/Bool.md): Boolean values (`true` and `false`).

-   [`Int`](../base/Int.md): Integers (…​,`-2`, `-1`, `0`, `1`, `2`, …​) - bounded and unbounded variants.

-   [`Nat`](../base/Nat.md): Natural numbers (`0`, `1`, `2`, …​) - bounded and unbounded variants.

- `Char`: Unicode text characters (`'a'`, `'B'`,  `'☃'`).

-   [`Text`](../base/Text.md): Text values of strings of unicode characters.

By default, **integers** and **natural numbers** are **unbounded** and do not overflow. Instead, they use representations that grow to accommodate any finite number.

For practical reasons, Motoko also includes **bounded** types for integers and natural numbers, distinct from the default versions. Each bounded variant has a fixed bit-width (one of `8`, `16`, `32`, `64`) that determines the range of representable values, and each carries the potential for overflow. Exceeding a bound is a run-time fault that causes the program to [trap](#traps-due-to-faults).

There are no unchecked, uncaught overflows in Motoko, except in well-defined situations, for explicitly **wrapping** operations, indicated by a conventional `%` character in the operator. The language provides primitive built-ins to convert between these various number representations.

### Non-primitive values

Motoko permits user-defined types and each of the following non-primitive value forms and associated types:

-   [Tuples](../reference/language-manual#tuples), including the unit value (the "empty tuple").

-   [Arrays](../reference/language-manual#arrays) with both **immutable** and **mutable** variants.

-   [Objects](../reference/language-manual#objects) with named, unordered fields and methods.

-   [Variants](../reference/language-manual#variant-types) with named constructors and optional payload values.

-   [Function values](../reference/language-manual#functions) including [shareable functions](../writing-motoko/sharing.md).

-   [Async values](../reference/language-manual#async), also known as **promises** or **futures**;

-   [Error values](../reference/language-manual#error-type) carry the payload of exceptions and system failures.

For precise language definitions of primitive and non-primitive values, see the [language reference](../reference/language-manual).

## Printing values

The function `print`, from base library [`Debug`](../base/Debug.md), accepts a text string of type [`Text`](../base/Text.md) as input, and produces the unit value of unit type or `()`, as its output.

Because unit values carry no information, all values of type unit are identical, so the `print` function doesn’t actually produce an interesting result. Instead of a result, the function `print` has the effect of emitting the text string in a human-readable form to the output terminal. Functions that have side effects, such as emitting output or modifying state, are often called **impure**. Functions that just return values without further side-effects, are called **pure**.

You can transform most Motoko values into human-readable text strings for debugging purposes, without having to write those transformations by hand. The `debug_show` primitive permits converting a large class of values into values of type [`Text`](../base/Text.md).

## Motoko program syntax

Each Motoko program is comprised of **declarations** and **expressions** whose syntactic classes are distinct but [related](../reference/language-manual).

Declarations introduce **immutable variables**, **mutable state**, **actors**, **objects**, **classes** and other types. Declarations can be mutually recursive, but in cases where they are not, they permit substitution semantics such as replacing values with an equal value.

Expressions describe computations that involve these notions.

To deploy a valid program to ICP, the program must consist of an actor expression introduced with the keyword `actor`.

As a starting point, the following code snippet consists of two declarations for the variables `x` and `y` followed by an expression to form a single program:

``` motoko
let x = 1;
let y = x + 1;
x * y + x;
```

This program’s type is [`Nat`](../base/Nat.md) (natural number), and when run, it evaluates to the [`Nat`](../base/Nat.md) value of `3`.

This program is a declaration list that consists of three declarations:

1.  Immutable variable `x`, via declaration `let x = 1;`.

2.  Immutable variable `y`, via declaration `let y = x + 1;`.

3.  An unnamed, implicit variable holding the final expression’s value, `x * y + x`.

The expression `x * y + x` illustrates that each expression can be thought of as a declaration where necessary, since the language implicitly declares an unnamed variable with that expression’s result value.

When the expression appears as the final declaration, this expression may have any type. Here, the expression `x * y + x` has type [`Nat`](../base/Nat.md).

Expressions that do not appear at the end, but rather within the list of declarations must have unit type `()`.

Unit-type restrictions can be ignored by explicitly using the `ignore` keyword to disregard any unused result values.

## Lexical scoping

A declaration list is not itself an expression, so you cannot declare another variable with its final value.

**Block expressions** can be formed from a list of declarations by enclosing it with matching curly braces. Blocks are only allowed as sub-expressions of control flow expressions like `if`, `loop`, `case`, etc. A block expression produces a value and, when enclosed in parentheses, can occur within some larger, compound expression.

:::note

A particular form of blocks are provided for convenience when processing data that may be missing or incomplete. These are described under [option blocks](../writing-motoko/pattern-matching.md#option-blocks-for-streamlined-processing-of-optional-data).

:::

In all other places, `do { … }` is used to represent block expressions and distinguish blocks from object literals. For example, `do {}` is the empty block of type `()`, while `{}` is an empty record of record type `{}`. This block form preserves the autonomy of the declaration list and its choice of variable names. This means that variables' scopes may nest, but they may not interfere as they nest. Language theorists call this idea **lexical scoping**.

Aside from mathematical clarity, the chief practical benefit of lexical scoping is security and its use in building compositionally-secure systems. Specifically, Motoko gives very strong composition properties. For example, nesting your program within a program you do not trust cannot arbitrarily redefine your variables with different meanings.

## Type soundness

Each Motoko expression that type checks is considered **well-typed**. The **type** of a Motoko expression serves as a promise from the language to the developer about the future behavior of the program, if executed.

First, each well-typed program will evaluate without undefined behavior. There is a precise space of meaningful programs. The type system enforces that programs stay within it and that all well-typed programs have a precise meaning.

Furthermore, the types make a precise prediction over the program’s result. If it yields control, the program will generate a **result value** that agrees with that of the original program.

In either case, the static and dynamic views of the program are linked by and agree with the static type system. This agreement is the central principle of a static type system, and is delivered by Motoko as a core aspect of its design.

The same type system also enforces that asynchronous interactions agree between static and dynamic views of the program, and that the resulting messages generated never mismatch at runtime. This agreement is similar in spirit to the caller/callee argument type and return type agreements that one ordinarily expects in a typed language.

## Type annotations and variables

Variables relate static names and static types with dynamic values that are present only at runtime.

In this sense, Motoko types provide a form of trusted, compiler-verified documentation in the program source code.

Consider this very short program:

``` motoko
let x : Nat = 1
```

In this example, the compiler infers that the expression `1` has type [`Nat`](../base/Nat.md), and that `x` has the same type.

In this case, we can omit this annotation without changing the meaning of the program:

``` motoko
let x = 1
```

Except for some esoteric situations involving operator overloading, type annotations do not typically affect the meaning of the program as it runs. If they are omitted and the compiler accepts the program, as is the case above, the program has the same meaning and behavior as it did originally.

However, sometimes type annotations are required by the compiler to infer other assumptions and to check the program as a whole. When they are added and the compiler still accepts the program, you can confirm that the added annotations are consistent with the existing ones.

## Type errors and messages

Motoko is a statically typed programming language. That means that the compiler will reject any program that contains obviously non-sensical code.

For example, while adding two numbers is allowed, adding a number to a text value makes no sense to the Motoko compiler and is flagged as an error that must be fixed before the code can be run or deployed.

The rules that Motoko applies to verify the correctness of code before compiling and running it are called its type system. Motoko's type system will detect and reject static errors such as applying a function to the wrong number of arguments, or to arguments of the wrong type.

The type system is a safety feature that prevents a slew of errors that would otherwise have to be detected and reported at runtime, when they would be difficult or impossible to address.


## Motoko base library

For various practical language engineering reasons, the design of Motoko strives to minimize built-in types and operations.

Instead, whenever possible, the Motoko base library provides the types and operations that make the language feel complete. However, this base library is still under development, and is still incomplete.

The [Motoko base library](../base/index.md) lists a selection of modules, focusing on core features used in the examples that are unlikely to change radically. The base library APIs will likely evolve over time and in particular, grow in size and number as Motoko matures.

To import from the base library, use the `import` keyword. Give a local module name to introduce, in this example `D` for “**D**ebug”, and a URL where the `import` declaration may locate the imported module:

``` motoko file=../examples/print.mo
```

In this case, we import Motoko code with the `mo:` prefix, specify the `base/` path, followed by the module’s file name `Debug.mo` minus its extension.

## Traps

Some errors, such as division by zero, out-of-bounds array indexing, and pattern match failure are by design not prevented by the type system, but instead cause dynamic faults called **traps**.

Because the meaning of execution is ill-defined after a faulting trap, execution of the code ends by aborting at the trap.

:::note

Traps that occur within actor messages are more subtle: they don’t abort the entire actor, but prevent that particular message from proceeding, rolling back any yet uncommitted state changes. Other messages on the actor will continue execution. This has subtle security implications, so be sure to consult the relevant [security recommendations](https://internetcomputer.org/docs/current/developer-docs/security/security-best-practices/inter-canister-calls#recommendation).

:::

Occasionally it can be useful to force an unconditional trap, with a user-defined message.

The [`Debug`](../base/Debug.md) library provides the function `trap(t)` for this purpose, which can be used in any context:

``` motoko
import Debug "mo:base/Debug";

Debug.trap("oops!");
```

**Assertions** allow you to conditionally trap when some Boolean test fails to hold, but continue execution otherwise:

``` motoko no-repl
let n = 65535;
assert n % 2 == 0; // traps when n not even
```

Because an assertion may succeed, and thus proceed with execution, it may only be used in context where a value of type `()` is expected.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
