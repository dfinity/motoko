# Language quick reference

<!--
* targetting release 0.5.4
* [X] Categorise primitives and operations as arithmetic (A), logical (L), bitwise (B) and relational (R) and use these categories to concisely present categorized operators (unop, binop, relop, a(ssigning)op) etc.
* [ ] Various inline TBCs and TBRs and TODOs
* [ ] Typing of patterns
* [X] Variants
* [X] Object patterns
* [X] Import expressions
* [X] Complete draft of Try/Throw expressions and primitive Error/ErrorCode type
* [ ] Prelude
* [ ] Modules and static restriction
* [X] Type components and paths
* [ ] Prelude (move scattered descriptions of assorted prims like charToText here)
* [X] Split category R into E (Equality) and O (Ordering) if we don't want Bool to support O. (Actually renamed R to O, and defined ==/!= on _shared_ types.
* [X] Include actual grammar (extracted from menhir) in appendix?
* [ ] Prose description of definedness checks
* [ ] Platform changes: remove async expressions (and perhaps types); restrict await to shared calls.
* [X] Queries
* [X] Remove Shared type
* [X] Explain dot keys, dot vals and iterators
* [X] Debug expressions
* [X] Document punning in type record patterns: https://github.com/dfinity/motoko/pull/964
* [X] Update ErrorCode section
* [Floats] Literals type and operations
* [ ] Re-section so headings appear in content outline
-->

This section serves as a technical reference for the previous chapters and has specific technical information for readers with specific interests. For example, this section provides technical details of interest to the following audiences:

-   Authors providing the higher-level documentation about the Motoko programming language.

-   Compiler experts interested in the details of Motoko and its compiler.

-   Advanced programmers who want to learn more about the lower-level details of Motoko.

The language quick reference is intended to provide complete reference information about Motoko, but this section does *not* provide explanatory text or usage information. Therefore, this section is typically not suitable for readers who are new to programming languages or who are looking for a general introduction to using Motoko.

Throughout, we use the term canister to refer to an Internet Computer canister smart contract.

## Basic language syntax

This section describes the basic language conventions you need to know for programming in Motoko.

### Whitespace

Space, newline, horizontal tab, carriage return, line feed and form feed are considered as whitespace. Whitespace is ignored but used to separate adjacent keywords, identifiers and operators.

In the definition of some lexemes, the quick reference uses the symbol `␣` to denote a single whitespace character.

### Comments

Single line comments are all characters following `//` until the end of the same line.

``` motoko
// single line comment
x = 1
```

Single or multi-line comments are any sequence of characters delimited by `/*` and `*/`:

``` motoko
/* multi-line comments
   look like this, as in C and friends */
```

Comments delimited by `/*` and `*/` may be nested, provided the nesting is well-bracketed.

``` motoko
/// I'm a documentation comment
/// for a function
```

Documentation comments start with `///` followed by a space until the end of line, and get attached to the definition immediately following them.

Deprecation comments start with `/// @deprecated` followed by a space until the end of line, and get attached to the definition immediately following them. They are only recognized in front of `public` declarations.

All comments are treated as whitespace.

### Keywords

The following keywords are reserved and may not be used as identifiers:

``` bnf
actor and assert async await break case catch class continue debug
debug_show do else flexible false for from_candid func if ignore import
in module not null object or label let loop private public query return
shared stable switch system throw to_candid true try type var while with
```

### Identifiers

Identifiers are alpha-numeric, start with a letter and may contain underscores:

``` bnf
<id>   ::= Letter (Letter | Digit | _)*
Letter ::= A..Z | a..z
Digit  ::= 0..9
```

### Integers

Integers are written as decimal or hexadecimal, `Ox`-prefixed natural numbers. Subsequent digits may be prefixed a single, semantically irrelevant, underscore.

``` bnf
digit ::= ['0'-'9']
hexdigit ::= ['0'-'9''a'-'f''A'-'F']
num ::= digit ('_'? digit)*
hexnum ::= hexdigit ('_'? hexdigit)*
nat ::= num | "0x" hexnum
```

Negative integers may be constructed by applying a prefix negation `-` operation.

### Floats

Floating point literals are written in decimal or `Ox`-prefixed hexadecimal scientific notation.

``` bnf
let frac = num
let hexfrac = hexnum
let float =
    num '.' frac?
  | num ('.' frac?)? ('e' | 'E') sign? num
  | "0x" hexnum '.' hexfrac?
  | "0x" hexnum ('.' hexfrac?)? ('p' | 'P') sign? num
```

The 'e' (or 'E') prefixes a base 10, decimal exponent; 'p' (or 'P') prefixes a base 2, binary exponent. In both cases, the exponent is in decimal notation.

:::note

the use of decimal notation, even for the base 2 exponent, is in keeping with the established hexadecimal floating point literal syntax of the `C` language.

:::

### Characters

A character is a single quote (`'`) delimited:

-   Unicode character in UTF-8,

-   `\`-escaped newline, carriage return, tab, single or double quotation mark

-   `\`-prefixed ASCII character (TBR),

-   or `\u{` hexnum `}` enclosed valid, escaped Unicode character in hexadecimal (TBR).

``` bnf
ascii ::= ['\x00'-'\x7f']
ascii_no_nl ::= ['\x00'-'\x09''\x0b'-'\x7f']
utf8cont ::= ['\x80'-'\xbf']
utf8enc ::=
    ['\xc2'-'\xdf'] utf8cont
  | ['\xe0'] ['\xa0'-'\xbf'] utf8cont
  | ['\xed'] ['\x80'-'\x9f'] utf8cont
  | ['\xe1'-'\xec''\xee'-'\xef'] utf8cont utf8cont
  | ['\xf0'] ['\x90'-'\xbf'] utf8cont utf8cont
  | ['\xf4'] ['\x80'-'\x8f'] utf8cont utf8cont
  | ['\xf1'-'\xf3'] utf8cont utf8cont utf8cont
utf8 ::= ascii | utf8enc
utf8_no_nl ::= ascii_no_nl | utf8enc

escape ::= ['n''r''t''\\''\'''\"']

character ::=
  | [^'"''\\''\x00'-'\x1f''\x7f'-'\xff']
  | utf8enc
  | '\\'escape
  | '\\'hexdigit hexdigit
  | "\\u{" hexnum '}'

char := '\'' character '\''
```

### Text

A text literal is `"`-delimited sequence of characters:

``` bnf
text ::= '"' character* '"'
```

### Literals

``` bnf
<lit> ::=                                     literals
  <nat>                                         natural
  <float>                                       float
  <char>                                        character
  <text>                                        Unicode text
```

Literals are constant values. The syntactic validity of a literal depends on the precision of the type at which it is used.

## Operators and types

To simplify the presentation of available operators, operators and primitive types are classified into basic categories:

| Abbreviation | Category   | Supported opertions             |
|--------------|------------|---------------------------------|
| A            | Arithmetic | arithmetic operations           |
| L            | Logical    | logical/Boolean operations      |
| B            | Bitwise    | bitwise and wrapping operations |
| O            | Ordered    | comparison                      |
| T            | Text       | concatenation                   |

Some types have several categories. For example, type `Int` is both arithmetic (A) and ordered (O) and supports both arithmetic addition (`+`) and relational less than (`<`) (amongst other operations).

### Unary operators

| `<unop>` | Category |                  |
|----------|----------|------------------|
| `-`      | A        | numeric negation |
| `+`      | A        | numeric identity |
| `^`      | B        | bitwise negation |
| `!`      |          | null break       |

### Relational operators

|           |          |                                                 |
|-----------|----------|-------------------------------------------------|
| `<relop>` | Category |                                                 |
| `==`      |          | equals                                          |
| `!=`      |          | not equals                                      |
| `␣<␣`     | O        | less than *(must be enclosed in whitespace)*    |
| `␣>␣`     | O        | greater than *(must be enclosed in whitespace)* |
| `<=`      | O        | less than or equal                              |
| `>=`      | O        | greater than or equal                           |

Note that equality (`==`) and inequality (`!=`) do not have categories. Instead, equality and inequality are applicable to arguments of all *shared* types, including non-primitive, compound types such as immutable arrays, records, and variants.

Equality and inequality are structural and based on the observable content of their operands (as determined by their static type).

### Numeric binary operators

| `<binop>` | Category |                |
|-----------|----------|----------------|
| `+`       | A        | addition       |
| `-`       | A        | subtraction    |
| `*`       | A        | multiplication |
| `/`       | A        | division       |
| `%`       | A        | modulo         |
| `**`      | A        | exponentiation |

### Bitwise and wrapping binary operators

| `<binop>` | Category |                                                |
|-----------|----------|------------------------------------------------|
| `&`       | B        | bitwise and                                    |
| `\|`      | B        | bitwise or                                     |
| `^`       | B        | exclusive or                                   |
| `<<`      | B        | shift left                                     |
| `␣>>`     | B        | shift right *(must be preceded by whitespace)* |
| `<<>`     | B        | rotate left                                    |
| `<>>`     | B        | rotate right                                   |
| `+%`      | A        | addition (wrap-on-overflow)                    |
| `-%`      | A        | subtraction (wrap-on-overflow)                 |
| `*%`      | A        | multiplication (wrap-on-overflow)              |
| `**%`     | A        | exponentiation (wrap-on-overflow)              |

### Text operators

| `<binop>` | Category |               |
|-----------|----------|---------------|
| `#`       | T        | concatenation |

### Assignment operators

| `:=`, `<unop>=`, `<binop>=` | Category |                                            |
|-----------------------------|----------|--------------------------------------------|
| `:=`                        | \*       | assignment (in place update)               |
| `+=`                        | A        | in place add                               |
| `-=`                        | A        | in place subtract                          |
| `*=`                        | A        | in place multiply                          |
| `/=`                        | A        | in place divide                            |
| `%=`                        | A        | in place modulo                            |
| `**=`                       | A        | in place exponentiation                    |
| `&=`                        | B        | in place logical and                       |
| `\|=`                       | B        | in place logical or                        |
| `^=`                        | B        | in place exclusive or                      |
| `<<=`                       | B        | in place shift left                        |
| `>>=`                       | B        | in place shift right                       |
| `<<>=`                      | B        | in place rotate left                       |
| `<>>=`                      | B        | in place rotate right                      |
| `+%=`                       | B        | in place add (wrap-on-overflow)            |
| `-%=`                       | B        | in place subtract (wrap-on-overflow)       |
| `*%=`                       | B        | in place multiply (wrap-on-overflow)       |
| `**%=`                      | B        | in place exponentiation (wrap-on-overflow) |
| `#=`                        | T        | in place concatenation                     |

The category of a compound assignment `<unop>=`/`<binop>=` is given by the category of the operator `<unop>`/`<binop>`.

### Operator and keyword precedence

The following table defines the relative precedence and associativity of operators and tokens, ordered from lowest to highest precedence. Tokens on the same line have equal precedence with the indicated associativity.

| Precedence | Associativity | Token                                                                                                                         |
|------------|---------------|-------------------------------------------------------------------------------------------------------------------------------|
| LOWEST     | none          | `if _ _` (no `else`), `loop _` (no `while`)                                                                                   |
| (higher)   | none          | `else`, `while`                                                                                                               |
| (higher)   | right         | `:=`, `+=`, `-=`, `*=`, `/=`, `%=`, `**=`, `#=`, `&=`, `\|=`, `^=`, `<<=`, `>>=`, `<<>=`, `<>>=`, `+%=`, `-%=`, `*%=`, `**%=` |
| (higher)   | left          | `:`                                                                                                                           |
| (higher)   | left          | `or`                                                                                                                          |
| (higher)   | left          | `and`                                                                                                                         |
| (higher)   | none          | `==`, `!=`, `<`, `>`, `<=`, `>`, `>=`                                                                                         |
| (higher)   | left          | `+`, `-`, `#`, `+%`, `-%`                                                                                                     |
| (higher)   | left          | `*`, `/`, `%`, `*%`                                                                                                           |
| (higher)   | left          | `\|`                                                                                                                          |
| (higher)   | left          | `&`                                                                                                                           |
| (higher)   | left          | `^`                                                                                                                           |
| (higher)   | none          | `<<`, `>>`, `<<>`, `<>>`                                                                                                      |
| HIGHEST    | left          | `**`, `**%`                                                                                                                   |

### Programs

The syntax of a *program* `<prog>` is as follows:

``` bnf
<prog> ::=             programs
  <imp>;* <dec>;*
```

A program is a sequence of imports `<imp>;*` followed by a sequence of declarations `<dec>;*` that ends with an optional actor or actor class declaration. The actor or actor class declaration determines the main actor, if any, of the program.

For now, compiled programs must obey the following additional restrictions (not imposed on interpreted programs):

-   a `shared` function can only appear as a public field of an actor or actor class;

-   a program may contain at most one actor or actor class declaration, i.e. the final main actor or actor class; and

-   any main actor class declaration should be *anonymous*; if named, the class name should not be used as a value within the class and will be reported as an unavailable identifier.

The last two restrictions are designed to forbid programmatic actor class recursion, pending compiler support.

Note that the parameters (if any) of an actor class must have shared type (see [Sharability](#sharability)). The parameters of a program’s final actor class provide access to the corresponding canister installation argument(s); the Candid type of this argument is determined by the Candid projection of the Motoko type of the class parameter.

### Imports

The syntax of an *import* `<imp>` is as follows:

``` bnf
<imp> ::= imports
  import <pat> =? <url>

<url> ::=
  "<filepath>"                      import module from relative <filepath>.mo
  "mo:<package-name>/<filepath>"    import module from package
  "canister:<canisterid>"           import external actor by <canisterid>
  "canister:<name>"                 import external actor by <name>
```

An import introduces a resource referring to a local source module, module from a package of modules, or canister (imported as an actor). The contents of the resource are bound to `<pat>`.

Though typically a simple identifier, `<id>`, `<pat>` can also be any composite pattern binding selective components of the resource.

The pattern must be irrefutable.

### Libraries

The syntax of a *library* (that can be referenced in an import) is as follows:

``` bnf
<lib> ::=                                               library
  <imp>;* module <id>? <obj-body>                         module
  <imp>;* <shared-pat>? actor class                       actor class
    <id> <typ-params>? <pat> (: <typ>)? <class-body>
```

A library `<lib>` is a sequence of imports `<imp>;*` followed by:

-   a named or anonymous (module) declaration; or

-   a named actor class declaration.

Libraries stored in `.mo` files may be referenced by `import` declarations.

In a module library, the optional name `<id>?` is only significant within the library and does not determine the name of the library when imported. Instead, the imported name of a library is determined by the `import` declaration, giving clients of the library the freedom to choose library names (e.g. to avoid clashes).

An actor class library, because it defines both a type constructor and a function with name `<id>`, is imported as a module defining both a type and a function named `<id>`. The name `<id>` is mandatory and cannot be omitted. An actor class constructor is always asynchronous, with return type `async T` where `T` is the inferred type of the class body. Because actor construction is asynchronous, an instance of an imported actor class can only be created in an asynchronous context (i.e. in the body of a (non-`query`) `shared` function or `async` expression).

### Declaration syntax

The syntax of a *declaration* is as follows:

``` bnf
<dec> ::=                                                               declaration
  <exp>                                                                  expression
  let <pat> = <exp>                                                      immutable
  var <id> (: <typ>)? = <exp>                                            mutable
  <sort> <id>? =? <obj-body>                                             object
  <shared-pat>? func <id>? <typ-params>? <pat> (: <typ>)? =? <exp>       function
  type <id> <typ-params>? = <typ>                                        type
  <shared-pat>? <sort>? class                                            class
    <id>? <typ-params>? <pat> (: <typ>)? <class-body>

<obj-body> ::=           object body
  { <dec-field>;* }       field declarations

<class-body> ::=         class body
  = <id>? <obj-body>     object body, optionally binding <id> to 'this' instance
  <obj-body>             object body
```

The syntax of a shared function qualifier with call-context pattern is as follows:

``` bnf
<shared-pat> ::=
  shared query? <pat>?
```

For `<shared-pat>`, an absent `<pat>?` is shorthand for the wildcard pattern `_`.

``` bnf
<dec-field> ::=                                object declaration fields
  <vis>? <stab>? <dec>                           field

<vis> ::=                                      field visibility
  public
  private
  system

<stab> ::=                                     field stability (actor only)
  stable
  flexible
```

The *visibility* qualifier `<vis>?` determines the accessibility of every field `<id>` declared by `<dec>`:

-   An absent `<vis>?` qualifier defaults to `private` visibility.

-   Visibility `private` restricts access to `<id>` to the enclosing object, module or actor.

-   Visibility `public` extends `private` with external access to `<id>` using the dot notation `<exp>.<id>`.

-   Visibility `system` extends `private` with access by the run-time system.

-   Visibility `system` *may only* appear on `func` declarations that are actor fields, and *must not* appear anywhere else.

The *stability* qualifier `<stab>` determines the *upgrade* behaviour of actor fields:

-   A stability qualifier *should* appear on `let` and `var` declarations that are actor fields. An absent stability qualifier defaults to `flexible`.

-   `<stab>` qualifiers must not appear on fields of objects or modules.

-   The pattern in a `stable let <pat> = <exp>` declaration must be *simple* where, a pattern `pat` is simple if it (recursively) consists of

    -   a variable pattern `<id>`, or

    -   an annotated simple pattern `<pat> : <typ>`, or

    -   a parenthesized simple pattern `( <pat> )`.

### Expression syntax

The syntax of an *expression* is as follows:

``` bnf
<exp> ::=                                      expressions
  <id>                                           variable
  <lit>                                          literal
  <unop> <exp>                                   unary operator
  <exp> <binop> <exp>                            binary operator
  <exp> <relop> <exp>                            binary relational operator
  ( <exp>,* )                                    tuple
  <exp> . <nat>                                  tuple projection
  ? <exp>                                        option injection
  { <exp-field>;* }                              object
  { <exp> (and <exp>)* (with <exp-field>;+)? }   object combination/extension
  # id <exp>?                                    variant injection
  <exp> . <id>                                   object projection/member access
  <exp> := <exp>                                 assignment
  <unop>= <exp>                                  unary update
  <exp> <binop>= <exp>                           binary update
  [ var? <exp>,* ]                               array
  <exp> [ <exp> ]                                array indexing
  <shared-pat>? func <func_exp>                  function expression
  <exp> <typ-args>? <exp>                        function call
  not <exp>                                      negation
  <exp> and <exp>                                conjunction
  <exp> or <exp>                                 disjunction
  if <exp> <block-or-exp> (else <block-or-exp>)? conditional
  switch <exp> { (case <pat> <block-or-exp>;)+ } switch
  while <exp> <block-or-exp>                     while loop
  loop <block-or-exp> (while <exp>)?             loop
  for ( <pat> in <exp> ) <block-or-exp>          iteration
  label <id> (: <typ>)? <block-or-exp>           label
  break <id> <exp>?                              break
  continue <id>                                  continue
  return <exp>?                                  return
  async <block-or-exp>                           async expression
  await <block-or-exp>                           await future (only in async)
  throw <exp>                                    raise an error (only in async)
  try <block-or-exp> catch <pat> <block-or-exp>  catch an error (only in async)
  assert <block-or-exp>                          assertion
  <exp> : <typ>                                  type annotation
  <dec>                                          declaration
  ignore <block-or-exp>                          ignore value
  do <block>                                     block as expression
  do ? <block>                                   option block
  <exp> !                                        null break
  debug <block-or-exp>                           debug expression
  actor <exp>                                    actor reference
  to_candid ( <exp>,* )                          Candid serialization
  from_candid <exp>                              Candid deserialization
  (system <exp> . <id>)                          System actor class constructor
  ( <exp> )                                      parentheses

<block-or-exp> ::=
  <block>
  <exp>

<block> ::=
  { <dec>;* }
```

### Patterns

The syntax of a *pattern* is as follows:

``` bnf
<pat> ::=                                      patterns
  _                                              wildcard
  <id>                                           variable
  <unop>? <lit>                                  literal
  ( <pat>,* )                                    tuple or brackets
  { <pat-field>;* }                              object pattern
  # <id> <pat>?                                  variant pattern
  ? <pat>                                        option
  <pat> : <typ>                                  type annotation
  <pat> or <pat>                                 disjunctive pattern

<pat-field> ::=                                object pattern fields
  <id> (: <typ>) = <pat>                         field
  <id> (: <typ>)                                 punned field
```

## Type syntax

Type expressions are used to specify the types of arguments, constraints (a.k.a bounds) on type parameters, definitions of type constructors, and the types of sub-expressions in type annotations.

``` bnf
<typ> ::=                                     type expressions
  <path> <typ-args>?                            constructor
  <sort>? { <typ-field>;* }                     object
  { <typ-tag>;* }                               variant
  { # }                                         empty variant
  [ var? <typ> ]                                array
  Null                                          null type
  ? <typ>                                       option
  <shared>? <typ-params>? <typ> -> <typ>        function
  async <typ>                                   future
  ( ((<id> :)? <typ>),* )                       tuple
  Any                                           top
  None                                          bottom
  <typ> and <typ>                               intersection
  <typ> or <typ>                                union
  Error                                         errors/exceptions
  ( <typ> )                                      parenthesized type

<sort> ::= (actor | module | object)

<shared> ::=                                 shared function type qualifier
  shared query?

<path> ::=                                   paths
  <id>                                         type identifier
  <path> . <id>                                projection
```

An absent `<sort>?` abbreviates `object`.

### Primitive types

Motoko provides the following primitive type identifiers, including support for Booleans, signed and unsigned integers and machine words of various sizes, characters and text.

The category of a type determines the operators (unary, binary, relational and in-place update via assignment) applicable to values of that type.

| Identifier                          | Category | Description                                                            |
|-------------------------------------|----------|------------------------------------------------------------------------|
| [`Bool`](./base/Bool.md)           | L        | Boolean values `true` and `false` and logical operators                |
| [`Char`](./base/Char.md)           | O        | Unicode characters                                                     |
| [`Text`](./base/Text.md)           | T, O     | Unicode strings of characters with concatenation `_ # _` and iteration |
| [`Float`](./base/Float.md)         | A, O     | 64-bit floating point values                                           |
| [`Int`](./base/Int.md)             | A, O     | signed integer values with arithmetic (unbounded)                      |
| [`Int8`](./base/Int8.md)           | A, O     | signed 8-bit integer values with checked arithmetic                    |
| [`Int16`](./base/Int16.md)         | A, O     | signed 16-bit integer values with checked arithmetic                   |
| [`Int32`](./base/Int32.md)         | A, O     | signed 32-bit integer values with checked arithmetic                   |
| [`Int64`](./base/Int64.md)         | A, O     | signed 64-bit integer values with checked arithmetic                   |
| [`Nat`](./base/Nat.md)             | A, O     | non-negative integer values with arithmetic (unbounded)                |
| [`Nat8`](./base/Nat8.md)           | A, O     | non-negative 8-bit integer values with checked arithmetic              |
| [`Nat16`](./base/Nat16.md)         | A, O     | non-negative 16-bit integer values with checked arithmetic             |
| [`Nat32`](./base/Nat32.md)         | A, O     | non-negative 32-bit integer values with checked arithmetic             |
| [`Nat64`](./base/Nat64.md)         | A, O     | non-negative 64-bit integer values with checked arithmetic             |
| [`Blob`](./base/Blob.md)           | O        | binary blobs with iterators                                            |
| [`Principal`](./base/Principal.md) | O        | principals                                                             |
| [`Error`](./base/Error.md)         |          | (opaque) error values                                                  |

Although many of these types have linguistic support for literals and operators, each primitive type also has an eponymous base library providing related functions and values (see [Motoko Base Library](./base-intro.md)). For example, the [`Text`](./base/Text.md) library provides common functions on `Text` values.

### Type `Bool`

The type `Bool` of category L (Logical) has values `true` and `false` and is supported by one and two branch `if _ <exp> (else <exp>)?`, `not <exp>`, `_ and _` and `_ or _` expressions. Expressions `if`, `and` and `or` are short-circuiting.

<!--
TODO: Comparison.
-->

### Type `Char`

A `Char` of category O (Ordered) represents a character as a code point in the Unicode character set.

Base library function `Char.toNat32(c)` converts a `Char` value, `c` to its `Nat32` code point. Function `Char.fromNat32(n)` converts a `Nat32` value, `n`, in the range *0x0..xD7FF* or *0xE000..0x10FFFF* of valid code points to its `Char` value; this conversion traps on invalid arguments. Function `Char.toText(c)` converts the `Char` `c` into the corresponding, single character `Text` value.

### Type `Text`

The type `Text` of categories T and O (Text, Ordered) represents sequences of Unicode characters (i.e. strings). Function `t.size` returns the number of characters in `Text` value `t`. Operations on text values include concatenation (`_ # _`) and sequential iteration over characters via `t.chars` as in `for (c : Char in t.chars()) { …​ c …​ }`.

<!--
TODO: Comparison.
-->

### Type `Float`

The type `Float` represents 64-bit floating point values of categories A (Arithmetic) and O (Ordered).

The semantics of `Float` and its operations is in accordance with standard [IEEE 754-2019](https://ieeexplore.ieee.org/document/8766229) (See [References](#references)).

Common functions and values are defined in base library "base/Float".

### Types `Int` and `Nat`

The types `Int` and `Nat` are signed integral and natural numbers of categories A (Arithmetic) and O (Ordered).

Both `Int` and `Nat` are arbitrary precision, with only subtraction `-` on `Nat` trapping on underflow.

The subtype relation `Nat <: Int` holds, so every expression of type `Nat` is also an expression of type `Int` (but *not* vice versa). In particular, every value of type `Nat` is also a value of type `Int`, without change of representation.

### Bounded integers `Int8`, `Int16`, `Int32` and `Int64`

The types `Int8`, `Int16`, `Int32` and `Int64` represent signed integers with respectively 8, 16, 32 and 64 bit precision. All have categories A (Arithmetic), B (Bitwise) and O (Ordered).

Operations that may under- or overflow the representation are checked and trap on error.

The operations `+%`, `-%`, `*%` and `**%` provide access to wrap-around, modular arithmetic.

As bitwise types, these types support bitwise operations *and* (`&`), *or* (`|`) and *exclusive-or* (`^`). Further, they can be rotated left (`<<>`), right (`<>>`), and shifted left (`<<`), right (`>>`). The right-shift preserves the two’s-complement sign. All shift and rotate amounts are considered modulo the numbers’s bit width *n*.

Bounded integer types are not in subtype relationship with each other or with other arithmetic types, and their literals need type annotation if the type cannot be inferred from context, e.g. `(-42 : Int16)`.

The corresponding module in the base library provides conversion functions: Conversion to `Int`, checked and wrapping conversions from `Int` and wrapping conversion to the bounded natural type of the same size.

### Bounded naturals `Nat8`, `Nat16`, `Nat32` and `Nat64`

The types `Nat8`, `Nat16`, `Nat32` and `Nat64` represent unsigned integers with respectively 8, 16, 32 and 64 bit precision. All have categories A (Arithmetic), B (Bitwise) and O (Ordered).

Operations that may under- or overflow the representation are checked and trap on error.

The operations `+%`, `-%`, `*%` and `**%` provide access to the modular, wrap-on-overflow operations.

As bitwise types, these types support bitwise operations *and* (`&`), *or* (`|`) and *exclusive-or* (`^`). Further, they can be rotated left (`<<>`), right (`<>>`), and shifted left (`<<`), right (`>>`). The right-shift is logical. All shift and rotate amounts are considered modulo the number’s bit width *n*.

The corresponding module in the base library provides conversion functions: Conversion to `Int`, checked and wrapping conversions from `Int` and wrapping conversion to the bounded natural type of the same size.

### Type `Blob`

The type `Blob` of category O (Ordered) represents binary blobs or sequences of bytes. Function `b.size` returns the number of characters in `Blob` value `b`. Operations on blob values include sequential iteration over bytes via function `b.vals` as in `for (v : Nat8 in b.vals()) { …​ v …​ }`.

### Type `Principal`

The type `Principal` of category O (Ordered) represents opaque principals such as canisters and users that can, for example, be used to identify callers of shared functions and used for simple authentication. Although opaque, principals may be converted to binary `Blob` values for more efficient hashing and other applications (see module `Principal` from the base library).

### Error type

Assuming base library import,

``` motoko no-repl
import E "mo:base/Error";
```

Errors are opaque values constructed and examined with operations:

-   `E.reject : Text -> Error`

-   `E.code : Error -> E.ErrorCode`

-   `E.message : Error -> Text`

Type `E.ErrorCode` is equivalent to variant type:

``` motoko no-repl
type ErrorCode = {
  // Fatal error.
  #system_fatal;
  // Transient error.
  #system_transient;
  // Destination invalid.
  #destination_invalid;
  // Explicit reject by canister code.
  #canister_reject;
  // Canister trapped.
  #canister_error;
  // Future error code (with unrecognized numeric code)
  #future : Nat32;
};
```

A constructed error `e = E.reject(t)` has `E.code(e) = #canister_reject` and `E.message(e) = t`.

`Error` values can be thrown and caught within an `async` expression or `shared` function (only). See [Throw](#throw) and [Try](#try).

Errors with codes other than `#canister_reject` (i.e. *system* errors) may be caught and thrown, but not user-constructed.

:::note

Exiting an async block or shared function with a non-`#canister-reject` system error exits with a copy of the error with revised code `#canister_reject` and the original `Text` message. This prevents programmatic forgery of system errors.

:::

### Constructed types

`<path> <typ-args>?` is the application of a type identifier or path, either built-in (i.e. `Int`) or user defined, to zero or more type **arguments**. The type arguments must satisfy the bounds, if any, expected by the type constructor’s type parameters (see [Well-formed types](#well-formed-types)).

Though typically a type identifier, more generally, `<path>` may be a `.`-separated sequence of actor, object or module identifiers ending in an identifier accessing a type component of a value (for example, `Acme.Collections.List`).

### Object types

`<sort>? { <typ-field>;* }` specifies an object type by listing its zero or more named *type fields*.

Within an object type, the names of fields must be distinct (both by name and hash value).

Object types that differ only in the ordering of the fields are equivalent.

When `<sort>?` is `actor`, all fields have `shared` function type (specifying messages).

### Variant types

`{ <typ-tag>;* }` specifies a variant type by listing its variant type fields as a sequence of `<typ-tag>`s.

Within a variant type, the tags of its variants must be distinct (both by name and hash value).

Variant types that differ only in the ordering of their variant type fields are equivalent.

`{ # }` specifies the empty variant type.

### Array types

`[ var? <typ> ]` specifies the type of arrays with elements of type `<typ>`.

Arrays are immutable unless specified with qualifier `var`.

### Null type

The `Null` type has a single value, the literal `null`. `Null` is a subtype of the option `? T`, for any type `T`.

### Option types

`? <typ>` specifies the type of values that are either `null` or a proper value of the form `? <v>` where `<v>` has type `<typ>`.

### Function types

Type `<shared>? <typ-params>? <typ1> -> <typ2>` specifies the type of functions that consume (optional) type parameters `<typ-params>`, consume a value parameter of type `<typ1>` and produce a result of type `<typ2>`.

Both `<typ1>` and `<typ2>` may reference type parameters declared in `<typ-params>`.

If `<typ1>` or `<typ2>` (or both) is a tuple type, then the length of that tuple type determines the argument or result arity of the function type.

The optional `<shared>` qualifier specifies whether the function value is shared, which further constrains the form of `<typ-params>`, `<typ1>` and `<typ2>` (see [Sharability](#sharability) below).

(Note that a `<shared>` function may itself be `shared` or `shared query`, determining the persistence of its state changes.)

### Async types

`async <typ>` specifies a future producing a value of type `<typ>`.

Future types typically appear as the result type of a `shared` function that produces an `await`-able value.

### Tuple types

`( ((<id> :)? <typ>),* )` specifies the type of a tuple with zero or more ordered components.

The optional identifier `<id>`, naming its components, is for documentation purposes only and cannot be used for component access. In particular, tuple types that differ only in the names of components are equivalent.

The empty tuple type `()` is called the *unit type*.

### Any type

Type `Any` is the *top* type, i.e. the super-type of all types. All values have type `Any`.

### None type

Type `None` is the *bottom* type, a subtype of all other types. No value has type `None`.

As an empty type, `None` can be used to specify the impossible return value of an infinite loop or unconditional trap.

### Intersection type

The type expression `<typ1> and <typ2>` denotes the syntactic *intersection* between its two type operands, that is, the greatest type that is a subtype of both. If both types are incompatible, the intersection is `None`.

The intersection is *syntactic*, in that it does not consider possible instantiations of type variables. The intersection of two type variables is `None`, unless they are equal, or one is declared to be a (direct or indirect) subtype of the other.

### Union type

The type expression `<typ1> or <typ2>` denotes the syntactic *union* between its two type operands, that is, the smallest type that is a supertype of both. If both types are incompatible, the union is `Any`.

The union is *syntactic*, in that it does not consider possible instantiations of type variables. The union of two type variables is the union of their bounds, unless the variables are equal, or one is declared to be a (direct or indirect) subtype of the other.

### Parenthesized type

A function that takes an immediate, syntactic tuple of length *n \>= 0* as its domain or range is a function that takes (respectively returns) *n* values.

When enclosing the argument or result type of a function, which is itself a tuple type, `( <tuple-typ> )` declares that the function takes or returns a single (boxed) value of type `<tuple-type>`.

In all other positions, `( <typ> )` has the same meaning as `<typ>`.

### Type fields

``` bnf
<typ-field> ::=                               object type fields
  <id> : <typ>                                  immutable value
  var <id> : <typ>                              mutable value
  <id> <typ-params>? <typ1> : <typ2>            function value (short-hand)
  type <id> <typ-params>? = <typ>                type component
```

A type field specifies the name and type of a value field of an object, or the name and definition of a type component of an object. The value field names within a single object type must be distinct and have non-colliding hashes. The type component names within a single object type must also be distinct and have non-colliding hashes. Value fields and type components reside in separate name spaces and thus may have names in common.

`<id> : <typ>` specifies an *immutable* field, named `<id>` of type `<typ>`.

`var <id> : <typ>` specifies a *mutable* field, named `<id>` of type `<typ>`.

`type <id> <typ-params>? = <typ>` specifies a *type* component, with field name `<id>`, abbreviating (parameterized) type `<typ>`.

Unlike type declarations, a type component is not, in itself, recursive (though it may abbreviate an existing recursive type).
In particular, the name `<id>` is not bound in `<typ>` nor in any other fields of the enclosing object type. The name `<id>` only determiness the label to use when accessing the definition through a record of this type (using the dot notation).


### Variant type fields

``` bnf
<typ-tag> ::=                                 variant type fields
  # <id> : <typ>                                tag
  # <id>                                        unit tag (short-hand)
```

A variant type field specifies the tag and type of a single variant of an enclosing variant type. The tags within a single variant type must be distinct and have non-colliding hashes.

`# <id> : <typ>` specifies an (immutable) field, named `<id>` of type `<typ>`. `# <id>` is sugar for an (immutable) field, named `<id>` of type `()`.

### Sugar

When enclosed by an `actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field named `<id>` of `shared` function type `shared <typ-params>? <typ1> → <typ2>`.

When enclosed by a non-`actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field named `<id>` of ordinary function type `<typ-params>? <typ1> → <typ2>`.

### Type parameters

``` bnf
<typ-params> ::=                              type parameters
  < typ-param,* >
<typ-param>
  <id> <: <typ>                               constrained type parameter
  <id>                                        unconstrained type parameter
```

A type constructors, function value or function type may be parameterised by a vector of comma-separated, optionally constrained, type parameters.

`<id> <: <typ>` declares a type parameter with constraint `<typ>`. Any instantiation of `<id>` must subtype `<typ>` (at that same instantiation).

Syntactic sugar `<id>` declares a type parameter with implicit, trivial constraint `Any`.

The names of type parameters in a vector must be distinct.

All type parameters declared in a vector are in scope within its bounds.

### Type arguments

``` bnf
<typ-args> ::=                                type arguments
  < <typ>,* >
```

Type constructors and functions may take type arguments.

The number of type arguments must agree with the number of declared type parameters of the type constructor.

For a function, the number of type arguments, when provided, must agree with the number of declared type parameters of the function’s type. Note that type arguments in function applications can typically be omitted and inferred by the compiler.

Given a vector of type arguments instantiating a vector of type parameters, each type argument must satisfy the instantiated bounds of the corresponding type parameter.

### Well-formed types

A type `T` is well-formed only if (recursively) its constituent types are well-formed, and:

-   if `T` is `async U` then `U` is shared, and

-   if `T` is `shared query? U -> V`, `U` is shared and `V == ()` or `V == async W` with `W` shared, and

-   if `T` is `C<T0, …​, Tn>` where:

    -   a declaration `type C<X0 <: U0, Xn <: Un>  = …​` is in scope, and

    -   `Ti <: Ui[ T0/X0, …​, Tn/Xn ]`, for each `0 <= i <= n`.

-   if `T` is `actor { …​ }` then all fields in `…​` are immutable and have `shared` function type.

### Subtyping

Two types `T`, `U` are related by subtyping, written `T <: U`, whenever, one of the following conditions is true:

-   `T` equals `U` (reflexivity).

-   `U` equals `Any`.

-   `T` equals `None`.

-   `T` is a type parameter `X` declared with constraint `U`.

-   `T` is `Nat` and `U` is `Int`.

-   `T` is a tuple `(T0, …​, Tn)`, `U` is a tuple `(U0, …​, Un)`, and for each `0 <= i <= n`, `Ti <: Ui`.

-   `T` is an immutable array type `[ V ]`, `U` is an immutable array type `[ W ]` and `V <: W`.

-   `T` is a mutable array type `[ var V ]`, `U` is a mutable array type `[ var W ]` and `V == W`.

-   `T` is `Null` and `U` is an option type `? W` for some `W`.

-   `T` is `? V`, `U` is `? W` and `V <: W`.

-   `T` is a future `async V`, `U` is a future `async W`, and `V <: W`.

-   `T` is an object type `sort0 { fts0 }`, `U` is an object type `sort1 { fts1 }` and

    -   `sort0` == `sort1`, and, for all fields,

    -   if field `id : V` is in `fts0` then `id : W` is in `fts1` and `V <: W`, and

    -   if mutable field `var id : V` is in `fts0` then `var id : W` is in `fts1` and `V == W`.

        (That is, object type `T` is a subtype of object type `U` if they have same sort, every mutable field in `U` super-types the same field in `T` and every mutable field in `U` is mutable in `T` with an equivalent type. In particular, `T` may specify more fields than `U`.)

-   `T` is a variant type `{ fts0 }`, `U` is a variant type `{ fts1 }` and

    -   if field `# id : V` is in `fts0` then `# id : W` is in `fts1` and `V <: W`.

        (That is, variant type `T` is a subtype of variant type `U` if every field of `T` subtypes the same field of `U`. In particular, `T` may specify fewer variants than `U`.)

-   `T` is a function type `<shared>? <X0 <: V0, ..., Xn <: Vn> T1 -> T2`, `U` is a function type `<shared>? <X0 <: W0, ..., Xn <: Wn> U1 -> U2` and

    -   `T` and `U` are either both equivalently `<shared>?`, and

    -   assuming constraints `X0 <: W0, …​, Xn <: Wn` then

        -   for all `i`, `Wi == Vi`, and

        -   `U1 <: T1`, and

        -   `T2 <: U2`.

            (That is, function type `T` is a subtype of function type `U` if they have same `<shared>?` qualification, they have the same type parameters (modulo renaming) and assuming the bounds in `U`, every bound in `T` supertypes the corresponding parameter bound in `U` (contra-variance), the domain of `T` supertypes the domain of `U` (contra-variance) and the range of `T` subtypes the range of `U` (co-variance).)

-   `T` (respectively `U`) is a constructed type `C<V0, …​, Vn>` that is equal, by definition of type constructor `C`, to `W`, and `W <: U` (respectively `U <: W`).

-   For some type `V`, `T <: V` and `V <: U` (*transitivity*).

### Sharability

A type `T` is *shared* if it is

-   `Any` or `None`, or

-   a primitive type other than `Error`, or

-   an option type `? V` where `V` is shared, or

-   a tuple type `(T0, …​, Tn)` where all `Ti` are shared, or

-   an immutable array type `[V]` where `V` is shared, or

-   an `object` type where all fields are immutable and have shared type, or

-   a variant type where all tags have shared type, or

-   a shared function type, or

-   an `actor` type.

### Stability

Stability extends sharability to include mutable types. More precisely:

A type `T` is *stable* if it is

-   `Any` or `None`, or

-   a primitive type other than `Error`, or

-   an option type `? V` where `V` is stable, or

-   a tuple type `(T0, …​, Tn)` where all `Ti` are stable, or

-   a (mutable or immutable) array type `[var? V]` where `V` is stable, or

-   an `object` type where all fields have stable type, or

-   a variant type where all tags have stable type, or

-   a shared function type, or

-   an `actor` type.

This definition implies that every shared type is a stable type. The converse does not hold: there are types that are stable but not shared (notably types with mutable components).

The types of actor fields declared with the `stable` qualifier must have stable type.

The (current) value of such a field is preserved upon *upgrade*, whereas the values of other fields are reinitialized after an upgrade.

## Static and dynamic semantics

Below, we give a detailed account of the semantics of Motoko programs.

For each [expression form](#expression-syntax) and each [declaration form](#declaration-syntax), we summarize its semantics, both in static terms (based on typing) and dynamic terms (based on program evaluation).

### Programs

A program `<imp>;* <dec>;*` has type `T` provided:

-   `<dec>;*` has type `T` under the static environment induced by the imports in `<imp>;*`.

All type and value declarations within `<dec>;*` are mutually-recursive.

A program evaluates by (transitively) evaluating the imports, binding their values to the identifiers in `<imp>;*` and then evaluating the sequence of declarations in `<dec>;*`.

### Libraries

Restrictions on the syntactic form of modules means that libraries can have no side-effects.

The imports of a library are local and not re-exported in its interface.

Multiple imports of the same library can be safely deduplicated without loss of side-effects.

#### Module libraries

A library `<imp>;* module <id>? <obj-body>` is a sequence of imports `<import>;*` followed by a single module declaration.

A library has module type `T` provided

-   `module <id>? <obj-body>` has (module) type `T` under the static environment induced by the imports in `<import>;*`.

A module library evaluates by (transitively) evaluating its imports, binding their values to the identifiers in `<imp>;*` and then evaluating `module <id>? <obj-body>`.

#### Actor class libraries

The actor class library `<imp>;* <dec>` where `<dec>` is of the form `<shared-pat>? actor class <id> <typ-params>? <pat> (: <typ>)? <class-body>` has type:

``` bnf
module {
  type <id> = T;
  <id> : (U1,...,Un) -> async T
}
```

provided that:

-   the actor class declaration `<dec>` has function type `(U1, ...​, Un) -> async T` under the static environment induced by the imports in `<import>;*`.

Notice that the imported type of the function `<id>` must be asynchronous.

An actor class library evaluates by (transitively) evaluating its imports, binding their values to the identifiers in `<imp>;*`, and evaluating the (derived) module:

``` bnf
module {
  <dec>
}
```

On the Internet Computer, if this library is imported as identifier `Lib`, then calling `await Lib.<id>(<exp1>, ..., <expn>)`, installs a fresh instance of the actor class as an isolated IC canister, passing the values of `<exp1>`, ...​, `<expn>` as installation arguments, and returns a reference to a (remote) actor of *type* `Lib.<id>`, that is, `T`. Installation is (necessarily) asynchronous.


#### Actor class management

On the Internet Computer, the primary constructor of an imported actor class always creates a new principal and installs a fresh instance of the class as the code for that principal.
While that is one way to install a canister on the IC, it is not the only way.

To provide further control over the installation of actor classes, Motoko endows each imported actor class with an extra, secondary constructor, for use on the Internet Computer.
This constructor takes an additional first argument that tailors the installation. The constructor is only available via special syntax that stresses its
`system` functionality.

Given some actor class constructor:

``` motoko no-repl
Lib.<id> : (U1, ...​, Un) -> async T
```

Its secondary constructor is accessed as `(system Lib.<id>)` with typing:

``` motoko no-repl
(system Lib.<id>) :
  { #new : CanisterSettings;
    #install : Principal;
    #reinstall : actor {} ;
    #upgrade : actor {} }  ->
    (U1, ...​, Un) -> async T
```

where

``` motoko no-repl
  type CanisterSettings = {
     settings : ?{
        controllers : ?[Principal];
        compute_allocation : ?Nat;
        memory_allocation : ?Nat;
        freezing_threshold : ?Nat;
     }
  }
```


Calling `(system Lib.<id>)(<exp>)(<exp1>, ...​, <expn>)` uses the first argument `<exp>`, a variant value, to control the installation of the canister further. Arguments `(<exp1>, ..., <expn>)` are just the user-declared constructor arguments of types `U1, ..., Un` that would also be passed to the primary constructor.

If `<exp>` is
* `#new s`, where `s` has type `CanisterSettings`:
  the call creates a fresh Internet Computer principal `p`, with settings `s`, and installs the instance to principal `p`.
* `#install p`, where `p` has type `Principal`, the call installs the actor to an already created Internet Computer principal `p`. The principal must be empty (have no previously installed code) or the call will return an error.
* `#upgrade a`, where `a` has type (or supertype) `actor {}`, the call installs the instance as an _upgrade_ of actor `a`, using its current stable storage to initialize stable variables and stable memory
   of the new instance.
* `#reinstall a`, where `a` has type (or supertype) `actor {}`, reinstalls the instance over the existing actor `a`, discarding its stable variables and stable memory.

:::note

On the Internet Computer, calling the primary constructor `Lib.<id>` is equivalent to calling the secondary constructor `(system Lib.<id>)` with argument `(#new {settings = null})` (i.e. using default settings).

:::

:::note

On the Internet Computer, calls to `Lib.<id>` and  `(system Lib.<id>)(#new ...)` must be provisioned with enough cycles for the creation of a new principal. Other call variants will use the cycles of the already allocated principal or actor.

:::

:::danger

The use of `#upgrade a` may be unsafe. Motoko will currently not verify that the upgrade is compatible with the code currently installed at `a`. (A future extension may verify compatibilty with a dynamic check.)

The use of `#reinstall a` may be unsafe. Motoko cannot verify that the reinstall is compatible with the code currently installed in actor `a` (even with a dynamic check).
A change in interface may break any existing clients of `a`. The current state of `a` will be lost.

:::

### Imports and Urls

An import `import <pat> =? <url>` declares a pattern `<pat>` bound to the contents of the text literal `<url>`.

`<url>` is a text literal that designates some resource: a local library specified with a relative path, a named module from a named package, or an external canister, referenced either by numeric canister id or by a named alias, and imported as a Motoko actor.

In detail, if `<url>` is of the form:

-   `"<filepath>"` then `<pat>` is bound to the library module defined in file `<filepath>.mo`. `<filepath>` is interpreted relative to the absolute location of the enclosing file. Note the `.mo` extension is implicit and should *not* be included in `<url>`. For example, `import U "lib/Util"` defines `U` to reference the module in local file `./lib/Util`.

-   `"mo:<package-name>/<path>"` then `<pat>` is bound to the library module defined in file `<package-path>/<path>.mo` in directory `<package-path>` referenced by package alias `<package-name>`. The mapping from `<package-name>` to `<package-path>` is determined by a compiler command-line argument `--package <package-name> <package-path>`. For example, `import L "mo:base/List"` defines `L` to reference the `List` library in package alias `base`.

-   `"ic:<canisterid>"` then `<pat>` is bound to a Motoko actor whose Motoko type is determined by the canister’s IDL interface. The IDL interface of canister `<canisterid>` must be found in file `<actorpath>/<canisterid>.did`. The compiler assumes that `<actorpath>` is specified by command line argument `--actor-idl <actorpath>` and that file `<actorpath>/<canisterid>.did` exists. For example, `import C "ic:lg264-qjkae"` defines `C` to reference the actor with canister id `lg264-qjkae` and IDL file `lg264-qjkae.did`.

-   `"canister:<name>"` is a symbolic reference to canister alias `<name>`. The compiler assumes that the mapping of `<name>` to `<canisterid>` is specified by command line argument `--actor-alias <name> ic:<canisterid>`. If so, `"canister:<name>"` is equivalent to `"ic:<cansterid>"` (see above). For example, `import C "canister:counter"` defines `C` to reference the actor otherwise known as `counter`.

The case sensitivity of file references depends on the host operating system so it is recommended not to distinguish resources by filename casing alone.

(Remark: when building multi-canister projects with the DFINITY Canister SDK, Motoko programs can typically import canisters by alias (e.g. `import C "canister:counter"`), without specifying low-level canister ids (e.g. `import C "ic:lg264-qjkae"`). The SDK tooling takes care of supplying the appropriate command-line arguments to the Motoko compiler.)

(Remark: sensible choices for `<pat>` are identifiers, such as `Array`, or object patterns like `{ cons; nil = empty }`, which allow selective importing of individual fields, under original or other names.)

### Declaration fields

A declaration field `<vis>? <stab>? <dec>` defines zero or more fields of an actor or object, according to the set of variables defined by `<dec>`.

Any identifier bound by a `public` declaration appears in the type of enclosing object, module or actor and is accessible via the dot notation.

An identifier bound by a `private` or `system` declaration is excluded from the type of the enclosing object, module or actor and thus inaccessible.

The declaration field has type `T` provided:

-   `<dec>` has type `T`;

-   if `<stab>?` is `stable` then `T` must be a stable type (see [Stability](#stability)).

(Actor fields declared `flexible` (implicitly or explicitly) can have any type, but will not be preserved across upgrades.)

Sequences of declaration fields are evaluated in order by evaluating their constituent declarations, with the following exception:

During an upgrade only, the value of a `stable` declaration is obtained as follows:

-   if the stable declaration was previously declared stable in the retired actor, its initial value is inherited from the retired actor.

-   if the stable declaration was not declared stable in the retired actor, and is thus new, its value is obtained by evaluating `<dec>`.

For an upgrade to be safe:

-   every stable identifier declared with type `T` in the retired actor and declared stable and of type `U` in the replacement actor, must satisfy `T <: U`.

This condition ensures that every stable variable is either fresh, requiring initialization, or its value can be safely inherited from the retired actor. Note that stable variables may be removed across upgrades, or may simply be deprecated by an upgrade to type `Any`.

#### System fields

The declaration `<dec>` of a `system` field must be a manifest `func` declaration with one of the following names and types:

| name          | type                                                          | description         |
|---------------|---------------------------------------------------------------|---------------------|
| `heartbeat`   | `() -> async ()`                                              | heartbeat action    |
| `inspect`     | `{ caller : Principal; msg : <Variant>; arg : Blob } -> Bool` | message predicate   |
| `preupgrade`  | `() -> ()`                                                    | pre upgrade action  |
| `postupgrade` | `() -> ()`                                                    | post upgrade action |

-   `heartbeat`, when declared, is called on every Internet Computer subnet **heartbeat**, scheduling an asynchronous call to the `heartbeat` function. Due to its `async` return type, a heartbeat function may send messages and await results. The result of a heartbeat call, including any trap or thrown error, is ignored. The implicit context switch means that the time the heartbeat body is executed may be later than the time the heartbeat was issued by the subnet.

-   `inspect`, when declared, is called as a predicate on every Internet Computer ingress message (with the exception of HTTP query calls). The return value, a `Bool`, indicates whether to accept or decline the given message. The argument type depends on the interface of the enclosing actor (see [Inspect](#inspect)).

-   `preupgrade`, when declared, is called during an upgrade, immediately *before* the (current) values of the (retired) actor’s stable variables are transferred to the replacement actor.

-   `postupgrade`, when declared, is called during an upgrade, immediately *after* the (replacement) actor body has initialized its fields (inheriting values of the retired actors' stable variables), and before its first message is processed.

These `preupgrade` and `postupgrade` system methods provide the opportunity to save and restore in-flight data structures (e.g. caches) that are better represented using non-stable types.

During an upgrade, a trap occurring in the implicit call to `preupgrade()` or `postupgrade()` causes the entire upgrade to trap, preserving the pre-upgrade actor.

##### `inspect`

Given a record of message attributes, this function produces a `Bool` that indicates whether to accept or decline the message by returning `true` or `false`. The function is invoked (by the system) on each ingress message (excluding non-replicated queries). Similar to a query, any side-effects of an invocation are transient and discarded. A call that traps due to some fault has the same result as returning `false` (message denial).

The argument type of `inspect` depends on the interface of the enclosing actor. In particular, the formal argument of `inspect` is a record of fields of the following types:

-   `caller : Principal`: the principal, possibly anonymous, of the caller of the message;

-   `arg : Blob`: the raw, binary content of the message argument;

-   `msg : <variant>`: a variant of *decoding* functions, where `<variant> == {…​; #<id>: () → T; …​}` contains one variant per shared function, `<id>`, of the actor. The variant’s tag identifies the function to be called; The variant’s argument is a function that, when applied, returns the (decoded) argument of the call as a value of type `T`.

Using a variant, tagged with `#<id>`, allows the return type, `T`, of the decoding function to vary with the argument type (also `T`) of the shared function `<id>`.

The variant’s argument is a function so that one can avoid the expense of message decoding (when appropriate).

:::danger

An actor that fails to declare system field `inspect` will simply accept all ingress messages.

:::

### Sequence of declarations

A sequence of declarations `<dec>;*` occurring in a block, a program or embedded in the `<dec-field>;*` sequence of an object body has type `T` provided:

-   `<dec>;*` is empty and `T == ()`; or

-   `<dec>;*` is non-empty and:

-   all value identifiers bound by `<dec>;*` are distinct, and

-   all type identifiers bound by `<dec>;*` are distinct, and

-   under the assumption that each value identifier `<id>` in `<dec>;*` has type `var_id? Tid`, and assuming the type definitions in `<dec>;*`:

-   each declaration in `<dec>;*` is well-typed, and

-   each value identifier `<id>` in bindings produced by `<dec>;*` has type `var_id? Tid`, and

-   all but the last `<dec>` in `<dec>;*` of the form `<exp>` has type `()`;

-   the last declaration in `<dec>;*` has type `T`.

Declarations in `<dec>;*` are evaluated sequentially. The first declaration that traps causes the entire sequence to trap. Otherwise, the result of the declaration is the value of the last declaration in `<dec>;*`. In addition, the set of value bindings defined by `<dec>;*` is the union of the bindings introduced by each declaration in `<dec>;*`.

It is a compile-time error if any declaration in `<dec>;*` might require the value of an identifier declared in `<dec>;*` before that identifier’s declaration has been evaluated. Such *use-before-define* errors are detected by a simple, conservative static analysis not described here.

### Patterns

Patterns bind function parameters, declare identifiers and decompose values into their constituent parts in the cases of a `switch` expression.

Matching a pattern against a value may *succeed*, *binding* the corresponding identifiers in the pattern to their matching values, or *fail*. Thus the result of a match is either a successful binding, mapping identifiers of the pattern to values, or failure.

The consequences of pattern match failure depends on the context of the pattern.

-   In a function application or `let`-binding, failure to match the formal argument pattern or `let`-pattern causes a *trap*.

-   In a `case` branch of a `switch` expression, failure to match that case’s pattern continues with an attempt to match the next case of the switch, trapping only when no such case remains.

### Wildcard pattern

The wildcard pattern `_` matches a single value without binding its contents to an identifier.

### Identifier pattern

The identifier pattern `<id>` matches a single value and binds it to the identifier `<id>`.

### Literal pattern

The literal pattern `<unop>? <lit>` matches a single value against the constant value of literal `<lit>` and fails if they are not (structurally) equal values.

For integer literals only, the optional `<unop>` determines the sign of the value to match.

### Tuple pattern

The tuple pattern `( <pat>,* )` matches a n-tuple value against an n-tuple of patterns (both the tuple and pattern must have the same number of items). The set of identifiers bound by each component of the tuple pattern must be distinct.

The empty tuple pattern `()` is called the *unit pattern*.

Pattern matching fails if one of the patterns fails to match the corresponding item of the tuple value. Pattern matching succeeds if every pattern matches the corresponding component of the tuple value. The binding returned by a successful match is the disjoint union of the bindings returned by the component matches.

### Object pattern

The object pattern `{ <pat-field>;* }` matches an object value, a collection of named field values, against a sequence of named pattern fields. The set of identifiers bound by each field of the object pattern must be distinct. The names of the pattern fields in the object pattern must be distinct.

Object patterns support *punning* for concision. A punned field `<id>` is shorthand for `<id> = <id>`; Similarly, a typed, punned field `<id> : <typ>` is short-hand for `<id> = <id> : <typ>`. Both bind the matched value of the field named `<id>` to the identifier `<id>`.

Pattern matching fails if one of the pattern fields fails to match the corresponding field value of the object value. Pattern matching succeeds if every pattern field matches the corresponding named field of the object value. The binding returned by a successful match is the union of the bindings returned by the field matches.

The `<sort>` of the matched object type must be determined by an enclosing type annotation or other contextual type information.

### Variant pattern

The variant pattern `# <id> <pat>?` matches a variant value (of the form `# <id'> v`) against a variant pattern. An absent `<pat>?` is shorthand for the unit pattern (`()`). Pattern matching fails if the tag `<id'>` of the value is distinct from the tag `<id>` of the pattern (i.e. `<id>` \<\> `<id'>`); or the tags are equal but the value `v` does not match the pattern `<pat>?`. Pattern matching succeeds if the tag of the value is `<id>` (i.e. `<id'>` = `<id>`) and the value `v` matches the pattern `<pat>?`. The binding returned by a successful match is just the binding returned by the match of `v` against `<pat>?`.

### Annotated pattern

The annotated pattern `<pat> : <typ>` matches value of `v` type `<typ>` against the pattern `<pat>`.

`<pat> : <typ>` is *not* a dynamic type test, but is used to constrain the types of identifiers bound in `<pat>`, e.g. in the argument pattern to a function.

### Option pattern

The option `? <pat>` matches a value of option type `? <typ>`.

The match *fails* if the value is `null`. If the value is `? v`, for some value `v`, then the result of matching `? <pat>` is the result of matching `v` against `<pat>`.

Conversely, the `null` literal pattern may be used to test whether a value of option type is the value `null` and not `? v` for some `v`.

### Or pattern

The or pattern `<pat1> or <pat2>` is a disjunctive pattern.

The result of matching `<pat1> or <pat2>` against a value is the result of matching `<pat1>`, if it succeeds, or the result of matching `<pat2>`, if the first match fails.

(Note, statically, neither `<pat1>` nor `<pat2>` may contain identifier (`<id>`) patterns so a successful match always binds zero identifiers.)

### Expression declaration

The declaration `<exp>` has type `T` provided the expression `<exp>` has type `T` . It declares no bindings.

The declaration `<exp>` evaluates to the result of evaluating `<exp>` (typically for `<exp>`'s side-effect).

Note that if `<exp>` appears within a sequence of declarations, but not as the last declaration of that sequence, then `T` must be `()`.

<!--
TBR
-->

### Let declaration

The let declaration `let <pat> = <exp>` has type `T` and declares the bindings in `<pat>` provided:

-   `<exp>` has type `T`.

-   `<pat>` has type `T`.

The declaration `let <pat> = <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, the declaration evaluates to `trap`. If `r` is a value `v` then evaluation proceeds by matching the value `v` against `<pat>`. If matching fails, then the result is `trap`. Otherwise, the result is `v` and the binding of all identifiers in `<pat>` to their matching values in `v`.

All bindings declared by a `let` (if any) are *immutable*.

### Var declaration

The variable declaration `var <id> (: <typ>)? = <exp>` declares a *mutable* variable `<id>` with initial value `<exp>`. The variable’s value can be updated by assignment.

The declaration `var <id>` has type `()` provided:

-   `<exp>` has type `T`; and

-   If the annotation `(:<typ>)?` is present, then `T` == `<typ>`.

Within the scope of the declaration, `<id>` has type `var T` (see [Assignment](#assignment)).

Evaluation of `var <id> (: <typ>)? = <exp>` proceeds by evaluating `<exp>` to a result `r`. If `r` is `trap`, the declaration evaluates to `trap`. Otherwise, the `r` is some value `v` that determines the initial value of mutable variable `<id>`. The result of the declaration is `()` and `<id>` is bound to a fresh location that contains `v`.

### Type declaration

The declaration `type <id> <typ-params>? = <typ>` declares a new type constructor `<id>`, with optional type parameters `<typ-params>` and definition `<typ>`.

The declaration `type C< X0 <: T0, …​, Xn <: Tn > = U` is well-formed provided:

-   type parameters `X0`, …​, `Xn` are distinct, and

-   assuming the constraints `X0 <: T0`, …​, `Xn <: Tn`:

-   constraints `T0`, …​, `Tn` are well-formed.

-   definition `U` is well-formed.

-   it is productive (see [Productivity](#productivity)).

-   it is non-expansive (see [Expansiveness](#expansiveness)).

In scope of the declaration `type C< X0<:T0, …​, Xn <: Tn > = U`, any well-formed type `C< U0, …​, Un >` is equivalent to its expansion `U [ U0/X0, …​, Un/Xn ]`. Distinct type expressions that expand to identical types are inter-changeable, regardless of any distinction between type constructor names. In short, the equivalence between types is structural, not nominal.

#### Productivity

A type is *productive* if recursively expanding any outermost type constructor in its definition eventually produces a type other than the application of a type constructor.

Motoko requires all type declarations to be productive.

For example, the type definitions:

``` motoko no-repl
  type Person = { first : Text; last : Text };

  type List<T> = ?(T, List<T>);

  type Fst<T, U> = T;

  type Ok<T> = Fst<Any, Ok<T>>;
```

are all productive and legal.

But the type definitions,

``` motoko no-repl
  type C = C;

  type D<T, U> = D<U, T>;

  type E<T> = F<T>;
  type F<T> = E<T>;

  type G<T> = Fst<G<T>, Any>;
```

are all non-productive, since each definition will enter a loop after one or more expansions of its body.

#### Expansiveness

A set of mutually recursive type or class declarations will be rejected if the set is *expansive*.

Expansiveness is a syntactic criterion. To determine whether a set of singly or mutually recursive type definitions, say

``` motoko no-repl
  type C<...,Xi,...> = T;
  ...
  type D<...,Yj,...> = U;
```

is expansive, construct a directed graph whose vertices are the formal type parameters (identified by position), `C#i`, with the following `{0,1}`-labeled edges:

-   For each occurrence of parameter `C#i` as immediate, `j`-th argument to type `D<…​,C#i,…​>`, add a *non-expansive*, `0`-labeled edge,`C#i -0-> D#j`.

-   For each occurrence of parameter `C#i` as a proper sub-expression of the `j`-th argument to type `D<…​,T[C#i],..>` add an *expansive* `1`-labeled edge, `C#i -1-> D#j`.

The graph is expansive if, and only if, it contains a cycle with at least one expansive edge.

For example, the type definition:

``` motoko no-repl
  type List<T> = ?(T, List<T>);
```

that recursively instantiates `List` at the same parameter `T`, is non-expansive and accepted, but the similar looking definition:

``` motoko no-repl
  type Seq<T> = ?(T, Seq<[T]>);
```

that recursively instantiates `Seq` with a larger type, `[T]`, containing `T`, is *expansive* and rejected.

-   Type `List<T>` is non-expansive because its graph, `{ List#0 -0-> List#0 }`, though cyclic, has no expansive edge.

-   Type `Seq<T>`, on the other hand, is expansive, because its graph, `{ Seq#0 -1-> Seq#0 }`, has a cycle that includes an expansive edge.

### Object declaration

Declaration `<sort> <id>? <obj-body>`, where `<obj_body>` is of the form `=? { <dec-field>;* }`, declares an object with optional identifier `<id>` and zero or more fields `<dec-field>;*`. Fields can be declared with `public` or `private` visibility; if the visibility is omitted, it defaults to `private`.

The qualifier `<sort>` (one of `actor`, `module` or `object`) specifies the *sort* of the object’s type. The sort imposes restrictions on the types of the public object fields.

Let `T = <sort> { [var0] id0 : T0, …​ , [varn] idn : T0 }` denote the type of the object. Let `<dec>;*` be the sequence of declarations embedded in `<dec-field>;*`. The object declaration has type `T` provided that:

1.  type `T` is well-formed for sort `sort`, and

2.  under the assumption that `<id> : T`,

    -   the sequence of declarations `<dec>;*` has type `Any` and declares the disjoint sets of private and public identifiers, `Id_private` and `Id_public` respectively, with types `T(id)` for `id` in `Id == Id_private union Id_public`, and

    -   `{ id0, …​, idn } == Id_public`, and

    -   for all `i in 0 <= i <= n`, `[vari] Ti == T(idi)`.

3.  If `<sort>` is `module`, then the declarations in `<dec>;*` must be *static* (see [Static declarations](#static-declarations)).

Note that requirement 1. imposes further constraints on the field types of `T`. In particular, if the sort is `actor` then:

-   all public fields must be non-`var` (immutable) `shared` functions (the public interface of an actor can only provide asynchronous messaging via shared functions);

Because actor construction is asynchronous, an actor declaration can only occur in an asynchronous context (i.e. in the body of a (non-`query`) `shared` function or `async` expression).

Evaluation of `<sort>? <id>? =? { <dec-field>;* }` proceeds by binding `<id>` (if present), to the eventual value `v`, and evaluating the declarations in `<dec>;*`. If the evaluation of `<dec>;*` traps, so does the object declaration. Otherwise, `<dec>;*` produces a set of bindings for identifiers in `Id`. let `v0`, …​, `vn` be the values or locations bound to identifiers `<id0>`, …​, `<idn>`. The result of the object declaration is the object `v == sort { <id0> = v1, …​, <idn> = vn}`.

If `<id>?` is present, the declaration binds `<id>` to `v`. Otherwise, it produces the empty set of bindings.

:::danger

Actor declaration is (implicitly) asynchronous and the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes.

:::

#### Static declarations

A declaration is *static* if it is:

-   a `type` declaration, or

-   a `class` declaration, or

-   a `let` declaration with a static pattern and a static expression, or

-   a module, function or object declaration that desugars to a static `let` declaration, or

-   a static expression.

An expression is *static* if it is:

-   a literal expression, or

-   a tuple of static expressions, or

-   an object of static expressions, or

-   a variant or option with a static expression, or

-   an immutable array, or

-   field access and projection from a static expression, or

-   a module expression, or

-   a function expression, or

-   a static declaration, or

-   an `ignore` of a static expression, or

-   a block, all of whose declarations are static, or

-   a type annotation with a static expression.

A pattern is *static* if it is:

-   an identifier, or

-   a wildcard, or

-   a tuple of static patterns, or

-   type annotation with a static pattern.

<!--
why not record patterns?
-->

Static phrases are designed to be side-effect free, allowing the coalescing of duplicate library imports (a.k.a deduplication).

### Function declaration

The function declaration `<shared-pat>? func <id>? <typ-params>? <pat> (: <typ>)? =? <exp>` is syntactic sugar for a named `let` or anonymous declaration of a function expression.

That is, when `<id>?` is present and the function is named:

``` bnf
<shared-pat>? func <id> <typ-params>? <pat> (: <typ>)? =? <block-or-exp> :=
  let <id> = <shared-pat>? func <typ-params>? <pat> (: <typ>)? =? <block-or-exp>
```

But when `<id>?` is absent and the function is anonymous:

``` bnf
<shared-pat>? func <typ-params>? <pat> (: <typ>)? =? <block-or-exp> :=
  <shared-pat>? func <typ-params>? <pat> (: <typ>)? =? <block-or-exp>
```

Named function definitions support recursion (a named function can call itself).

:::note

In compiled code, `shared` functions can only appear as public actor fields.

:::

### Class declaration

The *class* declaration `<shared-pat>? <sort>? class <id>? <typ-params>? <pat> (: <typ>)? <class-body>` is sugar for pair of a type and function declaration:

``` bnf
<shared-pat>? <sort>? class <id> <typ-params>? <pat> (: <typ>)? <class-body> :=
  type <id> <typ-params> = <sort> { <typ-field>;* };
  <shared-pat>? func <id> <typ-params>? <pat> : async? <id> <typ-args> =
    async? <sort> <id_this>? <obj-body>
```

where:

-   `<shared-pat>?`, when present, requires `<sort>` == `actor`, and provides access to the `caller` of an `actor` constructor, and

-   `<typ-args>?` is the sequence of type identifiers bound by `<typ-params>?` (if any), and

-   `<typ-field>;*` is the set of public field types inferred from `<dec-field>;*`.

-   `<obj-body>` is the object body of `<class-body>`.

-   `<id_this>?` is the optional *this* (a.k.a *self*), parameter of `<class-body>`.

-   `async?` is present, if only if, `<sort>` == `actor`.

Note `<shared-pat>?` must not be of the form `shared query <pat>?`: a constructor, unlike a function, cannot be a query.

An absent `<shared-pat>?` defaults to `shared` when `sort` = `actor`.

If `sort` is `actor`, then:

-   `<typ-args>?` must be absent or empty (`actor` classes cannot have type parameters);

-   `<pat>`'s type must be shared (see [Sharability](#sharability)).

-   `(: <typ>)?`, if present, must be of the form `: async T` for some actor type `T` (actor instantiation is asynchronous).

If `(: <typ>)` is present, then the type `<async?> <sort> {  <typ_field>;* }` must be a subtype of the annotation `<typ>`. In particular, the annotation is used only to check, but not affect, the inferred type of function `<id>`.

The class declaration has the same type as function `<id>` and evaluates to the function value `<id>`.

### Identifiers

The identifier expression `<id>` has type `T` provided `<id>` is in scope, defined and declared with explicit or inferred type `T`.

The expression `<id>` evaluates to the value bound to `<id>` in the current evaluation environment.

### Literals

A literal has type `T` only when its value is within the prescribed range of values of type `T`.

The literal (or constant) expression `<lit>` evaluates to itself.

### Unary operators

The unary operator `<unop> <exp>` has type `T` provided:

-   `<exp>` has type `T`, and

-   The category of `<unop>` is a category of `T`.

The unary operator expression `<unop> <exp>` evaluates `<exp>` to a result. If the result is a value `v`, it returns the result of `<unop> v`. If the result is `trap`, the entire expression results in `trap`.

### Binary operators

The binary operator expression `<exp1> <binop> <exp2>` has type `T` provided:

-   `<exp1>` has type `T`, and

-   `<exp2>` has type `T`, and

-   The category of `<binop>` is a category of `T`.

The binary operator expression `<exp1> <binop> <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` and `r2` are values `v1` and `v2` and the expression returns the result of `v1 <binop> v2`.

### Relational operators

The relational expression `<exp1> <relop> <exp2>` has type `Bool` provided:

-   `<exp1>` has type `T`, and

-   `<exp2>` has type `T`, and

-   `<relop>` is equality `==` or inequality `!=`, `T` is *shared*, and `T` is the least type such that `<exp1>` and `<exp2>` have type `T`;

-   the category O (Ordered) is a category of `T` and `<relop>`; or

The binary operator expression `<exp1> <relop> <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` and `r2` are values `v1` and `v2` and the expression returns the Boolean result of `v1 <relop> v2`.

For equality and inequality, the meaning of `v1 <relop> v2` depends on the compile-time, static choice of `T` (not the run-time types of `v1` and `v2`, which, due to subtyping, may be more precise).

### Tuples

Tuple expression `(<exp1>, …​, <expn>)` has tuple type `(T1, …​, Tn)`, provided `<exp1>`, …​, `<expn>` have types `T1`, …​, `Tn`.

The tuple expression `(<exp1>, …​, <expn>)` evaluates the expressions `exp1` …​ `expn` in order, trapping as soon as some expression `<expi>` traps. If no evaluation traps and `exp1`, …​, `<expn>` evaluate to values `v1`,…​,`vn` then the tuple expression returns the tuple value `(v1, …​ , vn)`.

The tuple projection `<exp> . <nat>` has type `Ti` provided `<exp>` has tuple type `(T1, …​, Ti, …​, Tn)`, `<nat>` == `i` and `1 <= i <= n`.

The projection `<exp> . <nat>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a tuple `(v1,…​,vi,…​,vn)` and the result of the projection is the value `vi`.

The empty tuple expression `()` is called the *unit value*.

### Option expressions

The option expression `? <exp>` has type `? T` provided `<exp>` has type `T`.

The literal `null` has type `Null`. Since `Null <: ? T` for any `T`, literal `null` also has type `? T` and signifies the "missing" value at type `? T`.

### Variant injection

The variant injection `# <id> <exp>` has variant type `{# id T}` provided:

-   `<exp>` has type `T`.

The variant injection `# <id>` is just syntactic sugar for `# <id> ()`.

The variant injection `# <id> <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a value `v` and the result of the injection is the tagged value `# <id> v`.

The tag and contents of a variant value can be tested and accessed using a [variant pattern](#variant-pattern).

### Objects

Objects can be written in literal form `{ <exp-field>;* }`, consisting of a list of expression fields:

``` bnf
<exp-field> ::=                                object expression fields
  var? <id> (: <typ>) = <exp>                    field
  var? <id> (: <typ>)                            punned field
```

Such an object literal, sometimes called a *record*, is equivalent to the object declaration `object { <dec-field>;* }` where the declaration fields are obtained from the expression fields by prefixing each of them with `public let`, or just `public` in case of `var` fields. However, unlike declarations, the field list does not bind each `<id>` as a local name within the literal, i.e., the field names are *not* in scope in the field expressions.

Object expressions support *punning* for concision. A punned field `<id>` is shorthand for `<id> = <id>`; Similarly, a typed, punned field `<id> : <typ>` is short-hand for `<id> = <id> : <typ>`. Both associate the field named `<id>` with the value of the identifier `<id>`.

### Object combination/extension

Objects can be combined and/or extended using the `and` and `with` keywords.

A record expression `{ <exp> (and <exp>)* (with <exp-field>;+)? }` merges the objects (or modules) specified as *base* expressions, and augments the result to also contain the specified fields. The `with <exp-field>;+` clause can be omitted when at least two bases appear and none have common field labels.
Thus the field list serves to:

-   disambiguate field labels occurring more than once in the bases,
-   define new fields,
-   override existing fields and their types, and
-   add new `var` fields
-   redefine existing `var` fields from some base to prevent aliasing.

The resulting type is determined by the bases' (and explicitly given fields') static type.

Any `var` field from some base must be overwritten in the explicit field list. This prevents introducing aliases of `var` fields.

The record expression `{ <exp1> and ... <expn> with <exp-field1>; ... <exp_fieldn>; }` has type `T` provided:

-  The record `{ <exp-field1>; ... <exp_fieldm>; }` has record type `{ field_tys } == { var? <id1> : U1; ... var? <idm> : Um }`.

-  Let `newfields == { <id1> , ..., <idm> }` be the set of new field names.

-   Considering value fields:

    -   Base expression `<expi>` has object or module type `sorti { field_tysi } == sorti { var? <idi1> : Ti1, …​, var? <idik> : Tik }` where `sorti <> Actor`.

    Let `fields(i) == { <idi1>, ..., <idik> }` be the set of static field names of base `i`. Then

    -   `fields(i)` is disjoint from `newfields` (possibly by applying subtyping to the type of `<expi>`);
    -   no field in `field_tysi` is a `var` field;
    -  `fields(i)` is disjoint from `fields(j)` for `j < i`.

-   Considering type fields:

    -   Base expression `<expi>` has object or module type `sorti { typ_fieldsi } == sorti { type <idj1> = … , …, type <idik> = … }` where `sorti <> Actor`.
    -   `typ_fieldsi` _agrees_ with `typ_fieldsj` for `j < i`.

-   `T` is `{ typ_fieldsi fields_tys1 ... typ_fieldsm fields_tysm field_tys }`.

Here, two sequences of type fields _agree_ only when any two type fields of the same name in each sequence have equivalent definitions.

<!--
Note that the case for type fields is simpler than the value fields case only because the clause `with <exp-field1>; ... <exp_fieldn>` cannot contain type fields.
-->

The record expression `{ <exp1> and ... <expn> with <exp-field1>; ... <exp_fieldm>; }` evaluates records `<exp1>` through `<expn>` and `{ exp-field1; ... <exp_fieldm }` to results `r1` through `rn` and `r`, trapping on the first result that is a trap. If none of the expressions produces a trap, the results are objects `sort1 { f1 }`, `sortn { fn }` and `object { f }`, where `f1` ... `fn` and `f` are maps from identifiers to values or mutable locations.

The result of the entire expression is the value `object { g }` where `g` is the partial map with domain `fields(1) union fields(n) union newfields` mapping identifiers to unique
values or locations such that `g(<id>) = fi(<id>)` if `<id>` is in `fields(i)`, for some `i`, or `f(<id>)` if `<id>` is in `newfields`.

### Object projection (member access)

The object projection `<exp> . <id>` has type `var? T` provided `<exp>` has object type `sort { var1? <id1> : T1, …​, var? <id> : T, …​, var? <idn> : Tn }` for some sort `sort`.

The object projection `<exp> . <id>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be an object value `{ <id1> = v1,…​, id = v, …​, <idm> = vm }` and the result of the projection is the value `w` obtained from value or location `v` in field `id`.

If `var` is absent from `var? T` then the value `w` is just the value `v` of immutable field `<id>`, otherwise:

-   if the projection occurs as the target of an assignment expression then `w` is just `v`, the mutable location in field `<id>`.

-   otherwise, `w` (of type `T`) is the value currently stored at the mutable location `v` in field `<id>`.

### Special member access

The iterator access `<exp> . <id>` has type `T` provided `<exp>` has type `U`, and `U`,`<id>` and `T` are related by a row of the following table:

|            |         |                         |                                              |
|------------|---------|-------------------------|----------------------------------------------|
| U          | `<id>`  | T                       | Description                                  |
| `Text`     | `size`  | `Nat`                   | size (or length) in characters               |
| `Text`     | `chars` | `{ next: () -> Char? }` | character iterator, first to last            |
|            |         |                         |                                              |
| `Blob`     | `size`  | `Nat`                   | size in bytes                                |
| `Blob`     | `vals`  | `{ next: () -> Nat8? }` | byte iterator, first to last                 |
|            |         |                         |                                              |
| `[var? T]` | `size`  | `Nat`                   | number of elements                           |
| `[var? T]` | `get`   | `Nat -> T`              | indexed read function                        |
| `[var? T]` | `keys`  | `{ next: () -> Nat? }`  | index iterator, by ascending index           |
| `[var? T]` | `vals`  | `{ next: () -> T? }`    | value iterator, by ascending index           |
| `[var T]`  | `put`   | `(Nat, T) -> ()`        | indexed write function (mutable arrays only) |

The projection `<exp> . <id>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a value of type `U` and the result of the projection is a value of type `T` whose semantics is given by the Description column of the previous table.

:::note

the `chars`, `vals`, `keys` and `vals` members produce stateful **iterator objects** than can be consumed by `for` expressions (see [For](#for)).

:::

### Assignment

The assignment `<exp1> := <exp2>` has type `()` provided:

-   `<exp1>` has type `var T`, and

-   `<exp2>` has type `T`.

The assignment expression `<exp1> := <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1` and `r2` are (respectively) a location `v1` (a mutable identifier, an item of a mutable array or a mutable field of an object) and a value `v2`. The expression updates the current value stored in `v1` with the new value `v2` and returns the empty tuple `()`.

### Unary compound assignment

The unary compound assignment `<unop>= <exp>` has type `()` provided:

-   `<exp>` has type `var T`, and

-   `<unop>`'s category is a category of `T`.

The unary compound assignment `<unop>= <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap` the evaluation traps, otherwise `r` is a location storing value `v` and `r` is updated to contain the value `<unop> v`.

### Binary compound assignment

The binary compound assignment `<exp1> <binop>= <exp2>` has type `()` provided:

-   `<exp1>` has type `var T`, and

-   `<exp2>` has type `T`, and

-   `<binop>`'s category is a category of `T`.

For binary operator `<binop>`, the compound assignment expression `<exp1> <binop>= <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1` and `r2` are (respectively) a location `v1` (a mutable identifier, an item of a mutable array or a mutable field of object) and a value `v2`. The expression updates the current value, `w` stored in `v1` with the new value `w <binop> v2` and returns the empty tuple `()`.

### Arrays

The expression `[ var? <exp>,* ]` has type `[var? T]` provided each expression `<exp>` in the sequence `<exp>,*` has type T.

The array expression `[ var <exp0>, …​, <expn> ]` evaluates the expressions `exp0` …​ `expn` in order, trapping as soon as some expression `<expi>` traps. If no evaluation traps and `exp0`, …​, `<expn>` evaluate to values `v0`,…​,`vn` then the array expression returns the array value `[var? v0, …​ , vn]` (of size `n+1`).

### Array indexing

The array indexing expression `<exp1> [ <exp2> ]` has type `var? T` provided:

-   `<exp>` has (mutable or immutable) array type `[var? T1]`.

The expression `<exp1> [ <exp2> ]` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is an array value, `var? [v0, …​, vn]`, and `r2` is a natural integer `i`. If `i > n` the index expression returns `trap`.

Otherwise, the index expression returns the value `v`, obtained as follows:

If `var` is absent from `var? T` then the value `v` is the constant value `vi`.

Otherwise,

-   if the indexing occurs as the target of an assignment expression then `v` is the `i`-th mutable location in the array;

-   otherwise, `v` is `vi`, the value currently stored in the `i`-th location of the array.

### Function calls

The function call expression `<exp1> <T0,…​,Tn>? <exp2>` has type `T` provided:

-   the function `<exp1>` has function type `<shared>? < X0 <: V0, ..., Xn <: Vn > U1-> U2`; and

-   if `<T0,…​,Tn>?` is absent but n > 0 then there exists minimal `T0, …​, Tn` (inferred by the compiler) such that:

-   each type argument satisfies the corresponding type parameter’s bounds: for each `1 <= i <= n`, `Ti <: [T0/X0, …​, Tn/Xn]Vi`; and

-   the argument `<exp2>` has type `[T0/X0, …​, Tn/Xn]U1`, and

-   `T == [T0/X0, …​, Tn/Xn]U2`.

The call expression `<exp1> <T0,…​,Tn>? <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is a function value, `<shared-pat>? func <X0 <: V0, …​, n <: Vn> <pat1> { <exp> }` (for some implicit environment), and `r2` is a value `v2`. If `<shared-pat>` is present and of the form `shared query? <pat>` then evaluation continues by matching the record value `{caller = p}` against `<pat>`, where `p` is the `Principal` invoking the function (typically a user or canister). Matching continues by matching `v1` against `<pat1>`. If pattern matching succeeds with some bindings, then evaluation returns the result of `<exp>` in the environment of the function value (not shown) extended with those bindings. Otherwise, some pattern match has failed and the call results in `trap`.

:::note

The exhaustiveness side condition on `shared` function expressions ensures that argument pattern matching cannot fail (see [Functions](#functions)).

:::

### Functions

The function expression `<shared-pat>? func < X0 <: T0, …​, Xn <: Tn > <pat1> (: U2)? =? <block-or-exp>` has type `<shared>? < X0 <: T0, ..., Xn <: Tn > U1-> U2` if, under the assumption that `X0 <: T0, …​, Xn <: Tn`:

-   `<shared-pat>?` is of the form `shared query? <pat>` if and only if `<shared>?` is `shared query?` (the `query` modifiers must agree);

-   all the types in `T0, …​, Tn` and `U2` are well-formed and well-constrained;

-   pattern `<pat>` has *context type* `{ caller : Principal }`;

-   pattern `<pat1>` has type `U1`;

-   if the function is `shared` then `<pat>` and `<pat1>` must be exhaustive;

-   expression `<block-or-exp>` has type return type `U2` under the assumption that `<pat1>` has type `U1`.

`<shared-pat>? func <typ-params>? <pat1> (: <typ>)? =? <block-or-exp>` evaluates to a function value (a.k.a. closure), denoted `<shared-pat>? func <typ-params>? <pat1> = <exp>`, that stores the code of the function together with the bindings from the current evaluation environment (not shown) needed to evaluate calls to the function value.

Note that a `<shared-pat>` function may itself be `shared <pat>` or `shared query <pat>`:

-   A `shared <pat>` function may be invoked from a remote caller. Unless causing a trap, the effects on the callee persist beyond completion of the call.

-   A `shared query <pat>` function may be also be invoked from a remote caller, but the effects on the callee are transient and discarded once the call has completed with a result (whether a value or error).

In either case, `<pat>` provides access to a context value identifying the *caller* of the shared (query) function.

:::note

The context type is a record to allow extension with further fields in future releases.

:::

### Blocks

The block expression `{ <dec>;* }` has type `T` provided the last declaration in the sequence `<dec>;*` has type `T`. All identifiers declared in block must be distinct type identifiers or distinct value identifiers and are in scope in the definition of all other declarations in the block.

The bindings of identifiers declared in `{ dec;* }` are local to the block.

The type system ensures that a value identifier cannot be evaluated before its declaration has been evaluated, precluding run-time errors at the cost of rejection some well-behaved programs.

Identifiers whose types cannot be inferred from their declaration, but are used in a forward reference, may require an additional type annotation (see [Annotated pattern](#annotated-pattern)) to satisfy the type checker.

The block expression `{ <dec>;* }` evaluates each declaration in `<dec>;*` in sequence (program order). The first declaration in `<dec>;*` that results in a trap causes the block to result in `trap`, without evaluating subsequent declarations.

### Do

The do expression `do <block>` allows the use of a block as an expression, in positions where the syntax would not directly allow a block.

The expression `do <block>` has type `T` provided `<block>` has type `T`.

The `do` expression evaluates by evaluating `<block>` and returning its result.

### Option block

The option block `do ? <block>` introduces scoped handling of null values.

The expression `do ? <block>` has type `?T` provided `<block>` has type `T`.

The `do ? <block>` expression evaluates `<block>` and returns its result as an optional value.

Within `<block>` the null break expression `<exp1> !` exits the nearest enclosing `do ?` block with value `null` whenever `<exp1>` has value `null`, or continues evaluation with the contents of `<exp1>`'s option value. (See [Null break](#null-break).)

Option blocks nest with the target of a null break determined by the nearest enclosing option block.

### Null break

The null break expression `<exp> !` invokes scoped handling of null values and returns the contents of an option value or changes control-flow when the value is `null`.

It has type `T` provided:

-   the expression appears in the body, `<block>`, of an enclosing option block of the form `do ? <block>` (see [Option block](#do-opt)).

-   `<exp>` has option type `? T`.

The expression `<exp> !` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`; if `r` is `null`, execution breaks with value `null` from the nearest enclosing option block of form `do ? <block>`; otherwise, `r` is `? v` and execution continues with value `v`.

### Not

The not expression `not <exp>` has type `Bool` provided `<exp>` has type `Bool`.

If `<exp>` evaluates to `trap`, the expression returns `trap`. Otherwise, `<exp>` evaluates to a Boolean value `v` and the expression returns `not v`, (the Boolean negation of `v`).

### And

The and expression `<exp1> and <exp2>` has type `Bool` provided `<exp1>` and `<exp2>` have type `Bool`.

The expression `<exp1> and <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise `r1` is a Boolean value `v`. If `v == false` the expression returns the value `false` (without evaluating `<exp2>`). Otherwise, the expression returns the result of evaluating `<exp2>`.

### Or

The or expression `<exp1> or <exp2>` has type `Bool` provided `<exp1>` and `<exp2>` have type `Bool`.

The expression `<exp1> and <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise `r1` is a Boolean value `v`. If `v == true` the expression returns the value `true` (without evaluating `<exp2>`). Otherwise, the expression returns the result of evaluating `<exp2>`.

### If

The expression `if <exp1> <exp2> (else <exp3>)?` has type `T` provided:

-   `<exp1>` has type `Bool`

-   `<exp2>` has type `T`

-   `<exp3>` is absent and `() <: T`, or

-   `<exp3>` is present and has type `T`.

The expression evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, `r1` is the value `true` or `false`. If `r1` is `true`, the result is the result of evaluating `<exp2>`. Otherwise, `r1` is `false` and the result is `()` (if `<exp3>` is absent) or the result of `<exp3>` (if `<exp3>` is present).

### Switch

The switch expression `switch <exp> { (case <pat> <block-or-exp>;)+ }` has type `T` provided:

-   `exp` has type `U`; and

-   for each case `case <pat> <block-or-exp>` in the sequence `(case <pat> <block-or-exp>;)+` :

-   pattern `<pat>` has type `U`; and,

-   expression `<block-or-exp>` has type `T`

The expression evaluates `<exp>` to a result `r`. If `r` is `trap`, the result is `trap`. Otherwise, `r` is some value `v`. Let `case <pat> <block-or-exp>;` be the first case in `(case <pat> <block-or-exp>;)+` such that `<pat>` matches `v` for some binding of identifiers to values. Then result of the `switch` is the result of evaluating `<block-or-exp>` under that binding. If no case has a pattern that matches `v`, the result of the switch is `trap`.

### While

The expression `while <exp1> <exp2>` has type `()` provided:

-   `<exp1>` has type `Bool`, and

-   `<exp2>` has type `()`.

The expression evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, `r1` is the value `true` or `false`. If `r1` is `true`, the result is the result of re-evaluating `while <exp1> <exp2>`. Otherwise, the result is `()`.

### Loop

The expression `loop <block-or-exp>` has type `None` provided `<block-or-exp>` has type `()`.

The expression evaluates `<block-or-exp>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, the result is the result of (re-)evaluating `loop <block-or-exp>`.

### Loop-while

The expression `loop <block-or-exp1> while <exp2>` has type `()` provided:

-   `<block-or-exp1>` has type `()`, and

-   `<exp2>` has type `Bool`.

The expression evaluates `<block-or-exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, evaluation continues with `<exp2>`, producing result `r2`. If `r2` is `trap` the result is `trap`. Otherwise, if `r2` is `true`, the result is the result of re-evaluating `loop <block-or-exp1> while <exp2>`. Otherwise, `r2` is false and the result is `()`.

### For

The iterator expression `for ( <pat> in <exp1> ) <block-or-exp2>` has type `()` provided:

-   `<exp1>` has type `{ next : () → ?T }`,

-   pattern `<pat>` has type `T`, and

-   expression `<block-or-exp2>` has type `()` (in the environment extended with the bindings of `<pat>`).

The `for`-expression is syntactic sugar for

``` bnf
for ( <pat> in <exp1> ) <block-or-exp2> :=
  {
    let x = <exp1>;
    label l loop {
      switch (x.next()) {
        case (? <pat>) <block-or-exp2>;
        case (null) break l;
      }
    }
  }
```

where `x` and `l` are fresh identifiers.

In particular, the `for` loop will trap if evaluation of `<exp1>` traps; as soon as `x.next()` traps, or the value of `x.next()` does not match pattern `<pat>`, or when `<block-or-exp2>` traps.

:::note

Although general purpose, `for` loops are commonly used to consume iterators produced by [Special member access](#special-member-access) to, for example, loop over the indices (`a.keys()`) or values (`a.vals()`) of some array (here `a`).

:::

### Label

The label-expression `label <id> (: <typ>)? <block-or-exp>` has type `T` provided:

-   `(: <typ>)?` is absent and `T` is unit; or `(: <typ>)?` is present and `T == <typ>`;

-   `<block-or-exp>` has type `T` in the static environment extended with `label l : T`.

The result of evaluating `label <id> (: <typ>)? <block-or-exp>` is the result of evaluating `<block-or-exp>`.

### Labeled loops

If `<exp>` in `label <id> (: <typ>)? <exp>` is a looping construct:

-   `while (exp2) <block-or-exp1>`,

-   `loop <block-or-exp1> (while (<exp2>))?`, or

-   `for (<pat> in <exp2>) <block-or-exp1>`

the body, `<exp1>`, of the loop is implicitly enclosed in `label <id_continue> (…​)` allowing early continuation of the loop by the evaluation of expression `continue <id>`.

`<id_continue>` is fresh identifier that can only be referenced by `continue <id>` (through its implicit expansion to `break <id_continue>`).

### Break

The expression `break <id>` is equivalent to `break <id> ()`.

The expression `break <id> <exp>` has type `None` provided:

-   The label `<id>` is declared with type `label <id> : T`.

-   `<exp>` has type `T`.

The evaluation of `break <id> <exp>` evaluates exp to some result `r`. If `r` is `trap`, the result is `trap`. If `r` is a value `v`, the evaluation abandons the current computation up to dynamically enclosing declaration `label <id> …​` using the value `v` as the result of that labelled expression.

### Continue

The expression `continue <id>` is equivalent to `break <id_continue>`, where `<id_continue>` is implicitly declared around the bodies of `<id>`-labelled looping constructs (see [Labeled loops](#labeled-loops)).

### Return

The expression `return` is equivalent to `return ()`.

The expression `return <exp>` has type `None` provided:

-   `<exp>` has type `T` and

-   `T` is the return type of the nearest enclosing function (with no intervening `async` expression), or

-   `async T` is the type of the nearest enclosing (perhaps implicit) `async` expression (with no intervening function declaration)

The `return` expression exits the corresponding dynamic function invocation or completes the corresponding dynamic async expression with the result of `<exp>`.

### Async

The async expression `async <block-or-exp>` has type `async T` provided:

-   `<block-or-exp>` has type `T`;

-   `T` is shared.

Any control-flow label in scope for `async <block-or-exp>` is not in scope for `<block-or-exp>`. However, `<block-or-exp>` may declare and use its own, local, labels.

The implicit return type in `<block-or-exp>` is `T`. That is, the return expression, `<exp0>`, (implicit or explicit) to any enclosed `return <exp0>?` expression, must have type `T`.

Evaluation of `async <block-or-exp>` queues a message to evaluate `<block-or-exp>` in the nearest enclosing or top-level actor. It immediately returns a future of type `async T` that can be used to `await` the result of the pending evaluation of `<exp>`.

### Await

The `await` expression `await <exp>` has type `T` provided:

-   `<exp>` has type `async T`,

-   `T` is shared,

-   the `await` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `await <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is a future. If the `future` is incomplete, that is, its evaluation is still pending, `await <exp>` suspends evaluation of the neared enclosing `async` or `shared`-function, adding the suspension to the wait-queue of the `future`. Execution of the suspension is resumed once the future is completed (if ever). If the future is complete with value `v`, then `await <exp>` suspends evaluation and schedules resumption of execution with value `v`. If the future is complete with (thrown) error value `e`, then `await <exp>` suspends evaluation and schedules resumption of execution by re-throwing the error `e`.

Note: suspending computation on `await`, regardless of the dynamic status of the future, ensures that all tentative state changes and message sends prior to the `await` are committed and irrevocable.

:::danger

Between suspension and resumption of a computation, the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes.

:::

### Throw

The `throw` expression `throw <exp>` has type `None` provided:

-   `<exp>` has type `Error`,

-   the `throw` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `throw <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is an error value `e`. Execution proceeds from the `catch` clause of the nearest enclosing `try <block-or-exp1> catch <pat> <block-or-exp2>` whose pattern `<pat>` matches value `e`. If there is no such `try` expression, `e` is stored as the erroneous result of the `async` value of the nearest enclosing `async` expression or `shared` function invocation.

### Try

The `try` expression `try <block-or-exp1> catch <pat> <block-or-exp2>` has type `T` provided:

-   `<block-or-exp1>` has type `T`,

-   `<pat>` has type `Error` and `<block-or-exp2>` has type `T` in the context extended with `<pat>`, and

-   the `try` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `try <block-or-exp1> catch <pat> <block-or-exp2>` evaluates `<block-or-exp1>` to a result `r`. If evaluation of `<block-or-exp1>` throws an uncaught error value `e`, the result of the `try` is the result of evaluating `<block-or-exp2>` under the bindings determined by the match of `e` against `pat`.

:::note

Because the `Error` type is opaque, the pattern match cannot fail (typing ensures that `<pat>` is an irrefutable wildcard or identifier pattern).

:::

See [Error type](#error-type).

### Assert

The assert expression `assert <exp>` has type `()` provided `<exp>` has type `Bool`.

Expression `assert <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap` evaluation returns `trap`. Otherwise `r` is a Boolean value `v`. The result of `assert <exp>` is:

-   the value `()`, when `v` is `true`; or

-   `trap`, when `v` is `false`.

### Type annotation

The type annotation expression `<exp> : <typ>` has type `T` provided:

-   `<typ>` is `T`, and

-   `<exp>` has type `U` where `U <: T`.

Type annotation may be used to aid the type-checker when it cannot otherwise determine the type of `<exp>` or when one wants to constrain the inferred type, `U` of `<exp>` to a less-informative super-type `T` provided `U <: T`.

The result of evaluating `<exp> : <typ>` is the result of evaluating `<exp>`.

:::note

Type annotations have no-runtime cost and cannot be used to perform the (checked or unchecked) `down-casts` available in other object-oriented languages.

:::

### Candid Serialization

The *Candid serialization* expression `to_candid ( <exp>,*)` has type `Blob` provided:

-   `(<exp>,*)` has type `(T1,…​,Tn)`, and each `Ti` is *shared*.

Expression `to_candid ( <exp>,* )` evaluates the expression sequence `( <exp>,* )` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise, `r` is a sequence of Motoko values `vs`. The result of evaluating `to_candid ( <exp>,* )` is some Candid blob `b = encode((T1,...,Tn))(vs)`, encoding `vs`.

The Candid *deserialization* expression `from_candid <exp>` has type `?(T1,…​,Tn)` provided:

-   `?(T1,…​,Tn)` is the expected type from the context;

-   `<exp>` has type `Blob`; and

-   `?(T1,…​,Tn)` is *shared*.

Expression `from_candid <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is a binary blob `b`. If `b` Candid-decodes to Candid value sequence `Vs` of type `ea((T1,...,Tn))` then the result of `from_candid` is `?v` where `v = decode((T1,...,Tn))(Vs)`. If `b` Candid-decodes to a Candid value sequence `Vs` that is not of Candid type `ea((T1,...,Tn))` (but well-formed at some other type) then the result is `null`. If `b` is not the encoding of any well-typed Candid value, but some arbitrary binary blob, then the result of `from_candid` is a trap.

(Informally, here `ea(_)` is the Motoko-to-Candid type sequence translation and `encode/decode((T1,...,Tn))(_)` are type-directed Motoko-Candid value translations.)

<!--
ea(_) is defined in design doc motoko/design/IDL-Motoko.md, but `encode` and `decode` are not explicitly defined anywhere except in the implementation.
-->

:::note

Operation `from_candid` returns `null` when the argument is a valid Candid encoding of the wrong type. It traps if the blob is not a valid Candid encoding at all.

:::

:::note

Operations `to_candid` and `from_candid` are syntactic operators, not first-class functions, and must be fully applied in the syntax.

:::

:::danger

The Candid encoding of a value as a blob is not unique and the same value may have many different Candid representations as a blob. For this reason, blobs should never be used to, for instance, compute hashes of values or determine equality, whether across compiler versions or even just different programs.

:::

### Declaration

The declaration expression `<dec>` has type `T` provided the declaration `<dec>` has type `T`.

Evaluating the expression `<dec>` proceeds by evaluating `<dec>`, returning the result of `<dec>` but discarding the bindings introduced by `<dec>` (if any).

(The expression `<dec>` is actually shorthand for the block expression `{ <dec> }`.)

### Ignore

The expression `ignore <exp>` has type `()` provided the expression `<exp>` has type `Any` .

The expression `ignore <exp>` evaluates `<exp>` (typically for some side-effect) but discards its value.

`Ignore` is useful for evaluating an expression within a sequence of declarations when that expression has non-`unit` type (and the simpler `<exp>` declaration would be ill-typed). Then the semantics is equivalent to `let _ = <exp> : Any`.

### Debug

The debug expression `debug <block-or-exp>` has type `()` provided the expression `<block-or-exp>` has type `()`.

When the program is compiled or interpreted with (default) flag `--debug`, evaluating the expression `debug <exp>` proceeds by evaluating `<block-or-exp>`, returning the result of `<block-or-exp>`.

When the program is compiled or interpreted with flag `--release`, evaluating the expression `debug <exp>` immediately returns the unit value `()`. The code for `<block-or-exp>` is never executed, nor is its code included in the compiled binary.

### Actor references

The actor reference `actor <exp>` has expected type `T` provided:

-   the expression is used in a context expecting an expression of type `T` (typically as the subject of a type annotation, typed declaration or function argument); and

-   `T` is an some actor type `actor { …​ }`; and

-   `<exp>` has type `Text`.

The argument `<exp>` must be, or evaluate to, the textual format of an IC canister identifier (specified elsewhere), otherwise the expression traps. The result of the expression is an actor value representing that canister.

The validity of the canister identifier and its asserted type `T` are promises and taken on trust.

An invalid canister identifier or type may manifest itself, if at all, as a later dynamic failure when calling a function on the actor’s proclaimed interface, which will either fail or be rejected.

:::note

The argument to `actor` should *not* include the `ic:` resource locator used to specify an `import`. For example, use `actor "lg264-qjkae"`, not `actor "ic:lg264-qjkae"`.

:::

:::danger

Although they do not compromise type safety, actor references can easily introduce latent, dynamic errors. Accordingly, actor references should be used sparingly and only when needed.

:::

### Parentheses

The parenthesized expression `( <exp> )` has type `T` provided `<exp>` has type `T`.

The result of evaluating `( <exp> )` is the result of evaluating `<exp>`.

### Subsumption

Whenever `<exp>` has type `T` and `T <: U` (`T` subtypes `U`) then by virtue of *implicit subsumption*, `<exp>` also has type `U` (without extra syntax).

In general, this means that an expression of a more specific type may appear wherever an expression of a more general type is expected, provided the specific and general types are related by subtyping. This static change of type has no runtime cost.

## References

-   *IEEE Standard for Floating-Point Arithmetic*, in IEEE Std 754-2019 (Revision of IEEE 754-2008), vol., no., pp.1-84, 22 July 2019, doi: 10.1109/IEEESTD.2019.8766229.
