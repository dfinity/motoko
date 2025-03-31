# Language reference
<!--TODO Link to new base library-->
:::info Intended Audience

This reference page provides technical details of interest to the following audiences:

- Authors providing the higher-level documentation about the Motoko programming language.
- Compiler experts interested in the details of Motoko and its compiler.
- Advanced programmers who want to learn more about the lower-level details of Motoko.
:::

:::warning Not for Beginners

This page is intended to provide complete reference information about Motoko, but this section does not provide explanatory text or usage information.

Therefore, this section is typically not suitable for readers who are new to programming languages or who are looking for a general introduction to using Motoko.
:::

:::info Terminology

In this documentation, the term canister is used to refer to an Internet Computer smart contract.
:::

### Primitive types

Motoko provides the following primitive type identifiers, including support for Booleans, signed and unsigned integers and machine words of various sizes, characters and text.

The category of a type determines the operators (unary, binary, relational and in-place update via assignment) applicable to values of that type.

| Identifier                         | Category | Description                                                            |
| ---------------------------------- | -------- | ---------------------------------------------------------------------- |
| [`Bool`](../base/Bool.md)           | L        | Boolean values `true` and `false` and logical operators                |
| [`Char`](../base/Char.md)           | O        | Unicode characters                                                     |
| [`Text`](../base/Text.md)           | T, O     | Unicode strings of characters with concatenation `_ # _` and iteration |
| [`Float`](../base/Float.md)         | A, O     | 64-bit floating point values                                           |
| [`Int`](../base/Int.md)             | A, O     | Signed integer values with arithmetic (unbounded)                      |
| [`Int8`](../base/Int8.md)          | A, O     | Signed 8-bit integer values with checked arithmetic                    |
| [`Int16`](../base/Int16.md)       | A, O     | Signed 16-bit integer values with checked arithmetic                   |
| [`Int32`](../base/Int32.md)]       | A, O     | Signed 32-bit integer values with checked arithmetic                   |
| [`Int64`](../base/Int64.md)         | A, O     | Signed 64-bit integer values with checked arithmetic                   |
| [`Nat`](../base/Nat.md)             | A, O     | Non-negative integer values with arithmetic (unbounded)                |
| [`Nat8`](../base/Nat8.md)          | A, O     | Non-negative 8-bit integer values with checked arithmetic              |
| [`Nat16`](../base/Nat16.md)      | A, O     | Non-negative 16-bit integer values with checked arithmetic             |
| [`Nat32`](../base/Nat32.md)      | A, O     | Non-negative 32-bit integer values with checked arithmetic             |
| [`Nat64`](../base/Nat64.md)        | A, O     | Non-negative 64-bit integer values with checked arithmetic             |
| [`Blob`](../base/Blob.md)          | O        | Binary blobs with iterators                                            |
| [`Principal`](../base/Principal.md) | O        | Principals                                                             |
| [`Error`](../base/Error.md)         |          | (Opaque) error values                                                  |
| [`Region`](../base/Region.md)       |          | (Opaque) stable memory region objects                                  |

Although many of these types have linguistic support for literals and operators, each primitive type also has an eponymous base library providing related [functions and values](../base/index.md). For example, the [`Text`](../base/Text.md) library provides common functions on `Text` values.

### Type [`Bool`](../base/Bool.md)

The type [`Bool`](../base/Bool.md) of category L (Logical) has values `true` and `false` and is supported by one and two branch `if _ <exp> (else <exp>)?`, `not <exp>`, `_ and _` and `_ or _` expressions. Expressions `if`, `and` and `or` are short-circuiting.

### Type `Char`

A `Char` of category O (Ordered) represents a character as a code point in the unicode character set.

Base library function `Char.toNat32(c)` converts a `Char` value, `c` to its [`Nat32`](../base/Nat32.md) code point. Function `Char.fromNat32(n)` converts a [`Nat32`](../base/Nat32.md) value, `n`, in the range *0x0..xD7FF* or *0xE000..0x10FFFF* of valid code points to its `Char` value; this conversion traps on invalid arguments. Function `Char.toText(c)` converts the `Char` `c` into the corresponding, single character [`Text`](../base/Text.md) value.

### Type [`Text`](../base/Text.md)

The type [`Text`](../base/Text.md) of categories T and O (Text, Ordered) represents sequences of unicode characters i.e. strings. Function `t.size` returns the number of characters in [`Text`](../base/Text.md) value `t`. Operations on text values include concatenation (`_ # _`) and sequential iteration over characters via `t.chars` as in `for (c : Char in t.chars()) { …​ c …​ }`.

Both [`Int`](../base/Int.md) and [`Nat`](../base/Nat.md) are arbitrary precision, with only subtraction `-` on [`Nat`](../base/Nat.md) trapping on underflow.

The subtype relation `Nat <: Int` holds, so every expression of type [`Nat`](../base/Nat.md) is also an expression of type [`Int`](../base/Int.md) but not vice versa. In particular, every value of type [`Nat`](../base/Nat.md) is also a value of type [`Int`](../base/Int.md), without change of representation.

### Bounded integers [`Int8`](../base/Int8.md), [`Int16`](../base/Int16.md), [`Int32`](../base/Int32.md) and [`Int64`](../base/Int64.md)

The types [`Int8`](../base/Int8.md), [`Int16`](../base/Int16.md), [`Int32`](../base/Int32.md) and [`Int64`](../base/Int64.md) represent signed integers with respectively 8, 16, 32 and 64 bit precision. All have categories A (Arithmetic), B (Bitwise) and O (Ordered).

Operations that may under- or overflow the representation are checked and trap on error.

The operations `+%`, `-%`, `*%` and `**%` provide access to wrap-around, modular arithmetic.

As bitwise types, these types support bitwise operations and (`&`), or (`|`) and exclusive-or (`^`). Further, they can be rotated left (`<<>`), right (`<>>`), and shifted left (`<<`), right (`>>`). The right-shift preserves the two’s-complement sign. All shift and rotate amounts are considered modulo the numbers’s bit width `n`.

Bounded integer types are not in subtype relationship with each other or with other arithmetic types, and their literals need type annotation if the type cannot be inferred from context, e.g. `(-42 : Int16)`.

The corresponding module in the base library provides conversion functions:

- Conversion to [`Int`](../base/Int.md).

- Checked and wrapping conversions from [`Int`](../base/Int.md).

- Wrapping conversion to the bounded natural type of the same size.

### Bounded naturals [`Nat8`](../base/Nat8.md), [`Nat16`](../base/Nat16.md), [`Nat32`](../base/Nat32.md) and [`Nat64`](../base/Nat64.md)

The types [`Nat8`](../base/Nat8.md), [`Nat16`](../base/Nat16.md), [`Nat32`](../base/Nat32.md) and [`Nat64`](../base/Nat64.md) represent unsigned integers with respectively 8, 16, 32 and 64 bit precision. All have categories A (Arithmetic), B (Bitwise) and O (Ordered).

Operations that may under- or overflow the representation are checked and trap on error.

The operations `+%`, `-%`, `*%` and `**%` provide access to the modular, wrap-on-overflow operations.

As bitwise types, these types support bitwise operations and (`&`), or (`|`) and exclusive-or (`^`). Further, they can be rotated left (`<<>`), right (`<>>`), and shifted left (`<<`), right (`>>`). The right-shift is logical. All shift and rotate amounts are considered modulo the number’s bit width *n*.

The corresponding module in the base library provides conversion functions:

- Conversion to [`Nat`](../base/Nat.md).

- Checked and wrapping conversions from [`Nat`](../base/Nat.md).

- Wrapping conversion to the bounded, signed integer type of the same size.

### Type [`Blob`](../base/Blob.md)

The type [`Blob`](../base/Blob.md) of category O (Ordered) represents binary blobs or sequences of bytes. Function `b.size()` returns the number of characters in [`Blob`](../base/Blob.md) value `b`. Operations on blob values include sequential iteration over bytes via function `b.values()` as in `for (v : Nat8 in b.values()) { …​ v …​ }`.

### Type [`Principal`](../base/Principal.md)

The type [`Principal`](../base/Principal.md) of category O (Ordered) represents opaque principals such as canisters and users that can be used to identify callers of shared functions and used for simple authentication. Although opaque, principals may be converted to binary [`Blob`](../base/Blob.md) values for more efficient hashing and other applications.

### Error type

Assuming base library import:

``` motoko no-repl
import E "mo:base/Error";
```

Errors are opaque values constructed and examined with operations:

- `E.reject : Text -> Error`

- `E.code : Error -> E.ErrorCode`

- `E.message : Error -> Text`

Type `E.ErrorCode` is equivalent to variant type:

``` motoko no-repl
type ErrorCode = {
  // Fatal error.
  #system_fatal;
  // Transient error.
  #system_transient;
  // Response unknown due to missed deadline.
  #system_unknown;
  // Destination invalid.
  #destination_invalid;
  // Explicit reject by canister code.
  #canister_reject;
  // Canister trapped.
  #canister_error;
  // Future error code (with unrecognized numeric code).
  #future : Nat32;
  // Error issuing inter-canister call
  // (indicating destination queue full or freezing threshold crossed).
  #call_error : { err_code : Nat32 }
};
```

A constructed error `e = E.reject(t)` has `E.code(e) = #canister_reject` and `E.message(e) = t`.

[`Error`](../base/Error.md) values can be thrown and caught within an `async` expression or `shared` function only.

Errors with codes other than `#canister_reject`, i.e. system errors, may be caught and thrown but not user-constructed.

:::note

Exiting an async block or shared function with a non-`#canister-reject` system error exits with a copy of the error with revised code `#canister_reject` and the original [`Text`](../base/Text.md) message. This prevents programmatic forgery of system errors.

:::

:::note

On ICP, the act of issuing a call to a canister function can fail, so that the call cannot (and will not be) performed.
This can happen due to a lack of canister resources, typically because the local message queue for the destination canister is full,
or because performing the call would reduce the current cycle balance of the calling canister to a level below its freezing threshold.
Such call failures are reported by throwing an [`Error`](../base/Error.md) with code `#call_error { err_code = n }`, where `n` is the non-zero `err_code` value returned by ICP.
Like other errors, call errors can be caught and handled using `try ... catch ...` expressions, if desired.

:::

### Type `Region`

The type `Region` represents opaque stable memory regions. Region objects are dynamically allocated and independently growable. They represent isolated partitions of IC stable memory.

The region type is stable but not shared and its objects, which are stateful, may be stored in stable variables and data structures.

Objects of type `Region` are created and updated using the functions provided by base library `Region`. See [stable regions](../stable-memory/stable-regions.md) and library [Region](../base/Region.md) for more information.

### Constructed types

`<path> <type-typ-args>?` is the application of a type identifier or path, either built-in (i.e. [`Int`](../base/Int.md)) or user defined, to zero or more type arguments. The type arguments must satisfy the bounds, if any, expected by the type constructor’s type parameters (see [Well-formed types](#well-formed-types)).

Though typically a type identifier, more generally, `<path>` may be a `.`-separated sequence of actor, object or module identifiers ending in an identifier accessing a type component of a value (for example, `Acme.Collections.List`).

### Object types

`<typ-sort>? { <typ-field>;* }` specifies an object type by listing its zero or more named **type fields**.

Within an object type, the names of fields must be distinct both by name and hash value.

Object types that differ only in the ordering of the fields are equivalent.

When `<typ-sort>?` is `actor`, all fields have `shared` function type for specifying messages.

### Variant types

`{ <typ-tag>;* }` specifies a variant type by listing its variant type fields as a sequence of `<typ-tag>`s.

Within a variant type, the tags of its variants must be distinct both by name and hash value.

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

Type `<shared>? <typ-params>? <typ1> -> <typ2>` specifies the type of functions that consume optional type parameters `<typ-params>`, consume a value parameter of type `<typ1>` and produce a result of type `<typ2>`.

Both `<typ1>` and `<typ2>` may reference type parameters declared in `<typ-params>`.

If `<typ1>` or `<typ2>` or both is a tuple type, then the length of that tuple type determines the argument or result *arity* of the function type.
The arity is the number of arguments or results a function returns.

The optional `<shared>` qualifier specifies whether the function value is shared, which further constrains the form of `<typ-params>`, `<typ1>` and `<typ2>` (see [sharability](#shareability) below).

Note that a `<shared>` function may itself be `shared` or `shared query` or `shared composite query`, determining the persistence of its state changes.

### Async types

`async <typ>` specifies a future producing a value of type `<typ>`.

Future types typically appear as the result type of a `shared` function that produces an `await`-able value.

### Async* types

`async* <typ>` specifies a delayed, asynchronous computation producing a value of type `<typ>`.

Computation types typically appear as the result type of a `local` function that produces an `await*`-able value.

They cannot be used as the return types of `shared` functions.

### Tuple types

`( ((<id> :)? <typ>),* )` specifies the type of a tuple with zero or more ordered components.

The optional identifier `<id>`, naming its components, is for documentation purposes only and cannot be used for component access. In particular, tuple types that differ only in the names of components are equivalent.

The empty tuple type `()` is called the *unit type*.

### Any type

Type `Any` is the top type, i.e. the supertype of all types. All values have type `Any`.

### None type

Type `None` is the bottom type, the subtype of all other types. No value has type `None`.

As an empty type, `None` can be used to specify the impossible return value of an infinite loop or unconditional trap.

### Intersection type

The type expression `<typ1> and <typ2>` denotes the syntactic **intersection** between its two type operands, that is, the greatest type that is a subtype of both. If both types are incompatible, the intersection is `None`.

The intersection is syntactic, in that it does not consider possible instantiations of type variables. The intersection of two type variables is `None`, unless they are equal, or one is declared to be a (direct or indirect) subtype of the other.

### Union type

The type expression `<typ1> or <typ2>` denotes the syntactic **union** between its two type operands, that is, the smallest type that is a supertype of both. If both types are incompatible, the union is `Any`.

The union is syntactic, in that it does not consider possible instantiations of type variables. The union of two type variables is the union of their bounds, unless the variables are equal, or one is declared to be a direct or indirect subtype of the other.

### Parenthesized type

A function that takes an immediate, syntactic tuple of length `n \>= 0` as its domain or range is a function that takes and respectively returns `n` values.

When enclosing the argument or result type of a function, which is itself a tuple type, `( <tuple-typ> )` declares that the function takes or returns a single boxed value of type `<tuple-type>`.

In all other positions, `( <typ> )` has the same meaning as `<typ>`.

### Type fields

``` bnf
<typ-field> ::=                               Object type fields
  <id> : <typ>                                  Immutable value
  var <id> : <typ>                              Mutable value
  <id> <typ-params>? <typ1> : <typ2>            Function value (short-hand)
  type <id> <type-typ-params>? = <typ>          Type component
```

A type field specifies the name and type of a value field of an object, or the name and definition of a type component of an object. The value field names within a single object type must be distinct and have non-colliding hashes. The type component names within a single object type must also be distinct and have non-colliding hashes. Value fields and type components reside in separate name spaces and thus may have names in common.

`<id> : <typ>` : Specifies an **immutable** field, named `<id>` of type `<typ>`.

`var <id> : <typ>` : Specifies a **mutable** field, named `<id>` of type `<typ>`.

`type <id> <type-typ-params>? = <typ>` : Specifies a **type component**, with field name `<id>`, abbreviating parameterized type `<typ>`.

Unlike type declarations, a type component is not, in itself, recursive though it may abbreviate an existing recursive type.
In particular, the name `<id>` is not bound in `<typ>` nor in any other fields of the enclosing object type. The name `<id>` only determines the label to use when accessing the definition through a record of this type using the dot notation.

### Variant type fields

``` bnf
<typ-tag> ::=                                 Variant type fields
  # <id> : <typ>                                Tag
  # <id>                                        Unit tag (short-hand)
```

A variant type field specifies the tag and type of a single variant of an enclosing variant type. The tags within a single variant type must be distinct and have non-colliding hashes.

`# <id> : <typ>` specifies an immutable field, named `<id>` of type `<typ>`. `# <id>` is sugar for an immutable field, named `<id>` of type `()`.

### Sugar

When enclosed by an `actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field named `<id>` of `shared` function type `shared <typ-params>? <typ1> → <typ2>`.

When enclosed by a non-`actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field named `<id>` of ordinary function type `<typ-params>? <typ1> → <typ2>`.

### Type parameters

``` bnf
<typ-params> ::=                              Type parameters
  < typ-param,* >
<typ-param>
  <id> <: <typ>                               Constrained type parameter
  <id>                                        Unconstrained type parameter
```

``` bnf
<type-typ-params> ::=                         Type parameters to type constructors
  < typ-param,* >

<typ-params> ::=                              Function type parameters
  < typ-param,* >                             Type parameters
  < system (, <typ-param>*)) >                System capability prefixed type parameters

<typ-param>
  <id> <: <typ>                               Constrained type parameter
  <id>                                        Unconstrained type parameter

```

A type constructor may be parameterized by a vector of comma-separated, optionally constrained, type parameters.

A function, class constructor or function type may be parameterized by a vector of comma-separated, optionally constrained, type parameters.
The first of these may be the special, pseudo type parameter `system`.

`<id> <: <typ>` declares a type parameter with constraint `<typ>`. Any instantiation of `<id>` must subtype `<typ>` at that same instantiation.

Syntactic sugar `<id>` declares a type parameter with implicit, trivial constraint `Any`.

The names of type parameters in a vector must be distinct.

All type parameters declared in a vector are in scope within its bounds.

The `system` pseudo-type parameter on function types indicates that a value of that type requires `system` capability in order to be called and may itself call functions requiring `system` capability during its execution.

### Type arguments

``` bnf
<type-typ-args> ::=                           Type arguments to type constructors
  < <typ>,* >


<typ-args> ::=                                Type arguments to functions
  < <typ>,* >                                   Plain type arguments
  < system (, <typ>*) >                         System capability prefixed type arguments

```

Type constructors and functions may take type arguments.

The number of type arguments must agree with the number of declared type parameters of the type constructor.

For a function, the number of type arguments, when provided, must agree with the number of declared type parameters of the function’s type. Note that type arguments in function applications can typically be omitted and inferred by the compiler.

Given a vector of type arguments instantiating a vector of type parameters, each type argument must satisfy the instantiated bounds of the corresponding type parameter.

In function calls, supplying the `system` pseudo type argument grants system capability to the function that requires it.

System capability is available only in the following syntactic contexts:

- In the body of an actor expression or actor class.
- In the body of a (non-`query`) `shared` function, asynchronous function, `async` expression or `async*` expression.
- In the body of a function or class that is declared with `system` pseudo type parameter.
- In system functions `preupgrade` and `postupgrade`.

No other context provides `system` capabilities, including `query` and `composite query` methods.

The `<system>` type parameters of shared and asynchronous functions need not be declared.

### Well-formed types

A type `T` is well-formed only if recursively its constituent types are well-formed, and:

- If `T` is `async U` or `async* U` then `U` is shared, and

- If `T` is `shared <query>? U -> V`:
  - `U` is shared and,
  - `V == ()` and `<query>?` is absent, or
  - `V == async W` with `W` shared, and

- If `T` is `C<T0, …​, Tn>` where:

  - A declaration `type C<X0 <: U0, Xn <: Un>  = …​` is in scope, and

  - `Ti <: Ui[ T0/X0, …​, Tn/Xn ]`, for each `0 <= i <= n`.

- If `T` is `actor { …​ }` then all fields in `…​` are immutable and have `shared` function type.

### Subtyping

Two types `T`, `U` are related by subtyping, written `T <: U`, whenever, one of the following conditions is true:

- `T` equals `U` (subtyping is *reflexive*).

- `U` equals `Any`.

- `T` equals `None`.

- `T` is a type parameter `X` declared with constraint `U`.

- `T` is [`Nat`](../base/Nat.md) and `U` is [`Int`](../base/Int.md).

- `T` is a tuple `(T0, …​, Tn)`, `U` is a tuple `(U0, …​, Un)`, and for each `0 <= i <= n`, `Ti <: Ui`.

- `T` is an immutable array type `[ V ]`, `U` is an immutable array type `[ W ]` and `V <: W`.

- `T` is a mutable array type `[ var V ]`, `U` is a mutable array type `[ var W ]` and `V == W`.

- `T` is `Null` and `U` is an option type `? W` for some `W`.

- `T` is `? V`, `U` is `? W` and `V <: W`.

- `T` is a future `async V`, `U` is a future `async W`, and `V <: W`.

- `T` is an object type `<typ-sort0> { fts0 }`, `U` is an object type `<typ-sort1> { fts1 }` and

  - `<typ-sort0>` == `<typ-sort1>`, and, for all fields,

  - If field `id : W` is in `fts1` then `id : V` is in `fts0` and `V <: W`, and

  - If mutable field `var id : W` is in `fts1` then `var id : V` is in `fts0` and `V == W`.

    That is, object type `T` is a subtype of object type `U` if they have the same sort, every mutable field in `U` super-types the same field in `T` and every mutable field in `U` is mutable in `T` with an equivalent type. In particular, `T` may specify more fields than `U`.
         Note that this clause defines subtyping for all sorts of object type, whether `module`, `object` or `actor`.

- `T` is a variant type `{ fts0 }`, `U` is a variant type `{ fts1 }` and

  - If field `# id : V` is in `fts0` then `# id : W` is in `fts1` and `V <: W`.

    That is, variant type `T` is a subtype of variant type `U` if every field of `T` subtypes the same field of `U`. In particular, `T` may specify fewer variants than `U`.

- `T` is a function type `<shared>? <X0 <: V0, ..., Xn <: Vn> T1 -> T2`, `U` is a function type `<shared>? <X0 <: W0, ..., Xn <: Wn> U1 -> U2` and

  - `T` and `U` are either both equivalently `<shared>?`, and

  - Assuming constraints `X0 <: W0, …​, Xn <: Wn` then

    - for all `i`, `Wi == Vi`, and

    - `U1 <: T1`, and

    - `T2 <: U2`.

        That is, function type `T` is a subtype of function type `U` if they have same `<shared>?` qualification, they have the same type parameters (modulo renaming) and assuming the bounds in `U`, every bound in `T` supertypes the corresponding parameter bound in `U` (contra-variance), the domain of `T` supertypes the domain of `U` (contra-variance) and the range of `T` subtypes the range of `U` (co-variance).

- `T` (respectively `U`) is a constructed type `C<V0, …​, Vn>` that is equal, by definition of type constructor `C`, to `W`, and `W <: U` (respectively `U <: W`).

- For some type `V`, `T <: V` and `V <: U` (*transitivity*).

### Shareability

A type `T` is **shared** if it is:

- `Any` or `None`, or

- A primitive type other than [`Error`](../base/Error.md), or

- An option type `? V` where `V` is shared, or

- A tuple type `(T0, …​, Tn)` where all `Ti` are shared, or

- An immutable array type `[V]` where `V` is shared, or

- An `object` type where all fields are immutable and have shared type, or

- A variant type where all tags have shared type, or

- A shared function type, or

- An `actor` type.

### Stability

Stability extends shareability to include mutable types. More precisely:

A type `T` is **stable** if it is:

- `Any` or `None`, or

- A primitive type other than [`Error`](../base/Error.md), or

- An option type `? V` where `V` is stable, or

- A tuple type `(T0, …​, Tn)` where all `Ti` are stable, or

- A (mutable or immutable) array type `[var? V]` where `V` is stable, or

- An `object` type where all fields have stable type, or

- A variant type where all tags have stable type, or

- A shared function type, or

- An `actor` type.

This definition implies that every shared type is a stable type. The converse does not hold: there are types that are stable but not share, notably types with mutable components.

The types of actor fields declared with the `stable` qualifier must have stable type.

The current value of such a field is preserved upon upgrade, whereas the values of other fields are reinitialized after an upgrade.

Note: the primitive `Region` type is stable.