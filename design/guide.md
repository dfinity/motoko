# A Guide to ActorScript

<!---
TODO
* use menhir --only-preprocess-uu parser.mly followed by sed to create concrete grammar
* perhaps use notions of left-evaluation and evaluation to talk about variable deref in just on place?
-->

## Introduction

ActorScript is a new, general purpose programming language for the
Dfinity platform.

### Why a new language?

Dfinity has chosen WebAssembly as its low-level virtual machine. 

The currently
available compilers targeting WebAssembly are for languages that are
either too unsafe (C, C++) or too complex (Rust) for mainstream
programmers. 

To promote correctness and reduce complexity, Dfinity is designing its own language, *ActorScript*, that is safe and expressive, yet simple and approachable to mainstream programmers.

#### Interoperability

ActorScript is just one of hopefully many languages able to run on the Dfinity platform. 

Since WebAssembly is language agnostic and, unlike other virtual machines, does not mandate a high-level type system for language interoperation, Dfinity will provide an *Interface Definition Language* to support typed, cross-language communication.

The ActorScript compiler will automate the production and consumption of IDL files, driven by type signatures ActorScript programs and the structure of imported IDL interfaces.

The IDL language is currently under design and outside the scope of this document.


### Design Goals

ActorScript provides:

* A high-level language for programming Dfinity applications

* A simple ("K.I.S.S.") design and familiar syntax for average programmers

* Good and convenient support for the actor model embodied in Dfinity conisters

* A good fit for underlying Wasm and Dfinity execution model

* A forward looking design that anticipates future extensions to WebAssembly

### Key Features

The key language features of ActorScript are:

* JavaScript/TypeScript-style syntax.

* Automatic memory management (by precise garbage collection).

* Strong, static typing with parametric polymorphism, subtype polymorphism and 
  structural typing.

* Unbounded and bounded numeric types with explicit conversions
  between them. Bounded numeric types are overflow-checked.

* Imperative programming features such a mutable variables and arrays
  and flexible, local control flow constructs (`return`, `break` and `continue`).

* Functions (and messages) are first-class values, argument evaluation
  is strict (call-by-value).

* Pattern matching on scalar and compound values.

* A simple, class-based object system without inheritance.

* The value of a reference can never implicitly be 'null',
  preventing a large class of 'null'-reference failures.
  Instead, an explicitly handled, possibly 'null', *option*`?<type>` type is provided.
   
* Classes can be actors (canisters).

* An Actor based concurrency model:

  * Actor state is isolated.

  * All communication with and between actors is by message passing (never through shared state).

  * An actor's messages are processed in sequence, so state modifications are
     always data-race free.

* Message passing is asynchronous (to hide network lacency).

* A familiar `async`/`await` constructs enables sequential programming with asynchronous messaging.

Like most programming lanuages, ActorScript borrows features from others and
draws inspirations from Java, C#, JavaScript, Swift, Pony, ML, Haskell.

## ActorScript Syntax (Sketch)

Productions marked * probably deferred to later versions.

## Lexical conventions

### Keywords
TBC

### Identifiers
TBC

### Literals
TBC

### Operators
TBC

## Types


Type expressions are used to specify the types of arguments, bound on type parameters,definitions of type constructors, and in type annotations.

```
<typ> ::=                                     type expressions
  <id> <typ-args>?                              constructor
  (shared|actor)? { <typ-field>;* }             object
  [ var? <typ> ]                                array
  Null                                          null type
  ? <typ>                                       option
  shared? <typ-params>? <typ> -> <typ>          function
  async <typ>                                   future
  ( ((<id> :)? <typ>),* )                       tuple
  Any                                           top
  None                                          bottom
  Shared                                        sharable types
  ( type )                                      parenthesized type
```
### Constructed types

 `<id> <typ-args>?` is the application of type a identifier, either built-in (i.e. Int) or user defined, to zero or more type *arguments*. 
 The type arguments must satisfy the bounds, if any, expected by the type constructor's type parameters (see below).

### Object types

`(shared|actor)? { <typ-field>;* }` specifies an object type by listing its zero or more named *type fields*. 

Within an object type, the names of fields must be distinct.

Object types that differ only in the ordering of the fields are equivalent.

The optional qualifier `shared` constrains the object's field to have *sharable* type. 

Messages (see below) always requires arguments of *sharable* (think *serializable*) type.

The optional qualifier `actor` constrains the object's fields to be *shared* functions (i.e. messages).


### Array types

`[ var? <typ> ]` specifies the type of arrays with elements of type '<typ>'.menhir --only-preprocess-uu parser.mly 

Arrays are immutable unless specified with qualifier `var`. 

### Null type

The `Null` type has a single value, the literal `null`. `Null` is a subtype of the option `? T`, for any type `T`. 

### Option types

`? <typ>` specifies the type of values that are either `null` or a proper value of the form `? <v>` where `<v>` has type `typ`.

### Function types

Type `shared? <typ-params>? <typ1> -> <typ2>` specifies the type of functions that consume (optional) type parameters `<typ-params>`, consume a value parameter of type `<typ1>` and produce a result of type `<typ2>`.

Both `<typ1>` and `<typ2>` may reference type parameters declared in `<typ-params>`.

If `<typ1>` or `<typ2>` (or both) is a tuple type, then the length of that tuple type determines the argument or result arity of the function type.

The optional `shared` qualifier specifies whether the function value is shared, which further constrains the form of `<typ-params>`, `<typ1>` and `<typ2>` (see *Sharability* below).

### Async types

`async <typ>` specifes a promise producing a value of a type `<typ>`. 

Promise types typically appear as the result type of a `shared` function that produces an `await`-able value.

### Tuple types

`( ((<id> :)? <typ>),* )` specifies the type of a tuple with zero or more ordered components.

The optional identifer `<id>`, naming its components, is for documentation purposes only and cannot be used for component access. In particular, tuple types that differ only in the names of fields are equivalent.

### Any type

Type `Any` is the *top* type, i.e. the super-type of all types, (think Object in Java or C#). All values have type any.

### None type

Type `None` is the *bottom* type, a subtype of all other types. 
No value has type `None`. 

As an empty type, `None` can be used to specify the (non-existant) return value of an infinite loop or trap.

### Shared type

Type `Shared`  is the super-type of all types that can be transmitted between actors (i.e. sent or received) as the arguments or return values of `shared` functions.


### Parenthesized type

A function that takes an immediate, syntactic tuple of length *n >= 0* as its domain or range is a function that takes (respectively returns) *n* values.

When enclosing the argument or result type of a function, which is itself a tuple type,  `( <tuple-typ> )` declares that the function takes or returns a single (boxed) value of type <tuple-type>.

In all other positions, `( <typ> )` has the same meaning as `<typ>`.

### Type fields

```
<typ-field> ::=                               object type fields
  <id> : <typ>                                  immutable
  var <id> : <typ>                              mutable
  <id> <typ-params>? <typ1> : <typ2>           function (short-hand)
```

A type field specifies the name and types of fields of an object.

`<id> : <typ>` specifies an *immutable* field, named `<id>` of type `<typ>`.

`var <id> : <typ>` specifies a *mutable* field, named `<id>` of type `<typ>`.

#### Sugar

When enclosed by an `actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field name `<id>` of `shared` function type 
`shared <typ-params>? <typ1> -> <typ2>`.

When enlosed by a non-`actor` object type, `<id> <typ-params>? <typ1> : <typ2>` is syntactic sugar for an immutable field name `<id>` of ordinary function type `<typ-params>? <typ1> -> <typ2>`.


### Type parameters

```
<typ-params> ::=                              type parameters
  < typ-param,* >                         
<typ-param>
  <id> <: <typ>                               constrained type parameter
  <id>                                        unconstrained type parameter
```

A type constructors, function value or function type may be parameterised by a vector of comma-separated, optionally constrained, type parameters. 

`<id> <: <typ>` declares a type parameter with constraint `<typ>`. 
Any instantiation of `<id>` must subtype `<typ>` (at that same instantiation).

Syntactic sugar `<id>` declares a type parameter with implicit, trivial constraint `Any`.

The names of type parameters in a vector must be distinct. 

All type parameters declared in a vector are in scope within its bounds.

### Type arguments

```
<typ-args> ::=                                type arguments
  < <typ>,* >
```

Type constructors and functions may take type arguments. 

The number of type arguments must agree with the number of declared type parameters of the function.

Given a vector of type arguments instantiating a vector of type parameters, 
each type argument must satisfy the instantiated bounds of the corresponding 
type parameter.


### Wellformed types

A type `T` is well-formed only if (recursively) its constituent types are well-formed,and: 
* if `T` is `async U` then `U <: Shared`, and
* if `T` is `shared < ... > V -> W` then `...` is empty, `U <: Shared` and
  `W == ()` or `W == async W`, and
* if `T` is `C<T0, ..., TN>` where `type C<X0 <: U0, Xn <: Un>  = ...` then
   `Ti <: Ui[T0/X0,...,Tn/Xn]`, for each `0 <= i <= n`. 
* if `T` is `actor { ... }` then all fields in `...` are immutable and have `shared` function type.
* if `T` is `shared { ... }` then all fields in `...` are immutable and have a type that subtypes `Shared`.

### Subtyping

Two types `T`, `U` are related by subtyping, written `T <: U`, whenever, one of the following conditions is true.

* `T` equals `U` (reflexivity).

* `U` equals `Any`. 

* `T` equals `None`.

*  `T` is a type parameter `X` declared with constraint 'U'.

*  `T` is a tuple `(T0, ..., Tn)`, `U` is a tuple `(U0, ..., Un)`, 
    and for each `0 <= i <= n`, `Ti <: Ui`.

*  `T` is an immutable array type `[ V ]`, `U` is an immutable array type  `[ W ]` 
    and `V <: W`.

*  `T` is a mutable array type `[ var T ]`, `V` is a mutable array type  `[ var W ]` 
    and `V == W`.

*  `T` is `Null` and `U` is the an option type `? W`.

*  `T` is `? V`, `U` is `? W` and `V <: U`.

*  `T` is a promise `async V`, `U` is a promise `async W`, 
    and `V <: W`.

*  `T` is an object type `sort0 { fts0 }`, 
   `U` is an object type `sort1 { fts1 }` and
   * `sort1` == `sort2`, and, for all fields,
   * if field `id : V` is in `fts0` then `id : W` is in `fts1` and `V <: W`, and 
   * if mutable field `var id : V` is in `fts0` then  `var id : W` is in `fts1` and `V == W`.

   (That is, object type `T` is a subtype of object type `U` if they have same sort, every mutable field in `U` super-types the same field in `T` and every mutable field in `U` is mutable in `T` with an equivalent type. In particular, `T` may specify more fields than `U`.)

*  `T` is a function type `shared? <X0 <: V0, ..., Xn <: Vn> T1 -> T2`, 
   `U` is a function type `shared? <X0 <: W0, ..., Xn <: Wn> U1 -> U2` and
   * `T` and `U` are either both `shared` or both non-`shared`,
   *  for all `i`, `Vi <: Wi`,
   * `U1 <: T1` and
   * `T2 <: U2`.
  
    (That is, function type `T` is a subtype of function type `U` if they have same sort, they have the same type parameters, every bound in `U` super-types the same parameter bound in `T`, the domain of `U` suptypes the domain of `T` (contra-variance) and the range of `T` subtypes the range of `U`).

* `T` (respectively `U`) is a constructed type `C<V0,...VN>` that is equal, by definition of type constructor `C`,  to `W`, and `W <: U` (respectively `U <: W`).

* For some type `V`, `T <: V` and `V <: U` (*transitivity*).

* `U` is type `Shared` and `T` is equivalent to:
  * a `shared` function type, or
  * a `shared` object type, or
  * a `actor` object type, or
  * a scalar primitive type, or
  * the type `Text`, or
  * an immutable array type `[V]` with `V <: Shared`, or
  * the type `Null`, or
  * an option type `? V` with `V <: Shared`, or
  * a tuple type `(T0, ..., Tn)` where, for each `0 <= i <= n`, `Ti <: Shared`. 

## Literals

```
<lit> ::=                                     literals
  <nat>                                         natural
  <float>                                       float
  <char>                                        character
  <text>                                        unicode text
```

## Expressions
```
<exp> ::=
  <id>                                           variable
  <lit>                                          literal
  <unop> <exp>                                   unary numeric operator
  <exp> <binop> <exp>                            binary numeric operator
  ( <exp>,* )                                    tuple
  <exp> . <nat>                                  tuple projection
  ? <exp>                                        option injection
  <exp> . <id>                                   object projection
  <exp> := <exp>                                 assignment
  <unop>= <exp>                                  unary update
  <exp> <binop>= <exp>                           binary update
  [ var? <exp>,* ]                               array
  <exp> [ <exp> ]                                array indexing
  <exp> <typ-args>? <exp>                        function call
  { <dec>;* }                                    block
  not <exp>                                      negation
  <exp> and <exp>                                conjunction
  <exp> or <exp>                                 disjunction
  if <exp> <exp> (else <exp>)?                   conditional
  switch <exp> { (case <pat> <exp>)+ }           switch
  while <exp> <exp>                              while loop
  loop <exp> (while <exp>)?                      loop
  for <id>? in <exp> <exp>                       iteration
  label <id> (: <typ>)? <exp>                    label
  break <id> <exp>?                              break
  continue <id>                                  continue
  return <exp>?                                  return                    function call

  async <exp>                                    async expression
  await <exp>                                    await future (only in async)
  assert <exp>                                   assertion
  <exp> : <typ>                                  type annotation
  <dec>                                          declaration (scopes to block)

<exp-field> ::=                                object expression fields
  private? dec                                   field
  private? <id> = <exp>                          short-hand
```

### Identifiers
                    function call

The expression `<id>` evaluates to the value bound to `<id>` in the current evaluation environment.

### Literals

The literal (or constant) expression `<lit>` evaluates to itself.

### Unary operators

The unary operator expressions `<unop> <exp>` evaluates `exp` to a result. If the result is a value `v` it returns the result of `<unop> v`.
If the result is a trap, it, too traps.

### Binary operators

The unary operator expression `<exp1> <binop> <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1`  and `r2` are values `v1` and `v2` and the expression returns 
the result of `v1 <binop> v2`.

### Tuples 
                    function call

If `<exp1>`, ..., `<expN>` have type `T1`, ..., `Tn` then 
tuple expressions `(<exp1>, ..., <expn>)` has tuple type `(T1, ..., Tn)`.

The tuple expression `(<exp1>, ..., <expn>)` evaluates the expressions `exp1` ... `expn` in order, trapping as soon as some expression <expi> traps. If no evaluation traps and `exp1`, ..., `<expn>` evaluate to values `v1`,...,`vn` then the tuple expression returns the tuple value `(v1, ... , vn)`.

The tuple projection '<exp> . <nat>' has type `Ti` provided <exp> has tuple type 
`(T1, ..., Ti, ..., Tn)`, `<nat>` == `i` and `1 <= i <= n`.

The projection `<exp> . <nat>` evaluates `exp` to a result `r`. If `r` is `trap`, then  the result is `trap`. Otherwise, `r` must be a tuple  `(v1,...,vi,...,vn)` and the result of the projection is the value `vi`.

### Option expressions

The option expression `? <exp>` has type `? T` provided `<exp>` has type `T`.

The literal `null` has type `Null`. Since `Null <: ? T` for any `T`, literal `null` also has type `? T` and signifies the "missing" value at type `? T`.

### Object projection (Member access)


The object projection '<exp> . <id>' has type `var? T` provided <exp> has object type 
`sort { var1? <id1> : T1, ..., var? <id> : T, ..., var? <idn> : Tn }` for some sort `sort`. 
                    function call

The object projection `<exp> . <id>` evaluates `exp` to a res                    function call
ult `r`. If `r` is `trap`,then the result is `trap`. Otherwise, `r` must be an o                    function call
bject value  ` { <id1> = v1,..., id = v, ..., <idn> = vn }` and the result of the proj                    function call
ection is the value `v` of field `id`.


If `var` is absent from `var? T` then the value `v` is the constant value of immutable field `<id>`, otherwise:
* if the projection occurs as the target an assignment statement;
  `v` is the mutable location of the field `<id>`. 
* otherwise, 
  `v` (of type `T`) is the value currently stored in mutable field `<id>`.
                    function call

### Assignment

The assigment `<exp1> := <exp2>` has type `()` provided:
* `<exp1>` has type `var T`, and
* `<exp2>` has type `T`.

The assignment expression `<exp1> := <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1`  and `r2` are (respectively) a location `v1` (a mutable identifier, an item of a mutable array or a mutable field of object) and a value `v2`. The expression updates the current value stored in `v1` with the new value `v2` and returns the empty tuple `()`.

### Unary Compound Assignment

The inary assigment `<exp1> <unop>= <exp2>` has type `()` provided:
* `<exp1>` has type `var T`, and
* `<exp2>` has type `T`, and
* <unop> is one of `+` (identity), `-` (negation) or  `^` (xor),
* <unop> is defined for type `(T,T) -> T`.

For unary operator `<unop>` in `+` (identity), `-` (negation) or  `^` (xor), the unary compound assignment
`<unop>= <exp>`  evaluates <exp> to a result `r`. If `r` is 'trap' the evaluation traps, otherwise `r` is a location storing value `v` and `r` is updated to 
contain the value '<unop> v'.

### Binary Compound Assignment

The assigment `<exp1> <binop>= <exp2>` has type `()` provided:
* `<exp1>` has type `var T`, and
* `<exp2>` has type `T`, and
* <binop> is defined for type `(T,T) -> T`.

For binary operator <binop>, `<exp1> <binop>= <exp1>`,
the compound assignment expression `<exp1> <binop>= <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.
Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1`  and `r2` are (respectively) a location `v1` (a mutable identifier, an item of a mutable array or a mutable field of object) and a value `v2`. The expression updates the current value, `w` stored in `v1` with the new value `w <binop> v2` and returns the empty tuple `()`.

### Arrays 

The expression `[ var? <exp>,* ]` has type `[var? T]` provided
 each expression in the sequence `<exp,>*` has type T.

 The array expression `[ var <exp0>, ..., <expn> ]` evaluates the expressions `exp0` ... `expn` in order, trapping as soon as some expression `<expi>` traps. If no evaluation traps and `exp0`, ..., `<expn>` evaluate to values `v0`,...,`vn` then the array expression returns the array value `[var? v0, ... , vn]` (of size `n+1`).

The array indexing expression '<exp1> [ <exp2> ]' has type `var? T` provided <exp> has (mutable or immutable) array type `[var? T1]`.

The projection `<exp1> . <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`. 

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is an array value, `var? [v0, ..., vn]`, and r2 is a natural integer `i`. If  'i > n' the index expression returns `trap`.

Otherwise, the index expression returns the value `v`, obtained as follows:

If `var` is absent from `var? T` then the value `v` is the constant value `vi`.

Otherwise,
* if the projection occurs as the target an assignment statement
  then `v` is the `i`-th location in the array;
* otherwise, 
  `v` is `vi`, the value currently stored in the `i`-th location of the array.

### Function Calls

The function call expression `<exp1> <T0,...,Tn>? <exp2>` has type `T` provided
* the function `<exp1>` has function type `shared? <X0 <: V0, ..., n <: Vn> U1-> U2`; and
* each type argument satisfies the corresponding type parameter's bounds:
  for each `1<= i <= n`, `Ti <: [T0/X0, ..., Tn/Xn]Vi`; and
* the argument `<exp2>` has type `[T0/X0, ..., Tn/Xn]U1`, and
* `T == [T0/X0, ..., Tn/Xn]U2`.

The call expression `<exp1> <T0,...,Tn>? <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`. 

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is a function value, `shared? func <X0 <: V0, ..., n <: Vn> pat { exp }` (for some implicit environment), and `r2` is a value `v2`. Evaluation contiues by matching `v1` against `pat`. If matching succeeds with some bindings, evaluation proceeds with `exp` using the environment of the function value (not shown) extended with those bindings. Otherwise, the pattern match has failed and the call results in `trap`.












## Patterns
```
<pat> ::=                                      patterns
  _                                              wildcard
  <id>                                           variable
  <unop>? <lit>                                  literal
  ( <pat>,* )                                    tuple or brackets
  ? <pat>                                        option
  <pat> : <typ>                                  type annotation
  <pat> or <pat>                                 disjunctive pattern
```
*Patterns* `pat` are used to bind function parameters, declare identifiers and decompose values into their constituent parts in the cases of a `switch` expression.

Matching a pattern against a value may *succeed*, binding the corresponding identifiers in the pattern to their matching values, or *fail*. Thus the result of a match is either a a successful mapping of identifiers to values, or failure.

The consequences of pattern match failure depends on the context of the pattern. 
* In a function application or `let`-binding, failure to match the formal argument pattern or `let`-pattern causes a *trap*.
* In a `case` branch of a `switch` expression, failure to match that case's pattern continues with an attempt to match the next case of the switch, trapping only when no such case remains.

### Wildcard pattern

The wildcard pattern `_`  matches a single value without binding its contents to an identifier. 


### Identifier pattern

The identifier pattern `<id>` matches a single value and binds it to the identifier `<id>`.

### Literal pattern

The literal pattern `<unop>? <lit>` matches a single value against the constant value of literal `<lit>` and fails if they are not (structurally) equal values. 

For integer literals only, the optional `<unop>` determines the sign of the value to match.

### Annotated pattern

The annotated pattern `<pat> : <typ>` matches value of `v` type `<typ>` against the pattern `<pat>`.

`<pat> : <typ>` is *not* a dynamic type test, but is used to constrain the types of identifiers bound in `<pat>`, e.g. in the argument pattern to a function.


### Option pattern

The option `? <pat>` matches a value of option type `? <typ>`. 

The match *fails* if the value is `null`. If the value is `? v`, for some value `v`, then the result of matching `? <pat>` is the result of matching `v` against <pat>.

Conversely, the `null` literal pattern may be used to test whether a value of option type is the value `null` and not `? v` for some `v`.

### Or pattern

The or pattern `<pat1> or <pat2>` is a disjunctive pattern. 

The result of matching `<pat1> or <pat2>` against a value is the result of
matching `<pat1>`, if it succeeds, or the result of matching `<pat2>`, if the first match fails. 

(Note, statically, neither `<pat1>` nor `<pat2>` may contain identifier ('<id>') patterns so a successful match always binds zero identifiers.)

## Declarations

```
<dec> ::=                                                 declaration
  <exp>                                                       expression
  let <pat> = <exp>                                           immutable
  var <id> (: <typ>)? = <exp>                                 mutable
  (new|object|actor|shared) <id>? =? { <exp-field>;* }        object
  shared? func <id>? <typ-params>? <pat> (: <typ>)? =? <exp>  function
  type <id> <typ-params>? = <typ>                             type
  actor? class <id> <typ-params>? <pat> (: <typ>)? =? <exp>   class
```

## Programs
```
<prog> ::= <dec>;*

```
