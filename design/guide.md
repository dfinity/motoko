# A Guide to ActorScript

## Introduction

ActorScript is a new, general purpose programming language for the
Dfinity platform.

Dfinity has chosen WebAssembly as its low-level virtual machine. The currently
available compilers targeting WebAssembly are for languages that are
either too unsafe (C, C++) or too complex (Java, C#, Rust) for mainstream
programmers. To promote correctness and reduce complexity, Dfinity
is designing its own language, ActorScript, that is safe and expressive, yet simple
and approachable to mainstream programmers.

ActorScript provides:

* A high-level language for programming Dfinity applications

* A simple ("K.I.S.S.") design and familiar syntax for average programmers

* Good and convenient support for the actor model embodied in Dfinity conisters

* A good fit for underlying Wasm and Dfinity execution model

* A forward looking design that anticipates future extensions to WebAssembly

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

# ActorScript Syntax (Sketch)

Productions marked * probably deferred to later versions.

## Types
```
<typ> ::=                                     type expressions
  <id> <typ-args>?                              constructor
  (shared|actor)? { <typ-field>;* }             object
  [ var? <typ> ]                                array
  ? <typ>                                       option
  shared <typ-params>? <typ> -> <typ>  function
  async <typ>                                   future
  ( ((<id> :)? <typ>),* )                       tuple
  Any                                           top
  None                                          bottom
* <typ> | <typ>                                 union
* # <id>                                        atom

<typ-field> ::=                               object type fields
  <id> : <typ>                                  immutable
  var <id> : <typ>                              mutable
  <id> <typ-params>? <params> : <typ>           function (short-hand)

<typ-args> ::=                                type arguments
  < <typ>,* >

<typ-params> ::=                              type parameters
  < (<id> <: <typ>),* >                         constrained
  < <id>,* >                                    unconstrained (short-hand)
```

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
  return <exp>?                                  return
  async <exp>                                    async expression
  await <exp>                                    await future (only in async)
  assert <exp>                                   assertion
  <exp> : <typ>                                  type annotation
  <dec>                                          declaration (scopes to block)
* throw <exp>                                    raise exception
* try <exp> (catch <pat> <exp>)+ (<finally> <exp>)?  try
* # <id>                                             atom

<exp-field> ::=                                object expression fields
  private? dec                                   field
  private? <id> = <exp>                          short-hand
```

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
* <pat> and <pat>                                conjunctive pattern
* { <pat-field>;* }                              object pattern
* async <pat>                                    asynchronous

<pat-field> ::=                                object pattern fields
* <id> = <pat>                                   field
```

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
