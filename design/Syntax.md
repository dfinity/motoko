# ActorScript Syntax Sketch

Productions marked * probably deferred to later versions.


## Types
```
<type> ::=                                     type expressions
  <id> <type-args>?                              constructor
  { <type-field>;* }                             object
  var? <type> [ ]                                array
  <type> ?                                       option
  <type-params>? <type> -> <type>                function
  async <type>                                   future
  like <type>                                    structural expansion
  ( ((<id> :)? <type>),* )                       tuple
* any                                            top
* <type> | <type>                                union
* # <id>                                         atom

<type-field> ::=                               object type fields
  <id> : <type>                                  immutable
  var <id> : <type>                              mutable
  <id> <type-params>? <params>+ : <type>         function (short-hand)

<type-args> ::=                                type arguments
  < <type>,* >

<type-params> ::=                              type parameters
  < <id>,* >                                     unconstrained
* < (<id> <: <type>),* >                         constrained
```

## Expressions
```
<expr> ::=
  <id>                                           variable
  <int>                                          integer literal
  <float>                                        float literal
  <char>                                         character literal
  <text>                                         unicode text literal
  <unop> <expr>                                  unary numeric operator
  <expr> <binop> <expr>                          binary numeric operator
  ( <expr>,* )                                   tuple
  <expr> . <nat>                                 tuple projection
  (<id> as)? { <expr-field>;* }                  object
  <expr> . <id>                                  object projection
  <expr> := <expr>                               assignment
  <expr> <binop>= <expr>                         binary update
  <unop>= <expr>                                 unary update
  [ <expr>,* ]                                   array
  <expr> [ <expr> ]                              array indexing
  <expr> <expr>                                  function call
  { <expr>;* }                                   block
  not <expr>                                     negation
  <expr> and <expr>                              conjunction
  <expr> or <expr>                               disjunction
  if <expr> <expr> (else <expr>)?                conditional
  switch <expr> (case <pat> <expr>)+             switch
  while <expr> <expr>                            while loop
  loop <expr> (while <expr>)?                    loop
  for <id>? in <expr> <expr>                     iteration
  label <id> <expr>                              label
  break <id> <expr>?                             break
  continue <id>                                  continue
  return <expr>?                                 return
  async <expr>                                   async expression
  await <expr>                                   await future (only in async)
  assert <expr>                                  assertion
  <expr> is <type>                               instance-of
  <expr> : <type>                                type annotation
  <dec>                                          declaration (scopes to block)
* throw <expr>                                   raise exception
* try <expr> (catch <params> <expr>)+ (<finally> <expr>)?    try
* # <id>                                                     atom

<expr-field> ::=                               object expression fields
  private? <id> (: <type>)? = <expr>                         immutable
  private? var <id> (: <type>)? = <expr>                     mutable
  private? <id> <type-params>? <pat>+ (: <type>)? = <expr>   function (short-hand)
```

## Patterns
```
<pat> ::=                                      patterns
  _                                              wildcard
  <id>                                           variable
  ( <pat>,* )                                    tuple or brackets
  <pat> : <type>                                 type annotation
* <int>                                          integer
* <char>                                         character
* <text>                                         text
* { <pat-field>;* }                              object pattern
* <pat> = <pat>                                  conjunctive pattern
* <pat> | <pat>                                  disjunctive pattern

<pat-field> ::=                                object pattern fields
* <id> = <pat>                                   field
```

## Declarations
```
<dec> ::=                                              declaration
  let <pat> = <expr>                                     immutable
  var <id> : <type> (= <expr>)?                          mutable
  func <id> <type-params>? <pat>+ (: <type>)? = <expr>   function
  type <id> <type-params>? = <type>                      type
  actor? class <id> <type-params>? <pat> = <expr>        class
```
