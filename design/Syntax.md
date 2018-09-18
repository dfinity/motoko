# ActorScript Syntax Sketch

Productions marked * probably deferred to later versions.


## Types
```
<typ> ::=                                     type expressions
  <id> <typ-args>?                              constructor
  actor? { <typ-field>;* }                      object
  var? <typ> [ ]                                array
  <typ> ?                                       option
  <class>? <typ-params>? <typ> -> <typ>         function
  async <typ>                                   future
  like <typ>                                    structural expansion
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
<sort> ::=
  new
  actor

<exp> ::=
  <id>                                           variable
  <lit>                                          literal
  <unop> <exp>                                   unary numeric operator
  <exp> <binop> <exp>                            binary numeric operator
  ( <exp>,* )                                    tuple
  <exp> . <nat>                                  tuple projection
  <exp> ?                                        option injection
  <sort> <id>? { <exp-field>;* }                 object
  <exp> . <id>                                   object projection
  <exp> := <exp>                                 assignment
  <unop>= <exp>                                  unary update
  <exp> <binop>= <exp>                           binary update
  [ <exp>,* ]                                    array
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
  <exp> is <exp>                                 instance-of
  <exp> : <typ>                                  type annotation
  <dec>                                          declaration (scopes to block)
* throw <exp>                                    raise exception
* try <exp> (catch <pat> <exp>)+ (<finally> <exp>)?  try
* # <id>                                             atom

<exp-field> ::=                                object expression fields
  private? <id> (: <typ>)? = <exp>                        immutable
  private? var <id> (: <typ>)? = <exp>                    mutable
  private? <id> <typ-params>? <pat> (: <typ>)? = <exp>    function (short-hand)
```

## Patterns
```
<pat> ::=                                      patterns
  _                                              wildcard
  <id>                                           variable
  <unop>? <lit>                                  literal
  ( <pat>,* )                                    tuple or brackets
  <pat> ?                                        option
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
  <exp>                                                     expression
  let <pat> = <exp>                                         immutable
  var <id> (: <typ>)? = <exp>                               mutable
  func <id>? <typ-params>? <pat> (: <typ>)? = <exp>         function
  type <id> <typ-params>? = <typ>                           type
  actor? class <id> <typ-params>? <pat> (: <typ>)? = <exp>  class
```

## Programs
```
<prog> ::= <dec>;*

```
