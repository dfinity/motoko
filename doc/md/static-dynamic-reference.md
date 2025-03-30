# Static and dynamic semantics
<!--TODO Link to new base library-->
Below is a detailed account of the semantics of Motoko programs.

For each [expression form](#expression-syntax) and each [declaration form](#declaration-syntax), this page summarizes its semantics, both in static terms based on typing and dynamic terms based on program evaluation.

### Programs

A program `<imp>;* <dec>;*` has type `T` provided:

- `<dec>;*` has type `T` under the static environment induced by the imports in `<imp>;*`.

All type and value declarations within `<dec>;*` are mutually-recursive.

A program evaluates by transitively evaluating the imports, binding their values to the identifiers in `<imp>;*` and then evaluating the sequence of declarations in `<dec>;*`.

### Libraries

Restrictions on the syntactic form of modules means that libraries can have no side-effects.

The imports of a library are local and not re-exported in its interface.

Multiple imports of the same library can be safely deduplicated without loss of side-effects.

#### Module libraries

A library `<imp>;* module <id>? (: <typ>)? =? <obj-body>` is a sequence of imports `<import>;*` followed by a single module declaration.

A library has module type `T` provided:

- `module <id>? (: <typ>)? =? <obj-body>` has (module) type `T` under the static environment induced by the imports in `<import>;*`.

A module library evaluates by transitively evaluating its imports, binding their values to the identifiers in `<imp>;*` and then evaluating `module <id>? =? <obj-body>`.

If `(: <typ>)?` is present, then `T` must be a subtype of `<typ>`.

#### Actor class libraries

The actor class library `<imp>;* <dec>` where `<dec>` is of the form `<shared-pat>? actor class <id> <typ-params>? <pat> (: <typ>)? <class-body>` has type:

``` bnf
module {
  type <id> = T;
  <id> : (U1,...,Un) -> async T
}
```

Provided that the actor class declaration `<dec>` has function type `(U1, ...​, Un) -> async T` under the static environment induced by the imports in `<import>;*`.

Notice that the imported type of the function `<id>` must be asynchronous.

An actor class library evaluates by transitively evaluating its imports, binding their values to the identifiers in `<imp>;*`, and evaluating the derived module:

``` bnf
module {
  <dec>
}
```

On ICP, if this library is imported as identifier `Lib`, then calling `await Lib.<id>(<exp1>, ..., <expn>)`, installs a fresh instance of the actor class as an isolated IC canister, passing the values of `<exp1>`, ...​, `<expn>` as installation arguments, and returns a reference to a remote actor of type `Lib.<id>`, that is, `T`. Installation is necessarily asynchronous.

#### Actor class management

On ICP, the primary constructor of an imported actor class always creates a new principal and installs a fresh instance of the class as the code for that principal.
While that is one way to install a canister on ICP, it is not the only way.

To provide further control over the installation of actor classes, Motoko endows each imported actor class with an extra, secondary constructor, for use on ICP.
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

where:

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

If `<exp>` is:

- `#new s`, where `s` has type `CanisterSettings`:

  - The call creates a fresh ICP principal `p`, with settings `s`, and installs the instance to principal `p`.

- `#install p`, where `p` has type [`Principal`](../base/Principal.md):

  - The call installs the actor to an already created ICP principal `p`. The principal must be empty, having no previously installed code, or the call will return an error.

- `#upgrade a`, where `a` has type (or supertype) `actor {}`:

  - The call installs the instance as an upgrade of actor `a`, using its current stable storage to initialize stable variables and stable memory of the new instance.

- `#reinstall a`, where `a` has type (or supertype) `actor {}`:

  - Reinstalls the instance over the existing actor `a`, discarding its stable variables and stable memory.

:::note

On ICP, calling the primary constructor `Lib.<id>` is equivalent to calling the secondary constructor `(system Lib.<id>)` with argument `(#new {settings = null})` i.e. using default settings.

:::

:::note

On ICP, calls to `Lib.<id>` and  `(system Lib.<id>)(#new ...)` must be provisioned with enough cycles for the creation of a new principal. Other call variants will use the cycles of the already allocated principal or actor.

:::

:::danger

The use of `#upgrade a` may be unsafe. Motoko will currently not verify that the upgrade is compatible with the code currently installed at `a`. A future extension may verify compatibility with a dynamic check.

The use of `#reinstall a` may be unsafe. Motoko cannot verify that the reinstall is compatible with the code currently installed in actor `a` even with a dynamic check.
A change in interface may break any existing clients of `a`. The current state of `a` will be lost.

:::

### Imports and URLs

An import `import <pat> =? <url>` declares a pattern `<pat>` bound to the contents of the text literal `<url>`.

`<url>` is a text literal that designates some resource: a local library specified with a relative path, a named module from a named package, or an external canister, referenced either by numeric canister id or by a named alias, and imported as a Motoko actor.

In detail, if `<url>` is of the form:

- `"<filepath>"` then `<pat>` is bound to the library module defined in file `<filepath>.mo`. `<filepath>` is interpreted relative to the absolute location of the enclosing file. Note the `.mo` extension is implicit and should not be included in `<url>`. For example, `import U "lib/Util"` defines `U` to reference the module in local file `./lib/Util`.

- `"mo:<package-name>/<path>"` then `<pat>` is bound to the library module defined in file `<package-path>/<path>.mo` in directory `<package-path>` referenced by package alias `<package-name>`. The mapping from `<package-name>` to `<package-path>` is determined by a compiler command-line argument `--package <package-name> <package-path>`. For example, `import L "mo:base/List"` defines `L` to reference the `List` library in package alias `base`.

- `"ic:<canisterid>"` then `<pat>` is bound to a Motoko actor whose Motoko type is determined by the canister’s IDL interface. The IDL interface of canister `<canisterid>` must be found in file `<actorpath>/<canisterid>.did`. The compiler assumes that `<actorpath>` is specified by command line argument `--actor-idl <actorpath>` and that file `<actorpath>/<canisterid>.did` exists. For example, `import C "ic:lg264-qjkae"` defines `C` to reference the actor with canister id `lg264-qjkae` and IDL file `lg264-qjkae.did`.

- `"canister:<name>"` is a symbolic reference to canister alias `<name>`. The compiler assumes that the mapping of `<name>` to `<canisterid>` is specified by command line argument `--actor-alias <name> ic:<canisterid>`. If so, `"canister:<name>"` is equivalent to `"ic:<cansterid>"` (see above). For example, `import C "canister:counter"` defines `C` to reference the actor otherwise known as `counter`.

The case sensitivity of file references depends on the host operating system so it is recommended not to distinguish resources by filename casing alone.

When building multi-canister projects with the [IC SDK](https://internetcomputer.org/docs/current/developer-docs/setup/install), Motoko programs can typically import canisters by alias (e.g. `import C "canister:counter"`), without specifying low-level canister ids (e.g. `import C "ic:lg264-qjkae"`). The SDK tooling takes care of supplying the appropriate command-line arguments to the Motoko compiler.)

Sensible choices for `<pat>` are identifiers, such as [`Array`](../base/Array.md), or object patterns like `{ cons; nil = empty }`, which allow selective importing of individual fields, under original or other names.

### Declaration fields

A declaration field `<vis>? <stab>? <dec>` defines zero or more fields of an actor or object, according to the set of variables defined by `<dec>`.

Any identifier bound by a `public` declaration appears in the type of enclosing object, module or actor and is accessible via the dot notation.

An identifier bound by a `private` or `system` declaration is excluded from the type of the enclosing object, module or actor and thus inaccessible.

In a `persistent` actor or actor class, all declarations are implicitly `stable` unless explicitly declared otherwise.
In a non-`persistent` actor or actor class, all declarations are implicitly `transient` (equivalently `flexible`) unless explicitly declared otherwise.

The declaration field has type `T` provided:

- `<dec>` has type `T`.

- If `<stab>?` is `stable`  then `T` must be a stable type (see [stability](./type-syntax-reference.md#stability)).

- If `<stab>?` is absent and the actor or actor class is `persistent`, then `T` must be a stable type (see [stability](type-syntax-reference.md#stability)).

Actor fields declared `transient` (or legacy `flexible`) can have any type, but will not be preserved across upgrades.

In the absence of any `<parenthetical>?` migration expression, sequences of declaration fields are evaluated in order by evaluating their constituent declarations, with the following exception:

- During an upgrade only, the value of a `stable` declaration is obtained as follows:

  - If the stable declaration was previously declared stable in the retired actor, its initial value is inherited from the retired actor.

  - If the stable declaration was not declared stable in the retired actor, and is thus new, its value is obtained by evaluating `<dec>`.

- For an upgrade to be safe:

  - Every stable identifier declared with type `T` in the retired actor and declared stable and of type `U` in the replacement actor, must satisfy `T <: U`.

This condition ensures that every stable variable is either fresh, requiring initialization, or its value can be safely inherited from the retired actor.
Note that stable variables may be removed across upgrades, or may simply be deprecated by an upgrade to type `Any`.

#### Migration expressions

Actors and actor class declaration may specify a migration expression, using an optional, leading `<parenthetical>` expression with a required field named `migration`.
The value of this field, a function, is applied to the stable variables of an upgraded actor, before initializing any stable fields of the declared actor.

The parenthetical expression must satisfy the following conditions:

- It must be static, that is, have no immediate side effects.
- Its `migration` field must be present and have a non-shared function type whose domain and codomain are both record types.
- The domain and the codomain must both be stable.
- Any field in the codomain must be declared as a stable field in the actor body.
- The content type of the codomain field must be a subtype of the content type of the actor's stable field.

The migration expression only affects upgrades of the actor and is otherwise ignored during fresh installation of the actor.

On upgrade, the domain of the migration function is used to construct a record of values containing the current contents of the corresponding stable fields
of the retired actor. If one of the fields is absent, the upgrade traps and is aborted.

Otherwise, we obtain an input record of stable values of the appropriate type.

The migration function is applied to the input record. If the application traps, the upgrade is aborted.

Otherwise, the application produces an output record of stable values whose type is the codomain.

The actor's declarations are evaluated in order by evaluating each declaration as usual except that
the value of a `stable` declaration is obtained as follows:

- If the stable declaration is present in the codomain, its initial value is obtained from the output record.

- Otherwise, if the stable declaration is not present in the domain and is declared stable in the retired actor,
  then its initial value is obtained from the retired actor.

- Otherwise, its value is obtained by evaluating the declaration's initalizer.

Thus a stable variable's initializer is run if the variable is not produced by the migration function and either
consumed by the migration function (by appearing in its domain) or absent in the retired actor.

For the upgrade to be safe:

- Every stable identifier declared with type `U` in the domain of the migration function
  must be declared stable for some type `T` in the retired actor, with `T <: U`.

- Every stable identifier declared with type `T` in the retired actor, not present in the domain or codomain,
  and declared stable and of type `U` in the replacement actor, must satisfy `T <: U`.

This condition ensures that every stable variable is either discarded or fresh, requiring initialization,
or that its value can be safely consumed from the output of migration or the retired actor.

The compiler will issue a warning if a migration function appears to be discarding data by consuming a field and not producing it.
The warnings should be carefully considered to verify any data loss is intentional and not accidental.

#### System fields

The declaration `<dec>` of a `system` field must be a manifest `func` declaration with one of the following names and types:

| name          | type                                                          | description         |
| ------------- | ------------------------------------------------------------- | ------------------- |
| `heartbeat`   | `() -> async ()`                                              | Heartbeat action    |
| `timer`       | `(Nat64 -> ()) -> async ()`                                   | Timer action        |
| `inspect`     | `{ caller : Principal; msg : <Variant>; arg : Blob } -> Bool` | Message predicate   |
| `preupgrade`  | `<system>() -> ()`                                            | Pre upgrade action  |
| `postupgrade` | `<system>() -> ()`                                            | Post upgrade action |

- `heartbeat`: When declared, is called on every Internet Computer subnet heartbeat, scheduling an asynchronous call to the `heartbeat` function. Due to its `async` return type, a heartbeat function may send messages and await results. The result of a heartbeat call, including any trap or thrown error, is ignored. The implicit context switch means that the time the heartbeat body is executed may be later than the time the heartbeat was issued by the subnet.

- `timer`: When declared, is called as a response of the canister global timer's expiration. The canister's global timer can be manipulated with the passed-in function argument of type `Nat64 -> ()` (taking an absolute time in nanoseconds) upon which libraries can build their own abstractions. When not declared (and in absence of the `-no-timer` flag), this system action is provided with default implementation by the compiler (additionally `setTimer` and `cancelTimer` are available as primitives).

- `inspect`: When declared, is called as a predicate on every Internet Computer ingress message with the exception of HTTP query calls. The return value, a [`Bool`](../base/Bool.md), indicates whether to accept or decline the given message. The argument type depends on the interface of the enclosing actor (see [inspect](#inspect)).

- `preupgrade`: When declared, is called during an upgrade, immediately before the current values of the retired actor’s stable variables are transferred to the replacement actor.
     Its `<system>` type parameter is implicitly assumed and need not be declared.

- `postupgrade`: When declared, is called during an upgrade, immediately after the replacement actor body has initialized its fields, inheriting values of the retired actors' stable variables, and before its first message is processed. Its `<system>` type parameter is implicitly assumed and need not be declared.

:::danger

Using the pre- and post-upgrade system methods is discouraged. It is error-prone and can render a canister unusable.
In particular, if a `preupgrade` method traps and cannot be prevented from trapping by other means, then your canister may be left in a state in which it can no longer be upgraded.
Per best practices, using these methods should be avoided if possible.

:::

These `preupgrade` and `postupgrade` system methods provide the opportunity to save and restore in-flight data structures, e.g. caches, that are better represented using non-stable types.

During an upgrade, a trap occurring in the implicit call to `preupgrade()` or `postupgrade()` causes the entire upgrade to trap, preserving the pre-upgrade actor.

##### `inspect`

Given a record of message attributes, this function produces a [`Bool`](../base/Bool.md) that indicates whether to accept or decline the message by returning `true` or `false`. The function is invoked by the system on each ingress message issue as an ICP update call, excluding non-replicated query calls. Similar to a query, any side-effects of an invocation are transient and discarded. A call that traps due to some fault has the same result as returning `false` message denial.

The argument type of `inspect` depends on the interface of the enclosing actor. In particular, the formal argument of `inspect` is a record of fields of the following types:

- `caller : Principal`: The principal, possibly anonymous, of the caller of the message.

- `arg : Blob`: The raw, binary content of the message argument.

- `msg : <variant>`: A variant of decoding functions, where `<variant> == {…​; #<id>: () → T; …​}` contains one variant per `shared` or `shared query` function, `<id>`, of the actor.
    The variant’s tag identifies the function to be called; The variant’s argument is a function that, when applied, returns the (decoded) argument of the call as a value of type `T`.

Using a variant, tagged with `#<id>`, allows the return type, `T`, of the decoding function to vary with the argument type (also `T`) of the shared function `<id>`.

The variant’s argument is a function so that one can avoid the expense of message decoding when appropriate.

:::danger

An actor that fails to declare system field `inspect` will simply accept all ingress messages.

:::

:::note

Any `shared composite query` function in the interface is *not* included in `<variant>` since, unlike a `shared query`, it can only be invoked as a non-replicated query call, never as an update call.

:::

### Sequence of declarations

A sequence of declarations `<dec>;*` occurring in a block, a program or embedded in the `<dec-field>;*` sequence of an object body has type `T` provided:

- `<dec>;*` is empty and `T == ()`, or

- `<dec>;*` is non-empty and:

  - All value identifiers bound by `<dec>;*` are distinct.

  - All type identifiers bound by `<dec>;*` are distinct.

  - Under the assumption that each value identifier `<id>` in `<dec>;*` has type `var_id? Tid`, and assuming the type definitions in `<dec>;*`:

    - Each declaration in `<dec>;*` is well-typed,.

    - Each value identifier `<id>` in bindings produced by `<dec>;*` has type `var_id? Tid`.

    - All but the last `<dec>` in `<dec>;*` of the form `<exp>` has type `()`.

    - The last declaration in `<dec>;*` has type `T`.

Declarations in `<dec>;*` are evaluated sequentially. The first declaration that traps causes the entire sequence to trap. Otherwise, the result of the declaration is the value of the last declaration in `<dec>;*`. In addition, the set of value bindings defined by `<dec>;*` is the union of the bindings introduced by each declaration in `<dec>;*`.

It is a compile-time error if any declaration in `<dec>;*` might require the value of an identifier declared in `<dec>;*` before that identifier’s declaration has been evaluated. Such use-before-define errors are detected by a simple, conservative static analysis not described here.

### Patterns

Patterns bind function parameters, declare identifiers and decompose values into their constituent parts in the cases of a `switch` expression.

Matching a pattern against a value may succeed, binding the corresponding identifiers in the pattern to their matching values, or fail. Thus the result of a match is either a successful binding, mapping identifiers of the pattern to values, or failure.

The consequences of pattern match failure depends on the context of the pattern.

- In a function application or `let`-binding, failure to match the formal argument pattern or `let`-pattern causes a trap.

- In a `case` branch of a `switch` expression, failure to match that case’s pattern continues with an attempt to match the next case of the switch, trapping only when no such case remains.

### Wildcard pattern

The wildcard pattern `_` matches a single value without binding its contents to an identifier.

### Identifier pattern

The identifier pattern `<id>` matches a single value and binds it to the identifier `<id>`.

### Literal pattern

The literal pattern `<unop>? <lit>` matches a single value against the constant value of literal `<lit>` and fails if they are not structurally equal values.

For integer literals only, the optional `<unop>` determines the sign of the value to match.

### Tuple pattern

The tuple pattern `( <pat>,* )` matches a n-tuple value against an n-tuple of patterns where both the tuple and pattern must have the same number of items. The set of identifiers bound by each component of the tuple pattern must be distinct.

The empty tuple pattern `()` is called the **unit pattern**.

Pattern matching fails if one of the patterns fails to match the corresponding item of the tuple value. Pattern matching succeeds if every pattern matches the corresponding component of the tuple value. The binding returned by a successful match is the disjoint union of the bindings returned by the component matches.

### Object pattern

The object pattern `{ <pat-field>;* }` matches an object value, a collection of named field values, against a sequence of named pattern fields. The set of identifiers bound by each field of the object pattern must be distinct. The names of the pattern fields in the object pattern must be distinct.

Object patterns support punning for concision. A punned field `<id>` is shorthand for `<id> = <id>`. Similarly, a typed, punned field `<id> : <typ>` is short-hand for `<id> = <id> : <typ>`. Both bind the matched value of the field named `<id>` to the identifier `<id>`.

Pattern matching fails if one of the pattern fields fails to match the corresponding field value of the object value. Pattern matching succeeds if every pattern field matches the corresponding named field of the object value. The binding returned by a successful match is the union of the bindings returned by the field matches.

The `<typ-sort>` of the matched object type must be determined by an enclosing type annotation or other contextual type information.

### Variant pattern

The variant pattern `# <id> <pat>?` matches a variant value (of the form `# <id'> v`) against a variant pattern. An absent `<pat>?` is shorthand for the unit pattern (`()`). Pattern matching fails if the tag `<id'>` of the value is distinct from the tag `<id>` of the pattern (i.e. `<id>` \<\> `<id'>`); or the tags are equal but the value `v` does not match the pattern `<pat>?`. Pattern matching succeeds if the tag of the value is `<id>` (i.e. `<id'>` = `<id>`) and the value `v` matches the pattern `<pat>?`. The binding returned by a successful match is just the binding returned by the match of `v` against `<pat>?`.

### Annotated pattern

The annotated pattern `<pat> : <typ>` matches value of `v` type `<typ>` against the pattern `<pat>`.

`<pat> : <typ>` is not a dynamic type test, but is used to constrain the types of identifiers bound in `<pat>`, e.g. in the argument pattern to a function.

### Option pattern

The option `? <pat>` matches a value of option type `? <typ>`.

The match fails if the value is `null`. If the value is `? v`, for some value `v`, then the result of matching `? <pat>` is the result of matching `v` against `<pat>`.

Conversely, the `null` literal pattern may be used to test whether a value of option type is the value `null` and not `? v` for some `v`.

### Or pattern

The or pattern `<pat1> or <pat2>` is a disjunctive pattern.

The result of matching `<pat1> or <pat2>` against a value is the result of matching `<pat1>`, if it succeeds, or the result of matching `<pat2>`, if the first match fails.

An `or`-pattern may contain identifier (`<id>`) patterns with the restriction that both alternatives must bind the same set of identifiers. Each identifier's type is the least upper bound of its type in `<pat1>` and `<pat2>`.

### Expression declaration

The declaration `<exp>` has type `T` provided the expression `<exp>` has type `T` . It declares no bindings.

The declaration `<exp>` evaluates to the result of evaluating `<exp>` typically for `<exp>`'s side-effect.

Note that if `<exp>` appears within a sequence of declarations, but not as the last declaration of that sequence, then `T` must be `()`.

### Let declaration

The `let` declaration `let <pat> = <exp>` has type `T` and declares the bindings in `<pat>` provided:

- `<exp>` has type `T`, and

- `<pat>` has type `T`.

The declaration `let <pat> = <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, the declaration evaluates to `trap`. If `r` is a value `v` then evaluation proceeds by matching the value `v` against `<pat>`. If matching fails, then the result is `trap`. Otherwise, the result is `v` and the binding of all identifiers in `<pat>` to their matching values in `v`.

All bindings declared by a `let` if any are immutable.

### Let-else declaration

The `let-else` declaration `let <pat> = <exp> else <block-or-exp>` has type `T` and declares the bindings in `<pat>` provided:

- `<exp>` has type `T`,

- `<pat>` has type `T`, and

- `<block-or-exp>` has type `None`.

The declaration `let <pat> = <exp> else <block-or-exp>` evaluates `<exp>` to a result `r`.
If `r` is `trap`, the declaration evaluates to `trap`.
If `r` is a value `v` then evaluation proceeds by matching the value `v` against `<pat>`.
If matching succeeds, the result is `v` and the binding of all identifiers in `<pat>` to their matching values in `v`.

If matching fails, then evaluation continues with `<block-or-exp>`, which, having type `None`, cannot proceed to the end of the declaration but may still alter control-flow to, for example `return` or `throw` to exit an enclosing function, `break` from an enclosing expression or simply diverge.

All bindings declared by a `let-else` if any are immutable.

#### Handling pattern match failures

In the presence of refutable patterns, the pattern in a `let` declaration may fail to match the value of its expression.
In such cases, the `let`-declaration will evaluate to a trap.
The compiler emits a warning for any `let`-declaration than can trap due to pattern match failure.

Instead of trapping, a user may want to explicitly handle pattern match failures.
The `let-else` declaration, `let <pat> = <exp> else <block-or-exp>`, has mostly identical static and dynamic semantics to `let`,
but diverts the program's control flow to `<block-or-exp>` when pattern matching fails, allowing recovery from failure.
The `else` expression, `<block-or-exp>`, must have type `None` and typically exits the declaration using imperative control flow
constructs such as `throw`, `return`, `break` or non-returning functions such as `Debug.trap(...)` that all produce a result of type `None`.
Any compilation warning that is produced for a `let` can be silenced by handling the potential pattern-match failure using `let-else`.

### Var declaration

The variable declaration `var <id> (: <typ>)? = <exp>` declares a mutable variable `<id>` with initial value `<exp>`. The variable’s value can be updated by assignment.

The declaration `var <id>` has type `()` provided:

- `<exp>` has type `T`; and

- If the annotation `(:<typ>)?` is present, then `T` == `<typ>`.

Within the scope of the declaration, `<id>` has type `var T` (see [Assignment](#assignment)).

Evaluation of `var <id> (: <typ>)? = <exp>` proceeds by evaluating `<exp>` to a result `r`. If `r` is `trap`, the declaration evaluates to `trap`. Otherwise, the `r` is some value `v` that determines the initial value of mutable variable `<id>`. The result of the declaration is `()` and `<id>` is bound to a fresh location that contains `v`.

### Type declaration

The declaration `type <id> <type-typ-params>? = <typ>` declares a new type constructor `<id>`, with optional type parameters `<type-typ-params>` and definition `<typ>`.

The declaration `type C< X0 <: T0, …​, Xn <: Tn > = U` is well-formed provided:

- Type parameters `X0`, …​, `Xn` are distinct, and

- Assuming the constraints `X0 <: T0`, …​, `Xn <: Tn`:

  - Constraints `T0`, …​, `Tn` are well-formed.

  - Definition `U` is well-formed.

  - It is productive (see [Productivity](#productivity)).

  - It is non-expansive (see [Expansiveness](#expansiveness)).

In scope of the declaration `type C< X0<:T0, …​, Xn <: Tn > = U`, any well-formed type `C< U0, …​, Un >` is equivalent to its expansion `U [ U0/X0, …​, Un/Xn ]`. Distinct type expressions that expand to identical types are inter-changeable, regardless of any distinction between type constructor names. In short, the equivalence between types is structural, not nominal.

#### Productivity

A type is **productive** if recursively expanding any outermost type constructor in its definition eventually produces a type other than the application of a type constructor.

Motoko requires all type declarations to be productive.

For example, the following type definitions are all productive and legal:

``` motoko no-repl
  type Person = { first : Text; last : Text };

  type List<T> = ?(T, List<T>);

  type Fst<T, U> = T;

  type Ok<T> = Fst<Any, Ok<T>>;
```

But in contrast, the following type definitions are all non-productive, since each definition will enter a loop after one or more expansions of its body:

``` motoko no-repl
  type C = C;

  type D<T, U> = D<U, T>;

  type E<T> = F<T>;
  type F<T> = E<T>;

  type G<T> = Fst<G<T>, Any>;
```

#### Expansiveness

A set of mutually recursive type or class declarations will be rejected if the set is **expansive**.

Expansiveness is a syntactic criterion. To determine whether a set of singly or mutually recursive type definitions is expansive, for example:

``` motoko no-repl
  type C<...,Xi,...> = T;
  ...
  type D<...,Yj,...> = U;
```

Take these definitions and construct a directed graph whose vertices are the formal type parameters identified by position, `C#i`, with the following `{0,1}`-labeled edges:

- For each occurrence of parameter `C#i` as immediate, `j`-th argument to type `D<…​,C#i,…​>`, add a **non-expansive**, `0`-labeled edge,`C#i -0-> D#j`.

- For each occurrence of parameter `C#i` as a proper sub-expression of the `j`-th argument to type `D<…​,T[C#i],..>` add an **expansive** `1`-labeled edge, `C#i -1-> D#j`.

The graph is expansive if, and only if, it contains a cycle with at least one expansive edge.

For example, the type definition that recursively instantiates `List` at the same parameter `T`, is non-expansive and accepted:

``` motoko no-repl
  type List<T> = ?(T, List<T>);
```

A similar looking definition that recursively instantiates `Seq` with a larger type, `[T]`, containing `T`, is expansive and rejected:

``` motoko no-repl
  type Seq<T> = ?(T, Seq<[T]>);
```

- Type `List<T>` is non-expansive because its graph, `{ List#0 -0-> List#0 }`, though cyclic, has no expansive edge.

- Type `Seq<T>`, on the other hand, is expansive, because its graph, `{ Seq#0 -1-> Seq#0 }`, has a cycle that includes an expansive edge.

### Object declaration

Declaration `<sort> <id>? (: <typ>)? =? <obj-body>`, where `<obj-body>` is of the form `{ <dec-field>;* }`, declares an object with optional identifier `<id>` and zero or more fields `<dec-field>;*`. Fields can be declared with `public` or `private` visibility; if the visibility is omitted, it defaults to `private`.

The qualifier `<sort>` (one of `persistent? actor <migration>?`, `module` or `object`) specifies the `<typ-sort>` of the object’s type (`actor`, `module` or `object`, respectively).
The sort imposes restrictions on the types of the public object fields.

Let `T = <typ-sort> { [var0] id0 : T0, …​ , [varn] idn : T0 }` denote the type of the object. Let `<dec>;*` be the sequence of declarations embedded in `<dec-field>;*`. The object declaration has type `T` provided that:

1. Type `T` is well-formed for sort `<typ-sort>`, and

2. Under the assumption that `<id> : T`,

    - The sequence of declarations `<dec>;*` has type `Any` and declares the disjoint sets of private and public identifiers, `Id_private` and `Id_public` respectively, with types `T(id)` for `id` in `Id == Id_private union Id_public`, and

    - `{ id0, …​, idn } == Id_public`, and

    - For all `i in 0 <= i <= n`, `[vari] Ti == T(idi)`.

3. If `<sort>` is `module`, then the declarations in `<dec>;*` must be *static* (see [static declarations](#static-declarations)).

Note that the first requirement imposes further constraints on the field types of `T`. In particular, if the sort is `actor` then:

- All public fields must be non-`var` immutable `shared` functions. The public interface of an actor can only provide asynchronous messaging via shared functions.

Because actor construction is asynchronous, an actor declaration can only occur in an asynchronous context, i.e. in the body of a non-`<query>` `shared` function, `async` expression or `async*` expression.

Evaluation of `<sort>? <id>? =? { <dec-field>;* }` proceeds by binding `<id>`, if present, to the eventual value `v`, and evaluating the declarations in `<dec>;*`. If the evaluation of `<dec>;*` traps, so does the object declaration. Otherwise, `<dec>;*` produces a set of bindings for identifiers in `Id`. let `v0`, …​, `vn` be the values or locations bound to identifiers `<id0>`, …​, `<idn>`. The result of the object declaration is the object `v == <typ-sort> { <id0> = v1, …​, <idn> = vn}`.

If `<id>?` is present, the declaration binds `<id>` to `v`. Otherwise, it produces the empty set of bindings.

If `(: <typ>)?` is present, then `T` must be a subtype of `<typ>`.

:::danger

Actor declaration is implicitly asynchronous and the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes.

:::

#### Static declarations

A declaration is **static** if it is:

- A `type` declaration.

- A `class` declaration.

- A `let` declaration with a static pattern and a static expression.

- A module, function or object declaration that de-sugars to a static `let` declaration.

- A static expression.

An expression is static if it is:

- A literal expression.

- A tuple of static expressions.

- An object of static expressions.

- A variant or option with a static expression.

- An immutable array.

- Field access and projection from a static expression.

- A module expression.

- A function expression.

- A static declaration.

- An `ignore` of a static expression.

- A block, all of whose declarations are static.

- A type annotation with a static expression.

A pattern is static if it is:

- An identifier.

- A wildcard.

- A tuple of static patterns.

- Type annotation with a static pattern.

<!--
why not record patterns?
-->

Static phrases are designed to be side-effect free, allowing the coalescing of duplicate library imports.

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

Named function definitions support recursion, i.e. a named function can call itself.

:::note

In compiled code, `shared` functions can only appear as public actor fields.

:::

### Class declaration

The class declaration `<shared-pat>? <sort>? class <id>? <typ-params>? <pat> (: <typ>)? <class-body>` is sugar for pair of a type and function declaration:

``` bnf
<shared-pat>? <sort>? class <id> <typ-params>? <pat> (: <typ>)? <class-body> :=
  type <id> <type-typ-params>? = <sort> { <typ-field>;* };
  <shared-pat>? func <id> <typ-params>? <pat> : async? <id> <typ-args> =
    async? <sort> <id_this>? <obj-body>
```

where:

- `<shared-pat>?`, when present, requires `<sort>` == `persistent? actor`, and provides access to the `caller` of an `actor` constructor, and

- `<typ-args>?` and `<type-typ-params>?` is the sequence of type identifiers bound by `<typ-params>?`, if any, and

- `<typ-field>;*` is the set of public field types inferred from `<dec-field>;*`.

- `<obj-body>` is the object body of `<class-body>`.

- `<id_this>?` is the optional **this** or **self** parameter of `<class-body>`.

- `async?` is present, if only if, `<sort>` == `persistent? actor`.

Note `<shared-pat>?` must not be of the form `shared <query> <pat>?`: a constructor, unlike a function, cannot be a `query` or `composite query`.

An absent `<shared-pat>?` defaults to `shared` when `<sort>` = `persistent? actor`.

If `sort` is `persistent? actor`, then:

- `<typ-args>?` must be absent or empty, such that `actor` classes cannot have type parameters.

- `<pat>`'s type must be shared (see [shareability](/doc/md/type-syntax-reference.md#shareability)).

- `(: <typ>)?`, if present, must be of the form `: async T` for some actor type `T`. Actor instantiation is asynchronous.

If `(: <typ>)` is present, then the type `<async?> <typ-sort> {  <typ_field>;* }` must be a subtype of the annotation `<typ>`. In particular, the annotation is used only to check, but not affect, the inferred type of function `<id>`. `<typ-sort>` is just `<sort>` erasing any `persistent?` modifier.

The class declaration has the same type as function `<id>` and evaluates to the function value `<id>`.

### Identifiers

The identifier expression `<id>` has type `T` provided `<id>` is in scope, defined and declared with explicit or inferred type `T`.

The expression `<id>` evaluates to the value bound to `<id>` in the current evaluation environment.

### Literals

A literal has type `T` only when its value is within the prescribed range of values of type `T`.

The literal (or constant) expression `<lit>` evaluates to itself.

### Unary operators

The unary operator `<unop> <exp>` has type `T` provided:

- `<exp>` has type `T`, and

- The category of `<unop>` is a category of `T`.

The unary operator expression `<unop> <exp>` evaluates `<exp>` to a result. If the result is a value `v`, it returns the result of `<unop> v`. If the result is `trap`, the entire expression results in `trap`.

### Binary operators

The binary operator expression `<exp1> <binop> <exp2>` has type `T` provided:

- `<exp1>` has type `T`.

- `<exp2>` has type `T`.

- The category of `<binop>` is a category of `T`.

The binary operator expression `<exp1> <binop> <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` and `r2` are values `v1` and `v2` and the expression returns the result of `v1 <binop> v2`.

### Relational operators

The relational expression `<exp1> <relop> <exp2>` has type [`Bool`](../base/Bool.md) provided:

- `<exp1>` has type `T`.

- `<exp2>` has type `T`.

- `<relop>` is equality `==` or inequality `!=`, `T` is shared, and `T` is the least type such that `<exp1>` and `<exp2>` have type `T`.

- Ihe category O (Ordered) is a category of `T` and `<relop>`.

The binary operator expression `<exp1> <relop> <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` and `r2` are values `v1` and `v2` and the expression returns the Boolean result of `v1 <relop> v2`.

For equality and inequality, the meaning of `v1 <relop> v2` depends on the compile-time, static choice of `T`. This means that only the static types of `<exp1>` and `<exp2>` are considered for equality, and not the run-time types of `v1` and `v2`, which, due to subtyping, may be more precise than the static types.

### Pipe operators and placeholder expressions

The pipe expression `<exp1> |> <exp2>` binds the value of `<exp1>` to the special placeholder expression `_`, that can be referenced in `<exp2>` and recursively in `<exp1>`.
Referencing the placeholder expression outside of a pipe operation is a compile-time error.

The pipe expression `<exp1> |> <exp2>` is just syntactic sugar for a `let` binding to a placeholder identifier, `p`:

``` bnf
do { let p = <exp1>; <exp2> }
```

The placeholder expression `_` is just syntactic sugar for the expression referencing the placeholder identifier:

``` bnf
p
```

The placeholder identifier, `p`, is a fixed, reserved identifier that cannot be bound by any other expression or pattern other than a pipe operation, and can only be referenced using the placeholder expression `_`.

`|>` has lowest precedence amongst all operators except `:` and associates to the left.

Judicious use of the pipe operator allows one to express a more complicated nested expression by piping arguments of that expression into their nested positions within that expression.

For example:

``` motoko no-repl
Iter.range(0, 10) |>
  Iter.toList _ |>
    List.filter<Nat>(_, func n { n % 3 == 0 }) |>
      { multiples = _ };
```

This may be a more readable rendition of:

``` motoko no-repl
{ multiples =
   List.filter<Nat>(
     Iter.toList(Iter.range(0, 10)),
     func n { n % 3 == 0 }) };
```

Above, each occurrence of `_` refers to the value of the left-hand-size of the nearest enclosing pipe operation, after associating nested pipes to the left.

Note that the evaluation order of the two examples is different, but consistently left-to-right.

:::note

Although syntactically identical, the placeholder expression is semantically distinct from, and should not be confused with, the wildcard pattern `_`.

Occurrences of the forms can be distinguished by their syntactic role as pattern or expression.

:::

### Tuples

Tuple expression `(<exp1>, …​, <expn>)` has tuple type `(T1, …​, Tn)`, provided `<exp1>`, …​, `<expn>` have types `T1`, …​, `Tn`.

The tuple expression `(<exp1>, …​, <expn>)` evaluates the expressions `exp1` …​ `expn` in order, trapping as soon as some expression `<expi>` traps. If no evaluation traps and `exp1`, …​, `<expn>` evaluate to values `v1`,…​,`vn` then the tuple expression returns the tuple value `(v1, …​ , vn)`.

The tuple projection `<exp> . <nat>` has type `Ti` provided `<exp>` has tuple type `(T1, …​, Ti, …​, Tn)`, `<nat>` == `i` and `1 <= i <= n`.

The projection `<exp> . <nat>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a tuple `(v1,…​,vi,…​,vn)` and the result of the projection is the value `vi`.

The empty tuple expression `()` is called the **unit value**.

### Option expressions

The option expression `? <exp>` has type `? T` provided `<exp>` has type `T`.

The literal `null` has type `Null`. Since `Null <: ? T` for any `T`, literal `null` also has type `? T` and signifies the "missing" value at type `? T`.

### Variant injection

The variant injection `# <id> <exp>` has variant type `{# id T}` provided:

- `<exp>` has type `T`.

The variant injection `# <id>` is just syntactic sugar for `# <id> ()`.

The variant injection `# <id> <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a value `v` and the result of the injection is the tagged value `# <id> v`.

The tag and contents of a variant value can be tested and accessed using a [variant pattern](#variant-pattern).

### Objects

Objects can be written in literal form `{ <exp-field>;* }`, consisting of a list of expression fields:

``` bnf
<exp-field> ::=                                Object expression fields
  var? <id> (: <typ>) = <exp>                    Field
  var? <id> (: <typ>)                            Punned field
```

Such an object literal, sometimes called a record, is equivalent to the object declaration `object { <dec-field>;* }` where the declaration fields are obtained from the expression fields by prefixing each of them with `public let`, or just `public` in case of `var` fields. However, unlike declarations, the field list does not bind each `<id>` as a local name within the literal, i.e., the field names are not in scope in the field expressions.

Object expressions support **punning** for concision. A punned field `<id>` is shorthand for `<id> = <id>`; Similarly, a typed, punned field `<id> : <typ>` is short-hand for `<id> = <id> : <typ>`. Both associate the field named `<id>` with the value of the identifier `<id>`.

### Object combination/extension

Objects can be combined and/or extended using the `and` and `with` keywords.

A record expression `{ <exp> (and <exp>)* (with <exp-field>;+)? }` merges the objects or module) specified as base expressions, and augments the result to also contain the specified fields. The `with <exp-field>;+` clause can be omitted when at least two bases appear and none have common field labels.
Thus the field list serves to:

- Disambiguate field labels occurring more than once in the bases.
- Define new fields.
- Override existing fields and their types.
- Add new `var` fields.
- Redefine existing `var` fields from some base to prevent aliasing.

The resulting type is determined by the bases' and explicitly given fields' static type.

Any `var` field from some base must be overwritten in the explicit field list. This prevents introducing aliases of `var` fields.

The record expression `{ <exp1> and ... <expn> with <exp-field1>; ... <exp_fieldn>; }` has type `T` provided:

- The record `{ <exp-field1>; ... <exp_fieldm>; }` has record type `{ field_tys } == { var? <id1> : U1; ... var? <idm> : Um }`.

- Let `newfields == { <id1> , ..., <idm> }` be the set of new field names.

- Considering value fields:

  - Base expression `<expi>` has object or module type `sorti { field_tysi } == sorti { var? <idi1> : Ti1, …​, var? <idik> : Tik }` where `sorti <> Actor`.

    Let `fields(i) == { <idi1>, ..., <idik> }` be the set of static field names of base `i`. Then:

  - `fields(i)` is disjoint from `newfields` (possibly by applying subtyping to the type of `<expi>`).

  - No field in `field_tysi` is a `var` field.

  - `fields(i)` is disjoint from `fields(j)` for `j < i`.

- Considering type fields:

  - Base expression `<expi>` has object or module type `sorti { typ_fieldsi } == sorti { type <idj1> = … , …, type <idik> = … }` where `sorti <> Actor`.

  - `typ_fieldsi` *agrees* with `typ_fieldsj` for `j < i`.

- `T` is `{ typ_fieldsi fields_tys1 ... typ_fieldsm fields_tysm field_tys }`.

Here, two sequences of type fields agree only when any two type fields of the same name in each sequence have equivalent definitions.

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

- If the projection occurs as the target of an assignment expression then `w` is just `v`, the mutable location in field `<id>`.

- Otherwise, `w` (of type `T`) is the value currently stored at the mutable location `v` in field `<id>`.

### Special member access

The iterator access `<exp> . <id>` has type `T` provided `<exp>` has type `U`, and `U`,`<id>` and `T` are related by a row of the following table:

|            |         |                         |                                              |
| ---------- | ------- | ----------------------- | -------------------------------------------- |
| U          | `<id>`  | T                       | Description                                  |
| [`Text`](../base/Text.md)     | `size`  | [`Nat`](../base/Nat.md)                   | Size (or length) in characters               |
| [`Text`](../base/Text.md)     | `chars` | `{ next: () -> Char? }` | Character iterator, first to last            |
|            |         |                         |                                              |
| [`Blob`](../base/Blob.md)     | `size`  | [`Nat`](../base/Nat.md)                   | Size in bytes                                |
| [`Blob`](../base/Blob.md)     | `vals`  | `{ next: () -> Nat8? }` | Byte iterator, first to last                 |
|            |         |                         |                                              |
| `[var? T]` | `size`  | [`Nat`](../base/Nat.md)                   | Number of elements                           |
| `[var? T]` | `get`   | `Nat -> T`              | Indexed read function                        |
| `[var? T]` | `keys`  | `{ next: () -> Nat? }`  | Index iterator, by ascending index           |
| `[var? T]` | `vals`  | `{ next: () -> T? }`    | Value iterator, by ascending index           |
| `[var T]`  | `put`   | `(Nat, T) -> ()`        | Indexed write function (mutable arrays only) |

The projection `<exp> . <id>` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`. Otherwise, `r` must be a value of type `U` and the result of the projection is a value of type `T` whose semantics is given by the Description column of the previous table.

:::note

the `chars`, `vals`, `keys` and `vals` members produce stateful iterator objects than can be consumed by `for` expressions (see [for](#for)).

:::

### Assignment

The assignment `<exp1> := <exp2>` has type `()` provided:

- `<exp1>` has type `var T`.

- `<exp2>` has type `T`.

The assignment expression `<exp1> := <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1` and `r2` are respectively a location `v1`, a mutable identifier, an item of a mutable array or a mutable field of an object, and a value `v2`. The expression updates the current value stored in `v1` with the new value `v2` and returns the empty tuple `()`.

### Unary compound assignment

The unary compound assignment `<unop>= <exp>` has type `()` provided:

- `<exp>` has type `var T`.

- `<unop>`'s category is a category of `T`.

The unary compound assignment `<unop>= <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap` the evaluation traps, otherwise `r` is a location storing value `v` and `r` is updated to contain the value `<unop> v`.

### Binary compound assignment

The binary compound assignment `<exp1> <binop>= <exp2>` has type `()` provided:

- `<exp1>` has type `var T`.

- `<exp2>` has type `T`.

- `<binop>`'s category is a category of `T`.

For binary operator `<binop>`, the compound assignment expression `<exp1> <binop>= <exp2>` evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise `r1` and `r2` are respectively a location `v1`, a mutable identifier, an item of a mutable array or a mutable field of object, and a value `v2`. The expression updates the current value, `w` stored in `v1` with the new value `w <binop> v2` and returns the empty tuple `()`.

### Arrays

The expression `[ var? <exp>,* ]` has type `[var? T]` provided each expression `<exp>` in the sequence `<exp>,*` has type T.

The array expression `[ var <exp0>, …​, <expn> ]` evaluates the expressions `exp0` …​ `expn` in order, trapping as soon as some expression `<expi>` traps. If no evaluation traps and `exp0`, …​, `<expn>` evaluate to values `v0`,…​,`vn` then the array expression returns the array value `[var? v0, …​ , vn]` of size `n+1`.

### Array indexing

The array indexing expression `<exp1> [ <exp2> ]` has type `var? T` provided:

- `<exp>` has mutable or immutable array type `[var? T1]`.

The expression `<exp1> [ <exp2> ]` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is an array value, `var? [v0, …​, vn]`, and `r2` is a natural integer `i`. If `i > n` the index expression returns `trap`.

Otherwise, the index expression returns the value `v`, obtained as follows:

- If `var` is absent from `var? T` then the value `v` is the constant value `vi`.

Otherwise,

- If the indexing occurs as the target of an assignment expression then `v` is the `i`-th mutable location in the array.

- Otherwise, `v` is `vi`, the value currently stored in the `i`-th location of the array.

### Function calls

The function call expression `<parenthetical>? <exp1> <T0,…​,Tn>? <exp2>` has type `T` provided:

- The function `<exp1>` has function type `<shared>? < X0 <: V0, ..., Xn <: Vn > U1-> U2`.

- If `<T0,…​,Tn>?` is absent but n > 0 then there exists minimal `T0, …​, Tn` inferred by the compiler such that:

- Each type argument satisfies the corresponding type parameter’s bounds: for each `1 <= i <= n`, `Ti <: [T0/X0, …​, Tn/Xn]Vi`.

- The argument `<exp2>` has type `[T0/X0, …​, Tn/Xn]U1`.

- `T == [T0/X0, …​, Tn/Xn]U2`.

The call expression `<exp1> <T0,…​,Tn>? <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, then the result is `trap`.

Otherwise, `exp2` is evaluated to a result `r2`. If `r2` is `trap`, the expression results in `trap`.

Otherwise, `r1` is a function value, `<shared-pat>? func <X0 <: V0, …​, n <: Vn> <pat1> { <exp> }` (for some implicit environment), and `r2` is a value `v2`. If `<shared-pat>` is present and of the form `shared <query>? <pat>` then evaluation continues by matching the record value `{caller = p}` against `<pat>`, where `p` is the [`Principal`](../base/Principal.md) invoking the function, typically a user or canister. Matching continues by matching `v1` against `<pat1>`. If pattern matching succeeds with some bindings, then evaluation returns the result of `<exp>` in the environment of the function value not shown extended with those bindings. Otherwise, some pattern match has failed and the call results in `trap`.

A `<parenthetical>`, when present, modifies dynamic attributes of the message send (provided that the return type `T` is of form `async U`, i.e. a future). The recognized attributes are:

- `cycles : Nat` to attach cycles
- `timeout : Nat32` to introduce a timeout for best-effort message execution.

:::note

The exhaustiveness side condition on `shared` function expressions ensures that argument pattern matching cannot fail (see [functions](#functions)).

:::

:::note

Calls to local functions with `async` return type and `shared` functions can fail due to a lack of canister resources.
Such failures will result in the call immediately throwing an error with  `code` `#call_error { err_code = n }`, where `n` is the non-zero `err_code` value returned by ICP.

Earlier versions of Motoko would trap in such situations, making it difficult for the calling canister to mitigate such failures.
Now, a caller can handle these errors using enclosing `try ... catch ...` expressions, if desired.

:::

### Functions

The function expression `<shared-pat>? func < X0 <: T0, …​, Xn <: Tn > <pat1> (: U2)? =? <block-or-exp>` has type `<shared>? < X0 <: T0, ..., Xn <: Tn > U1-> U2` if, under the assumption that `X0 <: T0, …​, Xn <: Tn`:

- `<shared-pat>?` is of the form `shared <query>? <pat>` if and only if `<shared>?` is `shared <query>?` (the `<query>` modifiers must agree, i.e. are either both absent, both `query`, or both `composite query`).

- All the types in `T0, …​, Tn` and `U2` are well-formed and well-constrained.

- Pattern `<pat>` has *context type* `{ caller : Principal }`.

- Pattern `<pat1>` has type `U1`.

- If the function is `shared` then `<pat>` and `<pat1>` must be exhaustive.

- Expression `<block-or-exp>` has type return type `U2` under the assumption that `<pat1>` has type `U1`.

`<shared-pat>? func <typ-params>? <pat1> (: <typ>)? =? <block-or-exp>` evaluates to a function value denoted `<shared-pat>? func <typ-params>? <pat1> = <exp>`, that stores the code of the function together with the bindings from the current evaluation environment needed to evaluate calls to the function value.

Note that a `<shared-pat>` function may itself be `shared <pat>` or `shared query <pat>` or  `shared composite query <pat>`.

- A `shared <pat>` function may be invoked from a remote caller. Unless causing a trap, the effects on the callee persist beyond completion of the call.

- A `shared query <pat>` function may be also be invoked from a remote caller, but the effects on the callee are transient and discarded once the call has completed with a result (whether a value or error).

- A `shared composite query <pat>` function may only be invoked as an ingress message, not from a remote caller.
    Like a query, the effects on the callee are transient and discarded once the call has completed with a result, whether a value or error.
    In addition, intermediate state changes made by the call are not observable by any of its own `query`  or `composite query` callees.

In either case, `<pat>` provides access to a context value identifying the *caller* of the shared function.

:::note

The context type is a record to allow extension with further fields in future releases.

:::

Shared functions have different capabilities dependent on their qualification as `shared`, `shared query` or `shared composite query`.

A `shared` function may call any `shared` or `shared query` function, but no `shared composite query` function.
A `shared query` function may not call any `shared`, `shared query` or `shared composite query` function.
A `shared composite query` function may call any `shared query` or `shared composite query` function, but no `shared` function.

All varieties of shared functions may call unshared functions.

Composite queries, though composable, can only be called externally such as from a frontend and cannot be initiated from an actor.

### Blocks

The block expression `{ <dec>;* }` has type `T` provided the last declaration in the sequence `<dec>;*` has type `T`. All identifiers declared in block must be distinct type identifiers or distinct value identifiers and are in scope in the definition of all other declarations in the block.

The bindings of identifiers declared in `{ dec;* }` are local to the block.

The type system ensures that a value identifier cannot be evaluated before its declaration has been evaluated, precluding run-time errors at the cost of rejection some well-behaved programs.

Identifiers whose types cannot be inferred from their declaration, but are used in a forward reference, may require an additional type annotation (see [annotated pattern](#annotated-pattern)) to satisfy the type checker.

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

- The expression appears in the body, `<block>`, of an enclosing option block of the form `do ? <block>` (see [option block](#option-block)).

- `<exp>` has option type `? T`.

The expression `<exp> !` evaluates `<exp>` to a result `r`. If `r` is `trap`, then the result is `trap`; if `r` is `null`, execution breaks with value `null` from the nearest enclosing option block of form `do ? <block>`; otherwise, `r` is `? v` and execution continues with value `v`.

### Not

The not expression `not <exp>` has type [`Bool`](../base/Bool.md) provided `<exp>` has type [`Bool`](../base/Bool.md).

If `<exp>` evaluates to `trap`, the expression returns `trap`. Otherwise, `<exp>` evaluates to a Boolean value `v` and the expression returns `not v`, the Boolean negation of `v`.

### And

The and expression `<exp1> and <exp2>` has type [`Bool`](../base/Bool.md) provided `<exp1>` and `<exp2>` have type [`Bool`](../base/Bool.md).

The expression `<exp1> and <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise `r1` is a Boolean value `v`. If `v == false` the expression returns the value `false` (without evaluating `<exp2>`). Otherwise, the expression returns the result of evaluating `<exp2>`.

### Or

The or expression `<exp1> or <exp2>` has type [`Bool`](../base/Bool.md) provided `<exp1>` and `<exp2>` have type [`Bool`](../base/Bool.md).

The expression `<exp1> and <exp2>` evaluates `exp1` to a result `r1`. If `r1` is `trap`, the expression results in `trap`. Otherwise `r1` is a Boolean value `v`. If `v == true` the expression returns the value `true` without evaluating `<exp2>`. Otherwise, the expression returns the result of evaluating `<exp2>`.

### If

The expression `if <exp1> <exp2> (else <exp3>)?` has type `T` provided:

- `<exp1>` has type [`Bool`](../base/Bool.md).

- `<exp2>` has type `T`.

- `<exp3>` is absent and `() <: T`.

- `<exp3>` is present and has type `T`.

The expression evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, `r1` is the value `true` or `false`. If `r1` is `true`, the result is the result of evaluating `<exp2>`. Otherwise, `r1` is `false` and the result is `()` (if `<exp3>` is absent) or the result of `<exp3>` (if `<exp3>` is present).

### Switch

The switch expression `switch <exp> { (case <pat> <block-or-exp>;)+ }` has type `T` provided:

- `exp` has type `U`.

- For each case `case <pat> <block-or-exp>` in the sequence `(case <pat> <block-or-exp>;)+`.

- Pattern `<pat>` has type `U`.

- Expression `<block-or-exp>` has type `T`.

The expression evaluates `<exp>` to a result `r`. If `r` is `trap`, the result is `trap`. Otherwise, `r` is some value `v`. Let `case <pat> <block-or-exp>;` be the first case in `(case <pat> <block-or-exp>;)+` such that `<pat>` matches `v` for some binding of identifiers to values. Then result of the `switch` is the result of evaluating `<block-or-exp>` under that binding. If no case has a pattern that matches `v`, the result of the switch is `trap`.

### While

The expression `while <exp1> <exp2>` has type `()` provided:

- `<exp1>` has type [`Bool`](../base/Bool.md).

- `<exp2>` has type `()`.

The expression evaluates `<exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, `r1` is the value `true` or `false`. If `r1` is `true`, the result is the result of re-evaluating `while <exp1> <exp2>`. Otherwise, the result is `()`.

### Loop

The expression `loop <block-or-exp>` has type `None` provided `<block-or-exp>` has type `()`.

The expression evaluates `<block-or-exp>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, the result is the result of re-evaluating `loop <block-or-exp>`.

### Loop-while

The expression `loop <block-or-exp1> while <exp2>` has type `()` provided:

- `<block-or-exp1>` has type `()`.

- `<exp2>` has type [`Bool`](../base/Bool.md).

The expression evaluates `<block-or-exp1>` to a result `r1`. If `r1` is `trap`, the result is `trap`. Otherwise, evaluation continues with `<exp2>`, producing result `r2`. If `r2` is `trap` the result is `trap`. Otherwise, if `r2` is `true`, the result is the result of re-evaluating `loop <block-or-exp1> while <exp2>`. Otherwise, `r2` is false and the result is `()`.

### For

The iterator expression `for ( <pat> in <exp1> ) <block-or-exp2>` has type `()` provided:

- `<exp1>` has type `{ next : () → ?T }`.

- pattern `<pat>` has type `T`.

- expression `<block-or-exp2>` has type `()` (in the environment extended with the bindings of `<pat>`).

The `for`-expression is syntactic sugar for the following, where `x` and `l` are fresh identifiers:

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

In particular, the `for` loop will trap if evaluation of `<exp1>` traps; as soon as `x.next()` traps, or the value of `x.next()` does not match pattern `<pat>`, or when `<block-or-exp2>` traps.

:::note

Although general purpose, `for` loops are commonly used to consume iterators produced by [special member access](#special-member-access) to, for example, loop over the indices (`a.keys()`) or values (`a.values()`) of some array, `a`.

:::

### Label

The label-expression `label <id> (: <typ>)? <block-or-exp>` has type `T` provided:

- `(: <typ>)?` is absent and `T` is unit; or `(: <typ>)?` is present and `T == <typ>`.

- `<block-or-exp>` has type `T` in the static environment extended with `label l : T`.

The result of evaluating `label <id> (: <typ>)? <block-or-exp>` is the result of evaluating `<block-or-exp>`.

### Labeled loops

If `<exp>` in `label <id> (: <typ>)? <exp>` is a looping construct:

- `while (exp2) <block-or-exp1>`.

- `loop <block-or-exp1> (while (<exp2>))?`.

- `for (<pat> in <exp2>) <block-or-exp1>`.

The body, `<exp1>`, of the loop is implicitly enclosed in `label <id_continue> (…​)` allowing early continuation of the loop by the evaluation of expression `continue <id>`.

`<id_continue>` is a fresh identifier that can only be referenced by `continue <id>`, through its implicit expansion to `break <id_continue>`.

### Break

The expression `break <id>` is equivalent to `break <id> ()`.

The expression `break <id> <exp>` has type `None` provided:

- The label `<id>` is declared with type `label <id> : T`.

- `<exp>` has type `T`.

The evaluation of `break <id> <exp>` evaluates `<exp>` to some result `r`. If `r` is `trap`, the result is `trap`. If `r` is a value `v`, the evaluation abandons the current computation up to the dynamically enclosing declaration `label <id> …​` using the value `v` as the result of that labelled expression.

### Continue

The expression `continue <id>` is equivalent to `break <id_continue>`, where `<id_continue>` is implicitly declared around the bodies of `<id>`-labelled looping constructs (see [labeled loops](#labeled-loops)).

### Return

The expression `return` is equivalent to `return ()`.

The expression `return <exp>` has type `None` provided:

- `<exp>` has type `T`.

- and either one of:

  - `T` is the return type of the nearest enclosing function with no intervening `async` expression.

  - `async T` is the type of the nearest enclosing, perhaps implicit, `async` expression with no intervening function declaration.

The `return` expression exits the corresponding dynamic function invocation or completes the corresponding dynamic `async` or `async*` expression with the result of `<exp>`.

### Async

The async expression `<parenthetical>? async <block-or-exp>` has type `async T` provided:

- `<block-or-exp>` has type `T`.

- `T` is shared.

Any control-flow label in scope for `async <block-or-exp>` is not in scope for `<block-or-exp>`. However, `<block-or-exp>` may declare and use its own, local, labels.

The implicit return type in `<block-or-exp>` is `T`. That is, the return expression, `<exp0>`, implicit or explicit, to any enclosed `return <exp0>?` expression, must have type `T`.

Evaluation of `async <block-or-exp>` queues a message to evaluate `<block-or-exp>` in the nearest enclosing or top-level actor. It immediately returns a future of type `async T` that can be used to `await` the result of the pending evaluation of `<exp>`.

The presence of `<parenthetical>` modifies the semantics of the async expression to

- attach cycles with attribute `cycles : Nat`
- impose a timeout (observed when awaiting the result) with attribute `timeout : Nat32`.

:::note

Because it involves messaging, evaluating an `async` expression can fail due to a lack of canister resources.

Such failures will result in the call immediately throwing an error with  `code` `#call_error { err_code = n }`, where `n` is the non-zero `err_code` value returned by ICP.

Earlier version of Motoko would trap in such situations, making it difficult for the producer of the async expression to mitigate such failures. Now, the producer can handle these errors using an enclosing `try ... catch ...` expression, if desired.

:::

### Await

The `await` expression `await <exp>` has type `T` provided:

- `<exp>` has type `async T`.

- `T` is shared.

- The `await` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `await <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is a future. If the `future` is incomplete, that is, its evaluation is still pending, `await <exp>` suspends evaluation of the neared enclosing `async` or `shared`-function, adding the suspension to the wait-queue of the `future`. Execution of the suspension is resumed once the future is completed, if ever. If the future is complete with value `v`, then `await <exp>` suspends evaluation and schedules resumption of execution with value `v`. If the future is complete with thrown error value `e`, then `await <exp>` suspends evaluation and schedules resumption of execution by re-throwing the error `e`.

Suspending computation on `await`, regardless of the dynamic status of the future, ensures that all tentative state changes and message sends prior to the `await` are committed and irrevocable.

:::danger

Between suspension and resumption of a computation, the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes.

Using `await` signals that the computation will commit its current state and suspend execution.

:::

:::note

Because it involves additional messaging, an `await` on a completed future can, in rare circumstances, fail due to a lack of canister resources.
Such failures will result in the call immediately throwing an error with `code` `#call_error { err_code = n }`, where `n` is the non-zero `err_code` value returned by ICP.

The error is produced eagerly, without suspending nor committing state.
Earlier versions of Motoko would trap in such situations, making it difficult for the consumer of the `await` to mitigate such failures. Now, the consumer can handle these errors by using an enclosing `try ... catch ...` expression, if desired.

:::

### Async*

The async expression `async* <block-or-exp>` has type `async* T` provided:

- `<block-or-exp>` has type `T`.

- `T` is shared.

Any control-flow label in scope for `async* <block-or-exp>` is not in scope for `<block-or-exp>`. However, `<block-or-exp>` may declare and use its own, local, labels.

The implicit return type in `<block-or-exp>` is `T`. That is, the return expression, `<exp0>`, implicit or explicit, to any enclosed `return <exp0>?` expression, must have type `T`.

Evaluation of `async* <block-or-exp>` produces a delayed computation to evaluate `<block-or-exp>`. It immediately returns a value of type `async* T`.
The delayed computation can be executed using `await*`, producing one evaluation of the computation `<block-or-exp>`.

:::danger

Note that `async <block-or-exp>` has the effect of scheduling a single asynchronous computation of `<exp>`, regardless of whether its result, a future, is consumed with an `await`.
Moreover, each additional consumption by an `await` just returns the previous result, without repeating the computation.

In comparison, `async* <block-or_exp>`, has no effect until its value is consumed by an `await*`.
Moreover, each additional consumption by an `await*` will trigger a new evaluation of `<block-or-exp>`, including repeated effects.

Be careful of this distinction, and other differences, when refactoring code.

:::

:::note

The `async*` and corresponding `await*` constructs are useful for efficiently abstracting asynchronous code into re-useable functions.
In comparison, calling a local function that returns a proper `async` type requires committing state and suspending execution with each `await` of its result, which can be undesirable.

:::

### Await*

The `await*` expression `await* <exp>` has type `T` provided:

- `<exp>` has type `async* T`.

- `T` is shared.

- the `await*` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `await* <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is a delayed computation `<block-or-exp>`. The evaluation of `await* <exp>` proceeds with the evaluation of `<block-or-exp>`, executing the delayed computation.

:::danger

During the evaluation of `<block-or-exp>`, the state of the enclosing actor may change due to concurrent processing of other incoming actor messages. It is the programmer’s responsibility to guard against non-synchronized state changes.

:::

:::note

Unlike `await`, which, regardless of the dynamic status of the future, ensures that all tentative state changes and message sends prior to the `await` are committed and irrevocable, `await*` does not, in itself, commit any state changes, nor does it suspend computation.
Instead, evaluation proceeds immediately according to `<block-or-exp>`, the value of `<exp>`, committing state and suspending execution whenever `<block-or-exp>` does, but not otherwise.

:::

:::note

Evaluation of a delayed `async*` block is synchronous while possible, switching to asynchronous when necessary due to a proper `await`.

Using `await*` signals that the computation *may* commit state and suspend execution during the evaluation of `<block-or-exp>`, that is, that evaluation of `<block-or-exp>` may perform zero or more proper `await`s and may be interleaved with the execution of other, concurrent messages.

:::

### Throw

The `throw` expression `throw <exp>` has type `None` provided:

- `<exp>` has type [`Error`](../base/Error.md).

- The `throw` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `throw <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap`, evaluation returns `trap`. Otherwise `r` is an error value `e`. Execution proceeds from the `catch` clause of the nearest enclosing `try <block-or-exp1> catch <pat> <block-or-exp2>` whose pattern `<pat>` matches value `e`. If there is no such `try` expression, `e` is stored as the erroneous result of the `async` value of the nearest enclosing `async`, `async*` expression or `shared` function invocation.

### Try

The `try` expression `try <block-or-exp1> catch <pat> <block-or-exp2>` has type `T` provided:

- `<block-or-exp1>` has type `T`.

- `<pat>` has type [`Error`](../base/Error.md) and `<block-or-exp2>` has type `T` in the context extended with `<pat>`.

- The `try` is explicitly enclosed by an `async`-expression or appears in the body of a `shared` function.

Expression `try <block-or-exp1> catch <pat> <block-or-exp2>` evaluates `<block-or-exp1>` to a result `r`. If evaluation of `<block-or-exp1>` throws an uncaught error value `e`, the result of the `try` is the result of evaluating `<block-or-exp2>` under the bindings determined by the match of `e` against `pat`.

:::note

Because the [`Error`](../base/Error.md) type is opaque, the pattern match cannot fail. Typing ensures that `<pat>` is an irrefutable wildcard or identifier pattern.

:::

The `try` expression can be provided with a `finally` cleanup clause to facilitate structured rollback of temporary state changes (e.g. to release a lock).
The preceding `catch` clause may be omitted in the presence of a `finally` clause.

This form is `try <block-or-exp1> (catch <pat> <block-or-exp2>)? finally <block-or-exp3>`, and evaluation proceeds as above with the crucial addition that every control-flow path leaving `<block-or-exp1>` or `<block-or-exp2>` will execute the unit-valued `<block-or-exp3>` before the entire `try` expression produces its result. The cleanup expression will additionally also be executed when the processing after an intervening `await` (directly, or indirectly as `await*`) traps.

:::danger

The code within a `finally` block should terminate promptly and not trap.
A trapping finally block will fail to free its callback table slot which
can prevent a future upgrade.
In this situation, the canister should be explicitly stopped before re-attempting the upgrade.
In addition, care should be taken to release any resources that may have remained acquired due to the trap.
The canister may be re-started after the upgrade.

:::

See [Error type](./type-syntax-reference.md#error-type)

### Assert

The assert expression `assert <exp>` has type `()` provided `<exp>` has type [`Bool`](../base/Bool.md).

Expression `assert <exp>` evaluates `<exp>` to a result `r`. If `r` is `trap` evaluation returns `trap`. Otherwise `r` is a Boolean value `v`. The result of `assert <exp>` is:

- The value `()`, when `v` is `true`.

- `trap`, when `v` is `false`.

### Type annotation

The type annotation expression `<exp> : <typ>` has type `T` provided:

- `<typ>` is `T`.

- `<exp>` has type `U` where `U <: T`.

Type annotation may be used to aid the type-checker when it cannot otherwise determine the type of `<exp>` or when one wants to constrain the inferred type, `U` of `<exp>` to a less-informative super-type `T` provided `U <: T`.

The result of evaluating `<exp> : <typ>` is the result of evaluating `<exp>`.

:::note

Type annotations have no-runtime cost and cannot be used to perform the checked or unchecked `down-casts` available in other object-oriented languages.

:::

### Declaration

The declaration expression `<dec>` has type `T` provided the declaration `<dec>` has type `T`.

Evaluating the expression `<dec>` proceeds by evaluating `<dec>`, returning the result of `<dec>` but discarding the bindings introduced by `<dec>`, if any.

The expression `<dec>` is actually shorthand for the block expression `do { <dec> }`.

### Ignore

The expression `ignore <exp>` has type `()` provided the expression `<exp>` has type `Any` .

The expression `ignore <exp>` evaluates `<exp>`, typically for some side-effect, but discards its value.

The `ignore` declaration is useful for evaluating an expression within a sequence of declarations when that expression has non-`unit` type, and the simpler `<exp>` declaration would be ill-typed. Then the semantics is equivalent to `let _ = <exp> : Any`.

### Debug

The debug expression `debug <block-or-exp>` has type `()` provided the expression `<block-or-exp>` has type `()`.

When the program is compiled or interpreted with (default) flag `--debug`, evaluating the expression `debug <exp>` proceeds by evaluating `<block-or-exp>`, returning the result of `<block-or-exp>`.

When the program is compiled or interpreted with flag `--release`, evaluating the expression `debug <exp>` immediately returns the unit value `()`. The code for `<block-or-exp>` is never executed, nor is its code included in the compiled binary.

### Actor references

The actor reference `actor <exp>` has expected type `T` provided:

- The expression is used in a context expecting an expression of type `T`, typically as the subject of a type annotation, typed declaration or function argument.

- `T` is an some actor type `actor { …​ }`.

- `<exp>` has type [`Text`](../base/Text.md).

The argument `<exp>` must be, or evaluate to, the textual format of a canister identifier, specified elsewhere, otherwise the expression traps. The result of the expression is an actor value representing that canister.

The validity of the canister identifier and its asserted type `T` are promises and taken on trust.

An invalid canister identifier or type may manifest itself, if at all, as a later dynamic failure when calling a function on the actor’s proclaimed interface, which will either fail or be rejected.

:::note

The argument to `actor` should not include the `ic:` resource locator used to specify an `import`. For example, use `actor "lg264-qjkae"`, not `actor "ic:lg264-qjkae"`.

:::

:::danger

Although they do not compromise type safety, actor references can easily introduce latent, dynamic errors. Accordingly, actor references should be used sparingly and only when needed.

:::

### Subsumption

Whenever `<exp>` has type `T` and `T <: U`, with `T` subtypes `U`, then by virtue of implicit subsumption, `<exp>` also has type `U` without extra syntax.

In general, this means that an expression of a more specific type may appear wherever an expression of a more general type is expected, provided the specific and general types are related by subtyping. This static change of type has no runtime cost.
