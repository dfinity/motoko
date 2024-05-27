_Motoko-san_
============

> Disclaimer — this is an _early prototype_; in particular:
> * This project comes with no guarantees whatsoever.
> * The [currently supported subset of Motoko](#Subset) is insufficient for most real-world applications.
> *  DFINITY currently has no plans to continue the development of _Motoko-san_.


_Motoko-san_ is a prototype code-level verifier for Motoko. The project started at the DFINITY Foundation as a way to demonstrate that Motoko (and the Internet Computer in general) are well-suited for developing formally verified Web3 software.

--------------------------------------------------------------------------------
**Jump to:**

**[Introduction](#Introduction) —**
  [Overview](#Overview)
  | [Formal specification](#FormalSpecs)
  | [Static vs. dynamic assertions](#statVsDyn)
  | [Testing vs. formal verification](#testsVsVerif)
  | [Precondition of public functions](#publicFuncPreconds)
  | [Examples](#Examples)

**[Contributing](#Contrib) —**
    [Building](#Building)
  | [Running](#Running)
  | [Testing](#Testing)
  | [File structure](#Struct)

**[Currently supported features](#Subset)**

**[Further information](#Further)**

--------------------------------------------------------------------------------


Introduction
------------
<a name="Intro"></a>

**Overview**
<a name="Overview"></a>

The verifier is implemented as a `moc` compiler feature. When `moc --viper $FILE` is invoked, the Motoko source code from `$FILE` is first translated into the Viper intermediate verification language (currently emitted into stdout) and then compiled. The generated Viper program can be submitted to the Viper tool. If the generated Viper program verifies, this implies that the Motoko source code respects the _formal code specification_, e.g., canister invariants, and that Viper established the corresponding formal proof of correctness.

**Formal code specification**
<a name="FormalSpecs"></a>

Formal code specifications are written as part of the Motoko source code. These specifications are _static_, i.e., they do not affect the canister's runtime behavior. If a Motoko canister is compiled with `moc --viper`, it may contain static code specifications defined via the following keywords (`exp : Type` below denotes an expression of type `Type`):

* `assert:invariant (exp : Bool);` — actor-level invariant; must be ensured right after canister initialization and after every _atomic block_ execution. May appear only at the level of actor members. 
* `assert:1:async (exp : Bool);` — an `await async { ... }` block may include this as the first statement; the intention is two-fold:
  * at most _one_ instance of this code block can be pending execution.
  * specify that the property `exp` is _intended_ to hold when this block execution begins. 
  * require that the tool actually _checks_ whether this assumption holds (given this actor's entire source code)
* `assert:system (exp : Bool);` — a _static assertion_ that asks the verifier to prove that the property `exp` holds. Useful while designing code-level canister specifications.
* `assert:loop:invariant (exp : Bool);` — a _loop invariant_ specifies that the property `exp` is intended to hold at the start and at the end of each iteration of a loop

Note: the above syntax is provisional. It has been used so far to avoid introducing breaking changes to the Motoko grammar. In the future, _Motoko-san_ may switch to bespoke syntax for code specifications.

**Static vs. dynamic assertions**
<a name="statVsDyn"></a>

The expression `assert <exp>` (which is already available in Motoko proper) means a _dynamic assertion_, i.e., a runtime check of the Boolean condition `<exp>`. Note, however, that adding an `assert <exp>` expression (for some Boolean expression `<exp>`) affects the static verification of the canister. Concretely, the verifier will _take for granted_ that `<exp>` holds after this statement (since any execution violating this assumption would trap).

**Testing vs. formal verification**
<a name="testsVsVerif"></a>

Dynamic assertions can also be used for testing. Of course, the downside of testing is that if is typically not feasible to test against _all_ possible scenarios; the untested scenarios can be exploited by an attacker. In contrast, formal verification relies on statically known information to _prove the absence of attacks_.

**Precondition of public functions**
<a name="publicFuncPreconds"></a>

In particular, dynamic assertions are very useful for specifying _preconditions_ of an actor's public functions (i.e., functions exposed in the Candid API configuration). Since such functions can be invoked by anyone; the identity of the caller is statically unknown. It is thus necessary to check all of the assumptions at runtime, by writing `assert <exp>`(here, `<exp>` denotes some `Bool` expression representing a function's precondition). Conversely, writing `assert:system <exp>` at the top of a public function will never verify because _Motoko-san_ has zero knowledge about the caller.

**Examples**
<a name="Examples"></a>

To get a better idea about how code-level specifications help formalize what a Motoko canister is intended to do, please refer to the examples in `moc/test/viper`.

Contributing to _Motoko-san_
----------------------------
<a name="Contrib"></a>

**Building the Motoko compiler**
<a name="Building"></a>

1. Clone https://github.com/dfinity/motoko
    ```bash
    cd motoko
    ```
4. Install Nix:
    ```bash
    curl -L https://nixos.org/nix/install | sh
    ```
5. Obtain Nix cache (this speeds up the following steps):
    ```bash
    nix-env -iA cachix -f https://cachix.org/api/v1/install
    cachix use ic-hs-test
    ```
6. Enter Nix shell (the first run of this command may take a while):
   ```bash
   nix-shell
   ```
7. Finally, build the Motoko compiler runtime and the compiler itself:
    ```bash
    [nix-shell:motoko]$ make -C rts
    [nix-shell:motoko]$ make -C src
    ```


**Running _Motoko-san_**
<a name="Running"></a>

```bash
[nix-shell:motoko] moc --viper input.mo > output.vpr
```

You may then verify the `output.vpr` file using [Viper](https://viper.ethz.ch/). Soon, there will be an interactive IDE integration for VS Code, s.t. the outputs do not need to be verified by manually invoking Viper.

**Testing _Motoko-san_**
<a name="Testing"></a>

After modifying the code and recompiling `moc`, don't forget to test the changes by running 
```bash
[nix-shell:motoko]$ make -C test/viper
```

Each test case consists of a (formally specified) Motoko source file, say, `$TEST` (e.g., `invariant.mo`) and the expected test results, represented via a set of files:
* `test/viper/ok/$TEST.vpr.ok` — what the Motoko compiler is expected to generate; this should be a Viper program.
* `test/viper/ok/$TEST.vpr.stderr.ok` — diagnostic messages (warnings) emitted by the Motoko compiler. No file if stderr is empty.
* `test/viper/ok/$TEST.silicon.ok` — verification errors reported by the Viper tool. For example:
    ```
    [0] Postcondition of __init__ might not hold. Assertion $Self.count > 0 might not hold. (invariant.vpr@7.13--7.24)
    ```
    Note: Silicon is the name of one of the backends supported by Viper.
* `test/viper/ok/$TEST.silicon.ret.ok` — the return code from running Viper on this input. For example:
    ```
    Return code 1
    ```

**File structure**
<a name="Struct"></a>

The implementation of _Motoko-san_ consists of the following source files:

* `src/viper/syntax.ml` — the Viper AST implementation.
* `src/viper/pretty.ml` — the Viper pretty printer. Used for serializing Viper AST into text.
* `src/viper/trans.ml` — the Motoko-to-Viper translation. Implements the logic of _Motoko-san_.
* `src/viper/prep.ml` —  the Motoko-to-Motoko pass that prepares the unit for translation.


Currently supported language features
-------------------------------------
<a name="Subset"></a>

_Motoko-san_ is an early prototype. While the tool supports only a modest subset of [_Motoko proper_](https://internetcomputer.org/docs/current/motoko/getting-started/motoko-introduction), we hope that _Motoko-san_ will enable the community to build more sophisticated Motoko code-level verifiers, simply by extending this prototype. In particular, the tool enables verifying _reentrancy safety_ in simple (asynchronous) smart contracts (e.g., `test/viper/claim.mo`).

Below, we summarize the language features that _Motoko-san_ currently supports. For each feature, we try to estimate the complexity of its natural generalization. For that purpose, we use the terms _trivial_ (e.g., extending code by analogy), _simple_ (we already know how to do it), _hard_ (more discussions would be needed to figure out the exact approach or feasible level of generality).

* **Literal actor declarations** — The only supported top-level entity is an actor literal:

    `actor ClaimReward { ... }` and `actor { ... }`
  
    Extending to actor classes and modules is _simple_.

* **Types and operations**

    * Type `Bool`
        * Literals: `true`, `false`
        * Operations: `not`, `or`, `and`, `implies` (short circuiting semantics)

    * Type `Int`
        * Literals: `0`, `23`, `-5`, etc
        * Operations: `+`, `/`, `*`, `-`, `%`

    * Type `Nat`
        * Supported via translation to `Int` (sound because `Nat` is a
          subtype of `Int`)

    * Relations on `Bool`, `Int`, `Nat`
        * `==`, `!=`, `<`, `>`, `>=`, `<=`

    * Array types `[T]` and `[var T]`, where `T` ∈ {`Bool`, `Int`, `Nat`}
        * Construction: `[]`, `[x]`, `[x, y, z]`, `[var x, y, z]`, etc
        * Indexing: `a[i]`, can be used on the LHS of an assignment
        * Operations: `a.size()`

    * Tuple types `(T₁, T₂)`, `(T₁, T₂, T₃)`, etc, where `T` ∈ {`Bool`, `Int`, `Nat`}
        * Construction: `(a, b)`, `(a, b, c)`, etc
        * Projections: `x.1`, `x.2`, etc
        * Deconstruction by pattern matching:

            * `let (a, b) = x`
            * `switch x { case (a, b) ... }`

          Nested matching is _not_ supported, i.e. subpatterns for tuple
          elements `a`, `b`, `c` in `(a, b, c)` must all be variable patterns.

    * Option types `Option<T>` (no restrictions on `T`)
        * Construction: `null`, `?a`
        * Deconstruction by pattern matching:

          ```motoko
          switch x {
              case null ...;
              case (?a) ...;
          }
          ```

          Nested matching is _not_ supported, i.e. the subpattern `a` in `?a`
          must be a variable pattern.

    * Variant types `{ #A; #B : T₁; #C : (T₂, T₃) }` (with limitations)
        * Construction: `#A`, `#B(x)`, `#C(x, y)`
        * Deconstruction by pattern matching:

          ```motoko
          switch x {
              case #A ...;
              case #B(x) ...;
              case #C(x, y) ...;
          }
          ```

          Nested matching is _not_ supported, i.e. the subpatterns `x`, `y` in
          `#B(x)` and `#C(x, y)` must all be variable patterns.

        * Generic and recursive variants are supported, e.g.

          ```motoko
          type List<T> = { #Nil; #Cons : (T, List<T>) }
          ```

        * Limitation 1: Nominal equality. In the Viper translation, equality
          of variant types is not _structural_ (as in normal Motoko programs)
          but _nominal_.
          * One consequence of this is that a variant type may not be used
            directly but must be bound to a name:

            ```motoko
            type N = { #A; #B }
            ```

            Instead of using `{ #A; #B }` in type signatures, one must always
            refer to this variant by its name `N`.
          * Furthermore, given two identical variant declarations, they are
            treated as distinct in the translation:

             ```motoko
             type N = { #A; #B }
             type M = { #A; #B }   // M ≠ N
             ```

          * Motoko's type checker has not been modified to account for
            nominal equality of variants, so type inference is not reliable:
            if a variant is bound to a variable in `let`, `var`, or `case`, the
            bound variable must be explicitly annotated with the named type of
            the variant.
        * Limitation 2: Tuple components. If the type associated with a
          constructor is a tuple, its components must be specified individually.
          That is:

          ```motoko
          type N = { #Con: (Int, Bool) }

          let x = #Con(10, true)   // supported

          let p = (10, true)
          let x = #Con(p)          // not supported
          ```

    Supporting `Text`, `Int32`, tuples, record, and variants is _simple_.

    Supporting `async` types is _hard_.

    Supporting `Float`, function types, co-inductive, mutually recursive, and sub-typing is _hard_.

    Supporting container types and generic types, e.g. `HashMap<K, V>`, is _hard_.

    Supporting other element types in arrays and tuples is _simple_.

* **Declarations**
  
    * **Actor fields**
        * Mutable: `var x = ...`
        * Immutable: `let y = ...`
        * Fields may _not_ be initialized via block expressions: `let z = { ... };`

    * **Functions** — Functions of multiple arguments are supported:

        ```motoko
        // public func, the result is async
        public func f(a: Int, b: Bool) : async [Bool] { ... };

        // private func, no async
        func g(a: Int, b: Bool) : [Bool] { ... };
        ```

        If a function result type is non-`()`, it must be returned using the
        `return` statement.

        Polymorphism is supported in private functions only:

        ```motoko
        func firstValue<T, U>(a : T, _b : U) : T { ... };
        ```

        In the Viper translation, a polymorphic function is treated as a
        template: each instantiation of its type parameters creates a new copy
        of the function definition, where the parameters have been instantiated
        to concrete types. The call sites are modified to use the monomorphised
        version. The instantiations are found by traversing the static call
        graph within the actor.

    * **Local declarations** — Local variables are declared with `var` or `let`:

        ```motoko
        var x = ...;
        let y = ...;
        ```

        Furthermore, `let`-declarations may pattern match on a tuple:

        ```motoko
        let (a, b) = ...;
        ```

* **Statements**

    * `()`-typed block statements and sequential composition:

        ```motoko
        {
            var x = 0 : Int;
            x := x + 1;
        }
        ```

        Supporting `let y = do { let y = 1 : Int; y + y };` is _simple_.

    * Runtime assertions: `assert i <= MAX;`

    * Assignments (to local variables, actor fields, and array elements):

      ```motoko
      x := x + 1;         // x   is a local variable
      fld := fld + 1;     // fld is an actor field
      a[i] := a[i] + 1;   // a   is an array
      ```

    * `if-[else]` statements

        Supporting pattern-matching is conceptually _simple_.

    * Return statements `return e;`

      Early exit from a function is also supported:

      ```motoko
      if (length == 0) {
        return a;   // conditional early exit
      };
      return a;     // normal exit
      ```

    * `while` loops and loop invariants

      ```motoko
      while (cond) {
          assert:loop:invariant (e1);
          assert:loop:invariant (e2);
          ...
      };
      ```

      The loop invariant assertions must precede all other statements in the
      loop body.

        Supporting `for` loops is _simple_.

        Supporting `break` and `continue` is _simple_.

    * Method calls `f(a,b,c);`

      ```motoko
      f1(e);           // standalone method call
      let c1 = f2(e);  // method call in let-declaration
      var c2 = f3(e);  // method call in var-declaration
      c2 := f4(e);     // method call in assignment
      return f5(e);    // method call in return statement
      ```

      Limitiation: at the moment, only one method call per statement is allowed. Nested calls `f(g(x), h(y)))` must be rewritten using temporary variables:

      ```motoko
      let a = g(x);
      let b = h(y);
      f(a, b);
      ```

    * `await async { ... }` — Asynchronous code blocks that are immediately awaited on.

        Supporting general `await`s and `async`s is _hard_.

        Supporting async function calls is _simple_.

* **Static code specifications** — Note that the syntax is provisional:

    * `assert:invariant` — Canister-level invariants
    
    * `assert:1:async` — Async constraints (at block entry)

        Extending to support, e.g., `assert:`_`N`_`:async` constraints (for _`N`_ > 1) is _simple_.

        Extending to support async constraints at block exit is _trivial_.
    
    * `assert:func` — Function preconditions
    
    * `assert:return` — Function postconditions. 
    
        * These may refer to variables in the _initial_ state of the function call using the syntax `(old <exp>)`, for example:

            ```motoko
            var x : Int;
            private func dec() : () {
                x -= 1;
                assert:return x < old(x);
            };
            ```

            is equivalent to

            ```motoko
            var x : Int;
            private func dec() : () {
                let old_x = x;
                x -= 1;
                assert:return x < old_x;
            };
            ```
        * To refer to the result value of a function in a postcondition, use the `var:return` syntax:

            ```motoko
            private func f(): [var Nat] {
                assert:return (var:return).size() == xarray.size();
                ...
            }
            ```
    
    * `assert:system` — Compile-time assertions
    
    * `assert:loop:invariant` — Loop invariants
    
    **Pure functions** — The tool could be easily extended with a keyword, e.g., `@pure`, to specify functions that are verifier to be side-effect free; such functions could be used inside other code specifications, e.g., `assert:invariant is_okay()` for some `@pure func is_okay() : Bool`.

Further information
-------------------
<a name="Further"></a>

If you have questions, please contact the Motoko compiler developers. You may do that, e.g., by filing a ticket via https://github.com/dfinity/motoko/issues/new
(please add `viper` into the labels).
