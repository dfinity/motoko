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

Each test case consists of a (formally specified) Motoko source file, say, `$TEST` (e.g., `invariant.mo`) and the expected test results, represented via a triplet of files:
* `test/viper/ok/$TEST.vpr.ok` — what the Motoko compiler is expected to generate; this should be a Viper program.
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


Currently supported language features
-------------------------------------
<a name="Subset"></a>

_Motoko-san_ is an early prototype. While the tool supports only a modest subset of [_Motoko proper_](https://internetcomputer.org/docs/current/motoko/main/about-this-guide), we hope that _Motoko-san_ will enable the community to build more sophisticated Motoko code-level verifiers, simply by extending this prototype. In particular, the tool enables verifying _reentrancy safety_ in simple (asynchronous) smart contracts (e.g., `test/viper/claim.mo`).

Below, we summarize the language features that _Motoko-san_ currently supports. For each feature, we try to estimate the complexity of its natural generalization. For that purpose, we use the terms _trivial_ (e.g., extending code by analogy), _simple_ (we already know how to do it), _hard_ (more discussions would be needed to figure out the exact approach or feasible level of generality).

* **Literal actor declarations** — The only supported top-level entity is an actor literal:

    `actor ClaimReward { ... }` and `actor { ... }`
  
    Extending to actor classes and modules is _simple_.

* **Primitive types** — Only integer and Boolean types are supported (including literals of these types):
  
  * `Bool`: `not`, `or`, `and`, `implies` (short circuiting semantics)

  * `Int`: `+`, `/`, `*`, `-`, `%`

  * Relations: `==`, `!=`, `<`, `>`, `>=`, `<=`

  Supporting `Text`, `Nat`, `Int32`, tuples, record, and variants is _simple_.

    Supporting `Option<T>` types is _trivial_.

    Supporting `async` types is _hard_.

    Supporting `Float`, function types, co-inductive, mutually recursive, and sub-typing is _hard_.

    Supporting container types and generic types, e.g., arrays (`[var T]`) and `HashMap<K, V>`, is _hard_.

* **Declarations**
  
    * **Actor fields**
        * Mutable: `var x = ...`
        * Immutable: `let y = ...`
        * Fields may _not_ be initialized via block expressions: `let z = { ... };`

    * **Functions** — Only functions of `()` and `async ()` type with no arguments are supported:

        `public shared func claim() : async () = { ... };`

        `private func reward() : () = { ... };`

        Supporting function arguments and return values is _simple_.

    * **Local declarations** — Only local variable declarations with trivial left-hand side are supported:

        `var x = ...;` and `let y = ...;`

        Supporting pattern matching declarations (e.g., `let (a, b) = ...;`) is _simple_.

* **Statements**

    * `()`-typed block statements and sequential composition:

        `{ var x = 0 : Int; x := x + 1; }`

        Supporting `let y = do { let y = 1 : Int; y + y };` is _simple_.

    * Runtime assertions: `assert i <= MAX;`

    * Assignments (to local variables and actor fields): `x := x + 1`
    
    * `if-[else]` statements
  
        Supporting pattern-matching is conceptually _simple_.
    
    * `while` loops (loop invariants are not currently supported)

        Supporting `for` loops is _simple_.

        Supporting `break` and `continue` is _simple_.
    
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
    
    * `assert:system` — Compile-time assertions
    
    **Loop invariants** — Extension is _simple_.
    
    **Pure functions** — The tool could be easily extended with a keyword, e.g., `@pure`, to specify functions that are verifier to be side-effect free; such functions could be used inside other code specifications, e.g., `assert:invariant is_okay()` for some `@pure func is_okay() : Bool`.

Further information
-------------------
<a name="Further"></a>

If you have questions, please contact the Motoko compiler developers. You may do that, e.g., by filing a ticket via https://github.com/dfinity/motoko/issues/new
(please add `viper` into the labels).
