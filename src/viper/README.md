# Formal Motoko

**Disclaimer** — this is an _early prototype_; in particular: 
1. This project comes with no guarantees whatsoever. 
2. The currently supported subset of Motoko is insufficient for most real-world applications.
3. DFINITY currently has no plans to continue the development of Formal Motoko.

Formal Motoko is a prototype code-level verifier for Motoko. The project started at the DFINITY Foundation as a way to demonstrate that Motoko (and the Internet Computer in general) are well-suited for developing formally verified Web3 software.

---
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
  | [Further information](#Further)

---

## Introduction
<a name="Intro"></a>

**Overview**
<a name="Overview"></a>

The verifier is implemented as a `moc` compiler feature. When `moc --viper $FILE` is invoked, the Motoko source code from `$FILE` is first translated into the Viper intermediate verification language (currently emitted into stdout) and then compiled. The generated Viper program can be submitted to the Viper tool. If the generated Viper program verifies, this implies that the Motoko source code respects the _formal code specification_, e.g., canister invariants, and that Viper established the corresponding formal proof of correctness.

**Formal code specification**
<a name="FormalSpecs"></a>

Formal code specifications are written as part of the Motoko source code. These specifications are _static_, i.e., they do not affect the canister's runtime behavior. If a Motoko canister is compiled with `moc --viper`, it may contain static code specifications defined via the following keywords (`exp : Type` below denotes an expression of type `Type`):

* `assert:invariant (exp : Bool);` — actor-level invariant; must be ensured right after canister initialization and after every _atomic block_ execution. May appear only at the level of actor members. 
* `assert:1:async (exp : Bool);` — an `await async { ... }` block may include this as the first statement; the intention is two-fold:
  * specify that the property `exp` is _intended_ to hold when this block execution begins. 
  * require that the tool actually _checks_ whether this assumption holds (given this actor's entire source code)
* `assert:system (exp : Bool);` — a _static assertion_ that asks the verifier to prove that the property `exp` holds. Useful while designing code-level canister specifications.

Note: the above syntax is provisional. It has been used so far to avoid introducing breaking changes to the Motoko grammar. In the future, Formal Motoko may switch to bespoke syntax for code specifications.

**Static vs. dynamic assertions**
<a name="statVsDyn"></a>

The expression `assert <exp>` (which is already available in Motoko proper) means a _dynamic assertion_, i.e., a runtime check of the Boolean condition `<exp>`. Note, however, that adding an `assert <exp>` expression (for some Boolean expression `<exp>`) affects the static verification of the canister. Concretely, the verifier will _take for granted_ that `<exp>` holds after this statement (since any execution violating this assumption would trap).

**Testing vs. formal verification**
<a name="testsVsVerif"></a>

Dynamic assertions can also be used for testing. Of course, the downside of testing is that if is typically not feasible to test against _all_ possible scenarios; the untested scenarios can be exploited by an attacker. In contrast, formal verification relies on statically known information to _prove the absence of attacks_.

**Precondition of public functions**
<a name="publicFuncPreconds"></a>

In particular, dynamic assertions are very useful for specifying _preconditions_ of an actor's public functions (i.e., functions exposed in the Candid API configuration). Since such functions can be invoked by anyone; the identity of the caller is statically unknown. It is thus necessary to check all of the assumptions at runtime, by writing `assert <exp>`(here, `<exp>` denotes some `Bool` expression representing a function's precondition). Conversely, writing `assert:system <exp>` at the top of a public function will never verify because Formal Motoko has zero knowledge about the caller.

**Examples**
<a name="Examples"></a>

To get a better idea about how code-level specifications help formalize what a Motoko canister is intended to do, please refer to the examples in `moc/test/viper`.

## Contributing to Formal Motoko
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


**Running Formal Motoko**
<a name="Running"></a>

```bash
[nix-shell:motoko] moc -viper input.mo > output.vpr
```

You may then verify the `output.vpr` file using [Viper](https://viper.ethz.ch/). Soon, there will be an interactive IDE integration for VS Code, s.t. the outputs do not need to be verified by manually invoking Viper.

**Testing Formal Motoko**
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

The implementation of Formal Motoko consists of the following source files:

* `src/viper/syntax.ml` — the Viper AST implementation.
* `src/viper/pretty.ml` — the Viper pretty printer. Used for serializing Viper AST into text.
* `src/viper/trans.ml` — the Motoko-to-Viper translation. Implements the logic of Formal Motoko.

**Further information**
<a name="Further"></a>

If you have questions, please contact the Motoko compiler developers. You may do that, e.g., by filing a ticket via https://github.com/dfinity/motoko/issues/new
(please add `viper` into the labels).