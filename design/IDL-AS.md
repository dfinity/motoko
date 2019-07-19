The IDL-ActorScript integration
===============================

## Goals

This document specifies the integration of the IDL with the ActorScript
language, in particular:

 * How to translate between an ActorScript `actor` type and an IDL `service`
   description in both ways (i.e. _exporting_ IDL from ActorScript, and _importing_
   IDL into ActorScript), and how to translate between the values of these types.

 * The supported work-flows, including a sketch of the involved tools.

We try to achieve the following goals (but do not achieve them completely)

 * We can round-trip all ActorScript values of sharable type. More precisely:

   When exporting an ActorScript type `ta` into an IDL type `ti`, then
   round-tripping a value `va : ta` through `ti` yields a value that is
   indistinguishable from `va` at type `ta`.

 * ActorScript can receive all IDL types: The type export has an inverse, the
   type import mapping, which is injective (up-to IDL type equivalence via
   shorthands).

 * ActorScript can receive all IDL values when importing an IDL:

   When importing an IDL type `ti` into `ta`, then every IDL value `vi : ti`
   will successfully translated to an ActorScript value of type `ta`.

The following are not necessary true:

 * The type export mapping is not total: there are ActorScript types that cannot
   be exported, such as mutable arrays.

 * The type export mapping is not injective: there may be different
   ActorScript types that map to the same IDL type, e.g. `Char`, `Word32` and
   `Nat32`.

   This implies that round-tripping an ActorScript type via the IDL can yield
   different types.

 * For some types, not all IDL values may be accepted: for example, `Char` is
   may be exported as `nat32`, but not all `nat32` values can be read as
   `Char`. This can only be the case for types not in the image of the type
   import mapping.

NB: These properties (and non-properties) are not specific to ActorScript, and
we expect that they will hold similarly for interfaces to other typed languages
with seamless serialization (e.g. built-in, macro or type class based
integration). In this sense, this document serves as a blueprint. Untyped
languages or languages with a code-generation workflow may have a different
story.

## The type mappings

We define
 * a partial function `e` from ActorScript types to IDL types.
   Types that are not in the domain of `e` cannot be exported.
 * a partial function `i` from IDL types to ActorScript types.
   Types that are not in the domain of `i` cannot be imported.

These definition treats ActorScript types and IDL types as structural and
infinite; a concrete implementation will have to look through type constructors
in ActorScript and introduce type definitions in the IDL as necessary.

It assumes that the IDL short-hands (e.g. named or anonymous fields)
are part of the grammar of types, and that `i` is allowed to make difference
choices for types that are short-hands.

The function is defined with regard to the grammars in [IDL.md](IDL.md) and [Syntax.md](Syntax.md).

### Type export

```
e : <typ> -> <datatype>
e(Null) = null
e(Bool) = bool
e(Nat) = nat
e(Int) = int
e(Nat<n>) = nat<n> for n = 8, 16, 32, 64
e(Int<n>) = int<n> for n = 8, 16, 32, 64
e(Word<n>) = nat<n> for n = 8, 16, 32, 64
e(Float) = float64
e(Char) = nat32
e(Text) = text
e(shared { <typ-field>;* }) = record { ef(<typ-field>);* }
e(variant { <typ-field>;* }) = variant { ef(<typ-field>);* }
e([<typ>]) = vec (e(<typ>))
e(? <typ>) = opt (e(<typ>))
e(shared <typ1> -> <typ2>) = func (efn(shared <typ1> -> <typ2>))
e(actor { <typ-field>;* }) = service { em(<typ-field>);* }
e( ( <typ>,* ) ) = record { e(<typ>);* }
e(Any) = reserved
e(None) = empty

ef : <typ-field> -> <fieldtype>
ef (<id> : <typ>) = unescape(<id>) : e(<typ>)

efn : <typ> -> <functype>
efn(shared <typ> -> ()) = ea(<typ>) -> () oneway
efn(shared <typ1> -> async <typ2>) = ea(<typ1>) -> ea(<typ2>)

ea : <typ> -> <argtype>,*
ea( ( <typ>,* ) ) = e(<typ>);*
ea(<typ>) = ( e(<typ>) )  otherwise

em : <typ-field> -> <methtype>
em(<id> : <typ>) = unescape(<id>) : efn(<typ>)

unescape : <id> -> <nat>|<name>
unescape("_" <nat> "_") = <nat>
unescape(<id> "_") = <id>
unescape(<id>) = <id>
```

### Type import

```
i : <datatype> -> <typ>
i(null) = Null
i(bool) = Bool
i(nat) = Nat
i(int) = Int
i(nat<n>) = Nat<n> for n = 8, 16, 32, 64
i(int<n>) = Int<n> for n = 8, 16, 32, 64
// i(float32) not defined
i(float64) = Float
i(text) = Text
i(reserved) = Any
i(opt <datatype>) = ? i(<datatype>)
i(vec <datatype>) = [ i(<datatype>) ]
i(blob) = [ word8 ] // if ActorScript had a bytes type, it would show up here
i(record { <datatype>;* }) = ( i(<datatype>),* ) // matches tuple short-hand
i(record { <fieldtype>;* }) = shared { if(<fieldtype>);* }
i(variant { <fieldtype>;* }) = variant { if(<typ-field>);* }
i(func <functype>) = ifn(<functype>)
i(service { <methtype>;* }) = actor { im(<methtype>);* }

if : <fieldtype> -> <typ>
if(<name> : <datatype>) = escape(<name>) : i(<datatype>)
if(<nat> : <datatype>) = "_" <nat> "_": i(<datatype>) // also for implicit labels

ifn : <functype> -> <typ>
ifn((<datatype>,*) -> () oneway pure?) = shared ia(<as>) -> ()
ifn((<datatype1>,*) -> (<datatype2>,*) pure?) = shared ia(<datatype1>,*) -> ia(<datatype2>,*)

ia : <argtype>,* -> <typ>
ia(<argtype>,) = i(<argtype>)
ia(<argtype>,*) = ( i(<argtype>),* )  otherwise

im : <methtype> -> <typ>
im(<name> : <functype>) = escape(<name>) : ifn(<functype>)

escape : <name> -> <id>
escape <name> = <name> "_"  if <name> is a reserved identifier in ActorScript
escape <name> = <name> "_"  if <name> ends with "_"
escape <name> = <name>  if <name> is a valid ActorScript <id> not ending in "_"
escape <name> = "_" hash(<name>) "_"  otherwise
```

### Notes:

 * Up-to short-hands, `i` is injective and the right-inverse of `e`.

   Formally: For all IDL types `t ∈ dom i`, we have that `e(i(t))` is equivalent to
   `t`, i.e. either they are the same types, or short-hands of each other.

 * Tuples are exported using the unnamed field short-hand, which is how tuples
   are idiomatically expressed in the IDL:
   ```
   e(()) = record {}
   e((Int, )) = record {int}
   e((Int, Nat)) = record {int;nat}
   e(shared {i:Int, n:Nat)) = record {i:int; n:nat}
   e(shared {_0_:Int, _1_:Nat)) = record {0:int; 1:nat}
   ```

 * The mapping `i` tries to detect types that can be expressed as tuples in
   ActorScript.
   ```
   i(record {int;nat}) = (Int, Nat)
   i(record {int; nat; foo:text}) = shared {_0_:Int; _1_:Nat; foo:Text}
   i(record {0:Int, 1:Nat)) = shared {_0_:int; _1_:nat}
   ```

 * The `escape` and `unescape` functions allow round-tripping of IDL field
   names that are not valid ActorScript names (fake hash values):
   ```
   i(record {int; if:text; foobar_:nat; "_0_":bool})
     = shared (_0_:Int; if_:Text; _1234_:Nat; _4321_:Bool)
   ```
   This is another source of partiality for `e`:
   ```
   e(shared {clash_ : Nat; clash : Int})
   ```
   is undefined, because `unescape(clash_) = unescape(clash)`.

 * ActorScript functions with type parameters are not in the domain of `e`.

 * Abstract ActorScript types are not in the domain of `e`

 * The translation produces IDL functions without parameter names.  But both
   the IDL and ActorScript conveniently support non-significant  names in
   parameter lists. These are essentially comments, and do not affect, for
   example, the type section in a message, so it is not necessary to specify
   them here.

   But tooling (e.g. `asc` exporting an IDL from an ActorScript type) is of
   course free to use any annotations in the ActorScript type (or even names
   from pattern in function definitions) also in the exported IDL.

 * The soundness of the ActorScript type system, when it comes to higher-order
   use of actor and function references, relies on
   ```
   ∀ t1 t2 : dom(e), t1 <: t2 ⟹ e(t1) <: e(t2)
   ```
   In other words: ActorScript subtyping must be contained in IDL subtyping.

 * There is no way to produce `float32` or functions with a `pure` annotation.
   Importing interfaces that contain these types fails.

## The value mappings

For each ActorScript type `t` in the domain of `e`, we need mapping from
ActorScript value of type `t` to an IDL value of type `e(t)`, and vice-versa.

Note that decoding may only fail for those `t` that are not in the range of `i`.

These mappings should be straight-forward, given the following clarifications:

* Characters (of type `Char`) are mapped to their Unicode scalar as a `nat32`.
  Decoding a `nat32` that is not a valid Unicode scalar fails.

## Works flows

The mapping specified here can be used to support the following use-cases. The
user interfaces (e.g. flag names, or whether `asc`, `idlc`, `dfx` is used) are
just suggestions.

* Generating IDL from ActorScript

  If `foo.as` is an ActorScript `actor` compilation unit, then

      asc --generate-idl foo.as -o foo.didl

  will type-check `foo.as` as `t = actor { … }`, map the ActorScript type `t`
  to an IDL type `e(t)` of the form `service <actortype>`, and produce a
  textual IDL file `foo.didl` that ends with a `service n : <actortype>`,
  where `n` is the name of the actor class, actor, or basename of the source
  file.

* Checking ActorScript against a given IDL

  If `foo.as` is an ActorScript `actor` compilation unit and `foo.didl` a
  textual IDL file, then

      asc --check-idl foo.didl foo.as -o foo.wasm

  will import the type service `t_spec` specified in `foo.didl`, using the
  mapping `i`, will generate an IDL type `e(t)` as in the previous point, and
  and check that `e(t) <: t_spec` (using IDL subtyping).

* Converting IDL types to ActorScript types

  If `foo.didl` a textual IDL file, then

      idlc foo.didl -o foo.as

  will create an ActorScript library unit `foo.as` that consists of type
  definitions.
  All `<def>`s and the final `<actor>` from `foo.didl` is turned into a `type`
  declaration in ActorScript, according to `i`.
  Imported IDL files are recursively inlined.

  Variant: Imported IDL files are translated separately and included via
  `import` in ActorScript.

* Importing IDL types from the ActorScript compiler

  If `path/to/foo.didl` a textual IDL file, then a declaration

      import Foo "path/to/foo"

  is treated by `asc` by reading `foo.didl` as if  the developer had
  run `idlc path/to/foo.didl -o path/to/foo.as`.
