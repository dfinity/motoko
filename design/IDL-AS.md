The IDL-ActorScript integration
===============================

## Goals

This document specifies the integration of the IDL with the ActorScript
language, in particular:

 * How to translate between an ActorScrpit `actor` type and an IDL `service`
   description in both ways (i.e. _exporting_ IDL from ActorScript, and _importing_
   IDL into ActorScript), and how to translate between the values of these types.

 * The supported work-flows, including a sketch of the involved tools.

The following should be true:

 * We can round-trip all ActorScript values. More precisely:

   When exporting an ActorScript type `ta` into an IDL type `ti`, then round-tripping
   a value `va : ta` through `ti` yields a value that is indistinguishable from
   `va` at type `ta`.

 * ActorScript can receive all IDL types: The type import function is total.

 * ActorScript can receive all IDL values when importing an IDL:

   When importing an IDL type `ti` into `ta`, then every IDL value `vi : ti`
   will successfully translated to an ActorScript value of type `ta`.


The following are not necessary true:

 * It is not necessary true that round-tripping an ActorScript _type_ (i.e.
   exporting as IDL and importing it again) preserves any relation. For
   example, round-tripping a record with particular field names may result in a
   tuple, or the other way around.

 * It is not necessary true that all ActorScript types can be exported (e.g.
   mutable arrays), nor that every IDL type is the export of some ActorScript
   type (e.g. `int8`).

 * When exporting an ActorScript type to an IDL type, not all IDL values may
   be accepted. (e.g. `Char` is exported as `text`, but not all `text` values
   can be read as `char`)

NB: These properties (and non-properties) are not specific to ActorScript, and
we expect that they will hold for every interface to a type language. In this
sense, this document serves as a blueprint.

## Exporting ActorScript types

We define a partial function `e` from ActorScript types to IDL types. Types
that are not in the domain of `e` cannot be exported.

This definition treats ActorScript types and IDL types as structural and
infinite; a concrete implementation will have to look through type constructors
in ActorScript and introduce type definitions in the IDL as necessary.

The function is defined with regard to the grammars in [IDL.md](IDL.md) and [Syntax.md](Syntax.md).

```
e : <typ> -> <datatype>
e(Null) = null
e(Bool) = bool
e(Nat) = nat
e(Int) = int
e(Word<n>) = nat<n> for n = 8, 16, 32, 64
e(Float) = float64
e(Char) = text
e(Text) = text
e(shared { <typ-field>^N }) = record { ef*(<typ-field>^N) }
e(variant { <typ-field>^N }) = variant { ef*(<typ-field>^N) }
e([<typ>]) = vec (e(<typ>))
e(? <typ>) = opt (e(<typ>))
e(shared <a:typ> -> async <r:typ>) = func (ea(<a:typ>) -> ea(<r:typ>))
e(shared <a:typ> -> ()) = func (ea(<a:typ>) -> ()) oneway
e(actor { <typ-field>^N }) = service { em*(<typ-field>^N) }
e( () ) = null
e( ( <typ>, ) ) = e(<typ>) // avoid exporting singleton tuples
e( ( <typ>^N ) ) = record { e*(<typ^N>) } if N > 1 // NB: unnamed field shorthand
e(None) = variant {}

ef : <typ-field> -> <fieldtype>
ef (<id> : <typ>) = <id> : e(<typ>)

em : <typ-field> -> <methtype>
em(<id> : shared <a:typ> -> async <r:typ>) = <id> : ea(<a:typ>) -> ea(<r:typ>)
em(<id> : shared <a:typ> -> ()) = <id> : ea(<a:typ>) -> () oneway

ea : <typ> -> <fieldtype>,* // function arguments
ea(shared { <type-field>^N }) = ef*(<typ-field>^N)
ea( () ) = . // NB: empty list of field types
ea( ( <typ>, ) ) = ea(<typ>)
ea( ( <typ>^N ) ) = e*(<typ^N>)  if N > 1
ea(<typ>) = e(<typ>)  if no earlier clause matches // NB: unnamed field shorthand
```

Notes:

 * Tuples are represented using the unnamed field short-hand; this allows
   consumers of the generated description to recognize tuples.
 * Singleton tuples are not exported to the IDL.
 * Functions with type parameters are not in the domain of this function.
 * Abstract types are not in the domain of this function.


For each ActorScript type `t`, the mapping of an ActorScript value of type `t`
to an IDL value of type `e(t)` should be straight-foward. Some notes:

 * Characters (of type `Char`) are mapped to singleton strings.

For each ActorScript type `t`, the mapping of an ActorScript value of type `t`
to an IDL value of type `e(t)` should be straight-foward. Some notes:

 * Decoding a `text` that is not a singleton as `Char` traps.

## Importing IDL types

We define a total function `i` from IDL types to ActorScript types

This definition treats ActorScript types and IDL types as structural and
infinite; a concrete implementation will have to look through imports, type
definitions in the IDL, and introduce type constructors in ActorScript as
necessary.

The function is defined with regard to the grammars in [IDL.md](IDL.md) and [Syntax.md](Syntax.md).

```
i : <datatype> -> <typ>
i(null) = Null
i(bool) = Bool
i(nat) = Nat
i(int) = Int
i(nat<n>) = Word<n> for n = 8, 16, 32, 64
i(int<n>) = Word<n> for n = 8, 16, 32, 64
i(float32) = Float
i(float64) = Float
i(text) = Text
i(unavailable) = Any
i(opt <datatype>) = ? i(<datatype>)
i(vec <datatype>) = [ i(<datatype>) ]
i(record { <datatype> }) = i(<datatype>)
i(record { <datatype>^N }) = ( i(<datatype>)^N ) if N > 1 // import as tuple
i(record { <fieldtype>^N }) = shared { if*(<fieldtype>^N) }
i(variant { <fieldtype>^N }) = variant { if*(<typ-field>^N) }
i(func <functype>) = ift<functype>
i(service { <methtype>^N }) = actor { im*(<methtype>^N) }

if : <fieldtype> -> <typ>
if(<name> : <datatype>) = escape(<name>) : i(<datatype>)
if(<nat> : <datatype>) = "field_" <nat> "_": i(<datatype>)

ift : <functype> -> <typ>
ift(a:(<fieldtype>^N) -> () oneway pure?) = shared ia(a) -> ()
ift(a:(<fieldtype>^N) -> r:<fieldtype>^N pure?) = shared ia(a) -> ia(r)

ia : <fieldtype>^N -> <typ>
ia() = ()
ia(<datatype>) = i(<datatype>)
ia(<datatype>^N) = ( i*(<datatype>^N) ) // import as tuple
ia(<fieldtype>^N) = shared { if*(<fieldtype>^N) }

escape : <name> -> <id>
escape <name> = <name> "_"  if <name> is a reserved identifer in ActorScript
escape <name> = <name> "_"  if <name> ends with "_"
escape <name> = "field_" hash(<name>) "_"  if <name> is not a valid ActorScript <id>
escape <name> = <name>   otherwise
```

Notes:

 * This mapping assumes that the tuple shorthand (i.e. record with unnamed fields)
   is visible in the input grammar, and imports these as tuples.

   One can also read the lines marked with `import as tuple` to match precisely those
   records with field ids `0,1,…`.

 * This mapping assumes that the string field short-hand is visible in the input
   grammar, and tries to use the field name.

   A simple encoding is used for reserved names. Since IDL field labels can be
   arbitrary strings, if they contain invalid characters, we fall back to encoding
   the actual IDL number.

 * Singleton tuples are not imported.


For each IDL type `t`, the mapping of an IS value of type `t` to an ActorScript
value of type `i(t)` should be straight-foward. Some notes:

 * Negative values in `int<n>` are mapped to their two's complement
   representation in `Word<n>`.


For each IDL type `t`, the mapping of an ActorScript value of type `i(t)` to an
IDL value of type `t` should be straight-foward. Some notes:

 * When converting a `Float` into a `float32`, the value is rounded.

## Worksflows

The mapping specified here can be used to support the following use-cases. The
user interfaces (e.g. flag names, or whether `asc`, `idlc`, `dfx` is used) are
just suggestions.

* Generating IDL from ActorScript

  If `foo.as` is an ActorScript `actor` compilation unit, then

      asc --generate-idl foo.as -o foo.didl

  will type-check `foo.as` as `t = actor { … }` and produce a textual IDL file
  `foo.didl` that ends with a `service n : ti` where `n` is the name of the
  actor class, actor, or basename of the source file, and `service ti = (t)`.

* Checking ActorScript against a given IDL

  If `foo.as` is an ActorScript `actor` compilation unit and `foo.didl` a
  textual IDL file, then

      asc --check-idl foo.didl foo.as -o foo.wasm

  will import the type service specified in `foo.didl`, using the mapping `i`,
  as ActorScript type `t1`; type-check `foo.as` as `t2 = actor { … }` and check
  that `t1 <: t1` (as ActorScript types).

* Converting IDL types to Actorscript types

  If `foo.didl` a textual IDL file, then

      idlc foo.didl -o foo.as

  will create an ActorScript library unit `foo.as` that consists of type
  definitions.
  All `<def>`s and the final `<actor>` from `foo.didl` is turned into a `type`
  declaration in ActorScript, according to `i`.
  Imported IDL files are recursively inlined.

  Variant: Imported IDL files are translated separately and included via
  `import` in ActorScript.

* Importing IDL types from the ActorScript compilero

  If `path/to/foo.didl` a textual IDL file, then a declaration

      import Foo "path/to/foo"

  is treated by `asc` by reading `foo.didl` as if  the developer had
  run `idcc path/to/foo.didl -o path/to/foo.as`.

## Future extensions

* Besides exporting and importing ActorScript types, it might also be useful to
  define a compatibility _relation_ between them. This is useful if the
  developer has a specific IDL interface to provide, but want to use
  _different_ ActorScript types than the import mapping would give him.

  In this workflow, the developer would specify both the IDL description _and_
  the ActorScript types, the compiler would check that they are compatible, and
  generate the appropriate serialization/deserialization code.

  Importing/exporting IDL would become a special case of this workflow, where
  the developer may omit one or the other.


