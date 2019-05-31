Temporary Wire Format
=====================

This document describes the serializaion format currently used by the
ActorScript runtime, i.e. a mapping from ActorScript types to DFINITY types (=
WebAssembly types + `databuf`, `elmembuf`, `funcref` and `actorref`), and a
mapping between the corresponding values.

This is a scaffolding tool to prototype applications until we have decided upon
the actual IDL of the system, which will change all that is described here.

It also does not support all features that we want to support eventually. In
particular, it does not support subtyping.

Some types have a *specialized argument format* when used directly as a
function arguments, rather than nested inside a data structure. Other types use
the _general argument format (without references)_ or the _general argument
format (with references)_.

Each argument of a function is serialized separately. If the function is
defined with a list of arguments, these all become arguments of the WebAssembly
function. See the [ActorScript guide](https://hydra.oregon.dfinity.build//job/dfinity-ci-build/actorscript.pr-252/users-guide/latest/download/1/guide/#function-types) for the precise rules for function arities.


Specialized argument format: `Text`
-------------------------------------

A message entry point with an argument of type `Text` is represented as a `databuf` that contains the UTF8-encoded string.

Note that there is no terminating `\0`, and the length is implicit as the
length of the `databuf`.

Specialized argument format: `actor {…}` and `shared … -> …`
------------------------------------------------------------

A message entry point with an argument of actor type or of shared function type is represented as an `actorref` resp. `funcref.`

General argument format (without references)
--------------------------------------------

Arguments with a type that does not mention any reference types (no actors, no
shared functions), are represented as a `databuf`. This `databuf` is generated
by an in-order traversal of the data type. All numbers are in little endian
format.

 * A `Nat` is represented by its shortest [LEB128 encoding]
 * An `Int` is represented by its shortest [SLEB128 encoding]
 * A `Word64` is represented by 8 bytes.
 * A `Word32` is represented by 4 bytes.
 * A `Word16` is represented by 2 bytes.
 * A `Word8` is represented by 1 byte.
 * A `Bool` is represented by 1 byte that is `0` for `false` and `1` for `true`.
 * A `Text` is represented by a LEB128-encoded number indicating the length of
   the following payload, followed by the payload as a utf8-encoded string (no
   trailing `\0`).
 * An `Array` is represented by a LEB128-encoded number indicating the number
   of entries, followed by the concatenation of the representation of these
   entries.
 * A `Tuple` is represented by the concatenation of the representation of its
   entries. (No need for a length field, as it can be statically determined.)
 * An `Object` is represented by the concatenation of the representation of its
   fields, sorted by field name. (The field names are not serialized, as they
   are statically known.)
 * An `Option` is represented by a single byte `0` if it is `null`, or
   otherwise by a single byte `1` followed by the representation of the value
 * An empty tuple, the type `Null` and the type `Shared` are represented by
   zero bytes.
 * A `Variant` with `n` constructors sorted by constructor name is represented
   by a LEB128-encoded number the constructor as a number `0..n-1`, followed
   by the payload of the constructor. (Yes, obviously no subtyping here.)

*Example:* The ActorScript value
```
(null, ?4, "!") : (?Text, ?Int, Text)
```
is represented as
```
00 01 04 00 00 00 00 00 00 00 01 21
```

[LEB128 encoding](https://en.wikipedia.org/wiki/LEB128)

General argument format (with references)
-----------------------------------------

Argument with a type that mentions reference types (actors or shared functions)
are represented as an `elembuf`:

 * the first entry is a `databuf` contains the data according to the format
   above.
 * all further entries are the references contained in the data.

The above format is thus extended with the following case:

 * A reference (`actor`, `shared func`) is represented as a 32-bit number (4
   bytes). Thus number is an index into the surrounding `elembuf`.

   NB: The index is never never `0`, as the first entry in the `elembuf` is the
   `databuf` with the actual data.

*Example:* The ActorScript value
```
(null, ?console) : (?actor {}, ?actor {log : Text -> () })
```
is represented as
```
elembuf [databuf [00 01 01 00 00 00], console]
```
