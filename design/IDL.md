## Motivation

To document, discover, and interact with actors on the platform, we need an interface description language (IDL) for specifying the signature of an actor.

#### Goals:

* Language-independent description of actor interfaces and the data they 
exchange (names, parameter and result formats of actor methods)
* Simple and canonical constructs (C-like; algebraically: sums, products, exponentials)
* Extensible, backwards-compatible
* Well-formedness is checkable and guaranteed by the platform
* Deterministic mapping to serialised representation
* Human-readable and machine-readable
* Declarative, usable as input to binding code generators

#### Non-Goals:

* Specification of semantic constraints beyond representation concerns
  (Rationale: (1) there is no natural boundary to what should be expressible, a scalable solution would quickly lead into the realm of program logics and/or dependent types; (2) cost and complexity for the platform, the hypervisor would have to check and guarantee these properties on every message send; (3) in general, interesting properties cannot be formulated or checked without contextual information such as an actor’s state.)
* Prescribing the wire format used internally by the network to transport data (though it will make sense to use an extension of the serialisation format decribed, which is fairly generic)

#### Inspiration:

* Protocol buffers
* Thrift
* Fuchsia IDL
* CORBA
* JSON, YAML
* ...
* Fisher, Mandelbaum, Walker: The next 700 data description languages

#### Why not Protocol Buffers or others?

Vanilla protocol buffers are not sufficient or well-suited for describing Dfinity actors:

* They are primarily a *data description language*, not an IDL. There is syntax for defining "services", but it assumes RPCs not messaging and requires a developing a plugin (replacing the gRPC a.k.a. Stubby one) to provide a semantics.

* They deserialise data into abstract protobuf objects, not actual language data structures. The "message" (a.k.a. objects/records/structs) format is designed to be represented as its own abstract in-memory type.

* They are an inherently first-order data format, i.e., cannot describe functions/callbacks or object/actor parameters with methods.

* They lack various data types that we want to be able to handle, such as proper arrays, big nums, variants, references.

* They provide no place to express other information we might need to incorporate into IDL descriptions, such as "read-onlyness" or access control specifications.

* They do not have a semantically sound and compositional foundation for defining safe upgradability (e.g., as a relation on interface types).

* They do have a number of features we do not need, or may want to define differently.

Given all of the above, I expect there would be fairly little we would be able to reuse from existing protobuf bindings. At the same time, there would be no easy way to incorporate various extensions we require.


## Type Structure

The purpose of an IDL is defining the signature, and thereby the *type* of an actor (service), that is, the set of messages and their parameter and result types. To that end, the grammar for the IDL consists mostly of a type grammar.
 
#### Core Grammar

This is a summary of the grammar proposed:
```
<prog>  ::= <def>;* <actor>;?
<def>   ::= type <id> = <datatype> | import <text>
<actor> ::= service <id> : (<actortype> | <id>)

<actortype> ::= { <methtype>;* }
<methtype>  ::= <name> : (<functype> | <id>)
<functype>  ::= ( <fieldtype>,* ) -> ( <fieldtype>,* ) <funcannot>*
<funcannot> ::= pure
<paramtype> ::= <datatype>
<datatype>  ::= <id> | <primtype> | <constype> | <reftype>

<primtype>  ::=
  | nat | nat8 | nat16 | nat32 | nat64
  | int | int8 | int16 | int32 | int64
  | float32 | float64
  | bool
  | text
  | null
  | unavailable

<constype>  ::=
  | opt <datatype>
  | vec <datatype>
  | record { <fieldtype>;* }
  | variant { <fieldtype>;* }

<reftype>  ::=
  | func <functype>
  | service <actortype>

<name> ::= <id> | <text>
<id>   ::= (A..Z|a..z|_)(A..Z|a..z|_|0..9)*
<text> ::= "<char>*"
<nat>  ::= (0..9)(_? 0..9)* | 0x(0..9|a..f|A..F)(_? 0..9|a..f|A..F)*
```

A `<char>` is a *Unicode scalar value* (i.e., a codepoint that is not a surrogate part).


#### Syntactic Shorthands

In addition to this basic grammar, a few syntactic shorthands are supported that can be reduced to the basic forms:

```
<constype> ::= ...
  | blob                   :=  vec nat8

<paramtype> ::= ...
  | <name> : <datatype>    :=  <datatype>

<fieldtype> ::= ...
  | <name> : <datatype>    :=  <hash(name)> : <datatype>
  | <datatype>             :=  N : <datatype>  where N is either 0 or previous + 1  (only in records)
  | <nat>                  :=  <nat> : null   (only in variants)
  | <name>                 :=  <name> : null  (only in variants)
```

#### Comments

Comments can be given as either single-line or block form:
```
<comment> ::=
  | //<codepoint>*<nl>
  | /*(<codepoint>|<comment>)*/
```
Block comments nest properly (unlike in C).


### Services

An *service* is a standalone actor on the platform that can communicate with other services via sending and receiving *messages*. Messages are sent to a service by invoking one of its *methods*, i.e., functions that the service provides.

**Note:** The IDL is in fact agnostic to the exact nature of services. In particular, it could be applied to a setting where services are synchronous (objects with RPCs) instead of asynchronous (actors with bidirectional message sends).


#### Structure

A service's signature is described by an *actor type*, which defines the list of *methods* that the service provides. Each method is described by its *name* and a *function type* describing its signature. The function type can also be given by referring to a type definition naming a function reference type.

```
<actortype> ::= { <methtype>;* }
<methtype>  ::= <name> : (<functype> | <id>)
```

#### Names

A name is given either in the syntax of a typical programming language identifier, or as an arbitrary string in quotes:
```
<name> ::= <id> | <text>
<id>   ::= (A..Z|a..z|_)(A..Z|a..z|_|0..9)*
<text> ::= "<char>*"
```
Identifiers cannot be keywords of the IDL grammar. In case a name is needed that coincides with a keyword, it has to be quoted as a text string.


#### Example
```
service {
  addUser : (name : text, age : nat8) -> (id : nat64);
  userName : (id : nat64) -> (text) pure;
  userAge : (id : nat64) -> (nat8) pure;
}
```


### Functions

*Functions* are endpoints for communication. A function invocation is a bidirectional communication, with *parameters* and *results*, a.k.a. request and response.

**Note:** The IDL is in fact agnostic to the question whether communication via functions is synchronous (like RPCs) or asynchronous (like messaging with callbacks as response continuations). However, it assumes that all invocations have the same semantics, i.e., there is no need to distinguish between both.


#### Structure

A function type describes the list of parameters and results and their respective types. It can optionally be annotated to be *pure*, which indicates that it does not modify any state and can potentially be executed more efficiently (e.g., on cached state). (Other annotations may be added in the future.)

```
<functype>  ::= ( <fieldtype>,* ) -> ( <fieldtype>,* ) <funcannot>*
<funcannot> ::= pure
```

The parameter and result lists are essentially treated as records, see below. That is, they are named, not positional.
The list of parameters must be shorter than 2^32 values and no name/id may occur twice in it. The same restrictions apply to the result list.

#### Example
```
(name : text, address : text, nr : nat16) -> (nick : text, id : nat64)
```


### Data

The content of message arguments and results is *data*. Three basic forms of *data types* can be distinguished: *primitive data*, which are basic values, and *constructed data*, which are compound forms of data types, and *references*, which point to a resource in the network.

```
<datatype>  ::= <primtype> | <constype> | <reftype>
```


### Primitive Data

*Primitive types* describe the possible forms of primitive data.


#### Natural Numbers

The type `nat` describes a natural number (unsigned integer) of unlimited range. There are also variants limited to 8, 16, 32, or 64 bit value range with fixed-size representations.

```
<primtype> ::= nat | nat8 | nat16 | nat32 | nat64 | ...
```
**Note:** Values of type `nat` have variable length representations in the  binary serialisation format, and hence take up space proportional to (the logarithm of) their value. As long as typical values are small, they may hence be more space-efficient than the fixed size types.

#### Integer Numbers

The type `int` describes an integer number (signed) of unlimited range. There are also variants limited to 8, 16, 32, or 64 bit value range with fixed-size representations.

```
<primtype> ::= ... | int | int8 | int16 | int32 | int64 | ...
```
**Note:** Values of type `nat` have variable length representations in the binary serialisation format, and hence take up space proportional to (the logarithm of) their value. As long as typical values are small, they may hence be more space-efficient than the fixed size types.

#### Floating-Point Numbers

Floating-point values are represented in IEEE 754 binary format and are supported in single precision (32 bit) and double precision (64 bit).

```
<primtype> ::= ... | float32 | float64 | ...
```

#### Boolean

Boolean truth values are represented by the type `bool`.
```
<primtype> ::= ... | bool | ...
```

#### Text

Text strings are represented by the type `text` and consist of a sequence of Unicode scalar values.
```
<primtype> ::= ... | text | ...
```
**Note:** The `text` type is distinguished from `vec nat8` (a UTF-8 string) or `vec nat32` (a sequence of code points) in order to allow bindings to map it to a suitable string type, and enable the binary format to select an efficient internal representation independently.

#### Null

The type `null` has exactly one value (the *null* value) and therefor carries no information. It can e.g. be used as a placeholder for optional fields that ought to be added to a record in future upgrades, or for *variant cases* that do not need any value, see below.
```
<primtype> ::= ... | null | ...
```

#### Unavailable

The type `unavailable` is a type with unknown content that ought to be ignored. Its purpose is to occupy field ids in records in order to prevent backwards/forwards compatibility problems, see the description of record types below.
```
<primtype> ::= ... | unavailable
```
**Note:** This type has a similar role as *reserved fields* in proto buffers.


### Constructed Data

*Constructed types* describe compound or aggregated forms of values.


#### Options

An *option* is a value of a specific data type that may be absent.
```
<constype>  ::= opt <datatype> | ...
```

#### Vectors

A *vector* is a *homogeneous* sequence of values of the same data type.
```
<constype>  ::= ... | vec <datatype> | ...
```

##### Shorthand: Blobs

A shorthand exists for the specific vector *blob*, which is an arbitrary sequence of bytes:
```
<constype> ::= ....
  | blob   := vec nat8
```

#### Records

A *record* is a *heterogeneous* sequence of values of different data types. Each value is tagged by a *field id* which is a numeric value that has to be unique within the record and carries a single value of specified data type. The order in which fields are specified is immaterial.

```
<constype>  ::= ... | record { <fieldtype>;* } | ...
<fieldtype> ::= <nat> : <datatype>
```

The id is described as a simple unsigned integer that has to fit the 64 bit value range. It can be given in either decimal or hexadecimal notation:

```
<nat> ::= (0..9)(_? 0..9)* | 0x(0..9|a..f|A..F)(_? 0..9|a..f|A..F)*
```
An id value must be smaller than 2^32 and no id may occur twice in the same record type.



##### Shorthand: Symbolic Field Ids

An id can also be given as a *name*, which is a shorthand for a numeric id that is the hash of that name wrt a specified hash function, e.g. SHA-256 mod 64.
```
<fieldtype> ::= ...
  | <name> : <datatype>    :=  <hash(name)> : <datatype>
```
This expansion implies that a hash collision between field names within a single record is disallowed. However, the chosen hash function makes such a collision highly unlikely in practice.

**Note:** For example, the following hash function [Jacques Garrigue, "Programming with Polymorphic Variants", ML 1998],
```
hash(id) = ( Sum_(i=0..|id|) id[i] * 223^(|id|-1) ) mod 2^64
```
guarantees that no hash collision occurs for regular identifiers of length up to 8, and that the overall likelihood of a collision for a variant with n cases is lower than
```
p(n) = Sum_(i=1..n-1) i/2^64
```
That is, the likelihood p(100) of a collision for a variant with 100 cases is less than 2.74 * 10^(-16).

##### Shorthand: Tuple Fields

Field ids can also be omitted entirely, which is just a shorthand for picking either 0 (for the first field) or N+1 when the previous field has id N.
```
<fieldtype> ::= ...
  | <datatype>    :=  N : <datatype>
```

##### Upgrading

The purpose of identifying fields by unique (numeric or textual) ids is to support safe upgrading of the record type returned by an IDL function: a new version of an IDL can safely *add* fields to a record as long as their id has not been used before. See below for more details.

##### Examples
```
record {
  name : text;
  street : text;
  num : nat;
  city : text;
  zip : nat;
  state : unavailable;  // removed since no longer needed
}

record { nat; nat }
record { 0 : nat; 1 : nat }
```
The latter two records are equivalent.

#### Variants

A *variant* is a tagged union of different possible data types. The tag is given by a numeric id that uniquely determines the variant case. Each case is described as a field. The order in which fields are specified is immaterial.

```
<constype>  ::= ... | variant { <fieldtype>;* } | ...
```
A field id must be smaller than 2^32 and no id may occur twice in the same variant type.


##### Shorthand: Symbolic Tag Ids

Like for record fields, the id for a variant tag can also be given as a *name*, which is a shorthand for its hash.

##### Shorthand: Enumeration Types

The type of a variant field can be omitted, in which case it is `null`.
```
<fieldtype> ::= ...
  | <nat>    :=  <nat> : null
  | <name>   :=  <name> : null
```
This abbreviation only applies to variants. At the same time, variants do not allow the tuple field abbreviation for omitting the field id.

##### Example
```
type color = variant { red; green; blue };

type tree = variant {
  leaf : int;
  branch : record {left : tree; val : int; right : tree};
}
```


### References

A third form of value are *references*. They represent first-class handles to (possibly remote) *functions* or *services*.

#### Function References

A *function reference* is described by its function type. For example, they allow passing callbacks to other functions.

```
<reftype> ::= func <functype> | ...
```

##### Example

```
type engine = service {
  search : (query : text, callback : func (vec result) -> ());
}
```

#### Actor References

An *actor reference* points to a service and is described by an actor type. Through this, services can communicate connections to other services.

```
<reftype> ::= ... | service <actortype>
```


##### Example

```
type broker = service {
  findCounterService : (name : text) ->
    (service {up : () -> (); current : () -> nat});
}
```


### Type Definitions

Types can be named via *type definitions*.

```
<def>   ::= type <id> = <datatype>
```

Type definitions are mutually recursive, i.e., they can refer to themselves or each other. However, every type cycle must be productive, i.e., go through a type expression that is not just an identifier. A type definition that is *vacuous*, i.e., is only equal to itself, is not allowed.

##### Examples

```
type stream = opt record {head : nat; next : func () -> stream};
```
```
type node = record {head : nat; tail : list};
type list = opt node;
```
```
type A = B;
type B = A;  // error: cyclic type definition
```


### Imports

In order to allow splitting interface definitions up into multiple files or share common definitions between multiple interfaces, *import* declarations are provided.

```
<def>   ::= ... | import <text>
```

An import refers to another interface file by URL. The semantics is that of textual inclusion, except that definitions from the imported file must not refer to definitions from the importing file.

##### Example

File `A.dfn`:
```
type A = service { f : () -> () };
```
File `B.dfn`:
```
import "A.dfn"
service B : A ;
```

Open Question: Instead of a flat name space, should we require qualified names for imports?


### Interfaces

An *interface description* consists of a sequence of imports and type definitions, possibly followed by a service declaration. A service declaration names and specifies a service actor by specifying an actor type. The actor type may also be given by referring to the name of a type definition for an actor reference type.

```
<desc>  ::= <def>;* <service>;?
<service> ::= service <id>? : (<actortype> | <id>)
```

The optional name given to the service in an interface description is immaterial; it only serves as documentation.


## Upgrading and Subtyping

Interfaces are allowed to evolve over time in a manner that is *robust*, i.e., cannot break existing client code. To capture this notion precisely, a service of type `T` is *upgradable* to a version with another type `T'` if and only if `T'` is *structural subtype* of `T`, written `T' <: T`. This defines that `T'` is more *specialised* than `T`. (Note: A more specialised type is less general, i.e., denotes a smaller set of possible values, thus the direction of the subtype ordering, even though a subtype record can have *more* fields.)

For upgrading data structures passed between service and client, it is important to distinguish the direction in which the data flows, as the upgrading requirements are opposite to each other:

* *Outbound* data returned from service to client as message results is *provided* by the service; an upgrade may provide *more* or more refined data without breaking clients. For example, an outbound record may provide additional fields after an upgrade.

* *Inbound* data passed from client to service as message parameters is *required* by the service; an upgrade may only require *less* or less specific data without breaking clients. For example, an inbound record may require fewer fields after an upgrade.

That is, outbound message results can only be replaced with a subtype (more fields) in an upgrade, while inbound message parameters can only be replaced with a supertype (fewer fields). This corresponds to the notions of co-variance and contra-variance in type systems.

Subtyping replies recursively to the types of the fields themselves. Moreover, the directions get *inverted* for inbound function and actor references, in compliance with standard rules.

To make these constraints as flexible as possible, two special rules apply:

* An absent record field is considered equivalent to a present field with value `null`. Moreover, a record field of type `null` is a subtype of a field with type `opt <datatype>`. That way,

  - in an outbound record, a field of option (or null) type can also be removed in an upgrade, in which case the client will read it as if its value was null;
  - in an inbound record, a field of option (or null) type can also be added, in which case the service will read it as if its value was null.

Future extensions: defaults, including for variants?


### Rules

#### Primitive Types

A primitive type cannot be changed in an upgrade.
```

------------------------
<primtype> <: <primtype>
```

An additional rule applies to `unavailable`, which makes it a top type, i.e., a supertype of every type.
```

-------------------------
<datatype> <: unavailable
```

#### Options and Vectors

An option or vector type can be specialised via its constituent type.
```
<datatype> <: <datatype'>
---------------------------------
opt <datatype> <: opt <datatype'>

<datatype> <: <datatype'>
---------------------------------
vec <datatype> <: vec <datatype'>
```
More flexible rules apply to option types used as record field types, see below.

#### Records

In a specialised record type, the type of a record field can be specialised,  or a field can be added.
```

---------------------------------------
record { <fieldtype'>;* } <: record { }

<datatype> <: <datatype'>
record { <fieldtype>;* } <: record { <fieldtype'>;* }
----------------------------------------------------------------------------------------------
record { <nat> : <datatype>; <fieldtype>;* } <: record { <nat> : <datatype'>; <fieldtype'>;* }
```

**TODO: Rules below are unsound as is, need fixing!**

In addition, special rules apply to fields of `null` or option type, which makes an absent field interchangeable with a field of value `null`. Moreover, an optional field can be specialised to non-optional if the constituent type is not itself `null` or an option (this restriction is necessary to avoid confusing the different null values in an `opt (opt T)` type).
```
<datatype> = null \/ <datatype> = opt <datatype'>
record { <fieldtype>;* } <: record { <fieldtype'>;* }
-----------------------------------------------------------------------------
record { <fieldtype>;* } <: record { <nat> : <datatype>; <fieldtype'>;* }

<datatype> <> null /\ <datatype> <> opt <datatype''>
<datatype> <: <datatype'>
record { <fieldtype>;* } <: record { <fieldtype'>;* }
--------------------------------------------------------------------------------------------------
record { <nat> : <datatype>; <fieldtype>;* } <: record { <nat> : opt <datatype'>; <fieldtype'>;* }
```

#### Variants

For a specialised variants, the type of a tag can be specialised, or a tag can be removed.
```

-----------------------------------------
variant { } <: variant { <fieldtype'>;* }

<datatype> <: <datatype'>
variant { <fieldtype>;* } <: variant { <fieldtype'>;* }
------------------------------------------------------------------------------------------------
variant { <nat> : <datatype>; <fieldtype>;* } <: variant { <nat> : <datatype'>; <fieldtype'>;* }
```

#### Functions

For a specialised function, any parameter type can be generalised and any result type specialised. Moreover, arguments can be dropped while results can be added. That is, the rules mirror those of records.
```
record { <fieldtype1'>;* } <: record { <fieldtype1>;* }
record { <fieldtype2>;* } <: record { <fieldtype2'>;* }
------------------------------------------------------------------------------------------------
func ( <fieldtype1>,* ) -> ( <fieldtype2>,* ) <: func ( <fieldtype1'>,* ) -> ( <fieldtype2'>,* )
```

#### Actors

For an actor, a method can be specialised (by specialising its function type), or a method added. Essentially, they are treated exactly like records of functions.
```

------------------------------------
actor { <methtype'>;* } <: actor { }

<functype> <: <functype'>
actor { <methtype>;* } <: actor { <methtype'>;* }
--------------------------------------------------------------------------------------------
actor { <name> : <functype>; <methtype>;* } <: actor { <name> : <functype'>; <methtype'>;* }
```

## Example Development Flow

We take the produce exchange app as an example to illustrate how a developer would use IDL in their development flow.

IDL for produce exchange server:
```
type TruckTypeId = nat;
type Weight = float32;

type TruckTypeInfo = record {
 id : TruckTypeId;
 short_name : Text;
 description : Text;
 capacity : opt Weight;
 isFridge : opt bool;
 isFreezer : opt bool;
};

service Server : {
  registrarAddTruckType : (TruckTypeInfo) -> (opt TruckTypeId);
  registrarRemTruckType : (id : nat) -> (opt ());
};
```

Note:
* `TruckTypeId` and `nat` are used interchangeably.

With this IDL file, the server actor code will be:
```
actor server = {
  registrarAddTruckType(truck_info : TruckTypeInfo) : async ?TruckTypeId {
    getModel().truckTypeTable.AddInfoGetId(
      func (id_ : TruckTypeId) : TruckTypeInfo = shared {
        id = id_ : TruckTypeId;
        short_name = truck_info.short_name : Text;
        description = truck_info.description : Text;
      }
    )
  }
}
```

## Open Questions

* Support default field values?
* Better upgradability for variants?
* Support generic type definitions?
* Namespaces for imports?


## Binary Format

### Serialisation

At runtime, every IDL value is serialised into a pair (M, T), where M ("memory") is a sequence of bytes and T ("table") a sequence of references. If T is empty, it can be omitted. By using references, (1) the wire representation of reference values (which may be complex and involve system meta data such as types) need not be exposed to client code, and (2) the system knows where the references are in the serialised data, such that it can rewrite/map/filter/adjust them as it sees fit.

Accordingly, serialisation is defined by two mapping functions, `M` and `T`, producing the respective parts. They are defined independently, but both definitions are indexed by IDL types.

`M` maps an IDL value to a byte sequence described in terms of natural storage types (`i<N>` for N = 8, 16, 32, 64`, `f<N>` for `N = 32, 64`).

Notation:

* `.` is the empty byte sequence
* `x1 x2` is concatenation
* `t^N`, `t+`, `t*`, `t?` are sequences of `N`, `N>0`, `N>=0`, or `N<=1` repetitions, respectively
* `leb128` and `sleb128` are the shortest unsigned and signed [LEB128](https://en.wikipedia.org/wiki/LEB128) encodings of a number, respectively
* `utf8` is the UTF-8 encoding of a text string (not 0-terminated)

```
M(n : nat)      = leb128(n)
M(i : int)      = sleb128(i)
M(n : nat<N>)   = i<N>(n)
M(i : int<N>)   = i<N>(signed_N^-1(i))
M(z : float<N>) = f<N>(z)
M(b : bool)     = i8(if b then 1 else 0)
M(t : text)     = leb128(|utf8(t)|) i8^*(utf8(t))
M(_ : null)     = .
M(_ : unavailable) = .

M(null  : opt <datatype>) = i8(0)
M(?v    : opt <datatype>) = i8(1) M(v : <datatype>)
M(v^N   : vec <datatype>) = leb128(N) M(v : <datatype>)^N
M(kv^N  : struct{<fieldtype>^K}) = leb128(K') M(kv : <fieldtype>^K)^N  where K' is the number of fields produced
M((k,v) : variant{<fieldtype>*}) = i32(k) M(v : <datatype>)  iff k : <datatype> in <fieldtype>*

M((k,v) : <fieldtype>^*) = i32(k) leb128(|F(v : <datatype>)|) F(v : <datatype>)  iff k : <datatype> in <fieldtype>* and F(v : <datatype>) =/= .
M((k,v) : <fieldtype>^*) = .                          otherwise

F(null : opt <datatype>) = .
F(v : opt <datatype>)    = M(v : <datatype>)
F(v : <datatype>)        = M(v : <datatype>)          otherwise

M(r : service <actortype>) = leb128(T(r))
M(r : func <functype>)     = leb128(T(r))
```

Notes:

* When an `opt` type occurs as a record field, it is optimised by omitting the option's tag byte; in case of a `null` value the entire field is omitted, similarly for `unavailable`.

* Every record field explicitly includes the size of its payload data. This is to allow skipping an unknown field upon deserialisation, see below.

* The M-representation of references is the respective index into T, denoted by `T(r)` above. A serialiser is allowed (but not required to) merge multiple occurrences of the same reference in T.

* It is unspecified how references *r* are represented, neither internally nor externally. When binding to Wasm, their internal representation is expected to be based on Wasm reference types, i.e., `anyref` or subtypes thereof. It is up to the system how to represent or translate the reference table on the wire.

* Serialisation is a function, i.e., it deterministically produces a unique output. However, this output depends on the type, so two binary representations (or hashes thereof) are only comparable when the serialisation side type is known and was the same for both.


### Deserialisation

Deserialisation is the parallel application of the inverse functions of `M` and `T` defined above, with the following relaxation:

* A record representation may include *additional* fields not occurring in the static type, or which serialisation would omit (`null`, `unavailable`); they are simply ignored, the deserialiser can skip over the body using the field size.

### Parameters

`P` defines the parameter mapping. Essentially, a parameter list is serialised into the pair (M,T) as if it was a single closed record:

```
P(kv* : <fieldtype>,*) = M(kv* : <fieldtype>;*)
```

The same representation is used for function results.

This representation has the following implications:

* Parameters/results can be serialised in any order.
* Parameters/results can be subject to width subtyping and respective upgrading.
