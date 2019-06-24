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
<functype>  ::= ( <fieldtype>,* ) -> ( <fieldtype>,* ) <funcann>*
<funcann>   ::= oneway | pure
<fieldtype> ::= <nat> : <datatype>
<datatype>  ::= <id> | <primtype> | <constype> | <reftype>

<primtype>  ::=
  | nat | nat8 | nat16 | nat32 | nat64
  | int | int8 | int16 | int32 | int64
  | float32 | float64
  | bool
  | text
  | null
  | reserved
  | empty

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
  deleteUser : (id : nat64) -> () oneway;
}
```


### Functions

*Functions* are endpoints for communication.   A typical function invocation is a bidirectional communication, with *parameters* and *results*, a.k.a. request and response. A `oneway` function invocation is a uni-directional communication with zero or more parameters but no results, intended for fire-and-forget scenarios.

**Note:** The IDL is in fact agnostic to the question whether communication via functions is synchronous (like RPCs) or asynchronous (like messaging with callbacks as response continuations). However, it assumes that all invocations have the same semantics, i.e., there is no need to distinguish between both.

**Note:** In a synchronous interpretation of functions, invocation of a oneway function would return immediately, without waiting for completion of the service-side invocation of the function. In an asynchronous interpretation of functions, the invocation of a `oneway` function does not accept a callback (to invoke on completion).

#### Structure

A function type describes the list of parameters and results and their respective types. It can optionally be annotated to be *pure*, which indicates that it does not modify any state and can potentially be executed more efficiently (e.g., on cached state). (Other annotations may be added in the future.)

```
<functype> ::= ( <fieldtype>,* ) -> ( <fieldtype>,* ) <funcann>*
<funcann>  ::= oneway | pure
```

The parameter and result lists are essentially treated as records, see below. That is, they are named, not positional.
The list of parameters must be shorter than 2^32 values and no name/id may occur twice in it. The same restrictions apply to the result list.
The result list of a `oneway` function must be empty.

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

#### Reserved

The type `reserved` is a type with unknown content that ought to be ignored. Its purpose is to occupy field ids in records in order to prevent backwards/forwards compatibility problems, see the description of record types below.
```
<primtype> ::= ... | reserved
```
**Note:** This type has a similar role as *reserved fields* in proto buffers.

#### Empty

The type `empty` is the type of values that are not present. Its purpose is to mark variants that are not actually there, or -- as argument types of function reference -- indicate that they will not be called.
```
<primtype> ::= ... | empty
```

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

An id can also be given as a *name*, which is a shorthand for a numeric id that is the hash of that name:
```
<fieldtype> ::= ...
  | <name> : <datatype>    :=  <hash(name)> : <datatype>
```

The purpose of identifying fields by unique (numeric or textual) ids is to support safe upgrading of the record type returned by an IDL function: a new version of an IDL can safely *add* fields to an out record as long as their id has not been used before. See the discussion on upgrading below for more details.

The hash function is specified as
```
hash(id) = ( Sum_(i=0..k) id[i] * 223^(k-i) ) mod 2^32 where k = |id|-1
```

This expansion implies that a hash collision between field names within a single record is disallowed.

This hash function has the the following useful properties:

 * Collisions are sufficiently rare. It has [no collisions for names up to length 4](https://caml.inria.fr/pub/papers/garrigue-polymorphic_variants-ml98.pdf).

 * It is rather simple to implement, compared to, say, a cryptographic hash function (we do not need resistence against collision attacks).

The hash function does not have the property that every numeric value can be turned into a human-readable preimage. Host languages that cannot support numeric field names will have to come up with a suitable encoding textual encoding of numeric field names, as well as of field names that are not valid in the host langauge.


##### Shorthand: Tuple Fields

Field ids can also be omitted entirely, which is just a shorthand for picking either 0 (for the first field) or N+1 when the previous field has id N.
```
<fieldtype> ::= ...
  | <datatype>    :=  N : <datatype>
```

##### Examples
```
record {
  name : text;
  street : text;
  num : nat;
  city : text;
  zip : nat;
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

Subtyping applies recursively to the types of the fields themselves. Moreover, the directions get *inverted* for inbound function and actor references, in compliance with standard rules.

**TODO: unsound, fix**

To make these constraints as flexible as possible, two special rules apply:

 * An absent record field is considered equivalent to a present field with value `null`. Moreover, a record field of type `null` is a subtype of a field with type `opt <datatype>`. That way,	That way, a field of option (or null) type can always be added to a record type, no matter whether in co- or contra-variant position. If an optional field is added to an inbound record, and he client did not provide it, the service will read it as if its value was null.

  - in an outbound record, a field of option (or null) type can also be removed in an upgrade, in which case the client will read it as if its value was null;	
  - in an inbound record, a field of option (or null) type can also be added, in which case the service will read it as if its value was null.

Future extensions: defaults, including for variants?


### Rules

#### Primitive Types

Most primitive types cannot be changed in an upgrade.
```

------------------------
<primtype> <: <primtype>
```

An exception are integers, which can be specialised to natural numbers:

```

-----------
nat <: int
```

Additional rules apply to `empty` and `reserved`, which makes these a bottom resp. top type:
```

-------------------------
<datatype> <: reserved


--------------------
empty <: <datatype>
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
Furthermore, an option type can be specialised to either `null` or to its constituent type:
```
not (null <: <datatype>)
------------------------
null <: opt <datatype>

not (null <: <datatype>)
<datatype> <: <datatype'>
-----------------------------
<datatype> <: opt <datatype'>
```
The premise means that the rule does not apply when the constituent type is itself `null` or an option. That restriction is necessary so that there is no ambiguity. For example, there would be two ways to interpret `null` when going from `opt nat` to `opt opt nat`, either as `null` or as `?null`.

Q: The negated nature of this premise isn't really compatible with parametric polymorphism. Is that a problem? We could always introduce a supertype of all non-nullable types and rephrase it with that.


#### Records

In a specialised record type, the type of a record field can be specialised, or a field can be added.
```

---------------------------------------
record { <fieldtype'>;* } <: record { }

<datatype> <: <datatype'>
record { <fieldtype>;* } <: record { <fieldtype'>;* }
----------------------------------------------------------------------------------------------
record { <nat> : <datatype>; <fieldtype>;* } <: record { <nat> : <datatype'>; <fieldtype'>;* }
```

**TODO: Rules below are unsound as is, need fixing!**

In addition, record fields of `null` or option type can be removed, treating the absent field as having value `null`.
```
record { <fieldtype>;* } <: record { <fieldtype'>;* }
-------------------------------------------------------------------
record { <fieldtype>;* } <: record { <nat> : null; <fieldtype'>;* }

record { <fieldtype>;* } <: record { <fieldtype'>;* }
-----------------------------------------------------------------------------
record { <fieldtype>;* } <: record { <nat> : opt <datatype>; <fieldtype'>;* }
```
TODO: What we want to achieve: Taken together, these rules ensure that adding an optional field creates both a co- and a contra-variant subtype. Consequently, it is always possible to add an optional field. In particular, that allows extending round-tripping record types as well. For example,
```
type T = {};
actor { f : T -> T };
```
can safely be upgraded to
```
type T' = {x : opt text};
actor { f : T' -> T' };
```
for all first-order uses of `T`, because both of the following hold:
```
upgrade T' <: T
upgrade T <: T'
```
And hence:
```
upgrade (T' -> T')  <:  (T -> T)
```
for some version of a type `upgrade T`.
Moreover, this extends to the higher-order case, where e.g. a function of the above type is expected as a parameter:
```
actor { g : (h : T -> T) -> ()}
```
Upgrading `T` as above contra-variantly requires
```
upgrade (T -> T)  <:  (T' -> T')
```
which also holds.

Note: Subtyping still needs to be transitive . We must not allow:
```
record {x : opt text} <: record {} <: record {x : opt nat}
```
**TODO: Sanity continues from here.**


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
-----------------------------------------------------------------------------------------------------------------------
func ( <fieldtype1>,* ) -> ( <fieldtype2>,* ) <funcann>* <: func ( <fieldtype1'>,* ) -> ( <fieldtype2'>,* ) <funcann>*
```

Viewed as sets, the annotations on the functions must be equal.

#### Actors

For an actor, a method can be specialised (by specialising its function type), or a method added. Essentially, they are treated like records of functions.
```

----------------------------------------
service { <methtype'>;* } <: service { }

<functype> <: <functype'>
service { <methtype>;* } <: service { <methtype'>;* }
------------------------------------------------------------------------------------------------
service { <name> : <functype>; <methtype>;* } <: service { <name> : <functype'>; <methtype'>;* }
```

### Elaboration

To define the actual coercion function, we extend the subtyping relation to a ternary *elaboration* relation `T <: T' ~> f`, where `f` is a suitable coercion function of type `T -> T'`.


#### Primitive Types

```

--------------------------------
<primtype> <: <primtype> ~> \x.x


------------------
Nat <: Int ~> \x.x


------------------------------
<datatype> <: reserved ~> \x.x


------------------------------
empty <: <datatype> ~> \_.unreachable
```

#### Options and Vectors

```
<datatype> <: <datatype'> ~> f
---------------------------------------------------
opt <datatype> <: opt <datatype'> ~> \x.map_opt f x

<datatype> <: <datatype'> ~> f
---------------------------------------------------
vec <datatype> <: vec <datatype'> ~> \x.map_vec f x

not (null <: <datatype>)
---------------------------------
null <: opt <datatype> ~> \x.null

not (null <: <datatype>)
<datatype> <: <datatype'> ~> f
------------------------------------------
<datatype> <: opt <datatype'> ~> \x.?(f x)
```


#### Records

```

------------------------------------------------
record { <fieldtype'>;* } <: record { } ~> \x.{}

<datatype> <: <datatype'> ~> f1
record { <fieldtype>;* } <: record { <fieldtype'>;* } ~> f2
----------------------------------------------------------------------------------------------
record { <nat> : <datatype>; <fieldtype>;* } <: record { <nat> : <datatype'>; <fieldtype'>;* }
  ~> \x.{f2 x with <nat> = f1 x.<nat>}
```

TODO: Fix
```
record { <fieldtype>;* } <: record { <fieldtype'>;* } ~> f
-------------------------------------------------------------------
record { <fieldtype>;* } <: record { <nat> : null; <fieldtype'>;* }
  ~> \x.{f x; <nat> = null}

record { <fieldtype>;* } <: record { <fieldtype'>;* } ~> f
-----------------------------------------------------------------------------
record { <fieldtype>;* } <: record { <nat> : opt <datatype>; <fieldtype'>;* }
  ~> \x.{f x; <nat> = null}
```


#### Variants

```

-------------------------------------------------
variant { } <: variant { <fieldtype'>;* } ~> \x.x

<datatype> <: <datatype'> ~> f1
variant { <fieldtype>;* } <: variant { <fieldtype'>;* } ~> f2
------------------------------------------------------------------------------------------------
variant { <nat> : <datatype>; <fieldtype>;* } <: variant { <nat> : <datatype'>; <fieldtype'>;* }
  ~> \x.case x of <nat> y => <nat> (f1 y) | _ => f2 x
```

#### Functions

```
record { <fieldtype1'>;* } <: record { <fieldtype1>;* } ~> f1
record { <fieldtype2>;* } <: record { <fieldtype2'>;* } ~> f2
----------------------------------------------------------------------------------------------------------------------
func ( <fieldtype1>,* ) -> ( <fieldtype2>,* ) <funcann>* <: func ( <fieldtype1'>,* ) -> ( <fieldtype2'>,* ) <funcann>*
  ~> \x.\y.f2 (x (f1 y))
```

#### Actors

```

-------------------------------------------------
service { <methtype'>;* } <: service { } ~> \x.{}

<functype> <: <functype'> ~> f1
service { <methtype>;* } <: service { <methtype'>;* } ~> f2
------------------------------------------------------------------------------------------------
service { <name> : <functype>; <methtype>;* } <: service { <name> : <functype'>; <methtype'>;* }
  ~> \x.{f1 x; <name> = f2 x.<name>}
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

With this IDL file, the server code in ActorScript could be:
```
actor Server {
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

At runtime, every IDL value is serialised into a triple (T, M, R), where T ("type") and M ("memory") are sequences of bytes and R ("references") is a sequence of references. If R is empty, it can be omitted.

By making the type of the data explicit, (1) the serialised data becomes self-describing, which is useful for tooling, (2) error discovery and error handling is improved, (3) the binary format is decoupled from versioning concerns, so that the latter can be designed more flexible.

By using references, (1) the wire representation of reference values (which may be complex and involve system meta data such as types) need not be exposed to client code, and (2) the system knows where the references are in the serialised data, such that it can rewrite/map/filter/adjust them as it sees fit.

Accordingly, serialisation is defined by three mapping functions, `T`, `M` and `R`, producing the respective components.

Note:

* While not required, a serialiser can minimise the size of the serialised type by first computing a value's *principal* type with respect to the subtyping relation. In particular, a variant type need not include more fields than necessary. For example, if `E = variant { A, B, C }` and the value to serialise is `A` of type `E`, then it can be serialised with type `variant { A }`. Similarly, if the value is the vector `[C, A, C]`, then the principal type `vec (variant { A, C })` suffices.


### Serialisation

#### Notation

`T` and `M` create a byte sequence described below in terms of natural storage types (`i<N>` for `N = 8, 16, 32, 64`, `f<N>` for `N = 32, 64`).

The following notation is used:

* `.` is the empty byte sequence
* `x1 x2` is concatenation
* `t^N`, `t+`, `t*`, `t?` are sequences of `N`, `N>0`, `N>=0`, or `N<=1` repetitions, respectively
* `leb128` and `sleb128` are the shortest unsigned and signed [LEB128](https://en.wikipedia.org/wiki/LEB128) encodings of a number, respectively
* `utf8` is the UTF-8 encoding of a text string (not 0-terminated)


#### Types

`T` maps an IDL type to a byte sequence representing that type.
Each type constructor is encoded as a negative opcode;
positive numbers index auxiliary *type definitions* that define more complex types.
We assume that the fields in a record or function type are sorted by increasing id and the methods in an actor are sorted by name.

```
T : <primtype> -> i8*
T(null)     = sleb128(-1)
T(bool)     = sleb128(-2)
T(nat)      = sleb128(-3)
T(int)      = sleb128(-4)
T(nat<N>)   = sleb128(-5 - log2(N/8))
T(int<N>)   = sleb128(-9 - log2(N/8))
T(float<N>) = sleb128(-13 - log2(N/32))
T(text)     = sleb128(-15)
T(reserved) = sleb128(-16)
T(empty)    = sleb128(-17)

T : <constype> -> i8*
T(opt <datatype>) = sleb128(-18) I(<datatype>)
T(vec <datatype>) = sleb128(-19) I(<datatype>)
T(record {<fieldtype>^N}) = sleb128(-20) T*(<fieldtype>^N)
T(variant {<fieldtype>^N}) = sleb128(-21) T*(<fieldtype>^N)

T : <fieldtype> -> i8*
T(<nat>:<datatype>) = leb128(<nat>) I(<datatype>)

T : <reftype> -> i8*
T(func (<fieldtype1>*) -> (<fieldtype2>*) <funcann>*) =
  sleb128(-21) T*(<fieldtype2>*) T*(<fieldtype2>*) T*(<funcann>*)
T(service {<methtype>*}) =
  sleb128(-22) T*(<methtype>*)

T : <methtype> -> i8*
T(<name>:<datatype>) = leb128(|utf8(<name>)|) i8*(utf8(<name>)) I(<datatype>)

T : <funcann> -> i8*
T(pure)   = i8(1)
T(oneway) = i8(2)

T* : <X>* -> i8*
T*(<X>^N) = leb128(N) T(<X>)^N
```

Every nested type is encoded as either a primitive type or an index into a list of *type definitions*. This allows for recursive types and sharing of types occuring multiple times:

```
I : <datatype> -> i8*
I(<primtype>) = T(<primtype>)
I(<datatype>) = sleb128(i)  where type definition i defines T(<datatype>)

I* : <X>* -> i8*
I*(<X>^N) = leb128(N) I(<X>)^N
```

Type definitions themselves are represented as a list of serialised data types:
```
T*(<datatype>*)
```
The data types in this list can themselves refer to each other (or themselves) via `I`.

Note:

* Due to the type definition prefix, there are always multiple possible ways to represent any given serialised type. Type serialisation hence is not technically a function but a relation.

* The serialised data type representing a method type must denote a function type.

* Because recursion goes through `T`, this format by construction rules out non-well-founded definitions like `type t = t`.

#### Memory

`M` maps an IDL value to a byte sequence representing that value. The definition is indexed by type.
We assume that the fields in a record value are sorted by increasing id.

```
M : <val> -> <primtype> -> i8*
M(n : nat)      = leb128(n)
M(i : int)      = sleb128(i)
M(n : nat<N>)   = i<N>(n)
M(i : int<N>)   = i<N>(signed_N^-1(i))
M(z : float<N>) = f<N>(z)
M(b : bool)     = i8(if b then 1 else 0)
M(t : text)     = leb128(|utf8(t)|) i8*(utf8(t))
M(_ : null)     = .
M(_ : reserved) = .
// NB: M(_ : empty) will never be called

M : <val> -> <constype> -> i8*
M(null : opt <datatype>) = i8(0)
M(?v   : opt <datatype>) = i8(1) M(v : <datatype>)
M(v*   : vec <datatype>) = leb128(N) M(v : <datatype>)*
M(kv*  : record {<fieldtype>*}) = M(kv : <fieldtype>)*
M(kv   : variant {<fieldtype>*}) = leb128(i) M(kv : <fieldtype>*[i])

M : (<nat>, <val>) -> <fieldtype> -> i8*
M((k,v) : k:<datatype>) = M(v : <datatype>)

M : <val> -> <reftype> -> i8*
M(r : service <actortype>) = .
M(r : func <functype>)     = .
```


#### References

`R` maps an IDL value to the sequence of references contained in that value. The definition is indexed by type.
We assume that the fields in a record value are sorted by increasing id.

```
R : <val> -> <primtype> -> <ref>*
R(_ : <primtype>) = .

R : <val> -> <constype> -> <ref>*
R(null : opt <datatype>) = .
R(?v   : opt <datatype>) = R(v : <datatype>)
R(v*   : vec <datatype>) = R(v : <datatype>)*
R(kv*  : record {<fieldtype>*}) = R(kv : <fieldtype>)*
R(kv   : variant {<fieldtype>*}) = R(kv : <fieldtype>*[i])

R : (<nat>, <val>) -> <fieldtype> -> <ref>*
R((k,v) : k:<datatype>) = R(v : <datatype>)

R : <val> -> <reftype> -> <ref>*
R(r : service <actortype>) = r
R(r : func <functype>)     = r
```

Note:

* It is unspecified how references *r* are represented, neither internally nor externally. When binding to Wasm, their internal representation is expected to be based on Wasm reference types, i.e., `anyref` or subtypes thereof. It is up to the system how to represent or translate the reference table on the wire.


### Deserialisation

Deserialisation is the parallel application of the inverse functions of `T`, `M`, and `R` defined above, with the following mechanism for robustness towards future extensions:

* A serialised type may be headed by an opcode other than the ones defined above (i.e., less than -23). Any such opcode is followed by an LEB128-encoded count, and then a number of bytes corresponding to this count. A type represented that way is called a *future type*.

* A value corresponding to a future type is called a *future value*. It is represented by two LEB128-encoded counts, *m* and *n*, followed by a *m* bytes in the memory representation M and accompanied by *n* corresponding references in R.

These measures allow the serialisation format to be extended with new types in the future, as long as their representation and the representation of the corresponding values include a length prefix matching the above scheme, and thereby allowing an older deserialiser not understanding them to skip over them. The subtyping rules ensure that upgradability is maintained in this situation, i.e., an old deserialiser has no need to understand the encoded data.


### Parameters and Results

`A` defines the argument mapping. Essentially, an argument list is serialised into the triple (T,M,R) as if it was a single closed record. T and M are combined into a single byte stream B, where they are preceded by the string "DIDL" as a magic number and a possible list of type definitions.
We assume that the argument values are sorted by increasing id.

```
A(kv* : <fieldtype>*) = ( B(kv* : <fieldtype>*), R(kv* : <fieldtype>*) )

B(kv* : <fieldtype>*) =
  i8('D') i8('I') i8('D') i8('L')      magic number
  T*(<datatype>*)                      type definition table
  I*(<fieldtype>*)                     type of argument list
  M(kv* : <fieldtype>*)                values of argument list
```
The `<datatype>` vector contains an arbitrary sequence of type definitions (see above), to be referenced in the serialisation of the `<fieldtype>` vector.

The same representation is used for function results.

Note:

* It is unspecified how the pair (B,R) representing a serialised value is bundled together in an external environment.
