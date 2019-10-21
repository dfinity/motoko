= Chapter: Introduction

{IDL} is an interface description language (IDL) for specifying
language-independent description of actor interfaces and the data they
exchange (names, parameter and result formats of actor methods).

The description can be either generated from the {proglang} code, or
written by hand. With the description file, you can use special
generated source code to easily send and receive messages to and from
actors in a variety of languages, such as {proglang}, Javascript and
SDK CLI.

== What is {IDL}?

* Language-independent description of actor interfaces and the data
they exchange (names, parameter and result formats of actor methods)
* Simple and canonical constructs (C-like; algebraically: sums,
  products, exponentials)
* Extensible, backwards-compatible
* Well-formedness is checkable and guaranteed by the platform
* Deterministic mapping to serialised representation
* Human-readable and machine-readable
* Declarative, usable as input to binding code generators

== How do they work?

You specify the actor interface and the associated types for each
actor in a `.did` file.  Here's a very basic example of a `.did` file
that defines an interface of an HR department.

...
type name = text;
type person = record {
  age : nat;
  address : text;
  level : variant {employee; manager; contractor};
};

service HR {
  add : (name : name, detail : person) -> (id : nat64);
  delete : (id : nat64) -> ();
  get : (id : nat64) -> (person) query;
  find : (name) -> (opt nat64) query;
  list : () -> (vec {name; person}) query;
};
...

As you can see, the interface description is mostly self-explanatory.
The HR actor (service) contains five methods: add, delete, get, find
and list. TODO: more text

== Generate {IDL} from {proglang}

If `foo.as` is an {proglang} `actor` compilation unit, then

...
asc --idl foo.as -o foo.did
...

will type-check the {proglang} code and map the {proglang} `actor`
type to an {IDL} `service` type and produce a textual {IDL} file
`foo.did`.

== Why not Protocol Buffers or others?

We draw inspiration from various existing designs, including Protocol
buffers, Thrift, Fuchsia IDL, CORBA, JSON, and YAML, as well as
[academic
papers](https://www.cs.princeton.edu/~dpw/papers/700popl06.pdf
"Fisher, Mandelbaum, Walker: The next 700 data description
languages").

Vanilla protocol buffers are not sufficient or well-suited for
describing actors:

* They are primarily a *data description language*, not an IDL. There
  is syntax for defining "services", but it assumes RPCs not messaging
  and requires a developing a plugin (replacing the gRPC a.k.a. Stubby
  one) to provide a semantics.

* They deserialise data into abstract protobuf objects, not actual
  language data structures. The "message"
  (a.k.a. objects/records/structs) format is designed to be
  represented as its own abstract in-memory type.

* They are an inherently first-order data format, i.e., cannot
  describe functions/callbacks or object/actor parameters with
  methods.

* They lack various data types that we want to be able to handle, such
  as proper arrays, big nums, variants, references.

* They provide no place to express other information we might need to
  incorporate into IDL descriptions, such as "read-onlyness" or access
  control specifications.

* They do not have a semantically sound and compositional foundation
  for defining safe upgradability (e.g., as a relation on interface
  types).

* They do have a number of features we do not need, or may want to
  define differently.

