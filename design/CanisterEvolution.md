
Claim 1: 
         Subtyping is the wrong idea for typing canister upgrades.

Subtyping is a partial order.  Time is a total order.  This idea
conflates these two (distinct) notion, which fundementally does not
make sense, and thus leads only to non-solutions.

Analogy from Gradual typing literature: Type consistency is a partial
order, but is distinct from subtyping.  These orderings are not
comparable, and attempts to use subtyping to define gradual type
consistency do not work.  Instead, type consistency is defined as
another (distinct) binary relation over types, with distinct
algebraic properties.

Analogously, we need a distinct relation (_not_ subtyping!) for
describing how the interface of a single canister relates to its
future interfaces over time.

= Evolution of interfaces in time

== Say goodbye to subtyping, for now

I invite you to forget about subtyping for the remainder of this
document.  I also invite you to pretend that {proglang} does not use
subtyping, and has a more minimal ML-like type system. The rest of
these ideas still make sense, and in no way hinge on us using
subtyping at any level of the canister interface story.  That is by
design.

== The "Evolves to" relation

Let us pronounce this new, distinct relation as "evolves to" since we
are describing canister interfaces that evolve over time:

   T1 $> T2

We read this as "type T1 _evolves to_ type T2".

The main design constraint here is _backwards compatibility_: 

For each canister with an interface type, we want to permit old
clients to interact with new evolutions of the original interface.

The converse is not true immediately, but is true in the higher order
case of fulfilling backwards compatibility as stated above: We also
care to help new canisters to talk to old clients, e.g., when those
old clients use the old interface to install callbacks (or otherwise
send higher-order data).

To accomplish these goals, we will extend this binary relation over
{proglang} types with a relation of the following judgement form:

  T $> S ~~> (e1; e2)

Where T and S are types related by the binary relation form, and
where the program terms e1 and e2 have the following types:

  e1 : T -> S
  e2 : S -> T

And these terms form a kind of "lens structure" (maybe?), which may or may not
be a Gaolis connection (maybe?).

These two properties say that these bridge functions compose as the
identity when going from the past to the future and back (claim 2, below),
and compose as a kind of "lossy identity" when going from the future
back to the past and then back to the future (claim 3, below):

  Claim 2. For all x : T.  
                   (e2 (e1 x)) = x

  Claim 3. For all y : S.
                   (e1 (e2 y)) <= y    <----- this ordering is over terms, not types,
                                              and it corresonds to "term evolution".

= Record type evolution and "the bridge algorithm"

Suppose that S1 evolves to S2.

Let's consider the case where S1 and S2 are record types that define
the input and output records of a canister's interface.  As we define
below, S1 and S2 can relate by:

 - S2 adds an option field to S1 (a form of "width" evolution).
 - S2 reserves an additional field compared with S1 (a form of "width" evolution).
 - S2 evolves the type of an existing field in S1 (a form of "depth" evolution).

To define the execution semantics of these accomodations, we can
extend the binary relation to generate the "bridge code" by adding and
removing these optional fields, or otherwise massaging the message.

Each case of bridge code is straightforward and consists of creating a
pair of {proglang} terms of the correct types.  In practice, the
effect of these terms can be performed on the fly given a runtime
representation of the two message types, which are available in our
current IDL design.

Hence, each canister's runtime can contain a single "bridge algorithm"
that is fixed for all time, that works for all current and future
message types.  Below, we discuss the higher order case of bridging
types across canister evolution.

= Higher order type evolution

As one piece of suggestive evidence that subtyping is wrong here:
Subtyping says that the arrow connective is contravariant in the first
(argument) type component.

This contravariance is wrong since conflating time and subtyping
orderings leads to confusing "old" and "new" times when data changes
polarity, as it does in the higher order case.

The same problem does not arise here, and we can evolve the following
interface easily:

time 1:
     pump : state1 -> state1

time 2: 
     pump : state2 -> state2

time 3:
     pump : state3 -> state3

If we have that these states are related by evolution:

   state1 $> state2 $> state3

We want that old clients can use the new interface without having to
use the new message format; hence, we want that these arrow types are
related thusly:

    state1 -> state1
   $>
    state2 -> state2
   $>
    state3 -> state3

Let us think in terms of time: The old type is related to the
new type when its parts are related in the same way, in the same
order:

S1 $> S2
T1 $> T2
--------------------- :: arrow
S1 -> T1 $> S2 -> T2

Since the types T1 and S1 may be equal (the third point of the design
triangle that Joachim mentioned in Zurich), this also permits the
following relation:

  (S1 -> S1)  $>  (S2 -> S2)  $>  (S3 -> S3)


