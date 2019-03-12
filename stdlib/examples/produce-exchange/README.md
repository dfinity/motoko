Produce Exchange Example
========================

The produce exchange (PE) is a canonical example Dapp, illustrating
the DFINITY Dapp design process on a realistic marketplace-like
application.  The PE concept began as a [two page Google drive
document](https://docs.google.com/document/d/1AxpcuFH-x_0ZSa32DfM_BCYnGxCS37ETPNWE4BXDNdo/edit)
giving its functional specifications.

The design of PE now evolves in three places:

 1. The SDK and ActorScript teams' documentation:  
    i. [The design document, under the SDK
     space](https://dfinity.atlassian.net/wiki/x/MwD2Bg).  
    ii. [The requirements document for the MVP
      Design](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)  
    iii. [Documentation under the ActorScript space](https://dfinity.atlassian.net/wiki/spaces/AST/pages/104401122/Example+Dapp+Produce+Exchange).

 2. [**This example folder** in the ActorScript Github repo](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange), 
    which is implementing the Produce Exchange as a way to push the development of
 the ActorScript language, its standard library, and elsewhere, the
 ambient DFINITY system that runs ActorScript canisters.

Scripted uses of the Produce Exchange
----------------------------------------

- [x] [Simple setup-and-query script](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/produceExchange.as)
- [ ] Randomly-generated scripts


Components in ActorScript
--------------------------

We decompose the ActorScript implementation of the Produce Exchange example Dapp into the following pieces:

 1. **Interface types**: See
    [`types.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/types.as).  
    Used in messages, and published/stored internally in the actor's
    state.
 
 2. **Model types**: See
    [`model.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/model.as).  
    Used internally to implement the actor, but not present in its
    interface.
    
    These models use collections from the standard library.
 
 3. **Message types**: See
    [`actor.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/actor.as).  
    Defined by the actor's public signature, which specifies the
    message formats for each participant.

 3. **Message implementations**: See
    [`actor.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/actor.as)
    and
    [`model.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/actor.as).  
    Defined by the actor's implementation for each message defined in
    item 3, in terms of the model collection types defined in item 2.



To do list
-----------

This example is a work in progress.  It will be finished (and merged
to `master`) when the following are in a stable state, and working
locally, as locally-running ActorScript.

**Requirements list**:  
  1. **the exchange’s interface definition**, as an ActorScript actor.

  2.  **the behavior of the exchange**, as that actor’s prototype
      implementation.

  3. **the internal data model representation**, based on the
     ActorScript language and standard library, used internally to the
     actor, to implement the specification behavior.

  4. **tests** of the above, using local execution and
     ActorScript-based test cases.

**Canister component**: The requirements listed above also correspond with
those of the *Canister component* in Milestone 2.0 of the [MVP
Requirements Spec, 1.ii
above](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements).

----------------------------------------------------------------------------

Open Questions:
================

As part of the to do list above, we have the following questions:

 1. Massive result messages: How do we represent and send these?

    - lazy lists? (seems "easy" from AS programmer perspective, but
      requires non-first-order data in the IDL)

    - list iterators? (almost as good as lazy lists, but requires
      references in the IDL, and complicates the GC story).

    - arrays? (expensive to build and send; can become way *too big*).


 2. For now, wan we assume that the canister is maintained by the
    central authority?

 3. "Conditional updates" -- important to Mack

 4. How complex can the queries become?  Can we do truck-sharing-based
    queries, where we query as a retailer that expects to share trucks
    across several producer orders (e.g., in the same origin region).
    Alternatively, can we order multiple items from a single producer to
    ship on a single truck route?  Presumably, we can?

 5. Define a query language?
    --- Not until ActorScript implements variant types.

----------------------------------------------------------------------------


Spec: Produce Exchange Standards (PES)
========================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ to which we refer to collectively as
the _"Produce Exchange Standards"_, or _"PES"_ for short.


The PES, defined formally, in ActorScript:
-------------------------------------------

We break this definition into several files, listed above in this
file, and below. These files make the PES definition into a **formal
definition**, to the same degree that ActorScript has a formal
semantics of its own, in terms of DFINITY's semantics, etc.

**PES files**: The file `types.as` defines ActorScript data types that are
included in the PES.  These will appear in the messages to and from
the produce exchange.  The actor class itself (see `actor.as`) gives
the interface for the PE service, is also part of the formal PES.  The
_behavior_ of this actor's implementation defines the _semantic_
aspects of the PES standard.

**Interface boundary**: The actor interface boundary only uses types
from `types.as`, and none from `model.as`; the implementation details
of this file and its use in the actor behavior are both subject to
change over time, independently of the standards' own evolution.  We
include the full implementation details here because the associated
behavior is needed to define the semantics of the PES, as explained
above.

**Non-PES files**: Additionally, the `model.as` file defines types
used to implement the specification behavior given in `actor.as`; this
file is not part of the PES.  The implementation details of this actor
lie outside the PES but are also present in the file `actor.as`, in
terms of types defined in `model.as`.  Whenever possible, we will push
the implementation of "business logic" into `model.as`, with the
aspiration of `actor.as` being a minimal wrapper over definitions in
`model.as`, and little to no logic of its own.


PES evolution via canister upgrade
-----------------------------------

The PES evolves according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation.

Updating the types in the PES requires changing the file `types.as`
mentioned above, and performing a canister upgrade on the running
system.  Similarly, to evolve the behavioral definition of PES, the
implementation of this actor will change (in `actor.as` and
`model.as`), and will also require a canister upgrade.
