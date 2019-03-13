Produce Exchange Example
========================

The produce exchange (PE) is a canonical example Dapp, illustrating
the DFINITY Dapp design process on a realistic marketplace-like
application.

The concept began as a [two page Google drive
document](https://docs.google.com/document/d/1AxpcuFH-x_0ZSa32DfM_BCYnGxCS37ETPNWE4BXDNdo/edit)
giving the Dapp's functional specifications.

The design now evolves in three places:

 1. The SDK and ActorScript teams' documentation:  
    i. [The design document, under the SDK
     space](https://dfinity.atlassian.net/wiki/x/MwD2Bg).  
    ii. [The requirements document for the MVP
      Design](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements).  
    iii. [Documentation under the ActorScript space](https://dfinity.atlassian.net/wiki/spaces/AST/pages/104401122/Example+Dapp+Produce+Exchange).  

 2. [**This example folder** in the ActorScript Github repo](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange),
    which is implementing the Produce Exchange as a way to push the development of
 the ActorScript language, its standard library, and elsewhere, the
 ambient DFINITY system that runs ActorScript canisters.

Scripted uses of the Produce Exchange
----------------------------------------

- [x] [Simple setup-and-query script](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/test/simpleSetupAndQuery.as)
- [ ] Randomly-generated scripts


Server components
-----------------------

We decompose the Produce Exchange example Dapp into an _ActorScript-based_ implementation of a "**Server**" with the following definitional pieces:

 1. **Basic types**: See
    [`serverTypes.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverTypes.as).  
    Basic types used in messages, and published/stored internally in the server actor's state.

 2. **Server messages**: See
    [`serverActor.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverActor.as).  
    Defined by the server actor's public signature, which specifies the messages and message formats for each participant.

 3. **Server model types**: See
    [`serverModelTypes.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModelTypes.as).  
    Defines structures that internally implement the server actor, and which are _not_ present in its public-facing interface.

    These models use [collections from the standard library](https://github.com/dfinity-lab/actorscript/tree/master/stdlib) [(Jira Story)](https://dfinity.atlassian.net/browse/AST-31).

 4. **Server implementation**: See
    [`serverModel.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModel.as).  
    Defines the _behavioral (input-output-based) semantics_ of each message from item 2, by
    implementing the server's interface in terms of the _server model types_ defined in item 3.

Test suite components
----------------------------------

We decompose the **test suite** for the Produce Exchange into the following milestones and associated test components:

 5. **Server actor example uses**:
    Use the system on small examples, whose output can be shown in a demo, presentation, slide deck, etc.

    To do

 6. **Automated regression tests**:
    Generate (online or offline) random example uses, and record the current output; check future revisions against this output.

    To do

 7. **Performance models**:
    Generate (online or offline) random example uses, and record time and space usage of the Wasm VM across different work loads; plot this data, and generate human-readable reports about it.

    To do


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

  4. **test suite** for all of the above, using local execution and
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

    **2019.03.12 -- TODO-Cursors:** Introduce the idea of "cursors", with
    allocation, de-allocation and movement messages, as per discussion in
    the 2019.03.12 ActorScript Team meeting.

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


Produce Exchange Standards Specification (PESS)
==================================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ whose **formal specification** we refer to collectively as
the _"Produce Exchange Standards Specification"_, or _"PESS"_ for short.


The PESS, defined formally, in ActorScript:
-------------------------------------------

We break this definition into several files, listed above in the
[server components list](#server-components), and mentioned again
below.

These files make the PESS definition into a **formal definition**, to
the same degree that ActorScript has a formal semantics of its own, in
terms of DFINITY's semantics, etc.

**Files for the PESS definition**: The file `serverTypes.as` defines
ActorScript data types that are included in the PESS, and will appear
in the messages to and from the produce exchange server.  The server
actor class itself (see `serverActor.as`) gives the interface for the
PE service, is also part of the formal PESS.  The _behavior_ of this
actor's implementation defines the _semantic_ aspects of the PESS
standard.

**Non-PESS files**: The `serverModel.as` file defines types used to
implement the specification behavior given in `serverActor.as`; this
file is not part of the PESS.

**Server message formats**: The server actor defines an interface boundary that only uses types
from `serverTypes.as`, and none from `serverModel.as`.  The implementation details
of this latter file and its use in the actor behavior are both subject to
change over time, independently of the standards' own evolution.  We
include the full implementation details here because the associated
behavior is needed to define the semantics of the PESS, as explained
above.

**Design principle for PESS interface**: Whenever possible, we will
push the implementation of "business logic" into `serverModel.as`,
with the aspiration of `serverActor.as` being a minimal wrapper over
definitions in `serverModel.as`, and little to no logic of its own.


PESS evolution via canister upgrade
-----------------------------------

The PESS evolves according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation.

Updating the types in the PESS requires changing the file `serverTypes.as`
mentioned above, and performing a canister upgrade on the running
system.  Similarly, to evolve the behavioral definition of PESS, the
implementation of this actor will change (in `serverActor.as` and
`serverModel.as`), and will also require a canister upgrade.
