Produce Exchange Canister Component
=====================================

We give an example of ActorScript's role in DFINITY by implementing
the **Produce Exchange Canister Component** in ActorScript.

The **produce exchange** gives a realistic marketplace-like
application, and servers as our canonical example Dapp.

We use it here to illustrate ActorScript the language, the standard
library, and the associated the DFINITY Dapp design process.

Prior documentation
-------------------

The design of the Produce Exchange example Dapp began as a [two page
Google drive
document](https://docs.google.com/document/d/1AxpcuFH-x_0ZSa32DfM_BCYnGxCS37ETPNWE4BXDNdo/edit),
giving the Dapp's functional specifications.

Current design documentation:
-----------------------------

The documentation of this design now evolves in two places:

 1. The SDK and ActorScript teams' documentation:  
    i. Current [design document, under the SDK
     space](https://dfinity.atlassian.net/wiki/x/MwD2Bg).  
    ii. Current [requirements document for the MVP
      Design](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements).  
    iii. Early, older [documentation under the ActorScript space](https://dfinity.atlassian.net/wiki/spaces/AST/pages/104401122/Example+Dapp+Produce+Exchange).  

 2. [**This documentation and associated source
    code**](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/stdlib/examples/produce-exchange)
    
    This code is implementing the **Produce Exchange Canister component**,
    as a way to push the development of the ActorScript language, its
    standard library, and elsewhere, the ambient DFINITY system that
    runs ActorScript canisters.

--------------------------------------------------------------

Produce Exchange Standards Specification (PESS)
==================================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ whose **formal specification** we refer to collectively as
the _"Produce Exchange Standards Specification"_, or _"PESS"_ for short.


The PESS, defined formally, in ActorScript:
-------------------------------------------

We break this definition into several files, described below in detail as
[**server components**](#server-components).

As ActorScript-based documentation, this documentation makes the PESS
definition into a **formal definition**, to the same degree that
ActorScript has a formal semantics of its own, in terms of DFINITY's
semantics, etc.

**Components for the PESS definition**: 

- The [Server Types component](#server-types) defines ActorScript data
types that are included in the server messages.

- The [Server Actor component](#server-actor) gives the interface for
the service, and is the bulk of the formal PESS.

The _behavior_ of this actor's implementation defines the _semantic_
aspects of the PESS standard; the implementation details of this
behavior are not included in the PESS standard.  We include a
prototype specification of this behavior, which is subject to change.
See the [server model types](#server-model-types) and [server model
implementation](#server-model-implementation) for details.

**Server message formats**:

The server actor defines an interface boundary that only uses types
from the server types component (no model types, no collection types
from the standard library).

**Design principle for interface design**:

Whenever possible, we will push the implementation of **"business logic"**
into the **server _model_ components**, with the aspiration of the server
component itself being a minimal wrapper over model definitions, and
little to no logic of its own.

These models are based closely on the ActorScript **standard library**,
and basic functional programming design patterns, which we
demonstrate through this example.

The standard library provides programming abstractions for
_executable_ functional specifications that run on the DFINITY system.

Whenever possible, we push reusable patterns and structures from the
model components into the standard library, with the aspiration of the
model components themselves being minimal wrappers over the standard
library.  The latter gives the former a simple **mathematical
vocabulary**, based on **pure functional programming**, for the
specified Canister behavior.


Server components
==========================

We decompose the _Canister_ for the **Produce Exchange example Dapp**
into an _ActorScript-based_ implementation of a "**Server**" with the
following definitional pieces, listed below.

**Server types**
-----------------

Basic types used in messages, and published/stored internally in the server actor's state.

See [`serverTypes.md`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverTypes.md) for authoritative documentation.

See [`serverTypes.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverTypes.as) for the source code.

**Server actor**
----------------------

Defined by the server actor's public signature, which specifies the messages and message formats for each participant.

See [`serverActor.md`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md) for authoritative documentation.

See [`serverActor.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverActor.as) for the source code.


**Server model types**
------------------------

This component defines structures that the next component uses to implement the server actor; neither component is exposed by the actor's public-facing interface.

See [`serverModelTypes.md`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverModelTypes.md) for authoritative documentation.

See [`serverModelTypes.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModelTypes.as) for the source code.

These models use [collections from the standard library](https://github.com/dfinity-lab/actorscript/tree/master/stdlib)

- Maps via the [`Trie` module](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/trie.md).  
- Maps via the [`AssocList` module](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/assocList.md).  

**Server model implementation**
--------------------------------

The model implementation formally defines the _behavioral
(input-output-based) semantics_ of [each message type](#server-actor),
by implementing the server's interface in terms of the [_server model
types_](#server-model-types).

See  [`serverModel.md`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverModel.md) for authoritative documentation.

See [`serverModel.as`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModel.as) for the source code.

Note that while we define the **behavior for the PESS**, the
_implementation details of this component and [server model
types](#server-model-types) themselves are not in PESS, and are
subject to change independently of PESS.

**Aside:** This model implementation is highly formulaic.  In the
future, we could likely _derive_ such implementations (auto-generate
them) from a higher-level property and relation markup language
defined over, and targeting, the existing actorscript type system and
associated standard library patterns. (#fumola).


Test suite components
=========================

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

See below for [more thoughts about performance testing](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#performance-considerations).



----------------------------------------------------------------------------


[Produce Exchange Canister: MVP Requirements](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements)
=============================================

**User interaction and design**

The only thing established are five views for the MVP:

-    Sign up/Login/Logout page
-    Producer view - if user authenticated is a producer
-    Transporter view - if user authenticated is a transporter
-    Retailer view - if user authenticated is a retailer
-    Developer view - if user authenticated is a Developer

ALL USERS
---------------

> **Sign up**	User can add their name and role and receive a unique ID	

See these, provided by the [**registrar** role](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#pess-registrar-based-ingress-messages):
- [**registrarAddProducer**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#registraraddproducer)
- [**registrarAddTransporter**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#registraraddtransporter)
- [**registrarAddRetailer**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#registraraddretailer)
	
> **Log in**	A user can identify themselves from a dropdown and "log in"	

??? no op

> **Log out**	User can log out of the app	

??? no op
	
PRODUCER
-------------
> **Add/update inventory**	Producer updates the goods, prices in the inventory available on the exchange	
	
See [**produerAddInventory**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#produceraddinventory)
    
> **View inventory**	Producer can see their inventory	
	
See [**`producerAllInventoryInfo`**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#producerallinventoryinfo)
    
> **View past sales orders**	Producer can see sales orders they fulfilled in the past	

Note: We call them "reservations".

See [**producerReservations**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#producerreservations)
	
> **View "market price"**	Producer can see the last sales price for any good within any geographic area	
	
See [**`produceMarketInfo`**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#producermarketinfo)

TRANSPORTER
---------------------
> **Add/update routes**	Transporter updates the routes available on the exchange. Transporter can see their routes. Each route is composed of an origin zone, destination zone, pickup date, delivery date, cost.	

See [**transporterAddRoute**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#transporteraddroute)
	
> **View routes**	Transporter can see their routes. Each route is composed of an origin zone, destination zone, pickup date, delivery date, cost.	

See [**transporterAllRouteInfo**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#transporterallrouteinfo)

> **View past sales orders**	Transporter can see routes which were utilized in the past	
	
Note: We call them "reservations".

See [**transporterReservationInfo**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#transporterreservationinfo)

	
RETAILER
-------------------
> **Query inventory**	Retailer can query a good with a delivery date. The Exchange will return a list of goods (and prices) that can be delivered to that retailer's geography within that date. 	

See [**`retailerQueryDates`**]https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#retailerquerydates)
and
[**retailerQueryAll**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#retailerqueryall)

	
> **Place a sales order**	Retailer can place order for one or more of options presented by any query.	

Note: We call them "reservations".

See [**retailerReserve**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#retailerreserve)
and
[**retailerReserveCheapest**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#retailerreservecheapest)
	
> **View past sales orders**	Retailer can see sales orders they placed in the past	

Note: We call them "reservations".

See [**retailerReservations**](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverActor.md#retailerreservations)

	
	
EXCHANGE DAPP DEVELOPER
---------------------------

> **View GMV**	Developer can see aggregate sum of how many sales have been processed	

To do

> **View queries**	Developer can see how many aggregate queries have been made by all retailers	

To do

> **View sales orders**	Developer can see how many aggregate sales orders have been made by all retailers	

To do

> **View producers**	Developer can see how many producers in the system and how many goods each has	

To do

> **View transporters**	Developer can see how many producers in the system and how many goods each has	

To do

> ****View retailers**	Developer can see how many retailers in the system and how many queries and how many sales orders	
	


---------------------------------------------------------------------------------

Define "Done"
================================

Merge to `master` requirements:
--------------------------------

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

We have the following questions:

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

 6. [Canister upgrades](#canisterupgrades)

--------------------------------------------------------------------------------


Canister upgrades
====================

The PESS evolves according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation.

Updating the types in the PESS requires changing the file `serverTypes.as`
mentioned above, and performing a canister upgrade on the running
system.  Similarly, to evolve the behavioral definition of PESS, the
implementation of this actor will change (in `serverActor.as` and
`serverModel.as`), and will also require a canister upgrade.


---------------------------------------------------------------------------------------

