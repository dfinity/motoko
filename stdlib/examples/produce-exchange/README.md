Produce Exchange Canister Component
=====================================

We give an example of Motoko by implementing
the **Produce Exchange Canister** in Motoko.

The **produce exchange** gives a realistic marketplace-like
application, and serves as a canonical example DFINITY Dapp.

We use it here to illustrate Motoko the language, the standard
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

 1. The SDK and Motoko teams' documentation:  
    i. Current [design document, under the SDK
     space](https://dfinity.atlassian.net/wiki/x/MwD2Bg).  
    ii. Current [requirements document for the MVP
      Design](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements).  
    iii. Early, older [documentation under the Motoko space](https://dfinity.atlassian.net/wiki/spaces/AST/pages/104401122/Example+Dapp+Produce+Exchange).  

 2. [**This documentation and associated source
    code**](https://github.com/dfinity-lab/motoko/tree/stdlib-examples/stdlib/examples/produce-exchange)
    
    This code is implementing the **Produce Exchange Canister component**,
    as a way to push the development of the Motoko language, its
    standard library, and elsewhere, the ambient DFINITY system that
    runs Motoko canisters.

--------------------------------------------------------------

Produce Exchange Standards Specification
==================================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ whose **formal specification** we refer to collectively as
the _"Produce Exchange Standards Specification"_.


Organizational overview
----------------------------

We break the standards definition into several files, described below in detail as
[**server components**](#server-components).

**Server message formats**

As Motoko-based documentation, the embedded source code for these
components makes the standards definition into a **formal definition**, to
the same degree that Motoko has a formal semantics of its own, in
terms of DFINITY's semantics, etc:

- The [server types](#server-types) define `shared` data types for client and server messages.

- The [server actor](#server-actor) defines the server message interface for all clients.

**Server message behavior**

The _behavior_ of this server defines the _semantic_ aspects of the
standards definition.

The _implementation details_ of this behavior are not included in the
standards definition. We include a prototype specification of this behavior,
which is subject to change:

- The [server model types](#server-model-types) define the internal data model used by the server to support its behavior.

- The [server model implementation](#server-model-implementation) defines the server behavior for all clients.


Organizational design
---------------------------

To determine how each component evolves, we employ the following design philosophy.

**Server message formats**:

The server actor defines an interface boundary that only uses types
from the server types component (no model types, no collection types
from the standard library).

**Design principle for interface design**:

Whenever possible, we will push the implementation of **"business logic"**
into the **server _model_ components**, with the aspiration of the server
component itself being a minimal wrapper over model definitions, and
little to no logic of its own.

These models are based closely on the Motoko **standard library**,
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
into an _Motoko-based_ implementation of a "**Server**" with the
following definitional pieces, listed below.

**Server types**
-----------------

Basic types used in messages, and published/stored internally in the server actor's state.

See [`serverTypes.md`]($DOCURL/examples/produce-exchange/serverTypes.html) for authoritative documentation.

See [`serverTypes.mo`](https://github.com/dfinity-lab/motoko/blob/stdlib-examples/stdlib/examples/produce-exchange/serverTypes.mo) for the source code.

**Server actor**
----------------------

Defined by the server actor's public signature, which specifies the messages and message formats for each participant.

See [`serverActor.md`]($DOCURL/examples/produce-exchange/serverActor.html) for authoritative documentation.

See [`serverActor.mo`](https://github.com/dfinity-lab/motoko/blob/stdlib-examples/stdlib/examples/produce-exchange/serverActor.mo) for the source code.


**Server model types**
------------------------

This component defines structures that the next component uses to implement the server actor; neither component is exposed by the actor's public-facing interface.

See [`serverModelTypes.md`]($DOCURL/examples/produce-exchange/serverModelTypes.html) for authoritative documentation.

See [`serverModelTypes.mo`](https://github.com/dfinity-lab/motoko/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModelTypes.mo) for the source code.

**Standard library**
--------------------------

See [the standard library](https://github.com/dfinity-lab/motoko/tree/stdlib-examples/stdlib/#produce-exchange)
for collection abstractions,
including 
    the [`DocTable` class]($DOCURL/docTable.html)
and the [`Trie` type]($DOCURL/trie.html).

**Server model implementation**
--------------------------------

The model implementation formally defines the _behavioral
(input-output-based) semantics_ of [each message type](#server-actor),
by implementing the server's interface in terms of the [_server model
types_](#server-model-types).

See  [`serverModel.md`]($DOCURL/examples/produce-exchange/serverModel.html) for authoritative documentation.

See [`serverModel.mo`](https://github.com/dfinity-lab/motoko/blob/stdlib-examples/stdlib/examples/produce-exchange/serverModel.mo) for the source code.

Note that while we define the **behavior for the server**, the
_implementation details of this component and [server model
types](#server-model-types) themselves are not in definition, and are
subject to change independently of this definition.

**Aside:** This model implementation is highly formulaic.  In the
future, we could likely _derive_ such implementations (auto-generate
them) from a higher-level property and relation markup language
defined over, and targeting, the existing motoko type system and
associated standard library patterns.


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

See below for [more thoughts about performance testing]($DOCURL/stdlib/examples/produce-exchange#performance-considerations).



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

See these, provided by the [**registrar** role]($DOCURL/examples/produce-exchange/serverActor.md#registrar-based-ingress-messages):
- [**`registrarAddProducer`**]($DOCURL/examples/produce-exchange/serverActor.md#registraraddproducer)
- [**`registrarAddTransporter`**]($DOCURL/examples/produce-exchange/serverActor.md#registraraddtransporter)
- [**`registrarAddRetailer`**]($DOCURL/examples/produce-exchange/serverActor.md#registraraddretailer)
	
> **Log in**	A user can identify themselves from a dropdown and "log in"	

??? no op

> **Log out**	User can log out of the app	

??? no op
	
PRODUCER
-------------
> **Add/update inventory**	Producer updates the goods, prices in the inventory available on the exchange	
	
See [**`produerAddInventory`**]($DOCURL/examples/produce-exchange/serverActor.md#produceraddinventory)
and [**`produerRemInventory`**]($DOCURL/examples/produce-exchange/serverActor.md#producerreminventory)
    
> **View inventory**	Producer can see their inventory	
	
See [**`producerAllInventoryInfo`**]($DOCURL/examples/produce-exchange/serverActor.md#producerallinventoryinfo)
    
> **View past sales orders**	Producer can see sales orders they fulfilled in the past	

Note: We call them "reservations".

See [**`producerReservations`**]($DOCURL/examples/produce-exchange/serverActor.md#producerreservations)
	
> **View "market price"**	Producer can see the last sales price for any good within any geographic area	
	
See [**`produceMarketInfo`**]($DOCURL/examples/produce-exchange/serverActor.md#producemarketinfo)

TRANSPORTER
---------------------
> **Add/update routes**	Transporter updates the routes available on the exchange. Transporter can see their routes. Each route is composed of an origin zone, destination zone, pickup date, delivery date, cost.	

See [**`transporterAddRoute`**]($DOCURL/examples/produce-exchange/serverActor.md#transporteraddroute)
and [**`transporterRemRoute`**]($DOCURL/examples/produce-exchange/serverActor.md#transporterremroute)
	
> **View routes**	Transporter can see their routes. Each route is composed of an origin zone, destination zone, pickup date, delivery date, cost.	

See [**`transporterAllRouteInfo`**]($DOCURL/examples/produce-exchange/serverActor.md#transporterallrouteinfo)

> **View past sales orders**	Transporter can see routes which were utilized in the past	
	
Note: We call them "reservations".

See [**`transporterAllReservationInfo`**]($DOCURL/examples/produce-exchange/serverActor.md#transporterallreservationinfo)

	
RETAILER
-------------------
> **Query inventory**	Retailer can query a good with a delivery date. The Exchange will return a list of goods (and prices) that can be delivered to that retailer's geography within that date. 	

See [**`retailerQueryDates`**]($DOCURL/examples/produce-exchange/serverActor.md#retailerquerydates)
and
[**`retailerQueryAll`**]($DOCURL/examples/produce-exchange/serverActor.md#retailerqueryall)

	
> **Place a sales order**	Retailer can place order for one or more of options presented by any query.	

Note: We call them "reservations".

See [**`retailerReserve`**]($DOCURL/examples/produce-exchange/serverActor.md#retailerreserve)
and
[**`retailerReserveCheapest`**]($DOCURL/examples/produce-exchange/serverActor.md#retailerreservecheapest)
	
> **View past sales orders**	Retailer can see sales orders they placed in the past	

Note: We call them "reservations".

See [**`retailerReservations`**]($DOCURL/examples/produce-exchange/serverActor.md#retailerreservations)

	
	
EXCHANGE DAPP DEVELOPER
---------------------------

> **View GMV**	Developer can see aggregate sum of how many sales have been processed	

See [**`devViewGMV`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewgmv).

> **View queries**	Developer can see how many aggregate queries have been made by all retailers	

See [**`devViewQueries`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewqueries).

> **View sales orders**	Developer can see how many aggregate sales orders have been made by all retailers	

See [**`devViewReservations`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewreservations).

> **View producers**	Developer can see how many producers in the system and how many goods each has	

See [**`devViewProducers`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewproducers).

> **View transporters**	Developer can see how many producers in the system and how many goods each has	

See [**`devViewTransporters`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewtransporters).

> ****View retailers**	Developer can see how many retailers in the system and how many queries and how many sales orders

See [**`devViewRetailers`**]($DOCURL/examples/produce-exchange/serverActor.md#devviewretailers).
	


---------------------------------------------------------------------------------

Define "Done"
================================

See also: [Exit criteria](#exit-criteria)
----------------------------------------

Merge to `master` requirements:
--------------------------------

This example is a work in progress.  It will be finished (and merged
to `master`) when the following are in a stable state, and working
locally, as locally-running Motoko.

**Requirements list**:
  1. **the exchange’s interface definition**, as an Motoko actor.

  2.  **the behavior of the exchange**, as that actor’s prototype
      implementation.

  3. **the internal data model representation**, based on the
     Motoko language and standard library, used internally to the
     actor, to implement the specification behavior.

  4. **test suite** for all of the above, using local execution and
     Motoko-based test cases.

**Canister component**: The requirements listed above also correspond with
those of the *Canister component* in Milestone 2.0 of the [MVP
Requirements Spec, 1.ii
above](https://dfinity.atlassian.net/wiki/spaces/DE/pages/116654198/Produce+Exchange+MVP+Product+Requirements).


---------------------------------------------------------------------------------

Exit Criteria
=====================

Background definitions
---------------

Mack (via email correspondance):

**Exit criteria**:

>  What objective tests will tell us that we have successfully built
>  the right thing? as a part of the functional specification - can be
>  implicit as you get good at it, better to start out explicit with a
>  separate list at first.

Exit criteria and process:

> Before transitioning from functional specification to design,
> clearly document assumptions (max 1000 concurrent users, prices
> never exceed X, alpha not needed before Aug1, do not have to handle
> [most important - non-goals] ) and dependencies.  Review these and
> get broad buy-in before starting design in earnest.




["Exit criteria" on wikipedia](https://en.wikipedia.org/wiki/Exit_criteria):

> understanding goals clearly; using language (and data) carefully
> when talking about (or measuring) methods for getting things done;
> and taking a scientific approach towards evaluating and improving
> the methods that are used.

> set of test specifications are created to test this new product to
> ensure that it meets minimum acceptable operational
> specifications. This test specification will state the minimum
> criteria necessary for the testing process to be considered complete
> and the product is ready for release IE: Exit the testing phase of
> the program

[Rice Consultng Services](https://www.riceconsulting.com/public_pdf/Ins%20and%20Outs%20of%20Entry%20and%20Exit%20Criteria%20-%20ASTQB%20Webinar%20v2.pdf)

Using DEFECT METRICS AS ENTRY AND EXIT CRITERIA:
- _NUMBER OF DEFECTS OUTSTANDING BY STATUS_
- _PERCENTAGE OF TESTS THAT EVENTUALLY PASS_
- _NUMBER OF TESTS THAT CONTINUE TO FAIL_

Exit criteria examples
--------------------------

Concrete examples at DFINITY:

 - [M1 Exit Criteria](https://dfinity.atlassian.net/wiki/spaces/M1/pages/491712/Basenet+M1+Release+Exit+Criteria)


Exit criteria for this Canister
--------------------------------


**Dates and people** 

- [MVP features](#produce-exchange-canister-mvp-requirements) done on or before March 26 Motoko team meeting

- Full exit criteria met asap, before Motoko and/or SDK launch (?)

- Most items below are tasked to @Matthewhammer; some require support
  from other teams at DFINITY, including the Motoko and SDK
  teams.

**Feature-based metrics** 

- Performs all features defined by MVP design doc, in the sense that
  every use case has one or more server messages that facilitate the
  use case.
  
- Front-end interacts with this Canister via DFINITY; see operational
  metrics below for more.

**Test-based metrics** 

- Hand-scripted, automated tests on small numbers of entities.

- Automatically generate some test data to variable sizes, including
  **hundreds or thousands** each of the following: regions, retailers,
  transporters, producers, inventory, routes.  Produce and truck types
  will still be hand-coded.

- Run automated tests that populate the system (with 100s or 1000s of
  entities of each type), then simulate the retailers making random
  queries and reservations over this random exchange data.
  
- Record machine-readable logs of these simulations, e.g., for
  future data analysis, demo visualizations, and regression tests.

**Operational metrics** 

- We can run this Canister on a single Wasm VM, within a single
  DFINITY node, running on its own.

- No hard number requirements yet

**Performance metrics** 

- We can gather performance metrics about the Wasm VM, whatever they
  may be. 
  
- No hard number requirements yet; any initial numbers are okay here; the point is to have the
  ability to gather numbers, not perform at a certain operational
  level (yet).


**Extras 1: Maybe not included**

- Standard library used by exchange is fully documented

- Standard library used by exchange has unit tests of its own


**Extras 2: definitely not included**

- Standard library collections used by exchange have high-performance representations

- Grow automated test system into a random test-simulation
  system. Perform simple goal-directed simulations; e.g., run a
  mark place simulation in time, wherein entities interact through the
  exchange, in simple goal-directed ways (e.g., every retailer tries
  to buy all the produce that they can, but get each kind of produce
  at the cheapest costs; the producers and transporters use
  marketplace information or other constraints to choose prices,
  routes and inventory).


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
    the 2019.03.12 Motoko Team meeting.

 2. For now, can we assume that the canister is maintained by the
    central authority?

 3. "Conditional updates" -- important to Mack

 4. How complex can the queries become?  Can we do truck-sharing-based
    queries, where we query as a retailer that expects to share trucks
    across several producer orders (e.g., in the same origin region).
    Alternatively, can we order multiple items from a single producer to
    ship on a single truck route?  Presumably, we can?

 5. Define a query language?
    --- Not until Motoko implements variant types.

 6. [Canister upgrades](#canisterupgrades)

--------------------------------------------------------------------------------


Canister upgrades
====================

The standards evolve according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation.

Updating the types in the standards requires changing the file `serverTypes.mo`
mentioned above, and performing a canister upgrade on the running
system.  Similarly, to evolve the behavioral definition of standards, the
implementation of this actor will change (in `serverActor.mo` and
`serverModel.mo`), and will also require a canister upgrade.


---------------------------------------------------------------------------------------

