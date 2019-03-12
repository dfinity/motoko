/**

Matt-Says: Let's use markdown in the longer comments, in anticipation
of a documentation tool for converting ActorScript into Markdown
files.  I'll use an extra `*` in the opening comment when I expect the
comment to be processed as markdown.


Produce Exchange Dapp
=====================

 Start here:
 - Detailed examples: https://dfinity.atlassian.net/wiki/x/joXUBg
 - More background: https://dfinity.atlassian.net/wiki/x/4gg5Bg


Open Questions:
-------------------------------------------------

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

*/


/**

Produce Exchange Standards (PES)
=================================

The Produce Exchange is a DFINITY canister whose implementation
defines a set of _standards_ that to which we refer to collectively as
the Produce Exchange Standards, or PES.

PES, defined formally:
-----------------------

This file makes the PES formal, to the same degree that ActorScript
has a formal semantics of its own, in terms of DFINITY's semantics,
etc.

Some parts of this file define ActorScript data types that are
included in the PES.  These will appear in the messages to and from
the produce exchange.  The actor class itself, as the interface for
the PE service, is also part of the formal PES.  The _behavior_ of
this actor's implementation defines the _semantic_ aspects of the PES
standard.

Outside the PES, are the implementation details of this actor, which
are also present in this file.  By virtue of the actor interface
boundary, the implementation details of this actor behavior are
subject to change over time, independently of the standard's own
evolution. We include them here because their behavior is needed to
define the PES, as explained above.


PES evolution via canister upgrade
-----------------------------------

The PES evolves according to the "central authority" (cf PE spec
document), who we identify as the github repo and open source
developer community that surrounds this implementation file.

Updating the types in the PES requires changing this file, and
performing a canister upgrade on the running system.  Similarly, to
evolve the behavioral definition of PES, the implementation of this
actor will change, and will require a canister upgrade.

*/

/**
// PES: Define types for PES
// -----------------------------
// These types standardize representations for many common PES notions
*/

// import Date
// xxx Dates, eventually from a standard library:
type Date = Nat;

// xxx standard weight units?
type Weight = Nat;

// xxx standard price units?
type Price = Nat;

type Unit = Nat; // xxx replace with a variant type
type Grade = Nat; // xxx replace with a variant type

type TruckKind = Nat; // ??? replace with a variant type

type TruckCapacity = Weight;

type Quantity = Nat;

type PricePerUnit = Price; // needed to calculate prices
type PriceTotal = Price;

type WeightPerUnit = Weight; // needed to meet truck constraints

type RegionId = Nat; // xxx variant type?

/**
//
// PES: Unique Ids
// -----------------
// Externally, these Ids give a unique identifier that is unique to its type, but not universally unique.
// Internally, each type of Id serves as a "row key" for a table (or two).
//
*/

type ProduceId     = Nat;
type ProducerId    = Nat;
type RetailerId    = Nat;
type TruckKindId   = Nat;
type InventoryId   = Nat;
type TransporterId = Nat;
type RouteId       = Nat;
type OrderId       = Nat;


/**
//
// PES: Query parameters and results
// ----------------------------------
// Externally, these types define the input and output structures for PES queries.
// Internally, producing instances of the result structures will require
// performing joins based on internal tables, and the information from the input structures.
*/

type OrderInfo = shared {
  produce:     ProduceId;
  producer:    ProducerId;
  quant:       Quantity;
  ppu:         PricePerUnit;
  transporter: TransporterId;
  truck_kind:  TruckKindId;
  weight:      Weight;
  region_begin:RegionId;
  region_end:  RegionId;
  date_begin:  Date;
  date_end:    Date;
  prod_cost:   PriceTotal;
  trans_cost:  PriceTotal;
};

// xxx same as an OrderInfo?  If different, then how?
type QueryAllResult = shared {
  produce:     ProduceId;
  producer:    ProducerId;
  quant:       Quantity;
  ppu:         PricePerUnit;
  transporter: TransporterId;
  truck_kind:  TruckKindId;
  weight:      Weight;
  region_begin:RegionId;
  region_end:  RegionId;
  date_begin:  Date;
  date_end:    Date;
  prod_cost:   PriceTotal;
  trans_cost:  PriceTotal;
};

// xxx how to represent huge result messages?

type QueryAllResults = [QueryAllResult];


/**
Internal Data Model
===============================

Above, we define types for the PES.

Here, we depart from defining the PES, and turn our attention to the
internal representation of the actor's state.  As explained above, the
behavior of this actor is part of the PES, but the internal data model
representations it uses are not.

Hence, we have the freedom to model the system to best match the kinds
of updates and queries that we must handle now, and anticipate
handling in the future.

We employ a purely-functional repesentation based on tree-shaped data
structures with sharing.  Below, we explain our representation of
traditional SQL tables in terms of nested structures and finite maps.


Finite Maps
--------------

Finite maps, or just "maps", will serve as the cornerstone of our
representations below.  The ActorScript standard library implements
functional association lists (modules `List` and `AssocList`) and
functional hash tries (module `Trie`), whose representation uses those
lists, for its "buckets".

These map representations could change and expand in the future, so we
introduce the name `Map` here to abstract over the representation
choice between (for now) using association lists and tries.

Aside: Eventually, we'll likely have a more optimized trie that uses
small arrays in its leaf nodes.  The current representation is simple,
uses lots of pointers, and is likely not the optimal candidate for
efficient Wasm.  However, its asymptotic behavior is good, and it thus
provides a good approximation of the eventual design that we want.

*/

//type Map = AssocList;
//let Map = AssocList;

type Map<K,V> = Trie<K,V>;
let Map = Trie;

/**
Nested structures
-----------------

Below, we define top-level structures for representing each Producer,
Retailer and Transporter's officially published state within the PES.

Formally, these types define the types of forests (a set of trees with many
roots) that constitute our internal data model.

For each kind of structure below, we assume a type of unique Id
(defined above, in the formal PES).  We associate information, such as
textual names and descriptions, where appropriate.  We include other
fields from the PES, such as "units", "grades", "dates" and time
intervals (start/end dates), each where appropriate.

To understand how this forest is rooted, see the private variables
defined by the actor:

```
  private var produce : ProduceTable = null;
  private var regions : RegionTable = null;

  private var producers    : ProducerTable = null;
  private var transporters : TransporterTable = null;
  private var retailers    : RetailerTable = null;
```

The first two tables are set up by the central authority when the Dapp
launches, and change seldomly.

The next three tables contain the interesting state of the system.
They give the three sets of roots into the structures defined below.

Query implementation
---------------------

The retailers perform queries by joining information across the
producers and transporters tables, and their inventory and route
information, respectively.

Orders (Reservations) implementation
-------------------------------------

We refer to orders placed by retailrs here as "reservations", since
the latter word is less ambiguous.

To simplify query implementation over reservations, and to improve
this query response time, we store reservations in two places, with
internal sharing:

 - The currently-reserved routes and inventory are stored with their
   transporters and producers, respectively.

 - The currently-reserved routes and inventory of each retailer are
   additionally stored with this retailer.

*/

// Using "reserve", "reserved" and "reservation" in place of "order"
// below, since "order" and "ordering" have too many meanings.
type ReservationId = OrderId;

type Produce = {
  id : ProduceId;
  short_name : Text;
  description : Text;
  grade : Grade;
};

type ProduceTable = Map<ProduceId, Produce>;

type Region = {
  id : RegionId;
  short_name : Text;
  description : Text;
};

type RegionTable = Map<RegionId, Region>;

type Producer = {
  id : ProducerId;
  short_name : Text;
  description : Text;
  region : Region;
  inventory : InventoryTable;
  reserved : ReservedInventoryTable;
};

type ProducerTable = Map<ProducerId, Producer>;

type InventoryItem = {
  id : InventoryId;
  produce : Produce;
  producer : Producer;
  // ... more ..
  quantity : Quantity;
  start_date : Date;
  end_date : Date;
  comments : Text;
};

type InventoryTable = Map<InventoryId, InventoryItem>;

type ReservedInventoryItem = {
  id : ReservationId;
  retailer : Retailer;
  item : InventoryItem;
};

type ReservedInventoryTable = Map<ReservationId, ReservedInventoryItem>;

type Retailer = {
  id : RetailerId;
  short_name : Text;
  description : Text;
  region : Region;
  reserved_routes : ReservedRouteTable;
  reserved_items : ReservedInventoryTable;
};

type RetailerTable = Map<RetailerId, Retailer>;

type Transporter = {
  id : TransporterId;
  // no region; the transporters are the supply of routes, not "end
  // points" of any single route.
  name : Text;
  description : Text;
  route : RouteTable;
  reserved : ReservedRouteTable;
};

type TransporterTable = Map<TransporterId, Transporter>;

type Route = {
  id : RouteId;
  transporter : TransporterId;
  start_region : RegionId;
  end_region : RegionId;
  start_date : Date;
  end_date : Date;
  // ... more?
};

type RouteTable = Map<RouteId, Route>;

type ReservedRoute = {
  id : ReservationId;
  retailer : Retailer;
  route : Route;
};

type ReservedRouteTable = Map<ReservationId, ReservedRoute>;


// the "Service"
actor class ProduceExchange() {

  private var nextProduceId : ProduceId = 0;
  private var nextProducerId : ProducerId = 0;
  private var nextRetailerId : RetailerId = 0;
  private var nextTruckKindId : TruckKindId = 0;
  private var nextInventoryId : InventoryId = 0;
  private var nextTransporterId : TransporterId = 0;
  private var nextxRouteId : RouteId = 0;
  private var nextOrderId : OrderId = 0;

  // Mostly-static collections:
  // ---------------------------
  // Represents the following tables:
  //  - producers table
  //  - inventory table
  private var produce : ProduceTable = null;
  private var regions : RegionTable = null;

  // Producers collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - producers table
  //  - inventory table
  //  - orderedInventory table
  private var producers : ProducerTable = null;


  // Transporters collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - transporters table
  //  - route table
  //  - orderedRoutes table
  private var transporters : TransporterTable = null;

  // Retailers collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - retailers table
  //  - orders table
  private var retailers : RetailerTable = null;

  // Producer-based ingress messages:
  // ================================

  producerAddInventory(
    id:   ProducerId,
    prod: ProduceId,
    quant:Quantity,
    ppu:  PricePerUnit,
    begin:Date,
    end:  Date,
  ) : async ?InventoryId {
    // xxx
    null
  };

  producerRemInventory(id:InventoryId) : async ?() {
    // xxx
    null
  };

  producerOrders(id:ProducerId) : async ?[OrderId] {
    // xxx
    null
  };

  // Transporter-based ingress messages:
  // ===================================

  transporterAddRoute(
    trans:  TransporterId,
    rstart: RegionId,
    rend:   RegionId,
    start:  Date,
    end:    Date,
    cost:   Price,
    tt:     TruckKindId
  ) : async ?RouteId {
    // xxx
    null
  };

  transporterRemRoute(id:RouteId) : async ?() {
    // xxx
    null
  };

  transporterOrders(id:TransporterId) : async ?[OrderId] {
    // xxx
    null
  };

  // Retailer-based ingress messages:
  // ===================================

  retailerQueryAll(id:RetailerId) : async ?QueryAllResults {
    // xxx
    null
  };

  retailerPlaceOrder(
    id:RetailerId,
    inventory:InventoryId,
    route:RouteId) : async ?OrderId
  {
    // xxx
    null
  };

  retailerOrders(id:RetailerId) : async ?[OrderId] {
    // xxx
    null
  };

  // (Producer/Transporter/Retailer) ingress messages:
  // ========================================================

  orderInfo(id:OrderId) : async ?OrderInfo {
    // xxx
    null
  };

};
