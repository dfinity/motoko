/**
Internal Data Model
===============================

Here, we depart from defining the PES, and turn our attention to the
internal representation of the actor's state.  The behavior of this
actor is part of the PES, but the internal data model representations
it uses are not.

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
(defined in `types.as`, in the formal PES).  We associate information, such as
textual names and descriptions, where appropriate.  We include other
fields from the PES, such as "units", "grades", "dates" and time
intervals (start/end dates), each where appropriate.

To understand how this forest is rooted, see the private variables
defined by the actor (in `actor.as`):

```
  private var trucktypes : TruckTypesTable = null;
  private var produce    : ProduceTable = null;
  private var regions    : RegionTable = null;

  private var producers    : ProducerTable = null;
  private var transporters : TransporterTable = null;
  private var retailers    : RetailerTable = null;
```

The first three tables are set up by the central authority when the Dapp
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

type TruckType = {
  id : TruckTypeId;
  short_name : Text;
  description : Text;
  capacity : TruckCapacity;
  // xxx variant type for this temperature-control information:
  isFridge : Bool;
  isFreezer : Bool;
};

type TruckTypeTable = Map<TruckTypeId, TruckType>;

type Region = {
  id : RegionId;
  short_name : Text;
  description : Text;
};

type RegionTable = Map<RegionId, Region>;

type Produce = {
  id : ProduceId;
  short_name : Text;
  description : Text;
  grade : Grade;
};

type ProduceTable = Map<ProduceId, Produce>;

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

type ByRegionInventoryTable = Map<RegionId, Map<ProducerId, InventoryTable>>;

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
  transporter : Transporter;
  truck_type : TruckType;
  start_region : Region;
  end_region : Region;
  start_date : Date;
  end_date : Date;
  // ... more?
};

type RouteTable = Map<RouteId, Route>;

// A possibly-sparse 3D table mapping each region-region-routeid triple to zero or one routes.
type ByRegionsRouteTable = Map<RegionId, Map<RegionId, RouteTable>>;

type ReservedRoute = {
  id : ReservationId;
  retailer : Retailer;
  route : Route;
};

type ReservedRouteTable = Map<ReservationId, ReservedRoute>;


////////////////////////////////////////////////////////////////////////////////////////////////////
class Model() = this {

  var nextTruckTypeId : TruckTypeId = 0;
  var nextRegionId : RegionId = 0;
  var nextProduceId : ProduceId = 0;
  var nextProducerId : ProducerId = 0;
  var nextRetailerId : RetailerId = 0;
  var nextInventoryId : InventoryId = 0;
  var nextTransporterId : TransporterId = 0;
  var nextxRouteId : RouteId = 0;
  var nextReservationId : ReservationId = 0;

  /**

   Internal data model, in terms of the collection types defined above
   ====================================================================

   First, see the types defined in `model.as`.

  // Mostly-static collections:
  // ---------------------------
  // Represents the following tables:
  //  - regions table
  //  - produce table
  //  - truck type table
  */

  private var regions : RegionTable = null;
  private var produce : ProduceTable = null;
  private var truckTypes : TruckTypeTable = null;

  /**
  // Producers collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - producer table
  //  - inventory table
  //  - reservedInventory table (formerly "orderedInventory" table)
  */
  private var producers : ProducerTable = null;


  /**
  // Transporters collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - transporter table
  //  - route table
  //  - reservedRoute table (formerly "orderedRoutes" table)
  */
  private var transporters : TransporterTable = null;

  /**
  // Retailers collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - retailer table
  //  - reservation table (formerly "orders" table)
  */
  private var retailers : RetailerTable = null;


  // xxx for efficient queries, need some extra indexing:
  //
  // Regions as keys in special global maps
  // ---------------------------------------
  // - inventory (across all producers) keyed by producer region
  // - routes (across all transporters) keyed by source region
  // - routes (across all transporters) keyed by destination region
  //
  // Indexing by time
  // -----------------
  // For now, we won't try to index based on days.
  //
  // If and when we want to do so, we would like to have a spatial
  // data structure that knows about each object's "interval" in a
  // single shared dimension (in time):
  //
  // - inventory, by availability window (start day, end day)
  // - routes, by transport window (departure day, arrival day)
  //

  // Routes by region-region pair
  // ----------------------------
  //
  // the actor maintains a possibly-sparse 3D table mapping each
  // region-region-routeid triple to zero or one routes.  First index
  // is destination region, second index is source region; this 2D
  // spatial coordinate gives all routes that go to that destination
  // from that source, keyed by their unique route ID, the third
  // coordinate of the mapping.
  private var routesByDstSrcRegions : ByRegionsRouteTable = null;

  // Inventory by source region
  // ----------------------------
  //
  // the actor maintains a possibly-sparse 3D table mapping each
  // sourceregion-producerid-inventoryid triple to zero or one
  // inventory items.  The 1D coordinate sourceregion gives all of the
  // inventory items, by producer id, for this source region.
  //
  private var inventorybyRegion : ByRegionInventoryTable = null;

  private unwrap<T>(ox:?T) : T {
    switch ox {
    case (null) { assert false ; unwrap<T>(ox) };
    case (?x) x;
    }
  };

};
