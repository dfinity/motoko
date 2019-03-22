/**

[PESS Background](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#Produce-Exchange-Standards-Specification-PESS)
--------------------
 
Server Model Types
==================

This file defines structures that implement the server actor's
internal model of the exchange.

They are _not_ present in the public-facing interface of the server;
they are only are used in its internal model implementation
`serverModel.as`.
    
*/


/**

Representation
=================

*/

/**
 Finite maps
 ------------

 See also: modules for [`Trie`]() 
 and [`DocTable`](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md).
 
*/

type Map<X, Y> = Trie<X, Y>;
let Map = Trie;


/**

[Document tables](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md)
--------------------------

Document tables abstract over the various finite map operations we
commonly need for each kind of entity in the exchange model.


Each table is a map from distinct ids to documents.  These tables, and
the documents that they manage, serve as the central abstraction in
the representation of the exchange.

Nested document structures
-----------------------------

Below, we define top-level **document structures** for representing each `Producer`,
`Retailer` and `Transporter`'s officially published state within the PESS.

Formally, these types define the types of forests (a set of trees with many
roots) that constitute our internal data model.

For each kind of structure below, we assume a type of unique Id.  

We associate document information, such as textual names and
descriptions, where appropriate.

We include other fields from the PESS, such as "units", "grades",
"dates" and time intervals (start/end dates), each where appropriate.

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

/**
 `TruckType` documents
 ==================

 - See also [`serverTypes`]() for `TypeTypeId` and `TypeTypeInfo`.
 - See also [`DocTable`](). 
 */

type TruckTypeDoc = {
  id : TruckTypeId;
  short_name : Text;
  description : Text;
  capacity : TruckCapacity;
  // xxx variant type for this temperature-control information:
  isFridge : Bool;
  isFreezer : Bool;
};

type TruckTypeTable = 
  DocTable<TruckTypeId, TruckTypeDoc, TruckTypeInfo>;

/**
 `Region` documents
 ==========================
 */

type RegionDoc = {
  id : RegionId;
  short_name : Text;
  description : Text;
};

type RegionTable = 
  DocTable<RegionId, RegionDoc, RegionInfo>;

/**
 `Produce` documents
 ==================
 */

type ProduceDoc = {
  id : ProduceId;
  short_name : Text;
  description : Text;
  grade : Grade;
};

type ProduceTable = 
  DocTable<ProduceId, ProduceDoc, ProduceInfo>;

/**
 `Producer` documents
 =======================
 */

type ProducerDoc = {
  id : ProducerId;
  short_name : Text;
  description : Text;
  region : RegionDoc;
  inventory : InventoryMap;
  reserved : ReservedInventoryMap;
};

type ProducerTable = 
  DocTable<ProducerId, ProducerDoc, ProducerInfo>;

/**
 `Inventory` documents
 ========================
 */

type InventoryDoc = {
  id : InventoryId;
  produce : ProduceDoc;
  producer : ProducerId;
  // ... more ..
  quantity : Quantity;
  start_date : Date;
  end_date : Date;
  comments : Text;
};

type InventoryTable = 
  DocTable<InventoryId, InventoryDoc, InventoryInfo>;

type InventoryMap = 
  Map<InventoryId, InventoryDoc>;

/**
 By-region inventory indexing
 -----------------------------
*/
type ByRegionInventoryMap = Map<RegionId, Map<ProducerId, InventoryMap>>;

type ReservedInventoryDoc= {
  id : ReservedInventoryId;
  retailer : RetailerId;
  item : InventoryDoc;
};

/**
 Reserved inventory indexing
 -----------------------------
*/

type ReservedInventoryTable = 
  DocTable<ReservedInventoryId, ReservedInventoryDoc, ReservedInventoryInfo>;

type ReservedInventoryMap = 
  Map<ReservedInventoryId, ReservedInventoryDoc>;

/**
 `Retailer` documents
 ==================
 */

type RetailerDoc = {
  id : RetailerId;
  short_name : Text;
  description : Text;
  region : RegionDoc;
  reserved : ReservedInventoryRouteMap;
};

type RetailerTable = 
  DocTable<RetailerId, RetailerDoc, RetailerInfo>;

type ReservedInventoryRouteMap = 
  Map<ReservedInventoryId, (ReservedInventoryDoc, ReservedRouteDoc)>;

/**
 `Transporter` documents
 ==================
 */

type TransporterDoc = {
  id : TransporterId;
  // no region; the transporters are the supply of routes, not "end
  // points" of any single route.
  short_name : Text;
  description : Text;
  routes : RouteMap;
  reserved : ReservedRouteMap;
};

type TransporterTable = 
  DocTable<TransporterId, TransporterDoc, TransporterInfo>;

/**
 `Route` documents
 ==================
 */

type RouteDoc = {
  id : RouteId;
  transporter : TransporterId;
  truck_type : TruckTypeDoc;
  start_region : RegionDoc;
  end_region : RegionDoc;
  start_date : Date;
  end_date : Date;
  cost : Price;
  // ... more?
};

type RouteTable = 
  DocTable<RouteId, RouteDoc, RouteInfo>;


type RouteMap = 
  Map<RouteId, RouteDoc>;

/**
 By-region inventory indexing
 -----------------------------
*/

// A possibly-sparse 3D table mapping each region-region-routeid triple to zero or one routes.
type ByRegionsRouteMap = Map<RegionId, Map<RegionId, RouteMap>>;

/**
 Reserved inventory indexing
 -----------------------------
*/

type ReservedRouteDoc = {
  id : ReservedRouteId;
  retailer : RetailerId;
  route : RouteDoc;
};

type ReservedRouteTable = DocTable<ReservedRouteId, ReservedRouteDoc, ReservedRouteInfo>;

type ReservedRouteMap = Map<ReservedRouteId, ReservedRouteDoc>;
