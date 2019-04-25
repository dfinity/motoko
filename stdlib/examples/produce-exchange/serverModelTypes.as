/**

[Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
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
 and [`DocTable`]($DOCURL/docTable.html).

*/

let T = (import "serverTypes.as");

let Trie = (import "../../trie.as");
type Trie<K,V> = Trie.Trie<K,V>;

type Map<X, Y> = Trie<X, Y>;
let Map = Trie;


/**

[Document tables]($DOCURL/docTable.html)
--------------------------

Document tables abstract over the various finite map operations we
commonly need for each kind of entity in the exchange model.


Each table is a map from distinct ids to documents.  These tables, and
the documents that they manage, serve as the central abstraction in
the representation of the exchange.

Nested document structures
-----------------------------

Below, we define top-level **document structures** for representing each `Producer`,
`Retailer` and `Transporter`'s officially published state within the exchange.

Formally, these types define the types of forests (a set of trees with many
roots) that constitute our internal data model.

For each kind of structure below, we assume a type of unique Id.

We associate document information, such as textual names and
descriptions, where appropriate.

We include other fields such as "units", "grades",
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

let DocTable = (import "../../docTable.as");
type DocTable<X,Y,Z> = DocTable.DocTable<X,Y,Z>;

/**
`UserDoc`
-------------
User documents.
*/

type UserDoc = {
  id: T.UserId;
  user_name: Text;
  public_key: Text;
  description: Text;
  region: T.RegionId;
  producerId: ?T.ProducerId;
  transporterId: ?T.TransporterId;
  retailerId: ?T.RetailerId;
  isDeveloper: Bool;
};

type UserTable =
  DocTable<T.UserId, UserDoc, T.UserInfo>;

type UserNameMap =
  Map<T.UserName, T.UserId>;

/**
 `TruckType` documents
 ==================

 - See also [`serverTypes`]() for `TypeTypeId` and `TypeTypeInfo`.
 - See also [`DocTable`]().
 */

type TruckTypeDoc = {
  id : T.TruckTypeId;
  short_name : Text;
  description : Text;
  capacity : T.TruckCapacity;
  // xxx variant type for this temperature-control information:
  isFridge : Bool;
  isFreezer : Bool;
};

type TruckTypeTable =
  DocTable<T.TruckTypeId, TruckTypeDoc, T.TruckTypeInfo>;

/**
 `Region` documents
 ==========================
 */

type RegionDoc = {
  id : T.RegionId;
  short_name : Text;
  description : Text;
};

type RegionTable =
  DocTable<T.RegionId, RegionDoc, T.RegionInfo>;

/**
 `Produce` documents
 ==================
 */

type ProduceDoc = {
  id : T.ProduceId;
  short_name : Text;
  description : Text;
  grade : T.Grade;
};

type ProduceTable =
  DocTable<T.ProduceId, ProduceDoc, T.ProduceInfo>;

/**
 `Producer` documents
 =======================
 */

type ProducerDoc = {
  id : T.ProducerId;
  short_name : Text;
  description : Text;
  region : RegionDoc;
  inventory : InventoryMap;
  reserved : ReservedInventoryMap;
};

type ProducerTable =
  DocTable<T.ProducerId, ProducerDoc, T.ProducerInfo>;

/**
 `Inventory` documents
 ========================
 */

type InventoryDoc = {
  id : T.InventoryId;
  produce : ProduceDoc;
  producer : T.ProducerId;
  ppu : T.Price;
  quantity : T.Quantity;
  weight : T.Weight;
  start_date : T.Date;
  end_date : T.Date;
  comments : Text;
};

type InventoryTable =
  DocTable<T.InventoryId, InventoryDoc, T.InventoryInfo>;

type InventoryMap =
  Map<T.InventoryId, InventoryDoc>;

type ByProducerInventoryMap =
  Map<T.ProducerId, Map<T.InventoryId, InventoryDoc>>;

/**
 By-region inventory indexing
 -----------------------------
*/
type ByRegionInventoryMap = Map<T.RegionId, ByProducerInventoryMap>;

/**
 `ReservedInventory` documents
 ==================================
*/

type ReservedInventoryDoc= {
  id : T.ReservedInventoryId;
  retailer : T.RetailerId;
  item : InventoryDoc;
};

/**
 Reserved inventory indexing
 -----------------------------
*/

type ReservedInventoryTable =
  DocTable<T.ReservedInventoryId, ReservedInventoryDoc, T.ReservedInventoryInfo>;

type ReservedInventoryMap =
  Map<T.ReservedInventoryId, ReservedInventoryDoc>;

/**
 `Retailer` documents
 ==================
 */

type RetailerDoc = {
  id : T.RetailerId;
  short_name : Text;
  description : Text;
  region : RegionDoc;
  reserved : ReservedInventoryRouteMap;
};

type RetailerTable =
  DocTable<T.RetailerId, RetailerDoc, T.RetailerInfo>;

type ReservedInventoryRouteMap =
  Map<T.ReservedInventoryId, (ReservedInventoryDoc, ReservedRouteDoc)>;

type ByProduceByRegionInventoryReservationMap =
  Map<T.ProduceId, Map<T.RegionId, Map<T.ReservedInventoryId, ReservedInventoryDoc>>>;

/**
 `Transporter` documents
 ==================
 */

type TransporterDoc = {
  id : T.TransporterId;
  // no region; the transporters are the supply of routes, not "end
  // points" of any single route.
  short_name : Text;
  description : Text;
  routes : RouteMap;
  reserved : ReservedRouteMap;
};

type TransporterTable =
  DocTable<T.TransporterId, TransporterDoc, T.TransporterInfo>;

/**
 `Route` documents
 ==================
 */

type RouteDoc = {
  id : T.RouteId;
  transporter : T.TransporterId;
  truck_type : TruckTypeDoc;
  start_region : RegionDoc;
  end_region : RegionDoc;
  start_date : T.Date;
  end_date : T.Date;
  cost : T.Price;
  // ... more?
};

type RouteTable =
  DocTable<T.RouteId, RouteDoc, T.RouteInfo>;


type RouteMap =
  Map<T.RouteId, RouteDoc>;

/**
 By-region inventory indexing
 -----------------------------
*/

// A possibly-sparse 2D table mapping each region-routeid pair to zero or one routes.
type ByRegionRouteMap = Map<T.RegionId, RouteMap>;

// A possibly-sparse 3D table mapping each region-region-routeid triple to zero or one routes.
type ByRegionPairRouteMap = Map<T.RegionId, ByRegionRouteMap>;


/**
 Reserved inventory indexing
 -----------------------------
*/

type ReservedRouteDoc = {
  id : T.ReservedRouteId;
  retailer : T.RetailerId;
  route : RouteDoc;
};

type ReservedRouteTable = DocTable<T.ReservedRouteId, ReservedRouteDoc, T.ReservedRouteInfo>;

type ReservedRouteMap = Map<T.ReservedRouteId, ReservedRouteDoc>;
