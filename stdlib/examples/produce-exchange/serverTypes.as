/**

 [PESS Background](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#Produce-Exchange-Standards-Specification-PESS)
 --------------------

 Server Types
 ==================

 This file defines structures that appear the server actor's messaging
 interface.  They are part of the formal PESS definition.

*/


/**
// PESS: Basic types for PESS
// -----------------------------
// These types standardize representations for many common PESS notions
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

type TruckCapacity = Weight;

type Quantity = Nat;

type PricePerUnit = Price; // needed to calculate prices
type PriceTotal = Price;


/**
//
// PESS: Unique Ids
// -----------------
// Externally, these Ids give a unique identifier that is unique to its type, but not universally unique.
// Internally, each type of Id serves as a "row key" for a table (or two).
//
*/
type RegionId      = Nat;
type TruckTypeId   = Nat;
type ProduceId     = Nat;

type ProducerId    = Nat;
type InventoryId   = Nat;
type ReservedInventoryId = Nat;
type RetailerId    = Nat;
type TransporterId = Nat;
type RouteId       = Nat;
type ReservedRouteId = Nat;


/**
PESS: Public info associated with Ids
-------------------------------------
*/

type TruckTypeInfo = shared {
  id : TruckTypeId;
  short_name : Text;
  description : Text;
  capacity : TruckCapacity;
  // xxx variant type for this temperature-control information:
  isFridge : Bool;
  isFreezer : Bool;
};

type RegionInfo = shared {
  id : RegionId;
  short_name : Text;
  description : Text;
};

type ProduceInfo = shared {
  id : ProduceId;
  short_name : Text;
  description : Text;
  grade : Grade;
};

type ProducerInfo = shared {
  id : ProducerId;
  short_name : Text;
  description : Text;
  region : RegionId;
  inventory : [InventoryId];
  reserved : [ReservedInventoryId]
};

type ReservedInventoryInfo = shared {
  id : ReservedInventoryId;
  retailer : RetailerId;
  item : InventoryId;
};

type InventoryInfo = shared {
  id : InventoryId;
  produce : ProduceId;
  producer : ProducerId;
  quantity : Quantity;
  ppu : Price;
  start_date : Date;
  end_date : Date;
  comments : Text;
};

// for now, this is the same as a ReservationInfo
type ProduceMarketInfo = shared {
  produce:     ProduceId;
  producer:    ProducerId;
  quant:       Quantity;
  ppu:         PricePerUnit;
  transporter: TransporterId;
  truck_type:  TruckTypeId;
  weight:      Weight;
  region_begin:RegionId;
  region_end:  RegionId;
  date_begin:  Date;
  date_end:    Date;
  prod_cost:   PriceTotal;
  trans_cost:  PriceTotal;
};

type RetailerInfo = shared {
  id : RetailerId;
  short_name : Text;
  description : Text;
  region : RegionId;
};

type TransporterInfo = shared {
  id : TransporterId;
  // no region; the transporters are the supply of routes, not "end
  // points" of any single route.
  short_name : Text;
  description : Text;
  routes : [RouteId];
  reserved : [ReservedRouteId]
};

type ReservedRouteInfo = shared {
  id : ReservedRouteId;
  retailer : RetailerId;
  route : RouteId;
};

type RouteInfo = shared {
  id : RouteId;
  transporter : TransporterId;
  truck_type : TruckTypeInfo;
  start_region : RegionId;
  end_region : RegionId;
  start_date : Date;
  end_date : Date;
  cost : Price;
};

/**
//
// PESS: Query parameters and results
// ----------------------------------
// Externally, these types define the input and output structures for PESS queries.
// Internally, producing instances of the result structures will require
// performing joins based on internal tables, and the information from the input structures.

// Note: We are using "reserve", "reserved" and "reservation" in place of "order"
// below, since "order" and "ordering" have too many meanings in a
// broader CS/programming/query context.

*/

type ReservationInfo = shared {
  produce:     ProduceId;
  producer:    ProducerId;
  quant:       Quantity;
  ppu:         PricePerUnit;
  transporter: TransporterId;
  truck_type:  TruckTypeId;
  weight:      Weight;
  region_begin:RegionId;
  region_end:  RegionId;
  date_begin:  Date;
  date_end:    Date;
  prod_cost:   PriceTotal;
  trans_cost:  PriceTotal;
};

// xxx same as a reservation structure; represents a possible reservation to make
type QueryAllResult = ReservationInfo;

/**

xxx how to represent huge result messages?

2019.03.12 *** TODO-Cursors: Introduce the idea of "cursors", with
allocation, de-allocation and movement messages, as per discussion in
the 2019.03.12 ActorScript Team meeting.

*/

type QueryAllResults = [QueryAllResult];

/**

 Produce Exchange counts
 =========================

 Developer-level, counter-based information about the exchange, including counts of each kind of entity.

*/
type ProduceExchangeCounts = shared {
  truck_type_count : Nat;
  region_count : Nat;
  produce_count : Nat;
  inventory_count : Nat;
  reserved_inventory_count : Nat;
  producer_count : Nat;
  transporter_count : Nat;
  route_count : Nat;
  reserved_route_count : Nat;
  retailer_count : Nat;
  retailer_query_count : Nat;
}
