module {
/**

[Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
--------------------

Server Types
==================

This file defines structures that appear the server actor's messaging
interface.  They are part of the formal standards definition.

*/


/**
Basic types
---------------------
These types standardize representations for many common notions

*/

// import Date
// xxx Dates, eventually from a standard library:
public type Date = Nat;

// xxx standard weight units?
public type Weight = Nat;

// xxx standard price units?
public type Price = Nat;

public type Unit = Nat; // xxx replace with a variant type
public type Grade = Nat; // xxx replace with a variant type

public type TruckCapacity = Weight;

public type Quantity = Nat;

public type PricePerUnit = Price; // needed to calculate prices
public type PriceTotal = Price;

/**
User Names
-----------------

Each user of the exchange chooses a unique _user name_, represented as `Text`.

In response to this choice, the exchange assigns the user a unique Id (see below); it maintains a mapping from this user name to the (internally-chosen) user Id.
*/

public type UserName  = Text;

/**
Unique Ids
-----------------

The produce exchange uses unique ids to concisely identify entities in the system.  Each id is a number.

Externally, these Ids give a unique identifier that is unique to its type, but not universally unique.

Internally, each type of Id serves as a "row key" for a table (or two).

*/

public type UserId        = Nat;

public type PublicKey     = Text;

public type RegionId      = Nat;
public type TruckTypeId   = Nat;
public type ProduceId     = Nat;

public type ProducerId    = Nat;
public type InventoryId   = Nat;
public type ReservedInventoryId = Nat;
public type RetailerId    = Nat;
public type TransporterId = Nat;
public type RouteId       = Nat;
public type ReservedRouteId = Nat;

/**
 `EntId`
 ----------------
 An entity's ID, distinguished (by tag) from other kinds of entities with the same ID.

 The optional payload of an `#idErr` error message.
*/
public type EntId = {
  #user        : UserId ;
  #truckType   : TruckTypeId ;
  #region      : RegionId ;
  #produce     : ProduceId ;
  #producer    : ProducerId ;
  #retailer    : RetailerId ;
  #transporter : TransporterId ;
  #inventory   : InventoryId ;
  #route       : RouteId ;
};

/**
 Errors
 ===============
*/

/** Optional entity ID */
public type OpEntId = ?EntId;

/**
 `IdErr`
 ---------
 An ID error consists only of the `#idErr` case, which carries an optional ID.
*/
public type IdErr = {
  #idErr: OpEntId;
};

/**
 `ServerErr`
 ------------
 A Server error occurs when the client either has access control issues (e.g., `#publicKeyErr`) or provides invalid parameters (e.g., `#idErr`).
 */
public type ServerErr = {
  #idErr: OpEntId;
  #publicKeyErr;
};


/**
Public info associated with Ids
=====================================
*/

/**
`UserInfo`
-------------
*/

public type UserInfo = {
  id: UserId;
  public_key: Text;
  user_name: Text;
  description: Text;
  region: RegionId;
  producerId: ?ProducerId;
  transporterId: ?TransporterId;
  retailerId: ?RetailerId;
  isDeveloper: Bool;
};


/**
`TruckTypeInfo`
-----------------
*/
public type TruckTypeInfo = {
  id : TruckTypeId;
  short_name : Text;
  description : Text;
  capacity : TruckCapacity;
  // xxx variant type for this temperature-control information:
  isFridge : Bool;
  isFreezer : Bool;
};

/**
`RegionInfo`
-----------------
*/
public type RegionInfo = {
  id : RegionId;
  short_name : Text;
  description : Text;
};

/**
`ProduceInfo`
-----------------
*/

public type ProduceInfo = {
  id : ProduceId;
  short_name : Text;
  description : Text;
  grade : Grade;
};

/**
`ProducerInfo`
-----------------
*/

public type ProducerInfo = {
  id : ProducerId;
  public_key: Text;
  short_name : Text;
  description : Text;
  region : RegionId;
  inventory : [InventoryId];
  reserved : [ReservedInventoryId]
};

/**
`InventoryInfo`
-----------------
*/

public type InventoryInfo = {
  id : InventoryId;
  produce : ProduceId;
  producer : ProducerId;
  quantity : Quantity;
  weight : Weight;
  ppu : Price;
  start_date : Date;
  end_date : Date;
  comments : Text;
};

/**
`ReservedInventoryInfo`
-----------------------------
*/

public type ReservedInventoryInfo = {
  id : ReservedInventoryId;
  retailer : RetailerId;
  item : InventoryInfo;
};

/**
`ProduceMarketInfo`
-----------------
*/

// for now, this is the same as a ReservationInfo
public type ProduceMarketInfo = {
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

/**
`RetailerInfo`
-----------------
*/

public type RetailerInfo = {
  id : RetailerId;
  public_key: Text;
  short_name : Text;
  description : Text;
  region : RegionId;
};

/**
`TransporterInfo`
-----------------
*/

public type TransporterInfo = {
  id : TransporterId;
  public_key: Text;
  // no region; the transporters are the supply of routes, not "end
  // points" of any single route.
  short_name : Text;
  description : Text;
  routes : [RouteId];
  reserved : [ReservedRouteId]
};

/**
`RouteInfo`
-----------------
*/

public type RouteInfo = {
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
`ReservedRouteInfo`
-----------------
*/

public type ReservedRouteInfo = {
  id : ReservedRouteId;
  retailer : RetailerId;
  route : RouteInfo;
};

/**
`ReservationInfo`
-----------------
*/

public type ReservationInfo = {
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
  inventoryId: InventoryId;
  routeId:     RouteId;
};


/**

 Produce Exchange counts
 =========================

 Developer-level, counter-based information about the exchange, including counts of each kind of entity.

*/
public type ProduceExchangeCounts = {
  hash_bit_length : Nat;
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
  retailer_query_size_max : Nat;
  retailer_query_count : Nat;
  retailer_query_cost : Nat;
  retailer_join_count : Nat;
};


/**
//
// Query parameters and results
// ----------------------------------
// Externally, these types define the input and output structures for queries.
// Internally, producing instances of the result structures will require
// performing joins based on internal tables, and the information from the input structures.

// Note: We are using "reserve", "reserved" and "reservation" in place of "order"
// below, since "order" and "ordering" have too many meanings in a
// broader CS/programming/query context.

*/

// xxx same as a reservation structure; represents a possible reservation to make
public type QueryAllResult = ReservationInfo;

/**

xxx how to represent huge result messages?

2019.03.12 *** TODO-Cursors: Introduce the idea of "cursors", with
allocation, de-allocation and movement messages, as per discussion in
the 2019.03.12 Motoko Team meeting.

*/

public type QueryAllResults = [QueryAllResult];


/**
 Synthetic workloads
 =======================

 The server can generate synthetic workloads given a small set of parameters.
 */

public type WorkloadParams = {
  day_count:Nat;
  max_route_duration:Nat;
  producer_count:Nat;
  transporter_count:Nat;
  retailer_count:Nat;
  region_count:Nat;
};

}
