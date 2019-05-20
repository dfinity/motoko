/**

[Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
--------------------

Server Types
==================

This file defines structures that appear the server actor's messaging
interface.  They are part of the formal standards definition.

*/

module {

  /**
  Basic types
  ---------------------
  These types standardize representations for many common notions

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
  User Names
  -----------------

  Each user of the exchange chooses a unique _user name_, represented as `Text`.

  In response to this choice, the exchange assigns the user a unique Id (see below); it maintains a mapping from this user name to the (internally-chosen) user Id.
  */

  type UserName  = Text;

  /**
  Unique Ids
  -----------------

  The produce exchange uses unique ids to concisely identify entities in the system.  Each id is a number.

  Externally, these Ids give a unique identifier that is unique to its type, but not universally unique.

  Internally, each type of Id serves as a "row key" for a table (or two).

  */

  type UserId        = Nat;

  type PublicKey     = Text;

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
   `EntId`
   ----------------
   An entity's ID, distinguished (by tag) from other kinds of entities with the same ID.

   The optional payload of an `#idErr` error message.
  */
  type EntId = {
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
  type OpEntId = ?EntId;

  /**
   `IdErr`
   ---------
   An ID error consists only of the `#idErr` case, which carries an optional ID.
  */
  type IdErr = {
    #idErr: OpEntId;
  };


  /**
   `ServerErr`
   ------------
   A Server error occurs when the client either has access control issues (e.g., `#publicKeyErr`) or provides invalid parameters (e.g., `#idErr`).
   */
  type ServerErr = {
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

  type UserInfo = shared {
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
  type TruckTypeInfo = shared {
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
  type RegionInfo = shared {
    id : RegionId;
    short_name : Text;
    description : Text;
  };

  /**
  `ProduceInfo`
  -----------------
  */

  type ProduceInfo = shared {
    id : ProduceId;
    short_name : Text;
    description : Text;
    grade : Grade;
  };

  /**
  `ProducerInfo`
  -----------------
  */

  type ProducerInfo = shared {
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

  type InventoryInfo = shared {
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

  type ReservedInventoryInfo = shared {
  id : ReservedInventoryId;
  retailer : RetailerId;
  item : InventoryInfo;
  };

  /**
  `ProduceMarketInfo`
  -----------------
  */

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

  /**
  `RetailerInfo`
  -----------------
  */

  type RetailerInfo = shared {
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

  type TransporterInfo = shared {
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
  `ReservedRouteInfo`
  -----------------
  */

  type ReservedRouteInfo = shared {
    id : ReservedRouteId;
    retailer : RetailerId;
    route : RouteInfo;
  };

  /**
  `ReservationInfo`
  -----------------
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
    inventoryId: InventoryId;
    routeId:     RouteId;
  };


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
  type QueryAllResult = ReservationInfo;

  /**

  xxx how to represent huge result messages?

  2019.03.12 *** TODO-Cursors: Introduce the idea of "cursors", with
  allocation, de-allocation and movement messages, as per discussion in
  the 2019.03.12 ActorScript Team meeting.

  */

  type QueryAllResults = [QueryAllResult];

}
