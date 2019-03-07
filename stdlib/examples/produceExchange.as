// Produce Exchange Dapp
// =====================
//
// Start here:
// - Detailed examples: https://dfinity.atlassian.net/wiki/x/joXUBg
// - More background: https://dfinity.atlassian.net/wiki/x/4gg5Bg
//

// Open Questions:
// -------------------------------------------------

// 1. Massive result messages:
//  How do we represent and send these?
//
// - lazy lists? (seems "easy" from AS programmer perspective, but
//   requires non-first-order data in the IDL)
// 
// - list iterators? (almost as good as lazy lists, but requires
//   references in the IDL, and complicates the GC story).
// 
// - arrays? (expensive to build and send; can become way *too big*).
//

// 2. For now, wan we assume that the canister is maintained by the
// central authority?

////////////////////////////////////////////////////////////////

// Use the standard library of AS:
// ===============================
//

// Collections implement internal tables:
// --------------------------------------
// import Table (same as Trie?) xxx

// import Date
// xxx Dates, eventually from a standard library:
type Date = Nat;

// xxx standard weight units?
type Weight = Nat;

// xxx standard price units?
type Price = Nat;

/////////////////////////////////////////////////////////////////

// Fixed types 
// ===============================
//
// We assume some fixed types (for now).
// Updating these types requires a canister upgrade.
//
// ?? defined by the central authority, aka, the "canister maintainer"?
//

type Unit = Nat; // xxx replace with a variant type
type Grade = Nat; // xxx replace with a variant type

type TruckType = Nat; // ??? replace with a variant type

type TruckCapacity = Weight;

type Quantity = Nat;

type PricePerUnit = Price; // needed to calculate prices
type PriceTotal = Price;

type WeightPerUnit = Weight; // needed to meet truck constraints

type RegionId = Nat; // xxx variant type?


//
// Unique Ids
// ----------
// Internally, each type of Id serves as a "row key" for a table (or two).
//

type ProduceId     = Nat;
type ProducerId    = Nat;
type RetailerId    = Nat;
type TruckTypeId   = Nat;
type InventoryId   = Nat;
type TransporterId = Nat;
type RouteId       = Nat;
type OrderId       = Nat;

//
// Query parameters and results
// ----------------------------
//

type OrderInfo = shared {
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

// xxx same as an OrderInfo?  If different, then how?
type QueryAllResult = shared {
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

// xxx how to represent huge result messages?
type QueryAllResults = [QueryAllResult];

// the "Service"
actor ProduceExchange {

  // Producer-based ingress messages:
  // ================================

  producerAddInventory(
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
    tt:     TruckTypeId
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
