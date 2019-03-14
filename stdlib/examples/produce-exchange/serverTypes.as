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

type WeightPerUnit = Weight; // needed to meet truck constraints

type RegionId = Nat; // xxx variant type?

/**
//
// PESS: Unique Ids
// -----------------
// Externally, these Ids give a unique identifier that is unique to its type, but not universally unique.
// Internally, each type of Id serves as a "row key" for a table (or two).
//
*/

type TruckTypeId   = Nat;
type ProduceId     = Nat;
type InventoryId   = Nat;

type ProducerId    = Nat;
type RetailerId    = Nat;
type TransporterId = Nat;

type RouteId       = Nat;
type ReservationId = Nat;

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
