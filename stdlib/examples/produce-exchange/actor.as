/**

PES: Produce Exchange message formats
=======================================

As part of the PES definition, we define format of each message sent
by a participant, and response received in return.  In ActorScript,
this format specification corresponds to the public-facing signature
of the actor defined below.

This actor gives a collection of ingress messages and corresponding
response types for each participant in the exchange, using only types
defined in the PES (e.g., no collection types, no standard library
types, and no higher-order ActorScript types).  As explained above,
this actor also gives a behavioral spec of the exchange's semantics,
by giving a prototype implementation of this behavior, whose
functional behavior, not implementation details, are part of the
formal PES.

*/
actor class ProduceExchange() {

  private var nextTruckTypeId : TruckTypeId = 0;
  private var nextRegionId : RegionId = 0;
  private var nextProduceId : ProduceId = 0;
  private var nextProducerId : ProducerId = 0;
  private var nextRetailerId : RetailerId = 0;
  private var nextInventoryId : InventoryId = 0;
  private var nextTransporterId : TransporterId = 0;
  private var nextxRouteId : RouteId = 0;
  private var nextReservationId : ReservationId = 0;

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
  // ?: Days as keys in a spatial-temporal data structure?
  // -----------------
  // - inventory, by availability window (start day, end day)
  // - routes, by transport window (departure day, arrival day)

  private unwrap<T>(ox:?T) : T {
    switch ox {
    case (null) { assert false ; unwrap<T>(ox) };
    case (?x) x;
    }
  };

  /*
  // PES: Registrar-based ingress messages
  // ================================================
  // Add/remove support across various mostly-static tables:
  //
  // - truck types, produce (types) and region information.
  // - participants: producers, retailers and transporters.
  //
  // For each of the six entities listed above, we have an add
  // (`Add`) and remove (`Rem`) function below, prefixed by
  // `registrar`-, and suffixed by one of the entities in `TruckType`,
  // `Region`, `Produce`, `Producer`, `Retailer`, `Transporter`.
  */

  /*
   // reigstrarTruckType
  // -------------------
  //
  */

  registrarAddTruckType(
    short_name:  Text,
    description: Text,
    capacity : Weight,
    isFridge : Bool,
    isFreezer : Bool,
  ) : async ?TruckTypeId {
    // xxx
    null
  };

  // registrarRemProduce
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemTruckType(
    id: TruckTypeId
  ) : async ?() {
    // xxx
    null
  };

  /**
  // registrarAddProduce
  // ---------------------
  //
  // adds the produce to the system; fails if the given information is
  // invalid in any way.
  */

  registrarAddRegion(
    short_name:  Text,
    description: Text,
  ) : async ?RegionId {
    // xxx
    null
  };

  // registrarRemProduce
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemRegion(
    id: RegionId
  ) : async ?() {
    // xxx
    null
  };

  // registrarAddProduce
  // ---------------------
  //
  // adds the produce to the system; fails if the given information is invalid in any way.

  registrarAddProduce(
    short_name:  Text,
    description: Text,
    grade: Grade,
  ) : async ?ProduceId {
    // xxx
    null
  };

  // registrarRemProduce
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemProduce(
    id: ProduceId
  ) : async ?() {
    // xxx
    null
  };


  // registrarAddProducer
  // ---------------------
  //
  // adds the producer to the system; fails if the given region is non-existent.

  registrarAddProducer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?ProducerId {
    // xxx
    null
  };

  // registrarRemProducer
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemProducer(
    id: ProducerId
  ) : async ?() {
    // xxx
    null
  };

  // registrarAddRetailer
  // ---------------------
  //
  // adds the producer to the system; fails if the given region is non-existent.

  registrarAddRetailer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?RetailerId {
    // xxx
    null
  };

  // registrarRemRetailer
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemRetailer(
    id: RetailerId
  ) : async ?() {
    // xxx
    null
  };

  // registrarAddTransporter
  // ---------------------
  //
  registrarAddTransporter(
    short_name:  Text,
    description: Text,
  ) : async ?TransporterId {
    // xxx
    null
  };


  // registrarRemTransporter
  // ---------------------
  //

  registrarRemTransporter(
    id: TransporterId
  ) : async ?() {
    // xxx
    null
  };


  /**
  // PES: Producer-based ingress messages:
  // ==========================================
  */

  /**
  // producerAddInventory
  // ---------------------------
  */
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

  /**
  // producerRemInventory
  // ---------------------------
  */
  producerRemInventory(id:InventoryId) : async ?() {
    // xxx
    null
  };

  /**
  // producerReservations
  // ---------------------------
  */
  producerReservations(id:ProducerId) : async ?[ReservationId] {
    // xxx
    null
  };

  /**
  // PES: Transporter-based ingress messages:
  // ===========================================
  */

  /**
  // transporterAddRoute
  // ---------------------------
  */
  transporterAddRoute(
    trans:  TransporterId,
    rstart: RegionId,
    rend:   RegionId,
    start:  Date,
    end:    Date,
    cost:   Price,
    ttid:   TruckTypeId
  ) : async ?RouteId {
    // xxx
    null
  };

  /**
  // transporterRemRoute
  // ---------------------------
  */
  transporterRemRoute(id:RouteId) : async ?() {
    // xxx
    null
  };

  /**
  // transporterReservations
  // ---------------------------
  */
  transporterReservations(id:TransporterId) : async ?[ReservationId] {
    // xxx
    null
  };

  /**
  // PES: Retailer-based ingress messages:
  // ======================================

  // retailerQueryAll
  // ---------------------------

  TODO-Cursors (see above).

  */
  retailerQueryAll(id:RetailerId) : async ?QueryAllResults {
    // xxx
    null
  };

  /**
  // retailerReserve
  // ---------------------------
  */
  retailerReserve(
    id:RetailerId,
    inventory:InventoryId,
    route:RouteId) : async ?ReservationId
  {
    // xxx
    null
  };

  /**
  // retailerReservations
  // ---------------------------

  TODO-Cursors (see above).

  */
  retailerReservations(id:RetailerId) : async ?[ReservationId] {
    // xxx
    null
  };

  /**
  // PES: (Producer/Transporter/Retailer) ingress messages:
  // ========================================================

  // reservationInfo
  // ---------------------------
  **/
  reservationInfo(id:ReservationId) : async ?ReservationInfo {
    // xxx
    null
  };

};
