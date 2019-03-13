/**

Server Model
===============================

Here, we depart from defining PESS, and turn our attention to the
internal representation of the server actor's state.  The behavior of
this actor is part of the PESS definition, but the internal data model
representations it uses are not.

We have the freedom to model the system to best match the kinds of
updates and queries that we must handle now, and anticipate handling
in the future.

We employ a purely-functional repesentation based on tree-shaped data
structures with sharing.  See `ServerModelTypes.as` for details.
Below, we explain our representation of traditional SQL tables in
terms of nested structures and finite maps.

*/


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

  private getRegion(id:RegionId) : ?Region {
    Map.find<RegionId, Region>(
      regions,
      new { key=id;
            hash=idHash(id) },
      idIsEq)
  };

  private getProduce(id:ProduceId) : ?Produce {
    Map.find<ProduceId, Produce>(
      produce,
      new { key=id;
            hash=idHash(id) },
      idIsEq)
  };

  private getTruckType(id:TruckTypeId) : ?TruckType {
    Map.find<TruckTypeId, TruckType>(
      truckTypes,
      new { key=id;
            hash=idHash(id) },
      idIsEq)
  };

  /**
  // Producers collection:
  // ----------------------
  // Represents the following tables, as a tree-shaped functional data structure, with sharing:
  //  - producer table
  //  - inventory table
  //  - reservedInventory table (formerly "orderedInventory" table)
  */
  private var producers : ProducerTable = null;

  private getProducer(id:ProducerId) : ?Producer {
    Map.find<ProducerId, Producer>(
      producers,
      new { key=id;
            hash=idHash(id) },
      idIsEq)
  };

  private var inventory : InventoryTable = null;

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
  private var inventoryByRegion : ByRegionInventoryTable = null;

  private unwrap<T>(ox:?T) : T {
    switch ox {
    case (null) { assert false ; unwrap<T>(ox) };
    case (?x) x;
    }
  };

  private idIsEq(x:Nat,y:Nat):Bool { x == y };

  private idHash(x:Nat):Hash { null /* xxx */ };

  private keyOf(x:Nat):Key<Nat> {
    new { key = x ; hash = idHash(x) }
  };

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
  ) : ?TruckTypeId {
    let id = nextTruckTypeId;
    nextTruckTypeId := id + 1;
    truckTypes :=
    Map.insertFresh<TruckTypeId, TruckType>(
      truckTypes,
      keyOf(id),
      idIsEq,
      // xxx: AS should have more concise syntax for this pattern, below:
      // two problems I see, that are separate:
      // 1: repeating the label/variable name, which is the same in each case, twice.
      // 2: explicit type annotations, because of "type error, cannot infer type of forward variable ..."
      //    but two other sources exist for each type: the type of `insert` is known, and hence, this record has a known type,
      //    and, the type of each of these `variables` is known, as well.
      new { id=id:TruckTypeId;
            short_name=short_name:Text;
            description=description:Text;
            capacity=capacity:Weight;
            isFridge=isFridge:Bool;
            isFreezer=isFreezer:Bool;
      },
    );
    ?id
  };

  // registrarRemTruckTypes
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemTruckType(
    id: TruckTypeId
  ) : ?() {
    Map.removeThen<TruckTypeId, TruckType, ?()>(
      truckTypes,
      keyOf(id),
      idIsEq,
      func (t:TruckTypeTable, tt:TruckType) : ?() {
        truckTypes := t;
        ?()
      },
      func ():?() = null
    )
  };

  /**
  // registrarAddRegion
  // ---------------------
  //
  // adds the region to the system; fails if the given information is
  // invalid in any way.
  */

  registrarAddRegion(
    short_name:  Text,
    description: Text,
  ) : ?RegionId {
    let id = nextRegionId;
    nextRegionId := id + 1;
    regions :=
    Map.insertFresh<RegionId, Region>(
      regions,
      keyOf(id),
      idIsEq,
      new { id=id:RegionId;
            short_name=short_name:Text;
            description=description:Text;
      },
    );
    ?id
  };

  // registrarRemProduce
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemRegion(
    id: RegionId
  ) : ?() {
    Map.removeThen<RegionId, Region, ?()>(
      regions,
      keyOf(id),
      idIsEq,
      func (t:RegionTable, r:Region) : ?() {
        regions := t;
        ?()
      },
      func ():?() = null
    )
  };

  // registrarAddProduce
  // ---------------------
  //
  // adds the produce to the system; fails if the given information is invalid in any way.

  registrarAddProduce(
    short_name:  Text,
    description: Text,
    grade: Grade,
  ) : ?ProduceId {
    let id = nextProduceId;
    nextProduceId := id + 1;
    produce :=
    Map.insertFresh<ProduceId, Produce>(
      produce,
      keyOf(id),
      idIsEq,
      new { id=id:ProduceId;
            short_name=short_name:Text;
            description=description:Text;
            grade=grade:Grade;
      },
    );
    ?id
  };

  // registrarRemProduce
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemProduce(
    id: ProduceId
  ) : ?() {
    Map.removeThen<ProduceId, Produce, ?()>(
      produce,
      keyOf(id),
      idIsEq,
      func (t:ProduceTable, p:Produce) : ?() {
        produce := t;
        ?()
      },
      func ():?() = null
    )
  };


  // registrarAddProducer
  // ---------------------
  //
  // adds the producer to the system; fails if the given region is non-existent.

  registrarAddProducer(
    short_name:  Text,
    description: Text,
    rid: RegionId,
  ) : ?ProducerId {
    // fail early if the region id is invalid
    let region = switch (getRegion(rid)) {
    case (null) { return null };
    case (?r) r;
    };
    // pre: region id is well-defined.
    let id = nextProducerId;
    nextProducerId := id + 1;
    producers :=
    Map.insertFresh<ProducerId, Producer>(
      producers,
      keyOf(id),
      idIsEq,
      new { id=id:ProducerId;
            short_name=short_name:Text;
            description=description:Text;
            region=region:Region;
            inventory=null:InventoryTable;
            reserved=null:ReservedInventoryTable;
      },
    );
    ?id
  };

  // registrarRemProducer
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemProducer(
    id: ProducerId
  ) : ?() {
    Map.removeThen<ProducerId, Producer, ?()>(
      producers,
      keyOf(id),
      idIsEq,
      func (t:ProducerTable, p:Producer) : ?() {
        producers := t;
        ?()
      },
      func ():?() = null
    )
  };

  // registrarAddRetailer
  // ---------------------
  //
  // adds the producer to the system; fails if the given region is non-existent.

  registrarAddRetailer(
    short_name:  Text,
    description: Text,
    rid: RegionId,
  ) : ?RetailerId {
    // fail early if the region id is invalid
    let region = switch (getRegion(rid)) {
    case (null) { return null };
    case (?r) r;
    };
    // pre: region id is well-defined.
    let id = nextRetailerId;
    nextRetailerId := id + 1;
    retailers :=
    Map.insertFresh<RetailerId, Retailer>(
      retailers,
      keyOf(id),
      idIsEq,
      new { id=id:RetailerId;
            short_name=short_name:Text;
            description=description:Text;
            region=region:Region;
            reserved_routes=null:ReservedRouteTable;
            reserved_items=null:ReservedInventoryTable;
      },
    );
    ?id
  };

  // registrarRemRetailer
  // ---------------------
  //
  // returns `?()` on success, and `null` on failure.

  registrarRemRetailer(
    id: RetailerId
  ) : ?() {
    Map.removeThen<RetailerId, Retailer, ?()>(
      retailers,
      keyOf(id),
      idIsEq,
      func (t:RetailerTable, r:Retailer) : ?() {
        retailers := t;
        ?()
      },
      func ():?() = null
    )
  };

  // registrarAddTransporter
  // ---------------------
  //
  registrarAddTransporter(
    short_name:  Text,
    description: Text,
  ) : ?TransporterId {
    let id = nextTransporterId;
    nextTransporterId := id + 1;
    transporters :=
    Map.insertFresh<TransporterId, Transporter>(
      transporters,
      keyOf(id),
      idIsEq,
      new { id=id:TransporterId;
            short_name=short_name:Text;
            description=description:Text;
            route=null:RouteTable;
            reserved=null:ReservedRouteTable;
      },
    );
    ?id
  };


  // registrarRemTransporter
  // ---------------------
  //

  registrarRemTransporter(
    id: TransporterId
  ) : ?() {
    Map.removeThen<TransporterId, Transporter, ?()>(
      transporters,
      keyOf(id),
      idIsEq,
      func (t:TransporterTable, tr:Transporter) : ?() {
        transporters := t;
        ?()
      },
      func ():?() = null
    )
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
    comments: Text,
  ) : ?InventoryId {
    let producer : ?Producer = getProducer(id);
    let produce  : ?Produce  = getProduce(prod);
    // check whether these ids are defined; fail fast if not defined
    switch (producer, produce) {
    case (?producer, ?produce) {
           // create item; give it an id; catalog this id
           let item: InventoryItem = new {
             id= nextInventoryId;
             produce= produce:Produce;
             producer= prod:ProducerId;
             quantity= quantity:Quantity;
             start_date=begin:Date;
             end_date=end:Date;
             comments=comments:Text;
           };
           nextInventoryId += 1;
           inventory :=
           Map.insertFresh<InventoryId, InventoryItem>(
             inventory,
             keyOf(item.id),
             idIsEq,
             item
           );

           // Update the producer:
           // xxx shorter syntax for this "record update" pattern?
           let updatedProducer : Producer = new {
             id = producer.id;
             short_name = producer.short_name;
             description = producer.description;
             region = producer.region;
             reserved = producer.reserved;
             inventory =
               Map.insertFresh<InventoryId, InventoryItem>(
                 producer.inventory,
                 keyOf(item.id),
                 idIsEq,
                 item
               )
           };

           // Update producers table:
           producers :=
           Map.insertFresh<ProducerId, Producer>(
             producers,
             keyOf(id),
             idIsEq,
             updatedProducer
           );

           // Update inventoryByRegion table:
           inventoryByRegion :=
           Map.insertFresh2D<RegionId, ProducerId, InventoryTable>(
             inventoryByRegion,
             /* key1: region id of the producer */
             keyOf(producer.region.id), idIsEq,
             /* key2: producer id */
             keyOf(producer.id), idIsEq,
             /* value: producer's updated inventory table */
             updatedProducer.inventory,
           );

           // return the item's id
           ?item.id
         };
    case (_, _) { return null };
    }
  };

  /**
  // producerRemInventory
  // ---------------------------
  */
  producerRemInventory(id:InventoryId) : ?() {
    // xxx
    // - remove from the inventory in inventory table; use `Trie.removeThen`
    // - if successful, look up the producer ID; should not fail; `Trie.find`
    // - update the producer, removing this inventory; use `Trie.{replace,remove}`
    // - finally, use producer's region to update inventoryByRegion table,
    //   removing this inventory item; use `Trie.remove2D`.
    null
  };

  /**
  // producerReservations
  // ---------------------------
  */
  producerReservations(id:ProducerId) : ?[ReservationId] {
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
  ) : ?RouteId {
    // xxx
    null
  };

  /**
  // transporterRemRoute
  // ---------------------------
  */
  transporterRemRoute(id:RouteId) : ?() {
    // xxx
    null
  };

  /**
  // transporterReservations
  // ---------------------------
  */
  transporterReservations(id:TransporterId) : ?[ReservationId] {
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
  retailerQueryAll(id:RetailerId) : ?QueryAllResults {
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
    route:RouteId) : ?ReservationId
  {
    // xxx
    null
  };

  /**
  // retailerReservations
  // ---------------------------

  TODO-Cursors (see above).

  */
  retailerReservations(id:RetailerId) : ?[ReservationId] {
    // xxx
    null
  };

  /**
  // PES: (Producer/Transporter/Retailer) ingress messages:
  // ========================================================

  // reservationInfo
  // ---------------------------
  **/
  reservationInfo(id:ReservationId) : ?ReservationInfo {
    // xxx
    null
  };

};
