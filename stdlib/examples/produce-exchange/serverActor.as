/**

 [PESS Background](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#Produce-Exchange-Standards-Specification-PESS)
 --------------------

 PESS: Server Actor
 =======================================

 As part of the PESS definition, we define an interface that gives the
 format of each message sent by a participant, and response received in
 return.

 In ActorScript, this format specification corresponds to the
 public-facing signature of the Server actor, defined below.

 This server actor gives a collection of ingress messages and
 corresponding response types for each participant in the exchange,
 using only types defined in the PESS (e.g., no collection types, no
 standard library types, and no higher-order ActorScript types).

 As explained in the `README.md` file,
 this actor also gives a behavioral spec of the
 exchange's semantics, by giving a prototype implementation of this
 behavior, whose functional behavior, not implementation details, are
 part of the formal PESS.

 */
actor class Server() {

// @Omit:

  // See `serverModel.as` for the Model class's implementation

  // Matthew-Says:
  // There are two initialization options for the model field:
  // 1. Call Model() directly; using this option now.
  // 2. Call Model() later, when we try to access the model field.

  private var model : Model = Model(); // OPTION 2: null;

  private getModel() : Model {
    model
    // OPTION 2:
    // switch model {
    //   case (null) {
    //          let m = Model();
    //          model := ?m; m
    //        };
    //   case (?m) m;
    // }
    //
  };

  /**

   PESS: Registrar-based ingress messages
   ================================================
   Add/remove support across various mostly-static tables:

   - truck types, produce (types) and region information.
   - participants: producers, retailers and transporters.

   For each of the six entities listed above, we have an add
   (`Add`) and remove (`Rem`) function below, prefixed by
   `registrar`-, and suffixed by one of the entities in `TruckType`,
   `Region`, `Produce`, `Producer`, `Retailer`, `Transporter`.
   */

  /**
   // `reigstrarTruckType`
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
    getModel()
      .registrarAddTruckType(
        short_name, description, capacity, isFridge, isFreezer
      )
  };

  /**
   // `registrarRemTruckType`
   // ---------------------
   //
   // returns `?()` on success, and `null` on failure.
   */

  registrarRemTruckType(
    id: TruckTypeId
  ) : async ?() { getModel().registrarRemTruckType(id) };

  /**
   // `registrarTruckType`
   // ---------------------
   //
   // adds the truck type to the system; fails if the given information is
   // invalid in any way.
   */

  registrarAddRegion(
    short_name:  Text,
    description: Text,
  ) : async ?RegionId { getModel().registrarAddRegion(short_name, description) };

  /**
   // `registrarRemProduce`
   // ---------------------
   //
   // returns `?()` on success, and `null` on failure.
   */

  registrarRemRegion(
    id: RegionId
  ) : async ?() {
    getModel().registrarRemRegion(id)
  };

  /**
   // `registrarAddProduce`
   // ---------------------
   //
   // adds the produce to the system; fails if the given information is invalid in any way.
   */

  registrarAddProduce(
    short_name:  Text,
    description: Text,
    grade: Grade,
  ) : async ?ProduceId {
    getModel().registrarAddProduce(short_name, description, grade)
  };

  /**
   // `registrarRemProduce`
   // ---------------------
   //
   // returns `?()` on success, and `null` on failure.
   */

  registrarRemProduce(
    id: ProduceId
  ) : async ?() {
    getModel().registrarRemProduce(id)
  };

  /**
   // `registrarAddProducer`
   // ---------------------
   //
   // adds the producer to the system; fails if the given region is non-existent.
   */

  registrarAddProducer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?ProducerId {
    getModel().registrarAddProducer(short_name, description, region)
  };

  /**
   // `registrarRemProducer`
   // ---------------------
   //
   // returns `?()` on success, and `null` on failure.
   */

  registrarRemProducer(
    id: ProducerId
  ) : async ?() {
    getModel().registrarRemProducer(id)

  };

  /**
   // `registrarAddRetailer`
   // ---------------------
   //
   // adds the producer to the system; fails if the given region is non-existent.
   */

  registrarAddRetailer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?RetailerId {
    getModel().registrarAddRetailer(short_name, description, region)
  };

  /**
   // `registrarRemRetailer`
   // ---------------------
   //
   // returns `?()` on success, and `null` on failure.
   */

  registrarRemRetailer(
    id: RetailerId
  ) : async ?() {
    getModel().registrarRemRetailer(id)
  };

  /**
   // `registrarAddTransporter`
   // ---------------------
   //
   */
  registrarAddTransporter(
    short_name:  Text,
    description: Text,
  ) : async ?TransporterId {
    getModel().registrarAddTransporter(short_name, description)
  };

  /**
   // `registrarRemTransporter`
   // ---------------------
   //
   */

  registrarRemTransporter(
    id: TransporterId
  ) : async ?() {
    getModel().registrarRemTransporter(id)
  };

  /**
   // PESS: Producer-based ingress messages:
   // ==========================================
   */

  /**
   // `producerAddInventory`
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
  ) : async ?InventoryId {
    getModel().
      producerAddInventory(
        id, prod, quant, ppu, begin, end, comments)
  };

  /**
   // `producerRemInventory`
   // ---------------------------
   */
  producerRemInventory(id:InventoryId) : async ?() {
    getModel()
      .producerRemInventory(id)
  };

  /**
   // `producerReservations`
   // ---------------------------
   */
  producerReservations(id:ProducerId) : async ?[ReservationId] {
    getModel()
      .producerReservations(id)
  };

  /**
   // PESS: Transporter-based ingress messages:
   // ===========================================
   */

  /**
   // `transporterAddRoute`
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
    getModel().transporterAddRoute(trans, rstart, rend, start, end, cost, ttid)
  };

  /**
   // `transporterRemRoute`
   // ---------------------------
   */
  transporterRemRoute(id:RouteId) : async ?() {
    getModel()
      .transporterRemRoute(id)
  };

  /**
   // `transporterReservations`
   // ---------------------------
   */
  transporterReservations(id:TransporterId) : async ?[ReservationId] {
    getModel()
      .transporterReservations(id)
  };

  /**
   // PESS: Retailer-based ingress messages:
   // ======================================

   // `retailerQueryAll`
   // ---------------------------

   TODO-Cursors (see above).

   */
  retailerQueryAll(id:RetailerId) : async ?QueryAllResults {
    getModel().
      retailerQueryAll(id)
  };

  /**
   // `retailerReserve`
   // ---------------------------
   */
  retailerReserve(
    id:RetailerId,
    inventory:InventoryId,
    route:RouteId) : async ?ReservationId
  {
    getModel().
      retailerReserve(id, inventory, route)
  };

  /**
   // `retailerReserveCheapest`
   // ---------------------------
   //
   // Like `retailerReserve`, but chooses cheapest choice among all
   // feasible produce inventory items and routes, given a grade,
   // quant, and delivery window.
   //
   // ?? This may be an example of what Mack described to me as
   // wanting, and being important -- a "conditional update"?
   //
   */
  retailerReserveCheapest(
    id:RetailerId,
    produce:ProduceId,
    grade:Grade,
    quant:Quantity,
    begin:Date,
    end:Date
  ) : async ?ReservationId
  {
    getModel().
      retailerReserveCheapest(id, produce, grade, quant, begin, end)
  };

  /**
   // `retailerReservations`
   // ---------------------------

   TODO-Cursors (see above).

   */
  retailerReservations(id:RetailerId) : async ?[ReservationId] {
    getModel().
      retailerReservations(id)
  };

  /**
   // PESS: (Producer/Transporter/Retailer) ingress messages:
   // ========================================================
   */

  /**
   // `reservationInfo`
   // ---------------------------
   */
  reservationInfo(id:ReservationId) : async ?ReservationInfo {
    getModel().
      reservationInfo(id)
  };

};

/////////////////////////////////////////////////////////////////////////////

/**
 
 To do: more registrar ingress messages:
 =======================================================
 
 - Get a list of all ids for each entity class in the registry:
 ids of all truck types, all regions, all produce, all transporters, all producers, all retailers.
 
 - For each id kind, provide a server message to get back the other registry info
 that the registrar stores in association with it (short_name, description, etc.).
 
 - not now, but eventually, may need a cursor-message sub-system for going through extremely long lists of ids.
 
 */
