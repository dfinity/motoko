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

  /**

   PESS: Registrar-based ingress messages
   ================================================

   The registrar provides functions to add and to remove entities from
   the following (mostly-static) tables:
   
   - **Resource information:** truck types, produce (types) and region information.
   - **Participant information:** producers, retailers and transporters.
   
   For each of the six entities listed above, we have an add (`Add`)
   and remove (`Rem`) function below, prefixed by `registrar`-, and
   suffixed by one of the entities in the following list:
   
   - `TruckType`,
   - `Region`, 
   - `Produce`, 
   - `Producer`, 
   - `Retailer`, or
   - `Transporter`.
   

  `TruckType`
  ==============
   Messages to `Add` and `Rem` truck types.
   */

  
  /**
   `reigstrarAddTruckType`
   ------------------------

   */

  registrarAddTruckType(
    short_name:  Text,
    description: Text,
    capacity : Weight,
    isFridge : Bool,
    isFreezer : Bool,
  ) : async ?TruckTypeId {
    getModel()
      .addTruckType(
        short_name, description, capacity, isFridge, isFreezer
      )
  };

  /**
   `registrarRemTruckType`
   ---------------------
   returns `?()` on success, and `null` on failure.
   */

  registrarRemTruckType(
    id: TruckTypeId
  ) : async ?() { getModel().remTruckType(id) };

  /**
   `getTruckTypeInfo`
   ---------------------
   */

  getTruckTypeInfo(
    id: TruckTypeId
  ) : async ?TruckTypeInfo { getModel().getTruckTypeInfo(id) };


  /**
   `Region`
   ==============
   Messages to `Add`, `Rem` and `Inspect` regions.

   */

  /**
   `registrarAddRegion`
   ---------------------
   adds the region to the system; fails if the given information is
   invalid in any way.
   */

  registrarAddRegion(
    short_name:  Text,
    description: Text,
  ) : async ?RegionId { getModel().addRegion(short_name, description) };

  /**
   `registrarRemRegion`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemRegion(
    id: RegionId
  ) : async ?() {
    getModel().remRegion(id)
  };

  /**
   `getRegionInfo`
   ---------------------
   */

  getRegionInfo(
    id: RegionId
  ) : async ?RegionInfo {
    getModel().getRegionInfo(id)
  };

  /**
   `Produce`
   =================
   Messages to `Add`, `Rem` and `Inspect` produce.

   */

  /**
   `registrarAddProduce`
   ---------------------
   
   adds the produce to the system; fails if the given information is invalid in any way.
   */

  registrarAddProduce(
    short_name:  Text,
    description: Text,
    grade: Grade,
  ) : async ?ProduceId {
    getModel().addProduce(short_name, description, grade)
  };

  /**
   `registrarRemProduce`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemProduce(
    id: ProduceId
  ) : async ?() {
    getModel().remProduce(id)
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  getProduceInfo(
    id: ProduceId
  ) : async ?ProduceInfo {
    getModel().getProduceInfo(id)
  };
 
  /**
   `Producer`
   ===============
   Messages to `Add`, `Rem` and `Inspect` prodcuers.

   */

  /**
   `registrarAddProducer`
   ---------------------
   
   adds the producer to the system; fails if the given region is non-existent.
   */

  registrarAddProducer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?ProducerId {
    getModel().addProducer(short_name, description, region)
  };

  /**
   `registrarRemProducer`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemProducer(
    id: ProducerId
  ) : async ?() {
    getModel().remProducer(id)
  };

  /**
   `producerInfo`
   ---------------------
   */
  producerInfo(id: ProducerId) 
    : async ?ProducerInfo {
      // xxx
      null
  };


  /**
   `Retailer`
   ============
   Messages to `Add`, `Rem` and `Inspect` retailers.
   */

  /**
   `registrarAddRetailer`
   ---------------------
   
   adds the producer to the system; fails if the given region is non-existent.
   */

  registrarAddRetailer(
    short_name:  Text,
    description: Text,
    region: RegionId,
  ) : async ?RetailerId {
    getModel().addRetailer(short_name, description, region)
  };

  /**
   `registrarRemRetailer`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemRetailer(
    id: RetailerId
  ) : async ?() {
    getModel().remRetailer(id)
  };

  /**
   `retailerInfo`
   ---------------------
   */
  
  retailerInfo(
    id: RetailerId
  ) : async ?RetailerInfo {
    /// xxx
    null
  };

  /**
   `Transporter`
   ================
   Messages to `Add`, `Rem` and `Inspect` transporters.
   */

  /**
   `registrarAddTransporter`
   ---------------------
   
   */
  registrarAddTransporter(
    short_name:  Text,
    description: Text,
  ) : async ?TransporterId {
    getModel().addTransporter(short_name, description)
  };

  /**
   `registrarRemTransporter`
   ---------------------
   
   */

  registrarRemTransporter(
    id: TransporterId
  ) : async ?() {
    getModel().remTransporter(id)
  };

  /**
   `transporterInfo`
   ---------------------
   */

  transporterInfo(
    id: TransporterId
  ) : async ?TransporterInfo {
    /// xxx
    null
  };

  /**
   PESS: `Producer`-based ingress messages:
   ==========================================
   */

  /**
   `producerAddInventory`
   ---------------------------
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
   `producerRemInventory`
   ---------------------------
   */
  producerRemInventory(id:InventoryId) : async ?() {
    getModel()
      .producerRemInventory(id)
  };

  /**
   `producerAllInventoryInfo`
   ---------------------------
   */
  producerAllInventoryInfo(id:ProducerId) : async ?[InventoryInfo] {
    getModel()
      .producerAllInventoryInfo(id)
  };
  
  /**
   `producerReservations`
   ---------------------------
   */
  producerReservations(id:ProducerId) : async ?[ReservationId] {
    getModel()
      .producerReservations(id)
  };


  /**
   `produceMarketInfo`
   ---------------------------
   The last sales price for produce within a given geographic area; null region id means "all areas."
   */
  produceMarketInfo(id:ProduceId, reg:?RegionId) : async ?[ProduceMarketInfo] {
    getModel()
      .produceMarketInfo(id, reg)
  };


  /**
   PESS: `Transporter`-based ingress messages:
   ===========================================
   */

  /**
   `transporterAddRoute`
   ---------------------------
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
   `transporterRemRoute`
   ---------------------------
   */
  transporterRemRoute(id:RouteId) : async ?() {
    getModel()
      .transporterRemRoute(id)
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(id:TransporterId) : async ?[RouteInfo] {
    getModel()
      .transporterAllRouteInfo(id)
  };

  /**
   `transporterAllReservationInfo`
   ---------------------------
   */
  transporterAllReservationInfo(id:TransporterId) : async ?[ReservationInfo] {
    getModel()
      .transporterAllReservationInfo(id)
  };

  /**
   PESS: `Retailer`-based ingress messages:
   ======================================

   `retailerQueryAll`
   ---------------------------

   TODO-Cursors (see above).

   */
  retailerQueryAll(id:RetailerId) : async ?QueryAllResults {
    getModel().
      retailerQueryAll(id)
  };

  /**
   `retailerQueryDates`
   ---------------------------
   
   Retailer queries available produce by delivery date range; returns
   a list of inventory items that can be delivered to that retailer's
   geography within that date.
   
   */
  retailerQueryDates(
    id:RetailerId,
    begin:Date,
    end:Date
  ) : async ?[InventoryInfo]
  {
    getModel().
      retailerQueryDates(id, begin, end)
  };

  /**
   `retailerReserve`
   ---------------------------
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
   `retailerReserveCheapest`
   ---------------------------
   
   Like `retailerReserve`, but chooses cheapest choice among all
   feasible produce inventory items and routes, given a grade,
   quant, and delivery window.
   
   ?? This may be an example of what Mack described to me as
   wanting, and being important -- a "conditional update"?
   
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
   `retailerReservations`
   ---------------------------

   TODO-Cursors (see above).

   */
  retailerReservations(id:RetailerId) : async ?[ReservationId] {
    getModel().
      retailerReservations(id)
  };

  /**
   
   PESS: general-use ingress messages:
   ========================================================
   
   The following messages may originate from any entity; they access
   published information in the tables maintained above.  

   */

  /**
   `reservationInfo`
   ---------------------------
   */
  reservationInfo(id:ReservationId) : async ?ReservationInfo {
    getModel().
      reservationInfo(id)
  };

  /**
   
   PESS: Developer-based ingress messages:
   ========================================================
   
   The following messages may originate from developers

   */

  /**
   `devViewGMV`
   -------------
   
   MVP:
   
   > Developer can see the GMV, the aggregate sum of how many sales have
been processed 
*/
  
  devViewGMV() : async ?Nat {
    // xxx
    null
  };

  /**
   `devViewQueries`
   ----------------

   MVP:
   
   > Developer can see how many aggregate queries have been made by all retailers

   */

  devViewQueries() : async ?Nat {
    // xxx
    null
  };


  /**
   `devViewReservations`
   ----------------------

   MVP:

   > Developer can see how many aggregate sales orders have been made by all retailers

   */
  
  devViewReservations() : async ?Nat {
    // xxx
    null
  };

  /**
   `devViewReservationInfo`
   ----------------------

   MVP:

   > Developer can see how many aggregate sales orders have been made by all retailers

   */
  devViewReservationInfo(id:ReservationId) : async ?ReservationInfo {
    // xxx
    null
  };


  /**
   `devViewProducers`
   -------------------   

   MVP:

   > Developer can see how many producers in the system and how many goods each has

   See also [`producerInfo`](#producerinfo).

   */

  devViewProducers() : async ?[ProducerId] {
    // xxx
    null
  };


  /**
   `devViewTransporters`
   -------------------   
 
   MVP:

   > Developer can see how many transporters in the system and how many routes each has

   See also [`transporterInfo`](#transporterinfo).

   */

  devViewTransporters() : async ?[TransporterId] {
    // xxx
    null
  };

  /**
   `devViewRetailers`
   -------------------   
   
   MVP:

   > Developer can see how many retailers in the system and how many queries and how many sales orders

   See also [`retailerInfo`](#retailerinfo).

   */

  devViewRetailers() : async ?[RetailerId] {
    // xxx
    null
  };


  ///////////////////////////////////////////////////////////////////////////
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
 End of PESS interface definition
-----------------------------------
  With the following closing brace, the interface of the PESS `Server` is thusly defined.
 */
}; // end: actor class `Server`

/////////////////////////////////////////////////////////////////////////////

/** 
 To do: PESS definition
 ================================================
 
 More registrar ingress messages:
 --------------------------------
 
 - Get a list of all ids for each entity class in the registry:
 ids of all truck types, all regions, all produce, all transporters, all producers, all retailers.
 
 - For each id kind, provide a server message to get back the other registry info
 that the registrar stores in association with it (short_name, description, etc.).
 
 - not now, but eventually, may need a cursor-message sub-system for going through extremely long lists of ids.
 
 */
