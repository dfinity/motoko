/**

 [PESS Background](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#Produce-Exchange-Standards-Specification-PESS)
 --------------------
*/

actor class Server() {

/**
 PESS: Server Actor
 =======================================

 The Server actor defines an interface for messages sent
 by all participants, and the responses received in return.

 See also: (Non-PESS) **[Server model](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/examples/produce-exchange/serverModel.md)**


 PESS: Registrar-based ingress messages
 ================================================

 The registrar provides functions to add and to remove entities from
 the following (mostly-static) tables:
 
 - **Static resource information:** truck types, produce types and region information.
 - **Participant information:** producers, retailers and transporters.
 - **Dynamic resource information:** inventory, routes and reservations.
 
 For each of the entities listed above, we have an add (`Add`)
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
 Messages about truck types.
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
      .truckTypeTable.addInfoGetId(
        func (id:TruckTypeId) : TruckTypeInfo =

        // xxx: AS should have more concise syntax for this pattern, below:
        // two problems I see, that are separate:
        // 1: repeating the label/variable name, which is the same in each case, twice.
        // 2: explicit type annotations, because of "type error, cannot infer type of forward variable ..."
        //    but two other sources exist for each type: the type of `insert` is known, and hence, this record has a known type,
        //    and, the type of each of these `variables` is known, as well.

        shared { id=id:TruckTypeId;
                 short_name=short_name:Text;
                 description=description:Text;
                 capacity=capacity:Weight;
                 isFridge=isFridge:Bool;
                 isFreezer=isFreezer:Bool;
        })
  };

  /**
   `registrarRemTruckType`
   ---------------------
   */

  registrarRemTruckType(
    id: TruckTypeId
  ) : async ?() { 
    getModel().truckTypeTable.remGetUnit(id)
  };

  /**
   `getTruckTypeInfo`
   ---------------------
   */

  getTruckTypeInfo(
    id: TruckTypeId
  ) : async ?TruckTypeInfo { 
    getModel().truckTypeTable.getInfo(id) 
  };

  /**
   `allTruckTypeInfo`
   ---------------------
   */

  allTruckTypeInfo() : async [TruckTypeInfo] { 
    getModel().truckTypeTable.allInfo()
  };
  

  /**
   `Region`
   ==============
   Messages about regions.

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
  ) : async ?RegionId { 
    getModel().regionTable.addInfoGetId(
      func (id:RegionId) : RegionInfo =
        shared {
          id = id:RegionId;
          short_name=short_name:Text;
          description=description:Text 
        })
  };

  /**
   `registrarRemRegion`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemRegion(
    id: RegionId
  ) : async ?() {
    getModel().regionTable.remGetUnit(id)
  };

  /**
   `getRegionInfo`
   ---------------------
   */

  getRegionInfo(
    id: RegionId
  ) : async ?RegionInfo {
    getModel().regionTable.getInfo(id)
  };
  

  /**
   `allRegionInfo`
   ---------------------
   */

  allRegionInfo() : async [RegionInfo] {
    getModel().regionTable.allInfo()
  };


  /**
   `Produce`
   =================
   Messages about produce

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
    getModel().produceTable.addInfoGetId(
      func (id:ProduceId) : ProduceInfo =
        shared {
          id = id:ProduceId;
          short_name=short_name:Text;
          description=description:Text;
          grade=grade:Grade
        })
  };

  /**
   `registrarRemProduce`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemProduce(
    id: ProduceId
  ) : async ?() {
    getModel().produceTable.remGetUnit(id)
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  getProduceInfo(
    id: ProduceId
  ) : async ?ProduceInfo {
    getModel().produceTable.getInfo(id)
  };

  /**
   `allProduceInfo`
   ---------------------
   */

  allProduceInfo() : async [ProduceInfo] {
    getModel().produceTable.allInfo()
  };
 
  /**
   `Producer`
   ===============
   Messages about prodcuers.

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
    getModel().producerTable.addInfoGetId(
      func(id:ProducerId):ProducerInfo {
        shared {
          id=id:ProducerId;
          short_name=short_name:Text;
          description=description:Text;
          region=region:RegionId;
          inventory=[];
          reserved=[];
        }
      })
  };

  /**
   `registrarRemProducer`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemProducer(
    id: ProducerId
  ) : async ?() {
    getModel().producerTable.remGetUnit(id)
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  getProducerInfo(
    id: ProducerId
  ) : async ?ProducerInfo {
    getModel().producerTable.getInfo(id)
  };

  /**
   `allProducerInfo`
   ---------------------
   */

  allProducerInfo() : async [ProducerInfo] {
    getModel().producerTable.allInfo()
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
    getModel().retailerTable.addInfoGetId(
      func(id:RetailerId):RetailerInfo {
        shared {
          id=id:RetailerId;
          short_name=short_name:Text;
          description=description:Text;
          region=region:RegionId
        }
      })
  };

  /**
   `registrarRemRetailer`
   ---------------------
   
   returns `?()` on success, and `null` on failure.
   */

  registrarRemRetailer(
    id: RetailerId
  ) : async ?() {
    getModel().retailerTable.remGetUnit(id)
  };

  /**
   `getRetailerInfo`
   ---------------------
   */
  
  getRetailerInfo(
    id: RetailerId
  ) : async ?RetailerInfo {
    getModel().retailerTable.getInfo(id)
  };

  /**
   `allRetailerInfo`
   ---------------------
   */
  
  allRetailerInfo() : async [RetailerInfo] {
    getModel().retailerTable.allInfo()
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
    getModel().transporterTable.addInfoGetId(
      func(id:TransporterId):TransporterInfo {
        shared {
          id=id:TransporterId;
          short_name=short_name:Text;
          description=description:Text;
          routes=[];
          reserved=[];
        }
      })
      
  };

  /**
   `registrarRemTransporter`
   ---------------------
   
   */

  registrarRemTransporter(
    id: TransporterId
  ) : async ?() {
    getModel().transporterTable.remGetUnit(id)
  };

  /**
   `getTransporterInfo`
   ---------------------
   */

  getTransporterInfo(
    id: TransporterId
  ) : async ?TransporterInfo {
    getModel().transporterTable.getInfo(id)
  };


  /**
   `allTransporterInfo`
   ---------------------
   */

  allTransporterInfo() : async [TransporterInfo] {
    getModel().transporterTable.allInfo()
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
  producerReservations(id:ProducerId) : async ?[ReservedInventoryId] {
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
  transporterAllReservationInfo(id:TransporterId) : async ?[ReservedRouteInfo] {
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
    route:RouteId) : async ?(ReservedInventoryId, ReservedRouteId)
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
  ) : async ?(ReservedInventoryId, ReservedRouteId)
  {
    getModel().
      retailerReserveCheapest(id, produce, grade, quant, begin, end)
  };

  /**
   `retailerReservations`
   ---------------------------

   TODO-Cursors (see above).

   */
  retailerReservations(id:RetailerId) : 
    async ?[(ReservedInventoryInfo,
             ReservedRouteInfo)]
  {
    getModel().
      retailerAllReservationInfo(id)
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
  
  devViewReservations() : async Nat {
    getModel().reservedInventoryTable.count()
  };

  /**
   `devViewProducers`
   -------------------   

   MVP:

   > Developer can see how many producers in the system and how many goods each has

   See also [`producerInfo`](#producerinfo).

   */

  devViewProducers() : async [ProducerInfo] {
    getModel().producerTable.allInfo()
  };


  /**
   `devViewTransporters`
   -------------------   
 
   MVP:

   > Developer can see how many transporters in the system and how many routes each has

   See also [`transporterInfo`](#transporterinfo).

   */

  devViewTransporters() : async [TransporterInfo] {
    getModel().transporterTable.allInfo()
  };

  /**
   `devViewRetailers`
   -------------------   
   
   MVP:

   > Developer can see how many retailers in the system and how many queries and how many sales orders

   See also [`retailerInfo`](#retailerinfo).

   */

  devViewRetailers() : async [RetailerInfo] {
    getModel().retailerTable.allInfo()
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
