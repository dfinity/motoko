/**

 [Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
 --------------------
*/

actor server = {

/**
 Server Actor
 =======================================

 The `Server` actor defines an interface for messages sent
 by all participants, and the responses received in return.

 See also:

 - [client-server types]($DOCURL/examples/produce-exchange/serverTypes.md#server-types).
 - the **[server `Model` class]($DOCURL/examples/produce-exchange/serverModel.html)**.


 Registrar-based ingress messages
 ================================================

 The registrar provides functions to add and to remove entities from
 the following (mostly-static) tables:

 - **Static resource information:** truck types, produce types and region information.
 - **Participant information:** producers, retailers and transporters.
 - **Dynamic resource information:** inventory, routes and reservations.

 For each of the entities listed above, we have an add (`Add`)
 and remove (`Rem`) function below, prefixed by `registrar`-, and
 suffixed by one of the entities in the following list:

 - `User`,
 - `TruckType`,
 - `Region`,
 - `Produce`,
 - `Producer`,
 - `Retailer`, or
 - `Transporter`.


 `User`
 =========
 Messages about users.


 `registrarAddUser`
 ----------------------
 Register a new user, who may play several roles in the exchange.

 The given `user_name` must be unique to the exchange; the operation fails otherwise.

 */

  registrarAddUser(
    public_key: PublicKey,
    user_name: Text,
    description: Text,
    region: RegionId,
    isDeveloper: Bool,
    isProducer: Bool,
    isRetailer: Bool,
    isTransporter: Bool
  ) : async Result<UserId,IdErr> {
    optionResult<UserId,IdErr>(
      getModel().addUser(
        public_key,
        user_name,
        description,
        region,
        isDeveloper,
        isProducer,
        isRetailer,
        isTransporter
      ),
      {#idErr}
    )
  };

  /**
   `allUserInfo`
   -------------
   Get info for all users.
   */
  allUserInfo() : async [UserInfo] {
    getModel().userTable.allInfo()
  };

  /**
   `getUserInfo`
   ---------------------------
   Get the information associated with a user, based on its id.
   */
  getUserInfo(id:UserId) : async Result<UserInfo, IdErr> {
    optionResult<UserInfo, IdErr>(
      getModel().userTable.getInfo(id),
      {#idErr}
    )
  };

 /**
 `TruckType`
 ==============
 Messages about truck types.
 */


  /**
   `reigstrarAddTruckType`
   ------------------------

   */

  registrarAddTruckType(
    short_name_:  Text,
    description_: Text,
    capacity_ : Weight,
    isFridge_ : Bool,
    isFreezer_ : Bool,
  ) : async Result<TruckTypeId,None> {
    optionUnwrapResult<TruckTypeId>(
      getModel()
        .truckTypeTable.addInfoGetId(
        func (id_:TruckTypeId) : TruckTypeInfo =

          // xxx: AS should have more concise syntax for this pattern, below:
          // two problems I see, that are separate:
          // 1: repeating the label/variable name, which is the same in each case, twice.
          // 2: explicit type annotations, because of "type error, cannot infer type of forward variable ..."
          //    but two other sources exist for each type: the type of `insert` is known, and hence, this record has a known type,
          //    and, the type of each of these `variables` is known, as well.

          shared {
            id=id_ :TruckTypeId;
            short_name=short_name_:Text;
            description=description_:Text;
            capacity=capacity_:Weight;
            isFridge=isFridge_:Bool;
            isFreezer=isFreezer_:Bool;
          })
    )
  };

  /**
   `registrarRemTruckType`
   ---------------------
   */

  registrarRemTruckType(
    id: TruckTypeId
  ) : async Result<(),ServerErr> {
    optionResult<(),IdErr>(
      getModel().truckTypeTable.remGetUnit(id),
      {#idErr}
    )
  };

  /**
   `getTruckTypeInfo`
   ---------------------
   */

  getTruckTypeInfo(
    id: TruckTypeId
  ) : async Result<TruckTypeInfo,IdErr> {
    optionResult<TruckTypeInfo,IdErr>(
      getModel().truckTypeTable.getInfo(id),
      {#idErr}
    )
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
    short_name_:  Text,
    description_: Text,
  ) : async Result<RegionId,None> {
    optionUnwrapResult<RegionId>(
      getModel().regionTable.addInfoGetId(
        func (id_:RegionId) : RegionInfo =
          shared {
            id = id_:RegionId;
            short_name=short_name_:Text;
            description=description_:Text
          })
    )
  };

  /**
   `registrarRemRegion`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemRegion(
    id: RegionId
  ) : async Result<(),IdErr> {
    optionResult<(),IdErr>(
      getModel().regionTable.remGetUnit(id),
      {#idErr},
    )
  };

  /**
   `getRegionInfo`
   ---------------------

   See also: [server type `RegionInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#regioninfo).

   */

  getRegionInfo(
    id: RegionId
  ) : async Result<RegionInfo,IdErr> {
    optionResult<RegionInfo,IdErr>(
      getModel().regionTable.getInfo(id),
      {#idErr}
    )
  };


  /**
   `allRegionInfo`
   ---------------------

   See also: [server type `RegionInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#regioninfo).

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
    short_name_:  Text,
    description_: Text,
    grade_: Grade,
  ) : async Result<ProduceId,None> {
    optionUnwrapResult<ProduceId>(
      getModel().produceTable.addInfoGetId(
        func (id_:ProduceId) : ProduceInfo =
          shared {
            id = id_:ProduceId;
            short_name=short_name_:Text;
            description=description_:Text;
            grade=grade_:Grade
          })
    )
  };

  /**
   `registrarRemProduce`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemProduce(
    id: ProduceId
  ) : async Result<(),IdErr> {
    optionResult<(),IdErr>(
      getModel().produceTable.remGetUnit(id),
      {#idErr},
    )
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  getProduceInfo(
    id: ProduceId
  ) : async Result<ProduceInfo,IdErr> {
    optionResult<ProduceInfo,IdErr>(
      getModel().produceTable.getInfo(id),
      {#idErr}
    )
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
   Messages about producers.

   */

  /**
   `registrarAddProducer`
   ---------------------

   adds the producer to the system; fails if the given region is non-existent.
   */

  registrarAddProducer(
    producer_public_key : PublicKey,
    short_name_:  Text,
    description_: Text,
    region_: RegionId,
  ) : async Result<ProducerId,IdErr> {
    optionResult<ProduceId,IdErr>(
      getModel().producerTable.addInfoGetId(
        func(id_:ProducerId):ProducerInfo {
          shared {
            id=id_:ProducerId;
            public_key=producer_public_key;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:RegionId;
            inventory=[];
            reserved=[];
          }
        }),
      {#idErr}
    )
  };

  /**
   `registrarRemProducer`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemProducer(
    id: ProducerId
  ) : async Result<(),IdErr> {
    optionResult<(),IdErr>(
      getModel().producerTable.remGetUnit(id),
      {#idErr}
    )
  };


  /**
   `getProducerInfo`
   ---------------------
   */

  getProducerInfo(
    id: ProducerId
  ) : async Result<ProducerInfo,IdErr> {
    optionResult<ProducerInfo,IdErr>(
      getModel().producerTable.getInfo(id),
      {#idErr}
    )
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
    retailer_public_key : PublicKey,
    short_name_:  Text,
    description_: Text,
    region_: RegionId,
  ) : async Result<RetailerId,IdErr> {
    optionResult<RetailerId,IdErr>(
      getModel().retailerTable.addInfoGetId(
        func(id_:RetailerId):RetailerInfo {
          shared {
            id=id_:RetailerId;
            public_key=retailer_public_key;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:RegionId
          }
        }),
      {#idErr}
    )
  };

  /**
   `registrarRemRetailer`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemRetailer(
    id: RetailerId
  ) : async Result<(),IdErr> {
    optionResult<(),IdErr>(
      getModel().retailerTable.remGetUnit(id),
      {#idErr}
    )
  };

  /**
   `getRetailerInfo`
   ---------------------
   */

  getRetailerInfo(
    id: RetailerId
  ) : async Result<RetailerInfo,IdErr> {
    optionResult<RetailerInfo,IdErr>(
      getModel().retailerTable.getInfo(id),
      {#idErr}
    )
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
    transporter_public_key: PublicKey,
    short_name_:  Text,
    description_: Text,
  ) : async Result<TransporterId,None> {
    optionUnwrapResult<TransporterId>(
      getModel().transporterTable.addInfoGetId(
        func(id_:TransporterId):TransporterInfo {
          shared {
            id=id_:TransporterId;
            public_key=transporter_public_key;
            short_name=short_name_:Text;
            description=description_:Text;
            routes=[];
            reserved=[];
          }
        })
    )
  };

  /**
   `registrarRemTransporter`
   ---------------------

   */

  registrarRemTransporter(
    id: TransporterId
  ) : async Result<(),IdErr> {
    optionResult<(),IdErr>(
      getModel().transporterTable.remGetUnit(id),
      {#idErr}
    )
  };

  /**
   `getTransporterInfo`
   ---------------------
   */

  getTransporterInfo(
    id: TransporterId
  ) : async Result<TransporterInfo,IdErr> {
    optionResult<TransporterInfo,IdErr>(
      getModel().transporterTable.getInfo(id),
      {#idErr}
    )
  };


  /**
   `allTransporterInfo`
   ---------------------
   */

  allTransporterInfo() : async [TransporterInfo] {
    getModel().transporterTable.allInfo()
  };


  /**
   `Producer`-based ingress messages:
   ==========================================
   */

  /**
   `producerAddInventory`
   ------------------------------------------

   See also [Model.producerAddInventory]($DOCURL/stdlib/examples/produce-exchange/serverModel.md#produceraddinventory)
   */
  producerAddInventory(
    public_key: PublicKey,
    id:   ProducerId,
    prod: ProduceId,
    quant:Quantity,
    weight:Weight,
    ppu:  PricePerUnit,
    begin:Date,
    end:  Date,
    comments: Text,
  ) : async Result<InventoryId,ServerErr> {
    if (not getModel().isValidPublicKey(#producer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().
      producerAddInventory(
        null, id, prod, quant, weight, ppu, begin, end, comments)
  };

  /**
   `producerUpdateInventory`
   ------------------------------------------

   */
  producerUpdateInventory(
    public_key: PublicKey,
    iid:  InventoryId,
    id:   ProducerId,
    prod: ProduceId,
    quant:Quantity,
    weight:Weight,
    ppu:  PricePerUnit,
    begin:Date,
    end:  Date,
    comments: Text,
  ) : async Result<(),ServerErr> {
    if (not getModel().isValidPublicKey(#producer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().
      producerUpdateInventory(
        iid, id, prod, quant, weight, ppu, begin, end, comments)
  };

  /**
   `producerRemInventory`
   ---------------------------
   */
  producerRemInventory(public_key: PublicKey, id:InventoryId) : async Result<(),ServerErr> {
    if (not getModel().isValidPublicKey(#producer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().producerRemInventory(id)
  };

  /**
   `producerAllInventoryInfo`
   ---------------------------
   */
  producerAllInventoryInfo(public_key: PublicKey, id:ProducerId) : async Result<[InventoryInfo],IdErr> {
    optionResult<[InventoryInfo],IdErr>(
      getModel().producerAllInventoryInfo(id),
      #idErr
    )
  };

  /**
   `producerAllReservationInfo`
   ---------------------------
   */
  producerAllReservationInfo(public_key: PublicKey, id:ProducerId) : async Result<[ReservedInventoryInfo],IdErr> {
    optionResult<[ReservedInventoryInfo],IdErr>(
      getModel().producerAllReservationInfo(id),
      #idErr
    )
  };


  /**
   Inventory and produce information
   ======================================
   Messages about produce and inventory

   */

  /**
   `produceMarketInfo`
   ---------------------------
   The last sales price for produce within a given geographic area; null region id means "all areas."
   */
  produceMarketInfo(public_key: PublicKey, id:ProduceId, reg:?RegionId) : async Result<[ProduceMarketInfo],IdErr> {
    optionResult<[ProduceMarketInfo],IdErr>(
      getModel().produceMarketInfo(id, reg),
      {#idErr}
    )
  };


  /**
   `allInventoryInfo`
   ---------------------------
   Get the information for all known inventory.
   */
  allInventoryInfo() : async [InventoryInfo] {
    getModel().inventoryTable.allInfo()
  };

  /**
   `getInventoryInfo`
   ---------------------------
   Get the information associated with inventory, based on its id.
   */
  getInventoryInfo(id:InventoryId) : async Result<InventoryInfo,IdErr> {
    optionResult<InventoryInfo,IdErr>(
      getModel().inventoryTable.getInfo(id),
      {#idErr}
    )
  };


  /**
   `Transporter`-based ingress messages:
   ===========================================
   */

  /**
   `transporterAddRoute`
   ---------------------------
   */
  transporterAddRoute(
    public_key: PublicKey,
    id:     TransporterId,
    rstart: RegionId,
    rend:   RegionId,
    start:  Date,
    end:    Date,
    cost:   Price,
    ttid:   TruckTypeId
  ) : async Result<RouteId,ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterAddRoute(null, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterUpdateRoute`
   ---------------------------
   */
  transporterUpdateRoute(
    public_key: PublicKey,
    route:  RouteId,
    id:     TransporterId,
    rstart: RegionId,
    rend:   RegionId,
    start:  Date,
    end:    Date,
    cost:   Price,
    ttid:   TruckTypeId
  ) : async Result<(),ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterUpdateRoute(route, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterRemRoute`
   ---------------------------
   */
  transporterRemRoute(public_key: PublicKey, id:RouteId) : async Result<(),ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterRemRoute(id)
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(public_key: PublicKey, id:TransporterId) : async Result<[RouteInfo],IdErr> {
    optionResult<[RouteInfo],IdErr>(
      getModel().transporterAllRouteInfo(id),
      #idErr
    )
  };

  /**
   `transporterAllReservationInfo`
   ---------------------------
   */
  transporterAllReservationInfo(public_key: PublicKey, id:TransporterId) : async Result<[ReservedRouteInfo],IdErr> {
    optionResult<[ReservedRouteInfo],IdErr>(
      getModel()
        .transporterAllReservationInfo(id),
      #idErr
    )
  };

  /**
   `allRouteInfo`
   ---------------------------
   Get the information for all known routes.
   */
  allRouteInfo() : async [RouteInfo] {
    getModel()
      .routeTable.allInfo()
  };

  /**
   `getRouteInfo`
   ---------------------

   See also: [server type `RouteInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#routeinfo).

   */

  getRouteInfo(
    id: RouteId
  ) : async Result<RouteInfo,IdErr> {
    optionResult<RouteInfo,IdErr>(
      getModel().routeTable.getInfo(id),
      {#idErr}
    )
  };

  /**
   `Retailer`-based ingress messages:
   ======================================

   `retailerQueryAll`
   ---------------------------

   */
  retailerQueryAll(public_key: PublicKey, id:RetailerId,
                   queryProduce:?ProduceId,
                   queryDate:?Date
  ) : async Result<QueryAllResults,IdErr> {
    optionResult<QueryAllResults,IdErr>(
      getModel().
        retailerQueryAll(id, queryProduce, queryDate),
      {#idErr}
    )
  };

  /**
   `retailerReserve`
   ---------------------------
   */
  retailerReserve(
    public_key: PublicKey,
    id:RetailerId,
    inventory:InventoryId,
    route:RouteId) : async Result<(ReservedInventoryId, ReservedRouteId),ServerErr>
  {
    if (not getModel().isValidPublicKey(#retailer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().
      retailerReserve(id, inventory, route)
  };

  /**
   `retailerReserveMany`
   ---------------------------
   */
  retailerReserveMany(
    public_key: PublicKey,
    id: RetailerId,
    list: [(InventoryId, RouteId)]
  )
    : async Result<[(ReservedInventoryId, ReservedRouteId)], ServerErr>
  {
    if (not getModel().isValidPublicKey(#retailer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().
      retailerReserveMany(id, list)
  };

  /**
   `retailerReservations`
   ---------------------------

  */
  retailerReservations(public_key: PublicKey, id:RetailerId) :
    async Result<[(ReservedInventoryInfo,
                   ReservedRouteInfo)],ServerErr>
  {
    optionResult<[(ReservedInventoryInfo,
                   ReservedRouteInfo)],ServerErr>(
      getModel().
        retailerAllReservationInfo(id),
      #idErr
    )
  };



  /**

   Developer-based ingress messages:
   ========================================================

   The following messages may originate from developers

   */

  /**
   `getCounts`
   ----------
   */

  getCounts() : async ProduceExchangeCounts {
    let m = getModel();
    shared {
      truck_type_count         = m.truckTypeTable.count();
      region_count             = m.regionTable.count();
      produce_count            = m.produceTable.count();
      inventory_count          = m.inventoryTable.count();
      reserved_inventory_count = m.reservedInventoryTable.count();
      producer_count           = m.producerTable.count();
      retailer_count           = m.retailerTable.count();
      transporter_count        = m.transporterTable.count();
      route_count              = m.routeTable.count();
      reserved_route_count     = m.reservedRouteTable.count();

      retailer_query_count     = m.retailerQueryCount;
      retailer_query_cost      = m.retailerQueryCost;
      retailer_join_count      = m.retailerJoinCount;
    }
  };

  /**
   `devViewGMV`
   -------------

   MVP:

   > Developer can see the GMV, the aggregate sum of how many sales have
been processed
*/

  devViewGMV() : async ?Nat {
    nyi()
  };

  /**
   `devViewQueries`
   ----------------

   MVP:

   > Developer can see how many aggregate queries have been made by all retailers

   */

  devViewQueries() : async ?Nat {
    ?getModel().retailerQueryCount;
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
 End of interface definition
-----------------------------------
  With the following closing brace, the interface of the `Server` is thusly defined.
 */
};// end: actor class `Server`
