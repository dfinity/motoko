/**

 [Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
 --------------------
*/

let P = (import "../../prelude.as");
let T = (import "serverTypes.as");
let Model = (import "serverModel.as");
let Result = (import "../../result.as");

type Result<Ok,Err> = Result.Result<Ok,Err>;

actor class Server () {

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
    public_key: T.PublicKey,
    user_name: Text,
    description: Text,
    region: T.RegionId,
    isDeveloper: Bool,
    isProducer: Bool,
    isRetailer: Bool,
    isTransporter: Bool
  ) : async Result<T.UserId,T.IdErr> {
    Result.fromOption<T.UserId,T.IdErr>(
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
  allUserInfo() : async [T.UserInfo] {
    getModel().userTable.allInfo()
  };

  /**
   `getUserInfo`
   ---------------------------
   Get the information associated with a user, based on its id.
   */
  getUserInfo(id:T.UserId) : async Result<T.UserInfo, T.IdErr> {
    Result.fromOption<T.UserInfo, T.IdErr>(
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
    capacity_ : T.Weight,
    isFridge_ : Bool,
    isFreezer_ : Bool,
  ) : async Result<T.TruckTypeId, ()> {
    Result.fromOption<T.TruckTypeId,()>(
      getModel()
        .truckTypeTable.addInfoGetId(
        func (id_:T.TruckTypeId) : T.TruckTypeInfo =
          
          // xxx: AS should have more concise syntax for this pattern, below:
          // two problems I see, that are separate:
          // 1: repeating the label/variable name, which is the same in each case, twice.
          // 2: explicit type annotations, because of "type error, cannot infer type of forward variable ..."
          //    but two other sources exist for each type: the type of `insert` is known, and hence, this record has a known type,
          //    and, the type of each of these `variables` is known, as well.
          
          shared {
            id=id_ :T.TruckTypeId;
            short_name=short_name_:Text;
            description=description_:Text;
            capacity=capacity_:T.Weight;
            isFridge=isFridge_:Bool;
            isFreezer=isFreezer_:Bool;
          }),
      ()
    )
  };

  /**
   `registrarRemTruckType`
   ---------------------
   */

  registrarRemTruckType(
    id: T.TruckTypeId
  ) : async Result<(),T.ServerErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().truckTypeTable.remGetUnit(id),
      {#idErr}
    )    
  };

  /**
   `getTruckTypeInfo`
   ---------------------
   */

  getTruckTypeInfo(
    id: T.TruckTypeId
  ) : async Result<T.TruckTypeInfo,T.IdErr> {
    Result.fromOption<T.TruckTypeInfo,T.IdErr>(
      getModel().truckTypeTable.getInfo(id),
      {#idErr}
    )
  };

  /**
   `allTruckTypeInfo`
   ---------------------
   */

  allTruckTypeInfo() : async [T.TruckTypeInfo] {
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
  ) : async Result<T.RegionId,()> {
    Result.fromOption<T.RegionId,()>(
      getModel().regionTable.addInfoGetId(
        func (id_:T.RegionId) : T.RegionInfo =
          shared {
            id = id_:T.RegionId;
            short_name=short_name_:Text;
            description=description_:Text
          }),
      ()
    )
  };

  /**
   `registrarRemRegion`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemRegion(
    id: T.RegionId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
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
    id: T.RegionId
  ) : async Result<T.RegionInfo,T.IdErr> {
    Result.fromOption<T.RegionInfo,T.IdErr>(
      getModel().regionTable.getInfo(id),
      {#idErr}
    )
  };


  /**
   `allRegionInfo`
   ---------------------

   See also: [server type `RegionInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#regioninfo).

   */

  allRegionInfo() : async [T.RegionInfo] {
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
    grade_: T.Grade,
  ) : async Result<T.ProduceId,()> {
    Result.fromOption<T.ProduceId,()>(
      getModel().produceTable.addInfoGetId(
        func (id_:T.ProduceId) : T.ProduceInfo =
          shared {
            id = id_:T.ProduceId;
            short_name=short_name_:Text;
            description=description_:Text;
            grade=grade_:T.Grade
          }),
      ()
    )
  };

  /**
   `registrarRemProduce`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  registrarRemProduce(
    id: T.ProduceId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().produceTable.remGetUnit(id),
      {#idErr},
    )
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  getProduceInfo(
    id: T.ProduceId
  ) : async Result<T.ProduceInfo,T.IdErr> {
    Result.fromOption<T.ProduceInfo,T.IdErr>(
      getModel().produceTable.getInfo(id),
      {#idErr}
    )
  };

  /**
   `allProduceInfo`
   ---------------------
   */

  allProduceInfo() : async [T.ProduceInfo] {
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
    short_name_:  Text,
    description_: Text,
    region_: T.RegionId,
  ) : async Result<T.ProducerId,T.IdErr> {
    Result.fromOption<T.ProduceId,T.IdErr>(
      getModel().producerTable.addInfoGetId(
        func(id_:T.ProducerId):T.ProducerInfo {
          shared {
            id=id_:T.ProducerId;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:T.RegionId;
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
    id: T.ProducerId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().producerTable.remGetUnit(id),
      {#idErr}
    )
  };


  /**
   `getProducerInfo`
   ---------------------
   */

  getProducerInfo(
    id: T.ProducerId
  ) : async Result<T.ProducerInfo,T.IdErr> {
    Result.fromOption<T.ProducerInfo,T.IdErr>(
      getModel().producerTable.getInfo(id),
      {#idErr}
    )  
  };

  /**
   `allProducerInfo`
   ---------------------
   */

  allProducerInfo() : async [T.ProducerInfo] {    
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
    short_name_:  Text,
    description_: Text,
    region_: T.RegionId,
  ) : async Result<T.RetailerId,T.IdErr> {
    Result.fromOption<T.RetailerId,T.IdErr>(
      getModel().retailerTable.addInfoGetId(
        func(id_:T.RetailerId):T.RetailerInfo {
          shared {
            id=id_:T.RetailerId;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:T.RegionId
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
    id: T.RetailerId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().retailerTable.remGetUnit(id),
      {#idErr}
    )    
  };

  /**
   `getRetailerInfo`
   ---------------------
   */

  getRetailerInfo(
    id: T.RetailerId
  ) : async Result<T.RetailerInfo,T.IdErr> {
    Result.fromOption<T.RetailerInfo,T.IdErr>(
      getModel().retailerTable.getInfo(id),
      {#idErr}
    )
  };

  /**
   `allRetailerInfo`
   ---------------------
   */

  allRetailerInfo() : async [T.RetailerInfo] {
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
    short_name_:  Text,
    description_: Text,
  ) : async Result<T.TransporterId,()> {
    Result.fromOption<T.TransporterId,()>(
      getModel().transporterTable.addInfoGetId(
        func(id_:T.TransporterId):T.TransporterInfo {
          shared {
            id=id_:T.TransporterId;
            short_name=short_name_:Text;
            description=description_:Text;
            routes=[];
            reserved=[];
          }
        }),
      ()
    )
  };

  /**
   `registrarRemTransporter`
   ---------------------

   */

  registrarRemTransporter(
    id: T.TransporterId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().transporterTable.remGetUnit(id),
      {#idErr}
    )
  };

  /**
   `getTransporterInfo`
   ---------------------
   */

  getTransporterInfo(
    id: T.TransporterId
  ) : async Result<T.TransporterInfo,T.IdErr> {
    Result.fromOption<T.TransporterInfo,T.IdErr>(
      getModel().transporterTable.getInfo(id),
      {#idErr}
    )
  };


  /**
   `allTransporterInfo`
   ---------------------
   */

  allTransporterInfo() : async [T.TransporterInfo] {
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
    public_key: T.PublicKey,
    id:   T.UserId,
    prod: T.ProduceId,
    quant:T.Quantity,
    weight:T.Weight,
    ppu:  T.PricePerUnit,
    begin:T.Date,
    end:  T.Date,
    comments: Text,
  ) : async Result<T.InventoryId,T.ServerErr> {
    getModel().
      producerAddInventory(
        public_key, null, id, prod, quant, weight, ppu, begin, end, comments)
  };

  /**
   `producerUpdateInventory`
   ------------------------------------------

   */
  producerUpdateInventory(
    public_key: T.PublicKey,
    iid:  T.InventoryId,
    id:   T.UserId,
    prod: T.ProduceId,
    quant:T.Quantity,
    weight:T.Weight,
    ppu:  T.PricePerUnit,
    begin:T.Date,
    end:  T.Date,
    comments: Text,
  ) : async Result<(),T.ServerErr> {
    getModel().
      producerUpdateInventory(
        public_key, iid, id, prod, quant, weight, ppu, begin, end, comments)
  };

  /**
   `producerRemInventory`
   ---------------------------
   */
  producerRemInventory(public_key: T.PublicKey, id:T.InventoryId) : async Result<(),T.ServerErr> {
    getModel()
      .producerRemInventory(public_key, id)
  };

  /**
   `producerAllInventoryInfo`
   ---------------------------
   */
  producerAllInventoryInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.InventoryInfo],T.IdErr> {
    Result.fromOption<[T.InventoryInfo],T.IdErr>(
      getModel()
        .producerAllInventoryInfo(public_key, id),
      {#idErr}
    )
  };

  /**
   `producerReservations`
   ---------------------------
   */
  producerReservations(public_key: T.PublicKey, id:T.UserId) : async Result<[T.ReservedInventoryInfo],T.IdErr> {
    Result.fromOption<[T.ReservedInventoryInfo],T.IdErr>(
      getModel()
        .producerReservations(public_key, id),
      {#idErr}
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
  produceMarketInfo(public_key: T.PublicKey, id:T.ProduceId, reg:?T.RegionId) : async Result<[T.ProduceMarketInfo],T.IdErr> {
    Result.fromOption<[T.ProduceMarketInfo],T.IdErr>(
      getModel()
        .produceMarketInfo(public_key, id, reg),
      {#idErr}
    )
  };


  /**
   `allInventoryInfo`
   ---------------------------
   Get the information for all known inventory.
   */
  allInventoryInfo() : async [T.InventoryInfo] {
    getModel()
      .inventoryTable.allInfo()
  };

  /**
   `getInventoryInfo`
   ---------------------------
   Get the information associated with inventory, based on its id.
   */
  getInventoryInfo(id:T.InventoryId) : async Result<T.InventoryInfo,T.IdErr> {
    Result.fromOption<T.InventoryInfo,T.IdErr>(
      getModel()
        .inventoryTable.getInfo(id),
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
    public_key: T.PublicKey,
    id:  T.UserId,
    rstart: T.RegionId,
    rend:   T.RegionId,
    start:  T.Date,
    end:    T.Date,
    cost:   T.Price,
    ttid:   T.TruckTypeId
  ) : async Result<T.RouteId,T.ServerErr> {
    getModel().transporterAddRoute(public_key, null, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterUpdateRoute`
   ---------------------------
   */
  transporterUpdateRoute(
    public_key: T.PublicKey,
    route:  T.RouteId,
    id:  T.UserId,
    rstart: T.RegionId,
    rend:   T.RegionId,
    start:  T.Date,
    end:    T.Date,
    cost:   T.Price,
    ttid:   T.TruckTypeId
  ) : async Result<(),T.ServerErr> {
    getModel().transporterUpdateRoute(public_key, route, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterRemRoute`
   ---------------------------
   */
  transporterRemRoute(public_key: T.PublicKey, id:T.RouteId) : async Result<(),T.ServerErr> {
      getModel()
        .transporterRemRoute(public_key, id)
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.RouteInfo],T.IdErr> {
    Result.fromOption<[T.RouteInfo],T.IdErr>(
      getModel()
        .transporterAllRouteInfo(public_key, id),
      {#idErr}
    )
  };

  /**
   `transporterAllReservationInfo`
   ---------------------------
   */
  transporterAllReservationInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.ReservedRouteInfo],T.IdErr> {
    Result.fromOption<[T.ReservedRouteInfo],T.IdErr>(
      getModel()
        .transporterAllReservationInfo(public_key, id),
      {#idErr}
    )
  };

  /**
   `allRouteInfo`
   ---------------------------
   Get the information for all known routes.
   */
  allRouteInfo() : async [T.RouteInfo] {
    getModel()
      .routeTable.allInfo()
  };

  /**
   `getRouteInfo`
   ---------------------

   See also: [server type `RouteInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#routeinfo).

   */

  getRouteInfo(
    id: T.RouteId
  ) : async Result<T.RouteInfo,T.IdErr> {
    Result.fromOption<T.RouteInfo,T.IdErr>(
      getModel().routeTable.getInfo(id),
      {#idErr}
    )
  };

  /**
   `Retailer`-based ingress messages:
   ======================================

   `retailerQueryAll`
   ---------------------------

   TODO-Cursors (see above).

   */
  retailerQueryAll(public_key: T.PublicKey, id:T.UserId) : async Result<T.QueryAllResults,T.IdErr> {
    Result.fromOption<T.QueryAllResults,T.IdErr>(
      getModel().
        retailerQueryAll(public_key, id),
      {#idErr}
    )
  };

  /**
   `retailerQueryDates`
   ---------------------------

   Retailer queries available produce by delivery date range; returns
   a list of inventory items that can be delivered to that retailer's
   geography within that date.

   */
  retailerQueryDates(
    public_key: T.PublicKey,
    id:T.UserId,
    begin:T.Date,
    end:T.Date
  ) : async Result<[T.InventoryInfo],T.IdErr>
  {
    Result.fromOption<[T.InventoryInfo],T.IdErr>(
      getModel().
        retailerQueryDates(public_key, id, begin, end),
      {#idErr}
    )
  };

  /**
   `retailerReserve`
   ---------------------------
   */
  retailerReserve(
    public_key: T.PublicKey,
    id:T.UserId,
    inventory:T.InventoryId,
    route:T.RouteId) : async Result<(T.ReservedInventoryId, T.ReservedRouteId),()>
  {
    Result.fromOption<(T.ReservedInventoryId, T.ReservedRouteId),()>(
      getModel().
        retailerReserve(public_key, id, inventory, route),
      ()
    )
  };

  /**
   `retailerReservations`
   ---------------------------

   TODO-Cursors (see above).

   */
  retailerReservations(public_key: T.PublicKey, id:T.UserId) :
    async Result<[(T.ReservedInventoryInfo,
                   T.ReservedRouteInfo)],()>
  {
    Result.fromOption<[(T.ReservedInventoryInfo,
                   T.ReservedRouteInfo)],()>(
      getModel().
        retailerAllReservationInfo(public_key, id),
      ()
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

  getCounts() : async T.ProduceExchangeCounts {
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
    P.nyi()
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

  devViewProducers() : async [T.ProducerInfo] {
    getModel().producerTable.allInfo()
  };


  /**
   `devViewTransporters`
   -------------------

   MVP:

   > Developer can see how many transporters in the system and how many routes each has

   See also [`transporterInfo`](#transporterinfo).

   */

  devViewTransporters() : async [T.TransporterInfo] {
    getModel().transporterTable.allInfo()
  };

  /**
   `devViewRetailers`
   -------------------

   MVP:

   > Developer can see how many retailers in the system and how many queries and how many sales orders

   See also [`retailerInfo`](#retailerinfo).

   */

  devViewRetailers() : async [T.RetailerInfo] {
    getModel().retailerTable.allInfo()
  };


  ///////////////////////////////////////////////////////////////////////////
  // @Omit:

  // See `serverModel.as` for the Model class's implementation

  // Matthew-Says:
  // There are two initialization options for the model field:
  // 1. Call Model() directly; using this option now.
  // 2. Call Model() later, when we try to access the model field.

  private var model : ?Model.Model = null;

  private getModel() : Model.Model {
    switch model {
    case (null) {
           let m = Model.Model();
           model := ?m; m
         };
    case (?m) m;
    }
  };

/**
 End of interface definition
-----------------------------------
  With the following closing brace, the interface of the `Server` is thusly defined.
 */
};// end: actor class `Server`
