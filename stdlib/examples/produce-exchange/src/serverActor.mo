/**

 [Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
 --------------------
*/

import Debug "mo:stdlib/Debug";
import P = "mo:stdlib/Prelude";
import Option = "mo:stdlib/Option";
import T = "serverTypes";
import L = "serverLang";
import Model = "serverModel";
import Result = "mo:stdlib/Result";

import Trie = "mo:stdlib/Trie";
import List = "mo:stdlib/List";

type List<T> = List.List<T>;

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
*/


/**

 `-User`
 =========
 Messages about users.


 `registrarAddUser`
 ----------------------
 Register a new user, who may play several roles in the exchange.

 The given `user_name` must be unique to the exchange; the operation fails otherwise.

 */

  public func registrarAddUser(
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
      #idErr null
    )
  };

  /**
   `allUserInfo`
   -------------
   Get info for all users.
   */
  public func allUserInfo() : async [T.UserInfo] {
    getModel().userTable.allInfo()
  };

  /**
   `getUserInfo`
   ---------------------------
   Get the information associated with a user, based on its id.
   */
  public func getUserInfo(id:T.UserId) : async Result<T.UserInfo, T.IdErr> {
    Result.fromOption<T.UserInfo, T.IdErr>(
      getModel().userTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `validateUser`
   ---------------------------
   Returns true if the user id matches the public key.
   */
  public func validateUser(public_key: T.PublicKey, id: T.UserId) : async Bool {
    getModel().isValidPublicKey(#user(id), public_key);
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

  public func registrarAddTruckType(
    short_name_:  Text,
    description_: Text,
    capacity_ : T.Weight,
    isFridge_ : Bool,
    isFreezer_ : Bool,
  ) : async Result<T.TruckTypeId,None> {
    Result.fromSome<T.TruckTypeId>(
      getModel()
        .truckTypeTable.addInfoGetId(
        func (id_:T.TruckTypeId) : T.TruckTypeInfo =

          // xxx: AS should have more concise syntax for this pattern, below:
          // two problems I see, that are separate:
          // 1: repeating the label/variable name, which is the same in each case, twice.
          // 2: explicit type annotations, because of "type error, cannot infer type of forward variable ..."
          //    but two other sources exist for each type: the type of `insert` is known, and hence, this record has a known type,
          //    and, the type of each of these `variables` is known, as well.

          {
            id=id_ :T.TruckTypeId;
            short_name=short_name_:Text;
            description=description_:Text;
            capacity=capacity_:T.Weight;
            isFridge=isFridge_:Bool;
            isFreezer=isFreezer_:Bool;
          })
    )
  };

  /**
   `registrarRemTruckType`
   ---------------------
   */

  public func registrarRemTruckType(
    id: T.TruckTypeId
  ) : async Result<(),T.ServerErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().truckTypeTable.remGetUnit(id),
      #idErr null
    )
  };

  /**
   `getTruckTypeInfo`
   ---------------------
   */

  public func getTruckTypeInfo(
    id: T.TruckTypeId
  ) : async Result<T.TruckTypeInfo,T.IdErr> {
    Result.fromOption<T.TruckTypeInfo,T.IdErr>(
      getModel().truckTypeTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `allTruckTypeInfo`
   ---------------------
   */

  public func allTruckTypeInfo() : async [T.TruckTypeInfo] {
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

  public func registrarAddRegion(
    short_name_:  Text,
    description_: Text,
  ) : async Result<T.RegionId,None> {
    Result.fromSome<T.RegionId>(
      getModel().regionTable.addInfoGetId(
        func (id_:T.RegionId) : T.RegionInfo =
          {
            id = id_:T.RegionId;
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

  public func registrarRemRegion(
    id: T.RegionId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().regionTable.remGetUnit(id),
      #idErr null,
    )
  };

  /**
   `getRegionInfo`
   ---------------------

   See also: [server type `RegionInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#regioninfo).

   */

  public func getRegionInfo(
    id: T.RegionId
  ) : async Result<T.RegionInfo,T.IdErr> {
    Result.fromOption<T.RegionInfo,T.IdErr>(
      getModel().regionTable.getInfo(id),
      #idErr null
    )
  };


  /**
   `allRegionInfo`
   ---------------------

   See also: [server type `RegionInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#regioninfo).

   */

  public func allRegionInfo() : async [T.RegionInfo] {
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

  public func registrarAddProduce(
    short_name_:  Text,
    description_: Text,
    grade_: T.Grade,
  ) : async Result<T.ProduceId,None> {
    Result.fromSome<T.ProduceId>(
      getModel().produceTable.addInfoGetId(
        func (id_:T.ProduceId) : T.ProduceInfo =
          {
            id = id_:T.ProduceId;
            short_name=short_name_:Text;
            description=description_:Text;
            grade=grade_:T.Grade
          })
    )
  };

  /**
   `registrarRemProduce`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  public func registrarRemProduce(
    id: T.ProduceId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().produceTable.remGetUnit(id),
      #idErr null,
    )
  };


  /**
   `getProduceInfo`
   ---------------------
   */

  public func getProduceInfo(
    id: T.ProduceId
  ) : async Result<T.ProduceInfo,T.IdErr> {
    Result.fromOption<T.ProduceInfo,T.IdErr>(
      getModel().produceTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `allProduceInfo`
   ---------------------
   */

  public func allProduceInfo() : async [T.ProduceInfo] {
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

  public func registrarAddProducer(
    producer_public_key : T.PublicKey,
    short_name_:  Text,
    description_: Text,
    region_: T.RegionId,
  ) : async Result<T.ProducerId,T.IdErr> {
    Result.fromOption<T.ProduceId,T.IdErr>(
      getModel().producerTable.addInfoGetId(
        func(id_:T.ProducerId):T.ProducerInfo {
          {
            id=id_:T.ProducerId;
            public_key=producer_public_key;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:T.RegionId;
            inventory=[];
            reserved=[];
          }
        }),
      #idErr null
    )
  };

  /**
   `registrarRemProducer`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  public func registrarRemProducer(
    id: T.ProducerId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().producerTable.remGetUnit(id),
      #idErr null
    )
  };


  /**
   `getProducerInfo`
   ---------------------
   */

  public func getProducerInfo(
    id: T.ProducerId
  ) : async Result<T.ProducerInfo,T.IdErr> {
    Result.fromOption<T.ProducerInfo,T.IdErr>(
      getModel().producerTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `allProducerInfo`
   ---------------------
   */

  public func allProducerInfo() : async [T.ProducerInfo] {
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

  public func registrarAddRetailer(
    retailer_public_key : T.PublicKey,
    short_name_:  Text,
    description_: Text,
    region_: T.RegionId,
  ) : async Result<T.RetailerId,T.IdErr> {
    Result.fromOption<T.RetailerId,T.IdErr>(
      getModel().retailerTable.addInfoGetId(
        func(id_:T.RetailerId):T.RetailerInfo {
          {
            id=id_:T.RetailerId;
            public_key=retailer_public_key;
            short_name=short_name_:Text;
            description=description_:Text;
            region=region_:T.RegionId
          }
        }),
      #idErr null
    )
  };

  /**
   `registrarRemRetailer`
   ---------------------

   returns `?()` on success, and `null` on failure.
   */

  public func registrarRemRetailer(
    id: T.RetailerId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().retailerTable.remGetUnit(id),
      #idErr null
    )
  };

  /**
   `getRetailerInfo`
   ---------------------
   */

  public func getRetailerInfo(
    id: T.RetailerId
  ) : async Result<T.RetailerInfo,T.IdErr> {
    Result.fromOption<T.RetailerInfo,T.IdErr>(
      getModel().retailerTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `allRetailerInfo`
   ---------------------
   */

  public func allRetailerInfo() : async [T.RetailerInfo] {
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
  public func registrarAddTransporter(
    transporter_public_key: T.PublicKey,
    short_name_:  Text,
    description_: Text,
  ) : async Result<T.TransporterId,()> {
    Result.fromOption<T.TransporterId,()>(
      getModel().transporterTable.addInfoGetId(
        func(id_:T.TransporterId):T.TransporterInfo {
          {
            id=id_:T.TransporterId;
            public_key=transporter_public_key;
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

  public func registrarRemTransporter(
    id: T.TransporterId
  ) : async Result<(),T.IdErr> {
    Result.fromOption<(),T.IdErr>(
      getModel().transporterTable.remGetUnit(id),
      #idErr null
    )
  };

  /**
   `getTransporterInfo`
   ---------------------
   */

  public func getTransporterInfo(
    id: T.TransporterId
  ) : async Result<T.TransporterInfo,T.IdErr> {
    Result.fromOption<T.TransporterInfo,T.IdErr>(
      getModel().transporterTable.getInfo(id),
      #idErr null
    )
  };


  /**
   `allTransporterInfo`
   ---------------------
   */

  public func allTransporterInfo() : async [T.TransporterInfo] {
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
  public func producerAddInventory(
    public_key: T.PublicKey,
    id:   T.ProducerId,
    prod: T.ProduceId,
    quant:T.Quantity,
    weight:T.Weight,
    ppu:  T.PricePerUnit,
    begin:T.Date,
    end:  T.Date,
    comments: Text,
  ) : async Result<T.InventoryId,T.ServerErr> {
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
  public func producerUpdateInventory(
    public_key: T.PublicKey,
    iid:  T.InventoryId,
    id:   T.ProducerId,
    prod: T.ProduceId,
    quant:T.Quantity,
    weight:T.Weight,
    ppu:  T.PricePerUnit,
    begin:T.Date,
    end:  T.Date,
    comments: Text,
  ) : async Result<(),T.ServerErr> {
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
  public func producerRemInventory(public_key: T.PublicKey, id:T.InventoryId) : async Result<(),T.ServerErr> {
    if (not getModel().isValidPublicKey(#producer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().producerRemInventory(id)
  };

  /**
   `producerAllInventoryInfo`
   ---------------------------
   */
  public func producerAllInventoryInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.InventoryInfo],T.IdErr> {
    Result.fromOption<[T.InventoryInfo],T.IdErr>(
      getModel()
        .producerAllInventoryInfo(id),
      #idErr null
    )
  };

  /**
   `producerAllReservationInfo`
   ---------------------------
   */
  public func producerReservations(public_key: T.PublicKey, id:T.UserId) : async Result<[T.ReservedInventoryInfo],T.IdErr> {
    Result.fromOption<[T.ReservedInventoryInfo],T.IdErr>(
      getModel()
        .producerAllReservationInfo(id),
      #idErr null
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
  public func produceMarketInfo(public_key: T.PublicKey, id:T.ProduceId, reg:?T.RegionId) : async Result<[T.ProduceMarketInfo],T.IdErr> {
    Result.fromOption<[T.ProduceMarketInfo],T.IdErr>(
      getModel()
        .produceMarketInfo(id, reg),
      #idErr null
    )
  };


  /**
   `allInventoryInfo`
   ---------------------------
   Get the information for all known inventory.
   */
  public func allInventoryInfo() : async [T.InventoryInfo] {
    getModel()
      .inventoryTable.allInfo()
  };

  /**
   `getInventoryInfo`
   ---------------------------
   Get the information associated with inventory, based on its id.
   */
  public func getInventoryInfo(id:T.InventoryId) : async Result<T.InventoryInfo,T.IdErr> {
    Result.fromOption<T.InventoryInfo,T.IdErr>(
      getModel()
        .inventoryTable.getInfo(id),
      #idErr null
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
  public func transporterAddRoute(
    public_key: T.PublicKey,
    id:     T.TransporterId,
    rstart: T.RegionId,
    rend:   T.RegionId,
    start:  T.Date,
    end:    T.Date,
    cost:   T.Price,
    ttid:   T.TruckTypeId
  ) : async Result<T.RouteId,T.ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterAddRoute(null, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterUpdateRoute`
   ---------------------------
   */
  public func transporterUpdateRoute(
    public_key: T.PublicKey,
    route:  T.RouteId,
    id:     T.TransporterId,
    rstart: T.RegionId,
    rend:   T.RegionId,
    start:  T.Date,
    end:    T.Date,
    cost:   T.Price,
    ttid:   T.TruckTypeId
  ) : async Result<(),T.ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterUpdateRoute(route, id, rstart, rend, start, end, cost, ttid)
  };

  /**
   `transporterRemRoute`
   ---------------------------
   */
  public func transporterRemRoute(public_key: T.PublicKey, id:T.RouteId) : async Result<(),T.ServerErr> {
    if (not getModel().isValidPublicKey(#transporter(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    getModel().transporterRemRoute(id)
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  public func transporterAllRouteInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.RouteInfo],T.IdErr> {
    Result.fromOption<[T.RouteInfo],T.IdErr>(
      getModel()
        .transporterAllRouteInfo(id),
      #idErr null
    )
  };

  /**
   `transporterAllReservationInfo`
   ---------------------------
   */
  public func transporterAllReservationInfo(public_key: T.PublicKey, id:T.UserId) : async Result<[T.ReservedRouteInfo],T.IdErr> {
    Result.fromOption<[T.ReservedRouteInfo],T.IdErr>(
      getModel()
        .transporterAllReservationInfo(id),
      #idErr null
    )
  };

  /**
   `allRouteInfo`
   ---------------------------
   Get the information for all known routes.
   */
  public func allRouteInfo() : async [T.RouteInfo] {
    getModel()
      .routeTable.allInfo()
  };

  /**
   `allReservedRouteInfo`
   ---------------------------
   Get the information for all reserved routes.
   */
  public func allReservedRouteInfo() : async [T.ReservedRouteInfo] {
    getModel()
      .reservedRouteTable.allInfo()
  };

  /**
   `getRouteInfo`
   ---------------------

   See also: [server type `RouteInfo`]($DOCURL/examples/produce-exchange/serverTypes.md#routeinfo).

   */

  public func getRouteInfo(
    id: T.RouteId
  ) : async Result<T.RouteInfo,T.IdErr> {
    Result.fromOption<T.RouteInfo,T.IdErr>(
      getModel().routeTable.getInfo(id),
      #idErr null
    )
  };

  /**
   `Retailer`-based ingress messages:
   ======================================

   `retailerQueryAll`
   ---------------------------

   */
  public func retailerQueryAll(
    public_key: T.PublicKey,
    id:T.RetailerId,
    queryProduce:?T.ProduceId,
    queryDate:?T.Date
  ) : async Result<T.QueryAllResults,T.IdErr> {
    Result.fromOption<T.QueryAllResults,T.IdErr>(
      getModel().
        retailerQueryAll(id, queryProduce, queryDate),
      #idErr null
    )
  };

  /**
   `retailerReserve`
   ---------------------------
   */
  public func retailerReserve(
    public_key: T.PublicKey,
    id:T.RetailerId,
    inventory:T.InventoryId,
    route:T.RouteId) : async Result<(T.ReservedInventoryId, T.ReservedRouteId),T.ServerErr>
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
  public func retailerReserveMany(
    public_key: T.PublicKey,
    id: T.RetailerId,
    list: [(T.InventoryId, T.RouteId)]
  )
    : async Result<[Result<(T.ReservedInventoryId, T.ReservedRouteId), T.ServerErr>], T.ServerErr>
  {
    if (not getModel().isValidPublicKey(#retailer(id), public_key)) {
      return (#err(#publicKeyErr))
    };
    #ok(getModel().retailerReserveMany(id, list))
  };

  /**
   `retailerReservations`
   ---------------------------

  */
  public func retailerReservations(public_key: T.PublicKey, id:T.RetailerId) :
    async Result<[(T.ReservedInventoryInfo,
                   T.ReservedRouteInfo)],T.ServerErr>
  {
    Result.fromOption<[(T.ReservedInventoryInfo,
                   T.ReservedRouteInfo)],T.ServerErr>(
      getModel().
        retailerAllReservationInfo(id),
      #idErr null
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

  public func getCounts() : async T.ProduceExchangeCounts {
    let m = getModel();
    {
      hash_bit_length          = 0;
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

      retailer_query_size_max  = m.retailerQuerySizeMax;
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

  public func devViewGMV() : async ?Nat {
    P.nyi()
  };

  /**
   `devViewQueries`
   ----------------

   MVP:

   > Developer can see how many aggregate queries have been made by all retailers

   */

  public func devViewQueries() : async ?Nat {
    ?getModel().retailerQueryCount;
  };


  /**
   `devViewReservations`
   ----------------------

   MVP:

   > Developer can see how many aggregate sales orders have been made by all retailers

   */

  public func devViewReservations() : async Nat {
    getModel().reservedInventoryTable.count()
  };

  /**
   `devViewProducers`
   -------------------

   MVP:

   > Developer can see how many producers in the system and how many goods each has

   See also [`producerInfo`](#producerinfo).

   */

  public func devViewProducers() : async [T.ProducerInfo] {
    getModel().producerTable.allInfo()
  };


  /**
   `devViewTransporters`
   -------------------

   MVP:

   > Developer can see how many transporters in the system and how many routes each has

   See also [`transporterInfo`](#transporterinfo).

   */

  public func devViewTransporters() : async [T.TransporterInfo] {
    getModel().transporterTable.allInfo()
  };

  /**
   `devViewRetailers`
   -------------------

   MVP:

   > Developer can see how many retailers in the system and how many queries and how many sales orders

   See also [`retailerInfo`](#retailerinfo).

   */

  public func devViewRetailers() : async [T.RetailerInfo] {
    getModel().retailerTable.allInfo()
  };


  /**
   `evalBulk`
   -------------------
   evaluate a collection of API calls (a "bulk request"), represented as an AS datatype.
   */

  public func evalBulkArray(reqs:[L.BulkReq]) : async [L.BulkResp] {
    getModel().evalBulkArray(reqs)
  };


  public func loadWorkload(params:T.WorkloadParams) : () {
    func db(s:Text) = if false {Debug.print "Model::loadWorkload: "; Debug.print s; Debug.print "\n"};
    getModel().loadWorkload(params)
  };

  /**
   Standard workloads
   =========================
   */

  public func devTestLoadQuery (region_count:Nat, scale_factor:Nat) {
    func scaledParams(region_count_:Nat, factor:Nat) : T.WorkloadParams = {
      {
        region_count        = region_count_:Nat;
        day_count           = 3:Nat;
        max_route_duration  = 1:Nat;
        producer_count      = region_count * factor:Nat;
        transporter_count   = region_count * factor:Nat;
        retailer_count      = region_count * factor:Nat;
      }
    };
    let m = Model.Model();
    let _ = m.loadWorkload(scaledParams(5, 2));
    let _ = m.retailerQueryAll(0, null, null);
  };

  ///////////////////////////////////////////////////////////////////////////

  // See `serverModel.mo` for the Model class's implementation

  // Matthew-Says:
  // There are two initialization options for the model field:
  // 1. Call Model() directly
  // 2. Call Model() later, when we try to access the model field.
  //
  // Option 1 is simpler, but option 2 permits a "static" class or actor definition,
  // which is needed in some contexts.

  //private var model : ?Model.Model = null;


  var model : ?Model.Model = ?(Model.Model());

  func getModel() : Model.Model {
    switch model {
    case (null) {
           let m = Model.Model();
           model := ?m;
           m
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
