
/**

[Background]($DOCURL/examples/produce-exchange#Produce-Exchange-Standards-Specification)
--------------------

Server Model
===============================

**[`Server` actor class]($DOCURL/examples/produce-exchange/serverActor.html)**
defines an interface for messages sent by all participants, and the responses received in return.


Here, we depart from defining messages and their data types, and
instead turn our attention to the _internal representation_ of the
server actor's state, defined by the **[server model
types]($DOCURL/examples/produce-exchange/serverModelTypes.html)**,
and the _outer behavior_ of this `Server` actor.  The latter behavior
is part of the standards definition, and the internal type definitions that it
uses are is not.

*/


let P = (import "../../prelude.as");

let T = (import "serverTypes.as");
let M = (import "serverModelTypes.as");

let Hash = (import "../../hash.as").BitVec;
type Hash = Hash.t;

let Option = (import "../../option.as");
let Trie = (import "../../trie.as");

type Trie<K,V> = Trie.Trie<K,V>;
type Key<K> = Trie.Key<K>;

type Table<K,V> = Trie.Trie<K,V>;
let Table = (import "../../trie.as");

type Map<K,V> = Trie.Trie<K,V>;
let Map = (import "../../trie.as");

let DT = (import "../../docTable.as");
let DocTable = DT.DocTable;
type DocTable<X,Y,Z> = DT.DocTable<X,Y,Z>;

let Result = (import "../../result.as");
type Result<Ok,Err> = Result.Result<Ok,Err>;

type RouteInventoryMap = Trie<(T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)>;

class Model() {




  /**
   Misc helpers
   ==================
   */

  private debug (t:Text)   { print t };
  private debugInt (i:Int) { printInt i };

  private debugOff (t:Text)   {  };
  private debugIntOff (i:Int) {  };

  private idIsEq(x:Nat,y:Nat):Bool { x == y };

  private textIsEq(x:Text,y:Text):Bool { x == y };

  private idPairIsEq(x:(Nat,Nat),y:(Nat,Nat)):Bool { x.0 == y.0 and x.1 == y.1 };

  private idHash(x:Nat):Hash { Hash.hashOfInt(x) };

  private idPairHash(x:(Nat,Nat)):Hash { Hash.hashOfIntAcc(Hash.hashOfInt(x.0), x.1) };

  private keyOf(x:Nat):Key<Nat> {
    new { key = x ; hash = idHash(x) }
  };

  private keyOfIdPair(x:Nat, y:Nat):Key<(Nat,Nat)> {
    new { key = (x,y) ; hash = idPairHash(x,y) }
  };

  private keyOfText(x:Text):Key<Text> {
    new { key = x ; hash = Hash.hashOfText(x) }
  };

  /**
   Misc counters
   ==================
   */

  var joinCount = 0;


/**

Representation
=================

We use several public-facing **tables**, implemented as document tables.


CRUD operations via [document tables]($DOCURL/docTable.html)
----------------------------------------------------

This server model provides [document table]($DOCURL/docTable.html) objects to hold the
following kinds of entities in the exchange:

- **Static resource information:** truck types, produce types and region information.
- **Participant information:** producers, retailers and transporters.
- **Dynamic resource information:** inventory, routes and reservations.

For each of the entity kinds listed above, we have a corresponding
`DocTable` defined below that affords ordinary CRUD
(create-read-update-delete) operations.


Secondary maps
----------------------

See also [indexing by `RegionId`](#indexing-by-regionid).

The secondary maps and intra-document maps enable faster query
performance.

When we update the primary tables, we update any associated
secondary indices maps and intra-document maps as well, to keep them
all in sync.

**To do:** We initialize the primary tables with callbacks that
refer to the secondary maps; the callbacks intercept add/remove
operations to maintain consistency between the primary tables and the
secondary maps.

*/

  /**
   `userTable`
   -----------------
   */

  var userTable : M.UserTable =
    DocTable<T.UserId, M.UserDoc, T.UserInfo>(
    0,
    func(x:T.UserId):T.UserId{x+1},
    func(x:T.UserId,y:T.UserId):Bool{x==y},
    idHash,
    func(doc:M.UserDoc):T.UserInfo = shared {
      id=doc.id;
      user_name=doc.user_name;
      public_key=doc.public_key;
      description=doc.description;
      region=doc.region;
      producerId=doc.producerId;
      transporterId=doc.transporterId;
      retailerId=doc.retailerId;
      isDeveloper=doc.isDeveloper;
    },
    func(info:T.UserInfo):?M.UserDoc = ?(new {
      id=info.id;
      user_name=info.user_name;
      public_key=info.public_key;
      description=info.description;
      region=info.region;
      producerId=info.producerId;
      transporterId=info.transporterId;
      retailerId=info.retailerId;
      isDeveloper=info.isDeveloper;
    }),
  );


  /**
   `truckTypeTable`
   -----------------
   */

  var truckTypeTable : M.TruckTypeTable =
    DocTable<T.TruckTypeId, M.TruckTypeDoc, T.TruckTypeInfo>(
    0,
    func(x:T.TruckTypeId):T.TruckTypeId{x+1},
    func(x:T.TruckTypeId,y:T.TruckTypeId):Bool{x==y},
    idHash,
    func(doc:M.TruckTypeDoc):T.TruckTypeInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      capacity=doc.capacity;
      isFridge=doc.isFridge;
      isFreezer=doc.isFreezer;
    },
    func(info:T.TruckTypeInfo):?M.TruckTypeDoc = ?(new {
      id=info.id;
      short_name=info.short_name;
      description=info.description;
      capacity=info.capacity;
      isFridge=info.isFridge;
      isFreezer=info.isFreezer;
    }),
  );

  /**
   `regionTable`
   -----------------
   */

  var regionTable : M.RegionTable =
    DocTable<T.RegionId, M.RegionDoc, T.RegionInfo>(
    0,
    func(x:T.RegionId):T.RegionId{x+1},
    func(x:T.RegionId,y:T.RegionId):Bool{x==y},
    idHash,
    func(doc:M.RegionDoc):T.RegionInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
    },
    func(info:T.RegionInfo):?M.RegionDoc = ?(new {
      id=info.id;
      short_name=info.short_name;
      description=info.description;
    }),
  );

  /**
   `produceTable`
   -----------------
   */

  var produceTable : M.ProduceTable =
    DocTable<T.ProduceId, M.ProduceDoc, T.ProduceInfo>(
    0,
    func(x:T.ProduceId):T.ProduceId{x+1},
    func(x:T.ProduceId,y:T.ProduceId):Bool{x==y},
    idHash,
    func(doc:M.ProduceDoc):T.ProduceInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      grade=doc.grade;
    },
    func(info:T.ProduceInfo):?M.ProduceDoc = ?(new {
      id=info.id;
      short_name=info.short_name;
      description=info.description;
      grade=info.grade;
    }),
  );

  /**
   `producerTable`
   -----------------
   */

  var producerTable : M.ProducerTable =
    DocTable<T.ProducerId, M.ProducerDoc, T.ProducerInfo>(
    0,
    func(x:T.ProducerId):T.ProducerId{x+1},
    func(x:T.ProducerId,y:T.ProducerId):Bool{x==y},
    idHash,
    func(doc:M.ProducerDoc):T.ProducerInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      region=doc.region.id;
      inventory=[];
      reserved=[];
    },
    func(info:T.ProducerInfo):?M.ProducerDoc =
      Option.map<M.RegionDoc, M.ProducerDoc>(
        regionTable.getDoc(info.region),
        func (regionDoc: M.RegionDoc): M.ProducerDoc = new {
          id=info.id;
          short_name=info.short_name;
          description=info.description;
          region=regionDoc;
          inventory=Table.empty<T.InventoryId, M.InventoryDoc>();
          reserved=Table.empty<T.ReservedInventoryId, M.ReservedInventoryDoc>();
        }
      )
    );


  /**
   `inventoryTable`
   ---------------
   */

  var inventoryTable : M.InventoryTable =
    DocTable<T.InventoryId, M.InventoryDoc, T.InventoryInfo>(
    0,
    func(x:T.InventoryId):T.InventoryId{x+1},
    func(x:T.InventoryId,y:T.InventoryId):Bool{x==y},
    idHash,
    func(doc:M.InventoryDoc):T.InventoryInfo = shared {
      id=doc.id;
      produce=doc.produce.id;
      producer=doc.producer;
      quantity=doc.quantity;
      weight=doc.weight;
      ppu=doc.ppu;
      start_date=doc.start_date;
      end_date=doc.end_date;
      comments=doc.comments;
    },
    func(info:T.InventoryInfo):?M.InventoryDoc = {
      // validate the info's producer and produce ids
      switch (producerTable.getDoc(info.producer),
              produceTable.getDoc(info.produce)) {
        case (?producerDoc, ?produceDoc) {
               ?(new {
                   id=info.id;
                   produce=produceDoc;
                   producer=producerDoc.id;
                   quantity=info.quantity;
                   weight=info.weight;
                   ppu=info.ppu;
                   start_date=info.start_date;
                   end_date=info.end_date;
                   comments=info.comments;
                 })
             };
        case _ {
               null
             }
      }}
    );


  /**
   `transporterTable`
   -----------------
   */

  var transporterTable : M.TransporterTable =
    DocTable<T.TransporterId, M.TransporterDoc, T.TransporterInfo> (
      0,
      func(x:T.TransporterId):T.TransporterId{x+1},
      func(x:T.TransporterId,y:T.TransporterId):Bool{x==y},
      idHash,
      func(doc:M.TransporterDoc):T.TransporterInfo = shared {
        id=doc.id;
        short_name=doc.short_name;
        description=doc.description;
        routes=[];
        reserved=[];
      },
      func(info:T.TransporterInfo):?M.TransporterDoc =
        ?(new {
            id=info.id;
            short_name=info.short_name;
            description=info.description;
            routes=Table.empty<T.RouteId, M.RouteDoc>();
            reserved=Table.empty<T.ReservedRouteId, M.ReservedRouteDoc>();
          })
    );

  /**
   `retailerTable`
   -----------------
   */

  var retailerTable : M.RetailerTable =
    DocTable<T.RetailerId, M.RetailerDoc, T.RetailerInfo>(
      0,
      func(x:T.RetailerId):T.RetailerId{x+1},
      func(x:T.RetailerId,y:T.RetailerId):Bool{x==y},
      idHash,
      func(doc:M.RetailerDoc):T.RetailerInfo = shared {
        id=doc.id;
        short_name=doc.short_name;
        description=doc.description;
        region=doc.region.id;
        reserved_routes=[];
        reserved_items=[];
      },
      func(info:T.RetailerInfo):?M.RetailerDoc =
        Option.map<M.RegionDoc, M.RetailerDoc>(
          regionTable.getDoc(info.region),
          func (regionDoc: M.RegionDoc): M.RetailerDoc = new {
            id=info.id;
            short_name=info.short_name;
            description=info.description;
            region=regionDoc;
            reserved=null;
          }
        )
      );

  var retailerQueryCount : Nat = 0;
  var retailerQueryCost : Nat = 0;
  var retailerJoinCount : Nat = 0;

  /**
   `routeTable`
   ----------------
   */

  var routeTable : M.RouteTable =
    DocTable<T.RouteId, M.RouteDoc, T.RouteInfo> (
      0,
      func(x:T.RouteId):T.RouteId{x+1},
      func(x:T.RouteId,y:T.RouteId):Bool{x==y},
      idHash,
      func(doc:M.RouteDoc):T.RouteInfo = shared {
        id=doc.id;
        transporter=doc.transporter;
        truck_type=(truckTypeTable.getInfoOfDoc())(doc.truck_type);
        start_region=doc.start_region.id;
        end_region=doc.end_region.id;
        start_date=doc.start_date;
        end_date=doc.end_date;
        cost=doc.cost;
      },
      func(info:T.RouteInfo):?M.RouteDoc {
        switch (transporterTable.getDoc(info.transporter),
                truckTypeTable.getDoc(info.truck_type.id),
                regionTable.getDoc(info.start_region),
                regionTable.getDoc(info.end_region))
        {
        case (?_, ?truckType, ?startRegion, ?endRegion) {
                 ?(new {
                     id=info.id;
                     transporter=info.transporter;
                     truck_type=truckType;
                     start_region=startRegion;
                     end_region=endRegion;
                     start_date=info.start_date;
                     end_date=info.end_date;
                     cost=info.cost;
                   })
               };
          case _ { null }
        }}
    );


  /**
   `reservedInventoryTable`
   ---------------------------
   */

  var reservedInventoryTable : M.ReservedInventoryTable =
    DocTable<T.ReservedInventoryId, M.ReservedInventoryDoc, T.ReservedInventoryInfo>(
    0,
    func(x:T.ReservedInventoryId):T.ReservedInventoryId{x+1},
    func(x:T.ReservedInventoryId,y:T.ReservedInventoryId):Bool{x==y},
    idHash,
    func(doc:M.ReservedInventoryDoc):T.ReservedInventoryInfo = shared {
      id=doc.id;
      item=doc.item.id;
      retailer=doc.retailer
    },
    func(info:T.ReservedInventoryInfo):?M.ReservedInventoryDoc = {
      // validate the info's item id
      switch (inventoryTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?item_, ?_) {
               ?(new {
                   id=info.id;
                   item=item_:M.InventoryDoc;
                   retailer=info.retailer;
                 })
             };
        case _ {
               null
             }
      }}
    );

  /**
   `reservedRouteTable`
   ----------------
   */

  var reservedRouteTable : M.ReservedRouteTable =
    DocTable<T.ReservedRouteId, M.ReservedRouteDoc, T.ReservedRouteInfo>(
    0,
    func(x:T.ReservedRouteId):T.ReservedRouteId{x+1},
    func(x:T.ReservedRouteId,y:T.ReservedRouteId):Bool{x==y},
    idHash,
    func(doc:M.ReservedRouteDoc):T.ReservedRouteInfo = shared {
      id=doc.id;
      route=doc.route.id;
      retailer=doc.retailer
    },
    func(info:T.ReservedRouteInfo):?M.ReservedRouteDoc = {
      // validate the info's item id
      switch (routeTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?route_, ?_) {
               ?(new {
                   id=info.id;
                   route=route_:M.RouteDoc;
                   retailer=info.retailer;
                 })
             };
        case _ {
               null
             }
      }}
    );

  /**
   Indexing by `UserName`
   =====================================
   */

  private var usersByUserName
    : M.UserNameMap = null;

  /**

   Indexing by `RegionId`
   =====================================

   For efficient joins, we need some extra indexing.

   Regions as keys in special global maps
   ---------------------------------------
   - inventory (across all producers) keyed by producer region
   - routes (across all transporters) keyed by source region
   - routes (across all transporters) keyed by destination region

   Routes by region
   ----------------------------

   the actor maintains a possibly-sparse 3D table mapping each
   region-region-routeid triple to zero or one routes.  First index
   is destination region, second index is source region; this 2D
   spatial coordinate gives all routes that go to that destination
   from that source, keyed by their unique route ID, the third
   coordinate of the mapping.

   */

  private var routesByDstSrcRegions : M.ByRegionPairRouteMap = null;

  /**
   Inventory by region
   ----------------------------

   the actor maintains a possibly-sparse 3D table mapping each
   sourceregion-producerid-inventoryid triple to zero or one
   inventory items.  The 1D coordinate sourceregion gives all of the
   inventory items, by producer id, for this source region.

  */

  private var inventoryByRegion : M.ByRegionInventoryMap = null;

  /**

   Reserved inventory by produce and region
   --------------------------------------------

   The `produceMarketInfo` query asks the server for market info:

   > the last sales price for produce within a given geographic area

   To answer this query more efficiently under a system with many
reservations across region and produce kind, the following mapping
maintains a 3D table of reservations organized by
region-produce-reservationid coordinates.

   There need only be one reservationid for any given region produce
pair: without affecting the ability to answer the query above, we can
drop reservations for a given region-produce that are older than newer
reservations for the same sub-space.

  Alternatively, we need not drop these older records, and
instead, we could do a weighted average of the entire reservation
history to answer market info queries more accurately; or, to save
space, we could eventually maintain a running average, rather than
merely forget the older prices.  Doing either of these is more complex
than the MVP goals, however.

   */
  private var reservationsByProduceByRegion
    : M.ByProduceByRegionInventoryReservationMap = null;


  /**

   Future work: Indexing by time
   --------------------------------
   For now, we won't try to index based on days.

   If and when we want to do so, we would like to have a spatial
   data structure that knows about each object's "interval" in a
   single shared dimension (in time):

   - inventory, by availability window (start day, end day)
   - routes, by transport window (departure day, arrival day)

   */

  /**

   Message-response specifications
   ======================================================

   As explained in the `README.md` file, this actor also gives a
   behavioral spec of the exchange's semantics, by giving a prototype
   implementation of this behavior (and wrapped trivially by `Server`).

   The functional behavior of this interface, but not implementation
   details, are part of the formal spec.

   */

  /**

   `User`-oriented operations
   ==========================================

   */


  /**

   `addUser`
   ---------

   The given `user_name` must be unique to the exchange; the operation fails otherwise.

   */
  addUser(
    public_key_: T.PublicKey,
    user_name_: Text,
    description_: Text,
    region_: T.RegionId,
    isDeveloper_: Bool,
    isProducer: Bool,
    isRetailer: Bool,
    isTransporter: Bool
  ) : ?T.UserId {

    /**- Fail immediately if the user name is already taken: */
    switch (Trie.find<T.UserName,T.UserId>(usersByUserName, keyOfText(user_name_), textIsEq)) {
      case null {};
      case (?_) { return null };
    };

    /**- Fail immediately if the region Id is invalid: */
    switch (regionTable.getDoc(region_)) {
      case null { return null };
      case (?_) {};
    };

    /** Input is valid: All subsequent operations will succeed: */

    /**- Create a producer role for the user: */
    let prId = if isProducer { producerTable.addInfoGetId(
      func(id_:T.ProducerId):T.ProducerInfo {
        shared {
          id=id_:T.ProducerId;
          short_name=user_name_;
          description=description_;
          region=region_;
          inventory=[];
          reserved=[];
        }
      }) } else null;

    /**- Create a transporter role for the user: */
    let trId = if isTransporter { transporterTable.addInfoGetId(
      func(id_:T.TransporterId):T.TransporterInfo {
        shared {
          id=id_:T.TransporterId;
          short_name=user_name_;
          description=description_;
          routes=[];
          reserved=[];
        }
      }) } else null;

    /**- Create a retailer role for the user: */
    let rrId = if isRetailer { retailerTable.addInfoGetId(
      func(id_:T.RetailerId):T.RetailerInfo {
        shared {
          id=id_;
          short_name=user_name_;
          description=description_;
          region=region_:T.RegionId;
        }
      }) } else null;

    /**- Record the user information: */
    let id = userTable.addInfoGetId(
      func (id_: T.UserId) : T.UserInfo =
        shared {
          id = id_;
          user_name = user_name_;
          public_key = public_key_;
          description = description_;
          region = region_;
          producerId = prId;
          transporterId = trId;
          retailerId = rrId;
          isDeveloper = isDeveloper_;
        });

    /**- Record the mapping from user-chosen name to exchange-chosen id: */
    usersByUserName :=
    Trie.insertFresh<T.UserName,T.UserId>(
      usersByUserName,
      keyOfText(user_name_), textIsEq,
      Option.unwrap<T.UserId>(id)
    );

    /**- return the id */
    id
  };

  /** Verifies that the user name and public key match */
  isValidUser(public_key: T.PublicKey, user_name: Text): Bool {
    switch (Trie.find<T.UserName,T.UserId>(usersByUserName, keyOfText(user_name), textIsEq)) {
      case null { return false };
      case (?userId) {
        Option.option<M.UserDoc, Bool>(
          userTable.getDoc(userId),
          func (u:M.UserDoc): Bool { u.public_key == public_key },
          false
        )
      }
    }
  };

  producerFromUserId(id: T.UserId): ?M.ProducerDoc = Option.fmap<M.UserDoc, M.ProducerDoc>(
    userTable.getDoc(id),
    func (u: M.UserDoc): ?M.ProducerDoc = Option.fmap<T.ProducerId, M.ProducerDoc>(
      u.producerId,
      func (i: T.ProducerId): ?M.ProducerDoc = producerTable.getDoc(i)
    )
  );

  transporterFromUserId(id: T.UserId): ?M.TransporterDoc = Option.fmap<M.UserDoc, M.TransporterDoc>(
    userTable.getDoc(id),
    func (u: M.UserDoc): ?M.TransporterDoc = Option.fmap<T.TransporterId, M.TransporterDoc>(
      u.transporterId,
      func (i: T.TransporterId): ?M.TransporterDoc = transporterTable.getDoc(i)
    )
  );

  retailerFromUserId(id: T.UserId): ?M.RetailerDoc = Option.fmap<M.UserDoc, M.RetailerDoc>(
    userTable.getDoc(id),
    func (u: M.UserDoc): ?M.RetailerDoc = Option.fmap<T.RetailerId, M.RetailerDoc>(
      u.retailerId,
      func (i: T.RetailerId): ?M.RetailerDoc = retailerTable.getDoc(i)
    )
  );

  /**

   `Produce`-oriented operations
   ==========================================

   */


  /**
   `produceMarketInfo`
   ---------------------------
   The last sales price for produce within a given geographic area; null region id means "all areas."
   */
  produceMarketInfo(public_key: T.PublicKey, produce_id:T.ProduceId, region_oid:?T.RegionId) : ?[T.ProduceMarketInfo] {
    // switch (Map.find<ProduceId,Map<RegionId,Map<ReservedInventoryId>>>(
    //           reservationsByProduceByRegion,
    //           produce_id, idIsEq)) {
    //   case null { return null };
    null
  };

  /**

   `Producer`-facing operations
   ==========================================

   */


  /**
   // `producerAllInventoryInfo`
   // ---------------------------
   */
  producerAllInventoryInfo(public_key: T.PublicKey, id:T.UserId) : ?[T.InventoryInfo] {
    let doc = switch (producerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    //assert(isValidUser(public_key, doc.short_name));

    ?Map.toArray<T.InventoryId,M.InventoryDoc,T.InventoryInfo>(
      doc.inventory,
      func (_:T.InventoryId,doc:M.InventoryDoc):[T.InventoryInfo] =
        [inventoryTable.getInfoOfDoc()(doc)]
    )
  };

  /**
   `producerAddInventory`
   ---------------------------

  */
  producerAddInventory(
    public_key : Text,
    iid_       : ?T.InventoryId,
    id_        : T.UserId,
    produce_id : T.ProduceId,
    quantity_  : T.Quantity,
    weight_    : T.Weight,
    ppu_       : T.Price,
    start_date_: T.Date,
    end_date_  : T.Date,
    comments_  : Text,
  ) : Result<T.InventoryId, T.ServerErr>
  {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oproducer: ?M.ProducerDoc = producerFromUserId(id_);
    let oproduce  : ?M.ProduceDoc  = produceTable.getDoc(produce_id);
    let (producer_, produce_) = {
      switch (oproducer, oproduce) {
      case (?producer, ?produce) (producer, produce);
      case _ { return #err(#idErr) };
      }};

    if (not isValidUser(public_key, producer_.short_name)) {
      return (#err(#publicKeyErr))
    };

    /**- Create the inventory item document: */
    let (_, item) = {
      switch (inventoryTable.addInfoAs(iid_,
                func(iid:T.InventoryId):T.InventoryInfo{
        shared {
          id        = iid       :T.InventoryId;
          produce   = produce_id:T.ProduceId;
          producer  = id_       :T.ProducerId;
          quantity  = quantity_ :T.Quantity;
          weight    = weight_   :T.Weight;
          ppu       = ppu_      :T.Price;
          start_date=start_date_:T.Date;
          end_date  =end_date_  :T.Date;
          comments  =comments_  :Text;
        };
      })) {
      case (?item) { item };
      case (null) { P.unreachable() };
      }
    };

    /**- Update the producer's inventory collection to hold the new inventory document: */
    let updatedInventory =
      Map.insertFresh<T.InventoryId, M.InventoryDoc>(
        producer_.inventory,
        keyOf(item.id),
        idIsEq,
        item
      );

    /**- Update the producer document; xxx more concise syntax for functional record updates would be nice: */
    let _ = producerTable.updateDoc(
      producer_.id,
      new {
        id = producer_.id;
        short_name = producer_.short_name;
        description = producer_.description;
        region = producer_.region;
        reserved = producer_.reserved;
        inventory = updatedInventory;
      });

    /**- Update inventoryByRegion mapping: */
    inventoryByRegion :=
    Map.insert2D<T.RegionId, T.ProducerId, M.InventoryMap>(
      inventoryByRegion,
      keyOf(producer_.region.id), idIsEq,
      keyOf(producer_.id), idIsEq,
      updatedInventory,
    );

    return #ok(item.id)
  };

  /**
   `producerUpdateInventory`
   ---------------------------

  */
  producerUpdateInventory(
    public_key : Text,
    iid_       : T.InventoryId,
    id_        : T.UserId,
    produce_id : T.ProduceId,
    quantity_  : T.Quantity,
    weight_    : T.Weight,
    ppu_       : T.Price,
    start_date_: T.Date,
    end_date_  : T.Date,
    comments_  : Text,
  ) : Result<(),T.ServerErr>
  {
    /**- Validate these ids; fail here if anything is invalid: */
    let oproducer: ?M.ProducerDoc = producerFromUserId(id_);
    let oinventory : ?M.InventoryDoc = inventoryTable.getDoc(iid_);
    let oproduce  : ?M.ProduceDoc  = produceTable.getDoc(produce_id);
    let (inventory_, producer_, produce_) = {
      switch (oinventory, oproducer, oproduce) {
      case (?inventory, ?producer, ?produce) {
             // it's an error if the producer is not fixed across the
             // update.  i.e., producer A cannot update the inventory
             // of producer B, only her own.
             if ( inventory.producer == producer.id ) {
               (inventory, producer, produce)
             } else {
               return (#err(#idErr))
             }
           };
      case _ { return (#err(#idErr)) };
      }};

    if (not isValidUser(public_key, producer_.short_name)) {
      return (#err(#publicKeyErr))
    };

    /**- remove the inventory item; given the validation above, this cannot fail. */
    Result.assertOk( producerRemInventory(public_key, iid_) );

    /**- add the (updated) inventory item; given the validation above, this cannot fail. */
    Result.assertOk(
      producerAddInventory(
        public_key, ?iid_, id_,
        produce_id,
        quantity_, weight_, ppu_, start_date_, end_date_, comments_ )
    );

    /**- Success! */
    #ok
  };

  /**
   `producerRemInventory`
   ---------------------------

   Remove the given inventory item from the exchange.

   */
  producerRemInventory(public_key: T.PublicKey, id:T.InventoryId) : Result<(),T.ServerErr> {

    /**- validate the `id` */
    /// xxx macro for this pattern?
    let doc = switch (inventoryTable.getDoc(id)) {
      case null { return #err(#idErr) };
      case (?doc) { doc };
    };

    /**- remove document from `producerTable`, in several steps: */
    let producer = Option.unwrap<M.ProducerDoc>(producerTable.getDoc(doc.producer));

    /// xxx: access control: Check that the current user is the owner of this inventory
    if (not isValidUser(public_key, producer.short_name)) {
      return (#err(#publicKeyErr))
    };

    /**- remove document from `inventoryTable` */
    Option.assertSome<M.InventoryDoc>(
      inventoryTable.rem( id )
    );

    /// xxx an abstraction to hide these type arguments?
    let (updatedInventory, _) =
      Trie.remove<T.InventoryId, M.InventoryDoc>(
        producer.inventory, keyOf(id), idIsEq);

    /// xxx syntax for functional record updates?
    let updatedProducer = new {
      id          = producer.id ;
      short_name  = producer.short_name ;
      description = producer.description ;
      region      = producer.region ;
      inventory   = updatedInventory ;
      reserved    = producer.reserved ;
    };

    Option.assertSome<M.ProducerDoc>(
      producerTable.updateDoc( producer.id, updatedProducer )
    );

    /**- remove document from table `inventoryByRegion`: */
    /// xxx an abstraction to hide this tuple projection, assignment, and type args?
    inventoryByRegion := {
      let (t, d) = Trie.remove3D<T.RegionId, T.ProducerId, T.InventoryId, M.InventoryDoc>(
        inventoryByRegion,
        keyOf(producer.region.id), idIsEq,
        keyOf(producer.id), idIsEq,
        keyOf(id), idIsEq
      );
      Option.assertSome<M.InventoryDoc>(d);
      t
    };

    #ok
  };

  /**
   `producerReservations`
   ---------------------------

   */
  producerReservations(public_key: T.PublicKey, id:T.UserId) : ?[T.ReservedInventoryInfo] {
    let doc = switch (producerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    //assert(isValidUser(public_key, doc.short_name));

    ?Map.toArray<T.ReservedInventoryId,
                 M.ReservedInventoryDoc,
                 T.ReservedInventoryInfo>(
      doc.reserved,
      func (_:T.ReservedInventoryId,
            doc:M.ReservedInventoryDoc):
        [T.ReservedInventoryInfo]
        =
        [reservedInventoryTable.getInfoOfDoc()(doc)]
    )
  };


   /**
   `Transporter`-facing operations
   =================
   */


  /**
   `transporterAddRoute`
   ---------------------------
  */
  transporterAddRoute(
    public_key:      Text,
    rid_:            ?T.RouteId,
    id_:             T.UserId,
    start_region_id: T.RegionId,
    end_region_id:   T.RegionId,
    start_date_:     T.Date,
    end_date_:       T.Date,
    cost_:           T.Price,
    trucktype_id:    T.TruckTypeId
  ) : Result<T.RouteId,T.ServerErr> {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let otransporter : ?M.TransporterDoc = transporterFromUserId(id_);
    let orstart      : ?M.RegionDoc  = regionTable.getDoc(start_region_id);
    let orend        : ?M.RegionDoc  = regionTable.getDoc(end_region_id);
    let otrucktype   : ?T.TruckTypeInfo  = truckTypeTable.getInfo(trucktype_id);
    let (transporter, start_region_, end_region_, truck_type_) = {
      switch (otransporter, orstart, orend, otrucktype) {
      case (?x1, ?x2, ?x3, ?x4) (x1, x2, x3, x4);
      case _ { return #err(#idErr) };
      }};
    let transporterId = transporter.id;

    if (not isValidUser(public_key, transporter.short_name)) {
      return (#err(#publicKeyErr))
    };

    /**- Create the route item document: */
    let route : M.RouteDoc = {
      switch (routeTable.addInfoAs(rid_, func(routeId:T.RouteId):T.RouteInfo{
        shared {
        id= routeId;
        transporter=transporterId;
        truck_type=truck_type_;
        start_date=start_date_;
        end_date=end_date_;
        start_region=start_region_id;
        end_region=end_region_id;
        cost=cost_;
        };
      })) {
      case (?(_, route)) { route };
      case null { P.unreachable() };
      }
    };

    /**- Update the transporter's routes collection to hold the new route document: */
    let updatedRoutes =
      Map.insertFresh<T.RouteId, M.RouteDoc>(
        transporter.routes,
        keyOf(route.id),
        idIsEq,
        route
      );

    /**- Update the transporter document; xxx more concise syntax for functional record updates would be nice: */
    let _ = transporterTable.updateDoc(
      transporter.id,
      new {
        id = transporter.id;
        short_name = transporter.short_name;
        description = transporter.description;
        reserved = transporter.reserved;
        routes = updatedRoutes;
      });

    /**- Update the [`routesByDstSrcRegions` mapping](#routes-by-region) using the route's regions and id */
    routesByDstSrcRegions :=
    Map.insert3D<T.RegionId, T.RegionId, T.RouteId, M.RouteDoc>(
      routesByDstSrcRegions,
      keyOf(end_region_.id), idIsEq,
      keyOf(start_region_.id), idIsEq,
      keyOf(route.id), idIsEq,
      route
    );

    #ok(route.id)
  };

  /**
   `transporterUpdateRoute`
   ---------------------------
   Update the given route with the given field values.
   */
  transporterUpdateRoute(
    public_key      : Text,
    rid_            : T.RouteId,
    id_             : T.UserId,
    start_region_id : T.RegionId,
    end_region_id   : T.RegionId,
    start_date_     : T.Date,
    end_date_       : T.Date,
    cost_           : T.Price,
    trucktype_id    : T.TruckTypeId
  ) : Result<(),T.ServerErr> {
    /** The model updates routes and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oroute       : ?M.RouteDoc   = routeTable.getDoc(rid_);
    let otransporter : ?M.TransporterDoc = transporterFromUserId(id_);
    let orstart      : ?M.RegionDoc  = regionTable.getDoc(start_region_id);
    let orend        : ?M.RegionDoc  = regionTable.getDoc(end_region_id);
    let otrucktype   : ?M.TruckTypeDoc  = truckTypeTable.getDoc(trucktype_id);
    let (route, transporter, start_region_, end_region_, truck_type_) = {
      switch (oroute, otransporter, orstart, orend, otrucktype) {
      case (?route, ?transporter, ?x2, ?x3, ?x4) {
             // it's an error if the transporter is not fixed across the
             // update.  i.e., transporter A cannot update the routes
             // of transporter B, only her own.
             if ( route.transporter == transporter.id ) {
               (route, transporter, x2, x3, x4);
             } else {
               return #err(#idErr)
             }
           };
      case _ { return #err(#idErr) };
      }};

    // xxx
    if (not isValidUser(public_key, transporter.short_name)) {
      return #err(#publicKeyErr)
    }

    /**- remove the route; given the validation above, this cannot fail. */
    Result.assertOk( transporterRemRoute(public_key, rid_) );

    /**- add the (updated) route; given the validation above, this cannot fail. */
    Result.assertOk(
      transporterAddRoute(
        public_key,
        ?rid_, id_,
        start_region_id,
        end_region_id,
        start_date_,
        end_date_,
        cost_,
        trucktype_id
      )
    );

    /**- Success! */
    #ok
  };

  /**
   `transporterRemRoute`
   ---------------------------
   Remove the given route from the exchange.
   */
  transporterRemRoute(public_key: T.PublicKey, id:T.RouteId) : Result<(),T.ServerErr> {

    let doc = switch (routeTable.getDoc(id)) {
      case null { return #err(#idErr) };
      case (?doc) { doc };
    };

    let transporter = Option.unwrap<M.TransporterDoc>(transporterTable.getDoc(doc.transporter));

    if (not isValidUser(public_key, transporter.short_name)) {
      return #err(#publicKeyErr)
    }

    Option.assertSome<M.RouteDoc>(
      routeTable.rem( id )
    );

    /// xxx: access control: Check that the current user is the owner of this route

    let (updatedRoutes, _) =
      Trie.remove<T.RouteId, M.RouteDoc>(
        transporter.routes, keyOf(id), idIsEq);

    let updatedTransporter = new {
      id          = transporter.id ;
      short_name  = transporter.short_name ;
      description = transporter.description ;
      routes      = updatedRoutes ;
      reserved    = transporter.reserved ;
    };

    Option.assertSome<M.TransporterDoc>(
      transporterTable.updateDoc( transporter.id, updatedTransporter )
    );

    routesByDstSrcRegions := {
      let (t, d) = Trie.remove3D<T.RegionId, T.RegionId, T.RouteId, M.RouteDoc>(
        routesByDstSrcRegions,
        keyOf(doc.end_region.id), idIsEq,
        keyOf(doc.start_region.id), idIsEq,
        keyOf(doc.id), idIsEq
      );
      Option.assertSome<M.RouteDoc>(d);
      t
    };

    #ok
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(public_key: T.PublicKey, id:T.UserId) : ?[T.RouteInfo] {
    let doc = switch (transporterFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    //assert(isValidUser(public_key, doc.short_name));

    ?Map.toArray<T.RouteId,
                 M.RouteDoc,
                 T.RouteInfo>(
      doc.routes,
      func (_:T.RouteId,
            doc:M.RouteDoc):
        [T.RouteInfo]
        =
        [routeTable.getInfoOfDoc()(doc)]
    )
  };

  /**
   `transporterReservationInfo`
   ---------------------------

   */
  transporterAllReservationInfo(public_key: T.PublicKey, id:T.UserId) : ?[T.ReservedRouteInfo] {
    let doc = switch (transporterFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    //assert(isValidUser(public_key, doc.short_name));

    ?Map.toArray<T.ReservedRouteId,
                 M.ReservedRouteDoc,
                 T.ReservedRouteInfo>(
      doc.reserved,
      func (_:T.ReservedRouteId,
            doc:M.ReservedRouteDoc):
        [T.ReservedRouteInfo]
        =
        [reservedRouteTable.getInfoOfDoc()(doc)]
    )
  };


  /**
   `Retailer`-facing operations
   ====================
   */


  /**
  `makeReservationInfo`
  ----------------------
  Prepare reservation information for a server client
  based on the given inventory and route documents.
  */
  makeReservationInfo(item:M.InventoryDoc, route:M.RouteDoc) : T.ReservationInfo {
    shared {
      produce  =item.produce.id :T.ProduceId;
      producer =item.producer   :T.ProducerId;
      quant    =item.quantity   :T.Quantity;
      ppu      =item.ppu        :T.Price;
      weight   =item.weight     :T.Weight;
      prod_cost=item.quantity * item.ppu:T.Price;

      transporter = route.transporter :T.TransporterId;
      truck_type  = route.truck_type.id :T.TruckTypeId;

      region_begin = route.start_region.id:T.RegionId;
      region_end   = route.end_region.id  :T.RegionId;
      date_begin   = route.start_date  :T.Date;
      date_end     = route.end_date    :T.Date;
      trans_cost   = route.cost:  T.Price;
    }
  };


  /**

  `isCompatibleTruckType`
  ----------------------

  Check whether the given truck type can accommodate the given produce type.

  */
  isCompatibleTruckType(tt:M.TruckTypeDoc, produce:M.ProduceDoc) : Bool {
    // todo
    true
  };

  /**

  `isFeasibleReservation`
  ----------------------
  Check whether the given retailer can reserve the given item and route pair.

  */

  isFeasibleReservation(retailer:M.RetailerDoc, item:M.InventoryDoc, route:M.RouteDoc) : Bool {
    /** - window start: check that the route begins after the inventory window begins */
    if (item.start_date > route.start_date) {
      debugOff "nope: item start after route start\n";
      return false
    };
    /** - window end: check that the route ends before the inventory window ends */
    if (route.end_date > item.end_date) {
      debugOff "nope: route ends after item ends\n";
      return false
    };
    /** - check that truck can carry the given produce */
    if (not isCompatibleTruckType(route.truck_type, item.produce)) {
      debugOff "nope: truck is not compatible\n";
      return false
    };
    /** - all checks pass: */
    true
  };

  /** to do: check route window inside of inventory window, e.g.,
   by 1 day before and 3 days after on each side: */

  /**
   `retailerQueryAll`
   ---------------------------

   List all available inventory items and routes for a given retailer.

   The business logic:
   - [`isCompatibleTruckType`](#isCompatibleTruckType): Checks truck and produce compatibility.
   - [`isFeasibleReservation`](#isFeasibleReservation): Checks timing constraints.
   - [`makeReservationInfo`](#makereservationinfo): Summarizes the reserved route and inventory documents.

   For `Trie`-based DB operations:
   - [`Trie.join`]($DOCURL/trie.md#join): For the inner join on common `RegionId`s of routes and inventory.
   - [`Trie.prod`]($DOCURL/trie.md#prod): For the catesian product of routes and inventory.
   - [`Trie.mergeDisjoint2D`]($DOCURL/trie.md#mergeDisjoint2D): To flatten 2D mappings into 1D mappings.
  */
  retailerQueryAll(public_key: T.PublicKey, id:T.UserId) : ?T.QueryAllResults {
    retailerQueryCount += 1;

    /** - Find the retailer's document: */
    let retailer =
      switch (retailerFromUserId(id)) {
      case (null) { return null };
      case (?x) { x }};

    //assert(isValidUser(public_key, retailer.short_name));

    debug "- user_name: ";
    debug (retailer.short_name);
    debug ", public_key: ";
    debug (public_key);
    debug "\n";

    /** - Temp: */
    debug "- retailer is located in region ";
    debugInt (retailer.region.id);
    debug ", and\n- is accessible via routes from ";

    /** - Find all routes whose the destination region is the retailer's region: */
    let retailerRoutes =
      switch (Trie.find<T.RegionId, M.ByRegionRouteMap>(
                routesByDstSrcRegions,
                keyOf(retailer.region.id),
                idIsEq
              )) {
      case (null) { return ?[] };
      case (?x) { x }};

    debugInt(Trie.count<T.RegionId, M.RouteMap>(retailerRoutes));
    debug " production regions.\n";

    /** - Join: For each production region, consider all routes and inventory: */
    let queryResults : Trie<T.RegionId, RouteInventoryMap> = {
      retailerJoinCount += 1;
      Trie.join<T.RegionId,
                M.RouteMap,
                M.ByProducerInventoryMap,
                RouteInventoryMap>(
        retailerRoutes,
        inventoryByRegion,
        idIsEq,
        func (routes:M.RouteMap,
              inventory:M.ByProducerInventoryMap) : RouteInventoryMap
      {

        /** - Within this production region, consider every route-item pairing: */
        let product = Trie.prod<T.RouteId, M.RouteDoc,
                                T.InventoryId, M.InventoryDoc,
                                (T.RouteId, T.InventoryId),
                                (M.RouteDoc, M.InventoryDoc)>(
          routes,
          /** - (To perform this Cartesian product, use a 1D inventory map:) */
          Trie.mergeDisjoint2D<T.ProducerId, T.InventoryId, M.InventoryDoc>(
            inventory, idIsEq, idIsEq),

          func (route_id:T.RouteId,
                route   :M.RouteDoc,
                item_id :T.InventoryId,
                item    :M.InventoryDoc) :
            ?(Key<(T.RouteId, T.InventoryId)>,
              (M.RouteDoc, M.InventoryDoc))
        {
          retailerQueryCost += 1;
          /** - Consider the constraints of the retailer-route-item combination: */
          if (isFeasibleReservation(retailer, item, route)) {
            ?( keyOfIdPair(route_id, item_id),
               (route, item)
            )
          } else { null }
        },
        idPairIsEq
        );
        product
      }
      )};

    /** - The results are still organized by producer region; merge all such regions: */
    let queryResultsMerged : RouteInventoryMap =
      Trie.mergeDisjoint2D<T.RegionId, (T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)>(
        queryResults, idIsEq, idPairIsEq);

    debug "- query result count: ";
    debugInt(Trie.count<(T.RouteId, T.InventoryId),
                        (M.RouteDoc, M.InventoryDoc)>(queryResultsMerged));
    debug " (count of feasible route-item pairs).\n";

    /** - Prepare reservation information for client, as an array; see also [`makeReservationInfo`](#makereservationinfo) */
    let arr =
      Trie.toArray<(T.RouteId, T.InventoryId),
                   (M.RouteDoc, M.InventoryDoc),
                   T.ReservationInfo>(
        queryResultsMerged,
        func (_:(T.RouteId,T.InventoryId), (r:M.RouteDoc, i:M.InventoryDoc))
          : [ T.ReservationInfo ] {
            [ makeReservationInfo(i, r) ]
          });

    ?arr
  };

  /**
   `retailerAllReservationInfo`
   ---------------------------

  */
  retailerAllReservationInfo(public_key: T.PublicKey, id:T.UserId) :
    ?[(T.ReservedInventoryInfo,
       T.ReservedRouteInfo)]
  {
    let doc = switch (retailerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    //assert(isValidUser(public_key, doc.short_name));

    ?Map.toArray<T.ReservedInventoryId,
                 (M.ReservedInventoryDoc,  M.ReservedRouteDoc),
                 (T.ReservedInventoryInfo, T.ReservedRouteInfo)>(
      doc.reserved,
      func (_:T.ReservedInventoryId,
            ((idoc:M.ReservedInventoryDoc),
             (rdoc:M.ReservedRouteDoc)))
            :
            [(T.ReservedInventoryInfo,
              T.ReservedRouteInfo)]
        =
        [(reservedInventoryTable.getInfoOfDoc()(idoc),
          reservedRouteTable.getInfoOfDoc()(rdoc))]
    )
  };

  /**
   `retailerQueryDates`
   ---------------------------

   Retailer queries available produce by delivery date range; returns
   a list of inventory items that can be delivered to that retailer's
   geography within that date.

   ```
   let jt = (joinTablesConditionally
               (routesByDstSrcRegionTable (retailer region))
               inventoryByRegionTable
               filterByDateConstraints
            );
   ```

   */
  retailerQueryDates(
    public_key: T.PublicKey,
    id:T.UserId,
    begin:T.Date,
    end:T.Date
  ) : ?[T.InventoryInfo]
  {
    retailerQueryCount += 1;

    P.nyi()
  };

  /**
   `retailerReserve`
   ---------------------------
  */
  retailerReserve(
    public_key: T.PublicKey,
    id:T.UserId,
    inventory:T.InventoryId,
    route:T.RouteId) : ?(T.ReservedRouteId, T.ReservedInventoryId)
  {
    P.nyi()
  };

};
