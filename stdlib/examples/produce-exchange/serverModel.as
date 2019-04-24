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

type RouteInventoryMap = Trie<(RouteId, InventoryId), (RouteDoc, InventoryDoc)>;

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

  var userTable : UserTable =
    DocTable<UserId, UserDoc, UserInfo>(
    0,
    func(x:UserId):UserId{x+1},
    func(x:UserId,y:UserId):Bool{x==y},
    idHash,
    func(doc:UserDoc):UserInfo = shared {
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
    func(info:UserInfo):?UserDoc = ?(new {
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

  var truckTypeTable : TruckTypeTable =
    DocTable<TruckTypeId, TruckTypeDoc, TruckTypeInfo>(
    0,
    func(x:TruckTypeId):TruckTypeId{x+1},
    func(x:TruckTypeId,y:TruckTypeId):Bool{x==y},
    idHash,
    func(doc:TruckTypeDoc):TruckTypeInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      capacity=doc.capacity;
      isFridge=doc.isFridge;
      isFreezer=doc.isFreezer;
    },
    func(info:TruckTypeInfo):?TruckTypeDoc = ?(new {
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

  var regionTable : RegionTable =
    DocTable<RegionId, RegionDoc, RegionInfo>(
    0,
    func(x:RegionId):RegionId{x+1},
    func(x:RegionId,y:RegionId):Bool{x==y},
    idHash,
    func(doc:RegionDoc):RegionInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
    },
    func(info:RegionInfo):?RegionDoc = ?(new {
      id=info.id;
      short_name=info.short_name;
      description=info.description;
    }),
  );

  /**
   `produceTable`
   -----------------
   */

  var produceTable : ProduceTable =
    DocTable<ProduceId, ProduceDoc, ProduceInfo>(
    0,
    func(x:ProduceId):ProduceId{x+1},
    func(x:ProduceId,y:ProduceId):Bool{x==y},
    idHash,
    func(doc:ProduceDoc):ProduceInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      grade=doc.grade;
    },
    func(info:ProduceInfo):?ProduceDoc = ?(new {
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

  var producerTable : ProducerTable =
    DocTable<ProducerId, ProducerDoc, ProducerInfo>(
    0,
    func(x:ProducerId):ProducerId{x+1},
    func(x:ProducerId,y:ProducerId):Bool{x==y},
    idHash,
    func(doc:ProducerDoc):ProducerInfo = shared {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      region=doc.region.id;
      inventory=[];
      reserved=[];
    },
    func(info:ProducerInfo):?ProducerDoc =
      map<RegionDoc, ProducerDoc>(
        regionTable.getDoc(info.region),
        func (regionDoc: RegionDoc): ProducerDoc = new {
          id=info.id;
          short_name=info.short_name;
          description=info.description;
          region=regionDoc;
          inventory=Table.empty<InventoryId, InventoryDoc>();
          reserved=Table.empty<ReservedInventoryId, ReservedInventoryDoc>();
        }
      )
    );


  /**
   `inventoryTable`
   ---------------
   */

  var inventoryTable : InventoryTable =
    DocTable<InventoryId, InventoryDoc, InventoryInfo>(
    0,
    func(x:InventoryId):InventoryId{x+1},
    func(x:InventoryId,y:InventoryId):Bool{x==y},
    idHash,
    func(doc:InventoryDoc):InventoryInfo = shared {
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
    func(info:InventoryInfo):?InventoryDoc = {
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

  var transporterTable : TransporterTable =
    DocTable<TransporterId, TransporterDoc, TransporterInfo> (
      0,
      func(x:TransporterId):TransporterId{x+1},
      func(x:TransporterId,y:TransporterId):Bool{x==y},
      idHash,
      func(doc:TransporterDoc):TransporterInfo = shared {
        id=doc.id;
        short_name=doc.short_name;
        description=doc.description;
        routes=[];
        reserved=[];
      },
      func(info:TransporterInfo):?TransporterDoc =
        ?(new {
            id=info.id;
            short_name=info.short_name;
            description=info.description;
            routes=Table.empty<RouteId, RouteDoc>();
            reserved=Table.empty<ReservedRouteId, ReservedRouteDoc>();
          })
    );

  /**
   `retailerTable`
   -----------------
   */

  var retailerTable : RetailerTable =
    DocTable<RetailerId, RetailerDoc, RetailerInfo>(
      0,
      func(x:RetailerId):RetailerId{x+1},
      func(x:RetailerId,y:RetailerId):Bool{x==y},
      idHash,
      func(doc:RetailerDoc):RetailerInfo = shared {
        id=doc.id;
        short_name=doc.short_name;
        description=doc.description;
        region=doc.region.id;
        reserved_routes=[];
        reserved_items=[];
      },
      func(info:RetailerInfo):?RetailerDoc =
        map<RegionDoc, RetailerDoc>(
          regionTable.getDoc(info.region),
          func (regionDoc: RegionDoc): RetailerDoc = new {
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

  var routeTable : RouteTable =
    DocTable<RouteId, RouteDoc, RouteInfo> (
      0,
      func(x:RouteId):RouteId{x+1},
      func(x:RouteId,y:RouteId):Bool{x==y},
      idHash,
      func(doc:RouteDoc):RouteInfo = shared {
        id=doc.id;
        transporter=doc.transporter;
        truck_type=(truckTypeTable.getInfoOfDoc())(doc.truck_type);
        start_region=doc.start_region.id;
        end_region=doc.end_region.id;
        start_date=doc.start_date;
        end_date=doc.end_date;
        cost=doc.cost;
      },
      func(info:RouteInfo):?RouteDoc {
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

  var reservedInventoryTable : ReservedInventoryTable =
    DocTable<ReservedInventoryId, ReservedInventoryDoc, ReservedInventoryInfo>(
    0,
    func(x:ReservedInventoryId):ReservedInventoryId{x+1},
    func(x:ReservedInventoryId,y:ReservedInventoryId):Bool{x==y},
    idHash,
    func(doc:ReservedInventoryDoc):ReservedInventoryInfo = shared {
      id=doc.id;
      item=doc.item.id;
      retailer=doc.retailer
    },
    func(info:ReservedInventoryInfo):?ReservedInventoryDoc = {
      // validate the info's item id
      switch (inventoryTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?item_, ?_) {
               ?(new {
                   id=info.id;
                   item=item_:InventoryDoc;
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

  var reservedRouteTable : ReservedRouteTable =
    DocTable<ReservedRouteId, ReservedRouteDoc, ReservedRouteInfo>(
    0,
    func(x:ReservedRouteId):ReservedRouteId{x+1},
    func(x:ReservedRouteId,y:ReservedRouteId):Bool{x==y},
    idHash,
    func(doc:ReservedRouteDoc):ReservedRouteInfo = shared {
      id=doc.id;
      route=doc.route.id;
      retailer=doc.retailer
    },
    func(info:ReservedRouteInfo):?ReservedRouteDoc = {
      // validate the info's item id
      switch (routeTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?route_, ?_) {
               ?(new {
                   id=info.id;
                   route=route_:RouteDoc;
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
    : UserNameMap = null;

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

  private var routesByDstSrcRegions : ByRegionPairRouteMap = null;

  /**
   Inventory by region
   ----------------------------

   the actor maintains a possibly-sparse 3D table mapping each
   sourceregion-producerid-inventoryid triple to zero or one
   inventory items.  The 1D coordinate sourceregion gives all of the
   inventory items, by producer id, for this source region.

  */

  private var inventoryByRegion : ByRegionInventoryMap = null;

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
    : ByProduceByRegionInventoryReservationMap = null;


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
    public_key_: PublicKey,
    user_name_: Text,
    description_: Text,
    region_: RegionId,
    isDeveloper_: Bool,
    isProducer: Bool,
    isRetailer: Bool,
    isTransporter: Bool
  ) : ?UserId {

    /**- Fail immediately if the user name is already taken: */
    switch (Trie.find<UserName,UserId>(usersByUserName, keyOfText(user_name_), textIsEq)) {
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
      func(id_:ProducerId):ProducerInfo {
        shared {
          id=id_:ProducerId;
          short_name=user_name_;
          description=description_;
          region=region_;
          inventory=[];
          reserved=[];
        }
      }) } else null;

    /**- Create a transporter role for the user: */
    let trId = if isTransporter { transporterTable.addInfoGetId(
      func(id_:TransporterId):TransporterInfo {
        shared {
          id=id_:TransporterId;
          short_name=user_name_;
          description=description_;
          routes=[];
          reserved=[];
        }
      }) } else null;

    /**- Create a retailer role for the user: */
    let rrId = if isRetailer { retailerTable.addInfoGetId(
      func(id_:RetailerId):RetailerInfo {
        shared {
          id=id_;
          short_name=user_name_;
          description=description_;
          region=region_:RegionId;
        }
      }) } else null;

    /**- Record the user information: */
    let id = userTable.addInfoGetId(
      func (id_: UserId) : UserInfo =
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
    Trie.insertFresh<UserName,UserId>(
      usersByUserName,
      keyOfText(user_name_), textIsEq,
      unwrap<UserId>(id)
    );

    /**- return the id */
    id
  };

  /** Verifies that the user name and public key match */
  isValidUser(public_key: PublicKey, user_name: Text): Bool {
    switch (Trie.find<UserName,UserId>(usersByUserName, keyOfText(user_name), textIsEq)) {
      case null { return false };
      case (?userId) {
        option<UserDoc, Bool>(
          userTable.getDoc(userId),
          func (u:UserDoc): Bool { u.public_key == public_key },
          false
        )
      }
    }
  };

  producerFromUserId(id: UserId): ?ProducerDoc = fmap<UserDoc, ProducerDoc>(
    userTable.getDoc(id),
    func (u: UserDoc): ?ProducerDoc = fmap<ProducerId, ProducerDoc>(
      u.producerId,
      func (i: ProducerId): ?ProducerDoc = producerTable.getDoc(i)
    )
  );

  transporterFromUserId(id: UserId): ?TransporterDoc = fmap<UserDoc, TransporterDoc>(
    userTable.getDoc(id),
    func (u: UserDoc): ?TransporterDoc = fmap<TransporterId, TransporterDoc>(
      u.transporterId,
      func (i: TransporterId): ?TransporterDoc = transporterTable.getDoc(i)
    )
  );

  retailerFromUserId(id: UserId): ?RetailerDoc = fmap<UserDoc, RetailerDoc>(
    userTable.getDoc(id),
    func (u: UserDoc): ?RetailerDoc = fmap<RetailerId, RetailerDoc>(
      u.retailerId,
      func (i: RetailerId): ?RetailerDoc = retailerTable.getDoc(i)
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
  produceMarketInfo(public_key: PublicKey, produce_id:ProduceId, region_oid:?RegionId) : ?[ProduceMarketInfo] {
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
  producerAllInventoryInfo(public_key: PublicKey, id:UserId) : ?[InventoryInfo] {
    let doc = switch (producerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<InventoryId,InventoryDoc,InventoryInfo>(
      doc.inventory,
      func (_:InventoryId,doc:InventoryDoc):[InventoryInfo] =
        [inventoryTable.getInfoOfDoc()(doc)]
    )
  };

  /**
   `producerAddInventory`
   ---------------------------

  */
  producerAddInventory(
    public_key : Text,
    iid_       : ?InventoryId,
    id_        : UserId,
    produce_id : ProduceId,
    quantity_  : Quantity,
    weight_    : Weight,
    ppu_       : Price,
    start_date_: Date,
    end_date_  : Date,
    comments_  : Text,
  ) : Result<InventoryId, ServerErr>
  {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oproducer: ?ProducerDoc = producerFromUserId(id_);
    let oproduce  : ?ProduceDoc  = produceTable.getDoc(produce_id);
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
                func(iid:InventoryId):InventoryInfo{
        shared {
          id        = iid       :InventoryId;
          produce   = produce_id:ProduceId;
          producer  = id_       :ProducerId;
          quantity  = quantity_ :Quantity;
          weight    = weight_   :Weight;
          ppu       = ppu_      :Price;
          start_date=start_date_:Date;
          end_date  =end_date_  :Date;
          comments  =comments_  :Text;
        };
      })) {
      case (?item) { item };
      case (null) { unreachable() };
      }
    };

    /**- Update the producer's inventory collection to hold the new inventory document: */
    let updatedInventory =
      Map.insertFresh<InventoryId, InventoryDoc>(
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
    Map.insert2D<RegionId, ProducerId, InventoryMap>(
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
    iid_       : InventoryId,
    id_        : UserId,
    produce_id : ProduceId,
    quantity_  : Quantity,
    weight_    : Weight,
    ppu_       : Price,
    start_date_: Date,
    end_date_  : Date,
    comments_  : Text,
  ) : Result<(),ServerErr>
  {
    /**- Validate these ids; fail here if anything is invalid: */
    let oproducer: ?ProducerDoc = producerFromUserId(id_);
    let oinventory : ?InventoryDoc = inventoryTable.getDoc(iid_);
    let oproduce  : ?ProduceDoc  = produceTable.getDoc(produce_id);
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
    assertOk( producerRemInventory(public_key, iid_) );

    /**- add the (updated) inventory item; given the validation above, this cannot fail. */
    assertOk(
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
  producerRemInventory(public_key: PublicKey, id:InventoryId) : Result<(),ServerErr> {

    /**- validate the `id` */
    /// xxx macro for this pattern?
    let doc = switch (inventoryTable.getDoc(id)) {
      case null { return #err(#idErr) };
      case (?doc) { doc };
    };

    /**- remove document from `producerTable`, in several steps: */
    let producer = unwrap<ProducerDoc>(producerTable.getDoc(doc.producer));

    /// xxx: access control: Check that the current user is the owner of this inventory
    if (not isValidUser(public_key, producer.short_name)) {
      return (#err(#publicKeyErr))
    };

    /**- remove document from `inventoryTable` */
    assertSome<InventoryDoc>(
      inventoryTable.rem( id )
    );

    /// xxx an abstraction to hide these type arguments?
    let (updatedInventory, _) =
      Trie.remove<InventoryId, InventoryDoc>(
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

    assertSome<ProducerDoc>(
      producerTable.updateDoc( producer.id, updatedProducer )
    );

    /**- remove document from table `inventoryByRegion`: */
    /// xxx an abstraction to hide this tuple projection, assignment, and type args?
    inventoryByRegion := {
      let (t, d) = Trie.remove3D<RegionId, ProducerId, InventoryId, InventoryDoc>(
        inventoryByRegion,
        keyOf(producer.region.id), idIsEq,
        keyOf(producer.id), idIsEq,
        keyOf(id), idIsEq
      );
      assertSome<InventoryDoc>(d);
      t
    };

    #ok
  };

  /**
   `producerReservations`
   ---------------------------

   */
  producerReservations(public_key: PublicKey, id:UserId) : ?[ReservedInventoryInfo] {
    let doc = switch (producerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<ReservedInventoryId,
                 ReservedInventoryDoc,
                 ReservedInventoryInfo>(
      doc.reserved,
      func (_:ReservedInventoryId,
            doc:ReservedInventoryDoc):
        [ReservedInventoryInfo]
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
    rid_:            ?RouteId,
    id_:             UserId,
    start_region_id: RegionId,
    end_region_id:   RegionId,
    start_date_:     Date,
    end_date_:       Date,
    cost_:           Price,
    trucktype_id:    TruckTypeId
  ) : Result<RouteId,ServerErr> {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let otransporter : ?TransporterDoc = transporterFromUserId(id_);
    let orstart      : ?RegionDoc  = regionTable.getDoc(start_region_id);
    let orend        : ?RegionDoc  = regionTable.getDoc(end_region_id);
    let otrucktype   : ?TruckTypeInfo  = truckTypeTable.getInfo(trucktype_id);
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
    let route : RouteDoc = {
      switch (routeTable.addInfoAs(rid_, func(routeId:RouteId):RouteInfo{
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
      case null { unreachable() };
      }
    };

    /**- Update the transporter's routes collection to hold the new route document: */
    let updatedRoutes =
      Map.insertFresh<RouteId, RouteDoc>(
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
    Map.insert3D<RegionId, RegionId, RouteId, RouteDoc>(
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
    rid_            : RouteId,
    id_             : UserId,
    start_region_id : RegionId,
    end_region_id   : RegionId,
    start_date_     : Date,
    end_date_       : Date,
    cost_           : Price,
    trucktype_id    : TruckTypeId
  ) : Result<(),ServerErr> {
    /** The model updates routes and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oroute       : ?RouteDoc   = routeTable.getDoc(rid_);
    let otransporter : ?TransporterDoc = transporterFromUserId(id_);
    let orstart      : ?RegionDoc  = regionTable.getDoc(start_region_id);
    let orend        : ?RegionDoc  = regionTable.getDoc(end_region_id);
    let otrucktype   : ?TruckTypeDoc  = truckTypeTable.getDoc(trucktype_id);
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

    /**- validate the user */
    if (not isValidUser(public_key, transporter.short_name)) {
      return #err(#publicKeyErr)
    }

    /**- remove the route; given the validation above, this cannot fail. */
    assertOk( transporterRemRoute(public_key, rid_) );

    /**- add the (updated) route; given the validation above, this cannot fail. */
    assertOk(
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
  transporterRemRoute(public_key: PublicKey, id:RouteId) : Result<(),ServerErr> {

    let doc = switch (routeTable.getDoc(id)) {
      case null { return #err(#idErr) };
      case (?doc) { doc };
    };

    let transporter = unwrap<TransporterDoc>(transporterTable.getDoc(doc.transporter));

    if (not isValidUser(public_key, transporter.short_name)) {
      return #err(#publicKeyErr)
    }

    assertSome<RouteDoc>(
      routeTable.rem( id )
    );

    let (updatedRoutes, _) =
      Trie.remove<RouteId, RouteDoc>(
        transporter.routes, keyOf(id), idIsEq);

    let updatedTransporter = new {
      id          = transporter.id ;
      short_name  = transporter.short_name ;
      description = transporter.description ;
      routes      = updatedRoutes ;
      reserved    = transporter.reserved ;
    };

    assertSome<TransporterDoc>(
      transporterTable.updateDoc( transporter.id, updatedTransporter )
    );

    routesByDstSrcRegions := {
      let (t, d) = Trie.remove3D<RegionId, RegionId, RouteId, RouteDoc>(
        routesByDstSrcRegions,
        keyOf(doc.end_region.id), idIsEq,
        keyOf(doc.start_region.id), idIsEq,
        keyOf(doc.id), idIsEq
      );
      assertSome<RouteDoc>(d);
      t
    };

    #ok
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(public_key: PublicKey, id:UserId) : ?[RouteInfo] {
    let doc = switch (transporterFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<RouteId,
                 RouteDoc,
                 RouteInfo>(
      doc.routes,
      func (_:RouteId,
            doc:RouteDoc):
        [RouteInfo]
        =
        [routeTable.getInfoOfDoc()(doc)]
    )
  };

  /**
   `transporterReservationInfo`
   ---------------------------

   */
  transporterAllReservationInfo(public_key: PublicKey, id:UserId) : ?[ReservedRouteInfo] {
    let doc = switch (transporterFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<ReservedRouteId,
                 ReservedRouteDoc,
                 ReservedRouteInfo>(
      doc.reserved,
      func (_:ReservedRouteId,
            doc:ReservedRouteDoc):
        [ReservedRouteInfo]
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
  makeReservationInfo(item:InventoryDoc, route:RouteDoc) : ReservationInfo {
    shared {
      produce  =item.produce.id :ProduceId;
      producer =item.producer   :ProducerId;
      quant    =item.quantity   :Quantity;
      ppu      =item.ppu        :Price;
      weight   =item.weight     :Weight;
      prod_cost=item.quantity * item.ppu:Price;

      transporter = route.transporter :TransporterId;
      truck_type  = route.truck_type.id :TruckTypeId;

      region_begin = route.start_region.id:RegionId;
      region_end   = route.end_region.id  :RegionId;
      date_begin   = route.start_date  :Date;
      date_end     = route.end_date    :Date;
      trans_cost   = route.cost:  Price;
    }
  };


  /**

  `isCompatibleTruckType`
  ----------------------

  Check whether the given truck type can accommodate the given produce type.

  */
  isCompatibleTruckType(tt:TruckTypeDoc, produce:ProduceDoc) : Bool {
    // todo
    true
  };

  /**

  `isFeasibleReservation`
  ----------------------
  Check whether the given retailer can reserve the given item and route pair.

  */

  isFeasibleReservation(
    retailer:RetailerDoc,
    item:InventoryDoc,
    route:RouteDoc,
    queryProduce:?ProduceId,
    queryDate:?Date)
    : Bool
  {

    switch queryProduce {
      case null { };
      case (?qp) {
             if (item.produce.id != qp) {
               debugOff "nope: wrong produce kind\n";
               return false
             };
           };
    };
    switch queryDate {
      case null { };
      case (?qd) {
             if (route.end_date > qd ) {
               debugOff "nope: route arrives too late\n";
               return false
             }
           }
    };
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
  retailerQueryAll(
    public_key: PublicKey,
    id:UserId,
    queryProduce:?ProduceId,
    queryDate:?Date
  ) : ?QueryAllResults
  {
    retailerQueryCount += 1;

    /** - Find the retailer's document: */
    let retailer =
      switch (retailerFromUserId(id)) {
      case (null) { return null };
      case (?x) { x }};

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
      switch (Trie.find<RegionId, ByRegionRouteMap>(
                routesByDstSrcRegions,
                keyOf(retailer.region.id),
                idIsEq
              )) {
      case (null) { return ?[] };
      case (?x) { x }};

    debugInt(Trie.count<RegionId, RouteMap>(retailerRoutes));
    debug " production regions.\n";

    /** - Join: For each production region, consider all routes and inventory: */
    let queryResults : Trie<RegionId, RouteInventoryMap> = {
      retailerJoinCount += 1;
      Trie.join<RegionId,
                RouteMap,
                ByProducerInventoryMap,
                RouteInventoryMap>(
        retailerRoutes,
        inventoryByRegion,
        idIsEq,
        func (routes:RouteMap,
              inventory:ByProducerInventoryMap) : RouteInventoryMap
      {

        /** - Within this production region, consider every route-item pairing: */
        let product = Trie.prod<RouteId, RouteDoc,
                                InventoryId, InventoryDoc,
                                (RouteId, InventoryId),
                                (RouteDoc, InventoryDoc)>(
          routes,
          /** - (To perform this Cartesian product, use a 1D inventory map:) */
          Trie.mergeDisjoint2D<ProducerId, InventoryId, InventoryDoc>(
            inventory, idIsEq, idIsEq),

          func (route_id:RouteId,
                route   :RouteDoc,
                item_id :InventoryId,
                item    :InventoryDoc) :
            ?(Key<(RouteId, InventoryId)>,
              (RouteDoc, InventoryDoc))
        {
          retailerQueryCost += 1;
          /** - Consider the constraints of the retailer-route-item combination: */
          if (isFeasibleReservation(retailer, item, route, queryProduce, queryDate)) {
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
      Trie.mergeDisjoint2D<RegionId, (RouteId, InventoryId), (RouteDoc, InventoryDoc)>(
        queryResults, idIsEq, idPairIsEq);

    debug "- query result count: ";
    debugInt(Trie.count<(RouteId, InventoryId),
                        (RouteDoc, InventoryDoc)>(queryResultsMerged));
    debug " (count of feasible route-item pairs).\n";

    /** - Prepare reservation information for client, as an array; see also [`makeReservationInfo`](#makereservationinfo) */
    let arr =
      Trie.toArray<(RouteId, InventoryId),
                   (RouteDoc, InventoryDoc),
                   ReservationInfo>(
        queryResultsMerged,
        func (_:(RouteId,InventoryId), (r:RouteDoc, i:InventoryDoc))
          : [ ReservationInfo ] {
            [ makeReservationInfo(i, r) ]
          });

    ?arr
  };

  /**
   `retailerAllReservationInfo`
   ---------------------------

  */
  retailerAllReservationInfo(public_key: PublicKey, id:UserId) :
    ?[(ReservedInventoryInfo,
       ReservedRouteInfo)]
  {
    let doc = switch (retailerFromUserId(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<ReservedInventoryId,
                 (ReservedInventoryDoc,  ReservedRouteDoc),
                 (ReservedInventoryInfo, ReservedRouteInfo)>(
      doc.reserved,
      func (_:ReservedInventoryId,
            ((idoc:ReservedInventoryDoc),
             (rdoc:ReservedRouteDoc)))
            :
            [(ReservedInventoryInfo,
              ReservedRouteInfo)]
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
    public_key: PublicKey,
    id:UserId,
    begin:Date,
    end:Date
  ) : ?[InventoryInfo]
  {
    retailerQueryCount += 1;

    nyi()
  };

  /**
   `retailerReserve`
   ---------------------------
  */
  retailerReserve(
    public_key: PublicKey,
    id:UserId,
    inventory:InventoryId,
    route:RouteId) : ?(ReservedRouteId, ReservedInventoryId)
  {
    nyi()
  };

};
