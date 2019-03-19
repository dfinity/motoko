/**

[PESS Background](https://github.com/dfinity-lab/actorscript/tree/stdlib-examples/design/stdlib/examples/produce-exchange#Produce-Exchange-Standards-Specification-PESS)
--------------------

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

class Model() = this {


/**
   
Representation
=================
   
We use several public-facing **tables**, implemented as document tables.
   

CRUD operations via [document tables](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md)
----------------------------------------------------

This server model provides [document table](https://github.com/dfinity-lab/actorscript/blob/stdlib-examples/design/stdlib/docTable.md) objects to hold the
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
   `truckTypeTable`
   -----------------
   */
  
  var truckTypeTable : TruckTypeTable = 
    DocTable<TruckTypeId, TruckTypeDoc, TruckTypeInfo>(
      //@Omit:
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
    /** */
  );    

  /** 
   `regionTable`
   -----------------
   */

  var regionTable : RegionTable = 
    DocTable<RegionId, RegionDoc, RegionInfo>(
      // @Omit:
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
    /** */
  );

  /** 
   `produceTable`
   -----------------
   */

  var produceTable : ProduceTable = 
    DocTable<ProduceId, ProduceDoc, ProduceInfo>(
    //@Omit:
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
    /** */
  );

  /**
   `inventoryTable`
   ---------------
   */

  private var inventoryTable : InventoryTable =
    DocTable<InventoryId, InventoryDoc, InventoryInfo>(
    //@Omit:
    0,
    func(x:InventoryId):InventoryId{x+1},
    func(x:InventoryId,y:InventoryId):Bool{x==y},
    idHash,
    func(doc:InventoryDoc):InventoryInfo = shared {
      id=doc.id; 
      produce=doc.produce.id;
      producer=doc.producer;
      quantity=doc.quantity;
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
                   start_date=info.start_date;
                   end_date=info.end_date;
                   comments=info.comments;
                 })
             };
        case _ {
               null
             }
      }}
  /** */
  );

  /**
   `reservedInventoryTable`
   ---------------------------
   */

  var reservedInventoryTable : ReservedInventoryTable = 
    DocTable<ReservedInventoryId, ReservedInventoryDoc, ReservedInventoryInfo>(
    //@Omit:
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
        case (?item, ?_) {
               ?(new {
                   id=info.id; 
                   item=item:InventoryDoc;
                   retailer=info.retailer;
                 })
             };
        case _ {
               null
             }
      }}
  /** */
    );


  /** 
   `producerTable`
   -----------------
   */

  var producerTable : ProducerTable = 
    DocTable<ProducerId, ProducerDoc, ProducerInfo>(      
      //@Omit:
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
      switch (regionTable.getDoc(info.region)) {
        case (?regionDoc) {
               ?(new {
                   id=info.id; 
                   short_name=info.short_name; 
                   description=info.description;
                   region=regionDoc;
                   inventory=inventoryTable.empty();
                   reserved=reservedInventoryTable.empty();
                 }
               )};
        case (null) {
               null
             };
      }
  /** */
    );

  /** 
   `transporterTable`
   -----------------
   */

  var transporterTable : TransporterTable = 
    DocTable<TransporterId, TransporterDoc, TransporterInfo> (
      //@Omit:
      
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
            routes=routeTable.empty();
            reserved=reservedRouteTable.empty();
          })
  /** */
    );
  
  /** 
   `retailerTable`
   -----------------
   */
  
  var retailerTable : RetailerTable = 
    DocTable<RetailerId, RetailerDoc, RetailerInfo>(
      //@Omit:
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
      func(info:RetailerInfo):?RetailerDoc {
        switch (regionTable.getDoc(info.region)) 
        {
        case (?regionDoc) {
               ?(new {
                   id=info.id; 
                   short_name=info.short_name; 
                   description=info.description;
                   region=regionDoc;
                   reserved=null;
                 }
               )};
        case (null) { null };
        }}
  /** */
    );

  /**
   `routeTable`
   ----------------
   */

  var routeTable : RouteTable = 
    DocTable<RouteId, RouteDoc, RouteInfo> (
      //@Omit:      
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
  /** */
    );

  /**
   `reservedRouteTable`
   ----------------
   */

  var reservedRouteTable : ReservedRouteTable = 
    DocTable<ReservedRouteId, ReservedRouteDoc, ReservedRouteInfo>(
    //@Omit:
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
        case (?route, ?_) {
               ?(new {
                   id=info.id; 
                   route=route:RouteDoc;
                   retailer=info.retailer;
                 })
             };
        case _ {
               null
             }
      }}
  /** */
    );

    
  /**

   Indexing by `RegionId`
   =====================================

   For efficient queries, need some extra indexing.

   Regions as keys in special global maps
   ---------------------------------------
   - inventory (across all producers) keyed by producer region
   - routes (across all transporters) keyed by source region
   - routes (across all transporters) keyed by destination region

   Routes by region-region pair
   ----------------------------

   the actor maintains a possibly-sparse 3D table mapping each
   region-region-routeid triple to zero or one routes.  First index
   is destination region, second index is source region; this 2D
   spatial coordinate gives all routes that go to that destination
   from that source, keyed by their unique route ID, the third
   coordinate of the mapping.

   */

  private var routesByDstSrcRegions : ByRegionsRouteMap = null;

  /**
   Inventory by source region
   ----------------------------
  
   the actor maintains a possibly-sparse 3D table mapping each
   sourceregion-producerid-inventoryid triple to zero or one
   inventory items.  The 1D coordinate sourceregion gives all of the
   inventory items, by producer id, for this source region.
  
  */
  
  private var inventoryByRegion : ByRegionInventoryMap = null;


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

   PESS Behavior: message-response specifications
   ======================================================

   As explained in the `README.md` file, this actor also gives a
   behavioral spec of the exchange's semantics, by giving a prototype
   implementation of this behavior (and wrapped trivially by `Server`).

   The functional behavior of this interface, but not implementation
   details, are part of the formal PESS.

   */



  /**

   `Produce`-oriented operations
   ==========================================

   */


  /**
   `produceMarketInfo`
   ---------------------------
   The last sales price for produce within a given geographic area; null region id means "all areas."
   */
  produceMarketInfo(id:ProduceId, reg:?RegionId) : ?[ProduceMarketInfo] {
    // xxx aggregate
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
  producerAllInventoryInfo(id:ProducerId) : ?[InventoryInfo] {
    // xxx view
    null
  };

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
  ) : ?InventoryId 
  {
    let oproducer : ?ProducerDoc = producerTable.getDoc(id);
    let oproduce  : ?ProduceDoc  = produceTable.getDoc(prod);
    
    // check whether these ids are defined; fail fast if not defined
    let (producer, produce) = {
      switch (oproducer, oproduce) {
      case (?producer, ?produce) (producer, produce);
      case _ { return null };
      }};
    
    // get a unique id for the inventory item; add it to the global table
    let (_, item) = {
      switch (inventoryTable.addInfo(
                func(inventoryId:InventoryId):InventoryInfo{
        shared {
          id= inventoryId;
          produce= produce:ProduceId;
          producer= prod:ProducerId;
          quantity= quantity:Quantity;
          start_date=begin:Date;
          end_date=end:Date;
          comments=comments:Text;
        };
      })) {
      case (?item) { item };
      case (null) { assert false ; return null };
      }
    };

    // update the producer document to hold the new inventory item
    //
    // xxx more concise syntax for record updates would be nice:
    let updatedProducer : ProducerDoc = new {
      id = producer.id;
      short_name = producer.short_name;
      description = producer.description;
      region = producer.region;
      reserved = producer.reserved;
      inventory =
        Map.insertFresh<InventoryId, InventoryDoc>(
          producer.inventory,
          keyOf(item.id),
          idIsEq,
          item
        )
    };
    // Update producer document table:
    let _ = producerTable.updateDoc(id, updatedProducer);
    
    // Update inventoryByRegion mapping:
    inventoryByRegion :=
    Map.insertFresh2D<RegionId, ProducerId, InventoryMap>(
      inventoryByRegion,
      // key1: region id of the producer
      keyOf(producer.region.id), idIsEq,
      // key2: producer id */
      keyOf(producer.id), idIsEq,
      // value: producer's updated inventory table
      updatedProducer.inventory,
    );
    
    // return the item's id
    ?item.id
  };

  /**
   `producerRemInventory`
   ---------------------------
   

   **Implementation summary:**

    - remove from the inventory in inventory table; use `Trie.removeThen`
    - if successful, look up the producer ID; should not fail; `Trie.find`
    - update the producer, removing this inventory; use `Trie.{replace,remove}`
    - finally, use producer's region to update inventoryByRegion table,
      removing this inventory item; use `Trie.remove2D`.   
   */
  producerRemInventory(id:InventoryId) : ?() {
    // xxx rem
    null
  };

  /**
   `producerReservations`
   ---------------------------
   
   */
  producerReservations(id:ProducerId) : ?[ReservedInventoryId] {
    // xxx view
    null
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
    trans:  TransporterId,
    rstart: RegionId,
    rend:   RegionId,
    start:  Date,
    end:    Date,
    cost:   Price,
    ttid:   TruckTypeId
  ) : ?RouteId {
    // xxx add
    null
  };

  /**
   `transporterRemRoute`
   ---------------------------
   

   **Implementation summary:**

    - remove from the inventory in inventory table; use `Trie.removeThen`
    - if successful, look up the producer ID; should not fail; `Trie.find`
    - update the transporter, removing this inventory; use `Trie.{replace,remove}`
    - finally, use route info to update the routesByRegion table,
      removing this inventory item; use `Trie.remove2D`.   
   */
  transporterRemRoute(id:RouteId) : ?() {
    // xxx rem
    null
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  transporterAllRouteInfo(id:RouteId) : ?[RouteInfo] {
    // xxx view
    null
  };

  /**
   `transporterReservationInfo`
   ---------------------------
   
   */
  transporterAllReservationInfo(id:TransporterId) : ?[ReservedRouteInfo] {
    // xxx view
    null
  };


  /**
   `Retailer`-facing operations
   ====================
   */
  

  /**
   `retailerQueryAll`
   ---------------------------

   TODO-Cursors (see above).

  */
  retailerQueryAll(id:RetailerId) : ?QueryAllResults {
    // xxx join
    null
  };

  /**
   `retailerAllReservationInfo`
   ---------------------------

   TODO-Cursors (see above).

  */
  retailerAllReservationInfo(id:RetailerId) : ?[ReservedInventoryInfo] {
    // xxx view
    null
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
  ) : ?[InventoryInfo]
  {
    // xxx join+filter
    null
  };

  /**
   `retailerReserve`
   ---------------------------
  */
  retailerReserve(
    id:RetailerId,
    inventory:InventoryId,
    route:RouteId) : ?(ReservedRouteId, ReservedInventoryId)
  {
    // xxx add/rem
    null
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
  ) : ?(ReservedInventoryId, ReservedRouteId)
  {
    // xxx query+add/rem
    null
  };



  /**
   Misc helpers
   ==================
   */ 

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

};
