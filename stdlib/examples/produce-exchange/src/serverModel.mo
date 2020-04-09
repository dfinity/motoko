import Debug "mo:stdlib/Debug";
import P "mo:stdlib/Prelude";

import T "serverTypes";
import L "serverLang";
import M "serverModelTypes";

import List "mo:stdlib/List";
import Hash "mo:stdlib/Hash";
import Option "mo:stdlib/Option";
import Trie "mo:stdlib/Trie";
import Iter "mo:stdlib/Iter";
import Array "mo:stdlib/Array";

import Result "mo:stdlib/Result";
import DT = "docTable";

module {
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


public type List<T> = List.List<T>;
public type Hash = Hash.Hash;
public type Trie<K,V> = Trie.Trie<K,V>;
public type TrieBuild<K,V> = Trie.Build.TrieBuild<K,V>;
public type Key<K> = Trie.Key<K>;

public type Table<K,V> = Trie.Trie<K,V>;
public let Table = Trie;

public type Map<K,V> = Trie.Trie<K,V>;
public let Map = Trie;

public let DocTable = DT.DocTable;
public type DocTable<X,Y,Z> = DT.DocTable<X,Y,Z>;

public type Result<Ok,Err> = Result.Result<Ok,Err>;

public type RouteInventoryMap = Trie<(T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)>;
public type QueryResult = TrieBuild<(T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)>;

public type RoleId = {
  #user        : T.UserId;
  #producer    : T.ProducerId;
  #transporter : T.TransporterId;
  #retailer    : T.RetailerId;
};

public class Model() {

  /**
   PX server language semantics
   ===================================================================================================
   Evaluation semantics for the PX server language
   */

  /**
   `loadWorkload`
   ----------------
   clear existing server state, and replace with a synthetic workload, based on the given parameters.
   */

  public func loadWorkload(params:T.WorkloadParams) : () {
    func db(s:Text) = if false {Debug.print "Model::loadWorkload: "; Debug.print s; Debug.print "\n"};

    /**- generate add requests for these params: */
    db "generate requests for workload...";
    let addreqs : List<L.AddReq> = genAddReqs(
      params.day_count,
      params.max_route_duration,
      params.producer_count,
      params.transporter_count,
      params.retailer_count,
      params.region_count
    );

    /**- reset to initial state before adding this new workload: */
    let reqs : List<L.Req> =
      ?(#reset, List.map<L.AddReq,L.Req>(addreqs, func (r:L.AddReq):L.Req = #add r));

    /**- evaluate each request: */
    db "evaluate requests for workload...";
    let resps = evalReqList(reqs);

    /**- assert that everything worked; a sanity check. */
    // to do
  };


  /**
   `evalReq`
   ----------
   evaluate an API call (a "request"), represented as an AS datatype.

   supported calls currently consists of each kind of "add", where
   each request has the form `#add (entityTag entityInfo)`, and
   where the type of `entityInfo` varies with each `entityTag` case.

   In each case, the evaluation logic either directly adds the entity
   to the relevant master table (e.g., the `#add #trucktype` case), or it
   uses an existing operation to do so, and keeps additional indexing
   maps up to date (e.g., `producerAddInventory`).  Critically, since
   this file does not perform access control checks, that logic is
   separate from this semantic model-level logic.

   To do, each similar to the `producerAddInventory` case:
   - remove (`#rem`) cases
   - update (`#update`) cases

   */
  public func evalReq(req:L.Req) : Result<L.Resp, T.IdErr> {
    if false {Debug.print "Model::evalReq: "; Debug.print (debug_show req); Debug.print "\n"; };
    switch req {
    case (#reset) {
           /**- 1. reset each entity table: */
           userTable.reset();
           truckTypeTable.reset();
           regionTable.reset();
           produceTable.reset();
           producerTable.reset();
           inventoryTable.reset();
           transporterTable.reset();
           retailerTable.reset();
           routeTable.reset();
           reservedInventoryTable.reset();
           reservedRouteTable.reset();

           /**- 2. clear secondary indices: */
           usersByUserName
             := Map.empty<T.UserName, T.UserId>();

           routesByDstSrcRegions
             := Map.empty<T.RegionId, M.ByRegionRouteMap>();

           inventoryByRegion
             := Map.empty<T.RegionId, M.ByProducerInventoryMap>();

           reservationsByProduceByRegion
             := Map.empty<T.ProduceId,
                          Map<T.RegionId,
                              Map<T.ReservedInventoryId,
                                  M.ReservedInventoryDoc>>>();

           /**- 3. reset counters: */
           joinCount := 0;
           retailerQueryCount := 0;
           retailerQueryCost := 0;
           retailerJoinCount := 0;
           retailerQuerySizeMax := 0;

           #ok(#reset)
         };
    case (#add (#truckType info)) Result.fromSomeMap<T.TruckTypeId,L.Resp,T.IdErr>(
           truckTypeTable.addInfoGetId(
             func (id_:T.TruckTypeId) : T.TruckTypeInfo =
               {
                 id=id_;
                 short_name=info.short_name;
                 description=info.description;
                 capacity=info.capacity;
                 isFridge=info.isFridge;
                 isFreezer=info.isFreezer;
               }
           ),
           func (id:T.TruckTypeId):L.Resp = #add(#truckType(id)),
           #idErr null
         );

    case (#add (#region info)) Result.fromSomeMap<T.RegionId,L.Resp,T.IdErr>(
           regionTable.addInfoGetId(
             func (id_:T.RegionId) : T.RegionInfo =
               {
                 id = id_;
                 short_name=info.short_name;
                 description=info.description
               }
           ),
           func (id:T.RegionId):L.Resp = #add(#region(id)),
           #idErr null
         );

    case (#add (#produce info)) Result.fromSomeMap<T.ProduceId,L.Resp,T.IdErr>(
           produceTable.addInfoGetId(
             func (id_:T.ProduceId) : T.ProduceInfo =
               {
                 id = id_;
                 short_name=info.short_name;
                 description=info.description;
                 grade=info.grade
               }
           ),
           func (id:T.ProduceId):L.Resp = #add(#produce(id)),
           #idErr null
         );

    case (#add (#producer info)) Result.fromSomeMap<T.ProducerId,L.Resp,T.IdErr>(
           producerTable.addInfoGetId(
             func(id_:T.ProducerId):T.ProducerInfo {
               {
                 id=id_;
                 public_key=info.public_key;
                 short_name=info.short_name;
                 description=info.description;
                 region=info.region;
                 inventory=[];
                 reserved=[];
               }
             }
           ),
           func (id:T.ProducerId):L.Resp = #add(#producer(id)),
           #idErr null
         );

    case (#add (#transporter info)) Result.fromSomeMap<T.TransporterId,L.Resp,T.IdErr>(
           transporterTable.addInfoGetId(
             func(id_:T.TransporterId):T.TransporterInfo {
               {
                 id=id_;
                 public_key=info.public_key;
                 short_name=info.short_name;
                 description=info.description;
                 routes=[];
                 reserved=[];
               }
             }
           ),
           func (id:T.TransporterId):L.Resp = #add(#transporter(id)),
           #idErr null
         );

    case (#add (#retailer info)) Result.fromSomeMap<T.RetailerId,L.Resp,T.IdErr>(
           retailerTable.addInfoGetId(
             func(id_:T.RetailerId):T.RetailerInfo {
               {
                 id=id_;
                 public_key=info.public_key;
                 short_name=info.short_name;
                 description=info.description;
                 region=info.region;
               }
             }
           ),
           func (id:T.RetailerId):L.Resp = #add(#retailer(id)),
           #idErr null
         );

    case (#add (#route info)) Result.mapOk<T.RouteId,L.Resp,T.IdErr>(
           transporterAddRoute(
             null,
             info.transporter,
             info.start_region,
             info.end_region,
             info.start_date,
             info.end_date,
             info.cost,
             info.truck_type
           ),
           func (id:T.RouteId):L.Resp = #add(#route(id))
         );

    case (#add (#inventory info)) Result.mapOk<T.InventoryId,L.Resp,T.IdErr>(
           producerAddInventory(
             null,
             info.producer,
             info.produce,
             info.quantity,
             info.weight,
             info.ppu,
             info.start_date,
             info.end_date,
             info.comments,
           ),
           func (id:T.RouteId):L.Resp = #add(#route(id))
         );

    case (#add (#user info)) Result.fromSomeMap<T.UserId,L.Resp,T.IdErr>(
           addUser(
             info.public_key,
             info.user_name,
             info.description,
             info.region,
             info.isDeveloper,
             info.isProducer,
             info.isRetailer,
             info.isTransporter
           ),
           func (id:T.UserId):L.Resp = #add(#user(id)),
           #idErr null
         );

    //case (#add _) P.unreachable();

    case _ P.nyi();
    }
  };

  /**
   `evalReqList`
   ----------
   */
  public func evalReqList(reqs:List<L.Req>) : List<L.ResultResp> =
    List.map<L.Req, L.ResultResp>(reqs, evalReq);

  /**
   `evalBulk`
   ----------
   evaluate groups of similar API calls, grouping their results.
   Each bulk request consists of an array of similar requests (adds, updates, or removes).
   */
  public func evalBulk(req:L.BulkReq) : L.BulkResp {
    func eatAdd (x:L.Resp) : T.EntId =
      switch x { case (#add i) i; case _ P.unreachable() };
    switch req {
      case (#add reqs)
      #add (
        Array.tabulate<Result<T.EntId, T.IdErr>>(
          reqs.len(),
          func(i:Nat):Result<T.EntId, T.IdErr> =
            Result.mapOk<L.Resp, T.EntId, T.IdErr>(
              evalReq(#add(reqs[i])),
              eatAdd
            )
        )
      );

      case (#rem reqs)
      P.nyi();

      case (#update reqs)
      P.nyi();
    }
  };

  /**
   `evalBulkArray`
   ----------
   evaluate arrays of bulk requests.
   Notice that there are two levels of grouping:
   - The request is an array of bulk requests, and
   - Each bulk request in this array consists of an array of similar requests (adds, updates, or removes).
   */
  public func evalBulkArray(reqs:[L.BulkReq]) : [L.BulkResp] {
    Array.tabulate<L.BulkResp>(
      reqs.len(),
      func(i:Nat):L.BulkResp = evalBulk(reqs[i])
    )
  };

  /**
   `countAddReqs`
   -------------
   xxx
   */
  public func countAddReqs(
    day_count:Nat,
    max_route_duration:Nat,
    producer_count:Nat,
    transporter_count:Nat,
    retailer_count:Nat,
    region_count:Nat
  ) : (Nat, Nat) {
    var inventoryCount : Nat = 0;
    var routeCount : Nat = 0;

    assert(region_count > 0);
    func min(x:Nat, y:Nat) : Nat = if (x < y) { x } else { y };

    /**- add routes and inventory, across time and space: */
    for (start_day in Iter.range(0, day_count-1)) {

      let max_end_day =
        min( start_day + max_route_duration,
             day_count - 1 );

      for (end_day in Iter.range(start_day, max_end_day)) {

        /**- consider all pairs of start and end region: */
        for (start_reg in Iter.range(0, region_count - 1)) {
          for (end_reg in Iter.range(0, region_count - 1)) {

            /**- for each producer we choose,
             add inventory that will be ready on "start_day", but not beforehand.
             It will remain ready until the end of the day count. */

            for (p in Iter.range(0, producer_count - 1)) {
              /**- choose this producer iff they are located in the start region: */
              if ((p % region_count) == start_reg) {
                inventoryCount := inventoryCount + 1;
              };
            };

            /**- for each transporter we choose,
             add a route that will start and end on the current values
             of `start_day`, `end_day`, `start_reg`, `end_reg`, respectively: */

            for (t in Iter.range(0, transporter_count - 1)) {
              /**- choose this transporter iff their id matches the id of the region, modulo the number of regions: */
              if ((t % region_count) == start_reg) {
                routeCount := routeCount + 1;
              }
            }
          };
        };
      };
    };

    return (inventoryCount, routeCount)
  };

  /**
   `genAddReqs`
   -------------
   generate a list of add requests, based on a set of parameters,
   and a simple, deterministic strategy, with predictable combinatoric properties.

   Used for seeding unit tests with predictable properties,
   and for profiling performance properties of the produce exchange,
   e.g., average response time for a retailer query.
   */
  public func genAddReqs(
    day_count:Nat,
    max_route_duration:Nat,
    producer_count:Nat,
    transporter_count:Nat,
    retailer_count:Nat,
    region_count:Nat
  ) : List<L.AddReq> {
    assert(region_count > 0);
    func min(x:Nat, y:Nat) : Nat = if (x < y) { x } else { y };
    /**- `reqs` accumulates the add requests that we form below: */
    var reqs : List<L.AddReq> = null;
    /**- add truck types; for now, just 2. */
    for (i in Iter.range(0, 1)) {
      reqs := ?(
        #truckType (
          {
            id=i; short_name=""; description="";
            capacity=i * 50;
            isFridge=true; isFreezer=true
          }
        ), reqs);
    };
    /**- add regions */
    for (i in Iter.range(0, region_count - 1)) {
      reqs := ?(#region {
                           id=i; short_name=""; description=""
                        }, reqs);
    };
    /**- add produce types, one per region. */
    for (i in Iter.range(0, region_count - 1)) {
      reqs := ?(#produce {
                           id=i; short_name=""; description=""; grade=1;
                         }, reqs);
    };
    /**- add producers  */
    for (i in Iter.range(0, producer_count - 1)) {
      reqs := ?(#producer {
                            id=i; public_key=""; short_name=""; description="";
                            region=(i % region_count);
                            inventory=[]; reserved=[];
                          }, reqs);
    };
    /**- add transporters  */
    for (i in Iter.range(0, producer_count - 1)) {
      reqs := ?(#transporter {
                                id=i; public_key=""; short_name=""; description="";
                                routes=[]; reserved=[];
                             }, reqs);
    };
    /**- add retailers  */
    for (i in Iter.range(0, retailer_count - 1)) {
      reqs := ?(#retailer {
                            id=i; public_key=""; short_name=""; description="";
                            region=(i % region_count);
                          }, reqs);
    };
    /**- add routes and inventory, across time and space: */
    for (start_day in Iter.range(0, day_count-1)) {

      let max_end_day =
        min( start_day + max_route_duration,
             day_count - 1 );

      for (end_day in Iter.range(start_day, max_end_day)) {

        /**- consider all pairs of start and end region: */
        for (start_reg in Iter.range(0, region_count - 1)) {
          for (end_reg in Iter.range(0, region_count - 1)) {

            /**- for each producer we choose,
             add inventory that will be ready on "start_day", but not beforehand.
             It will remain ready until the end of the day count. */

            for (p in Iter.range(0, producer_count)) {
              /**- choose this producer iff they are located in the start region: */
              if ((p % region_count) == start_reg) {
                reqs := ?(#inventory
                {
                   id=666;
                   producer   = p;
                   produce    = start_reg;
                   start_date = start_day;
                   end_date   = day_count-1;
                   quantity   = 33;
                   weight     = 66;
                   ppu        = 22;
                   comments   = "";
                }, reqs);
              }
            };

            /**- for each transporter we choose,
             add a route that will start and end on the current values
             of `start_day`, `end_day`, `start_reg`, `end_reg`, respectively: */

            for (t in Iter.range(0, transporter_count - 1)) {
              /**- choose this transporter iff their id matches the id of the region, modulo the number of regions: */
              if ((t % region_count) == start_reg) {
                reqs := ?(#route
                {
                   id=666;
                   transporter  = t;
                   start_region = start_reg;
                   end_region   = end_reg;
                   // there are two truck types; just choose one based on the "produce type", which is based on the region:
                   truck_type   = (start_reg % 2);
                   start_date   = start_day;
                   end_date     = end_day;
                   // cost is inversely proportional to route duration: slower is cheaper.
                   // this example cost formula is "linear in duration, with a fixed base cost":
                   cost         = 100 + ((end_day - start_day) * 20);
                }, reqs);
              }
            };
          };
        };
      };
    };
    /**- use "chronological order" for adds above: */
    List.rev<L.AddReq>(reqs)
  };

  /**
   Access control
   ===================================================================================================

   `isValidPublicKey`
   -------------------
   Match a given public key to that of an identified role, whose public key on record.

   */
  public func isValidPublicKey(id:RoleId, public_key:T.PublicKey) : Bool {
    switch id {
    case (#user id) {
           switch (userTable.getDoc(id)) {
           case null false;
           case (?p) { p.public_key == public_key };
           }
         };
    case (#producer id) {
           switch (producerTable.getDoc(id)) {
           case null false;
           case (?p) { p.public_key == public_key };
           }
         };
    case (#transporter id) {
           switch (transporterTable.getDoc(id)) {
           case null false;
           case (?p) { p.public_key == public_key };
           }
         };
    case (#retailer id) {
           switch (retailerTable.getDoc(id)) {
           case null false;
           case (?p) { p.public_key == public_key };
           }
         };
    }
  };

  /**
   Misc helpers
   ==================
   */

  func debugOut (t:Text)   { debug { Debug.print t } };
  func debugInt (i:Int)    { debug { Debug.print (debug_show i) } };

  func debugOff (t:Text)   { debug {  } };
  func debugIntOff (i:Int) { debug {  } };
  func debugOffInt (i:Int) { debug {  } };

  func idIsEq(x:Nat,y:Nat):Bool { x == y };

  func textIsEq(x:Text,y:Text):Bool { x == y };

  func idPairIsEq(x:(Nat,Nat),y:(Nat,Nat)):Bool { x.0 == y.0 and x.1 == y.1 };

  func idHash(x:Nat):Hash { Hash.hashOfInt(x) };

  func idPairHash(x:(Nat,Nat)):Hash { Hash.hashOfIntAcc(Hash.hashOfInt(x.0), x.1) };

  func keyOf(x:Nat):Key<Nat> {
   { key = x ; hash = idHash(x) }
  };

  func keyOfIdPair(x:Nat, y:Nat):Key<(Nat,Nat)> {
   { key = (x,y) ; hash = idPairHash(x,y) }
  };

  func keyOfText(x:Text):Key<Text> {
   { key = x ; hash = Hash.hashOfText(x) }
  };

  /**
   Misc counters
   ==================
   */

  public var joinCount = 0;
  public var retailerQuerySizeMax = 0;


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

  public var userTable : M.UserTable =
    DocTable<T.UserId, M.UserDoc, T.UserInfo>(
    0,
    func(x:T.UserId):T.UserId{x+1},
    func(x:T.UserId,y:T.UserId):Bool{x==y},
    idHash,
    func(doc:M.UserDoc):T.UserInfo = {
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
    func(info:T.UserInfo):?M.UserDoc = ?{
      id=info.id;
      user_name=info.user_name;
      public_key=info.public_key;
      description=info.description;
      region=info.region;
      producerId=info.producerId;
      transporterId=info.transporterId;
      retailerId=info.retailerId;
      isDeveloper=info.isDeveloper;
    },
  );


  /**
   `truckTypeTable`
   -----------------
   */

  public var truckTypeTable : M.TruckTypeTable =
    DocTable<T.TruckTypeId, M.TruckTypeDoc, T.TruckTypeInfo>(
    0,
    func(x:T.TruckTypeId):T.TruckTypeId{x+1},
    func(x:T.TruckTypeId,y:T.TruckTypeId):Bool{x==y},
    idHash,
    func(doc:M.TruckTypeDoc):T.TruckTypeInfo = {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      capacity=doc.capacity;
      isFridge=doc.isFridge;
      isFreezer=doc.isFreezer;
    },
    func(info:T.TruckTypeInfo):?M.TruckTypeDoc = ?{
      id=info.id;
      short_name=info.short_name;
      description=info.description;
      capacity=info.capacity;
      isFridge=info.isFridge;
      isFreezer=info.isFreezer;
    },
  );

  /**
   `regionTable`
   -----------------
   */

  public var regionTable : M.RegionTable =
    DocTable<T.RegionId, M.RegionDoc, T.RegionInfo>(
    0,
    func(x:T.RegionId):T.RegionId{x+1},
    func(x:T.RegionId,y:T.RegionId):Bool{x==y},
    idHash,
    func(doc:M.RegionDoc):T.RegionInfo = {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
    },
    func(info:T.RegionInfo):?M.RegionDoc = ?{
      id=info.id;
      short_name=info.short_name;
      description=info.description;
    },
  );

  /**
   `produceTable`
   -----------------
   */

  public var produceTable : M.ProduceTable =
    DocTable<T.ProduceId, M.ProduceDoc, T.ProduceInfo>(
    0,
    func(x:T.ProduceId):T.ProduceId{x+1},
    func(x:T.ProduceId,y:T.ProduceId):Bool{x==y},
    idHash,
    func(doc:M.ProduceDoc):T.ProduceInfo = {
      id=doc.id;
      short_name=doc.short_name;
      description=doc.description;
      grade=doc.grade;
    },
    func(info:T.ProduceInfo):?M.ProduceDoc = ?{
      id=info.id;
      short_name=info.short_name;
      description=info.description;
      grade=info.grade;
    },
  );

  /**
   `producerTable`
   -----------------
   */

  public var producerTable : M.ProducerTable =
    DocTable<T.ProducerId, M.ProducerDoc, T.ProducerInfo>(
    0,
    func(x:T.ProducerId):T.ProducerId{x+1},
    func(x:T.ProducerId,y:T.ProducerId):Bool{x==y},
    idHash,
    func(doc:M.ProducerDoc):T.ProducerInfo = {
      id=doc.id;
      public_key=doc.public_key;
      short_name=doc.short_name;
      description=doc.description;
      region=doc.region.id;
      inventory=[];
      reserved=[];
    },
    func(info:T.ProducerInfo):?M.ProducerDoc =
      Option.map<M.RegionDoc, M.ProducerDoc>(
        func (regionDoc: M.RegionDoc): M.ProducerDoc = {
          id=info.id;
          public_key=info.public_key;
          short_name=info.short_name;
          description=info.description;
          region=regionDoc;
          inventory=Table.empty<T.InventoryId, M.InventoryDoc>();
          reserved=Table.empty<T.ReservedInventoryId, M.ReservedInventoryDoc>();
        },
        regionTable.getDoc(info.region),
      )
    );


  /**
   `inventoryTable`
   ---------------
   */

  public var inventoryTable : M.InventoryTable =
    DocTable<T.InventoryId, M.InventoryDoc, T.InventoryInfo>(
    0,
    func(x:T.InventoryId):T.InventoryId{x+1},
    func(x:T.InventoryId,y:T.InventoryId):Bool{x==y},
    idHash,
    func(doc:M.InventoryDoc):T.InventoryInfo = {
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
               ?{
                   id=info.id;
                   produce=produceDoc;
                   producer=producerDoc.id;
                   quantity=info.quantity;
                   weight=info.weight;
                   ppu=info.ppu;
                   start_date=info.start_date;
                   end_date=info.end_date;
                   comments=info.comments;
                 }
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

  public var transporterTable : M.TransporterTable =
    DocTable<T.TransporterId, M.TransporterDoc, T.TransporterInfo> (
      0,
      func(x:T.TransporterId):T.TransporterId{x+1},
      func(x:T.TransporterId,y:T.TransporterId):Bool{x==y},
      idHash,
      func(doc:M.TransporterDoc):T.TransporterInfo = {
        id=doc.id;
        public_key=doc.public_key;
        short_name=doc.short_name;
        description=doc.description;
        routes=[];
        reserved=[];
      },
      func(info:T.TransporterInfo):?M.TransporterDoc =
        ?{
            id=info.id;
            public_key=info.public_key;
            short_name=info.short_name;
            description=info.description;
            routes=Table.empty<T.RouteId, M.RouteDoc>();
            reserved=Table.empty<T.ReservedRouteId, M.ReservedRouteDoc>();
          }
    );

  /**
   `retailerTable`
   -----------------
   */

  public var retailerTable : M.RetailerTable =
    DocTable<T.RetailerId, M.RetailerDoc, T.RetailerInfo>(
      0,
      func(x:T.RetailerId):T.RetailerId{x+1},
      func(x:T.RetailerId,y:T.RetailerId):Bool{x==y},
      idHash,
      func(doc:M.RetailerDoc):T.RetailerInfo = {
        id=doc.id;
        public_key=doc.public_key;
        short_name=doc.short_name;
        description=doc.description;
        region=doc.region.id;
        reserved_routes=[];
        reserved_items=[];
      },
      func(info:T.RetailerInfo):?M.RetailerDoc =
        Option.map<M.RegionDoc, M.RetailerDoc>(
          func (regionDoc: M.RegionDoc): M.RetailerDoc = {
            id=info.id;
            public_key=info.public_key;
            short_name=info.short_name;
            description=info.description;
            region=regionDoc;
            reserved=Map.empty<T.ReservedInventoryId, (M.ReservedInventoryDoc, M.ReservedRouteDoc)>();
          },
          regionTable.getDoc(info.region),
        )
      );

  public var retailerQueryCount : Nat = 0;
  public var retailerQueryCost : Nat = 0;
  public var retailerJoinCount : Nat = 0;

  /**
   `routeTable`
   ----------------
   */

  public var routeTable : M.RouteTable =
    DocTable<T.RouteId, M.RouteDoc, T.RouteInfo> (
      0,
      func(x:T.RouteId):T.RouteId{x+1},
      func(x:T.RouteId,y:T.RouteId):Bool{x==y},
      idHash,
      func(doc:M.RouteDoc):T.RouteInfo = {
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
                 ?{
                     id=info.id;
                     transporter=info.transporter;
                     truck_type=truckType;
                     start_region=startRegion;
                     end_region=endRegion;
                     start_date=info.start_date;
                     end_date=info.end_date;
                     cost=info.cost;
                   }
               };
          case _ { null }
        }}
    );


  /**
   `reservedInventoryTable`
   ---------------------------
   */

  public var reservedInventoryTable : M.ReservedInventoryTable =
    DocTable<T.ReservedInventoryId, M.ReservedInventoryDoc, T.ReservedInventoryInfo>(
    0,
    func(x:T.ReservedInventoryId):T.ReservedInventoryId{x+1},
    func(x:T.ReservedInventoryId,y:T.ReservedInventoryId):Bool{x==y},
    idHash,
    func(doc:M.ReservedInventoryDoc):T.ReservedInventoryInfo = {
      id=doc.id;
      retailer=doc.retailer;
      item={
        id=doc.item.id;
        produce=doc.item.produce.id;
        producer=doc.item.producer;
        quantity=doc.item.quantity;
        weight=doc.item.weight;
        ppu=doc.item.ppu;
        start_date=doc.item.start_date;
        end_date=doc.item.end_date;
        comments=doc.item.comments;
      };
    },
    func(info:T.ReservedInventoryInfo):?M.ReservedInventoryDoc = {
      // validate the info's item id
      switch (inventoryTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?item_, ?_) {
               ?{
                   id=info.id;
                   item=item_:M.InventoryDoc;
                   retailer=info.retailer;
                 }
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

  public var reservedRouteTable : M.ReservedRouteTable =
    DocTable<T.ReservedRouteId, M.ReservedRouteDoc, T.ReservedRouteInfo>(
    0,
    func(x:T.ReservedRouteId):T.ReservedRouteId{x+1},
    func(x:T.ReservedRouteId,y:T.ReservedRouteId):Bool{x==y},
    idHash,
    func(doc:M.ReservedRouteDoc):T.ReservedRouteInfo = {
      id=doc.id;
      retailer=doc.retailer;
      route={
        id=doc.route.id;
        transporter=doc.route.transporter;
        truck_type={
          id=doc.route.truck_type.id;
          short_name=doc.route.truck_type.short_name;
          description=doc.route.truck_type.description;
          capacity=doc.route.truck_type.capacity;
          isFridge=doc.route.truck_type.isFridge;
          isFreezer=doc.route.truck_type.isFreezer;
        };
        start_region=doc.route.start_region.id;
        end_region=doc.route.end_region.id;
        start_date=doc.route.start_date;
        end_date=doc.route.end_date;
        cost=doc.route.cost;
      }
    },
    func(info:T.ReservedRouteInfo):?M.ReservedRouteDoc = {
      // validate the info's item id
      switch (routeTable.getDoc(info.id),
              retailerTable.getDoc(info.retailer)) {
        case (?route_, ?_) {
               ?{
                   id=info.id;
                   route=route_:M.RouteDoc;
                   retailer=info.retailer;
                 }
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

  public var usersByUserName
    : M.UserNameMap = Map.empty<T.UserName, T.UserId>();

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

  public var routesByDstSrcRegions : M.ByRegionPairRouteMap
    = Map.empty<T.RegionId, M.ByRegionRouteMap>();

  /**
   Inventory by region
   ----------------------------

   the actor maintains a possibly-sparse 3D table mapping each
   sourceregion-producerid-inventoryid triple to zero or one
   inventory items.  The 1D coordinate sourceregion gives all of the
   inventory items, by producer id, for this source region.

  */

  public var inventoryByRegion : M.ByRegionInventoryMap
    = Map.empty<T.RegionId, M.ByProducerInventoryMap>();

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
  public var reservationsByProduceByRegion
    : M.ByProduceByRegionInventoryReservationMap
    = Map.empty<T.ProduceId,
                Map<T.RegionId,
                    Map<T.ReservedInventoryId,
                        M.ReservedInventoryDoc>>>();

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
  public func addUser(
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
        {
          id=id_:T.ProducerId;
          public_key=public_key_;
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
        {
          id=id_:T.TransporterId;
          public_key=public_key_;
          short_name=user_name_;
          description=description_;
          routes=[];
          reserved=[];
        }
      }) } else null;

    /**- Create a retailer role for the user: */
    let rrId = if isRetailer { retailerTable.addInfoGetId(
      func(id_:T.RetailerId):T.RetailerInfo {
        {
          id=id_;
          public_key=public_key_;
          short_name=user_name_;
          description=description_;
          region=region_:T.RegionId;
        }
      }) } else null;

    /**- Record the user information: */
    let id = userTable.addInfoGetId(
      func (id_: T.UserId) : T.UserInfo =
        {
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

  /**

   `Produce`-oriented operations
   ==========================================

   */


  /**
   `produceMarketInfo`
   ---------------------------
   The last sales price for produce within a given geographic area; null region id means "all areas."
   */
  public func produceMarketInfo(produce_id:T.ProduceId, region_oid:?T.RegionId) : ?[T.ProduceMarketInfo] {
    // switch (Map.find<ProduceId,Map<RegionId,Map<ReservedInventoryId>>>(
    //           reservationsByProduceByRegion,
    //           produce_id, idIsEq)) {
    //   case null { return null };
    P.nyi()
  };

  /**

   `Producer`-facing operations
   ==========================================

   */


  /**
   // `producerAllInventoryInfo`
   // ---------------------------
   */
  public func producerAllInventoryInfo(id:T.UserId) : ?[T.InventoryInfo] {
    let doc = switch (producerTable.getDoc(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<T.InventoryId,M.InventoryDoc,T.InventoryInfo>(
      doc.inventory,
      func (_:T.InventoryId,doc:M.InventoryDoc):T.InventoryInfo =
        inventoryTable.getInfoOfDoc()(doc)
    )
  };

  /**
   `producerAddInventory`
   ---------------------------

  */
  public func producerAddInventory(
    iid_       : ?T.InventoryId,
    id_        : T.ProducerId,
    produce_id : T.ProduceId,
    quantity_  : T.Quantity,
    weight_    : T.Weight,
    ppu_       : T.Price,
    start_date_: T.Date,
    end_date_  : T.Date,
    comments_  : Text,
  ) : Result<T.InventoryId, T.IdErr>
  {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oproducer: ?M.ProducerDoc = producerTable.getDoc(id_);
    let oproduce  : ?M.ProduceDoc  = produceTable.getDoc(produce_id);
    let (producer_, produce_) = {
      switch (oproducer, oproduce) {
      case (?producer, ?produce) (producer, produce);
      case _ { return #err(#idErr null) };
      }};

    /**- Create the inventory item document: */
    let (_, item) = {
      switch (inventoryTable.addInfoAs(iid_,
                func(iid:T.InventoryId):T.InventoryInfo{
        {
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
      {
        id = producer_.id;
        public_key = producer_.public_key;
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
  public func producerUpdateInventory(
    iid_       : T.InventoryId,
    id_        : T.ProducerId,
    produce_id : T.ProduceId,
    quantity_  : T.Quantity,
    weight_    : T.Weight,
    ppu_       : T.Price,
    start_date_: T.Date,
    end_date_  : T.Date,
    comments_  : Text,
  ) : Result<(),T.IdErr>
  {
    /**- Validate these ids; fail here if anything is invalid: */
    let oproducer: ?M.ProducerDoc = producerTable.getDoc(id_);
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
               return (#err(#idErr null))
             }
           };
      case _ { return (#err(#idErr null)) };
      }};

    /**- remove the inventory item; given the validation above, this cannot fail. */
    Result.assertOk( producerRemInventory(iid_) );

    /**- add the (updated) inventory item; given the validation above, this cannot fail. */
    Result.assertOk(
      producerAddInventory(
        ?iid_, id_,
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
  public func producerRemInventory(id:T.InventoryId) : Result<(),T.ServerErr> {

    /**- validate the `id` */
    /// xxx macro for this pattern?
    let doc = switch (inventoryTable.getDoc(id)) {
      case null { return #err(#idErr null) };
      case (?doc) { doc };
    };

    /**- remove document from `producerTable`, in several steps: */
    let producer = Option.unwrap<M.ProducerDoc>(producerTable.getDoc(doc.producer));

    /**- remove document from `inventoryTable` */
    Option.assertSome(inventoryTable.rem( id ));

    /// xxx an abstraction to hide these type arguments?
    let (updatedInventory, _) =
      Trie.remove<T.InventoryId, M.InventoryDoc>(
        producer.inventory, keyOf(id), idIsEq);

    /// xxx syntax for functional record updates?
    let updatedProducer = {
      id          = producer.id ;
      public_key  = producer.public_key ;
      short_name  = producer.short_name ;
      description = producer.description ;
      region      = producer.region ;
      inventory   = updatedInventory ;
      reserved    = producer.reserved ;
    };

    Option.assertSome(
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
      Option.assertSome(d);
      t
    };

    #ok
  };

  /**
   `producerAllReservationInfo`
   ---------------------------

   */
  public func producerAllReservationInfo(id:T.ProducerId) : ?[T.ReservedInventoryInfo] {
    let doc = switch (producerTable.getDoc(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<T.ReservedInventoryId,
                 M.ReservedInventoryDoc,
                 T.ReservedInventoryInfo>(
      doc.reserved,
      func (_:T.ReservedInventoryId,
            doc:M.ReservedInventoryDoc):
        T.ReservedInventoryInfo
        =
        reservedInventoryTable.getInfoOfDoc()(doc)
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
  public func transporterAddRoute(
    rid_:            ?T.RouteId,
    id_:             T.TransporterId,
    start_region_id: T.RegionId,
    end_region_id:   T.RegionId,
    start_date_:     T.Date,
    end_date_:       T.Date,
    cost_:           T.Price,
    trucktype_id:    T.TruckTypeId
  ) : Result<T.RouteId,T.IdErr> {
    /** The model adds inventory and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let otransporter : ?M.TransporterDoc = transporterTable.getDoc(id_);
    let orstart      : ?M.RegionDoc  = regionTable.getDoc(start_region_id);
    let orend        : ?M.RegionDoc  = regionTable.getDoc(end_region_id);
    let otrucktype   : ?T.TruckTypeInfo  = truckTypeTable.getInfo(trucktype_id);
    let (transporter, start_region_, end_region_, truck_type_) = {
      switch (otransporter, orstart, orend, otrucktype) {
      case (?x1, ?x2, ?x3, ?x4) (x1, x2, x3, x4);
      case _ { return #err(#idErr null) };
      }};
    let transporterId = transporter.id;

    /**- Create the route item document: */
    let route : M.RouteDoc = {
      switch (routeTable.addInfoAs(rid_, func(routeId:T.RouteId):T.RouteInfo{
        {
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
      {
        id = transporter.id;
        public_key = transporter.public_key;
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
  public func transporterUpdateRoute(
    rid_            : T.RouteId,
    id_             : T.TransporterId,
    start_region_id : T.RegionId,
    end_region_id   : T.RegionId,
    start_date_     : T.Date,
    end_date_       : T.Date,
    cost_           : T.Price,
    trucktype_id    : T.TruckTypeId
  ) : Result<(),T.IdErr> {
    /** The model updates routes and maintains secondary indicies as follows: */

    /**- Validate these ids; fail fast if not defined: */
    let oroute       : ?M.RouteDoc   = routeTable.getDoc(rid_);
    let otransporter : ?M.TransporterDoc = transporterTable.getDoc(id_);
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
               return #err(#idErr null)
             }
           };
      case _ { return #err(#idErr null) };
      }};

    /**- remove the route; given the validation above, this cannot fail. */
    Result.assertOk( transporterRemRoute(rid_) );

    /**- add the (updated) route; given the validation above, this cannot fail. */
    Result.assertOk(
      transporterAddRoute(
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
  public func transporterRemRoute(id:T.RouteId) : Result<(),T.IdErr> {

    let doc = switch (routeTable.getDoc(id)) {
      case null { return #err(#idErr null) };
      case (?doc) { doc };
    };

    let transporter = Option.unwrap<M.TransporterDoc>(transporterTable.getDoc(doc.transporter));

    Option.assertSome(
      routeTable.rem( id )
    );

    let (updatedRoutes, _) =
      Trie.remove<T.RouteId, M.RouteDoc>(
        transporter.routes, keyOf(id), idIsEq);

    let updatedTransporter = {
      id          = transporter.id ;
      public_key  = transporter.public_key;
      short_name  = transporter.short_name ;
      description = transporter.description ;
      routes      = updatedRoutes ;
      reserved    = transporter.reserved ;
    };

    Option.assertSome(
      transporterTable.updateDoc( transporter.id, updatedTransporter )
    );

    routesByDstSrcRegions := {
      let (t, d) = Trie.remove3D<T.RegionId, T.RegionId, T.RouteId, M.RouteDoc>(
        routesByDstSrcRegions,
        keyOf(doc.end_region.id), idIsEq,
        keyOf(doc.start_region.id), idIsEq,
        keyOf(doc.id), idIsEq
      );
      Option.assertSome(d);
      t
    };

    #ok
  };

  /**
   `transporterAllRouteInfo`
   ---------------------------
   */
  public func transporterAllRouteInfo(id:T.TransporterId) : ?[T.RouteInfo] {
    let doc = switch (transporterTable.getDoc(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<T.RouteId,
                 M.RouteDoc,
                 T.RouteInfo>(
      doc.routes,
      func (_:T.RouteId,
            doc:M.RouteDoc):
        T.RouteInfo
        =
        routeTable.getInfoOfDoc()(doc)
    )
  };

  /**
   `transporterReservationInfo`
   ---------------------------

   */
  public func transporterAllReservationInfo(id:T.TransporterId) : ?[T.ReservedRouteInfo] {
    let doc = switch (transporterTable.getDoc(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<T.ReservedRouteId,
                 M.ReservedRouteDoc,
                 T.ReservedRouteInfo>(
      doc.reserved,
      func (_:T.ReservedRouteId,
            doc:M.ReservedRouteDoc):
        T.ReservedRouteInfo
        =
        reservedRouteTable.getInfoOfDoc()(doc)
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
  public func makeReservationInfo(item:M.InventoryDoc, route:M.RouteDoc) : T.ReservationInfo {
    {
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

      inventoryId = item.id  : T.InventoryId;
      routeId = route.id : T.RouteId;
    }
  };


  /**

  `isCompatibleTruckType`
  ----------------------

  Check whether the given truck type can accommodate the given produce type.

  */
  public func isCompatibleTruckType(tt:M.TruckTypeDoc, produce:M.ProduceDoc) : Bool {
    // todo
    true
  };

  /**

  `isFeasibleReservation`
  ----------------------
  Check whether the given retailer can reserve the given item and route pair.

  */

  public func isFeasibleReservation(
    retailer:M.RetailerDoc,
    item:M.InventoryDoc,
    route:M.RouteDoc,
    queryProduce:?T.ProduceId,
    queryDate:?T.Date)
    : Bool
    =
    label profile_isFeasibleReservation_begin : Bool
  {

    switch queryProduce {
      case null { };
      case (?qp) {
             if (item.produce.id != qp) {
               debugOff "nope: wrong produce kind\n";
               label profile_isFeasibleReservation_false_wrong_product_kind
               return false
             };
           };
    };
    switch queryDate {
      case null { };
      case (?qd) {
             if (route.end_date > qd ) {
               debugOff "nope: route arrives too late\n";
               label profile_isFeasibleReservation_false_arrives_too_late
               return false
             }
           }
    };
    /** - window start: check that the route begins after the inventory window begins */
    if (not (item.start_date <= route.start_date)) {
      debugOff "nope: item start after route start\n";
      label profile_isFeasibleReservation_false_item_start_ater_route_start
      return false
    };
    /** - window end: check that the route ends before the inventory window ends */
    if (not (route.end_date <= item.end_date)) {
      debugOff "nope: route ends after item ends\n";
      label profile_isFeasibleReservation_false_item_route_ends_after_item_ends
      return false
    };
    /** - check that truck can carry the given produce */
    if (not isCompatibleTruckType(route.truck_type, item.produce)) {
      debugOff "nope: truck is not compatible\n";
      label profile_isFeasibleReservation_false_truck_is_not_compatible
      return false
    };
    /** - all checks pass: */
    label profile_isFeasibleReservation_true : Bool
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
  public func retailerQueryAll(
    id:T.RetailerId,
    queryProduce:?T.ProduceId,
    queryDate:?T.Date
  ) : ?T.QueryAllResults =
    label profile_retailerQueryAll_begin : (?T.QueryAllResults)
  {
    retailerQueryCount += 1;

    /** - Find the retailer's document: */
    let retailer =
      switch (retailerTable.getDoc(id)) {
      case (null) { return null };
      case (?x) { x }};

    debugOff "- user_name: ";
    debugOff (retailer.short_name);
    debugOff "\n";

    /** - Temp: */
    debugOff "- retailer is located in region ";
    debugOffInt (retailer.region.id);
    debugOff ", and\n- is accessible via routes from ";

    /** - Find all routes whose the destination region is the retailer's region: */
    let retailerRoutes =
      switch (Trie.find<T.RegionId, M.ByRegionRouteMap>(
                routesByDstSrcRegions,
                keyOf(retailer.region.id),
                idIsEq
              )) {
      case (null) { return ?[] };
      case (?x) { x }};

    debugOffInt(Trie.count<T.RegionId, M.RouteMap>(retailerRoutes));
    debugOff " production regions.\n";

    /** - Join: For each production region, consider all routes and inventory: */
    let queryResults : Trie<T.RegionId, QueryResult> = {
      retailerJoinCount += 1;
      Trie.join<T.RegionId,
                M.RouteMap,
                M.ByProducerInventoryMap,
                QueryResult>(
        retailerRoutes,
        inventoryByRegion,
        idIsEq,
        func (routes:M.RouteMap,
              inventory:M.ByProducerInventoryMap) : QueryResult
          =
          label profile_retailerQueryAll_product_region : QueryResult
      {

        /** - Within this production region, consider every route-item pairing: */
        let product = Trie.Build.prodBuild
        <T.RouteId, M.RouteDoc,
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
            ?((T.RouteId, T.InventoryId),
              (M.RouteDoc, M.InventoryDoc))
            =
            label profile_retailerQueryAll_candidate_check :
            ( ?((T.RouteId, T.InventoryId),
                (M.RouteDoc, M.InventoryDoc)) )
        {
          retailerQueryCost += 1;
          /** - Consider the constraints of the retailer-route-item combination: */
          if (isFeasibleReservation(retailer, item, route, queryProduce, queryDate)) {
            label profile_retailerQueryAll_candidate_check_true :
              ( ?((T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)) )
            ?( (route_id, item_id), (route, item) )
          } else {
            label profile_retailerQueryAll_candidate_check_false :
              ( ?((T.RouteId, T.InventoryId),
                  (M.RouteDoc, M.InventoryDoc)) )

            null
          }
        },
        idPairIsEq
        );
        product
      }
      )};

    /** - The results are still organized by producer region; merge all such regions: */
    let queryResult : QueryResult =
      Trie.Build.projectInnerBuild
    <T.RegionId, (T.RouteId, T.InventoryId), (M.RouteDoc, M.InventoryDoc)>
    (queryResults);

    debugOff "- query result count: ";
    let size = Trie.Build.buildCount
    <(T.RouteId, T.InventoryId),
     (M.RouteDoc, M.InventoryDoc)>(queryResult);
    if ( size > retailerQuerySizeMax ) {
      retailerQuerySizeMax := size;
    };
    debugOffInt(size);
    debugOff " (count of feasible route-item pairs).\n";

    /** - Prepare reservation information for client, as an array; see also [`makeReservationInfo`](#makereservationinfo) */
    let arr =
      Trie.Build.buildToArray2
    <(T.RouteId, T.InventoryId),
     (M.RouteDoc, M.InventoryDoc),
     T.ReservationInfo>(
        queryResult,
        func (_:(T.RouteId,T.InventoryId), (r:M.RouteDoc, i:M.InventoryDoc))
          : T.ReservationInfo {
            makeReservationInfo(i, r)
          });

    ?arr
  };

  /**
   `retailerAllReservationInfo`
   ---------------------------

  */
  public func retailerAllReservationInfo(id:T.RetailerId) :
    ?[(T.ReservedInventoryInfo,
       T.ReservedRouteInfo)]
  {
    let doc = switch (retailerTable.getDoc(id)) {
      case null { return null };
      case (?doc) { doc };
    };

    ?Map.toArray<T.ReservedInventoryId,
                 (M.ReservedInventoryDoc,  M.ReservedRouteDoc),
                 (T.ReservedInventoryInfo, T.ReservedRouteInfo)>(
      doc.reserved,
      func (_:T.ReservedInventoryId,
            ((idoc:M.ReservedInventoryDoc),
             (rdoc:M.ReservedRouteDoc)))
            :
            ((T.ReservedInventoryInfo,
              T.ReservedRouteInfo))
        =
        ( (reservedInventoryTable.getInfoOfDoc()(idoc),
           reservedRouteTable.getInfoOfDoc()(rdoc)) )
    )
  };


  /**
   `retailerReserve`
   ---------------------------

   ### Ids before/after this operation:

   after this operation, the reserved documents have Ids that are stale/dangling:
   These old Ids are no longer valid in the master tables; instead, their owners,
   the created reservation documents, each have a valid id, which this operation returns.

  */
  public func retailerReserve(
    retailer_id:T.RetailerId,
    inventory_id:T.InventoryId,
    route_id:T.RouteId) : Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>
  {
    /** ### validate Ids */

    // xxx -- AS wishlist: macros that expand into case-null-return-#err forms, like below:

    let retailerDocOp = retailerTable.getDoc(retailer_id);
    let retailerDoc = switch retailerDocOp {
    case null return #err(#idErr(?#retailer(retailer_id)));
    case (?x) x};

    let routeDocOp = routeTable.getDoc(route_id);
    let routeDoc = switch routeDocOp {
    case null return #err(#idErr(?#route(route_id))); case (?x) x};

    let transporterDocOp = transporterTable.getDoc(routeDoc.transporter);
    let transporterDoc = switch transporterDocOp {
    // internal error, if any:
    case null return #err(#idErr null); case (?x) x};

    let inventoryDocOp = inventoryTable.getDoc(inventory_id);
    let inventoryDoc = switch inventoryDocOp {
    case null return #err(#idErr(?#inventory(inventory_id)));
    case (?x) x};

    let producerDocOp = producerTable.getDoc(inventoryDoc.producer);
    let producerDoc = switch producerDocOp {
    // internal error, if any:
    case null return #err(#idErr null); case (?x) x};

    /**if no errors in any results above, then continue: */

    /**
     ### remove resources

     in preparation for _moving_ these resources, begin by removing
     these reserved resources from the inventory and route tables, as well
     as from all secondary mappings.  We reuse existing Model operations
     for these removals.

     */

    Result.assertOk(producerRemInventory(inventory_id));
    Result.assertOk(transporterRemRoute(route_id));

    /**
     ## refresh documents

     critically, after we do the remove operations above, we need to
     "re-fetch" the documents that we've affected before using them again below in subsequent updates.
     */

    let transporterDoc2 = Option.unwrap<M.TransporterDoc>(transporterTable.getDoc(transporterDoc.id));
    let producerDoc2 = Option.unwrap<M.ProducerDoc>(producerTable.getDoc(producerDoc.id));


    /**
     ### create reservation documents

     These new documents will "own" the removed documents for the
     reserved resources.

     Note: these owned documents have Ids that are stale/dangling.
     They are no longer valid in the master tables.  Instead, their owners,
     reservation documents, have valid ids.

     */

    /**- move route document into reserved route table, and mint an ID. */
    let (_, reservedRouteDoc) = reservedRouteTable.addDoc(
      func (rrid:T.ReservedRouteId):M.ReservedRouteDoc {
        {
          id=rrid;
          retailer=retailerDoc.id;
          route=routeDoc;
        }
      }
    );

    /**- move inventory document into reserved inventory table, and mint an ID. */
    let (_, reservedInventoryDoc) = reservedInventoryTable.addDoc(
      func (rrid:T.ReservedInventoryId):M.ReservedInventoryDoc {
        {
          id=rrid;
          retailer=retailerDoc.id;
          item=inventoryDoc;
        }
      }
    );

    /**
     ### Update secondary mappings

     add the newly-created reservation documents to reservation
     collections of producers, transporters and retailers documents:

     */
    {
      /**- Update the producer's reserved inventory: */

      let (updatedProducerReserved,_) =
        Map.insert<T.ReservedInventoryId,M.ReservedInventoryDoc>(
          producerDoc2.reserved,
          keyOf(reservedInventoryDoc.id), idIsEq,
          reservedInventoryDoc);

      // xxx -- AS wishlist: better syntax for functional record update:
      let updatedProducer = {
        id=producerDoc2.id;
        public_key=producerDoc2.public_key;
        short_name=producerDoc2.short_name;
        description=producerDoc2.description;
        region=producerDoc2.region;
        inventory=producerDoc2.inventory;
        reserved=updatedProducerReserved; // <-- the only field we are updating
      };

      Option.assertSome(
        producerTable.updateDoc( producerDoc2.id, updatedProducer )
      )
    };
    {
      /**- Update the transporter's reserved routes: */

      let (updatedTransporterReserved,_) =
        Map.insert<T.ReservedRouteId,M.ReservedRouteDoc>(
          transporterDoc2.reserved,
          keyOf(reservedRouteDoc.id), idIsEq,
          reservedRouteDoc);

      // xxx -- AS wishlist: better syntax for functional record update:
      let updatedTransporter = {
        id=transporterDoc2.id;
        public_key=transporterDoc2.public_key;
        short_name=transporterDoc2.short_name;
        description=transporterDoc2.description;
        routes=transporterDoc2.routes;
        reserved=updatedTransporterReserved; // <-- the only field we are updating
      };

      Option.assertSome(
        transporterTable.updateDoc( transporterDoc2.id, updatedTransporter )
      )
    };
    {
      /**- Update the retailer's reserved routes and inventory: */

      let (updatedRetailerReserved,_) =
        Map.insert<T.ReservedInventoryId, (M.ReservedInventoryDoc, M.ReservedRouteDoc)>(
          retailerDoc.reserved,
          keyOf(reservedInventoryDoc.id), idIsEq,
          (reservedInventoryDoc, reservedRouteDoc));

      // xxx -- AS wishlist: better syntax for functional record update:
      let updatedRetailer = {
        id=retailerDoc.id;
        public_key=retailerDoc.public_key;
        short_name=retailerDoc.short_name;
        description=retailerDoc.description;
        region=retailerDoc.region;
        reserved=updatedRetailerReserved; // <-- the only field we are updating
      };

      Option.assertSome(
        retailerTable.updateDoc( retailerDoc.id, updatedRetailer )
      )
    };

    return #ok (reservedRouteDoc.id, reservedInventoryDoc.id)
  };

  /**
   `retailerReserveMany`
   ---------------------------
  */
  public func retailerReserveMany(
    id:T.RetailerId,
    array:[(T.InventoryId,T.RouteId)])
    : [Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>]
  {
    let a = Array.init<?(Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>)>(
      array.len(),
      null
    );
    for (i in array.keys()) {
      let (item, route) = array[i];
      let x = retailerReserve(id, item, route);
      a[i] := ?x;
    };
    let results =
      Array.tabulate<Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>>(
        array.len(),
        func(i:Nat):Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>{
          Option.unwrap<Result<(T.ReservedRouteId, T.ReservedInventoryId), T.IdErr>>(a[i])
        });
    results
  };

};
}
