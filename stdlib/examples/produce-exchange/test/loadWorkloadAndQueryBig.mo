//let T = (import "../serverTypes.mo");
//let A = (import "../serverActor.mo");
//let Result = (import "../../../result.mo");
//let Option = (import "../../../option.mo");

func scaledParams(region_count_:Nat, factor:Nat) : T.WorkloadParams = shared {
  region_count        = region_count_:Nat;
  day_count           = 3:Nat;
  max_route_duration  = 1:Nat;
  producer_count      = region_count * factor:Nat;
  transporter_count   = region_count * factor:Nat;
  retailer_count      = region_count * factor:Nat;
};

/////////////////////////////////////////////////////////////////////////////////////

actor class Test() = this {
  go() {
    ignore(async
    {
      // Vary the choice of region count and scaling factor here;
      // Each choice leads to a different count of (InventoryCount, RouteCount), and load time:
      let params = {
        //scaledParams(2, 2);     // (40,    40   ), loads in 0.7s in my AS intepreter
        //scaledParams(5, 5);   // (625  , 625  ), loads in 8.8s in my AS interpreter
        scaledParams(8, 10);  // (3_200, 3_200), loads in 1:07min in my AS interpreter
        //scaledParams(10, 10); // (5_000, 5_000), loads in 2:13min in my AS interpreter
      };

      /////////////////////////////////////////////////////////////////////////////////////
      let s = server;

      Debug.print "\nExchange setup: Begin...\n====================================\n";
      await s.loadWorkload(params);

      if false {
        Debug.print "\nRetailer queries\n====================================\n";
        // do a query for each retailer:
        for (r in range(0, params.retailer_count)) {
          await retailerQueryAll("", r)
        };
      };

      Debug.print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      Debug.print "\nDump all\n====================================\n";
      Debug.print (Debug.show (await s.getCounts()));
      if false {
        await debugDumpAll()
      }
    })
  };
};


//func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {
func retailerQueryAll(pk:Text, retailerId:T.RetailerId) : async () {

  Debug.print "\nRetailer ";
  Debug.printInt retailerId;
  Debug.print " sends `retailerQueryAll`\n";
  Debug.print "------------------------------------\n";

  Debug.print "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  Debug.print "\n## Query end.";

  Debug.print "\n## Query results (";
  Debug.printInt (res.len());
  Debug.print ")\n";
  for (info in res.vals()) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  }
};

//func debugDumpAll(server:A.Server) : async () {
func debugDumpAll() : async () {

  Debug.print "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  Debug.print "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  Debug.print "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  Debug.print "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  Debug.print "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  Debug.print "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (Debug.show info);
    Debug.print "\n";
  };

  { Debug.print "\nInventory info\n----------------\n";
    let items = await server.allInventoryInfo();
    Debug.print "total count of ";
    Debug.printInt (items.len());
    Debug.print ":\n";
    for ( info in items.vals() ) {
      Debug.print "- ";
      Debug.print (Debug.show info);
      Debug.print "\n";
    };
  };

  { Debug.print "\nRoute info\n----------------\n";
    let routes = await server.allRouteInfo();
    Debug.print "total count of ";
    Debug.printInt (routes.len());
    Debug.print ":\n";
    for ( info in (routes.vals()) ) {
      Debug.print "- ";
      Debug.print (Debug.show info);
      Debug.print "\n";
    };
  };
};

func printEntityCount(entname:Text, count:Nat) {
  Debug.print ("- " # entname # " count: ");
  Debug.printInt count;
  Debug.print "\n";
};

func printLabeledCost(lab:Text, cost:Nat) {
  Debug.print ("- " # lab # " cost: ");
  Debug.printInt cost;
  Debug.print "\n";
};

let test = Test();
test.go()
