//let T = (import "../src/serverTypes");
//let A = (import "../src/serverActor");
//let Result = (import "mo:stdlib/Result");
//let Option = (import "mo:stdlib/Option");

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
        //scaledParams(2, 2);     // (40,    40   ), loads in 0.7s in my AS interpreter
        //scaledParams(5, 5);   // (625  , 625  ), loads in 8.8s in my AS interpreter
        scaledParams(8, 10);  // (3_200, 3_200), loads in 1:07min in my AS interpreter
        //scaledParams(10, 10); // (5_000, 5_000), loads in 2:13min in my AS interpreter
      };

      /////////////////////////////////////////////////////////////////////////////////////
      let s = server;

      debugPrint "\nExchange setup: Begin...\n====================================\n";
      await s.loadWorkload(params);

      if false {
        debugPrint "\nRetailer queries\n====================================\n";
        // do a query for each retailer:
        for (r in range(0, params.retailer_count)) {
          await retailerQueryAll("", r)
        };
      };

      debugPrint "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      debugPrint "\nDump all\n====================================\n";
      debugPrint (debug_show (await s.getCounts()));
      if false {
        await debugDumpAll()
      }
    })
  };
};


//func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {
func retailerQueryAll(pk:Text, retailerId:T.RetailerId) : async () {

  debugPrint "\nRetailer ";
  debugPrintInt retailerId;
  debugPrint " sends `retailerQueryAll`\n";
  debugPrint "------------------------------------\n";

  debugPrint "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  debugPrint "\n## Query end.";

  debugPrint "\n## Query results (";
  debugPrintInt (res.len());
  debugPrint ")\n";
  for (info in res.vals()) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  }
};

//func debugDumpAll(server:A.Server) : async () {
func debugDumpAll() : async () {

  debugPrint "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  debugPrint "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  debugPrint "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  debugPrint "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  debugPrint "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  debugPrint "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    debugPrint "- ";
    debugPrint (debug_show info);
    debugPrint "\n";
  };

  { debugPrint "\nInventory info\n----------------\n";
    let items = await server.allInventoryInfo();
    debugPrint "total count of ";
    debugPrintInt (items.len());
    debugPrint ":\n";
    for ( info in items.vals() ) {
      debugPrint "- ";
      debugPrint (debug_show info);
      debugPrint "\n";
    };
  };

  { debugPrint "\nRoute info\n----------------\n";
    let routes = await server.allRouteInfo();
    debugPrint "total count of ";
    debugPrintInt (routes.len());
    debugPrint ":\n";
    for ( info in (routes.vals()) ) {
      debugPrint "- ";
      debugPrint (debug_show info);
      debugPrint "\n";
    };
  };
};

func printEntityCount(entname:Text, count:Nat) {
  debugPrint ("- " # entname # " count: ");
  debugPrintInt count;
  debugPrint "\n";
};

func printLabeledCost(lab:Text, cost:Nat) {
  debugPrint ("- " # lab # " cost: ");
  debugPrintInt cost;
  debugPrint "\n";
};

let test = Test();
test.go()
