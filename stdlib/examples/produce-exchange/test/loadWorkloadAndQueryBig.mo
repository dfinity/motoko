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

      debug_print "\nExchange setup: Begin...\n====================================\n";
      await s.loadWorkload(params);

      if false {
        debug_print "\nRetailer queries\n====================================\n";
        // do a query for each retailer:
        for (r in range(0, params.retailer_count)) {
          await retailerQueryAll("", r)
        };
      };

      debug_print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      debug_print "\nDump all\n====================================\n";
      debug_print (debug_show (await s.getCounts()));
      if false {
        await debugDumpAll()
      }
    })
  };
};


//func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {
func retailerQueryAll(pk:Text, retailerId:T.RetailerId) : async () {

  debug_print "\nRetailer ";
  debug_print_Int retailerId;
  debug_print " sends `retailerQueryAll`\n";
  debug_print "------------------------------------\n";

  debug_print "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  debug_print "\n## Query end.";

  debug_print "\n## Query results (";
  debug_print_Int (res.len());
  debug_print ")\n";
  for (info in res.vals()) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  }
};

//func debugDumpAll(server:A.Server) : async () {
func debugDumpAll() : async () {

  debug_print "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  debug_print "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  debug_print "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  debug_print "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  debug_print "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  debug_print "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    debug_print "- ";
    debug_print (debug_show info);
    debug_print "\n";
  };

  { debug_print "\nInventory info\n----------------\n";
    let items = await server.allInventoryInfo();
    debug_print "total count of ";
    debug_print_Int (items.len());
    debug_print ":\n";
    for ( info in items.vals() ) {
      debug_print "- ";
      debug_print (debug_show info);
      debug_print "\n";
    };
  };

  { debug_print "\nRoute info\n----------------\n";
    let routes = await server.allRouteInfo();
    debug_print "total count of ";
    debug_print_Int (routes.len());
    debug_print ":\n";
    for ( info in (routes.vals()) ) {
      debug_print "- ";
      debug_print (debug_show info);
      debug_print "\n";
    };
  };
};

func printEntityCount(entname:Text, count:Nat) {
  debug_print ("- " # entname # " count: ");
  debug_print_Int count;
  debug_print "\n";
};

func printLabeledCost(lab:Text, cost:Nat) {
  debug_print ("- " # lab # " cost: ");
  debug_print_Int cost;
  debug_print "\n";
};

let test = Test();
test.go()
