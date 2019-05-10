let T = (import "../serverTypes.as");
//let A = (import "../serverActor.as");
let Result = (import "../../../result.as");
let Option = (import "../../../option.as");

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
      // Vary the choice of region count and scaling factor here:
      let params = {
        scaledParams(2, 2);     // (40,    40   ), loads in 0.7s in my AS intepreter
        //scaledParams(5, 5);   // (625  , 625  ), loads in 8.8s in my AS interpreter
        //scaledParams(8, 10);  // (3_200, 3_200), loads in 1:07min in my AS interpreter
        //scaledParams(10, 10); // (5_000, 5_000), loads in 2:13min in my AS interpreter
      };

      /////////////////////////////////////////////////////////////////////////////////////
      let s = server;

      print "\nExchange setup: Begin...\n====================================\n";
      await s.loadWorkload(params);

      if false {
        print "\nRetailer queries\n====================================\n";
        // do a query for each retailer:
        for (r in range(0, params.retailer_count)) {
          await retailerQueryAll("", r)
        };
      };

      print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      print "\nDump all\n====================================\n";
      print (debug_show (await s.getCounts()));
      if false {
        await debugDumpAll()
      }
    })
  };
};


//func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {
func retailerQueryAll(pk:Text, retailerId:T.RetailerId) : async () {

  print "\nRetailer ";
  printInt retailerId;
  print " sends `retailerQueryAll`\n";
  print "------------------------------------\n";

  print "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  print "\n## Query end.";

  print "\n## Query results (";
  printInt (res.len());
  print ")\n";
  for (info in res.vals()) {
    print "- ";
    print (debug_show info);
    print "\n";
  }
};

//func debugDumpAll(server:A.Server) : async () {
func debugDumpAll() : async () {

  print "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  { print "\nInventory info\n----------------\n";
    let items = await server.allInventoryInfo();
    print "total count of ";
    printInt (items.len());
    print ":\n";
    for ( info in items.vals() ) {
      print "- ";
      print (debug_show info);
      print "\n";
    };
  };

  { print "\nRoute info\n----------------\n";
    let routes = await server.allRouteInfo();
    print "total count of ";
    printInt (routes.len());
    print ":\n";
    for ( info in (routes.vals()) ) {
      print "- ";
      print (debug_show info);
      print "\n";
    };
  };
};

func printEntityCount(entname:Text, count:Nat) {
  print ("- " # entname # " count: ");
  printInt count;
  print "\n";
};

func printLabeledCost(lab:Text, cost:Nat) {
  print ("- " # lab # " cost: ");
  printInt cost;
  print "\n";
};

let test = Test();
test.go()
