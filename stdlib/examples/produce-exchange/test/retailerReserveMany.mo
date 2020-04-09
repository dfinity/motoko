import Debug "mo:stdlib/Debug";
import T = "../src/serverTypes";
import A = "../src/serverActor";
import Result = "mo:stdlib/Result";
import Option = "mo:stdlib/Option";

func printEntityCount(entname:Text, count:Nat) {
  Debug.print ("- " # entname # " count: " # debug_show count # "\n");
};

func printLabeledCost(lab:Text, cost:Nat) {
  Debug.print ("- " # lab # " cost: " # debug_show cost # "\n");
};


actor class Test() = this {
  public func go() {
    ignore(async
    {
      let s = A.Server();

      Debug.print "\nExchange setup: Begin...\n====================================\n";

      let pka = "beef";
      let pkb = "dead";
      let pkc = "4242";
      let pkd = "1234";
      let pke = "f00d";

      // populate with truck types
      let tta = await s.registrarAddTruckType("tta", "", 10, false, false);
      let ttb = await s.registrarAddTruckType("ttb", "", 20, false, false);
      let ttc = await s.registrarAddTruckType("ttc", "", 10, true, false);
      let ttd = await s.registrarAddTruckType("ttd", "", 30, true, false);
      let tte = await s.registrarAddTruckType("tte", "", 50, false, true);

      printEntityCount("Truck type", (await s.getCounts()).truck_type_count);

      // populate with regions
      let rega = await s.registrarAddRegion("rega", "");
      let regb = await s.registrarAddRegion("regb", "");
      let regc = await s.registrarAddRegion("regc", "");
      let regd = await s.registrarAddRegion("regd", "");
      let rege = await s.registrarAddRegion("rege", "");

      printEntityCount("Region", (await s.getCounts()).region_count);

      // populate with produce
      let pea = await s.registrarAddProduce("avocado1", "avocado", 1);
      let peb = await s.registrarAddProduce("avocado2", "avocado avocado", 2);
      let pec = await s.registrarAddProduce("avocado3", "avocado avocado avocado", 3);
      let ped = await s.registrarAddProduce("avocado4", "avocado avocado avocado avocado", 4);
      let pee = await s.registrarAddProduce("avocado5", "avocado avocado avocado avocado avocado", 5);

      printEntityCount("Produce", (await s.getCounts()).produce_count);

      // register all users
      let uida = await s.registrarAddUser(pka, "usera", "", Result.assertUnwrapAny<T.RegionId>rega, true, true, true, true);
      let uidb = await s.registrarAddUser(pkb, "userb", "", Result.assertUnwrapAny<T.RegionId>regb, true, true, true, true);
      let uidc = await s.registrarAddUser(pkc, "userc", "", Result.assertUnwrapAny<T.RegionId>regc, true, true, true, true);
      let uidd = await s.registrarAddUser(pkd, "userd", "", Result.assertUnwrapAny<T.RegionId>regd, true, true, true, true);
      let uide = await s.registrarAddUser(pke, "usere", "", Result.assertUnwrapAny<T.RegionId>rege, true, true, true, true);

      printEntityCount("Producer", (await s.getCounts()).producer_count);
      printEntityCount("Transporter", (await s.getCounts()).transporter_count);
      printEntityCount("Retailer", (await s.getCounts()).retailer_count);

      // populate with inventory
      let praia = await s.producerAddInventory(
        pka,
        Result.assertUnwrapAny<T.UserId>(uida),
        Result.assertUnwrapAny<T.ProduceId>(pea), 100, 100, 10, 0, 110, ""
      );

      printEntityCount("Inventory@time1", (await s.getCounts()).inventory_count);

      let rta_a_c_tta = await s.transporterAddRoute(
        pka,
        Result.assertUnwrapAny<T.UserId>(uida),
        Result.assertUnwrapAny<T.RegionId>(rega),
        Result.assertUnwrapAny<T.RegionId>(regc),
        0, 20, 100,
        Result.assertUnwrapAny<T.TruckTypeId>(tta)
      );
      let rta_b_c_ttb = await s.transporterAddRoute(
        pka,
        Result.assertUnwrapAny<T.UserId>(uida),
        Result.assertUnwrapAny<T.RegionId>(regb),
        Result.assertUnwrapAny<T.RegionId>(regc),
        0, 20, 100,
        Result.assertUnwrapAny<T.TruckTypeId>(ttb)
      );

      printEntityCount("Route@time1", (await s.getCounts()).route_count);

      //////////////////////////////////////////////////////////////////

      Debug.print "\nExchange setup: Done.\n====================================\n";

      let inventoryCount1 = await debugDumpInventory(s, pka, 0);
      let routeCount1 = await debugDumpRoutes(s, pka, 0);
      await debugDumpAll(s);

      //////////////////////////////////////////////////////////////////

      Debug.print "\nRetailer queries\n====================================\n";

      // do some queries
      await retailerQueryAll(s, pka, ? Result.assertUnwrapAny<T.UserId>(uida));
      await retailerQueryAll(s, pkb, ? Result.assertUnwrapAny<T.UserId>(uidb));
      await retailerQueryAll(s, pkc, ? Result.assertUnwrapAny<T.UserId>(uidc));
      await retailerQueryAll(s, pkd, ? Result.assertUnwrapAny<T.UserId>(uidd));
      await retailerQueryAll(s, pke, ? Result.assertUnwrapAny<T.UserId>(uide));

      Debug.print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      Debug.print "\nRetailer reservations\n====================================\n";


      Debug.print "\nRetailer reservations: begin...\n------------------------\n";

      let rrm =
        await s.retailerReserveMany(pka,
                                    Result.assertUnwrapAny<T.UserId>(uida),
                                    [(0, 0),
                                     (0, 1)]);


      let urrm = Result.assertUnwrapAny<[Result.Result<(T.ReservedInventoryId, T.ReservedRouteId), T.ServerErr>]>(rrm);

      Debug.print "\nRetailer reservations: results:\n---------------------------------\n";

      for (i in urrm.keys()) {
        Debug.print (debug_show i # ". " # debug_show urrm[i] # "\n");
      };

      Debug.print "\nRetailer reservations: results[0]: expect success.\n---------------------------------\n";

      let (ri0, rr0) = Result.assertUnwrapAny<(T.ReservedInventoryId, T.ReservedRouteId)>(urrm[0]);

      Debug.print "- ";
      Debug.print (debug_show (ri0, rr0));
      Debug.print "\n";

      Debug.print "\nRetailer reservations: results[1]: expect error: already reserved by us!\n---------------------------------\n";

      Result.assertErrAs<T.ServerErr, ()>
      (urrm[1],
       func (err:T.ServerErr):(()) {
         switch err {
         case (#idErr entid) {
                switch entid {
                case (?(#inventory 0)) Debug.print "- error is `#idErr(?(#inventory 0))`\n";
                case _ assert false;
                }
              };
         case _ assert false;
         }
       });

      Debug.print "\nRetailer reservations: done.\n---------------------------------\n";

      Debug.print "\nExchange interactions: Done.\n====================================\n";

      let inventoryCount2 = await debugDumpInventory(s, pka, 0);
      let routeCount2 = await debugDumpRoutes(s, pka, 0);
      assert (inventoryCount2 == 0);
      assert (routeCount2 == 1);

      await debugDumpAll(s);
    })
  };
};

func debugDumpInventory(server:A.Server, pk:T.PublicKey, p:T.ProducerId) : async Nat {
  Debug.print ("\nProducer " # debug_show p # "'s inventory:\n--------------------------------\n");
  let res = await server.producerAllInventoryInfo(pk, p);
  let items = Result.assertUnwrapAny<[T.InventoryInfo]>(res);
  for (i in items.keys()) {
    Debug.print (debug_show i # ". " # debug_show items[i] # "\n");
  };
  Debug.print "(list end)\n";
  items.len()
};

func debugDumpRoutes(server:A.Server, pk:T.PublicKey, t:T.TransporterId) : async Nat {
  Debug.print ("\nTransporter " # debug_show t # "'s routes:\n--------------------------------\n");
  let res = await server.transporterAllRouteInfo(pk, t);
  let items = Result.assertUnwrapAny<[T.RouteInfo]>(res);
  for (i in items.keys()) {
    Debug.print (debug_show i # ". " # debug_show items[i] # "\n");
  };
  Debug.print "(list end)\n";
  items.len()
};

func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {
  let retailerId: T.UserId = Option.unwrap<T.UserId>(r);
  Debug.print ("\nRetailer " # debug_show retailerId # " sends `retailerQueryAll`\n");

  Debug.print "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  Debug.print "\n## Query end.";

  Debug.print ("\n## Query results (" # debug_show res.len() # ")\n");
  for (info in res.vals()) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  }
};

func debugDumpAll(server:A.Server) : async () {

  Debug.print "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nInventory info\n----------------\n";
  for ( info in ((await server.allInventoryInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };

  Debug.print "\nRoute info\n----------------\n";
  for ( info in ((await server.allRouteInfo()).vals()) ) {
    Debug.print "- ";
    Debug.print (debug_show info);
    Debug.print "\n";
  };
};

let test = Test();
test.go()
