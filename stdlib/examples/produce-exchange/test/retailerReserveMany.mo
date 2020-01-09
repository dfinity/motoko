import Prim "mo:prim";
import T = "../serverTypes.mo";
import A = "../serverActor.mo";
import Result = "../../../result.mo";
import Option = "../../../option.mo";

func printEntityCount(entname:Text, count:Nat) {
  Prim.debugPrint ("- " # entname # " count: ");
  Prim.debugPrintInt count;
  Prim.debugPrint "\n";
};

func printLabeledCost(lab:Text, cost:Nat) {
  Prim.debugPrint ("- " # lab # " cost: ");
  Prim.debugPrintInt cost;
  Prim.debugPrint "\n";
};

actor class Test() = this {
  public func go() {
    ignore(async
    {
      let s = A.Server();

      Prim.debugPrint "\nExchange setup: Begin...\n====================================\n";

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

      Prim.debugPrint "\nExchange setup: Done.\n====================================\n";

      let inventoryCount1 = await debugDumpInventory(s, pka, 0);
      let routeCount1 = await debugDumpRoutes(s, pka, 0);
      await debugDumpAll(s);

      //////////////////////////////////////////////////////////////////

      Prim.debugPrint "\nRetailer queries\n====================================\n";

      // do some queries
      await retailerQueryAll(s, pka, ? Result.assertUnwrapAny<T.UserId>(uida));
      await retailerQueryAll(s, pkb, ? Result.assertUnwrapAny<T.UserId>(uidb));
      await retailerQueryAll(s, pkc, ? Result.assertUnwrapAny<T.UserId>(uidc));
      await retailerQueryAll(s, pkd, ? Result.assertUnwrapAny<T.UserId>(uidd));
      await retailerQueryAll(s, pke, ? Result.assertUnwrapAny<T.UserId>(uide));

      Prim.debugPrint "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      Prim.debugPrint "\nRetailer reservations\n====================================\n";


      Prim.debugPrint "\nRetailer reservations: begin...\n------------------------\n";

      let rrm =
        await s.retailerReserveMany(pka,
                                    Result.assertUnwrapAny<T.UserId>(uida),
                                    [(0, 0),
                                     (0, 1)]);


      let urrm = Result.assertUnwrapAny<[Result.Result<(T.ReservedInventoryId, T.ReservedRouteId), T.ServerErr>]>(rrm);

      Prim.debugPrint "\nRetailer reservations: results:\n---------------------------------\n";

      for (i in urrm.keys()) {
        Prim.debugPrintInt i;
        Prim.debugPrint ". ";
        Prim.debugPrint (debug_show urrm[i]);
        Prim.debugPrint "\n";
      };

      Prim.debugPrint "\nRetailer reservations: results[0]: expect success.\n---------------------------------\n";

      let (ri0, rr0) = Result.assertUnwrapAny<(T.ReservedInventoryId, T.ReservedRouteId)>(urrm[0]);

      Prim.debugPrint "- ";
      Prim.debugPrint (debug_show (ri0, rr0));
      Prim.debugPrint "\n";

      Prim.debugPrint "\nRetailer reservations: results[1]: expect error: already reserved by us!\n---------------------------------\n";

      Result.assertErrAs<T.ServerErr, ()>
      (urrm[1],
       func (err:T.ServerErr):(()) {
         switch err {
         case (#idErr entid) {
                switch entid {
                case (?(#inventory 0)) Prim.debugPrint "- error is `#idErr(?(#inventory 0))`\n";
                case _ assert false;
                }
              };
         case _ assert false;
         }
       });

      Prim.debugPrint "\nRetailer reservations: done.\n---------------------------------\n";

      Prim.debugPrint "\nExchange interactions: Done.\n====================================\n";

      let inventoryCount2 = await debugDumpInventory(s, pka, 0);
      let routeCount2 = await debugDumpRoutes(s, pka, 0);
      assert (inventoryCount2 == 0);
      assert (routeCount2 == 1);

      await debugDumpAll(s);
    })
  };
};

func debugDumpInventory(server:A.Server, pk:T.PublicKey, p:T.ProducerId) : async Nat {
  Prim.debugPrint "\nProducer ";
  Prim.debugPrintInt p;
  Prim.debugPrint "'s inventory:\n--------------------------------\n";
  let res = await server.producerAllInventoryInfo(pk, p);
  let items = Result.assertUnwrapAny<[T.InventoryInfo]>(res);
  for (i in items.keys()) {
    Prim.debugPrintInt i;
    Prim.debugPrint ". ";
    Prim.debugPrint (debug_show (items[i]));
    Prim.debugPrint "\n";
  };
  Prim.debugPrint "(list end)\n";
  items.len()
};

func debugDumpRoutes(server:A.Server, pk:T.PublicKey, t:T.TransporterId) : async Nat {
  Prim.debugPrint "\nTransporter ";
  Prim.debugPrintInt t;
  Prim.debugPrint "'s routes:\n--------------------------------\n";
  let res = await server.transporterAllRouteInfo(pk, t);
  let items = Result.assertUnwrapAny<[T.RouteInfo]>(res);
  for (i in items.keys()) {
    Prim.debugPrintInt i;
    Prim.debugPrint ". ";
    Prim.debugPrint (debug_show (items[i]));
    Prim.debugPrint "\n";
  };
  Prim.debugPrint "(list end)\n";
  items.len()
};

func retailerQueryAll(server:A.Server, pk:Text, r:?T.UserId) : async () {

  Prim.debugPrint "\nRetailer ";
  let retailerId: T.UserId = Option.unwrap<T.UserId>(r);
  Prim.debugPrintInt retailerId;
  Prim.debugPrint " sends `retailerQueryAll`\n";
  Prim.debugPrint "------------------------------------\n";

  Prim.debugPrint "\n## Query begin:\n";
  let res = Result.assertUnwrapAny<T.QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId, null, null)
  );
  Prim.debugPrint "\n## Query end.";

  Prim.debugPrint "\n## Query results (";
  Prim.debugPrintInt (res.len());
  Prim.debugPrint ")\n";
  for (info in res.vals()) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  }
};

func debugDumpAll(server:A.Server) : async () {

  Prim.debugPrint "\nTruck type info\n----------------\n";
  for ( info in ((await server.allTruckTypeInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nRegion info\n----------------\n";
  for ( info in ((await server.allRegionInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nProduce info\n----------------\n";
  for ( info in ((await server.allProduceInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nProducer info\n----------------\n";
  for ( info in ((await server.allProducerInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nTransporter info\n----------------\n";
  for ( info in ((await server.allTransporterInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nRetailer info\n----------------\n";
  for ( info in ((await server.allRetailerInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nInventory info\n----------------\n";
  for ( info in ((await server.allInventoryInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };

  Prim.debugPrint "\nRoute info\n----------------\n";
  for ( info in ((await server.allRouteInfo()).vals()) ) {
    Prim.debugPrint "- ";
    Prim.debugPrint (debug_show info);
    Prim.debugPrint "\n";
  };
};

let test = Test();
test.go()
