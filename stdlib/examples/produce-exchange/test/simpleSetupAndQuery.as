import "../../../prelude.as";
import "../serverActor.as";

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

actor class Test() = this {
  go() {
    ignore(async
    {
      let s = server; //Server();

      print "\nExchange setup: Begin...\n====================================\n";

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
      let uida = await s.registrarAddUser(pka, "usera", "", unwrap<RegionId>rega, true, true, true, true);
      let uidb = await s.registrarAddUser(pkb, "userb", "", unwrap<RegionId>regb, true, true, true, true);
      let uidc = await s.registrarAddUser(pkc, "userc", "", unwrap<RegionId>regc, true, true, true, true);
      let uidd = await s.registrarAddUser(pkd, "userd", "", unwrap<RegionId>regd, true, true, true, true);
      let uide = await s.registrarAddUser(pke, "usere", "", unwrap<RegionId>rege, true, true, true, true);

      printEntityCount("Producer", (await s.getCounts()).producer_count);
      printEntityCount("Transporter", (await s.getCounts()).transporter_count);
      printEntityCount("Retailer", (await s.getCounts()).retailer_count);

      // populate with inventory
      let praia = await s.producerAddInventory(
        pka,
        unwrap<UserId>(uida),
        unwrap<ProduceId>(pea), 100, 100, 10, 0, 110, ""
      );
      let praib = await s.producerAddInventory(
        pka,
        unwrap<UserId>(uida),
        unwrap<ProduceId>(peb), 200, 200, 10, 1, 111, ""
      );
      let praic = await s.producerAddInventory(
        pka,
        unwrap<UserId>(uida),
        unwrap<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prbia = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 200, 200, 10, 4, 117, ""
      );
      let prbib = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 1500, 1600, 9, 2, 115, ""
      );
      let prbic = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prcia = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 200, 200, 9, 4, 711, ""
      );
      let prdib = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 1500, 1500, 7, 2, 115, ""
      );
      let prdic = await s.producerAddInventory(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(pec), 300, 300, 6, 2, 112, ""
      );

      printEntityCount("Inventory@time1", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- remove some of the inventory items added above */

      let x = await s.producerRemInventory(pkb, unwrap<InventoryId>(prdib));
      assertSome<()>(x);

      // a double-remove should return null
      assertNull<()>(await s.producerRemInventory(pkb, unwrap<InventoryId>(prdib)));

      let y = await s.producerRemInventory(pka, unwrap<InventoryId>(praib));
      assertSome<()>(y);

      // a double-remove should return null
      assertNull<()>(await s.producerRemInventory(pka, unwrap<InventoryId>(praib)));

      printEntityCount("Inventory@time2", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- update some of the (remaining) inventory items added above */

      let praic2 = await s.producerUpdateInventory(
        pka,
        unwrap<InventoryId>(praic),
        unwrap<UserId>(uida),
        unwrap<ProduceId>(pec), 666, 300, 10, 2, 112, ""
      );
      assertSome<()>(praic2);

      let prbia2 = await s.producerUpdateInventory(
        pkb,
        unwrap<InventoryId>(prbia),
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 200, 666, 10, 4, 117, ""
      );
      assertSome<()>(prbia2);

      let prbib2 = await s.producerUpdateInventory(
        pkb,
        unwrap<InventoryId>(prbib),
        unwrap<UserId>(uidb),
        unwrap<ProduceId>(peb), 666, 1600, 9, 2, 115, ""
      );
      assertSome<()>(prbib2);

      printEntityCount("Inventory@time3", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- populate with routes */

      let rta_a_c_tta = await s.transporterAddRoute(
        pka,
        unwrap<UserId>(uida),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(tta)
      );
      let rta_b_c_ttb = await s.transporterAddRoute(
        pka,
        unwrap<UserId>(uida),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(ttb)
      );
      let rta_a_c_ttc = await s.transporterAddRoute(
        pka,
        unwrap<UserId>(uida),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(rege),
        0, 20, 100,
        unwrap<TruckTypeId>(ttc)
      );

      let rtb_a_c_tta = await s.transporterAddRoute(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(rege),
        0, 20, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtb_b_c_ttb = await s.transporterAddRoute(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 40, 70,
        unwrap<TruckTypeId>(ttb)
      );
      let rtb_a_c_ttc = await s.transporterAddRoute(
        pkb,
        unwrap<UserId>(uidb),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtc_b_c_tta = await s.transporterAddRoute(
        pkc,
        unwrap<UserId>(uidc),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regb),
        20, 40, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_c_e_tta = await s.transporterAddRoute(
        pkc,
        unwrap<UserId>(uidc),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(regb),
        20, 40, 70,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_a_c_ttc = await s.transporterAddRoute(
        pkc,
        unwrap<UserId>(uidc),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtd_b_c_ttb = await s.transporterAddRoute(
        pkd,
        unwrap<UserId>(uidd),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regd),
        20, 40, 50,
        unwrap<TruckTypeId>(ttb)
      );
      let rtd_c_e_tta = await s.transporterAddRoute(
        pkd,
        unwrap<UserId>(uidd),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(regd),
        20, 40, 70,
        unwrap<TruckTypeId>(tta)
      );

      let rte_a_c_ttc = await s.transporterAddRoute(
        pke,
        unwrap<UserId>(uide),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regd),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      printEntityCount("Route@time1", (await s.getCounts()).route_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- remove some of the routes added above */

      { let x = await s.transporterRemRoute(pkc, unwrap<RouteId>(rtc_b_c_tta));
      assertSome<()>(x); };

      // a double-remove should return null
      assertNull<()>(await s.transporterRemRoute(pkc, unwrap<RouteId>(rtc_b_c_tta)));

      printEntityCount("Route@time2", (await s.getCounts()).route_count);

      { let x = await s.transporterRemRoute(pkc, unwrap<RouteId>(rtc_c_e_tta));
      assertSome<()>(x); };

      // a double-remove should return null
      assertNull<()>(await s.transporterRemRoute(pkc, unwrap<RouteId>(rtc_c_e_tta)));

      printEntityCount("Route@time3", (await s.getCounts()).route_count);

      //////////////////////////////////////////////////////////////////

      print "\nExchange setup: Done.\n====================================\n";
      await debugDumpAll();

      //////////////////////////////////////////////////////////////////

      print "\nRetailer queries\n====================================\n";

      // do some queries
      await retailerQueryAll(pka, uida);
      await retailerQueryAll(pkb, uidb);
      await retailerQueryAll(pkc, uidc);
      await retailerQueryAll(pkd, uidd);
      await retailerQueryAll(pke, uide);

      print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);

      //////////////////////////////////////////////////////////////////
      // xxx --- todo: separate test(s) for expected failures
      // User c should not be able to remove user a's route
      if false {
        print "\nAuthentication test, expect assertion failure:\n";
        ignore(await s.transporterRemRoute(pkc, unwrap<RouteId>(rta_a_c_tta)))
      };
    })
  };
};


func retailerQueryAll(pk:Text, r:?UserId) : async () {

  print "\nRetailer ";
  let retailerId: UserId = unwrap<UserId>(r);
  printInt retailerId;
  print " sends `retailerQueryAll`\n";
  print "------------------------------------\n";

  print "\n## Query begin:\n";
  let res = unwrap<QueryAllResults>(
    await server.retailerQueryAll(pk, retailerId)
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

  print "\nInventory info\n----------------\n";
  for ( info in ((await server.allInventoryInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };

  print "\nRoute info\n----------------\n";
  for ( info in ((await server.allRouteInfo()).vals()) ) {
    print "- ";
    print (debug_show info);
    print "\n";
  };
};

let test = Test();
test.go()
