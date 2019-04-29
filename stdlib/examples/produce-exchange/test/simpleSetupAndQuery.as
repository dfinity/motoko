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
      let uida = await s.registrarAddUser(pka, "usera", "", assertUnwrapAny<RegionId>rega, true, true, true, true);
      let uidb = await s.registrarAddUser(pkb, "userb", "", assertUnwrapAny<RegionId>regb, true, true, true, true);
      let uidc = await s.registrarAddUser(pkc, "userc", "", assertUnwrapAny<RegionId>regc, true, true, true, true);
      let uidd = await s.registrarAddUser(pkd, "userd", "", assertUnwrapAny<RegionId>regd, true, true, true, true);
      let uide = await s.registrarAddUser(pke, "usere", "", assertUnwrapAny<RegionId>rege, true, true, true, true);

      printEntityCount("User", (await s.getCounts()).user_count);
      printEntityCount("Producer", (await s.getCounts()).producer_count);
      printEntityCount("Transporter", (await s.getCounts()).transporter_count);
      printEntityCount("Retailer", (await s.getCounts()).retailer_count);

      // populate with inventory
      let praia = await s.producerAddInventory(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<ProduceId>(pea), 100, 100, 10, 0, 110, ""
      );
      let praib = await s.producerAddInventory(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<ProduceId>(peb), 200, 200, 10, 1, 111, ""
      );
      let praic = await s.producerAddInventory(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prbia = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(peb), 200, 200, 10, 4, 117, ""
      );
      let prbib = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(peb), 1500, 1600, 9, 2, 115, ""
      );
      let prbic = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prcia = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(peb), 200, 200, 9, 4, 711, ""
      );
      let prdib = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(peb), 1500, 1500, 7, 2, 115, ""
      );
      let prdic = await s.producerAddInventory(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<ProduceId>(pec), 300, 300, 6, 2, 112, ""
      );

      printEntityCount("Inventory@time1", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- remove some of the inventory items added above */

      //assertOk(await s.producerRemInventory(pkd, assertUnwrapAny<InventoryId>(prdib)));

      // a double-remove should return null
      //assertErr(await s.producerRemInventory(pkb, assertUnwrapAny<InventoryId>(prdib)));

      //assertOk(await s.producerRemInventory(pka, assertUnwrapAny<InventoryId>(praib)));

      // a double-remove should return null
      //assertErr(await s.producerRemInventory(pka, assertUnwrapAny<InventoryId>(praib)));

      printEntityCount("Inventory@time2", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- update some of the (remaining) inventory items added above */

      assertOk(
        await s.producerUpdateInventory(
          pka,
          assertUnwrapAny<InventoryId>(praic),
          assertUnwrapAny<UserId>(uida),
          assertUnwrapAny<ProduceId>(pec), 666, 300, 10, 2, 112, ""
        ));

      assertOk(
        await s.producerUpdateInventory(
          pkb,
          assertUnwrapAny<InventoryId>(prbia),
          assertUnwrapAny<UserId>(uidb),
          assertUnwrapAny<ProduceId>(peb), 200, 666, 10, 4, 117, ""
        ));

      assertOk(
        await s.producerUpdateInventory(
          pkb,
          assertUnwrapAny<InventoryId>(prbib),
          assertUnwrapAny<UserId>(uidb),
          assertUnwrapAny<ProduceId>(peb), 666, 1600, 9, 2, 115, ""
        ));

      printEntityCount("Inventory@time3", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- populate with routes */

      let rta_a_c_tta = await s.transporterAddRoute(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<RegionId>(rega),
        assertUnwrapAny<RegionId>(regc),
        0, 20, 100,
        assertUnwrapAny<TruckTypeId>(tta)
      );
      let rta_b_c_ttb = await s.transporterAddRoute(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<RegionId>(regb),
        assertUnwrapAny<RegionId>(regc),
        0, 20, 100,
        assertUnwrapAny<TruckTypeId>(ttb)
      );
      let rta_a_c_ttc = await s.transporterAddRoute(
        pka,
        assertUnwrapAny<UserId>(uida),
        assertUnwrapAny<RegionId>(rega),
        assertUnwrapAny<RegionId>(rege),
        0, 20, 100,
        assertUnwrapAny<TruckTypeId>(ttc)
      );

      let rtb_a_c_tta = await s.transporterAddRoute(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<RegionId>(regc),
        assertUnwrapAny<RegionId>(rege),
        0, 20, 40,
        assertUnwrapAny<TruckTypeId>(tta)
      );
      let rtb_b_c_ttb = await s.transporterAddRoute(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<RegionId>(regb),
        assertUnwrapAny<RegionId>(regc),
        0, 40, 70,
        assertUnwrapAny<TruckTypeId>(ttb)
      );
      let rtb_a_c_ttc = await s.transporterAddRoute(
        pkb,
        assertUnwrapAny<UserId>(uidb),
        assertUnwrapAny<RegionId>(rega),
        assertUnwrapAny<RegionId>(regc),
        20, 40, 97,
        assertUnwrapAny<TruckTypeId>(ttc)
      );

      let rtc_b_c_tta = await s.transporterAddRoute(
        pkc,
        assertUnwrapAny<UserId>(uidc),
        assertUnwrapAny<RegionId>(regb),
        assertUnwrapAny<RegionId>(regb),
        20, 40, 40,
        assertUnwrapAny<TruckTypeId>(tta)
      );
      let rtc_c_e_tta = await s.transporterAddRoute(
        pkc,
        assertUnwrapAny<UserId>(uidc),
        assertUnwrapAny<RegionId>(regc),
        assertUnwrapAny<RegionId>(regb),
        20, 40, 70,
        assertUnwrapAny<TruckTypeId>(tta)
      );
      let rtc_a_c_ttc = await s.transporterAddRoute(
        pkc,
        assertUnwrapAny<UserId>(uidc),
        assertUnwrapAny<RegionId>(rega),
        assertUnwrapAny<RegionId>(regc),
        20, 40, 97,
        assertUnwrapAny<TruckTypeId>(ttc)
      );

      let rtd_b_c_ttb = await s.transporterAddRoute(
        pkd,
        assertUnwrapAny<UserId>(uidd),
        assertUnwrapAny<RegionId>(regb),
        assertUnwrapAny<RegionId>(regd),
        20, 40, 50,
        assertUnwrapAny<TruckTypeId>(ttb)
      );
      let rtd_c_e_tta = await s.transporterAddRoute(
        pkd,
        assertUnwrapAny<UserId>(uidd),
        assertUnwrapAny<RegionId>(regc),
        assertUnwrapAny<RegionId>(regd),
        20, 40, 70,
        assertUnwrapAny<TruckTypeId>(tta)
      );

      let rte_a_c_ttc = await s.transporterAddRoute(
        pke,
        assertUnwrapAny<UserId>(uide),
        assertUnwrapAny<RegionId>(rega),
        assertUnwrapAny<RegionId>(regd),
        20, 40, 97,
        assertUnwrapAny<TruckTypeId>(ttc)
      );

      printEntityCount("Route@time1", (await s.getCounts()).route_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- remove some of the routes added above */

      //assertOk(await s.transporterRemRoute(pkc, assertUnwrapAny<RouteId>(rtc_b_c_tta)));

      // a double-remove should return null
      //assertErr(await s.transporterRemRoute(pkc, assertUnwrapAny<RouteId>(rtc_b_c_tta)));

      printEntityCount("Route@time2", (await s.getCounts()).route_count);

      //assertOk(await s.transporterRemRoute(pkc, assertUnwrapAny<RouteId>(rtc_c_e_tta)));

      // a double-remove should return null
      //assertErr(await s.transporterRemRoute(pkc, assertUnwrapAny<RouteId>(rtc_c_e_tta)));

      printEntityCount("Route@time3", (await s.getCounts()).route_count);

      //////////////////////////////////////////////////////////////////

      print "\nExchange setup: Done.\n====================================\n";
      await debugDumpAll();

      //////////////////////////////////////////////////////////////////

      print "\nRetailer queries\n====================================\n";

      // do some queries
      await retailerQueryAll(pka, ? assertUnwrapAny<UserId>(uida));
      await retailerQueryAll(pkb, ? assertUnwrapAny<UserId>(uidb));
      await retailerQueryAll(pkc, ? assertUnwrapAny<UserId>(uidc));
      await retailerQueryAll(pkd, ? assertUnwrapAny<UserId>(uidd));
      await retailerQueryAll(pke, ? assertUnwrapAny<UserId>(uide));

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
        ignore(await s.transporterRemRoute(pkc, assertUnwrapAny<RouteId>(rta_a_c_tta)))
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
  let res = assertUnwrapAny<QueryAllResults>(
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

  print "\nUser info\n----------------\n";
  for ( info in ((await server.allUserInfo()).vals()) ) {
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
