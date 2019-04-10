/** See `README.md` in this directory. */

/**
Matt-Says: Let's use markdown in the longer comments, in anticipation
of a documentation tool for converting ActorScript into Markdown
files.  I'll use an extra `*` in the opening comment when I expect the
comment to be processed as markdown.
*/

// xxx regression tests go here, for now

func unwrap<T>(ox:?T) : T {
  switch ox {
    case (null) { assert false ; unwrap<T>(ox) };
    case (?x) x;
  }
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

actor class Test() = this {
  go() {
    ignore(async
    {
      let s = server; //Server();

      print "\nExchange setup\n====================================\n";

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

      // populate with producers
      let pra = await s.registrarAddProducer("pra", "", unwrap<RegionId>(rega) );
      let prb = await s.registrarAddProducer("prb", "", unwrap<RegionId>(rega) );
      let prc = await s.registrarAddProducer("prc", "", unwrap<RegionId>(regb) );
      let prd = await s.registrarAddProducer("prd", "", unwrap<RegionId>(rega) );
      let pre = await s.registrarAddProducer("pre", "", unwrap<RegionId>(regb) );

      printEntityCount("Producer", (await s.getCounts()).producer_count);

      // populate with transporters
      let tra = await s.registrarAddTransporter("tra", "" );
      let trb = await s.registrarAddTransporter("trb", "" );
      let trc = await s.registrarAddTransporter("trc", "" );
      let trd = await s.registrarAddTransporter("trd", "" );
      let tre = await s.registrarAddTransporter("tre", "" );

      printEntityCount("Transporter", (await s.getCounts()).transporter_count);

      // populate with retailers
      let rra = await s.registrarAddRetailer("rra", "", unwrap<RegionId>(regc) );
      let rrb = await s.registrarAddRetailer("rrb", "", unwrap<RegionId>(regd) );
      let rrc = await s.registrarAddRetailer("rrc", "", unwrap<RegionId>(rege) );
      let rrd = await s.registrarAddRetailer("rrd", "", unwrap<RegionId>(regc) );
      let rre = await s.registrarAddRetailer("rre", "", unwrap<RegionId>(rege) );

      printEntityCount("Retailer", (await s.getCounts()).retailer_count);

      // populate with inventory
      let praia = await s.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(pea), 100, 100, 10, 0, 110, ""
      );
      let praib = await s.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(peb), 200, 200, 10, 1, 111, ""
      );
      let praic = await s.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prbia = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 200, 200, 10, 4, 117, ""
      );
      let prbib = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 1500, 1600, 9, 2, 115, ""
      );
      let prbic = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(pec), 300, 300, 10, 2, 112, ""
      );
      let prcia = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 200, 200, 9, 4, 711, ""
      );
      let prdib = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 1500, 1500, 7, 2, 115, ""
      );
      let prdic = await s.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(pec), 300, 300, 6, 2, 112, ""
      );

      printEntityCount("Inventory@time1", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- remove some of the inventory items added above */

      let x = await s.producerRemInventory(unwrap<InventoryId>(prdib));
      assertSome<()>(x);

      // a double-remove should return null
      assertNull<()>(await s.producerRemInventory(unwrap<InventoryId>(prdib)));

      let y = await s.producerRemInventory(unwrap<InventoryId>(praib));
      assertSome<()>(y);

      // a double-remove should return null
      assertNull<()>(await s.producerRemInventory(unwrap<InventoryId>(praib)));

      printEntityCount("Inventory@time2", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- update some of the (remaining) inventory items added above */

      let praic2 = await s.producerUpdateInventory(
        unwrap<InventoryId>(praic),
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(pec), 666, 300, 10, 2, 112, ""
      );
      assertSome<()>(praic2);

      let prbia2 = await s.producerUpdateInventory(
        unwrap<InventoryId>(prbia),
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 200, 666, 10, 4, 117, ""
      );
      assertSome<()>(prbia2);

      let prbib2 = await s.producerUpdateInventory(
        unwrap<InventoryId>(prbib),
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 666, 1600, 9, 2, 115, ""
      );
      assertSome<()>(prbib2);

      printEntityCount("Inventory@time3", (await s.getCounts()).inventory_count);

      ////////////////////////////////////////////////////////////////////////////////////

      /**- populate with routes */

      let rta_a_c_tta = await s.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(tta)
      );
      let rta_b_c_ttb = await s.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(ttb)
      );
      let rta_a_c_ttc = await s.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(rege),
        0, 20, 100,
        unwrap<TruckTypeId>(ttc)
      );

      let rtb_a_c_tta = await s.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(rege),
        0, 20, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtb_b_c_ttb = await s.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 40, 70,
        unwrap<TruckTypeId>(ttb)
      );
      let rtb_a_c_ttc = await s.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtc_b_c_tta = await s.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regb),
        20, 40, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_c_e_tta = await s.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(regb),
        20, 40, 70,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_a_c_ttc = await s.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtd_b_c_ttb = await s.transporterAddRoute(
        unwrap<TransporterId>(trd),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regd),
        20, 40, 50,
        unwrap<TruckTypeId>(ttb)
      );
      let rtd_c_e_tta = await s.transporterAddRoute(
        unwrap<TransporterId>(trd),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(regd),
        20, 40, 70,
        unwrap<TruckTypeId>(tta)
      );

      let rte_a_c_ttc = await s.transporterAddRoute(
        unwrap<TransporterId>(tre),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regd),
        20, 40, 97,
        unwrap<TruckTypeId>(ttc)
      );

      printEntityCount("Route", (await s.getCounts()).route_count);

      //////////////////////////////////////////////////////////////////

      print "\nRetailer queries\n====================================\n";

      // do some queries
      let rra_qa = await s.retailerQueryAll(unwrap<RetailerId>(rra));
      let rrb_qa = await s.retailerQueryAll(unwrap<RetailerId>(rrb));
      let rrc_qa = await s.retailerQueryAll(unwrap<RetailerId>(rrc));
      let rrd_qa = await s.retailerQueryAll(unwrap<RetailerId>(rrd));
      let rre_qa = await s.retailerQueryAll(unwrap<RetailerId>(rre));

      print "\nQuery counts\n----------------\n";
      let counts = await s.getCounts();

      printEntityCount("Retailer join", counts.retailer_join_count);
      printEntityCount("Retailer query", counts.retailer_query_count);
      printLabeledCost("Retailer query", counts.retailer_query_cost);


    })
  };
};

let test = Test();
test.go()
