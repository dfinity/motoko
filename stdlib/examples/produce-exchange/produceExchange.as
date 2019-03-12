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

actor class Test() = this {
  go() {
    ignore(async
    {
      let pe = ProduceExchange();

      // populate with truck types
      let tta = await pe.registrarAddTruckType("tta", "", 10, false, false);
      let ttb = await pe.registrarAddTruckType("ttb", "", 20, false, false);
      let ttc = await pe.registrarAddTruckType("ttc", "", 10, true, false);
      let ttd = await pe.registrarAddTruckType("ttd", "", 30, true, false);
      let tte = await pe.registrarAddTruckType("tte", "", 50, false, true);

      // populate with regions
      let rega = await pe.registrarAddRegion("rega", "");
      let regb = await pe.registrarAddRegion("regb", "");
      let regc = await pe.registrarAddRegion("regc", "");
      let regd = await pe.registrarAddRegion("regd", "");
      let rege = await pe.registrarAddRegion("rege", "");

      // populate with produce
      let pea = await pe.registrarAddProduce("avocado1", "avocado", 1);
      let peb = await pe.registrarAddProduce("avocado2", "avocado avocado", 2);
      let pec = await pe.registrarAddProduce("avocado3", "avocado avocado avocado", 3);
      let ped = await pe.registrarAddProduce("avocado4", "avocado avocado avocado avocado", 4);
      let pee = await pe.registrarAddProduce("avocado5", "avocado avocado avocado avocado avocado", 5);

      // populate with transporters
      let tra = await pe.registrarAddTransporter("tra", "" );
      let trb = await pe.registrarAddTransporter("trb", "" );
      let trc = await pe.registrarAddTransporter("trc", "" );
      let trd = await pe.registrarAddTransporter("trd", "" );
      let tre = await pe.registrarAddTransporter("tre", "" );

      // populate with producers
      let pra = await pe.registrarAddRetailer("pra", "", unwrap<RegionId>(rega) );
      let prb = await pe.registrarAddRetailer("prb", "", unwrap<RegionId>(rega) );
      let prc = await pe.registrarAddRetailer("prc", "", unwrap<RegionId>(regb) );
      let prd = await pe.registrarAddRetailer("prd", "", unwrap<RegionId>(rega) );
      let pre = await pe.registrarAddRetailer("pre", "", unwrap<RegionId>(regb) );

      // populate with retailers
      let rra = await pe.registrarAddRetailer("rra", "", unwrap<RegionId>(regc) );
      let rrb = await pe.registrarAddRetailer("rrb", "", unwrap<RegionId>(regd) );
      let rrc = await pe.registrarAddRetailer("rrc", "", unwrap<RegionId>(rege) );
      let rrd = await pe.registrarAddRetailer("rrd", "", unwrap<RegionId>(regc) );
      let rre = await pe.registrarAddRetailer("rre", "", unwrap<RegionId>(rege) );

      // populate with inventory
      let praia = await pe.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(pea), 100, 10, 0, 10
      );
      let paib = await pe.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(peb), 200, 10, 1, 11
      );
      let praic = await pe.producerAddInventory(
        unwrap<ProducerId>(pra),
        unwrap<ProduceId>(pec), 300, 10, 2, 12
      );
      let prbia = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 200, 10, 4, 7
      );
      let prbib = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 1500, 9, 2, 15
      );
      let prbic = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(pec), 300, 10, 2, 12
      );
      let prcia = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 200, 9, 4, 7
      );
      let prdib = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(peb), 1500, 7, 2, 15
      );
      let prdic = await pe.producerAddInventory(
        unwrap<ProducerId>(prb),
        unwrap<ProduceId>(pec), 300, 6, 2, 12
      );

      // populate with routes
      let rta_a_c_tta = await pe.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(tta)
      );
      let rta_b_c_ttb = await pe.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(ttb)
      );
      let rta_a_c_ttc = await pe.transporterAddRoute(
        unwrap<TransporterId>(tra),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 100,
        unwrap<TruckTypeId>(ttc)
      );

      let rtb_a_c_tta = await pe.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtb_b_c_ttb = await pe.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 70,
        unwrap<TruckTypeId>(ttb)
      );
      let rtb_a_c_ttc = await pe.transporterAddRoute(
        unwrap<TransporterId>(trb),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtc_b_c_tta = await pe.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 40,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_c_e_tta = await pe.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(rege),
        0, 20, 70,
        unwrap<TruckTypeId>(tta)
      );
      let rtc_a_c_ttc = await pe.transporterAddRoute(
        unwrap<TransporterId>(trc),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 97,
        unwrap<TruckTypeId>(ttc)
      );

      let rtd_b_c_ttb = await pe.transporterAddRoute(
        unwrap<TransporterId>(trd),
        unwrap<RegionId>(regb),
        unwrap<RegionId>(regc),
        0, 20, 50,
        unwrap<TruckTypeId>(ttb)
      );
      let rtd_c_e_tta = await pe.transporterAddRoute(
        unwrap<TransporterId>(trd),
        unwrap<RegionId>(regc),
        unwrap<RegionId>(rege),
        0, 20, 70,
        unwrap<TruckTypeId>(tta)
      );

      let rte_a_c_ttc = await pe.transporterAddRoute(
        unwrap<TransporterId>(tre),
        unwrap<RegionId>(rega),
        unwrap<RegionId>(regc),
        0, 20, 97,
        unwrap<TruckTypeId>(ttc)
      );

      // ----------------------------------------------------------------------------

      // do some queries
      let rra_qa = await pe.retailerQueryAll(unwrap<RetailerId>(rra));
      let rrb_qa = await pe.retailerQueryAll(unwrap<RetailerId>(rrb));
      let rrc_qa = await pe.retailerQueryAll(unwrap<RetailerId>(rrc));
      let rrd_qa = await pe.retailerQueryAll(unwrap<RetailerId>(rrd));
      let rre_qa = await pe.retailerQueryAll(unwrap<RetailerId>(rre));

      // xxx find a way to display these query results, by printing them out here
    })
  };
};

let test = Test();
test.go()
