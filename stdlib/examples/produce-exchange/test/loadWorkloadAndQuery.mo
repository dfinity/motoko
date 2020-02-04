func scaledParams(region_count_:Nat, factor:Nat) : T.WorkloadParams = {
  debugPrint "## region_count = "; debugPrintInt region_count_; debugPrint "\n";
  debugPrint "## factor = "; debugPrintInt factor; debugPrint "\n";
  debugPrint "## data format: source-region, evaluation-count:\n";
  shared {
    region_count        = region_count_:Nat;
    day_count           = 3:Nat;
    max_route_duration  = 1:Nat;
    producer_count      = region_count * factor:Nat;
    transporter_count   = region_count * factor:Nat;
    retailer_count      = region_count * factor:Nat;
  }
};

actor class Test() = this {
  go() {
    ignore(async
    {
      // Vary the choice of region count and scaling factor here;
      // Each choice leads to a different count of (InventoryCount, RouteCount), and load time:
      let params = {
        //scaledParams(2, 2);   // (40,    40   ), loads in 0.7s in my AS interpreter
        //scaledParams(5, 5);   // (625  , 625  ), loads in 8.8s in my AS interpreter
        //scaledParams(8, 10);  // (3_200, 3_200), loads in 1:07min in my AS interpreter
        //scaledParams(10, 10); // (5_000, 5_000), loads in 2:13min in my AS interpreter

        scaledParams(5, 2);
      };
      let s = server;
      await s.loadWorkload(params);
    })
  };
};

let test = Test();
test.go()
