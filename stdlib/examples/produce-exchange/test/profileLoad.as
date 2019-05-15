func scaledParams(region_count_:Nat, factor:Nat) : T.WorkloadParams = {
  shared {
    region_count        = region_count_:Nat;
    day_count           = 3:Nat;
    max_route_duration  = 1:Nat;
    producer_count      = region_count * factor:Nat;
    transporter_count   = region_count * factor:Nat;
    retailer_count      = region_count * factor:Nat;
  }
};
let _ = server.loadWorkload(scaledParams(region_count, scale_factor));
server.getCounts()
