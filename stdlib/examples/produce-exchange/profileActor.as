let T = (import "serverTypes.as");
let Model = (import "serverModel.as");

actor server {
  // morally: loadQuery (region_count:Nat, scale_factor:Nat) {
  //
  // except, we assume that region_count and scale_factor are already defined in the enclosing env
  //
  private var dummy = {
    func scaledParams(region_count_:Nat, factor:Nat) : T.WorkloadParams = {
      new {
        region_count        = region_count_:Nat;
        day_count           = 3:Nat;
        max_route_duration  = 1:Nat;
        producer_count      = region_count * factor:Nat;
        transporter_count   = region_count * factor:Nat;
        retailer_count      = region_count * factor:Nat;
      }
    };
    let m = Model.Model();
    let _ = m.loadWorkload(scaledParams(region_count, scale_factor));
    let _ = m.retailerQueryAll(0, null, null);
    ()
  };
}
