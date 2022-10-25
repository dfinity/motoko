// @verify

// This example should demonstrate all static assertions that are currently 
// supported.

actor {

  var u = false;
  var v = 0 : Int;

  assert:invariant u;                 // canister invariant

  public shared func claim() : async () {
      assert:func v >= 0;             // function precondition
      
      assert:system u implies v > 0;  // static assertion
      assert u implies v > 0;         // dynamic assertion

      await async {
        assert:1:async true;          // concurrency constraints
      };

      assert:return v >= 0;           // function postcondition
  };

}
