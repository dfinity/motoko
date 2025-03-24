actor {
   public shared query func test(x: ?Nat) : async () { loop {} };
   stable let shared_function = test;
};
