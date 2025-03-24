actor {
   public shared query func test(x: ?Nat) : async ?Int { loop {} };
   stable let shared_function = test;
};
