actor {
   public shared query func test() : async () { loop {} };
   stable let shared_function = test;
};
