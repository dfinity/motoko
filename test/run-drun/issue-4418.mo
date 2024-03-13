// repro for issue-4418
actor {
  public shared func a() : async () {
    ignore actor (await async { "" }) : actor {}; // crashed compiler with effect error
  };
};
