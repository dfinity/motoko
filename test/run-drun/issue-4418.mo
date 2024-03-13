actor {
  public shared func a() : async () {
    let a = actor (await async { "" }) : actor {};
  };
};