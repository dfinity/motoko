actor {
  public shared func a() : async () {
    let a = actor (await async { "" }) : actor {};
    // let t = await async {};let a = actor (t) : actor {}; works

  };
};