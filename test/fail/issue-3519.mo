shared ({ caller = owner }) actor class Service(playerPrincipal : Principal) = this {

  type GenericType<T> = {
    thing : Nat;
  };

  // test
  public shared ({ caller }) func asplode<T>() : async GenericType<T> {
    return {
      thing = 1;
    };
  };

};