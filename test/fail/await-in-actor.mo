let _ = async {
  let a = actor {
    let x = await { async 1 };
    public func getX() : async Nat { x };
  };
  ()
}
