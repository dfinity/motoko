let _ = async {
  let a = actor {
    private x = await { async 1 };
    getX() : async Nat { x };
  };
  ()
}
