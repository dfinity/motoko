let _ = future {
  let a = actor {
    let x = await { future 1 };
    public func getX() : future Nat { x };
  };
  ()
}
