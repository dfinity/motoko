actor Atomicity {

  var s = 0;
  var pinged = false;

  public func ping() : async () {
    pinged := true;
  };

  // an atomic method
  public func atomic() : async () {
    s := 1;
    let _ = ping();
    ignore 0/0; // trap!
  };

  // a non-atomic method
  public func nonAtomic() : async () {
    s := 1;
    let f = ping();
    s := 2;
    await f;
    s := 3; // this will not be rolled back!
    await f;
    ignore 0/0; // trap!
  };

};
