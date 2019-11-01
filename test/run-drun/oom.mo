actor Oom {
  public func doloop() {
    for (i in range(0,1200)) {
      ignore(Array_init<()>(1024/4, ()));
    };
  };
}

// on dvm:
// CALL doloop 0x4449444C0000 []
// CALL doloop 0x4449444C0000 []

// on drun:
//CALL ingress doloop 0x4449444C0000
//CALL ingress doloop 0x4449444C0000
