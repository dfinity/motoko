actor Oom {
  public func doloop() {
    ignore(Array_init<()>(1200*1024/4, ()));
  };
}

// on drun:
//CALL ingress doloop 0x4449444C0000
//CALL ingress doloop 0x4449444C0000
