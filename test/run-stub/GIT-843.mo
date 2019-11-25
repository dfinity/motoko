// Both check_ir and ir_interpreter would failed
// if an actor field name (legally) shadowed the actor name
actor Bad {
  public func Bad (){ debugPrint "ok"};
};
Bad.Bad();
