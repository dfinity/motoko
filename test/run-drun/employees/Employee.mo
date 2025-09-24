module {
  public type Employee = { id : Nat; name : Text; var manager : ?Employee };
  public type Self = Employee;

  public func new(id : Nat, name : Text) : Employee {
    { id; name; var manager = null }
  };

  public func setManager(e : Employee, newManager : Employee) {
    e.manager := ?newManager;
  };

  public func getManager(e : Employee) : ?Employee {
    e.manager
  };

  public func toText(e : Employee) : Text {
    e.name # "@" # debug_show e.id
  };
}
