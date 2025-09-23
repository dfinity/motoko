module {
  public persistent class Employee(id : Nat, name : Text) {
    var manager : ?Employee = null;

    public func setManager(newManager : Employee) {
      manager := ?newManager;
    };

    public func getManager() : ?Employee {
      return manager
    };

    public func toText() : Text {
      name # "@" # debug_show id
    };
  };
}
