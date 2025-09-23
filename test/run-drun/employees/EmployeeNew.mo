module {
  public persistent class Employee(id_ : Nat, name_ : Text) {
    public let id = id_;
    public let name = name_;
    var manager : ?Employee = null;

    public func setManager(newManager : Employee) {
      manager := ?newManager;
    };

    public func getManager() : ?Employee {
      return manager
    };

    public func getManagerOfManager() : ?Employee {
      switch manager {
        case null null;
        case (?m) m.getManager();
      }
    };

    public func toText() : Text {
      name # "@" # debug_show id
    };
  };
}
