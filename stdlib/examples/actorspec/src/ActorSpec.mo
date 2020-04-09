import Debug "mo:stdlib/Debug";
import Array "mo:stdlib/Array";
import Int "mo:stdlib/Int";
import Text "mo:stdlib/Text";

module {
  public type Group = {
    name : Text;
    groups : [Group];
    status : Status;
  };


  type Status = {
    failed : Nat;
    passed : Nat;
    pending : Nat;
  };

  func eqStatus(x : Status, y : Status) : Bool {
    x.failed == y.failed and x.passed == y.passed and x.pending == y.pending;
  };

  let emptyStatus : Status = {
    failed = 0;
    passed = 0;
    pending = 0;
  };

  func appendStatus(x : Status, y : Status) : Status {
    {
      failed = x.failed + y.failed;
      passed = x.passed + y.passed;
      pending = x.pending + y.pending;
    };
  };

  func printStatus(status : Status) : Text {
    "Failed: " # Int.toText(status.failed) # ", Passed: " # Int.toText(status.passed) # ", Pending: " # Int.toText(status.pending);
  };


  public func run(groups_ : [Group]) : Bool {
    let (groups, status) = getGroups(groups_);
    printGroups(groups, "");
    Debug.print("\n");
    Debug.print(printStatus(status));
    Debug.print("\n");
    Debug.print("\n");
    status.failed == 0;
  };

  func getGroups(groups_ : [Group]) : ([Group], Status) {
    let groups = Array.thaw<Group>(groups_);
    var status = emptyStatus;
    for (index in groups_.keys()) {
      let group = groups[index];
      let (newGroups, newGroupsStatus) = getGroups(group.groups);
      let newStatus = appendStatus(group.status, newGroupsStatus);
      status := appendStatus(status, newStatus);
      let newGroup = {
        name = group.name;
        groups = newGroups;
        status = newStatus;
      };
      groups[index] := newGroup;
    };
    (Array.freeze<Group>(groups), status);
  };

  func printGroups(groups_ : [Group], indent : Text) {
    for (group in groups_.vals()) {
      let isDescribe = group.groups.len() > 0;
      let newline = if isDescribe "\n" else "";
      let status = group.status;
      let statusText = if (isDescribe) {
        ": " # printStatus(status);
      } else {
        let failed = status.failed;
        let passed = status.passed;
        let pending = status.pending;
        switch(failed, passed, pending) {
          case (0, 0, 0) { ""; };
          case (1, 0, 0) { ": Failed"; };
          case (0, 1, 0) { ": Passed"; };
          case (0, 0, 1) { ": Pending"; };
          case (_, _, _) { ":" # printStatus(status); };
        };
      };
      Debug.print(newline # indent # group.name # statusText # "\n");
      printGroups(group.groups, indent # "  ");
    };
  };


  public func describe(name_ : Text, groups_ : [Group]) : Group {
    {
      name = name_;
      groups = groups_;
      status = emptyStatus;
    };
  };

  public func it(name_ : Text, passed_ : Bool) : Group {
    {
      name = name_;
      groups = [];
      status = {
        failed = if passed_ 0 else 1;
        passed = if passed_ 1 else 0;
        pending = 0;
      };
    };
  };

  public let test = it;

  public func pending(name_ : Text) : Group {
    {
      name = name_;
      groups = [];
      status = {
        failed = 0;
        passed = 0;
        pending = 1;
      };
    };
  };

  public func assertTrue(x : Bool) : Bool {
    x == true;
  };

  public func assertFalse(x : Bool) : Bool {
    x == false;
  };

  public func assertAllTrue(xs : [Bool]) : Bool {
    var allTrue = true;
    for (val in xs.vals()) {
      if (val == false) {
        return false;
      };
      allTrue := allTrue and val;
    };
    allTrue;
  };
}
