import Hash "mo:base/Hash";
import Types "Types";

module {
public type Role = Types.Role;

public func equal(r1 : Role, r2 : Role) : Bool {
  r1 == r2
};

public func max(r1 : Role, r2 : Role) : Role {
  switch (r1, r2) {
    case (#admin, _) { #admin };
    case (_, #admin) { #admin };
    case (#user, #user) { #user };
    case (#user, #guest) { #user };
    case (#guest, #user) { #user };
    case (#guest, #guest) { #guest };
  }
};

public func hash(r : Role) : Hash.Hash {
  switch r {
  case (#user) 0;
  case (#admin) 1;
  case (#guest) 2;
  };
};
}
