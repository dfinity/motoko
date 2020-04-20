import Debug "mo:stdlib/Debug";
import P "mo:stdlib/Prelude";
import Nat "mo:stdlib/Nat";
//import Render "../render/render";
import I "mo:stdlib/Iter";
import List "mo:stdlib/List";
import RBT "mo:stdlib/RedBlackTree";

module {

  public func run() {
    let sorted =
      [
        (1, "reformer"),
        (2, "helper"),
        (3, "achiever"),
        (4, "individualist"),
        (5, "investigator"),
        (6, "loyalist"),
        (7, "enthusiast"),
        (8, "challenger"),
        (9, "peacemaker"),
      ];

    let unsort =
      [
        (6, "loyalist"),
        (3, "achiever"),
        (9, "peacemaker"),
        (1, "reformer"),
        (4, "individualist"),
        (2, "helper"),
        (8, "challenger"),
        (5, "investigator"),
        (7, "enthusiast"),
      ];

    var t = RBT.RBTree<Nat, Text>(Nat.compare);

    for ((num, lab) in sorted.vals()) {
      Debug.print (Nat.toText num);
      Debug.print lab;
      ignore t.insert(num, lab);
    };

    { var i = 1;
    for ((num, lab) in t.iter()) { 
      assert(num == i);
     i += 1;
    }};

    { var i = 9;
    for ((num, lab) in t.rev()) {
      assert(num == i);
      i -= 1;
    }};

  };
}
