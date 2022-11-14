import Prim "mo:⛔";
import List "lib/list";
import Option "lib/option";
import P "lib/prelude";

module {
    public type Stack = List.List<Int>;

    let (a, b) = (1, 2);

    public func empty(): Stack =
      List.nil<Int>();

    public func push(x: Int, s: Stack): Stack =
      List.push<Int>(x, s);

    public func pop(s: Stack): Option.t<Int> =
      List.pop<Int>(s).0
}
