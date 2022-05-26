import MyDependency "mo:mydep/lib";
import List "lib/list";
import { last } "lib/list";

module {
    public func myFunc() {
        let myClass = MyDependency.MyClass();
        let myNil = List.nil<Nat>();
        assert last(myNil) == null;
    }
}
