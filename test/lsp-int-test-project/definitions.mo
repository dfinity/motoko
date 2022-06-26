import MyDependency "mo:mydep/lib";
import List "lib/list";
import { nil; last = lastElement } "lib/list";

module {
    public func myFunc() {
        let myClass = MyDependency.MyClass();
        let myNil = List.nil<Nat>();
        assert lastElement(myNil) == null;
    }
}
