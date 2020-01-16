import MyDependency "mo:mydep/lib.mo";
import List "lib/list.mo";

module {
    public func myFunc() {
        let myClass = MyDependency.MyClass();
        let myNil = List.nil<Nat>();
    }
}
