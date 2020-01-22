import MyDependency "mo:mydep/lib";
import List "lib/list";

module {
    public func myFunc() {
        let myClass = MyDependency.MyClass();
        let myNil = List.nil<Nat>();
    }
}
