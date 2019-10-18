type Info = {
    name : Text;
    _1_ : Info;
    _345_ : Text;
    _34_5_: Bool;
    _name_: Int;
    name__ : Nat;
};
type Tup = (Text, Info, Text, Nat);
actor Server {
    public func f_(user : Tup) : async () {
    };
    public func f__(user : ?Tup) : () { };
}

