type Info = {
    name : Text;
    _1_ : Info;
    _345_ : Text;
    name_: Int;
};
type Tup = (Text, Info, Text, Nat);
actor Server {
    public query func f_(user : Tup) : async () {
    };
    public func f__(user : ?Tup) : () { };
}

