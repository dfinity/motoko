type Info = {
    name : Text;
    // TODO not accepted by parser
    //_1_ : Info;
    //_34_5_ : Text;
    name_ : Nat;
};
type Tup = (Text, Info, Text, Nat);
actor Server {
    public func f_(user1 : Info, user2 : Tup) : async () {
    };
}
