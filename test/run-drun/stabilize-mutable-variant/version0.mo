import Prim "mo:prim";

actor {
    type OriginalVariant = {
        #Option1;
        #Option2;
    };

    stable let instance = [ var #Option1: OriginalVariant ];
    stable let alias = instance;

    public func test() : async () {
        Prim.debugPrint("instance=" # debug_show (instance));
        Prim.debugPrint("alias=" # debug_show (alias));
    };
};
