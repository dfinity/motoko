import Prim "mo:prim";

actor {
  stable let value : {} = {
    stableField = "Version 1";
  };

  stable let any : Any = null;
  stable let tuple : (Int, Any) = (0, null);
  stable let variant : { #tag : Any } = #tag null;
  stable let record : { lab : Any } = { lab = null };
  stable let vector : [Any] = [null];
  stable let array : [var Any] = [var null];
  stable let opt : ?Any = null;

  public func print() : async () {
    Prim.debugPrint(debug_show (value));
  };
};
