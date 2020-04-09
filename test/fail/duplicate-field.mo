{ actor class C() {
  public func m() {};
  public func n() {};
  public func m() {};  // Illegal!
}; };

{
type T = {foo : Int; foo: Bool}
};

{
type T = {foo : Int; nxnnbkddcv: Bool}
};

{
type T = {#foo : Int; #nxnnbkddcv: Bool}
};

{
ignore({foo = 5; foo = true});
};

{
ignore({foo = 5; nxnnbkddcv = true});
};

{
ignore(object {public func foo() = (); public func nxnnbkddcv() = ()});
};

{
ignore(actor {public func foo() {}; public func nxnnbkddcv() {}});
};

{
ignore(module {public func foo() = (); public func nxnnbkddcv() = ()});
};

{
// not a clash
ignore(module {
  public let foo = 1;
  public type foo = Int;
  public type nxnnbkddcv = Nat
});};

{
// not a clash
// syntax not supported yet
// type T = {type foo = Int; nxnnbkddcv: Bool}
};
