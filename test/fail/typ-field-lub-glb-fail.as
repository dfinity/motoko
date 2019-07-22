class Ctx() {
  public type T = Int;
  public let x = 0;
};

class Ctx2() {
  public type T = Int;
  public let x : Int = 0
};

class Cty() {
  public type T = Int;
  public let y = true
};


class Ct() {
  public type T = Int;
};

class Cx() {
  public let x = 1;
};

class Dtx() {
  public type T = Bool;
  public let x = 1
};

let _ : [Ctx] = [ Ctx(), Cx() ]; //wrong
let _ : [Ctx] = [ Ctx(), Ct() ]; //wrong

let otx : Ctx =
  [ object { public type T = Int; public let x = 1 },
    object { public type T = Int; public let x = 2 } ][0];

let ox =
  [ object { public type T = Int; public let x = 1 },
    object { public type T = Bool; public let x = 2 } ][0];

func wrong3() { type U = ox.T }; // wrong

