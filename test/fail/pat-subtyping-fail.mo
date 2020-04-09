func magic() : None = magic();

switch (magic () : Nat) {
  case (-1) {};
  case _ {};
};
switch (magic () : Nat) {
  case (-1 : Int) {};
  case _ {};
};

switch (magic () : (Nat, Nat)) {
  case ((1, -2) : (Int, Int)) {};
  case _ {};
};
switch (magic () : (Nat, Nat)) {
  case (1 : Int, -2 : Int) {};
  case _ {};
};

switch (magic () : {a : Nat; b : Nat}) {
  case ({a = 1; b = -2} : {a : Int; b : Int}) {};
  case _ {};
};
switch (magic () : {a : Nat; b : Nat}) {
  case {a = 1 : Int; b = -2 : Int} {};
  case _ {};
};

switch (magic () : ?Nat) {
  case (? -1) {};
  case _ {};
};
switch (magic () : ?Nat) {
  case (? -1 : ?Int) {};
  case _ {};
};
switch (magic () : ?Nat) {
  case (?(-1 : Int)) {};
  case _ {};
};

switch (magic () : {#A : Nat; #B}) {
  case (#A(-1)) {};
  case _ {};
};
switch (magic () : {#A : Nat; #B}) {
  case (#A(-1 : Int)) {};
  case _ {};
};
switch (magic () : {#A : Nat; #B}) {
  case (#A(-1) : {#A : Int; #B}) {};
  case _ {};
};

switch (magic () : Int) {
  case (n : Nat) {};
};

switch (magic () : (Int, Int)) {
  case (p : (Nat, Nat)) {};
};
switch (magic () : (Int, Int)) {
  case ((x, y) : (Nat, Nat)) {};
};
switch (magic () : (Int, Int)) {
  case (x : Nat, y : Nat) {};
};

switch (magic () : {a : Int; b : Int}) {
  case (r : {a : Nat; b : Nat}) {};
};
switch (magic () : {a : Int; b : Int}) {
  case ({a; b} : {a : Nat; b : Nat}) {};
};
switch (magic () : {a : Int; b : Int}) {
  case {a = _ : Nat; b = _ : Nat} {};
};
switch (magic () : {a : Nat}) {
  case (r : {a : Nat; b : Nat}) {};
};
switch (magic () : {a : Nat}) {
  case ({a; b} : {a : Nat; b : Nat}) {};
};
switch (magic () : {}) {
  case (r : {a : Nat; b : Nat}) {};
};
switch (magic () : {}) {
  case ({a; b} : {a : Nat; b : Nat}) {};
};

switch (magic () : Null) {
  case (null) {};
  case (? _) {};
};
switch (magic () : ?Nat) {
  case (null : Null) {};
  case _ {};
};
switch (magic () : ?Int) {
  case (?n : ?Nat) {};
  case null {};
};
switch (magic () : ?Int) {
  case (?(_ : Nat)) {};
  case _ {};
};

switch (magic () : {#}) {
  case (#A _ : {#A : Nat; #B}) {};  // redundant
};
switch (magic () : {#A : Nat; #B}) {
  case (#A _ : {#A : Nat}) {};
  case _ {};
};
switch (magic () : {#A : Nat; #B}) {
  case (#B : {#B}) {};
  case _ {};
};


switch (magic ()) {
  case true {};
  case 1 {};
};
