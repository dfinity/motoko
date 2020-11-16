let x = ();
func f(_ : Any) {};


// deprecated
f({ x });
f({ let x = 0 });
f { x };
f { let x = 0 };
f {};
f {x = 0};
f {var x = 0};

// okay
f({});  // object
f({var x = 0});  // object
f({x = 0});
f(do { x });
f(do { let x = 0 });


// deprecated
{ x };
{ let x = 0 };
ignore (do {x = 0});
ignore do {
  { x };
};
ignore (do ({}));
ignore (do ({ var x = 0 }));

// okay
do {};
do { x };
do { let x = 0 };
do { var x = 0 };
ignore do {
  {};  // object
};
ignore do {
  {x = 0};
};
ignore do {
  {x = 0}.x;
};
ignore do {
  {var x = 0};  // object
};
ignore do {
  {var x = 0}.x;
};


// deprecated
if true do {};
ignore (if true object {});
ignore (if true object { var x = 0 });
ignore (if true {x = 0});
ignore (if true {{}} else {x = 0});
ignore (if true {x = 0}.x);
ignore (if true ({}));
ignore (if true ({x = 0}));
ignore (if true ({x = 0}.x));
ignore (if true ({var x = 0}));
ignore (if true ({var x = 0}.x));
ignore do {
  switch 0 {
    case 1 {x = 0};
    case 4 do {};
    case 5 ();
    case 6 ({});
    case 7 ({var x = 0});
    case 8 ({x = 0});
    case 9 {x = 0}.x;
    case _ object { var x = 0 };
  }
};

// okay
if true {};
if true { x };
if true { let x = 0 };
if true { var x = 0 };  // block
ignore (if true { {} });
ignore (if true { {x = 0} });
ignore (if true { {var x = 0} });
ignore {};
ignore do {
  switch 0 {
    case 0 {};
    case 1 { x };
    case 2 { let x = 0 };
    case 3 { var x = 0 };  // block
    case 9 { {} };
    case 10 { {var x = 0} };
    case _ { {x = 0} };
  }
};


// deprecated
func g1() = { return };
func g2() : () = { return };

// okay
func f1() { return };
func f2() = do { return };
func f3() : () { return };
func f4() : () = do { return };
func f5() : {} = {};
func f6() : {} = {x = 0};
func f7() : {} = {var x = 0};
func f8() : Nat = {x = 0}.x;
func f9() : Nat = {var x = 0}.x;


// deprecated
actor a2 {
  shared {} func f() : async () {};
  shared query {} func g() : async () {};
  query {} func h() : async () {};
};

// okay
actor a1 {
  shared({}) func f() : async () {};
  shared query({}) func g() : async () {};
  query({}) func h() : async () {};
};
