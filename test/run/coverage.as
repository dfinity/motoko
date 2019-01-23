func f() {
  let _ = 0;
  let a = 0;
  let 5 = 0;
  let (5 or 5) = 0;
  let (5 or _) = 0;
  let (_ or 6) = 0;
  let (_ or _) = 0;
  let ?b = ?0;
  let ?_ = ?0;
  let ?9 = ?0;

  func(_ : Nat) {};
  func(x : Nat) {};
  func(5) {};
  func(5 or 5) {};
  func(5 or _ : Nat) {};
  func(_ or 6 : Nat) {};
  func((_ or _) : Nat) {};

  switch 0 { case _ {} };
  switch 0 { case x {} };
  switch 0 { case 5 {} };
  switch 0 { case 5 {}; case 5 {} };
  switch 0 { case 5 {}; case _ {} };
  switch 0 { case 5 {}; case x {} };
  switch 0 { case _ {}; case 6 {} };
  switch 0 { case _ {}; case x {} };
  switch 0 { case x {}; case _ {} };
  switch 0 { case x {}; case x {} };
  switch 0 { case _ {}; case _ {} };
  switch 0 { case (5 or 6) {}; case (7 or 6) {} };
  switch (0, 0) { case (_, _) {}; case (_, 6) {} };
  switch (0, 0) { case (_, (6 or _)) {}; case _ {} };
  switch (0, 0) { case (0, _) {}; case (_, 0) {} };
  switch (0, 0) { case (0, _) {}; case (_, 0) {}; case _ {} };
};
