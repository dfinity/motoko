func f() {
  let _ = 0;
  let a = 0;
  let 5 = 0;
  let (5; 5) = 0;
  let (5; _) = 0;
  let (_; 6) = 0;
  let (_; _) = 0;

  func(_ : Nat) {};
  func(x : Nat) {};
  func(5) {};
  func(5; 5) {};
  func(5; _ : Nat) {};
  func(_ : Nat; 6) {};
  func((_; _) : Nat) {};

  switch 0 { case _ {} };
  switch 0 { case x {} };
  switch 0 { case 5 {} };
  switch 0 { case 5 {} case 5 {} };
  switch 0 { case 5 {} case _ {} };
  switch 0 { case 5 {} case x {} };
  switch 0 { case _ {} case 6 {} };
  switch 0 { case _ {} case x {} };
  switch 0 { case x {} case _ {} };
  switch 0 { case x {} case x {} };
  switch 0 { case _ {} case _ {} };
  switch 0 { case (5; 6) {} case (7; 6) {} };
};
