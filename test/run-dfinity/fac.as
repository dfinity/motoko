func fac(n : Nat) : Nat {
  if (n == 0) return(1);
  n * fac (n-1);
};

printInt(fac 5);
