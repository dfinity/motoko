func fib(n : Nat) : Nat {
  if (n == 0) return(1);
  n * fib (n-1);
};

assert (fib 5 == 120);
