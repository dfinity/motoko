do { // reject
  type C<T> = C<T>;
};

do { // reject
  type C = C;
};

do { // reject
  type C<T> = D<T>;
  type D<T> = C<T>;
};

do { // reject
  type C<T> = T;
  type D<T> = C<D<T>>;
};

do { //accept
  type C<T> = T;
  type D<T> = C<T>;
  type E<T> = D<T>;
};

do { //reject
  type C<T> = E<T>;
  type D<T> = C<T>;
  type E<T> = D<T>;
  type T = Nat
};

do { // reject
  type C<T,U> = T;
  type D<T> = C<D<T>,T>;
};

do { // accept
  type C<T,U> = T;
  type D<T> = C<T,D<T>>;
};

do { // reject (ill-formed)
  type C<T,U> = T;
  type D<T> = C<T,D<T>,T>;
};

// nested variants
do { // reject
  type C<T,U> = T;
  do {
    type D<T> = C<D<T>,T>;
  };
};

do { // accept
  type C<T,U> = T;
  do {
    type D<T> = C<T,D<T>>;
  };
};

do { // reject (ill-formed)
  type C<T,U> = T;
  do {
    type D<T> = C<T,D<T>,T>;
  }
};

// accept
do {
  type Top = Any;
  type Bot = None;
  type P = Nat;
  type F = <T>F -> F;
  type T = (T,T);
  type R = {f: R};
  type VarR = {var f: R};
  type V = {#f : V};
  type A = [A];
  type VarA = [var VarA];
  type Async = async ();
};
