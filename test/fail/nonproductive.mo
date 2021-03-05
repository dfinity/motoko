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
  type t = Nat
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
