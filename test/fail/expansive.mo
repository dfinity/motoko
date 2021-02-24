do {
  type C<T> = ?C<T>;  // ok
};

do {
  type C<T,U> = ?C<U,T>;  // ok
};


do {
  type C<T,U> = ?D<T,U>;  // ok
  type D<T,U> = ?C<T,U>;  // ok
};


do {
  type C<T> = ?C<?T>; // reject
};

do {
  type C<T> = <A>C<T>->C<T>; // accept
};


do {
  type C<T> = C< <A>T->T >; // reject
};

do {
  type C<T,U> = ?D<T,?U>;  // reject
  type D<T,U> = ?C<T,U>;
};
