module M {
  public let value = null;
  public type T = Null;
  public type U<T> = T;
};

type MT = module { value : Null;
                   type T = Null;
                   type U<T> = T};

ignore (M : module { value : Null; type T = Null });
ignore (M : MT);
