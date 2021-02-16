module M {
  public let value = null;
  public type T = Null;
};

type MT = module { value : Null; type T = Null };

ignore (M : module { value : Null; type T = Null });
ignore (M : MT);
