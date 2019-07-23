module {
  public type Type<Value, Error> = {
    #next: Value;
    #error: Error;
    #completed;
  };
};
