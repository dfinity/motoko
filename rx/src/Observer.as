module {
  private type Params<Value, Error> = {
    onNext : Value -> ();
    onError : Error -> ();
    onCompleted : () -> ();
  };

  class Type<Value, Error>(params : Params<Value, Error>) {
    let onNext = params.onNext;
    let onError = params.onError;
    let onCompleted = params.onCompleted;
  };

  func init<Value, Error>(params : Params<Value, Error>) : Type<Value, Error> {
    Type<Value, Error>(params);
  };
};
