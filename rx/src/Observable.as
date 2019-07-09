module {
  import Disposable = "Disposable.as";
  import Observer = "Observer.as";

  private type Params<Value, Error> = {
    subscribe : Observer.Type<Value, Error> -> Disposable.Type;
  };

  class Type<Value, Error>(params : Params<Value, Error>) {
    let subscribe = params.subscribe;
  };

  func init<Value, Error>(params : Params<Value, Error>) : Type<Value, Error> {
    Type<Value, Error>(params);
  };
};
