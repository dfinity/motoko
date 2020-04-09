import Event "Event";

module {
  public type EventHandler<Value, Error> = Event.Type<Value, Error> -> ();

  public class Type<Value, Error>(handler_ : EventHandler<Value, Error>) {
    let handler = handler_;
    var isDone = false;

    public func send(event : Event.Type<Value, Error>) : () {
      if (isDone) {
        return;
      };
      handler(event);
      switch (event) {
        case (#next _) {};
        case _ {
          isDone := true;
        };
      };
    }
  };

  public func init<Value, Error>(send : EventHandler<Value, Error>) : Type<Value, Error> {
    Type<Value, Error>(send);
  };
};
