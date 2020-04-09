import Disposable "Disposable";
import Observer "Observer";

module {
  type Subscriber<Value, Error> = Observer.Type<Value, Error> -> Disposable.Type;

  public class Type<Value, Error>(subscribe_ : Subscriber<Value, Error>) {
    public let subscribe = subscribe_;
  };

  public func init<Value, Error>(subscriber : Subscriber<Value, Error>) : Type<Value, Error> {
    Type<Value, Error>(subscriber);
  };
};
