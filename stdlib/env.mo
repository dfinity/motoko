/*
  Mocking an environment that can include time, caller id, etc.
*/

module {
  public type Time = Nat; 
   
  public class Env() {
    // Environmental state. Currently, this is only time.
    var t : Time = 0;

    // Access functions.
    // Currently, we make the time advance with every access.
    public func currentTime() : Time { t += 10; t };

    // Wrap value plus environmental state into an "event".
    public func wrap<T>(v : T) : Event<T> = Event<T>(v, currentTime()); 
    public func unwrap<T>(e : Event<T>) : T = e.value();
  };

  // The Event class bundles a value with environmental state.
  // The class is static, i.e. has no write functions besides the constructor.
  public type EventRep<T> = (Time, T);
  public class Event<T>(value_ : T, time_ : Time) {
    let t : Time = time_;
    let v : T = value_;
  
    public func time() : Time = t ;
    public func value() : T = v ;
    public func rep() : EventRep<T> = (t,v) ; 
  };
}