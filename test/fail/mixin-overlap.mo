import CounterMixin "mixins/Counter";

actor {
  include CounterMixin(0);
  public func increment() { };
}
