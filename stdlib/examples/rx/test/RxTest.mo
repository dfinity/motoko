import Array "mo:stdlib/Array";
import Rx "../src/Rx";

type Error = {
  description : Text;
};

func assertEqualError(e1 : Error, e2 : Error) {
  assert (e1.description == e2.description);
};

func assertEqualEvent(
  ev1 : Rx.Event.Type<Nat, Error>,
  ev2 : Rx.Event.Type<Nat, Error>,
) {
  switch (ev1, ev2) {
    case (#next v1, #next v2) {
      assert (v1 == v2);
    };
    case (#error e1, #error e2) {
      assertEqualError(e1, e2);
    };
    case (#completed, #completed) {
      assert true;
    };
    case _ {
      assert false;
    };
  };
};

{
  var events : [Rx.Event.Type<Nat, Error>] = [];

  let observer = Rx.Observer.init<Nat, Error>(
    func (event : Rx.Event.Type<Nat, Error>) {
      events := Array.append<Rx.Event.Type<Nat, Error>>(events, [event]);
    }
  );

  let sequence = Rx.Observable.init<Nat, Error>(
    func (observer : Rx.Observer.Type<Nat, Error>) : Rx.Disposable.Type {
      observer.send(#next 1);
      observer.send(#next 2);
      observer.send(#next 3);
      observer.send(#completed);
      Rx.Disposable.empty();
    }
  );

  let disposable = sequence.subscribe(observer);

  assert (events.len() == 4);
  assertEqualEvent(events[0], #next 1);
  assertEqualEvent(events[1], #next 2);
  assertEqualEvent(events[2], #next 3);
  assertEqualEvent(events[3], #completed);
};

{
  var events : [Rx.Event.Type<Nat, Error>] = [];

  let observer = Rx.Observer.init<Nat, Error>(
    func (event : Rx.Event.Type<Nat, Error>) {
      events := Array.append<Rx.Event.Type<Nat, Error>>(events, [event]);
    }
  );

  let sequence = Rx.Observable.init<Nat, Error>(
    func (observer : Rx.Observer.Type<Nat, Error>) : Rx.Disposable.Type {
      observer.send(#completed);
      observer.send(#next 1);
      observer.send(#error { description = "Error"; });
      observer.send(#completed);
      Rx.Disposable.empty();
    }
  );

  let disposable = sequence.subscribe(observer);

  assert (events.len() == 1);
  assertEqualEvent(events[0], #completed);
};

{
  var events : [Rx.Event.Type<Nat, Error>] = [];

  let observer = Rx.Observer.init<Nat, Error>(
    func (event : Rx.Event.Type<Nat, Error>) {
      events := Array.append<Rx.Event.Type<Nat, Error>>(events, [event]);
    }
  );

  let sequence = Rx.Observable.init<Nat, Error>(
    func (observer : Rx.Observer.Type<Nat, Error>) : Rx.Disposable.Type {
      observer.send(#error { description = "Error"; });
      observer.send(#next 1);
      observer.send(#error { description = "Another error"; });
      observer.send(#completed);
      Rx.Disposable.empty();
    }
  );

  let disposable = sequence.subscribe(observer);

  assert (events.len() == 1);
  assertEqualEvent(events[0], #error { description = "Error"; });
};
