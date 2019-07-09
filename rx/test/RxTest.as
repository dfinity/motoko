import Rx = "../src/Rx.as";

func printLn(x : Text) {
  print(x # "\n");
};

type Error = {
  description : Text;
};

let printLnObserver = Rx.Observer.init<Nat, Error>(new {
  onNext = func (value : Int) {
    printLn("onNext: " # debug_show(value));
  };
  onError = func (error : Error) {
    printLn("onError: " # error.description);
  };
  onCompleted = func () {
    printLn("onCompleted");
  };
});

let sequence = Rx.Observable.init<Nat, Error>(new {
  subscribe = func (observer : Rx.Observer.Type<Nat, Error>) : Rx.Disposable.Type {
    observer.onNext(1);
    observer.onNext(2);
    observer.onNext(3);
    observer.onCompleted();
    Rx.Disposable.empty();
  };
});

let disposable = sequence.subscribe(printLnObserver);
