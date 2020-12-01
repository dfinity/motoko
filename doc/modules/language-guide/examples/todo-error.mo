// tag::imports[]
import Int "mo:base/Int";
import Hash "mo:base/Hash";
import Map "mo:base/HashMap";
import Time "mo:base/Time";
import Result "mo:base/Result";
import Error "mo:base/Error";
// end::imports[]

actor Todo {

// tag::intro[]
type Time = Int;
type Seconds = Int;

func secondsBetween(start : Time, end : Time) : Seconds =
  (end - start) / 1_000_000_000;

public type TodoId = Nat;

type Todo = { #todo : { text : Text; opened : Time }; #done : Time };
type TodoMap = Map.HashMap<TodoId, Todo>;

var idGen : TodoId = 0;
let todos : TodoMap = Map.HashMap(32, Int.equal, Hash.hash);

private func nextId() : TodoId {
  let id = idGen;
  idGen += 1;
  id
};

/// Creates a new todo and returns its id
public shared func newTodo(txt : Text) : async TodoId {
  let id = nextId();
  let now = Time.now();
  todos.put(id, #todo({ text = txt; opened = now }));
  id
};
// end::intro[]

// tag::sentinel-definition[]
public shared func markDoneBad(id : TodoId) : async Seconds {
  switch (todos.get(id)) {
    case (?(#todo(todo))) {
      let now = Time.now();
      todos.put(id, #done(now));
      secondsBetween(todo.opened, now)
    };
    case _ { -1 };
  }
};
// end::sentinel-definition[]

// tag::option-definition[]
public shared func markDoneOption(id : TodoId) : async ?Seconds {
  switch (todos.get(id)) {
    case (?(#todo(todo))) {
      let now = Time.now();
      todos.put(id, #done(now));
      ?(secondsBetween(todo.opened, now))
    };
    case _ { null };
  }
};
// end::option-definition[]

// tag::todo-error[]
public type TodoError = { #notFound; #alreadyDone : Time };
// end::todo-error[]

// tag::result-definition[]
public shared func markDoneResult(id : TodoId) : async Result.Result<Seconds, TodoError> {
  switch (todos.get(id)) {
    case (?(#todo(todo))) {
      let now = Time.now();
      todos.put(id, #done(now));
      #ok(secondsBetween(todo.opened, now))
    };
    case (?(#done(time))) {
      #err(#alreadyDone(time))
    };
    case null {
      #err(#notFound)
    };
  }
};
// end::result-definition[]
// tag::exception-definition[]
public shared func markDoneException(id : TodoId) : async Seconds {
  switch (todos.get(id)) {
    case (?(#todo(todo))) {
      let now = Time.now();
      todos.put(id, #done(now));
      secondsBetween(todo.opened, now)
    };
    case (?(#done(time))) {
      throw Error.reject("Already done")
    };
    case null {
      throw Error.reject("Not Found")
    };
  }
};
// end::exception-definition[]
};

actor TodoCaller {

type Time = Int;
type Seconds = Int;

func secondsBetween(start : Time, end : Time) : Seconds =
  (end - start) / 1_000_000_000;

public shared func mkTodo() : async Todo.TodoId {
  await Todo.newTodo("Write error handling tutorial")
};
// tag::sentinel-caller[]
public shared func doneTodo1(id : Todo.TodoId) : async Text {
  let seconds = await Todo.markDoneBad(id);
  if (seconds != -1) {
    "Congrats! That took " # Int.toText(seconds) # " seconds.";
  } else {
    "Something went wrong.";
  };
};
// end::sentinel-caller[]

// tag::option-caller[]
public shared func doneTodo2(id : Todo.TodoId) : async Text {
  switch (await Todo.markDoneOption(id)) {
    case null {
      "Something went wrong."
    };
    case (?seconds) {
      "Congrats! That took " # Int.toText(seconds) # " seconds."
    };
  };
};
// end::option-caller[]

// tag::result-caller[]
public shared func doneTodo3(id : Todo.TodoId) : async Text {
  switch (await Todo.markDoneResult(id)) {
    case (#err(#notFound)) {
      "There is no Todo with that ID."
    };
    case (#err(#alreadyDone(at))) {
      let doneAgo = secondsBetween(at, Time.now());
      "You've already completed this todo " # Int.toText(doneAgo) # " seconds ago."
    };
    case (#ok(seconds)) {
      "Congrats! That took " # Int.toText(seconds) # " seconds."
    };
  };
};
// end::result-caller[]
// tag::exception-caller[]
public shared func doneTodo4(id : Todo.TodoId) : async Text {
  try {
    let seconds = await Todo.markDoneException(id);
    "Congrats! That took " # Int.toText(seconds) # " seconds.";
  } catch (e) {
    "Something went wrong.";
  }
};
// end::exception-caller[]
};
