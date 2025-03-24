// @verify

import Prim "mo:⛔";
import Array "mo:base/Array";

actor Assistant {
  type ToDo =  { id: Nat; desc: Text; state: State };
  public type State = { #TODO; #DONE };

  var todos : [var ToDo] = [var ];
  var num : Nat = 0;
  var nextId : Nat = 1;

  assert:invariant 0 <= num and num <= todos.size();
  assert:invariant Prim.forall<Nat>(func i = (0 <= i and i < num) implies todos[i].id < nextId);

  private func resize(n: Nat) {
    // Actor's invariant is preserved:
    assert:func 0 <= num and num <= todos.size();
    assert:return 0 <= num and num <= todos.size();
    assert:func Prim.forall<Nat>(func i = (0 <= i and i < num) implies todos[i].id < nextId);
    assert:return Prim.forall<Nat>(func i = (0 <= i and i < num) implies todos[i].id < nextId);
    // unchanged fields:
    assert:return num == (old(num)) and nextId == (old(nextId));
    // functional specification:
    assert:return todos.size() >= n;
    assert:return (old(todos.size())) <= todos.size();
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    if (n <= todos.size())
      return;
    let new_array = Array.init<ToDo>(n, { id = 0; desc = ""; state = #TODO });
    var i: Nat = 0;
    while (i < todos.size()) {
      // actor invariant:
      assert:loop:invariant 0 <= num and num <= todos.size();
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < num) implies todos[ii].id < nextId);
      // unchanged fields:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant 0 <= i and i <= todos.size();
      // functional specification:
      assert:loop:invariant todos.size() < new_array.size();
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < (old(todos.size())) implies todos[ii] == (old(todos[ii]))));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i                 implies todos[ii] == new_array[ii]));
      new_array[i] := todos[i];
      i += 1;
    };
    todos := new_array;
  };

  public query func getTodos() : async [ToDo] {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    // TODO: Array.tabulate is not supported yet, do it manually (as in reverse.mo)
    // Alternative: support Array.freeze
    // assert:return Prim.forall<Nat>(func i =
    //   (0 <= i and i < (old(todos.size())) implies todos[i] == Prim.Ret<[ToDo]>()[i]));
    // let new_array = Array.tabulate<(Nat, Text, State)>(num, func i = todos[i]);
    let new_array : [ToDo] = [ { id = 0; desc = ""; state = #TODO } ];
    return new_array;
  };

  public query func getTodo(id : Nat): async (?ToDo, Nat) {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    assert:return (Prim.exists<Nat>(func i = (0 <= i and i < num and todos[i].id == id))
                   implies
                   Prim.exists<Nat>(func i = (0 <= i and i < num and ?todos[i] == Prim.Ret<(?ToDo, Nat)>().0)));
    assert:return (Prim.Ret<(?ToDo, Nat)>().0 == null) == (Prim.forall<Nat>(func i = (0 <= i and i < num implies todos[i].id != id)));
    assert:return (Prim.Ret<(?ToDo, Nat)>().0 != null) implies 0 <= Prim.Ret<(?ToDo, Nat)>().1 and Prim.Ret<(?ToDo, Nat)>().1 < todos.size();
    assert:return (Prim.Ret<(?ToDo, Nat)>().0 != null) implies (Prim.Ret<(?ToDo, Nat)>().0 == ?(todos[Prim.Ret<(?ToDo, Nat)>().1]));
    var i : Nat = 0;
    var res : ?ToDo = null;
    label l while (i < num) {
      // actor invariant:
      assert:loop:invariant 0 <= num and num <= todos.size();
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < num) implies todos[ii].id < nextId);
      // fields unchanged:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      // functional specification:
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < (old(todos.size())) implies todos[ii] == (old(todos[ii]))));
      assert:loop:invariant 0 <= i and i <= num;
      assert:loop:invariant (Prim.forall<Nat>(func j = (0 <= j and j < i implies todos[j].id != id))) == (res == null);
      assert:loop:invariant (res != null) implies (res == ?(todos[i]));
      if (todos[i].id == id) {
        res := ?todos[i];
        break l;
      };
    };
    return (res, i);
  };

  // Returns the ID that was given to the ToDo item
  public func addTodo(description : Text) : async Nat {
    assert:return 0 <= num and num <= todos.size(); // actor invariant (rise it earler)
    assert:return num == (old(num)) + 1;
    assert:return nextId == (old(nextId)) + 1;
    assert:return Prim.Ret<Nat>() == (old(nextId));
    assert:return todos[num-1] == ({ id = Prim.Ret<Nat>(); desc = description; state = #TODO });
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < num-1 implies todos[i] == (old(todos[i]))));
    let id = nextId;
    if (num >= todos.size()) {
      resize(num * 2+1);
    };
    todos[num] := { id = id; desc = description; state = #TODO };
    num += 1;
    nextId += 1;
    return id;
  };

  public func completeTodo(id : Nat) : async () {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      ((0 <= i and i < num and todos[i].id != id) implies todos[i] == (old(todos[i])) ));
    assert:return Prim.forall<Nat>(func i =
      ((0 <= i and i < num and todos[i].id == id) implies todos[i].state == #DONE ));
    var i : Nat = 0;
    while (i < num) {
      // actor invariant
      assert:loop:invariant 0 <= num and num <= todos.size();
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < num) implies todos[ii].id < nextId);
      // functional specifications:
      assert:loop:invariant 0 <= i and i <= todos.size();
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (i <= ii and ii < todos.size() implies todos[ii] == (old(todos[ii]))));

      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i and todos[ii].id != id implies todos[ii] == (old(todos[ii]))));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i and todos[ii].id == id implies todos[ii].state == #DONE));
      // let (taskId, taskDesc, _) = todos[i]; // TODO: requires recursive patterns
      switch (todos[i]) {
        case({ id = taskId; desc = taskDesc; /* _ */state = (_state: State) }) {
          if (taskId == id) {
            todos[i] := { id = taskId; desc = taskDesc; state = #DONE };
          };
          i += 1;
        };
      };
    }
  };

  public query func showTodos() : async Text {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      ((0 <= i and i < num) implies todos[i] == (old(todos[i])) ));
    var output : Text = "\n___TO-DOs___";
    var i : Nat = 0;
    while (i < num) {
      // actor invariant
      assert:loop:invariant 0 <= num and num <= todos.size();
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < num) implies todos[ii].id < nextId);
      // unchanged fields:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant todos.size() == (old(todos.size()));
      // NOTE: translation is not handling shadowing here, so `func i` causes an error here
      assert:loop:invariant Prim.forall<Nat>(func ii =
        ((0 <= ii and ii < num) implies todos[ii] == (old(todos[ii])) ));
      let todo: ToDo = todos[i];
      output := output # "\n" # todo.desc;
      switch (todo.state) {
       case (#DONE) { output := output # " ✔"; };
       case (#TODO) { output := output # " ❌"; };
      }
    };
    return output # "\n";
  };

  public func clearCompleted() : async () {
    assert:return num <= (old(num));
    assert:return nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(num)) and (old(todos[i].state)) == #TODO
      implies Prim.exists<Nat> (func k =
            ( 0 <= k and k < todos.size() and todos[k] == (old(todos[i])) )) ));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < num implies todos[i].state == #TODO));
    let new_array = Array.init<ToDo>(todos.size(), { id = 0; desc = ""; state = #TODO });
    var i: Nat = 0;
    var j: Nat = 0;
    while (i < num) {
      // actor invariant
      assert:loop:invariant 0 <= num and num <= todos.size();
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < num) implies todos[ii].id < nextId);
      // unchanged fields:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < (old(todos.size())) implies todos[ii] == (old(todos[ii]))));
      // functional specification:
      assert:loop:invariant num <= new_array.size();
      assert:loop:invariant 0 <= i and i <= num;
      assert:loop:invariant j <= i;
      assert:loop:invariant 0 <= j and j <= num;
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i and todos[ii].state == #TODO
         implies Prim.exists<Nat>(func k =
                  (0 <= k and k < j and new_array[k] == todos[ii] ))));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < j implies new_array[ii].state == #TODO ));
      assert:loop:invariant Prim.forall<Nat>(func ii = (0 <= ii and ii < j) implies new_array[ii].id < nextId);
      if (todos[i].state == #TODO) {
        new_array[j] := todos[i];
        j += 1;
      };
      i += 1;
    };
    todos := new_array;
    num := j;
  };
}
