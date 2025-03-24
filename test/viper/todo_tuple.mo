// @verify

import Prim "mo:⛔";
import Array "mo:base/Array";

actor Assistant {
  // type ToDo = (Nat, Text, State); // (id, desc, completed)
  public type State = { #TODO; #DONE };

  var todos : [var (Nat, Text, State)] = [var ];
  var num : Nat = 0;
  var nextId : Nat = 1;

  assert:invariant 0 <= num and num <= todos.size();

  private func resize(n: Nat) {
    // Actor's invariant is preserved:
    assert:func 0 <= num and num <= todos.size();
    assert:return 0 <= num and num <= todos.size();
    // unchanged fields:
    assert:return num == (old(num)) and nextId == (old(nextId));
    // functional specification:
    assert:return todos.size() >= n;
    assert:return (old(todos.size())) <= todos.size();
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    if (n <= todos.size())
      return;
    let new_array = Array.init<(Nat, Text, State)>(n, (0, "", #TODO));
    var i: Nat = 0;
    while (i < todos.size()) {
      // actor invariant:
      assert:loop:invariant 0 <= num and num <= todos.size();
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

  public query func getTodos() : async [(Nat, Text, State)] {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    // TODO: is not supported yet, do it manually (as in reverse.mo)
    // assert:return Prim.forall<Nat>(func i =
    //   (0 <= i and i < (old(todos.size())) implies todos[i] == Prim.Ret<[Nat, Text, State]>()[i]));
    // let new_array = Array.tabulate<(Nat, Text, State)>(num, func i = todos[i]);
    let new_array : [(Nat, Text, State)] = [(0, "", #TODO)];
    return new_array;
  };

  public query func getTodo(id : Nat): async ?(Nat, Text, State) {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < (old(todos.size())) implies todos[i] == (old(todos[i]))));
    assert:return (Prim.exists<Nat>(func i = (0 <= i and i < num and todos[i].0 == id))
                   implies
                   Prim.exists<Nat>(func i = (0 <= i and i < num and ?todos[i] == Prim.Ret<?(Nat, Text, State)>())));
    var i : Nat = 0;
    var res : ?(Nat, Text, State) = null;
    label l while (i < num) {
      // actor invariant:
      assert:loop:invariant 0 <= num and num <= todos.size();
      // fields unchanged:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      // functional specification:
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < (old(todos.size())) implies todos[ii] == (old(todos[ii]))));
      assert:loop:invariant 0 <= i and i <= num;
      if (todos[i].0 == id) {
        res := ?todos[i];
        break l;
      };
    };
    return res;
  };

  // Returns the ID that was given to the ToDo item
  public func addTodo(description : Text) : async Nat {
    assert:return 0 <= num and num <= todos.size(); // actor invariant (rise it earler)
    assert:return num == (old(num)) + 1;
    assert:return nextId == (old(nextId)) + 1;
    assert:return Prim.Ret<Nat>() == (old(nextId));
    assert:return todos[num-1] == (Prim.Ret<Nat>(), description, #TODO);
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i+1 < num implies todos[i] == (old(todos[i]))));
    let id = nextId;
    if (num >= todos.size()) {
      resize(num * 2+1);
    };
    todos[num] := (id, description, #TODO);
    num += 1;
    nextId += 1;
    return id;
  };

  public func completeTodo(id : Nat) : async () {
    assert:return num == (old(num)) and nextId == (old(nextId));
    assert:return todos.size() == (old(todos.size()));
    assert:return Prim.forall<Nat>(func i =
      ((0 <= i and i < num and todos[i].0 != id) implies todos[i] == (old(todos[i])) ));
    assert:return Prim.forall<Nat>(func i =
      ((0 <= i and i < num and todos[i].0 == id) implies todos[i].2 == #DONE ));
    var i : Nat = 0;
    while (i < num) {
      // actor invariant
      assert:loop:invariant 0 <= num and num <= todos.size();
      // functional specifications:
      assert:loop:invariant 0 <= i and i <= todos.size();
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant todos.size() == (old(todos.size()));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (i <= ii and ii < todos.size() implies todos[ii] == (old(todos[ii]))));

      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i and todos[ii].0 != id implies todos[ii] == (old(todos[ii]))));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < i and todos[ii].0 == id implies todos[ii].2 == #DONE));
      // let (taskId, taskDesc, _) = todos[i]; // TODO: requires recursive patterns
      switch (todos[i]) {
        case(taskId, taskDesc, /* _ */_completed: State) {
          if (taskId == id) {
            todos[i] := (taskId, taskDesc, #DONE);
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
      // unchanged fields:
      assert:loop:invariant num == (old(num)) and nextId == (old(nextId));
      assert:loop:invariant todos.size() == (old(todos.size()));
      // NOTE: translation is not handling shadowing here, so `func i` causes an error here
      assert:loop:invariant Prim.forall<Nat>(func ii =
        ((0 <= ii and ii < num) implies todos[ii] == (old(todos[ii])) ));
      let todo: (Nat, Text, State) = todos[i];
      output := output # "\n" # todo.1;
      switch (todo.2) {
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
      (0 <= i and i < (old(num)) and (old(todos[i].2)) == #TODO
      implies Prim.exists<Nat> (func k =
            ( 0 <= k and k < todos.size() and todos[k] == (old(todos[i])) )) ));
    assert:return Prim.forall<Nat>(func i =
      (0 <= i and i < num implies todos[i].2 == #TODO));
    let new_array = Array.init<(Nat, Text, State)>(todos.size(), (0, "", #TODO));
    var i: Nat = 0;
    var j: Nat = 0;
    while (i < num) {
      // actor invariant
      assert:loop:invariant 0 <= num and num <= todos.size();
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
        (0 <= ii and ii < i and todos[ii].2 == #TODO
         implies Prim.exists<Nat>(func k =
                  (0 <= k and k < j and new_array[k] == todos[ii] ))));
      assert:loop:invariant Prim.forall<Nat>(func ii =
        (0 <= ii and ii < j implies new_array[ii].2 == #TODO ));
      if (todos[i].2 == #TODO) {
        new_array[j] := todos[i];
        j += 1;
      };
      i += 1;
    };
    todos := new_array;
    num := j;
  };
}
