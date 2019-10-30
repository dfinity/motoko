// Very simple To Do List app.
// ============================

// Author(s): Matthew Hammer, ...
// Inspiration from Stanley Jones.
// (Implementation notes and future directions at end)

type Item = {
  desciption: Text; 
  isDone: Bool;
};

object todoList {
  var items : [Item] = [];

  public func getItems() : [Item] = items;

  public func addItem(newItem:Item) {
    items := Array_tabulate<Item>(items.len() + 1,
      func (i:Nat) : Item {
        if ( i < items.len() ) {
          items[i]
        } else { 
          newItem
        }
      });
  };

  // mark an existing item done, based on its index.
  //
  // returns ?() on success, and null on failure
  //
  public func markDone(itemNum:Nat) : ?() {
    if ( itemNum < items.len() ) {
      items := Array_tabulate<Item>(items.len(),
        func (i:Nat) : Item {
          if ( i == itemNum ) {
            { desciption = items[i].desciption ;
              isDone     = true }
          } else { // no change to other items
            items[i]
          }
        }); 
      ?()
    } else {
      null
    }
  };   
};

//
// this service simply 
// wraps the object above with public, shared functions:
//
actor todoListService {
  public func addItem(d:Text) {
    todoList.addItem({desciption=d; isDone=false})
  };
  public func markDone(i:Nat) : async ?() {
    todoList.markDone(i)
  };
  public func getItems () : async [Item] {
    todoList.getItems()
  };
}

// Notes:

// here, we implement the todo list object
// with a single mutable variable holding an immutable array;
// this implementation is simple, but not scalable.
//
// in the future: 
// we could also try implementing a _mutable array 
// buffer_ that keeps a capacity and count as separate numbers, 
// and which uses these (distinct) numbers to avoid re-allocating 
// and copying the existing content for every new item insertion.
//
// however, even a buffer like this has some worst-case behavior
// on some operations, typically (e.g., when it needs to grow,
// which happens from time to time as we insert items).
//
// we could also use a _tree-shaped, functional_ 
// data structure that _has no worst-case behavior_ (in expectation), 
// like a functional hash trie.
// we have one of those already (see `stdlib/trie2.mo` module).
