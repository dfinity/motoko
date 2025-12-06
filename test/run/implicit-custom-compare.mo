import Text "../core-stub/src/Text";
import Nat "../core-stub/src/Nat";
import Map "../core-stub/src/Map";
import { type Order } "../core-stub/src/Types";

actor {
  type User = { name : Text };
  type Invoice = { id : Nat };

  func implicit(compare) compareUser(self : User, other : User) : Order {
    Text.compare(self.name, other.name)
  };

  func implicit(compare) compareInvoice(self : Invoice, other : Invoice) : Order {
    Nat.compare(self.id, other.id)
  };

  let users = Map.empty<User, ()>();
  let invoices = Map.empty<Invoice, ()>();

  users.add({ name = "foo" }, ());
  invoices.add({ id = 10 }, ());
}

//SKIP comp
