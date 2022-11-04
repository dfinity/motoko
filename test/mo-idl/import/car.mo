// Stateless Suspendable Workflow
// ==============================

// Creating an Account for the PA parking service

// - Client: Insert Car data and holder's name
// - Server: Validate car is registered under the given name
// - Client: Pick a parking spot from a Google Map like interface + time
// - Server: Register the parking spot for the user
module {
  public type Car = { model : Text; plate : Text };
  public type DMV = actor { check : Car -> async CarInfo };
  public type CarInfo = {
    model : Text;
    plate : Text;
    isValid : Bool;
    wasStolen : Bool;
    expires : Nat;
  };
}
