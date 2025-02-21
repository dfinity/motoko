---
sidebar_position: 11
---

---
sidebar_position: 11
---

# Options

An option expression in Motoko is used to represent a value that may or may not be present. It is useful when a value could be missing, and it allows the program to handle both cases (present or absent) safely. An option can either hold a value or be `null` (indicating the absence of a value).

## Defining an option

An option is defined using `?` followed by the type of the value it can hold.

```motoko
var username: ?Text = null;
```

`username` is an optional `Text` value that starts as `null` (no username set).

## Unwapping an option

```motoko
actor App {
  var username: ?Text = ?Alice;

  pulic func getUsername() : async Text {
      switch (username) {
          case (?name) "Username: " # name; 
          case null "No username set";
      }
  }
}
await App.getUsername();
```

To unwrap an option (`?T`), both cases must be handled: 

1. When the value is present.
2. When it is absent (`null`). 

If the value exists (`?value`), it can be accessed directly as its inner type (`T`) within the `case (?value)` branch. If the value is `null`, an alternative action or default value must be provided in the `case null` branch to ensure safe execution. This prevents runtime errors and ensures that optional values are handled explicitly.
