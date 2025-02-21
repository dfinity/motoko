---
sidebar_position: 12
---

# Records

A record is a collection of labeled fields, where each field has a name, type and a value. Records allow grouping related data into a structured format, making it easier to manage and access.

## Defining a record

```motoko
let person = {
   name : Text = "Alice";
   age : Nat = 25;
  };
```

`person` is a record with two labeled fields:

1. `name` of type `Text`.
2. `age` of type `Nat`.

The values of the fields are `"Alice"` and `25`.

## Accessing fields

Fields in a record can be accessed using the dot (`.`) notation:

```motoko
let person = {
   name : Text = "Alice";
   age : Nat = 25;
  };

let personName = person.name;  // "Alice"
let personAge = person.age;    // 25

Debug.print(personName # is # debug_show(personAge));
```

If a field does not exist in the record, the program traps.

## Record mutability

By default, record fields are immutable. To create a mutable field, use `var`:

```motoko
let person = {
    var name: Text = "Alice";
    var age: Nat = 25;
};
```

`var name` and `var age` allow the values to be updated later.  

```motoko
let person = {
   var name : Text = "Alice";
   var age : Nat = 25;
  };

person.name := "Bob";  // Now person.name is "Bob"
person.age := 30;      // Now person.age is 30

Debug.print(person.name # is # debug_show(person.age));
```

Attempting to update an immutable field will raise an error.

## Merging and modifying fields

Motoko allows combining and extending records using the `and` and `with` keywords.

### Merging records with `and`

The `and` keyword merges multiple records when they have no conflicting fields:  

```motoko
let contact = { email: Text = "alice@example.com"; };
let person = { name: Text = "Alice"; age: Nat =  25; };

let profile = { person and contact };

Debug.print(debug_show (profile));
```

`profile` combines `person` and `contact` because they have unique fields. If any field name overlaps, `and` alone is not allowed. Use  the `with` keyword to resolve conflicts.

### Overriding and extending records using `with`

The `with` keyword modifies, overrides, or adds fields when combining records.  

```motoko
let person = { name : Text = "Alice"; age : Nat =  25; };
// age = 26; updates the existing age field.
// city = "New York" adds a new field to the record.
let updatedPerson = { person with age : Nat = 26; city : Text = "New York"; };

Debug.print(debug_show (updatedPerson));
```


⚠ **Note:** If `person` contained a mutable (`var`) field, `with` must redefine it, preventing aliasing.

### Combining `and` and `with`

```motoko
let person = {name : Text = "Alice"; age : Nat = 25};
let contact = {email : Text = "alice@example.com"};

// profile and contact merge with and since they have unique fields.
// age = 26; updates the age field from profile.
// location = "New York"; adds a new field.
let fullProfile = {person and contact with age = 26; location : Text = "New York"};
Debug.print(debug_show (fullProfile));
```

