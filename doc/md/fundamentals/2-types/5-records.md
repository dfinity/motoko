---
sidebar_position: 5
---

# Records

Records provide a structured way to group related values using named fields. Unlike [tuples](https://internetcomputer.org/docs/motoko/fundamentals/types/tuples), which rely on positional access, records use field names for retrieval, improving clarity and maintainability.

## Defining a record

```motoko
let person = {
   name : Text = "Motoko";
   age : Nat = 25;
  };
```

`person` is a record with two labeled fields, `name` of type [`Text`](https://internetcomputer.org/docs/motoko/base/Text) and `age` of type [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat).

The values of the fields are `"Motoko"` and `25` respectively.

## Accessing fields

Fields in a record can be accessed using the dot (`.`) notation.

```motoko no-repl
let person = {
   name : Text = "Motoko";
   age : Nat = 25;
  };

let personName = person.name;  // "Motoko"
let personAge = person.age;    // 25

Debug.print(personName # is # debug_show(personAge));
```

If a field does not exist in the record, the program traps.

## Record mutability

By default, record fields are immutable. To create a mutable field, use `var`.

```motoko
let person = {
    var name: Text = "Motoko";
    var age: Nat = 25;
};
```

`var name` and `var age` allow the values to be updated later.

```motoko no-repl
let person = {
   var name : Text = "Motoko";
   var age : Nat = 25;
  };

person.name := "Ghost";  // Now person.name is "Ghost"
person.age := 30;      // Now person.age is 30

Debug.print(person.name # is # debug_show(person.age));
```

Attempting to update an immutable field will raise an error.

## Nested records

Records can contain other records, allowing for hierarchical data structures that maintain organization while ensuring type safety and clarity.

```motoko no-repl
type Address = {
    city: Text;
    street: Text;
    zip: Nat;
};

type Individual = {
    name: Text;
    address: Address;
};

let individual: Individual = {
    name = "Diana";
    address = { street = "101 Broadway"; city = "New York"; zip = 10001 };
};
```

## Pattern matching on records

Records can be destructured using [`switch`](https://internetcomputer.org/docs/motoko/fundamentals/control-flow/switch), allowing selective extraction of fields. This approach makes accessing deeply nested fields more explicit and readable.


```motoko no-repl
type Address = {
    city: Text;
    street: Text;
    zip: Nat;
};

type Individual = {
    name: Text;
    address: Address;
};

let individual: Individual = {
    name = "Diana";
    address = { street = "101 Broadway"; city = "New York"; zip = 10001 };
};

let cityName: Text = switch (individual) {
    case ({ address = { city } }) city; //New York
};
```

## Using records in collections

Records are commonly used in arrays and other collections for structured data storage, allowing efficient  data organization and retrieval.

```motoko no-repl
type Product = {
    name: Text;
    price: Float;
};

let inventory: [Product] = [
    { name = "Laptop"; price = 999.99 },
    { name = "Smartphone"; price = 599.99 }
];

let firstProduct: Product = inventory[0];
let productName: Text = firstProduct.name;
```


## Updating records programmatically

Since records are immutable by default, updating a record requires creating a modified copy. Motoko allows combining and extending records using the `and` and `with` keywords.

### Merging records with `and`

The `and` keyword merges multiple records when they have no conflicting fields.

```motoko no-repl
let contact = { email: Text = "alice@example.com"; };
let person = { name: Text = "Motoko"; age: Nat =  25; };

let profile = { person and contact };

Debug.print(debug_show (profile));
```

`profile` combines `person` and `contact` because they have unique fields. If any field name overlaps, `and` alone is not allowed. Use  the `with` keyword to resolve conflicts.

### Overriding and extending records using `with`

The `with` keyword modifies, overrides, or adds fields when combining records.

```motoko no-repl
let person = { name : Text = "Motoko"; age : Nat =  25; };
// age = 26; updates the existing age field.
// city = "New York" adds a new field to the record.
// city = "New York" adds a new field to the record.
let updatedPerson = { person with age : Nat = 26; city : Text = "New York"; };

Debug.print(debug_show (updatedPerson));
```

:::info

If `person` contained a mutable (`var`) field, `with` must redefine it, preventing aliasing.

:::

### Combining `and` and `with`

```motoko no-repl
let person = {name : Text = "Motoko"; age : Nat = 25};
let contact = {email : Text = "alice@example.com"};

// profile and contact merge with and since they have unique fields.
// age = 26; updates the age field from profile.
// location = "New York"; adds a new field.
let fullProfile = {person and contact with age = 26; location : Text = "New York"};
Debug.print(debug_show (fullProfile));
```

## Tuples vs records

Tuples and records both allow grouping values, but they have key differences in structure, mutability, and field access. While tuples provide a compact way to group values, records offer more flexibility for structured data modeling, especially when dealing with complex relationships or named fields.

| Feature         | Tuple                                     | Record                                |
|----------------|------------------------------------------|----------------------------------------|
| Structure      | Ordered collection of values             | Unordered collection of named fields  |
| Projection     | By position (`.n`)                       | By field name (`.fieldName`)          |
| Mutability     | Immutable after creation                 | Can have mutable fields               |
| Naming         | Elements can be named within collections | Fields are always named               |
| Use Case       | Temporary grouping of related values     | Defining structured data types        |

### Example of a record

```motoko no-repl
type User = {
    name: Text;
    age: Nat;
};

let user: User = { name = "Motoko"; age = 25 };

let username: Text = user.name;
```

### Example of a tuple

```motoko no-repl
let user: (Text, Nat) = ("Motoko", 25);

let username: Text = switch (user) {
    case (name, _) name;
};
```


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />