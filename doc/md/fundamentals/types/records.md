---
sidebar_position: 4
---

# Records  

Records provide a structured way to group related values using named fields. Unlike tuples, which rely on positional access, records use field names for retrieval, improving clarity and maintainability.

## Nested records

Records can contain other records, allowing for hierarchical data structures.  

```motoko norepl
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

This structure maintains organization while ensuring type safety and clarity.

## Pattern matching on records

Records can be destructured using `switch`, allowing selective extraction of fields.

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

This approach makes accessing deeply nested fields more explicit and readable.

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


## Updating records programatically

Since records are immutable by default, updating a record requires creating a modified copy.

```motoko no-repl
let person: Individual = {
    name = "Eve";
    address = { street = "5th Avenue"; city = "Chicago"; zip = 60601 };
};

let updatedPerson: Individual = { person with name = "Eva" };
```

Using `with` ensures immutability while allowing modifications.

## Tuples vs records

Tuples and records both allow grouping values, but they have key differences in structure, mutability, and field access.

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

let user: User = { name = "Alice"; age = 25 };

let username: Text = user.name;
```

### Example of a tuple  

```motoko no-repl
let user: (Text, Nat) = ("Alice", 25);

let username: Text = switch (user) {
    case (name, _) name;
};
```

While tuples provide a compact way to group values, records offer more flexibility for structured data modeling, especially when dealing with complex relationships or named fields.
