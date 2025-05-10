---
sidebar_position: 5
---

# Records

Records let you group related values using named fields. Each field can have a different type. Unlike [tuples](https://internetcomputer.org/docs/motoko/fundamentals/types/tuples), which rely on positional access, records use field names for retrieval, improving maintainability. Records also support mutable fields, declared using the `var` keyword.  The fields of a tuple are always immutable.

## Defining a record

```motoko
let person = {
   name : Text = "Motoko";
   age : Nat = 25;
};
```

:::info Inferred types
Type annotations on immutable record fields aren’t necessary as the compiler will infer them.
However, for mutable fields (`var`), it’s good practice to explicitly declare the type. Otherwise, the compiler might infer a more specific type than intended, preventing future assignments of a broader type.

Example:

```motoko
let account = { name = "Motoko"; var balance = 0 };
// Inferred as { name : Text; var balance : Nat }
account.balance := -100; //  Rejected, -100 is not a Nat
````

To avoid this issue, annotate the field explicitly:

```motoko
let account = { name = "Motoko"; var balance : Int = 0 };
account.balance := -100; //  Allowed
```

This recommendation also applies to all `var` declarations—not just record fields.
:::

`person` is a record with two labeled fields, `name` of type [`Text`](https://internetcomputer.org/docs/motoko/base/Text) and `age` of type [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat).

The values of the fields are `"Motoko"` and `25` respectively.  

The type of the `persion` value is `{age : Nat; name : Text}`, or, equivalently,  `{name: Text; age : Nat}`.  
Unlike tuples, the order of record fields is immaterial and all record types with the same field names and types are considered equivalent, regardless of field ordering. 

## Accessing fields

Fields in a record can be accessed using the dot (`.`) notation.

```motoko
let person = {
   name : Text = "Motoko";
   age : Nat = 25;
  };

let personName = person.name;  // "Motoko"
let personAge = person.age;    // 25

person;
```

Attempting to access a field that isn't available in the type of the record is a compile-time type error.  

## Record mutability

By default, record fields are immutable. To create a mutable field, use `var`.

```motoko
let person = {
    var name : Text = "Motoko";
    var age : Nat = 25;
};
```

This `person` has type `{var age : Nat; var name : Text}`.  

`var name` and `var age` allow the values to be updated later.

```motoko
let person = {
   var name : Text = "Motoko";
   var age : Nat = 25;
  };

person.name := "Ghost";  // Now person.name is "Ghost"
person.age := 30;      // Now person.age is 30

person
```

Attempting to update an immutable field is a compile-time type error.

## Nested records

Records can contain other records, allowing for hierarchical data structures that maintain organization while ensuring type safety and clarity.

```motoko no-repl
type Address = {
    city : Text;
    street : Text;
    zip : Nat;
};

type Individual = {
    name : Text;
    address : Address;
};

let individual : Individual = {
    name = "Diana";
    address = { street = "101 Broadway"; city = "New York"; zip = 10001 };
};
```

## Pattern matching on records

Records can be destructured using [`switch`](https://internetcomputer.org/docs/motoko/fundamentals/control-flow/switch), allowing selective extraction of fields. This approach makes accessing deeply nested fields more explicit and readable.

```motoko no-repl
type Address = {
    city : Text;
    street : Text;
    zip : Nat;
};

type Individual = {
    name : Text;
    address : Address;
};

let individual : Individual = {
    name = "Diana";
    address = { street = "101 Broadway"; city = "New York"; zip = 10001 };
};

let { name; address = { city = cityName }} = individual;  
```

The pattern both defines `name` as the contents of eponymous field `individual.name` and `cityName` as the contents of field `individual.address.city`.  Irrelevant fields need not be listed in the pattern.  
The field pattern `name` is just shorthand for `name = name`: it binds the contents of the field called `name`, to the variable called `name`.  

## Using records in collections

Records are commonly used in arrays and other collections for structured data storage, allowing efficient  data organization and retrieval.

```motoko no-repl
type Product = {
    name : Text;
    price : Float;
};

let inventory : [Product] = [
    { name = "Laptop"; price = 999.99 },
    { name = "Smartphone"; price = 599.99 }
];

let firstProduct : Product = inventory[0];
let productName : Text = firstProduct.name;
```

## Updating records programmatically

Since records are immutable by default, updating a record requires creating a modified copy. Motoko allows combining and extending records using the `and` and `with` keywords.

### Merging records with `and`

The `and` keyword merges multiple records when they have no conflicting fields.

```motoko no-repl
let contact = { email : Text = "motoko@example.com"; };
let person = { name : Text = "Motoko"; age : Nat =  25; };

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
let contact = {email : Text = "motoko@example.com"};

// profile and contact merge with and since they have unique fields.
// age = 26; updates the age field from profile.
// location = "New York"; adds a new field.
let fullProfile = {
  person and contact with
  age = 26;
  location : Text = "New York";
};
Debug.print(debug_show (fullProfile));
```

## Tuples vs records

Tuples and records both allow grouping values, but they have key differences in structure, mutability, and field access. While tuples provide a compact way to group values, records offer more flexibility for structured data modeling, especially when dealing with complex relationships or named fields.

| Feature         | Tuple                                     | Record                                |
|----------------|------------------------------------------|----------------------------------------|
| Structure      | Ordered collection of values             | Unordered collection of named fields  |
| Projection     | By position (`.n`)                       | By field name (`.fieldName`)          |
| Pattern matching| Complete, using ordered tuple patterns                       | Selective, using unordered record patterns  |  
| Mutability     | Immutable after creation                 | Can have mutable fields               |
| Naming         | Fields are anonymous  | Fields are named               |  
| Subtyping     | Fields cannot be removed | Fields can be removed |
| Use Case       | Positional grouping of related values, e.g. vectors     |  structured data types        |

Motoko's records afford more flexible subtyping than tuples. Subtyping on records allows fields to be removed in the subtype, while for tuples, subtyping always requires the length of the tuples to be equal.

For example,, `{x : Int, y : Int, z : Int}` is a subtype of `{x : Int, y : Int}`, but `(Int, Int, Int)` is not a subtype of `(Int, Int)`.  

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />