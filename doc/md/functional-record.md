# Creating new records (objects) from old ones

Often times the need arises to build objects/records from exististing ones by combining them (possibly with modified filed contents), or simply adding new fields. _Functional record updates_ fill in this use case.

To give an example, database-like canisters frequently partition data into separate tables (e.g. to improve sharing), but intend to present the data in a consolidated (flattened) manner to the user. Thus the query in the Motoko source might look like this:


``` motoko
type Product = { id : Blob; description : Text; dose : Nat, drug : Text };
type Producer = { ... };
type Retrieved = { version : Nat; time : Timestamp };

public query func medicineItem(id : Blob) : async (Product and Producer and Retrieved) {
  let products : [Product and { producer : Producer }] = tables.products;
  let product = findProductById(products, id);
  let producer = findProducerById(tables.producers, product.producer);

  { product and producer with version = tables.version; time = Time.now() }
};
```

Here we have a sketch of a `query` that internally first obtains a product by Id and then proceeds to obtain the producer by its foreign key.
Finally the relevant information is joined to a monolithic record, adding two dynamic fields in the process.

Let's dissect the syntax for building the final record. The part following the keyword `with` is familiar from the way how we build records from individual fields alone. Before the `with` comes a non-empty list of record-typed expressions joined by the `and` keyword.

Note that we have an analogous joining of record types with the (type-level) `and`, and they similarly ease the modularisation of the type
annotations.

The sequence of field definitions following `with` also serves as a way to disambiguate possible field name conflicts that can arise from
the records being merged.

In summary, the `and-with` syntax for record formation is just a compact way of spelling out each field's value in turn.

## Contrasting with mutation of `var` fields

Note that this is something entirely different than modifying a pre-existing record with a `var`-field in-place. Destructively
modifying `var` fields will preserve the object's identity, i.e. holders of a reference to the object/record will be able to observe
the change if the field's value (e.g. by comparison with a previously saved value). Functional record updates OTOH are working with immutable
data and leave the inputs unchanged, while creating a new identity.

## Experimental aliasing and prototype objects

There is an experimental feature in the mix that we should mention for the sake of completeness. When invoking the compiler with the flag
`--experimental_field_aliasing`, additionally `var` fields can be contained in the records that are joined. The identities of the mutable
cells that hold the vales are preserved in the process, so that possible methods along them keep referencing those.

This (extended) semantics allows to upgrade objects with new methods without rendering the old methods useless:

``` motoko
TBW
```

## Dropping fields

You might wonder how specific fields can be dropped. Due to the fact that records with added fields become subtypes, explicitly annotating a
record with a supertype (that doesn't mention a specific field) will remove access to undesired fields.

## Further reading

See [here](language-manual.md#object-combination-extension) for more details.
