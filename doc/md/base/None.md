# None
The absent value

The `None` type represents a type with _no_ value.

It is often used to type code that fails to return control (e.g. an infinite loop)
or to designate impossible values (e.g. the type `?None` only contains `null`).

## Type `None`
`type None = Prim.Types.None`

The empty type. A subtype of all types.

## Value `impossible`
`let impossible : <A>None -> A`

Turns an absurd value into an arbitrary type.
