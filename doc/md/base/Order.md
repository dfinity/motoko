# Order
Order

## Type `Order`
`type Order = {#less; #equal; #greater}`

A type to represent an order.

## Function `isLess`
`func isLess(order : Order) : Bool`

Check if an order is #less.

## Function `isEqual`
`func isEqual(order : Order) : Bool`

Check if an order is #equal.

## Function `isGreater`
`func isGreater(order : Order) : Bool`

Check if an order is #greater.

## Function `equal`
`func equal(o1 : Order, o2 : Order) : Bool`

Returns true if only if  `o1` and `o2` are the same ordering.
