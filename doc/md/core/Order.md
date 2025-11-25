# core/Order
Utilities for `Order` (comparison between two values).

## Type `Order`
``` motoko no-repl
type Order = Types.Order
```

A type to represent an order.

## Function `isLess`
``` motoko no-repl
func isLess(order : Order) : Bool
```

Check if an order is #less.

## Function `isEqual`
``` motoko no-repl
func isEqual(order : Order) : Bool
```

Check if an order is #equal.

## Function `isGreater`
``` motoko no-repl
func isGreater(order : Order) : Bool
```

Check if an order is #greater.

## Function `equal`
``` motoko no-repl
func equal(order1 : Order, order2 : Order) : Bool
```

Returns true if only if  `order1` and `order2` are the same.

## Function `allValues`
``` motoko no-repl
func allValues() : Types.Iter<Order>
```

Returns an iterator that yields all possible `Order` values:
`#less`, `#equal`, `#greater`.
