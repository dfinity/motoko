# Order
Order

## Type `Order`
``` motoko no-repl
type Order = {#less; #equal; #greater}
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
func equal(o1 : Order, o2 : Order) : Bool
```

Returns true if only if  `o1` and `o2` are the same ordering.
