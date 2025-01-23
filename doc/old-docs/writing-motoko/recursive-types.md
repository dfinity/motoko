---
sidebar_position: 29
---

# Recursive types



A recursive type is a type that contains the values of the same type. Recursive types enable you to create complex recursive data structures, such as linked lists or trees.

Motoko supports linked lists, a data structure that is an example of a recursive type.

## Recursive lists

``` motoko no-repl
type List<T> = ?(T, List<T>);
```

In this example, the generic type `List<T>` is defined with one type parameter. `List<T>` is a tuple with two components: the first component is the type parameter `T` , and the second component is `List<T>`. The second component is the recursive type, as the generic type `List<T>` contains a value of itself within its own definition.

`List` is a repeating pattern, where each repeated component is a tuple that contains a value of type `T` and a reference to the tail of `List<T>`.

## Recursive functions

A recursive function can be used to retrieve the last element of a given list:

```motoko no-repl
func last<T>(l : List<T>) : ?T {
  switch l {
    case null { null };
    case (?(x, null)) { ?x };
    case (?(_, t)) { last<T>(t) };
  };
};
```

This generic function `last<T>` takes one argument `l` of type `List<T>`, which refers to the head of a list. If this function returns the last element of a list, it returns an optional value `?T`. If there isn't a last element, it will return `null`. The body of the function uses a `switch` statement to determine if the list passed as an argument is an empty list, the last element of a list, or if it is the tail of a list with a next value.

In this switch statement, the `last<T>` function is used recursively, since it is called within itself with `t` as the argument. The function is called again each time the case statement is satisfied, and the function receives a list head that it can switch on until the last element is returned.

:::info
Note that you will need to use recursive functions to access all data in a recursive type.
:::

## Resources

- [Recursive types](https://github.com/Web3NL/motoko-book/blob/main/src/advanced-types/recursive-types.md).
