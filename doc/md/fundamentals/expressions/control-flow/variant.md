---
sidebar_position: 13
---

# Variants

A variant is a type that can hold one of multiple possible values, each labeled with a distinct tag. Unlike records, which contain multiple fields at once, a variant only holds a single field at a time.

## Defining a variant

A variant type consists of tags, each representing a possible value. Each tag can have an associated type or be a unit tag (no associated value). Variants are especially useful for defining recursive data structures, such as a binary tree.

```motoko
type Status = {
    #Active;
    #Inactive;
    #Banned : Text;
};
```

`#Active` and `#Inactive` are unit tags, meaning they do not store any extra data `()`. `#Banned` carries a `Text` value, such as the reason for banning.

## Assigning variants

To assign a variant value, use one of the defined tags:

```motoko
let activeUser: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");
```

## Accessing a variant's value

To work with a variant, use a `switch` expression to match each possible case:

```motoko
let activeUser: Status = #Active;
let bannedUser: Status = #Banned("Violation of rules");

func getStatusMessage(status: Status) : Text {
    switch (status) {
        case (#Active) "User is active";
        case (#Inactive) "User is inactive";
        case (#Banned(reason)) "User is banned: " # reason;
    }
}

Debug.print(getStatusMessage(activeUser));
Debug.print(getStatusMessage(bannedUser));
```

`switch` ensures every possible variant is handled safely. The `#Banned` case extracts the `reason` stored inside.
