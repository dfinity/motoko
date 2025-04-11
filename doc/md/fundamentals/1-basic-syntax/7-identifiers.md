---
sidebar_position: 7
hide_table_of_contents: true
---

# Identifiers

Identifiers are names used for variables, functions, and other entities. They must start with a letter and can contain letters, digits, and underscores.

```motoko no-repl
let name = "Motoko";
let a1 = 123;
let my_Motoko_identifier = "Hello, world!"
```

## Reserved syntax keywords

Motoko reserves certain words for its syntax and they cannot be used as identifiers. These include:

- [`actor`](https://internetcomputer.org/docs/motoko/language-manual#programs)
- [`and`](https://internetcomputer.org/docs/motoko/language-manual#and)
- [`assert`](https://internetcomputer.org/docs/motoko/language-manual#assert)
- [`async`](https://internetcomputer.org/docs/motoko/language-manual#async)
- [`async*`](https://internetcomputer.org/docs/motoko/language-manual#async-1)
- [`await`](https://internetcomputer.org/docs/motoko/language-manual#await)
- [`await*`](https://internetcomputer.org/docs/motoko/language-manual#await-1)
- [`break`](https://internetcomputer.org/docs/motoko/language-manual#break)
- [`case`](https://internetcomputer.org/docs/motoko/language-manual#switch)
- [`catch`](https://internetcomputer.org/docs/motoko/language-manual#throw)
- [`class`](https://internetcomputer.org/docs/motoko/language-manual#class-declaration)
- [`composite`](https://internetcomputer.org/docs/motoko/language-manual#function)
- [`continue`](https://internetcomputer.org/docs/motoko/language-manual#labeled-loops)
- [`debug`](https://internetcomputer.org/docs/motoko/language-manual#debug)
- [`debug_show`](https://internetcomputer.org/docs/motoko/language-manual#debug)
- [`do`](https://internetcomputer.org/docs/motoko/language-manual#do)
- [`else`](https://internetcomputer.org/docs/motoko/language-manual#let-else-declaration)
- [`false`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types)
- [`flexible`](https://internetcomputer.org/docs/motoko/language-manual#declaration-fields)
- [`finally`](https://internetcomputer.org/docs/motoko/language-manual#try)
- [`for`](https://internetcomputer.org/docs/motoko/language-manual#for)
- [`from_candid`](https://internetcomputer.org/docs/motoko/language-manual#candid-serialization)
- [`func`](https://internetcomputer.org/docs/motoko/language-manual#functions)
- [`if`](https://internetcomputer.org/docs/motoko/language-manual#if)
- [`ignore`](https://internetcomputer.org/docs/motoko/language-manual#ignore)
- [`import`](https://internetcomputer.org/docs/motoko/language-manual#imports)
- [`in`](https://internetcomputer.org/docs/motoko/language-manual)
- [`label`](https://internetcomputer.org/docs/motoko/language-manual#label)
- [`let`](https://internetcomputer.org/docs/motoko/language-manual#let-declaration)
- [`loop`](https://internetcomputer.org/docs/motoko/language-manual#loop)
- [`module`](https://internetcomputer.org/docs/motoko/language-manual#imports)
- [`not`](https://internetcomputer.org/docs/motoko/language-manual#not)
- [`null`](https://internetcomputer.org/docs/motoko/language-manual#null-break)
- [`object`](https://internetcomputer.org/docs/motoko/language-manual#object-pattern)
- [`or`](https://internetcomputer.org/docs/motoko/language-manual#or)
- [`persistent`](https://internetcomputer.org/docs/motoko/language-manual#declaration-fields)
- [`private`](https://internetcomputer.org/docs/motoko/language-manual#object-declaration)
- [`public`](https://internetcomputer.org/docs/motoko/language-manual#object-declaration)
- [`query`](https://internetcomputer.org/docs/motoko/language-manual#functions)
- [`return`](https://internetcomputer.org/docs/motoko/language-manual#return)
- [`shared`](https://internetcomputer.org/docs/motoko/language-manual#programs)
- [`stable`](https://internetcomputer.org/docs/motoko/language-manual#type-region)
- [`switch`](https://internetcomputer.org/docs/motoko/language-manual#patterns)
- [`system`](https://internetcomputer.org/docs/motoko/language-manual#type-parameters)
- [`throw`](https://internetcomputer.org/docs/motoko/language-manual#throw)
- [`to_candid`](https://internetcomputer.org/docs/motoko/language-manual#candid-serialization)
- [`transient`](https://internetcomputer.org/docs/motoko/language-manual#error-type)
- [`true`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types)
- [`try`](https://internetcomputer.org/docs/motoko/language-manual#try)
- [`type`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types)
- [`var`](https://internetcomputer.org/docs/motoko/language-manual#varient-types)
- [`while`](https://internetcomputer.org/docs/motoko/language-manual#while)
- [`with`](https://internetcomputer.org/docs/motoko/language-manual)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />