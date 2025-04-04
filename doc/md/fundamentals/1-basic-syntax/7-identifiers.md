---
sidebar_position: 7
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

- [`actor`](/docs/motoko/language-manual#programs)
- [`and`](/docs/motoko/language-manual#and)
- [`assert`](/docs/motoko/language-manual#assert)
- [`async`](/docs/motoko/language-manual#async)
- [`async*`](/docs/motoko/language-manual#async-1)
- [`await`](/docs/motoko/language-manual#await)
- [`await*`](/docs/motoko/language-manual#await-1)
- [`break`](/docs/motoko/language-manual#break)
- [`case`](/docs/motoko/language-manual#switch)
- [`catch`](/docs/motoko/language-manual#throw)
- [`class`](/docs/motoko/language-manual#class-declaration)
- [`composite`](/docs/motoko/language-manual#function)
- [`continue`](/docs/motoko/language-manual#labeled-loops)
- [`debug`](/docs/motoko/language-manual#debug)
- [`debug_show`](/docs/motoko/language-manual#debug)
- [`do`](/docs/motoko/language-manual#do)
- [`else`](/docs/motoko/language-manual#let-else-declaration)
- [`false`](/docs/motoko/language-manual#primitive-types)
- [`flexible`](/docs/motoko/language-manual#declaration-fields)
- [`finally`](/docs/motoko/language-manual#try)
- [`for`](/docs/motoko/language-manual#for)
- [`from_candid`](/docs/motoko/language-manual#candid-serialization)
- [`func`](/docs/motoko/language-manual#functions)
- [`if`](/docs/motoko/language-manual#if)
- [`ignore`](/docs/motoko/language-manual#ignore)
- [`import`](/docs/motoko/language-manual#imports)
- [`in`](/docs/motoko/language-manual)
- [`label`](/docs/motoko/language-manual#label)
- [`let`](/docs/motoko/language-manual#let-declaration)
- [`loop`](/docs/motoko/language-manual#loop)
- [`module`](/docs/motoko/language-manual#imports)
- [`not`](/docs/motoko/language-manual#not)
- [`null`](/docs/motoko/language-manual#null-break)
- [`object`](/docs/motoko/language-manual#object-pattern)
- [`or`](/docs/motoko/language-manual#or)
- [`persistent`](/docs/motoko/language-manual#declaration-fields)
- [`private`](/docs/motoko/language-manual#object-declaration)
- [`public`](/docs/motoko/language-manual#object-declaration)
- [`query`](/docs/motoko/language-manual#functions)
- [`return`](/docs/motoko/language-manual#return)
- [`shared`](/docs/motoko/language-manual#programs)
- [`stable`](/docs/motoko/language-manual#type-region)
- [`switch`](/docs/motoko/language-manual#patterns)
- [`system`](/docs/motoko/language-manual#type-parameters)
- [`throw`](/docs/motoko/language-manual#throw)
- [`to_candid`](/docs/motoko/language-manual#candid-serialization)
- [`transient`](/docs/motoko/language-manual#error-type)
- [`true`](/docs/motoko/language-manual#primitive-types)
- [`try`](/docs/motoko/language-manual#try)
- [`type`](/docs/motoko/language-manual#primitive-types)
- [`var`](/docs/motoko/language-manual#varient-types)
- [`while`](/docs/motoko/language-manual#while)
- [`with`](/docs/motoko/language-manual)

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />