---
sidebar_position: 7
hide_table_of_contents: true
---

# Identifiers

Identifiers are names used for variables, functions, types, and other entities. They must start with a letter or an underscore and can contain letters, digits, and underscores.

```motoko no-repl
let name = "Motoko";
let a1 = 123;
let camelCaseIdentifier = "best practice";
let snake_case_identifier = "for compatibility with other languages";
```

## Reserved syntax keywords

Motoko reserves certain words for its syntax and they cannot be used as identifiers. These include:


|  |  | |  |
|--------|---------|---------|---------|
| [`actor`](https://internetcomputer.org/docs/motoko/language-manual#programs) | [`async*`](https://internetcomputer.org/docs/motoko/language-manual#async-1) | [`composite`](https://internetcomputer.org/docs/motoko/language-manual#function) | [`false`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types) |
| [`and`](https://internetcomputer.org/docs/motoko/language-manual#and) | [`await`](https://internetcomputer.org/docs/motoko/language-manual#await) | [`continue`](https://internetcomputer.org/docs/motoko/language-manual#labeled-loops) | [`flexible`](https://internetcomputer.org/docs/motoko/language-manual#declaration-fields) |
| [`assert`](https://internetcomputer.org/docs/motoko/language-manual#assert) | [`await*`](https://internetcomputer.org/docs/motoko/language-manual#await-1) | [`debug`](https://internetcomputer.org/docs/motoko/language-manual#debug) | [`finally`](https://internetcomputer.org/docs/motoko/language-manual#try) |
| [`async`](https://internetcomputer.org/docs/motoko/language-manual#async) | [`break`](https://internetcomputer.org/docs/motoko/language-manual#break) | [`debug_show`](https://internetcomputer.org/docs/motoko/language-manual#debug) | [`for`](https://internetcomputer.org/docs/motoko/language-manual#for) |
| [`case`](https://internetcomputer.org/docs/motoko/language-manual#switch) | [`catch`](https://internetcomputer.org/docs/motoko/language-manual#throw) | [`do`](https://internetcomputer.org/docs/motoko/language-manual#do) | [`from_candid`](https://internetcomputer.org/docs/motoko/language-manual#candid-serialization) |
| [`func`](https://internetcomputer.org/docs/motoko/language-manual#functions) | [`if`](https://internetcomputer.org/docs/motoko/language-manual#if) | [`ignore`](https://internetcomputer.org/docs/motoko/language-manual#ignore) | [`import`](https://internetcomputer.org/docs/motoko/language-manual#imports) |
| [`in`](https://internetcomputer.org/docs/motoko/language-manual) | [`label`](https://internetcomputer.org/docs/motoko/language-manual#label) | [`let`](https://internetcomputer.org/docs/motoko/language-manual#let-declaration) | [`loop`](https://internetcomputer.org/docs/motoko/language-manual#loop) |
| [`module`](https://internetcomputer.org/docs/motoko/language-manual#imports) | [`not`](https://internetcomputer.org/docs/motoko/language-manual#not) | [`null`](https://internetcomputer.org/docs/motoko/language-manual#null-break) | [`object`](https://internetcomputer.org/docs/motoko/language-manual#object-pattern) |
| [`or`](https://internetcomputer.org/docs/motoko/language-manual#or) | [`persistent`](https://internetcomputer.org/docs/motoko/language-manual#declaration-fields) | [`private`](https://internetcomputer.org/docs/motoko/language-manual#object-declaration) | [`public`](https://internetcomputer.org/docs/motoko/language-manual#object-declaration) |
| [`query`](https://internetcomputer.org/docs/motoko/language-manual#functions) | [`return`](https://internetcomputer.org/docs/motoko/language-manual#return) | [`shared`](https://internetcomputer.org/docs/motoko/language-manual#programs) | [`stable`](https://internetcomputer.org/docs/motoko/language-manual#type-region) |
| [`switch`](https://internetcomputer.org/docs/motoko/language-manual#patterns) | [`system`](https://internetcomputer.org/docs/motoko/language-manual#type-parameters) | [`throw`](https://internetcomputer.org/docs/motoko/language-manual#throw) | [`to_candid`](https://internetcomputer.org/docs/motoko/language-manual#candid-serialization) |
| [`transient`](https://internetcomputer.org/docs/motoko/language-manual#error-type) | [`true`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types) | [`try`](https://internetcomputer.org/docs/motoko/language-manual#try) | [`type`](https://internetcomputer.org/docs/motoko/language-manual#primitive-types) |
| [`var`](https://internetcomputer.org/docs/motoko/language-manual#varient-types) | [`while`](https://internetcomputer.org/docs/motoko/language-manual#while) | [`with`](https://internetcomputer.org/docs/motoko/language-manual) | [`class`](https://internetcomputer.org/docs/motoko/language-manual#class-declaration) |


