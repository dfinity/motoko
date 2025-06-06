---
sidebar_position: 24
---

# Static types



Like other modern programming languages, Motoko permits each variable to carry the value of a function, object, or a primitive datum such as a string, word, or integer. Other [types of values](../getting-started/basic-concepts.md#intro-values) exist as well, including records, tuples, and tagged data are called variants.

Motoko uses the formal property of type safety, also known as type soundness. Each variable in a Motoko program carries an associated type, and this type is known statically before the program executes. Each use of every variable is checked by the compiler to prevent runtime type errors, including `null` reference errors, invalid field access, and the like. In this sense, Motoko types provide a form of trustworthy, **compiler-verified** documentation in the program source code.

To execute, Motoko statically compiles to WebAssembly, a portable binary format that abstracts cleanly over modern computer hardware, and thus permits its execution broadly on the Internet and ICP.

This is often summarized with the phrase ["well-typed Motoko programs don’t go wrong"](../getting-started/basic-concepts.md#type-soundness), meaning that the only operations that will be performed on data are those permitted by its static type.

Dynamic testing can check properties that are beyond the reach of the Motoko type system. The Motoko type system is intentionally not advanced. Rather, the type system of Motoko integrates standard concepts from modern, but well-understood, [type systems](../getting-started/motoko-introduction) to provide an approachable, expressive, yet safe language for programming general-purpose, distributed applications.


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />