# Motoko Programming Language Guide

## About this guide

The *Motoko Programming Language Guide* introduces key features of the general-purpose Motoko programming language and provides examples and reference information to help you learn the nuances of the language and the practical implications of how to apply it.

The Motoko programming language is optimized for developing programs that run on the Internet Computer blockchain network and to work with the DFINITY Canister Software Development Kit (SDK). You could, in principle, also write programs using Motoko for more traditional platforms and to run in other contexts, though support for this is currently best-effort and incomplete. This guide attempts to strike a balance between highlighting features that are uniquely suited to running on the Internet Computer and features that are generally-applicable or well-suited for programs running on all targets.

## Intended audience

This guide provides reference information and examples for programmers who want to explore or plan to use the Motoko programming language. Most of the information in this guide is applicable independent of whether you are developing programs to run on the Internet Computer or working with the DFINITY Canister SDK.

The guide assumes you are familiar with basic programming principles and terminology and have at least some experience writing programs in a high-level programming language such as C++ or Rust, or have practical experience working with a scripting language such as JavaScript or TypeScript. In addition, Motoko incorporates some aspects of functional programming, so you might find some knowledge of functional programming design principles helpful in learning to use Motoko.

Although this guide is intended to help readers from different backgrounds understand the basic design principles and semantics of the Motoko, you should keep in mind that the language implementation and the documentation are also continuing to evolve.

## Using this guide

To provide a framework for learning Motoko, you might want to start by reviewing [Engineering values and goals](#engineering-values-and-goals). The [Engineering values and goals](#engineering-values-and-goals) describe the core design considerations for the development and evolution of the Motoko programming language.

With those considerations in mind, you can start to explore fundamental concepts, including the role of types and type annotations, in simple code examples and small programs.

Once you are familiar with the basic concepts and terminology, later sections introduce programs that compute in more interesting ways, including function abstractions, user-defined type definitions, user-defined actors, and asynchronous communication.

As you begin using Motoko to write your own programs, you can return to this guide for reference information and examples.

Most of the code examples in this guide are interactive: you can live edit the example, interpret the code in the browser and see the result. The interpreter is provided for education purposes. While most of the language features are supported in the interpreter, they are not exactly the same as the real compiler. For example, you may get a stack overflow for a medium-size input, while the real compiler handles the input just fine. Some of the system features are not fully supported, such as cycles, canister imports and state-mutating query calls.

## Documentation conventions

The following conventions are used in this guide:

-   `Fixed-width` font is used for sample code, program names, program output, file names, and commands that you type at the command line.

-   **Bold** text is used to emphasize commands, buttons, or user interface text, and to introduce new terms.

-   *Italics* are used for book titles and to emphasize specific words or terms.

-   The CAUTION style is used to indicate content that is missing or incomplete.

-   The WARNING style is used to indicate where content is outdated or potentially inaccurate.

-   The NOTE style is used to indicate content that describes a feature that is not yet supported but is planned for a future release.

## Engineering values and goals

The engineering effort behind the design and implementation of Motoko is driven by a core set of values and goals. The DFINITY engineering organization uses these values and goals to define and prioritize the language features and enhancements to add and improve as part of *ongoing* language development.

For transparency into the principles that guide the engineering effort, the engineering organization has identified the following sets of core values and secondary values for driving the direction of the Motoko programming language.

### Core values

The following guiding principles represent the core values of the engineering organization in prioritized order:

1.  Seamless integration with the [Internet Computer blockchain network](../../../../concepts/what-is-IC.md#what-is-the-internet-computer) to ensure that Motoko provides full language support for the actor-based model, asynchronous messaging, data persistence, interface description language interoperability, and other features.

2.  Ergonomics to ensure that Motoko embraces familiarity, simplicity, clarity, explicitness, and other human factors.

3.  Formal correctness to ensure that Motoko maintains state isolation, a sound type system and type safety, precision, pattern matching, appropriate defaults, and coding best-practices.

### Secondary values

The following principles represent the secondary values of the engineering organization that are deemed important but not primary driving factors:

1.  Expressiveness, so that Motoko provides first-class functions, polymorphism, pattern matching, and more as the language evolves.

2.  Performance, so that Motoko provides reasonably fast operation initially and continues to improves as the language evolves.

3.  Readiness, so the Motoko comes with "batteries included" in the form of libraries and examples and out-of-the-box integration with the DFINITY Canister SDK.

### Non-goals

As a counterpoint to the core values and goals, the engineering organization also identified the following as "non-goals" that are outside of the scope of the engineering effort:

1.  Having a more advanced type system, with cutting-edge features.

2.  Simplicity over functionality in design or implementation (the "Worse is Better" approach).

3.  Interoperability or support for running Motoko programs on blockchains other than the Internet Computer.

## Finding more information

For information about using Motoko with the DFINITY Canister SDK, see the [SDK Developer Tools](../../index.md).

For background information on various topics relevant to the design, use, or deployment of Motoko services, or the language’s design itself, consider the following resources as a starting point:

### WebAssembly

-   [WebAssembly home page](https://webassembly.org/).

-   [WebAssembly overview video (youtube)](https://www.youtube.com/watch?v=fvkIQfRZ-Y0).

### Modern type systems

-   [Practical Foundations for Programming Languages](http://www.cs.cmu.edu/~rwh/pfpl/) by Robert Harper. Cambridge University Press, 2016.

-   [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) by Benjamin C. Pierce. The MIT Press.

## Getting additional support

If you are looking for more information or technical support, the DFINITY website provides quick access to frequently-asked questions, technical articles, developer updates, and other resources. From the website, you can search knowledge base articles, open and view support cases, sign up for the newsletter, read the latest blog posts, view how-to videos, download software updates, or exchange ideas with members of the community.

In addition to the resources available on the website, you can connect with DFINITY or other developers using social media or by visiting the DFINITY Community Forum on Discourse and joining the conversation.
