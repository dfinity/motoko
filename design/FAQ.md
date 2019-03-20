> 1. Why would I want an `actor class` versus just a `class`?

An actor is instantiated as a separate wasm instance, with isolated state. Classes can capture (mutable) free variables state, actors class should not (but currently can because the typechecker is too liberal).

> 2. Where am I permitted to place `async` blocks?

At the moment, anywhere - they get compiled to async message sends to the enclosing (perhaps implicit) actor that return a promise. You might, however, not be able to await the result  unless you are in an outer async context! But you could pass in into another async block that can await it.

> 3. What kinds of datatypes are permitted to be declared with the `share` qualifier; and why some but not others (what’s the source of the distinction, and its role in writing actorscript-based systems)

Shared means transmittable without losing identity, essentially. So scalars, immutable data, option of shared, shared (immutable objects), shared functions and actor references can be sent/received, but nothing else that  might contain or close over mutable state. That's the idea, assuming it isn't broken. Not all restriction are currently checked (i.e. escape of shared state into actors and shared functions for instance.) Note that serialization is mostly by value, apart from actors and shared functions, which are by reference, so identity can't be preserved for most values, ruling out them containing state.

> 4. Where would I want to define a `class` versus an `object`? (explain that distinction, if possible)

A class is a family of objects of the same type. There used to be support for checked down-casting (instance of) on classes but we got rid of that, so really a class is just a type def plus constructor function (that's actually what it desugars to as well).

> 5. Where would I want to define an `object` versus a `record` (explain that distinction, if possible)

I guess an object would typically encapsulate state, and fields would be private by default (eventhough they aren't currently). Records would have fields public by default. In the end, an object really is just a record of values and any state encapsulation is by virtue of the object having fields that are functions that close over state in their environments. The distinction between shared objects and non-shared ones is that we don't an  object that has a mutable field, and thus isn't sharable, to become sharable just by virtue of forgetting that field through subtyping. That was the intention anyway.

> 6. What types permit mutation and how is that expressed; what restrictions come with using mutable memory?

Just mutable arrays, mutable fields of objects and mutable locals. Mutable types can't be transmitted or received in messages (shared function calls). There's no first class ref type but one can simulate that with an object with a single mutable field.

