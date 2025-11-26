# Contextual dot resolution

## Motivation
There is a schism in Motoko between object-oriented (using classes and methods) vs procedural/functional (using types and functions) patterns for defining abstractions. Both have pros and cons that cannot be reconciled in the language as is.

We have special rules for treating arrays, blobs, and text as pseudo objects when used with dot notation. Besides being super ad-hoc, the special status of these functions has repeatedly confused users who tried to find them, e.g., in the Array module.

Functional notation, as required for most of our library functions, has a well-known UX disadvantage in that e.g. code completion does not apply.


## Design
The basic idea is to generalise the typing rule for the dot operator. Given the expression `e.x(args)`:

If e does not have object type, or it is an object with no field `x`, then the field is searched for in the environment.

The search looks for a module with a value member `x` with a function type `x : (self : t_arg, ...) -> t_res` where `t_arg` (can be instantiated such that it) is a supertype of `e`'s type. Notably the first parameter of the function needs to be named "self".

In case there are multiple modules in the context satisfying these two conditions, we report an ambiguity error. This is to avoid "spooky action at a distance" when reordering definitions/imports.

If a suitable module, say `M`, is found, then the expression is typed as if it was the application `M.x(e, args)` (including possible inference for omitted type arguments).

## Implementation
The resolution happens during type-checking and is completely eliminated when lowering to IR.
