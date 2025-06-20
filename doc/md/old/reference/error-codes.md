---
sidebar_position: 6
---

# Error codes

| Error code | Error description |
|------------|-------------------|
| M0001      | Parsing errors    |
| M0003      | Module tried to import itself |
| M0009      | File not found for import |
| M0010      | Imported package was not defined |
| M0014      | Non-static expression in library or module |
| M0029      | Unbound type |
| M0030      | Type field does not exist in type |
| M0031      | Shared function has non-shared parameter type |
| M0032      | Shared function has non-shared return type |
| M0033      | Async has non-shared content type |
| M0036      | Invalid return type for shared query function |
| M0038      | Misplaced await |
| M0045      | Wrong number of type arguments |
| M0047      | Send capability required |
| M0050      | Literal does not have expected type |
| M0055      | Cannot infer type of forward variable |
| M0057      | Unbound variable |
| M0060      | Operator is not defined for operand types |
| M0064      | Misplaced '!' without enclosing do block |
| M0070      | Expected object type |
| M0072      | Field does not exist in type |
| M0073      | Expected mutable assignment target |
| M0082      | Expected iterable type |
| M0088      | Expected async type |
| M0089      | Redundant ignore |
| M0090      | Actor reference must have an actor type |
| M0096      | Expression can't produce expected type |
| M0097      | Expected function type |
| M0098      | Cannot instantiate function type |
| M0112      | Tuple pattern cannot consume type |
| M0116      | Variant pattern cannot consume type |
| M0126      | Shared function cannot be private |
| M0137      | A type or class was declared that explicitly or implicitly references an outer type parameter. |
| M0139      | Inner actor classes are not supported |
| M0141      | Forbidden declaration in program |
| M0145      | Pattern does not cover value |
| M0149      | An immutable record field (declared without `var`) was supplied where a mutable record field (specified with `var`), was expected. |
| M0150      | A mutable record field (declared with `var`) was supplied where an immutable record field (specified without `var`) was expected. |
| M0151      | A object literal is missing some fields. |
| M0153      | An imported Candid file (.did) mentions types that cannot be represented in Motoko. |
| M0154      | Deprecation annotation |
| M0155      | Inferred type Nat for subtraction |
| M0156      | A parameterized type definition, or set of type definitions, is too complicated for Motoko to accept. |
| M0157      | A type definition, or set of type definitions, is ill-defined. |
| M0158      | A public class was declared without providing it with a name. |
| M0194      | An identifier was defined without referencing it later. |
| M0195      | A function that demands elevated (system) capabilities was called without manifestly passing the capability. |
| M0197      | A function that requires (system) capabilities was called in a context that does not provide them. |
| M0198      | A field identifier was specified in an object pattern without referencing this identifier later. |


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />