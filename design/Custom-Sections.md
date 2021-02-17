== Motoko specific Wasm custom sections

The Motoko compiler produces its own optional custom section named "motoko".

When present, Motoko subsection 0 contains a non-empty vector of UTF-8 encoded
strings in the ASCII subset. It lists the set of record field names or
variant tags, sans further distinction, used by the program.

A tool, such as debugger or decompiler, may use the field names to
construct a map from field/tag hashes occurring in object
representations to symbolic field/tag names.
Due to rare hash collisions, this map may be ambiguous.

Using the notation and definitions of the Wasm reference implementation in OCaml:

```ocaml
let motoko_section_body labels =
  section 0 (vec string) labels (labels <> [])

let motoko_sections motoko =
  custom_section "motoko" motoko_section_body motoko.labels (motoko.labels <> [])
```

The hash function on ASCII strings results in values [0..2^31-1], is given by the following OCaml code:

```ocaml
let hash : string -> int32 = fun s ->
  let open Int32 in

  logand (shift_right_logical minus_one 1) (
    List.fold_left
      (fun s c -> add (mul s (of_int 223)) (of_int (Char.code c)))
      zero
      (Lib.String.explode s)
  )
```
