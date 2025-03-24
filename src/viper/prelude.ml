open Common

let prelude_array_encoding : string = {prelude|/* Array encoding */
domain Array {
  function $loc(a: Array, i: Int): Ref
  function $size(a: Array): Int
  function $loc_inv1(r: Ref): Array
  function $loc_inv2(r: Ref): Int
  axiom $all_diff_array { forall a: Array, i: Int :: {$loc(a, i)} $loc_inv1($loc(a, i)) == a && $loc_inv2($loc(a, i)) == i }
  axiom $size_nonneg { forall a: Array :: $size(a) >= 0 }
}
define $array_acc(a, t, p) forall j: Int :: 0 <= j && j < $size(a) ==> acc($loc(a, j).t, p)
define $array_untouched(a, t) forall j: Int :: 0 <= j && j < $size(a) ==> $loc(a, j).t == old($loc(a, j).t)
define $array_init(a, t, x) forall i : Int :: {$loc(a, i).t} 0 <= i && i < $size(a) ==> $loc(a, i).t == x|prelude}

let pp_tup_decl_param  ppf i = Format.fprintf ppf "T%d" i
let pp_tup_decl_params ppf = function
  | 0 -> ()
  | n ->
      let comma ppf () = Format.fprintf ppf ",@ " in
      Format.fprintf ppf "[%a]"
        (Format.pp_print_list pp_tup_decl_param ~pp_sep:comma) (List.init n Fun.id)

let pp_tup_decl_con_field n ppf i =
  Format.fprintf ppf "%s : %a"
    (tup_prj_name n i)
    pp_tup_decl_param i
let pp_tup_decl_con ppf n =
  let comma ppf () = Format.fprintf ppf ",@ " in
  Format.fprintf ppf "%s@[(%a)@]"
    (tup_con_name n)
    (Format.pp_print_list ~pp_sep:comma (pp_tup_decl_con_field n)) (List.init n Fun.id)

let pp_tup_decl n =
  Format.asprintf "@[<2>adt Tuple$%d@;%a@;@[<v 2>{ %a }@]@]"
    n
    pp_tup_decl_params n
    pp_tup_decl_con n

let prelude_tuple_encoding (tuple_arities : IntSet.t) : string =
  String.concat "\n"
  ("/* Tuple encoding */" ::
    List.map pp_tup_decl (IntSet.elements tuple_arities))

let prelude_option_encoding : string = {prelude|/* Option encoding */
adt Option[T] {
  None()
  Some(some$0: T)
}|prelude}

let prelude_text_encoding : string = {prelude|/* Text encoding */
function $concat(a: Int, b: Int): Int|prelude}

let pp_typed_field (name, typ) =
  Format.asprintf "@[<2>field %s:@ %a@]"
      name
      Pretty.pp_typ typ

let prelude_typed_references typed_fields : string =
  String.concat "\n"
  ("/* Typed references */" ::
    List.map pp_typed_field (StrMap.bindings typed_fields))

let prelude reqs: string =
  String.concat "\n"
  [
    "/* BEGIN PRELUDE */";
    prelude_array_encoding;
    prelude_tuple_encoding !(reqs.tuple_arities);
    prelude_option_encoding;
    prelude_text_encoding;
    prelude_typed_references !(reqs.typed_fields);
    "/* END PRELUDE */"
  ]
