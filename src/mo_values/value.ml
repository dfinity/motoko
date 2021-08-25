open Numerics

(* Environments *)

module Env = Env.Make(String)

(* Blobs *)

module Blob = struct
  let escape b =
    String.concat "" (
      List.of_seq (
        Seq.map (fun c ->
          "\\" ^ Lib.Hex.hex_of_char c
        ) (String.to_seq b)
      )
    )
end

(* Types *)

type unicode = int

type actor_id = string

type context = value

and func =
  context -> value -> value cont -> unit

and value =
  | Null
  | Bool of bool
  | Int of Int.t
  | Int8 of Int_8.t
  | Int16 of Int_16.t
  | Int32 of Int_32.t
  | Int64 of Int_64.t
  | Nat8 of Nat8.t
  | Nat16 of Nat16.t
  | Nat32 of Nat32.t
  | Nat64 of Nat64.t
  | Float of Float.t
  | Char of unicode
  | Text of string
  | Blob of string
  | Tup of value list
  | Opt of value
  | Variant of string * value
  | Array of value array
  | Obj of value Env.t
  | Func of Call_conv.t * func
  | Async of async
  | Mut of value ref
  | Iter of value Seq.t ref (* internal to {b.vals(), t.chars()} iterator *)

and res = Ok of value | Error of value
and async = {result : res Lib.Promise.t ; mutable waiters : (value cont * value cont) list}

and def = value Lib.Promise.t
and 'a cont = 'a -> unit


(* Shorthands *)

let unit = Tup []

let local_func n m f = Func (Call_conv.local_cc n m, f)
let message_func s n f = Func (Call_conv.message_cc s n, f)
let async_func s n m f = Func (Call_conv.async_cc s n m, f)
let replies_func s n m f = Func (Call_conv.replies_cc s n m, f)


(* Projections *)

let invalid s = raise (Invalid_argument ("Value." ^ s))

let as_null = function Null -> () | _ -> invalid "as_null"
let as_bool = function Bool b -> b | _ -> invalid "as_bool"
let as_int = function Int n -> n | _ -> invalid "as_int"
let as_int8 = function Int8 w -> w | _ -> invalid "as_int8"
let as_int16 = function Int16 w -> w | _ -> invalid "as_int16"
let as_int32 = function Int32 w -> w | _ -> invalid "as_int32"
let as_int64 = function Int64 w -> w | _ -> invalid "as_int64"
let as_nat8 = function Nat8 w -> w | _ -> invalid "as_nat8"
let as_nat16 = function Nat16 w -> w | _ -> invalid "as_nat16"
let as_nat32 = function Nat32 w -> w | _ -> invalid "as_nat32"
let as_nat64 = function Nat64 w -> w | _ -> invalid "as_nat64"
let as_float = function Float f -> f | _ -> invalid "as_float"
let as_char = function Char c -> c | _ -> invalid "as_char"
let as_text = function Text s -> s | _ -> invalid "as_text"
let as_blob = function Blob b -> b | _ -> invalid "as_blob"
let as_iter = function Iter i -> i | _ -> invalid "as_iter"
let as_array = function Array a -> a | _ -> invalid "as_array"
let as_opt = function Opt v -> v | _ -> invalid "as_opt"
let as_variant = function Variant (i, v) -> i, v | _ -> invalid "as_variant"
let as_tup = function Tup vs -> vs | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [v1; v2] -> v1, v2 | _ -> invalid "as_pair"
let as_obj = function Obj ve -> ve | _ -> invalid "as_obj"
let as_func = function Func (cc, f) -> cc, f | _ -> invalid "as_func"
let as_async = function Async a -> a | _ -> invalid "as_async"
let as_mut = function Mut r -> r | _ -> invalid "as_mut"


(* Ordering *)

let generic_compare = compare

let rec compare x1 x2 =
  if x1 == x2 then 0 else
  match x1, x2 with
  | Int n1, Int n2 -> Int.compare n1 n2
  | Int8 n1, Int8 n2 -> Int_8.compare n1 n2
  | Int16 n1, Int16 n2 -> Int_16.compare n1 n2
  | Int32 n1, Int32 n2 -> Int_32.compare n1 n2
  | Int64 n1, Int64 n2 -> Int_64.compare n1 n2
  | Nat8 n1, Nat8 n2 -> Nat8.compare n1 n2
  | Nat16 n1, Nat16 n2 -> Nat16.compare n1 n2
  | Nat32 n1, Nat32 n2 -> Nat32.compare n1 n2
  | Nat64 n1, Nat64 n2 -> Nat64.compare n1 n2
  | Opt v1, Opt v2 -> compare v1 v2
  | Tup vs1, Tup vs2 -> Lib.List.compare compare vs1 vs2
  | Array a1, Array a2 -> Lib.Array.compare compare a1 a2
  | Obj fs1, Obj fs2 -> Env.compare compare fs1 fs2
  | Variant (l1, v1), Variant (l2, v2) ->
    (match String.compare l1 l2 with
    | 0 -> compare v1 v2
    | i -> i
    )
  | Mut r1, Mut r2 -> compare !r1 !r2
  | Async _, Async _ -> raise (Invalid_argument "Value.compare")
  | _ -> generic_compare x1 x2

let equal x1 x2 = compare x1 x2 = 0


(* (Pseudo)-Identities (for caller and self) *)

let next_id = ref 0

let fresh_id() =
  let id = Printf.sprintf "ID:%i" (!next_id) in
  next_id := !next_id + 1;
  id

let top_id = fresh_id ()

(* Pretty Printing *)

let add_unicode buf = function
  | 0x09 -> Buffer.add_string buf "\\t"
  | 0x0a -> Buffer.add_string buf "\\n"
  | 0x22 -> Buffer.add_string buf "\\\""
  | 0x27 -> Buffer.add_string buf "\\\'"
  | 0x5c -> Buffer.add_string buf "\\\\"
  | c when 0x20 <= c && c < 0x7f -> Buffer.add_char buf (Char.chr c)
  | c -> Printf.bprintf buf "\\u{%02x}" c

let string_of_string lsep s rsep =
  let buf = Buffer.create 256 in
  Buffer.add_char buf lsep;
  List.iter (add_unicode buf) s;
  Buffer.add_char buf rsep;
  Buffer.contents buf

let pos_sign b = if b then "+" else ""

open Format

let pr = pp_print_string

let comma ppf () = fprintf ppf ",@ "

let semi ppf () = fprintf ppf ";@ "

let rec pp_val_nullary d ppf = function
  | Null -> pr ppf "null"
  | Bool b -> pr ppf (if b then "true" else "false")
  | Int n when Int.(ge n zero) -> pr ppf (Int.to_pretty_string n)
  | Int8 n when Int_8.(n = zero) -> pr ppf (Int_8.to_pretty_string n)
  | Int16 n when Int_16.(n = zero) -> pr ppf (Int_16.to_pretty_string n)
  | Int32 n when Int_32.(n = zero) -> pr ppf (Int_32.to_pretty_string n)
  | Int64 n when Int_64.(n = zero) -> pr ppf (Int_64.to_pretty_string n)
  | Nat8 n -> pr ppf (Nat8.to_pretty_string n)
  | Nat16 n -> pr ppf (Nat16.to_pretty_string n)
  | Nat32 n -> pr ppf (Nat32.to_pretty_string n)
  | Nat64 n -> pr ppf (Nat64.to_pretty_string n)
  | Float f -> pr ppf (Float.to_pretty_string f)
  | Char c ->  pr ppf (string_of_string '\'' [c] '\'')
  | Text t -> pr ppf (string_of_string '\"' (Wasm.Utf8.decode t) '\"')
  | Blob b -> pr ppf ("\"" ^ Blob.escape b ^ "\"")
  | Tup vs ->
    fprintf ppf "@[<1>(%a%s)@]"
      (pp_print_list ~pp_sep:comma (pp_val d)) vs
      (if List.length vs = 1 then "," else "")
  | Obj ve ->
    if d = 0 then pr ppf "{...}" else
    fprintf ppf "@[<hv 2>{@;<0 0>%a@;<0 -2>}@]"
      (pp_print_list ~pp_sep:semi (pp_field d)) (Env.bindings ve)
  | Array a ->
    fprintf ppf "@[<1>[%a]@]"
      (pp_print_list ~pp_sep:comma (pp_val d)) (Array.to_list a)
  | Func (_, _) -> pr ppf "func"
  | v ->
    (* "(" ^ string_of_val d v ^ ")" *)
    fprintf ppf "@[<1>(%a)@]" (pp_val d) v

and pp_field d ppf (lab, v) =
    fprintf ppf "@[<2>%s =@ %a@]" lab (pp_val d) v

and pp_val d ppf = function
  | Int i -> pr ppf (Int.to_pretty_string i)
  | Int8 i -> pr ppf (Int_8.(pos_sign (gt i zero) ^ to_pretty_string i))
  | Int16 i -> pr ppf (Int_16.(pos_sign (gt i zero) ^ to_pretty_string i))
  | Int32 i -> pr ppf (Int_32.(pos_sign (gt i zero) ^ to_pretty_string i))
  | Int64 i -> pr ppf (Int_64.(pos_sign (gt i zero) ^ to_pretty_string i))
  | Opt v -> fprintf ppf "@[<1>?%a@]" (pp_val_nullary d) v
  | Variant (l, Tup []) -> fprintf ppf "#%s" l
  | Variant (l, Tup vs) -> fprintf ppf "@[#%s@;<0 1>%a@]" l (pp_val d) (Tup vs)
  | Variant (l, v) -> fprintf ppf "@[#%s@;<0 1>(%a)@]" l (pp_val d) v
  | Async {result; waiters = []} ->
    fprintf ppf "@[<2>async@ %a@]" (pp_res d) result
  | Async {result; waiters} ->
    fprintf ppf "@[<2>async[%d]@ %a@]"
      (List.length waiters) (pp_res d) result
  | Mut r -> pp_val d ppf !r
  | v -> pp_val_nullary d ppf v

and pp_res d ppf result =
  match Lib.Promise.value_opt result with
  | Some (Error v)-> fprintf ppf "@[Error@ %a@]" (pp_val_nullary d) v
  | Some (Ok v) -> pp_val_nullary d ppf v
  | None -> pr ppf "_"

and pp_def d ppf def =
  match Lib.Promise.value_opt def with
  | Some v -> pp_val d ppf v
  | None -> pr ppf "_"

let string_of_val d v : string =
  Lib.Format.with_str_formatter (fun ppf ->
    pp_val d ppf) v

let string_of_def d def : string =
  Lib.Format.with_str_formatter (fun ppf ->
    pp_def d ppf) def
