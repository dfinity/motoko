open Source
open Syntax_idl
open Wasm.Sexpr

let string_of_prim p =
  match p with
  | Nat -> "nat"
  | Nat8 -> "nat8"
  | Nat16 -> "nat16"
  | Nat32 -> "nat32"
  | Nat64 -> "nat64"
  | Int -> "int"
  | Int8 -> "int8"
  | Int16 -> "int16"
  | Int32 -> "int32"
  | Int64 -> "int64"
  | Float32 -> "float32"
  | Float64 -> "float64"
  | Bool -> "bool"
  | Text -> "text"
  | Null -> "null"
  | Unavailable -> "unavailable"          
   
let ($$) head inner = Node (head, inner)

and id i = Atom i.it
and tag i = Atom ("#" ^ i.it)

let field_tag (tf : typ_field)
  = tf.it.name.it ^ "(" ^ Stdint.Uint64.to_string tf.it.id ^ ")"

let rec typ_field (tf : typ_field)
  = field_tag tf $$ [typ tf.it.typ]

and typ_meth (tb : typ_meth)
  = tb.it.var.it $$ [typ tb.it.bound]

and mode m = match m.it with
  | Sensitive -> Atom "Sensitive"
  | Pure -> Atom "Pure"
  | Updatable -> Atom "Updatable"
  
and typ t = match t.it with
  | VarT s        -> "VarT" $$ [id s]
  | PrimT p             -> "PrimT" $$ [Atom (string_of_prim p)]
  | RecordT ts        -> "RecordT" $$ List.map typ_field ts
  | VecT t       -> "VecT" $$ [typ t]
  | OptT t              -> "OptT" $$ [typ t]
  | VariantT cts        -> "VariantT" $$ List.map typ_field cts
  | TupT ts             -> "TupT" $$ List.map typ ts
  | FuncT (ms, at, rt) -> "FuncT" $$ List.map mode ms @ [ typ at; typ rt]
  | ServT ts -> "ServT" $$ List.map typ_meth ts
  | PreT -> Atom "PreT"
                        
and dec d = match d.it with
  | TypD (x, t) ->
    "TypD" $$ [id x] @ [typ t]
  | ActorD (x, tp) ->
     "ActorD" $$ id x :: List.map typ_meth tp
  | ActorVarD (x, var) ->
     "ActorVarD" $$ [id x] @ [id var]

and prog prog = "BlockE"  $$ List.map dec prog.it


(* Pretty printing  *)
              
open Printf
let string_of_list f sep list = String.concat sep (List.map f list)
let string_of_list_opt left right f sep list =
  match list with
  | [] -> ""
  | l -> left ^ string_of_list f sep l ^ right
         
let rec string_of_typ t =
  match t.it with
  | VarT id -> sprintf "var %s" id.it
  | PrimT s -> string_of_prim s
  | FuncT (ms,s,t) ->
     sprintf "%s -> %s%s" (string_of_typ s) (string_of_list_opt "[" "] " string_of_mode ", " ms) (string_of_typ t)
  | TupT ts -> "(" ^ string_of_list string_of_typ ", " ts ^ ")"
  | OptT t -> "opt " ^ string_of_typ t
  | VecT t -> "vec " ^ string_of_typ t
  | RecordT fs -> sprintf "{%s}" (string_of_list string_of_field "; " fs)
  | VariantT fs -> sprintf "variant {%s}" (string_of_list string_of_field "; " fs)
  | ServT ms -> sprintf "service {%s}" (string_of_list string_of_meth "; " ms)
  | PreT -> "Pre"

and string_of_field f =
  sprintf "%s : %s" f.it.name.it (string_of_typ f.it.typ)
and string_of_meth m =
  sprintf "%s : %s" m.it.var.it (string_of_typ m.it.bound)
and string_of_mode m =
  match m.it with
  | Sensitive -> "Sensitive"
  | Pure -> "Pure"
  | Updatable -> "Update"
