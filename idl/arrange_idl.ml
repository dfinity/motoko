open Source
open Syntax_idl
open Wasm.Sexpr

module Type = Type_idl
   
let ($$) head inner = Node (head, inner)

and id i = Atom i.it
and tag i = Atom ("#" ^ i.it)

let field_tag (tf : typ_field)
  = tf.it.name.it ^ "(" ^ Stdint.Uint64.to_string tf.it.id ^ ")"

let rec typ_field (tf : typ_field)
  = field_tag tf $$ [typ tf.it.typ]

and typ_bind (tb : typ_bind)
  = tb.it.var.it $$ [typ tb.it.bound]

and mode m = match m.it with
  | Type.Sensitive -> Atom "Sensitive"
  | Type.Pure -> Atom "Pure"
  | Type.Updatable -> Atom "Updatable"
  
and typ t = match t.it with
  | VarT s        -> "VarT" $$ [id s]
  | PrimT p             -> "PrimT" $$ [Atom p]
  | RecordT ts        -> "RecordT" $$ List.map typ_field ts
  | VecT t       -> "VecT" $$ [typ t]
  | OptT t              -> "OptT" $$ [typ t]
  | VariantT cts        -> "VariantT" $$ List.map typ_field cts
  | TupT ts             -> "TupT" $$ List.map typ ts
  | FuncT (ms, at, rt) -> "FuncT" $$ List.map mode ms @ [ typ at; typ rt]
  | RefFuncT t -> "RefFuncT" $$ [typ t]
  | RefServT ts -> "RefServT" $$ List.map typ_bind ts
                        
and dec d = match d.it with
  | TypD (x, t) ->
    "TypD" $$ [id x] @ [typ t]
  | ActorD (x, tp) ->
    "ActorD" $$ id x :: List.map typ_bind tp

and prog prog = "BlockE"  $$ List.map dec prog.it
