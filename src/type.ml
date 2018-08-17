(* Representation *)

type con = Con.t
type sort = Object | Actor

type prim =
  | Null
  | Bool
  | Nat
  | Int
  | Word8
  | Word16
  | Word32
  | Word64
  | Float
  | Char
  | Text

type t = typ
and typ =
  | Var of con * typ list                     (* constructor *)
  | Prim of prim                              (* primitive *)
  | Obj of sort * field list                  (* object *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of bind list * typ * typ             (* function *)
  | Async of typ                              (* future *)
  | Like of typ                               (* expansion *)
  | Mut of typ                                (* mutable type *)
  | Any                                       (* top *)
  | Pre                                       (* pre-type *)

and bind = {con : con; bound : typ}
and field = {name : string; typ : typ}

type kind =
  | Def of bind list * typ
  | Abs of bind list * typ

type con_env = kind Con.Env.t


(* Short-hands *)

let unit = Tup []
let bool = Prim Bool
let nat = Prim Nat
let int = Prim Int


(* Pretty printing *)

open Printf

let string_of_prim = function
  | Null -> "Null"
  | Int -> "Int"
  | Bool -> "Bool"
  | Float -> "Float"
  | Nat -> "Nat"
  | Word8 -> "Word8"
  | Word16 -> "Word16"
  | Word32 -> "Word32"
  | Word64 -> "Word64"
  | Char -> "Char"
  | Text -> "Text"

let rec string_of_typ_nullary = function
  | Pre -> "???"
  | Any -> "any"
  | Prim p -> string_of_prim p
  | Var (c, []) -> Con.to_string c
  | Var (c, ts) ->
    sprintf "%s<%s>"
      (Con.to_string c) (String.concat ", " (List.map string_of_typ ts))
  | Tup ts ->
    sprintf "(%s)" (String.concat ", " (List.map string_of_typ ts))
  | Obj (Object, fs) ->
    sprintf "{%s}" (String.concat "; " (List.map string_of_field fs))
  | t ->
    sprintf "(%s)" (string_of_typ t)

and string_of_typ t =
  match t with
  | Array (Mut t) ->
    sprintf "var %s[]" (string_of_typ_nullary t)  
  | Array t ->
    sprintf "%s[]" (string_of_typ_nullary t)  
  | Func (tbs, t1, t2) ->
    sprintf "%s%s -> %s"
      (string_of_binds tbs) (string_of_typ_nullary t1) (string_of_typ t2)
  | Opt t ->
    sprintf "%s?"  (string_of_typ_nullary t)
  | Async t -> 
    sprintf "async %s" (string_of_typ_nullary t)
  | Like t -> 
    sprintf "like %s" (string_of_typ_nullary t)
  | Obj (Actor, fs) ->
    sprintf "actor %s" (string_of_typ_nullary (Obj (Object, fs)))
  | Mut t ->
    sprintf "var %s" (string_of_typ t)
  | t -> string_of_typ_nullary t

and string_of_field {name; typ} =
  sprintf "%s : %s" name (string_of_typ typ)

and string_of_bind {con; bound} =
  Con.to_string con ^
  (if bound = Any then "" else " <: " ^ string_of_typ bound)

and string_of_binds = function
  | [] -> ""
  | tbs -> "<" ^ String.concat ", " (List.map string_of_bind tbs) ^ "> "

let string_of_kind = function
  | Def (tbs, t) ->
    sprintf "= %s%s" (string_of_binds tbs) (string_of_typ t)
  | Abs (tbs, t) -> 
    sprintf "<: %s%s" (string_of_binds tbs) (string_of_typ t) 


(* First-order substitution *)

type subst = typ Con.Env.t

let rec subst sigma t =
  if sigma = Con.Env.empty then t else
  match t with
  | Prim p -> Prim p
  | Var (c, ts) ->
    (match Con.Env.find_opt c sigma with
    | Some t -> assert (List.length ts = 0); t
    | None -> Var (c, List.map (subst sigma) ts)
    )
  | Array t -> Array (subst sigma t)
  | Tup ts -> Tup (List.map (subst sigma) ts)
  | Func (tbs, t1, t2) ->
    let sigma', tbs' = rename_binds sigma tbs in
    Func (tbs', subst sigma' t1, subst sigma' t2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Like t -> Like (subst sigma t)
  | Obj (a, fs) -> Obj (a, List.map (subst_field sigma) fs)
  | Mut t -> Mut (subst sigma t)
  | Any -> Any
  | Pre -> Pre

and subst_field sigma {name; typ} =
  {name; typ = subst sigma typ}

and rename_binds sigma = function
  | [] -> sigma, []
  | {con; bound}::binds ->
    let con' = Con.fresh (Con.name con) in
    let sigma' = Con.Env.add con (Var (con', [])) sigma in
    let sigma'', binds' = rename_binds sigma' binds in
    sigma'', {con = con'; bound = subst sigma'' bound}::binds'

let subst_binds sigma binds = snd (rename_binds sigma binds)

let make_subst =
  List.fold_left2 (fun sigma t tb -> Con.Env.add tb.con t sigma) Con.Env.empty


(* Normalization *)

let rec normalize env = function
  | Var (con, ts) as t ->
    (match Con.Env.find_opt con env with
    | Some (Def (tbs, t)) -> normalize env (subst (make_subst ts tbs) t)
    | Some _ -> t
    | None -> assert false
    )
  | t -> t

let rec structural env = function
  | Var (con, ts) ->
    (match Con.Env.find_opt con env with
    | Some (Def (tbs, t) | Abs (tbs, t)) ->
      structural env (subst (make_subst ts tbs) t)
    | None -> assert false
    )
  | Like t -> structural env t (*TBR*)
  | t -> t

let immutable = function
  | Mut t -> t
  | t -> t


(* Equivalence *)

(* TBR: Use something more efficient for eqs than an associ list? *)
(* TBR: Thread back eqs so that we don't check multiple times *)

let eq_list eq env eqs xs1 xs2 =
  try List.for_all2 (eq env eqs) xs1 xs2 with Invalid_argument _ -> false
  
let rec eq (env : con_env) t1 t2 : bool =
  eq_typ env [] t1 t2

and eq_typ env (eqs : (typ * typ list) list) t1 t2 =
(*printf "[eq] %s == %s\n" (string_of_typ t1) (string_of_typ t2); flush_all();*)
  match List.assq_opt t1 eqs with
  | Some ts when List.memq t2 ts -> true (* physical equivalence! *)
  | Some ts -> eq_typ' env ((t1, t2::ts)::eqs) t1 t2
  | None -> eq_typ' env ((t1, [t2])::eqs) t1 t2

and eq_typ' env (eqs : (typ * typ list) list) t1 t2 =
  match t1, t2 with
  | Var (con1, ts1), Var (con2, ts2) ->
    con1 = con2 && eq_list eq_typ env eqs ts1 ts2 ||
    (match Con.Env.find_opt con1 env, Con.Env.find_opt con2 env with
    | Some (Def (tbs, t)), _ -> (* TBR this may fail to terminate *)
      eq_typ env eqs (subst (make_subst ts1 tbs) t) t2
    | _, Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (subst (make_subst ts2 tbs) t)
    | _ -> false
    )
  | Var (con1, ts1), t2 ->
    (match Con.Env.find_opt con1 env with
    | Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs (subst (make_subst ts1 tbs) t) t2
    | _ -> false
    )
  | t1, Var (con2, ts2) ->
    (match Con.Env.find_opt con2 env with
    | Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (subst (make_subst ts2 tbs) t)
    | _ -> false
    )
  | Prim p1, Prim p2 ->
    p1 = p2
  | Obj (a1, tfs1), Obj (a2, tfs2) ->
    a1 = a2 &&
    (* assuming tf1 and tf2 are sorted by var *)
    eq_list eq_field env eqs tfs1 tfs2
  | Array t1, Array t2 ->
    eq_typ env eqs t1 t2
  | Opt (t1), Opt (t2) ->
    eq_typ env eqs t1 t2
  | Tup (ts1), Tup (ts2) ->
    eq_list eq_typ env eqs ts1 ts2
  | Func (tbs1, t11, t12), Func (tbs2, t21, t22) ->
    (match eq_binds env eqs tbs1 tbs2 with
    | Some env' -> eq_typ env' eqs t11 t21 && eq_typ env' eqs t12 t22
    | None -> false
    )
  | Async t1, Async t2 ->
    eq_typ env eqs t1 t2
  | Like t1, Like t2 ->
    eq_typ env eqs t1 t2
  | Mut t1, Mut t2 ->
    eq_typ env eqs t1 t2
  | Any, Any -> true
  | _, _ -> false

and eq_field env eqs tf1 tf2 =
  tf1.name = tf2.name &&
  eq_typ env eqs tf1.typ tf2.typ

and eq_binds env eqs tbs1 tbs2 =
  match tbs1, tbs2 with
  | [], [] -> Some env
  | tb1::tbs1', tb2::tbs2' when eq_typ env eqs tb1.bound tb2.bound ->
    let env' = Con.Env.add tb1.con (Def ([], Var (tb2.con, []))) env in
    eq_binds env' eqs tbs1' tbs2'
  | _, _ -> None


(* Environments *)

module Env = Env.Make(String) 
