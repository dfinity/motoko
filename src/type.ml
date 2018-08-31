(* Representation *)

type con = Con.t
type sort = Object | Actor
type eff = Triv | Await

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
  | Var of string * int                       (* variable *)
  | Con of con * typ list                     (* constructor *)
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

and bind = {var : string; bound : typ}
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
  | Var (s, 0) -> sprintf "%s" s
  | Var (s, i) -> sprintf "%s.%d" s i
  | Con (c, []) -> Con.to_string c
  | Con (c, ts) ->
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

and string_of_bind {var; bound} =
  var ^ (if bound = Any then "" else " <: " ^ string_of_typ bound)

and string_of_binds = function
  | [] -> ""
  | tbs -> "<" ^ String.concat ", " (List.map string_of_bind tbs) ^ ">"

let string_of_kind = function
  | Def (tbs, t) ->
    sprintf "= %s%s" (string_of_binds tbs) (string_of_typ t)
  | Abs (tbs, t) -> 
    sprintf "<: %s%s" (string_of_binds tbs) (string_of_typ t)

let strings_of_kind = function
  | Def (tbs, t) ->
    "=", string_of_binds tbs, string_of_typ t
  | Abs (tbs, t) -> 
    "<:", string_of_binds tbs, string_of_typ t


(* Shifting *)

let rec shift i t =
  match t with
  | Prim _ -> t
  | Var (s, j) -> Var (s, if j < i then j else j + 1)
  | Con (c, ts) -> Con (c, List.map (shift i) ts)
  | Array t -> Array (shift i t)
  | Tup ts -> Tup (List.map (shift i) ts)
  | Func (tbs, t1, t2) ->
    let i' = i + 1 in
    Func (List.map (shift_bind i') tbs, shift i' t1, shift i' t2)
  | Opt t -> Opt (shift i t)
  | Async t -> Async (shift i t)
  | Like t -> Like (shift i t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field i) fs)
  | Mut t -> Mut (shift i t)
  | Any -> Any
  | Pre -> Pre

and shift_bind i {var; bound} =
  {var; bound = shift i bound}

and shift_field i {name; typ} =
  {name; typ = shift i typ}


(* First-order substitution *)

let rec subst sigma t =
  if sigma = Con.Env.empty then t else
  match t with
  | Prim _
  | Var _ -> t
  | Con (c, ts) ->
    (match Con.Env.find_opt c sigma with
    | Some t -> assert (List.length ts = 0); t
    | None -> Con (c, List.map (subst sigma) ts)
    )
  | Array t -> Array (subst sigma t)
  | Tup ts -> Tup (List.map (subst sigma) ts)
  | Func (tbs, t1, t2) ->
    let sigma' = Con.Env.map (shift 0) sigma in
    Func (List.map (subst_bind sigma') tbs, subst sigma' t1, subst sigma' t2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Like t -> Like (subst sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field sigma) fs)
  | Mut t -> Mut (subst sigma t)
  | Any -> Any
  | Pre -> Pre

and subst_bind sigma {var; bound} =
  {var; bound = subst sigma bound}

and subst_field sigma {name; typ} =
  {name; typ = subst sigma typ}


(* Handling binders *)

let close cs t =
  let ts = List.map (fun c -> Var (Con.name c, 0)) cs in
  let sigma = List.fold_right2 Con.Env.add cs ts Con.Env.empty in
  subst sigma t

let close_binds cs tbs =
  List.map (fun {var; bound} -> {var; bound = close cs bound}) tbs


let rec open' i ts t =
  if ts = [] then t else
  match t with
  | Prim _ -> t
  | Var (_, j) -> if j < i then t else List.nth ts (j - i)
  | Con (c, ts') -> Con (c, List.map (open' i ts) ts')
  | Array t -> Array (open' i ts t)
  | Tup ts' -> Tup (List.map (open' i ts) ts')
  | Func (tbs, t1, t2) ->
    let i' = i + 1 in
    Func (List.map (open_bind i' ts) tbs, open' i' ts t1, open' i' ts t2)
  | Opt t -> Opt (open' i ts t)
  | Async t -> Async (open' i ts t)
  | Like t -> Like (open' i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field i ts) fs)
  | Mut t -> Mut (open' i ts t)
  | Any -> Any
  | Pre -> Pre

and open_bind i ts {var; bound} =
  {var; bound = open' i ts bound}

and open_field i ts {name; typ} =
  {name; typ = open' i ts typ}

let open_ = open' 0

let open_binds env tbs =
  let cs = List.map (fun {var; _} -> Con.fresh var) tbs in
  let ts = List.map (fun c -> Con (c, [])) cs in
  let ks = List.map (fun {bound; _} -> Abs ([], open_ ts bound)) tbs in
  ts, List.fold_right2 Con.Env.add cs ks env


(* Normalization and Classification *)

let reduce tbs t ts =
  assert (List.length ts = List.length tbs);
  open_ ts t

let rec normalize env = function
  | Con (con, ts) as t ->
    (match Con.Env.find_opt con env with
    | Some (Def (tbs, t)) -> normalize env (reduce tbs t ts)
    | Some _ -> t
    | None -> assert false
    )
  | t -> t

let nonopt env t =
  match normalize env t with
  | Opt t -> normalize env t
  | t -> t

let rec structural env = function
  | Con (con, ts) ->
    (match Con.Env.find_opt con env with
    | Some (Def (tbs, t) | Abs (tbs, t)) -> structural env (reduce tbs t ts)
    | None -> assert false
    )
  | Like t -> structural env t (*TBR*)
  | t -> t

let immutable = function
  | Mut t -> t
  | t -> t


(* Avoiding local constructors *)

exception Unavoidable of con

let rec avoid env env' = function
  | (Prim _ | Var _ | Any | Pre) as t -> t
  | Con (c, ts) ->
    (match Con.Env.find_opt c env' with
    | Some (Abs _) -> raise (Unavoidable c)
    | Some (Def (tbs, t)) -> avoid env env' (reduce tbs t ts)
    | None ->
      try
        Con (c, List.map (avoid env env') ts)
      with Unavoidable _ ->
        match Con.Env.find c env with
        | Abs _ -> raise (Unavoidable c)
        | Def (tbs, t) -> avoid env env' (reduce tbs t ts)
    )
  | Array t -> Array (avoid env env' t)
  | Tup ts -> Tup (List.map (avoid env env') ts)
  | Func (tbs, t1, t2) ->
    Func (List.map (avoid_bind env env') tbs, avoid env env' t1, avoid env env' t2)
  | Opt t -> Opt (avoid env env' t)
  | Async t -> Async (avoid env env' t)
  | Like t -> Like (avoid env env' t)
  | Obj (s, fs) -> Obj (s, List.map (avoid_field env env') fs)
  | Mut t -> Mut (avoid env env' t)

and avoid_bind env env' {var; bound} =
  {var; bound = avoid env env' bound}

and avoid_field env env' {name; typ} =
  {name; typ = avoid env env' typ}


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
  | Var (_, i1), Var (_, i2) ->
    i1 = i2
  | Con (con1, ts1), Con (con2, ts2) ->
    con1 = con2 && eq_list eq_typ env eqs ts1 ts2 ||
    (match Con.Env.find_opt con1 env, Con.Env.find_opt con2 env with
    | Some (Def (tbs, t)), _ -> (* TBR this may fail to terminate *)
      eq_typ env eqs (open_ ts1 t) t2
    | _, Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (open_ ts2 t)
    | _ -> false
    )
  | Con (con1, ts1), t2 ->
    (match Con.Env.find_opt con1 env with
    | Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (con2, ts2) ->
    (match Con.Env.find_opt con2 env with
    | Some (Def (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (open_ ts2 t)
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
    | Some (ts, env') ->
      eq_typ env' eqs (open_ ts t11) (open_ ts t21) &&
      eq_typ env' eqs (open_ ts t12) (open_ ts t22)
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
  if eq_list eq_bind env eqs tbs1 tbs2
  then Some (open_binds env tbs1)
  else None

and eq_bind env eqs tb1 tb2 =
  eq_typ env eqs tb1.bound tb2.bound


(* Subtyping *)

let rec sub env t1 t2 =
  (* TBR: this is just a quick hack *)
  match normalize env t1, normalize env t2 with
  | _, Any -> true
  | Prim Nat, Prim Int -> true
  | Opt t1', Opt t2' -> sub env t1' t2'
  | t1', Opt t2' -> sub env t1' t2'
  | t1', t2' -> eq env t1' t2'


(* Join and Meet *)

let rec join env t1 t2 =
  (* TBR: this is just a quick hack *)
  match normalize env t1, normalize env t2 with
  | _, Any
  | Any, _ -> Any
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Int
  | Opt t1', Opt t2' -> Opt (join env t1' t2')
  | t1', Opt t2'
  | Opt t1', t2' -> Opt (join env t1' t2')
  | t1', t2' when eq env t1' t2' -> t1
  | _ -> Any


let rec meet env t1 t2 =
  (* TBR: this is just a quick hack *)
  match normalize env t1, normalize env t2 with
  | _, Any -> t1
  | Any, _ -> t2
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Nat
  | Opt t1', Opt t2' -> Opt (meet env t1' t2')
  | t1', Opt t2'
  | Opt t1', t2' -> meet env t1' t2'
  | t1', t2' when eq env t1' t2' -> t1
  | _ -> failwith "meet"  (* TBR *)


(* Environments *)

module Env = Env.Make(String) 
