(* Representation *)

type con = Con.t
type mut = ConstMut | VarMut
type actor = Object | Actor

type width =
  | Width8
  | Width16
  | Width32
  | Width64

type prim =
  | NullT
  | BoolT
  | NatT
  | IntT
  | WordT of width
  | FloatT
  | CharT
  | TextT

type typ =
  | VarT of con * typ list                     (* constructor *)
  | PrimT of prim                              (* primitive *)
  | ObjT of actor * typ_field list             (* object *)
  | ArrayT of mut * typ                        (* array *)
  | OptT of typ                                (* option *)
  | TupT of typ list                           (* tuple *)
  | FuncT of typ_bind list * typ * typ         (* function *)
  | AsyncT of typ                              (* future *)
  | LikeT of typ                               (* expansion *)
  | AnyT                                       (* top *)
(*
  | UnionT of type * typ                       (* union *)
  | AtomT of string                            (* atom *)
*)

and typ_bind = {con : con; bound : typ}
and typ_field = {lab : string; typ : typ; mut : mut}

type kind =
  | DefK of typ_bind list * typ
  | AbsK of typ_bind list * typ

type con_env = kind Con.Env.t


(* Short-hands *)

let unitT = TupT []
let boolT = PrimT BoolT
let intT = PrimT IntT


(* Pretty printing *)

open Printf

let string_of_mut = function
  | VarMut -> "var "
  | ConstMut -> ""

let string_of_width = function
  | Width8 -> "8"
  | Width16 -> "16"
  | Width32 -> "32"
  | Width64 -> "64"

let string_of_prim = function
  | NullT -> "Null"
  | IntT -> "Int"
  | BoolT -> "Bool"
  | FloatT -> "Float"
  | NatT -> "Nat"
  | WordT w -> "Word" ^ string_of_width w
  | CharT -> "Char"
  | TextT -> "Text"

let rec string_of_typ_nullary = function
  | AnyT -> "Any"
  | PrimT p -> string_of_prim p
  | VarT (c, []) -> Con.to_string c
  | VarT (c, ts) ->
    sprintf "%s<%s>"
      (Con.to_string c) (String.concat ", " (List.map string_of_typ ts))
  | TupT ts ->
    sprintf "(%s)" (String.concat ", " (List.map string_of_typ ts))
  | ObjT (Object, fs) ->
    sprintf "{%s}" (String.concat "; " (List.map string_of_typ_field fs))
  | t ->
    sprintf "(%s)" (string_of_typ t)

and string_of_typ t =
  match t with
  | ArrayT (m, t) ->
    sprintf "%s%s[]" (string_of_mut m) (string_of_typ_nullary t)  
  | FuncT (tbs, t1, t2) ->
    sprintf "%s%s -> %s"
      (string_of_typ_binds tbs) (string_of_typ_nullary t1) (string_of_typ t2)
  | OptT t ->
    sprintf "%s?"  (string_of_typ_nullary t)
  | AsyncT t -> 
    sprintf "async %s" (string_of_typ_nullary t)
  | LikeT t -> 
    sprintf "like %s" (string_of_typ_nullary t)
  | ObjT (Actor, fs) ->
    sprintf "actor %s" (string_of_typ_nullary (ObjT (Object, fs)))
  | t -> string_of_typ_nullary t

and string_of_typ_field {lab; mut; typ} =
  sprintf "%s : %s%s" lab (string_of_mut mut) (string_of_typ typ)

and string_of_typ_bind {con; bound} =
  Con.to_string con ^
  (if bound = AnyT then "" else " <: " ^ string_of_typ bound)

and string_of_typ_binds = function
  | [] -> ""
  | tbs -> "<" ^ String.concat ", " (List.map string_of_typ_bind tbs) ^ "> "

let string_of_kind = function
  | DefK (tbs, t) ->
    sprintf "= %s%s" (string_of_typ_binds tbs) (string_of_typ t)
  | AbsK (tbs, t) -> 
    sprintf "<: %s%s" (string_of_typ_binds tbs) (string_of_typ t) 


(* First-order substitution *)

type subst = typ Con.Env.t

let rec rename_binds sigma = function
  | [] -> sigma, []
  | {con; bound}::binds ->
    let con' = Con.fresh (Con.name con) in
    let sigma' = Con.Env.add con (VarT (con', [])) sigma in
    let rho, binds' = rename_binds sigma' binds in
    rho, {con = con'; bound = subst rho bound}::binds'

and subst sigma t =
  if sigma = Con.Env.empty then t else
  match t with
  | PrimT p -> PrimT p
  | VarT (c, ts) ->
    (match Con.Env.find_opt c sigma with
    | Some t -> assert (List.length ts = 0); t
    | None -> VarT (c, List.map (subst sigma) ts)
    )
  | ArrayT (m, t) ->
    ArrayT (m, subst sigma t)
  | TupT ts ->
    TupT (List.map (subst sigma) ts)
  | FuncT(ts, t1, t2) ->
    let (rho, ts') = rename_binds sigma ts in
    FuncT (ts', subst rho t1, subst rho t2)
  | OptT t ->
    OptT (subst sigma t)
  | AsyncT t ->
    AsyncT (subst sigma t)
  | LikeT t ->
    LikeT (subst sigma t)
  | ObjT (a, fs) ->
    ObjT (a, subst_fields sigma fs)
  | AnyT ->
    AnyT

and subst_fields sigma fs = 
  List.map (fun {lab; mut; typ} -> {lab; mut; typ = subst sigma typ}) fs


let make_subst =
  List.fold_left2 (fun sigma t tb -> Con.Env.add tb.con t sigma) Con.Env.empty


(* Normalization *)

let rec normalize kindenv = function
  | VarT (con, ts) as t ->
    (match Con.Env.find_opt con kindenv with
    | Some (DefK (tbs, t)) -> normalize kindenv (subst (make_subst ts tbs) t)
    | Some _ -> t
    | None -> assert false
    )
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
  | VarT (con1, ts1), VarT (con2, ts2) ->
    con1 = con2 && eq_list eq_typ env eqs ts1 ts2 ||
    (match Con.Env.find_opt con1 env, Con.Env.find_opt con2 env with
    | Some (DefK (tbs, t)), _ -> (* TBR this may fail to terminate *)
      eq_typ env eqs (subst (make_subst ts1 tbs) t) t2
    | _, Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (subst (make_subst ts2 tbs) t)
    | _ -> false
    )
  | VarT (con1, ts1), t2 ->
    (match Con.Env.find_opt con1 env with
    | Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs (subst (make_subst ts1 tbs) t) t2
    | _ -> false
    )
  | t1, VarT (con2, ts2) ->
    (match Con.Env.find_opt con2 env with
    | Some (DefK (tbs, t)) -> (* TBR this may fail to terminate *)
      eq_typ env eqs t1 (subst (make_subst ts2 tbs) t)
    | _ -> false
    )
  | PrimT p1, PrimT p2 ->
    p1 = p2
  | ObjT (a1, tfs1), ObjT (a2, tfs2) ->
    a1 = a2 &&
    (* assuming tf1 and tf2 are sorted by var *)
    eq_list eq_typ_field env eqs tfs1 tfs2
  | ArrayT (m1, t1), ArrayT (m2, t2) ->
    m1 = m2 && eq_typ env eqs t1 t2
  | OptT (t1), OptT (t2) ->
    eq_typ env eqs t1 t2
  | TupT (ts1), TupT (ts2) ->
    eq_list eq_typ env eqs ts1 ts2
  | FuncT (tbs1, t11, t12), FuncT (tbs2, t21, t22) ->
    (match eq_typ_binds env eqs tbs1 tbs2 with
    | Some env' -> eq_typ env' eqs t11 t21 && eq_typ env' eqs t12 t22
    | None -> false
    )
  | AsyncT (t1), AsyncT (t2) ->
    eq_typ env eqs t1 t2
  | LikeT (t1), LikeT (t2) ->
    eq_typ env eqs t1 t2
  | AnyT, AnyT -> true
  | _, _ -> false

and eq_typ_field env eqs (tf1 : typ_field) (tf2 : typ_field) =
  tf1.lab = tf2.lab &&
  tf1.mut = tf2.mut &&
  eq_typ env eqs tf1.typ tf2.typ

and eq_typ_binds env eqs tbs1 tbs2 =
  match tbs1, tbs2 with
  | [], [] -> Some env
  | tb1::tbs1', tb2::tbs2' when eq_typ env eqs tb1.bound tb2.bound ->
    let env' = Con.Env.add tb1.con (DefK ([], VarT (tb2.con, []))) env in
    eq_typ_binds env' eqs tbs1' tbs2'
  | _, _ -> None


(* Environments *)

module Env = Env.Make(String) 
