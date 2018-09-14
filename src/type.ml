(* Representation *)

type con = Con.t
type obj_sort = Object | Actor
type func_sort = Call | Construct
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
  | Obj of obj_sort * field list              (* object *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of func_sort * bind list * typ * typ (* function *)
  | Async of typ                              (* future *)
  | Like of typ                               (* expansion *)
  | Mut of typ                                (* mutable type *)
  | Class                                     (* class *)
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

let prim = function
  | "Null" -> Null
  | "Bool" -> Bool
  | "Nat" -> Nat
  | "Int" -> Int
  | "Word8" -> Word8
  | "Word16" -> Word16
  | "Word32" -> Word32
  | "Word64" -> Word64
  | "Float" -> Float
  | "Char" -> Char
  | "Text" -> Text
  | _ -> raise (Invalid_argument "Type.prim")

let iter_obj t =
  Obj (Object, [{name = "next"; typ = Func (Call, [], unit, Opt t)}])

let array_obj t =
  let immut t =
    [ {name = "get";  typ = Func (Call, [], Prim Nat, t)};
      {name = "len";  typ = Func (Call, [], unit, Prim Nat)};
      {name = "keys"; typ = Func (Call, [], unit, iter_obj (Prim Nat))};
      {name = "vals"; typ = Func (Call, [], unit, iter_obj t)};
    ] in
  let mut t = immut t @
    [ {name = "set"; typ = Func (Call, [], Tup [Prim Nat; t], unit)} ] in
  match t with
  | Mut t' -> Obj (Object, List.sort compare (mut t'))
  | t -> Obj (Object, List.sort compare (immut t))


(* Shifting *)

let rec shift i n t =
  match t with
  | Prim _ -> t
  | Var (s, j) -> Var (s, if j < i then j else j + n)
  | Con (c, ts) -> Con (c, List.map (shift i n) ts)
  | Array t -> Array (shift i n t)
  | Tup ts -> Tup (List.map (shift i n) ts)
  | Func (s, tbs, t1, t2) ->
    let i' = i + List.length tbs in
    Func (s, List.map (shift_bind i' n) tbs, shift i' n t1, shift i' n t2)
  | Opt t -> Opt (shift i n t)
  | Async t -> Async (shift i n t)
  | Like t -> Like (shift i n t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field n i) fs)
  | Mut t -> Mut (shift i n t)
  | Class -> Class
  | Any -> Any
  | Pre -> Pre

and shift_bind i n {var; bound} =
  {var; bound = shift i n bound}

and shift_field i n {name; typ} =
  {name; typ = shift i n typ}


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
  | Func (s, tbs, t1, t2) ->
    let sigma' = Con.Env.map (shift 0 (List.length tbs)) sigma in
    Func (s, List.map (subst_bind sigma') tbs, subst sigma' t1, subst sigma' t2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Like t -> Like (subst sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field sigma) fs)
  | Mut t -> Mut (subst sigma t)
  | Class -> Class
  | Any -> Any
  | Pre -> Pre

and subst_bind sigma {var; bound} =
  {var; bound = subst sigma bound}

and subst_field sigma {name; typ} =
  {name; typ = subst sigma typ}


(* Handling binders *)

let close cs t =
  if cs = [] then t else
  let ts = List.mapi (fun i c -> Var (Con.name c, i)) cs in
  let sigma = List.fold_right2 Con.Env.add cs ts Con.Env.empty in
  subst sigma t

let close_binds cs tbs =
  if cs = [] then tbs else
  List.map (fun {var; bound} -> {var; bound = close cs bound}) tbs


let rec open' i ts t =
  match t with
  | Prim _ -> t
  | Var (_, j) -> if j < i then t else List.nth ts (j - i)
  | Con (c, ts') -> Con (c, List.map (open' i ts) ts')
  | Array t -> Array (open' i ts t)
  | Tup ts' -> Tup (List.map (open' i ts) ts')
  | Func (s, tbs, t1, t2) ->
    let i' = i + List.length tbs in
    Func (s, List.map (open_bind i' ts) tbs, open' i' ts t1, open' i' ts t2)
  | Opt t -> Opt (open' i ts t)
  | Async t -> Async (open' i ts t)
  | Like t -> Like (open' i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field i ts) fs)
  | Mut t -> Mut (open' i ts t)
  | Class -> Class
  | Any -> Any
  | Pre -> Pre

and open_bind i ts {var; bound} =
  {var; bound = open' i ts bound}

and open_field i ts {name; typ} =
  {name; typ = open' i ts typ}

let open_ ts t =
  if ts = [] then t else
  open' 0 ts t

let open_binds env tbs =
  if tbs = [] then [], env else
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
  | Mut t -> Mut (normalize env t)
  | t -> t

let rec structural env = function
  | Con (con, ts) ->
    (match Con.Env.find_opt con env with
    | Some (Def (tbs, t) | Abs (tbs, t)) -> structural env (reduce tbs t ts)
    | None -> assert false
    )
  | Like t -> structural env t (*TBR*)
  | t -> t


(* Projections *)

let is_prim p = function Prim p' -> p = p' | _ -> false
let is_obj = function Obj _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_opt = function Opt _ -> true | _ -> false
let is_tup = function Tup _ -> true | _ -> false
let is_unit = function Tup [] -> true | _ -> false
let is_pair = function Tup [_; _] -> true | _ -> false
let is_func = function Func _ -> true | _ -> false
let is_async = function Async _ -> true | _ -> false
let is_mut = function Mut _ -> true | _ -> false

let invalid s = raise (Invalid_argument ("Type." ^ s))

let as_prim p = function Prim p' when p = p' -> () | _ -> invalid "as_prim"
let as_obj = function Obj (s, tfs) -> s, tfs | _ -> invalid "as_obj"
let as_array = function Array t -> t | _ -> invalid "as_array"
let as_opt = function Opt t -> t | _ -> invalid "as_opt"
let as_tup = function Tup ts -> ts | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [t1; t2] -> t1, t2 | _ -> invalid "as_pair"
let as_func = function Func (s, tbs, t1, t2) -> s, tbs, t1, t2 | _ -> invalid "as_func"
let as_async = function Async t -> t | _ -> invalid "as_async"
let as_mut = function Mut t -> t | _ -> invalid "as_mut"
let as_immut = function Mut t -> t | t -> t

let as_prim_sub p env t = match structural env t with
  Prim p' when p = p' -> () | _ -> invalid "as_prim_sub"
let rec as_obj_sub env t = match structural env t with
  Obj (s, tfs) -> s, tfs | Array t -> as_obj_sub env (array_obj t) | _ -> invalid "as_obj_sub"
let as_array_sub env t = match structural env t with
  Array t -> t | _ -> invalid "as_array_sub"
let as_opt_sub env t = match structural env t with
  Opt t -> t | t -> t
let as_tup_sub env t = match structural env t with
  Tup ts -> ts | _ -> invalid "as_tup_sub"
let as_unit_sub env t = match structural env t with
  Tup [] -> () | _ -> invalid "as_unit_sub"
let as_pair_sub env t = match structural env t with
  Tup [t1; t2] -> t1, t2 | _ -> invalid "as_pair_sub"
let as_func_sub env t = match structural env t with
  Func (_, tbs, t1, t2) -> tbs, t1, t2 | _ -> invalid "as_func_sub"
let as_mono_func_sub env t = match structural env t with
  Func (_, [], t1, t2) -> t1, t2 | _ -> invalid "as_func_sub"
let as_async_sub env t = match structural env t with
  Async t -> t | _ -> invalid "as_async_sub"

let lookup_field name' tfs =
  match List.find_opt (fun {name; _} -> name = name') tfs with
  | Some {typ = t; _} -> t
  | None -> invalid "lookup_field"


(* Avoiding local constructors *)

exception Unavoidable of con

let rec avoid' env env' = function
  | (Prim _ | Var _ | Any | Class | Pre) as t -> t
  | Con (c, ts) ->
    (match Con.Env.find_opt c env' with
    | Some (Abs _) -> raise (Unavoidable c)
    | Some (Def (tbs, t)) -> avoid' env env' (reduce tbs t ts)
    | None ->
      try
        Con (c, List.map (avoid' env env') ts)
      with Unavoidable _ ->
        match Con.Env.find c env with
        | Abs _ -> raise (Unavoidable c)
        | Def (tbs, t) -> avoid' env env' (reduce tbs t ts)
    )
  | Array t -> Array (avoid' env env' t)
  | Tup ts -> Tup (List.map (avoid' env env') ts)
  | Func (s, tbs, t1, t2) ->
    Func (
      s,
      List.map (avoid_bind env env') tbs,
      avoid' env env' t1, avoid' env env' t2
    )
  | Opt t -> Opt (avoid' env env' t)
  | Async t -> Async (avoid' env env' t)
  | Like t -> Like (avoid' env env' t)
  | Obj (s, fs) -> Obj (s, List.map (avoid_field env env') fs)
  | Mut t -> Mut (avoid' env env' t)

and avoid_bind env env' {var; bound} =
  {var; bound = avoid' env env' bound}

and avoid_field env env' {name; typ} =
  {name; typ = avoid' env env' typ}

let avoid env env' t =
  if env' = Con.Env.empty then t else
  avoid' env env' t


(* Equivalence & Subtyping *)

module S = Set.Make (struct type t = typ * typ let compare = compare end)

let rel_list p env rel eq xs1 xs2 =
  try List.for_all2 (p env rel eq) xs1 xs2 with Invalid_argument _ -> false

let rec rel_typ env rel eq t1 t2 =
(*printf "[sub] %s == %s\n" (string_of_typ t1) (string_of_typ t2); flush_all();*)
  t1 == t2 || S.mem (t1, t2) !rel || begin
  rel := S.add (t1, t2) !rel;
  match t1, t2 with
  | Any, Any -> 
    true
  | _, Any when rel != eq ->
    true
  | Con (con1, ts1), Con (con2, ts2) ->
    (match Con.Env.find con1 env, Con.Env.find con2 env with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ env rel eq (open_ ts1 t) t2
    | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ env rel eq t1 (open_ ts2 t)
    | _ when con1 = con2 ->
      rel_list eq_typ env rel eq ts1 ts2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ env rel eq (open_ ts1 t) t2
    | _ ->
      false
    )
  | Con (con1, ts1), t2 ->
    (match Con.Env.find con1 env, t2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ env rel eq (open_ ts1 t) t2
    | Abs (tbs, t), Opt t2' when rel != eq ->
      rel_typ env rel eq t1 t2'
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ env rel eq (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (con2, ts2) ->
    (match Con.Env.find con2 env with
    | Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ env rel eq t1 (open_ ts2 t)
    | _ -> false
    )
  | Prim p1, Prim p2 when p1 = p2 ->
    true
  | Prim p1, Prim p2 when rel != eq ->
    p1 = Nat && p2 = Int
  | Obj (a1, tfs1), Obj (a2, tfs2) ->
    a1 = a2 &&
    rel_fields env rel eq tfs1 tfs2
  | Array t1', Array t2' ->
    rel_typ env rel eq t1' t2'
  | Array t1', Obj _ when rel != eq ->
    rel_typ env rel eq (array_obj t1') t2
  | Opt t1', Opt t2' ->
    rel_typ env rel eq t1' t2'
  | Prim Null, Opt t2' when rel != eq ->
    true
  | t1, Opt t2' when rel != eq ->
    rel_typ env rel eq t1 t2'
  | Tup ts1, Tup ts2 ->
    rel_list rel_typ env rel eq ts1 ts2
  | Func (s1, tbs1, t11, t12), Func (s2, tbs2, t21, t22) ->
    (s1 = s2 || rel != eq && s2 = Call) &&
    (match rel_binds env rel eq tbs1 tbs2 with
    | Some (ts, env') ->
      rel_typ env' rel eq (open_ ts t21) (open_ ts t11) &&
      rel_typ env' rel eq (open_ ts t12) (open_ ts t22)
    | None -> false
    )
  | Func (Construct, _, _, _), Class when rel != eq ->
    true
  | Class, Class ->
    true
  | Async t1', Async t2' ->
    rel_typ env rel eq t1' t2'
  | Like t1', Like t2' ->
    rel_typ env rel eq t1' t2'
  | Mut t1', Mut t2' ->
    eq_typ env rel eq t1' t2'
  | _, _ -> false
  end

and rel_fields env rel eq tfs1 tfs2 =
  (* Assume that tf1 and tf2 are sorted. *)
  match tfs1, tfs2 with
  | [], [] ->
    true
  | _, [] when rel != eq ->
    true
  | tf1::tfs1', tf2::tfs2' ->
    (match compare tf1.name tf2.name with
    | 0 ->
      rel_typ env rel eq tf1.typ tf2.typ &&
      rel_fields env rel eq tfs1' tfs2'
    | -1 when rel != eq ->
      rel_fields env rel eq tfs1' tfs2
    | _ -> false
    )
  | _, _ -> false

and rel_binds env rel eq tbs1 tbs2 =
  let ts, env' = open_binds env tbs2 in
  if rel_list (rel_bind ts) env' rel eq tbs2 tbs1
  then Some (ts, env')
  else None

and rel_bind ts env rel eq tb1 tb2 =
  rel_typ env rel eq (open_ ts tb1.bound) (open_ ts tb2.bound)

and eq_typ env rel eq t1 t2 = rel_typ env eq eq t1 t2

and eq (env : con_env) t1 t2 : bool =
  let eq = ref S.empty in eq_typ env eq eq t1 t2
and sub (env : con_env) t1 t2 : bool =
  rel_typ env (ref S.empty) (ref S.empty) t1 t2


(* Join and Meet *)

let rec join env t1 t2 =
  if t1 == t2 then t1 else
  (* TBR: this is just a quick hack *)
  match normalize env t1, normalize env t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any
  | Any, _ -> Any
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Int
  | Opt t1', Opt t2' -> Opt (join env t1' t2')
  | t1', Opt t2'
  | Opt t1', t2' -> Opt (join env t1' t2')
  | Array t1', (Obj _ as t2) -> join env (array_obj t1') t2
  | (Obj _ as t1), Array t2' -> join env t1 (array_obj t2')
  | t1', t2' when eq env t1' t2' -> t1
  | _ -> Any


let rec meet env t1 t2 =
  if t1 == t2 then t1 else
  (* TBR: this is just a quick hack *)
  match normalize env t1, normalize env t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any -> t1
  | Any, _ -> t2
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Nat
  | Opt t1', Opt t2' -> Opt (meet env t1' t2')
  | t1', Opt t2'
  | Opt t1', t2' -> meet env t1' t2'
  | t1', t2' when eq env t1' t2' -> t1
  | _ -> failwith "meet"  (* TBR *)


(* Pretty printing *)

open Printf

let string_of_prim = function
  | Null -> "Null"
  | Bool -> "Bool"
  | Nat -> "Nat"
  | Int -> "Int"
  | Float -> "Float"
  | Word8 -> "Word8"
  | Word16 -> "Word16"
  | Word32 -> "Word32"
  | Word64 -> "Word64"
  | Char -> "Char"
  | Text -> "Text"

let string_of_var (x, i) =
  if i = 0 then sprintf "%s" x else sprintf "%s.%d" x i

let string_of_con vs c =
  let s = Con.to_string c in
  if List.mem (s, 0) vs then s ^ "/0" else s  (* TBR *)

let string_of_func_sort = function
  | Call -> ""
  | Construct -> "class "

let rec string_of_typ_nullary vs = function
  | Pre -> "???"
  | Any -> "Any"
  | Class -> "Class"
  | Prim p -> string_of_prim p
  | Var (s, i) -> string_of_var (List.nth vs i)
  | Con (c, []) -> string_of_con vs c
  | Con (c, ts) ->
    sprintf "%s<%s>" (string_of_con vs c)
      (String.concat ", " (List.map (string_of_typ' vs) ts))
  | Tup ts ->
    sprintf "(%s%s)"
      (String.concat ", " (List.map (string_of_typ' vs) ts))
      (if List.length ts = 1 then "," else "")
  | Obj (Object, fs) ->
    sprintf "{%s}" (String.concat "; " (List.map (string_of_field vs) fs))
  | t -> sprintf "(%s)" (string_of_typ' vs t)

and string_of_typ' vs t =
  match t with
  | Array (Mut t) ->
    sprintf "var %s[]" (string_of_typ_nullary vs t)
  | Array t ->
    sprintf "%s[]" (string_of_typ_nullary vs t)
  | Func (s, [], t1, t2) ->
    sprintf "%s%s -> %s" (string_of_func_sort s)
      (string_of_typ_nullary vs t1) (string_of_typ' vs t2)
  | Func (s, tbs, t1, t2) ->
    let vs' = names_of_binds vs tbs in
    sprintf "%s%s%s -> %s"
      (string_of_func_sort s) (string_of_binds (vs' @ vs) vs' tbs)
      (string_of_typ_nullary (vs' @ vs) t1) (string_of_typ' (vs' @ vs) t2)
  | Opt t ->
    sprintf "%s?"  (string_of_typ_nullary vs t)
  | Async t ->
    sprintf "async %s" (string_of_typ_nullary vs t)
  | Like t ->
    sprintf "like %s" (string_of_typ_nullary vs t)
  | Obj (Actor, fs) ->
    sprintf "actor %s" (string_of_typ_nullary vs (Obj (Object, fs)))
  | Mut t ->
    sprintf "var %s" (string_of_typ' vs t)
  | t -> string_of_typ_nullary vs t

and string_of_field vs {name; typ} =
  sprintf "%s : %s" name (string_of_typ' vs typ)

and names_of_binds vs bs =
  List.map (fun b -> name_of_var vs (b.var, 0)) bs

and name_of_var vs v =
  match vs with
  | [] -> v
  | v'::vs' -> name_of_var vs' (if v = v' then (fst v, snd v + 1) else v)

and string_of_bind vs v {bound; _} =
  string_of_var v ^
  (if bound = Any then "" else " <: " ^ string_of_typ' vs bound)

and string_of_binds vs vs' = function
  | [] -> ""
  | tbs -> "<" ^ String.concat ", " (List.map2 (string_of_bind vs) vs' tbs) ^ ">"

let string_of_typ = string_of_typ' []


let strings_of_kind k =
  let op, tbs, t =
    match k with
    | Def (tbs, t) -> "=", tbs, t
    | Abs (tbs, t) -> "<:", tbs, t
  in
  let vs = names_of_binds [] tbs in
  op, string_of_binds vs vs tbs, string_of_typ' vs t

let string_of_kind k =
  let op, sbs, st = strings_of_kind k in
  sprintf "%s %s%s" op sbs st


let rec string_of_typ_expand env t =
  let s = string_of_typ t in
  match t with
  | Con (c, ts) ->
    (match Con.Env.find c env with
    | Abs _ -> s
    | Def _ ->
      match normalize env t with
      | Prim _ -> s
      | t' -> s ^ " = " ^ string_of_typ_expand env t'
    )
  | _ -> s


(* Environments *)

module Env = Env.Make(String) 
