(* Representation *)

type control = Returns | Promises (* Returns a computed value or immediate promise *)
type sharing = Local | Sharable
type obj_sort = Object of sharing | Actor
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
  | Func of sharing * control *
            bind list * typ list * typ list   (* function *)
  | Async of typ                              (* future *)
  | Mut of typ                                (* mutable type *)
  | Shared                                    (* sharable *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Pre                                       (* pre-type *)

and kind_field = kind ref (* abstract, only this module knows its a ref  *)
and con = { con : Con.t; kind : kind_field }

and bind = {var : string; bound : typ}
and field = {name : string; typ : typ}

and kind =
  | Def of bind list * typ
  | Abs of bind list * typ

(* cons *)

(* The con field is a reference to break the recursion in open_binds,
   and to allow the multiple passes in typing *)
let kind con = !(con.kind)

let fresh_con n k = { con = Con.fresh n; kind = ref k }
let modify_kind c f = c.kind := f !(c.kind)

(* field ordering *)

let compare_field {name=n;_} {name=m;_} = compare n m

type con_env = kind Con.Env.t
type con_set = Con.Set.t

let seq ts =
    match ts with
    | [t] -> t
    | ts -> Tup ts

let as_seq t =
    match t with
    | Tup ts -> ts
    | t -> [t]

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
  Obj (Object Local,
    [{name = "next"; typ = Func (Local, Returns, [], [], [Opt t])}])

let array_obj t =
  let immut t =
    [ {name = "get";  typ = Func (Local, Returns, [], [Prim Nat], [t])};
      {name = "len";  typ = Func (Local, Returns, [], [], [Prim Nat])};
      {name = "keys"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat)])};
      {name = "vals"; typ = Func (Local, Returns, [], [], [iter_obj t])};
    ] in
  let mut t = immut t @
    [ {name = "set"; typ = Func (Local, Returns, [], [Prim Nat; t], [])} ] in
  match t with
  | Mut t' -> Obj (Object Local, List.sort compare_field (mut t'))
  | t -> Obj (Object Local, List.sort compare_field (immut t))


(* Shifting *)

let rec shift i n t =
  match t with
  | Prim _ -> t
  | Var (s, j) -> Var (s, if j < i then j else j + n)
  | Con (c, ts) -> Con (c, List.map (shift i n) ts)
  | Array t -> Array (shift i n t)
  | Tup ts -> Tup (List.map (shift i n) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (shift_bind i' n) tbs, List.map (shift i' n) ts1, List.map (shift i' n) ts2)
  | Opt t -> Opt (shift i n t)
  | Async t -> Async (shift i n t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field n i) fs)
  | Mut t -> Mut (shift i n t)
  | Shared -> Shared
  | Any -> Any
  | Non -> Non
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
    (match Con.Env.find_opt c.con sigma with
    | Some t -> assert (List.length ts = 0); t
    | None -> Con (c, List.map (subst sigma) ts)
    )
  | Array t -> Array (subst sigma t)
  | Tup ts -> Tup (List.map (subst sigma) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let sigma' = Con.Env.map (shift 0 (List.length tbs)) sigma in
    Func (s, c, List.map (subst_bind sigma') tbs,
          List.map (subst sigma') ts1, List.map (subst sigma') ts2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field sigma) fs)
  | Mut t -> Mut (subst sigma t)
  | Shared -> Shared
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and subst_bind sigma {var; bound} =
  {var; bound = subst sigma bound}

and subst_field sigma {name; typ} =
  {name; typ = subst sigma typ}


(* Handling binders *)

let close cs t =
  if cs = [] then t else
  let cons = List.map (fun c -> c.con) cs in
  let ts = List.mapi (fun i c -> Var (Con.name c, i)) cons in
  let sigma = List.fold_right2 Con.Env.add cons ts Con.Env.empty in
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
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (open_bind i' ts) tbs, List.map (open' i' ts) ts1, List.map (open' i' ts) ts2)
  | Opt t -> Opt (open' i ts t)
  | Async t -> Async (open' i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field i ts) fs)
  | Mut t -> Mut (open' i ts t)
  | Shared -> Shared
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and open_bind i ts {var; bound} =
  {var; bound = open' i ts bound}

and open_field i ts {name; typ} =
  {name; typ = open' i ts typ}

let open_ ts t =
  if ts = [] then t else
  open' 0 ts t

let open_binds tbs =
  if tbs = [] then [] else
  let cs = List.map (fun {var; _} -> Con.fresh var) tbs in
  let ps = List.map (fun _ -> ref (Abs ([],Pre))) tbs in
  let ts = List.map2 (fun con kind-> Con ({con;kind}, [])) cs ps in
  let ks = List.map (fun {bound; _} -> Abs ([], open_ ts bound)) tbs in
  List.iter2 (fun p k -> p := k) ps ks;
  ts


(* Normalization and Classification *)

let reduce tbs t ts =
  assert (List.length ts = List.length tbs);
  open_ ts t

let rec normalize = function
  | Con (con, ts) as t ->
    (match kind con with
    | Def (tbs, t) -> normalize (reduce tbs t ts)
    | _ -> t
    )
  | Mut t -> Mut (normalize t)
  | t -> t

let rec promote = function
  | Con (con, ts) ->
    (match kind con with
    | Def (tbs, t) | Abs (tbs, t) -> promote (reduce tbs t ts)
    )
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
let as_func = function Func (s, c, tbs, ts1, ts2) -> s, c, tbs, ts1, ts2 | _ -> invalid "as_func"
let as_async = function Async t -> t | _ -> invalid "as_async"
let as_mut = function Mut t -> t | _ -> invalid "as_mut"
let as_immut = function Mut t -> t | t -> t

let as_prim_sub p t = match promote t with
  | Prim p' when p = p' -> ()
  | Non -> ()
  | _ -> invalid "as_prim_sub"
let rec as_obj_sub name t = match promote t with
  | Obj (s, tfs) -> s, tfs
  | Array t -> as_obj_sub name (array_obj t)
  | Non -> Object Sharable, [{name; typ = Non}]
  | _ -> invalid "as_obj_sub"
let as_array_sub t = match promote t with
  | Array t -> t
  | Non -> Non
  | _ -> invalid "as_array_sub"
let as_opt_sub t = match promote t with
  | Opt t -> t
  | _ -> invalid "as_opt_sub"
let as_tup_sub n t = match promote t with
  | Tup ts -> ts
  | Non -> Lib.List.make n Non
  | _ -> invalid "as_tup_sub"
let as_unit_sub t = match promote t with
  | Tup []
  | Non -> ()
  | _ -> invalid "as_unit_sub"
let as_pair_sub t = match promote t with
  | Tup [t1; t2] -> t1, t2
  | Non -> Non, Non
  | _ -> invalid "as_pair_sub"
let as_func_sub n t = match promote t with
  | Func (_, _, tbs, ts1, ts2) -> tbs, seq ts1,  seq ts2
  | Non -> Lib.List.make n {var = "X"; bound = Any}, Any, Non
  | _ -> invalid "as_func_sub"
let as_mono_func_sub t = match promote t with
  | Func (_, _, [], ts1, ts2) -> seq ts1, seq ts2
  | Non -> Any, Non
  | _ -> invalid "as_func_sub"
let as_async_sub t = match promote t with
  | Async t -> t
  | Non -> Non
  | _ -> invalid "as_async_sub"

let lookup_field name' tfs =
  match List.find_opt (fun {name; _} -> name = name') tfs with
  | Some {typ = t; _} -> t
  | None -> invalid "lookup_field"


(* Span *)

let rec span = function
  | Var _ | Pre -> assert false
  | Con _ as t -> span (promote t)
  | Prim Null -> Some 1
  | Prim Bool -> Some 2
  | Prim (Nat | Int | Float | Text) -> None
  | Prim Word8 -> Some 0x100
  | Prim Word16 -> Some 0x10000
  | Prim (Word32 | Word64 | Char) -> None  (* for all practical purpuses *)
  | Obj _ | Tup _ | Async _ -> Some 1
  | Array _ | Func _ | Shared | Any -> None
  | Opt _ -> Some 2
  | Mut t -> span t
  | Non -> Some 0


(* Avoiding local constructors *)

exception Unavoidable of con

let rec avoid' env to_avoid = function
  | (Prim _ | Var _ | Any | Non | Shared | Pre) as t -> t
  | Con (c, ts) ->
    if Con.Set.mem c.con to_avoid
    then match kind c with
      | Abs _ -> raise (Unavoidable c)
      | Def (tbs, t) -> avoid' env to_avoid (reduce tbs t ts)
    else
      begin try
        Con (c, List.map (avoid' env to_avoid) ts)
      with Unavoidable _ ->
        begin match Con.Env.find c.con env with
        | Def (tbs, t) -> avoid' env to_avoid (reduce tbs t ts)
        | Abs _ -> assert false (* c can only have parameters if it is a type def, or bound by Pre *)
        end
      end
  | Array t -> Array (avoid' env to_avoid t)
  | Tup ts -> Tup (List.map (avoid' env to_avoid) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    Func (s,
          c,
          List.map (avoid_bind env to_avoid) tbs,
          List.map (avoid' env to_avoid) ts1, List.map (avoid' env to_avoid) ts2)
  | Opt t -> Opt (avoid' env to_avoid t)
  | Async t -> Async (avoid' env to_avoid t)
  | Obj (s, fs) -> Obj (s, List.map (avoid_field env to_avoid) fs)
  | Mut t -> Mut (avoid' env to_avoid t)

and avoid_bind env to_avoid {var; bound} =
  {var; bound = avoid' env to_avoid bound}

and avoid_field env to_avoid {name; typ} =
  {name; typ = avoid' env to_avoid typ}

let avoid env to_avoid t =
  if to_avoid = Con.Set.empty then t else
  avoid' env to_avoid t


(* Equivalence & Subtyping *)

module S = Set.Make (struct type t = typ * typ let compare = compare end)

let rel_list p rel eq xs1 xs2 =
  try List.for_all2 (p rel eq) xs1 xs2 with Invalid_argument _ -> false

let str = ref (fun _ -> failwith "")
let rec rel_typ rel eq t1 t2 =
(*Printf.printf "[sub] %s == %s\n%!" (!str t1) (!str t2);*)
  t1 == t2 || S.mem (t1, t2) !rel || begin
  rel := S.add (t1, t2) !rel;
  match t1, t2 with
  | Any, Any ->
    true
  | _, Any when rel != eq ->
    true
  | Non, Non ->
    true
  | Non, _ when rel != eq ->
    true
  | Con (con1, ts1), Con (con2, ts2) ->
    (match kind con1, kind con2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (open_ ts1 t) t2
    | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (open_ ts2 t)
    | _ when con1.con = con2.con ->
      rel_list eq_typ rel eq ts1 ts2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ ->
      false
    )
  | Con (con1, ts1), t2 ->
    (match kind con1, t2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (open_ ts1 t) t2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (con2, ts2) ->
    (match kind con2 with
    | Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (open_ ts2 t)
    | _ -> false
    )
  | Prim p1, Prim p2 when p1 = p2 ->
    true
  | Prim p1, Prim p2 when rel != eq ->
    p1 = Nat && p2 = Int
  | Prim p1, Shared when rel != eq ->
    true
  | Obj (s1, tfs1), Obj (s2, tfs2) ->
    s1 = s2 &&
    rel_fields rel eq tfs1 tfs2
  | Obj (s, _), Shared when rel != eq ->
    s != Object Local
  | Array t1', Array t2' ->
    rel_typ rel eq t1' t2'
  | Array t1', Obj _ when rel != eq ->
    rel_typ rel eq (array_obj t1') t2
  | Array t, Shared when rel != eq ->
    rel_typ rel eq t Shared
  | Opt t1', Opt t2' ->
    rel_typ rel eq t1' t2'
  | Opt t1', Shared ->
    rel_typ rel eq t1' Shared
  | Prim Null, Opt t2' when rel != eq ->
    true
  | Tup ts1, Tup ts2 ->
    rel_list rel_typ rel eq ts1 ts2
  | Tup ts1, Shared ->
    rel_list rel_typ rel eq ts1 (List.map (fun _ -> Shared) ts1)
  | Func (s1, c1, tbs1, t11, t12), Func (s2, c2, tbs2, t21, t22) ->
    c1 = c2 && s1 = s2 &&
    (match rel_binds rel eq tbs1 tbs2 with
    | Some ts ->
      rel_list rel_typ rel eq (List.map (open_ ts) t21) (List.map (open_ ts) t11) &&
      rel_list rel_typ rel eq (List.map (open_ ts) t12) (List.map (open_ ts) t22)
    | None -> false
    )
  | Func (Sharable, _,  _, _, _), Shared when rel != eq ->
    true
  | Shared, Shared ->
    true
  | Async t1', Async t2' ->
    rel_typ rel eq t1' t2'
  | Mut t1', Mut t2' ->
    eq_typ rel eq t1' t2'
  | _, _ -> false
  end

and rel_fields rel eq tfs1 tfs2 =
  (* Assume that tf1 and tf2 are sorted. *)
  match tfs1, tfs2 with
  | [], [] ->
    true
  | _, [] when rel != eq ->
    true
  | tf1::tfs1', tf2::tfs2' ->
    (match compare_field tf1 tf2 with
    | 0 ->
      rel_typ rel eq tf1.typ tf2.typ &&
      rel_fields rel eq tfs1' tfs2'
    | -1 when rel != eq ->
      rel_fields rel eq tfs1' tfs2
    | _ -> false
    )
  | _, _ -> false

and rel_binds rel eq tbs1 tbs2 =
  let ts = open_binds tbs2 in
  if rel_list (rel_bind ts) rel eq tbs2 tbs1
  then Some ts
  else None

and rel_bind ts rel eq tb1 tb2 =
  rel_typ rel eq (open_ ts tb1.bound) (open_ ts tb2.bound)

and eq_typ rel eq t1 t2 = rel_typ eq eq t1 t2

and eq t1 t2 : bool =
  let eq = ref S.empty in eq_typ eq eq t1 t2
and sub  t1 t2 : bool =
  rel_typ (ref S.empty) (ref S.empty) t1 t2

and eq_kind k1 k2 : bool =
  let eq = ref S.empty in
  match k1, k2 with
  | Def (tbs1, t1), Def (tbs2, t2)
  | Abs (tbs1, t1), Abs (tbs2, t2) ->
  begin match rel_binds eq eq tbs1 tbs2 with
    | Some ts -> eq_typ eq eq  (open_ ts t1) (open_ ts t2)
    | None -> false
  end
  | _ -> false

(* Moved here to use eq_kind *)
let set_kind c k = match !(c.kind) with
  | Abs (_, Pre) -> c.kind := k
  (* This safeguards against mutating redefinitions *)
  | k' when eq_kind k k' -> ()
  | _ -> raise (Invalid_argument "set_kind")

(* Least upper bound and greatest lower bound *)

let rec lub t1 t2 =
  if t1 == t2 then t1 else
  (* TBR: this is just a quick hack *)
  match normalize t1, normalize t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any
  | Any, _ -> Any
  | _, Non -> t1
  | Non, _ -> t2
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Int
  | Opt t1', Opt t2' -> Opt (lub t1' t2')
  | Prim Null, Opt t'
  | Opt t', Prim Null -> Opt t'
  | Array t1', (Obj _ as t2) -> lub (array_obj t1') t2
  | (Obj _ as t1), Array t2' -> lub t1 (array_obj t2')
  | t1', t2' when eq t1' t2' -> t1
  | _ -> Any


let rec glb t1 t2 =
  if t1 == t2 then t1 else
  (* TBR: this is just a quick hack *)
  match normalize t1, normalize t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any -> t1
  | Any, _ -> t2
  | _, Non -> Non
  | Non, _ -> Non
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Nat
  | Opt t1', Opt t2' -> Opt (glb t1' t2')
  | Prim Null, Opt _
  | Opt _, Prim Null -> Prim Null
  | t1', t2' when eq t1' t2' -> t1
  | _ -> Non


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

let string_of_sharing = function
  | Local -> ""
  | Sharable -> "shared "

let rec string_of_typ_nullary vs = function
  | Pre -> "???"
  | Any -> "Any"
  | Non -> "Non"
  | Shared -> "Shared"
  | Prim p -> string_of_prim p
  | Var (s, i) -> (try string_of_var (List.nth vs i) with _ -> assert false)
  | Con (c, []) -> string_of_con vs c.con
  | Con (c, ts) ->
    sprintf "%s<%s>" (string_of_con vs c.con)
      (String.concat ", " (List.map (string_of_typ' vs) ts))
  | Tup ts ->
    sprintf "(%s%s)"
      (String.concat ", " (List.map (string_of_typ' vs) ts))
      (if List.length ts = 1 then "," else "")
  | Array (Mut t) ->
    sprintf "[var %s]" (string_of_typ_nullary vs t)
  | Array t ->
    sprintf "[%s]" (string_of_typ_nullary vs t)
  | Obj (Object Local, fs) ->
    sprintf "{%s}" (String.concat "; " (List.map (string_of_field vs) fs))
  | t -> sprintf "(%s)" (string_of_typ' vs t)

and string_of_dom vs ts =
  let dom = string_of_typ_nullary vs (seq ts) in
  match ts with
  | [Tup _] ->
     sprintf "(%s)" dom
  | _ -> dom

and string_of_cod c vs ts =
  let cod = string_of_typ' vs (seq ts) in
  match ts with
  | [Tup _] ->
    sprintf "(%s)" cod
  | [Async _] ->
    (match c with
     | Returns -> sprintf "(%s)" cod
     | Promises -> sprintf "%s" cod
    )
  | _ -> cod

and string_of_typ' vs t =
  match t with
  | Func (s, c, [], ts1, ts2) ->
    sprintf "%s%s -> %s" (string_of_sharing s)
      (string_of_dom vs ts1)
      (string_of_cod c vs ts2)
  | Func (s, c, tbs, ts1, ts2) ->
    let vs' = names_of_binds vs tbs in
    sprintf "%s%s%s -> %s"
      (string_of_sharing s) (string_of_binds (vs' @ vs) vs' tbs)
      (string_of_dom (vs' @ vs) ts1) (string_of_cod c (vs' @ vs) ts2)
  | Opt t ->
    sprintf "?%s"  (string_of_typ_nullary vs t)
  | Async t ->
    sprintf "async %s" (string_of_typ_nullary vs t)
  | Obj (Object Sharable, fs) ->
    sprintf "shared %s" (string_of_typ_nullary vs (Obj (Object Local, fs)))
  | Obj (Actor, fs) ->
    sprintf "actor %s" (string_of_typ_nullary vs (Obj (Object Local, fs)))
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
let _ = str := string_of_typ

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


let rec string_of_typ_expand t =
  let s = string_of_typ t in
  match t with
  | Con (c, ts) ->
    (match kind c with
    | Abs _ -> s
    | Def _ ->
      match normalize t with
      | Prim _ | Any | Non -> s
      | t' -> s ^ " = " ^ string_of_typ_expand t'
    )
  | _ -> s


(* Environments *)

module Env = Env.Make(String)
