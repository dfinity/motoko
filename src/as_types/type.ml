(* Representation *)

type lab = string
type var = string

type control = Returns | Promises (* Returns a computed value or immediate promise *)
type obj_sort = Object | Actor | Module
type func_sort = Local | Shared
type eff = Triv | Await

type prim =
  | Null
  | Bool
  | Nat
  | Nat8
  | Nat16
  | Nat32
  | Nat64
  | Int
  | Int8
  | Int16
  | Int32
  | Int64
  | Word8
  | Word16
  | Word32
  | Word64
  | Float
  | Char
  | Text

type t = typ
and typ =
  | Var of var * int                          (* variable *)
  | Free of con                               (* constructor *)
  | Con of typ * typ list                     (* application *)
  | Prim of prim                              (* primitive *)
  | Obj of obj_sort * field list              (* object *)
  | Variant of field list                     (* variant *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Tup of typ list                           (* tuple *)
  | Func of func_sort * control * bind list * typ list * typ list  (* function *)
  | Async of typ                              (* future *)
  | Mut of typ                                (* mutable type *)
  | Serialized of typ                         (* a serialized value *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Typ of con                                (* type (field of module) *)
  | Pre                                       (* pre-type *)

and bind = {var : var; bound : typ}
and field = {lab : lab; typ : typ}

and con = kind Con.t
and kind =
  | Def of bind list * typ
  | Abs of bind list * typ

(* Constructors *)

let set_kind c k =
  match Con.kind c with
  | Abs (_, Pre) -> Con.unsafe_set_kind c k
  | _ -> raise (Invalid_argument "set_kind")

module ConEnv = Env.Make(struct type t = con let compare = Con.compare end)
module ConSet = ConEnv.Dom

(* Short-hands *)

let unit = Tup []
let bool = Prim Bool
let nat = Prim Nat
let int = Prim Int
let text = Prim Text
let char = Prim Char

let prim = function
  | "Null" -> Null
  | "Bool" -> Bool
  | "Nat" -> Nat
  | "Nat8" -> Nat8
  | "Nat16" -> Nat16
  | "Nat32" -> Nat32
  | "Nat64" -> Nat64
  | "Int" -> Int
  | "Int8" -> Int8
  | "Int16" -> Int16
  | "Int32" -> Int32
  | "Int64" -> Int64
  | "Word8" -> Word8
  | "Word16" -> Word16
  | "Word32" -> Word32
  | "Word64" -> Word64
  | "Float" -> Float
  | "Char" -> Char
  | "Text" -> Text
  | _ -> raise (Invalid_argument "Type.prim")

let seq = function [t] -> t | ts -> Tup ts


let compare_field f1 f2 =
  match f1,f2 with
  | {lab = l1; typ = Typ _}, {lab = l2; typ = Typ _ } -> compare l1 l2
  | {lab = l1; typ = Typ _}, {lab = l2; typ = _ } -> -1
  | {lab = l1; typ = _}, {lab = l2; typ = Typ _ } -> 1
  | {lab = l1; typ = _}, {lab = l2; typ = _ } -> compare l1 l2

(* Coercions *)

let iter_obj t =
  Obj (Object,
    [{lab = "next"; typ = Func (Local, Returns, [], [], [Opt t])}])

(* Collecting type constructors *)

let rec cons t cs =
  match t with
  | Var _ ->  cs
  | (Prim _ | Any | Non | Pre) -> cs
  | Free c -> ConSet.add c cs
  | Con (t', ts) ->
    List.fold_right cons ts  (cons t' cs)
  | (Opt t | Async t | Mut t | Serialized t | Array t) ->
    cons t cs
  | Tup ts -> List.fold_right cons ts cs
  | Func (s, c, tbs, ts1, ts2) ->
    let cs = List.fold_right cons_bind tbs  cs in
    let cs = List.fold_right cons ts1 cs in
    List.fold_right cons ts2 cs
  | (Obj (_, fs) | Variant fs) ->
    List.fold_right cons_field fs cs
  | Typ c -> ConSet.add c cs

and cons_bind {var; bound} cs =
  cons bound cs

and cons_field {lab; typ} cs =
  cons typ cs

let cons_kind k =
  match k with
  | Def (tbs, t)
  | Abs (tbs, t) ->
    cons t (List.fold_right cons_bind tbs ConSet.empty)

let rec is_closed seen i t =
  match t with
  | Prim _ -> true
  | Var (_, j) ->  j < i
  | Free c -> is_closed_con seen i c
  | Con (t', ts) -> is_closed seen i t' && List.for_all (is_closed seen i) ts
  | Array t -> is_closed seen i t
  | Tup ts -> List.for_all (is_closed seen i) ts
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    List.for_all (fun {var;bound} -> is_closed seen i' bound) tbs &&
    List.for_all (is_closed seen i') ts1 &&
    List.for_all (is_closed seen i') ts2
  | Opt t -> is_closed seen i t
  | Async t -> is_closed seen i t
  | Obj (s, fs) -> List.for_all (fun {typ;_} -> is_closed seen i typ) fs
  | Variant fs -> List.for_all (fun {typ;_} -> is_closed seen i typ) fs
  | Mut t -> is_closed seen i t
  | Serialized t -> is_closed seen i t
  | Any -> true
  | Non -> true
  | Pre -> true
  | Typ c -> is_closed_con seen i c

and is_closed_kind seen i k =
  match k with
  | Abs (tbs,t) -> (* TBR *)
    assert (List.for_all (fun {var;bound} -> is_closed seen 0 (*!*) bound) tbs &&
              is_closed seen 0 (*!*) t);
    true
  | Def (tbs,t) ->
    let i' = i + 1 + List.length tbs in
    List.for_all (fun {var;bound} -> is_closed seen i' bound) tbs &&
    is_closed seen i' t

and is_closed_con seen i c =
  let k = Con.kind c in
  ConSet.mem c seen || is_closed_kind (ConSet.add c seen) i k

let is_closed_con i c = is_closed_con ConSet.empty i c

(* Shifting *)

let rec shift i n t =
  match t with
  | Prim _ -> t
  | Var (s, j) -> Var (s, if j < i then j else j + n)
  | Free c -> Free (shift_con i n c)
  | Con (t, ts) -> Con (shift i n t, List.map (shift i n) ts)
  | Array t -> Array (shift i n t)
  | Tup ts -> Tup (List.map (shift i n) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (shift_bind i' n) tbs, List.map (shift i' n) ts1, List.map (shift i' n) ts2)
  | Opt t -> Opt (shift i n t)
  | Async t -> Async (shift i n t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field n i) fs)
  | Variant fs -> Variant (List.map (shift_field n i) fs)
  | Mut t -> Mut (shift i n t)
  | Serialized t -> Serialized (shift i n t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c ->
    Typ (shift_con i n c)

and shift_con i n c =
  if is_closed_con i c
  then c
  else clone c (fun k' -> shift_kind i n k')
and shift_bind i n {var; bound} =
  {var; bound = shift i n bound}

and shift_field i n {lab; typ} =
  {lab; typ = shift i n typ}

and shift_kind i n k =
  match k with
  | Def (tbs, t) ->
    let i' = i + 1 + List.length tbs in (* +1 for recursive reference *)
    Def (List.map (shift_bind i' n) tbs, shift i' n t)
  | Abs (tbs, t) ->
    let i' = i + List.length tbs in
    Abs (List.map (shift_bind i' n) tbs, shift i' n t)

(* First-order substitution *)

and subst sigma t =
  if sigma = ConEnv.empty then t else
  match t with
  | Prim _
  | Var _ -> t
  | Free c ->
    (match ConEnv.find_opt c sigma with
     | Some t' -> t'
     | None -> Free (subst_con sigma c))
  | Con (Free c, ts) ->
    (match ConEnv.find_opt c sigma with
     | Some t' ->
       if ts = [] then
         t'
       else
         Con(t', List.map (subst sigma) ts)
     | None -> Con (Free (subst_con sigma c), List.map (subst sigma) ts))
  | Con (t', ts) ->
    Con (subst sigma t', List.map (subst sigma) ts)
  | Array t -> Array (subst sigma t)
  | Tup ts -> Tup (List.map (subst sigma) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let sigma' = ConEnv.map (shift 0 (List.length tbs)) sigma in
    Func (s, c, List.map (subst_bind sigma') tbs,
          List.map (subst sigma') ts1, List.map (subst sigma') ts2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field sigma) fs)
  | Variant fs -> Variant (List.map (subst_field sigma) fs)
  | Mut t -> Mut (subst sigma t)
  | Serialized t -> Serialized (subst sigma t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c ->
    (match ConEnv.find_opt c sigma with
     | Some t -> assert false
     | None -> Typ (subst_con sigma c)
    )

and subst_con sigma c =
  let cons = cons_kind (Con.kind c) in
  if List.exists (fun c -> ConSet.mem c cons) (ConEnv.keys sigma)
  then clone c (fun k -> subst_kind sigma k)
  else c

and clone c f =
  let k = Con.kind c in
  match k with
  | Abs(tbs,t) -> c
  | Def(tbs,t) ->
  let c' = Con.fresh (Con.name c) (f k) in
  c'

and subst_bind sigma {var; bound} =
  {var; bound = subst sigma bound}

and subst_field sigma {lab; typ} =
  {lab; typ = subst sigma typ}

and subst_kind sigma (k:kind) =
  match k with
  | Def (tbs, t) ->
    let sigma' = ConEnv.map (shift 0 (1 + List.length tbs)) sigma in
                                           (* ^ for recursive reference *)
    Def (List.map (subst_bind sigma') tbs, subst sigma' t)
  | Abs (tbs, t) ->
    let sigma' = ConEnv.map (shift 0 (List.length tbs)) sigma in
    Abs (List.map (subst_bind sigma') tbs, subst sigma' t)

(* Handling binders *)

let close cs t =
  if cs = [] then t else
  let ts = List.mapi (fun i c -> Var (Con.name c, i)) cs in
  let sigma = List.fold_right2 ConEnv.add cs ts ConEnv.empty in
  subst sigma t

let close_binds cs tbs =
  if cs = [] then tbs else
  List.map (fun {var; bound} -> {var; bound = close cs bound}) tbs

let rec open' i ts t =
  match t with
  | Prim _ -> t
  | Var (_, j) -> if j < i then t else List.nth ts (j - i)
  | Free c -> Free (open_con i ts c)
  | Con (t', ts') -> Con (open' i ts t', List.map (open' i ts) ts')
  | Array t -> Array (open' i ts t)
  | Tup ts' -> Tup (List.map (open' i ts) ts')
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (open_bind i' ts) tbs, List.map (open' i' ts) ts1, List.map (open' i' ts) ts2)
  | Opt t -> Opt (open' i ts t)
  | Async t -> Async (open' i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field i ts) fs)
  | Variant fs -> Variant (List.map (open_field i ts) fs)
  | Mut t -> Mut (open' i ts t)
  | Serialized t -> Serialized (open' i ts t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c -> Typ (open_con i ts c)

and open_con i ts c =
  if is_closed_con i c
  then c
  else clone c (fun k -> open_kind i ts k)

and open_bind i ts {var; bound} =
  {var; bound = open' i ts bound}

and open_field i ts {lab; typ} =
  {lab; typ = open' i ts typ}

and open_kind i ts k =
  match k with
  | Def (tbs, t) ->
    let i' = i + 1 + List.length tbs in
    Def (List.map (open_bind i' ts) tbs, open' i' ts t)
  | Abs (tbs, t) ->
    let i' = i + List.length tbs in
    Abs (List.map (open_bind i' ts) tbs, open' i' ts t)

let open_ ts t =
  if ts = [] then t else
  open' 0 ts t

let open_binds tbs =
  if tbs = [] then [] else
  let cs = List.map (fun {var; _} -> Con.fresh var (Abs ([], Pre))) tbs in
  let ts = List.map (fun c -> Free c) cs in
  let ks = List.map (fun {bound; _} -> Abs ([], open_ ts bound)) tbs in
  List.iter2 set_kind cs ks;
  ts


let unfold_binds c tbs =
  if tbs = [] then [] else
  let cs = List.map (fun {var; _} -> Con.fresh var (Abs ([], Pre))) tbs in
  let ts = List.map (fun c -> Free c) cs in
  let tbs1 =
    List.map
      (fun {var; bound} -> {var; bound = open_ ((Free c)::ts) bound})
      tbs
  in
  close_binds cs tbs1

(* kind of c, unfolded if necessary *)
let kind c =
  match Con.kind c with
  | Def (tbs, t) ->
    let cs = List.map (fun {var; _} -> Con.fresh var (Abs ([], Pre))) tbs in
    let ts = List.map (fun c -> Free c) cs in
    let tbs' =
      List.map
        (fun tb ->
          {var = tb.var; bound = close cs (open_ (Free c::ts) tb.bound)})
        tbs
    in
    Def (tbs', close cs (open_ (Free c::ts) t))
  | Abs (tbs, t) as k ->
    k

(* Normalization and Classification *)

let reduce c ts =
  match Con.kind c with
  | Def (tbs,t)  ->
    assert (List.length ts = List.length tbs);
    open_ (Free c::ts) t
  | Abs (tbs,t)  ->
    assert (List.length ts = List.length tbs);
    open_ ts t

let rec normalize = function
  | Free c as t ->
    (match Con.kind c with
    | Def (tbs, _) -> normalize (reduce c [])
    | _ -> t
    )
  | Con (Free c, ts) as t ->
    (match Con.kind c with
    | Def (tbs, _) -> normalize (reduce c ts)
    | _ -> t
    )
  | Mut t -> Mut (normalize t)
  | t -> t

let rec promote = function
  | Free c ->
    promote (reduce c [])
  | Con (Free c, ts) ->
    promote (reduce c ts)
  | t -> t


(* Projections *)

let is_non = function Non -> true | _ -> false
let is_prim p = function Prim p' -> p = p' | _ -> false
let is_obj = function Obj _ -> true | _ -> false
let is_variant = function Variant _ -> true | _ -> false
let is_array = function Array _ -> true | _ -> false
let is_opt = function Opt _ -> true | _ -> false
let is_tup = function Tup _ -> true | _ -> false
let is_unit = function Tup [] -> true | _ -> false
let is_pair = function Tup [_; _] -> true | _ -> false
let is_func = function Func _ -> true | _ -> false
let is_async = function Async _ -> true | _ -> false
let is_mut = function Mut _ -> true | _ -> false
let is_serialized = function Serialized _ -> true | _ -> false
let is_typ = function Typ _ -> true | _ -> false

let invalid s = raise (Invalid_argument ("Type." ^ s))

let as_prim p = function Prim p' when p = p' -> () | _ -> invalid "as_prim"
let as_obj = function Obj (s, tfs) -> s, tfs | _ -> invalid "as_obj"
let as_array = function Array t -> t | _ -> invalid "as_array"
let as_opt = function Opt t -> t | _ -> invalid "as_opt"
let as_variant = function Variant fs -> fs | _ -> invalid "as_variant"
let as_tup = function Tup ts -> ts | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [t1; t2] -> t1, t2 | _ -> invalid "as_pair"
let as_func = function Func (s, c, tbs, ts1, ts2) -> s, c, tbs, ts1, ts2 | _ -> invalid "as_func"
let as_async = function Async t -> t | _ -> invalid "as_async"
let as_mut = function Mut t -> t | _ -> invalid "as_mut"
let as_immut = function Mut t -> t | t -> t
let as_serialized = function Serialized t -> t | _ -> invalid "as_serialized"
let as_typ = function Typ c -> c | _ -> invalid "as_typ"

let as_seq = function Tup ts -> ts | t -> [t]

let as_prim_sub p t = match promote t with
  | Prim p' when p = p' -> ()
  | Non -> ()
  | _ -> invalid "as_prim_sub"
let as_obj_sub ls t = match promote t with
  | Obj (s, tfs) -> s, tfs
  | Non -> Object, List.map (fun l -> {lab = l; typ = Non}) ls
  | _ -> invalid "as_obj_sub"
let as_variant_sub l t = match promote t with
  | Variant tfs -> tfs
  | Non -> [{lab = l; typ = Non}]
  | _ -> invalid "as_variant_sub"
let as_array_sub t = match promote t with
  | Array t -> t
  | Non -> Non
  | _ -> invalid "as_array_sub"
let as_opt_sub t = match promote t with
  | Opt t -> t
  | Non -> Non
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
let as_func_sub default_s default_arity t = match promote t with
  | Func (s, _, tbs, ts1, ts2) -> s, tbs, seq ts1,  seq ts2
  | Non -> default_s, Lib.List.make default_arity {var = "X"; bound = Any}, Any, Non
  | _ -> invalid "as_func_sub"
let as_mono_func_sub t = match promote t with
  | Func (_, _, [], ts1, ts2) -> seq ts1, seq ts2
  | Non -> Any, Non
  | _ -> invalid "as_func_sub"
let as_async_sub t = match promote t with
  | Async t -> t
  | Non -> Non
  | _ -> invalid "as_async_sub"


let lookup_val_field l tfs =
  let is_lab = function {typ = Typ _; _} -> false | {lab; _} -> lab = l in
  match List.find_opt is_lab tfs with
  | Some tf -> tf.typ
  | None -> invalid "lookup_val_field"

let lookup_typ_field l tfs =
  let is_lab = function {typ = Typ _; lab} -> lab = l | _ -> false in
  match List.find_opt is_lab tfs with
  | Some {typ = Typ c; _} -> c
  | _ -> invalid "lookup_typ_field"


(* Span *)

let rec span = function
  | Var _ | Pre -> assert false
  | Free c as t -> span (Con (t,[])) (* TBR *)
  | Con _ as t -> span (promote t)
  | Prim Null -> Some 1
  | Prim Bool -> Some 2
  | Prim (Nat | Int | Float | Text) -> None
  | Prim (Nat8 | Int8 | Word8) -> Some 0x100
  | Prim (Nat16 | Int16 | Word16) -> Some 0x10000
  | Prim (Nat32 | Int32 | Word32 | Nat64 | Int64 | Word64 | Char) -> None  (* for all practical purposes *)
  | Obj _ | Tup _ | Async _ -> Some 1
  | Variant fs -> Some (List.length fs)
  | Array _ | Func _ | Any -> None
  | Opt _ -> Some 2
  | Mut t -> span t
  | Serialized t -> None
  | Non -> Some 0
  | Typ _ -> assert false (* TBR *)


(* Avoiding local constructors *)

exception Unavoidable of con

let rec avoid' cons seen = function
  | (Prim _ | Var _ | Any | Non | Pre) as t -> t
  | Free c -> avoid' cons seen (Con (Free c,[])) (* TBR *)
  | Con (Free c, ts) ->
    if ConSet.mem c seen then raise (Unavoidable c) else
    if ConSet.mem c cons
    then match Con.kind c with
      | Abs _ -> raise (Unavoidable c)
      | Def (tbs, t) -> avoid' cons (ConSet.add c seen) (reduce c ts)
    else
      begin try
        Con (Free c, List.map (avoid' cons seen) ts)
      with Unavoidable d ->
        match Con.kind c with
        | Def (tbs, t) -> avoid' cons seen (reduce c ts)
        | Abs _ -> raise (Unavoidable d)
      end
  | Con (t',ts) -> Con(t', List.map (avoid' cons seen) ts)
  | Array t -> Array (avoid' cons seen t)
  | Tup ts -> Tup (List.map (avoid' cons seen) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    Func (s,
          c,
          List.map (avoid_bind cons seen) tbs,
          List.map (avoid' cons seen) ts1, List.map (avoid' cons seen) ts2)
  | Opt t -> Opt (avoid' cons seen t)
  | Async t -> Async (avoid' cons seen t)
  | Obj (s, fs) -> Obj (s, List.map (avoid_field cons seen) fs)
  | Variant fs -> Variant (List.map (avoid_field cons seen) fs)
  | Mut t -> Mut (avoid' cons seen t)
  | Serialized t -> Serialized (avoid' cons seen t)
  | Typ c ->
    if ConSet.mem c seen then raise (Unavoidable c)
    else Typ (clone c (avoid_kind cons seen)) (* TBR *)

and avoid_bind cons seen {var; bound} =
  {var; bound = avoid' cons seen bound}

and avoid_field cons seen {lab; typ} =
  {lab; typ = avoid' cons seen typ}

and avoid_kind cons seen k =
  match k with
  | Def (tbs, t) ->
    Def (List.map (avoid_bind cons seen) tbs,
         avoid' cons seen t)
  | Abs (tbs, t) ->
    Abs (List.map (avoid_bind cons seen) tbs,
         avoid' cons seen t)

and avoid_cons cons1 cons2 =
  ConSet.iter (fun c -> Con.unsafe_set_kind c (avoid_kind cons1 ConSet.empty (Con.kind c))) cons2

let avoid cons t =
  if cons = ConSet.empty then t else
   avoid' cons ConSet.empty t

(* Checking for concrete types *)

module S = Set.Make (struct type t = typ let compare = compare end)

(*
This check is a stop-gap measure until we have an IDL strategy that
allows polymorphic types, see #250. It is not what we desire for ActorScript.
*)

let concrete t =
  let seen = ref S.empty in
  let rec go t =
    S.mem t !seen ||
    begin
      seen := S.add t !seen;
      match t with
      | Var _ -> assert false
      | (Prim _ | Any | Non | Pre) -> true
      | Free c -> go (Con (Free c, [])) (* TBR*)
      | Con (Free c, ts) ->
        begin match Con.kind c with
        | Abs _ -> false
        | Def (tbs,t) -> go (open_ (Free c::ts) t) (* TBR this may fail to terminate *)
        end
      | Con (_, _) ->
        assert false
      | Array t | Opt t | Async t | Mut t | Serialized t -> go t
      | Tup ts -> List.for_all go ts
      | Obj (_, fs) | Variant fs -> List.for_all (fun f -> go f.typ) fs
      | Func (s, c, tbs, ts1, ts2) ->
        let ts = open_binds tbs in
        List.for_all go (List.map (open_ ts) ts1) &&
        List.for_all go (List.map (open_ ts) ts2)
      | Typ c -> assert false (* TBR *)
    end
  in go t


let shared t =
  let seen = ref S.empty in
  let rec go t =
    S.mem t !seen ||
    begin
      seen := S.add t !seen;
      match t with
      | Var _ | Pre -> assert false
      | Any | Non | Prim _ | Typ _ -> true
      | Async _ | Mut _ -> false
      | Free c -> go (Con (Free c,[]))
      | Con (Free c, ts) ->
        (match Con.kind c with
        | Abs _ -> false
        | Def (_, t) -> go (open_ (Free c::ts) t) (* TBR this may fail to terminate *)
        )
      | Con (_, ts) -> assert false
      | Array t | Opt t | Serialized t -> go t
      | Tup ts -> List.for_all go ts
      | Obj (s, fs) -> s = Actor || List.for_all (fun f -> go f.typ) fs
      | Variant fs -> List.for_all (fun f -> go f.typ) fs
      | Func (s, c, tbs, ts1, ts2) -> s = Shared
    end
  in go t


(* Equivalence & Subtyping *)

module SS = Set.Make (struct type t = typ * typ let compare = compare end)

(* Debugging rel_typ *)

let debug = false (* true, to debug *)

let max_depth = 40

let str = ref (fun _ -> failwith "")

let debug_string_of_typ t =
  match t with
  | Free c
  | Con (Free c, _) when match Con.kind c with Def _ -> true | _ -> false ->
    Printf.sprintf "%s where %s" (!str t) (!str (Typ c))
  | _ -> !str t

let trace_rel_typ rel eq t1 t2 =
  let n = SS.cardinal (!rel) in
  match compare n max_depth with
  | -1 ->
    let indent = String.make n ' ' in
    begin
      Printf.printf "\n %s rel_typ %s%!" indent (debug_string_of_typ t1);
      Printf.printf "\n %s         %s%!" indent (debug_string_of_typ t2)
    end
  | 0 ->
    let indent = String.make n ' ' in
    Printf.printf "\n %s rel_type %s%!" indent "..."
  | _ -> ()

module O = Map.Make (struct type t = typ list * typ let compare = compare end)

let rels () =

let seen = ref O.empty in

let open_  ts t  =
    match O.find_opt (ts,t) !seen with
    | Some t' -> t'
    | None  ->
      let t' = open_ ts t in
      seen := O.add (ts,t) t' !seen;
      t'
in

let rel_list p rel eq xs1 xs2 =
  try List.for_all2 (p rel eq) xs1 xs2 with Invalid_argument _ -> false
in

let rec rel_typ rel eq t1 t2 =
  if debug then trace_rel_typ rel eq t1 t2;
  t1 == t2 || SS.mem (t1, t2) !rel || begin
  rel := SS.add (t1, t2) !rel;
  match t1, t2 with
  | Pre, _ | _, Pre ->
    assert false
  | Any, Any ->
    true
  | _, Any when rel != eq ->
    true
  | Non, Non ->
    true
  | Non, _ when rel != eq ->
    true
  | Free con1, _ ->
    rel_typ rel eq (Con (t1, []))  t2
  | _, Free con2 ->
    rel_typ rel eq t1 (Con (t2, []))
  | Con (Free con1, ts1), Con (Free con2, ts2) ->
    (match Con.kind con1, Con.kind con2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (open_ (Free con1::ts1) t) t2
    | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (open_ (Free con2::ts2) t)
    | _ when Con.eq con1 con2 ->
      rel_list eq_typ rel eq ts1 ts2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ ->
      false
    )
  | Con (Free con1, ts1), t2 ->
    (match Con.kind con1, t2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (open_ (Free con1::ts1) t) t2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (Free con2, ts2) ->
    (match Con.kind con2 with
    | Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (open_ (Free con2::ts2) t)
    | _ -> false
    )
  | Prim p1, Prim p2 when p1 = p2 ->
    true
  | Prim p1, Prim p2 when rel != eq ->
    p1 = Nat && p2 = Int
  | Obj (s1, tfs1), Obj (s2, tfs2) ->
    s1 = s2 &&
    rel_fields rel eq tfs1 tfs2
  | Array t1', Array t2' ->
    rel_typ rel eq t1' t2'
  | Opt t1', Opt t2' ->
    rel_typ rel eq t1' t2'
  | Prim Null, Opt t2' when rel != eq ->
    true
  | Variant fs1, Variant fs2 ->
    rel_tags rel eq fs1 fs2
  | Tup ts1, Tup ts2 ->
    rel_list rel_typ rel eq ts1 ts2
  | Func (s1, c1, tbs1, t11, t12), Func (s2, c2, tbs2, t21, t22) ->
    c1 = c2 && s1 = s2 &&
    (match rel_binds rel eq tbs1 tbs2 with
    | Some ts ->
      rel_list rel_typ rel eq (List.map (open_ ts) t21) (List.map (open_ ts) t11) &&
      rel_list rel_typ rel eq (List.map (open_ ts) t12) (List.map (open_ ts) t22)
    | None -> false
    )
  | Async t1', Async t2' ->
    rel_typ rel eq t1' t2'
  | Mut t1', Mut t2' ->
    eq_typ rel eq t1' t2'
  | Serialized t1', Serialized t2' ->
    rel_typ rel eq t1' t2'
  | Typ c1, Typ c2 ->
    eq_con rel eq c1 c2
  | _, _ -> false
  end

and rel_fields rel eq tfs1 tfs2 =
  (* Assume that tfs1 and tfs2 are sorted. *)
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

and rel_tags rel eq tfs1 tfs2 =
  (* Assume that tfs1 and tfs2 are sorted. *)
  match tfs1, tfs2 with
  | [], [] ->
    true
  | [], _ when rel != eq ->
    true
  | tf1::tfs1', tf2::tfs2' ->
    (match compare_field tf1 tf2 with
    | 0 ->
      rel_typ rel eq tf1.typ tf2.typ &&
      rel_tags rel eq tfs1' tfs2'
    | +1 when rel != eq ->
      rel_tags rel eq tfs1 tfs2'
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

and eq_kind rel eq k1 k2 =
  match k1, k2 with
  | Def (tbs1, t1), Def (tbs2, t2) ->
    let c1 = Con.fresh "anon" k1 in
    let c2 = Con.fresh "anon" k2 in
    let tbs1' = unfold_binds c1 tbs1 in
    let tbs2' = unfold_binds c2 tbs2 in
    begin match rel_binds eq eq tbs1' tbs2' with
    | Some ts -> eq_typ eq eq  (open_ (Free c1::ts) t1) (open_ (Free c2::ts) t2)
    | None -> false
    end
  | Abs (tbs1, t1), Abs (tbs2, t2) ->
    begin match rel_binds eq eq tbs1 tbs2 with
    | Some ts -> eq_typ eq eq  (open_ ts t1) (open_ ts t2)
    | None -> false
    end
  | _ -> false

and eq_con rel eq c1 c2 =
  match Con.kind c1, Con.kind c2 with
  | Def (tbs1, t1), Def (tbs2, t2) ->
    let tbs1' = unfold_binds c1 tbs1 in
    let tbs2' = unfold_binds c2 tbs2 in
    begin match rel_binds eq eq tbs1' tbs2' with
    | Some ts -> eq_typ eq eq  (open_ (Free c1::ts) t1) (open_ (Free c2::ts) t2)
    | None -> false
    end
  | Abs (tbs1, t1), Abs (tbs2, t2) ->
    begin match rel_binds eq eq tbs1 tbs2 with
    | Some ts -> eq_typ eq eq  (open_ ts t1) (open_ ts t2)
    | None -> false
    end
  | _ -> false

and eq_typ rel eq t1 t2 =
  rel_typ eq eq t1 t2

in
  (rel_typ, eq_typ, eq_kind)


let eq t1 t2 : bool =
  let (rel_typ, eq_typ, eq_kind) = rels() in
  let eq = ref SS.empty in eq_typ eq eq t1 t2

let sub t1 t2 : bool =
  let (rel_typ, eq_typ, eq_kind) = rels() in
  rel_typ (ref SS.empty) (ref SS.empty) t1 t2

let eq_kind k1 k2 : bool =
  let (rel_typ, eq_typ, eq_kind) = rels() in
  let eq = ref SS.empty in
  eq_kind eq eq k1 k2


(* Compatibility *)

let compatible_list p co xs1 xs2 =
  try List.for_all2 (p co) xs1 xs2 with Invalid_argument _ -> false

let rec compatible_typ co t1 t2 =
  t1 == t2 || SS.mem (t1, t2) !co || begin
  co := SS.add (t1, t2) !co;
  match promote t1, promote t2 with
  | (Pre | Serialized _), _ | _, (Pre | Serialized _) ->
    assert false
  | Any, Any ->
    true
  | Any, _ | _, Any ->
    false
  | Non, _ | _, Non ->
    true
  | Prim p1, Prim p2 when p1 = p2 ->
    true
  | Prim (Nat | Int), Prim (Nat | Int) ->
    true
  | Array t1', Array t2' ->
    compatible_typ co t1' t2'
  | Tup ts1, Tup ts2 ->
    compatible_list compatible_typ co ts1 ts2
  | Obj (s1, tfs1), Obj (s2, tfs2) ->
    s1 = s2 &&
    compatible_fields co tfs1 tfs2
  | Opt t1', Opt t2' ->
    compatible_typ co t1' t2'
  | Prim Null, Opt _ | Opt _, Prim Null  ->
    true
  | Variant tfs1, Variant tfs2 ->
    compatible_tags co tfs1 tfs2
  | Async t1', Async t2' ->
    compatible_typ co t1' t2'
  | Func _, Func _ ->
    true
  | Typ _, Typ _ ->
    true
  | Mut t1', Mut t2' ->
    compatible_typ co t1' t2'
  | _, _ ->
    false
  end

and compatible_fields co tfs1 tfs2 =
  (* Assume that tfs1 and tfs2 are sorted. *)
  match tfs1, tfs2 with
  | [], [] -> true
  | tf1::tfs1', tf2::tfs2' ->
    tf1.lab = tf2.lab && compatible_typ co tf1.typ tf2.typ &&
    compatible_fields co tfs1' tfs2'
  | _, _ -> false

and compatible_tags co tfs1 tfs2 =
  (* Assume that tfs1 and tfs2 are sorted. *)
  match tfs1, tfs2 with
  | [], _ | _, [] -> true
  | tf1::tfs1', tf2::tfs2' ->
    match compare_field tf1 tf2 with
    | -1 -> compatible_tags co tfs1' tfs2
    | +1 -> compatible_tags co tfs1 tfs2'
    | _ -> compatible_typ co tf1.typ tf2.typ && compatible_tags co tfs1' tfs2'

and compatible t1 t2 : bool =
  compatible_typ (ref SS.empty) t1 t2


let opaque t = compatible t Any


(* Inhabitance *)

let rec inhabited_typ co t =
  S.mem t !co || begin
  co := S.add t !co;
  match promote t with
  | Pre | Serialized _ -> assert false
  | Non -> false
  | Any | Prim _ | Array _ | Opt _ | Async _ | Func _ | Typ _ -> true
  | Mut t' -> inhabited_typ co t'
  | Tup ts -> List.for_all (inhabited_typ co) ts
  | Obj (_, tfs) -> List.for_all (inhabited_field co) tfs
  | Variant tfs -> List.exists (inhabited_field co) tfs
  | Var _ -> true  (* TODO(rossberg): consider bound *)
  | Free c -> inhabited_typ co (Con (Free c, [])) (* TBR *)
  | Con (Free c, ts) ->
    (match Con.kind c with
    | Def (tbs, t') -> (* TBR this may fail to terminate *)
      inhabited_typ co (open_ (Free c::ts) t')
    | Abs (tbs, t') ->
      inhabited_typ co t')
  | Con (_, ts) -> assert false (* TBR *)
  end

and inhabited_field co tf = inhabited_typ co tf.typ

and inhabited t : bool = inhabited_typ (ref S.empty) t


(* Least upper bound and greatest lower bound *)

module M = Map.Make (struct type t = typ * typ let compare = compare end)

module N = Map.Make (struct type t = typ let compare = compare end)

let lubglb () =

let seen = ref N.empty in
let normalize t =
  match N.find_opt t !seen with
  | Some t' -> t'
  | None ->
    let t' = normalize t in
    seen := N.add t t' !seen;
    t'
in

let rec lub' lubs glbs t1 t2 =
  (*  Printf.printf "\n lub %s %s" (debug_string_of_typ t1) (debug_string_of_typ t2); *)
  if t1 == t2 then t1 else
  match M.find_opt (t1, t2) !lubs with
  | Some t -> t
  | _ ->
    match t1, t2 with
    | _, Pre
    | Pre, _ -> assert false
    | _, Any
    | Any, _ -> Any
    | _, Non -> t1
    | Non, _ -> t2
    | Prim Nat, (Prim Int as t)
    | (Prim Int as t), Prim Nat -> t
    | Opt t1', Opt t2' ->
      Opt (lub' lubs glbs t1' t2')
    | Prim Null, Opt t' -> t2
    | Opt t', Prim Null -> t1
    | Variant t1', Variant t2' ->
      Variant (lub_tags lubs glbs t1' t2')
    | Array t1', Array t2' ->
      Array (lub' lubs glbs t1' t2')
    | Tup ts1, Tup ts2 when List.(length ts1 = length ts2) ->
      Tup (List.map2 (lub' lubs glbs) ts1 ts2)
    | Obj (s1, tf1), Obj (s2, tf2) when s1 = s2 ->
      Obj (s1, lub_fields lubs glbs tf1 tf2)
    | Func (s1, c1, bs1, args1, res1), Func (s2, c2, bs2, args2, res2) when
        s1 = s2 && c1 = c2 && List.(length bs1 = length bs2) &&
        List.(length args1 = length args2 && length res1 = length res2) ->
      combine_func_parts s1 c1 bs1 args1 res1 bs2 args2 res2 lubs glbs glb' lub'
    | Async t1', Async t2' ->
      Async (lub' lubs glbs t1' t2')
    | _, Free _ ->
      lub' lubs glbs t1 (Con (t2, []))
    | Free _, _ ->
      lub' lubs glbs (Con (t1, [])) t2
    | Con _, _
    | _, Con _ ->
      (* TODO(rossberg): fix handling of bounds *)
      combine_con_parts t1 t2 "lub" lubs (lub' lubs glbs)
    | _ when eq t1 t2 -> t1
    | _ -> Any

and lub_fields lubs glbs fs1 fs2 = match fs1, fs2 with
  | _, [] -> []
  | [], _ -> []
  | f1::fs1', f2::fs2' ->
    match compare_field f1 f2 with
    | -1 -> lub_fields lubs glbs fs1' fs2
    | +1 -> lub_fields lubs glbs fs1 fs2'
    | _ -> {f1 with typ = lub' lubs glbs f1.typ f2.typ}::lub_fields lubs glbs fs1' fs2'

and lub_tags lubs glbs fs1 fs2 = match fs1, fs2 with
  | fs1, [] -> fs1
  | [], fs2 -> fs2
  | f1::fs1', f2::fs2' ->
    match compare_field f1 f2 with
    | -1 -> f1 :: lub_tags lubs glbs fs1' fs2
    | +1 -> f2 :: lub_tags lubs glbs fs1 fs2'
    | _ -> {f1 with typ = lub' lubs glbs f1.typ f2.typ} :: lub_tags lubs glbs fs1' fs2'

and glb' lubs glbs t1 t2 =
  if t1 == t2 then t1 else
  match M.find_opt (t1, t2) !glbs with
  | Some t -> t
  | _ ->
    match t1, t2 with
    | _, Pre
    | Pre, _ -> assert false
    | _, Any -> t1
    | Any, _ -> t2
    | _, Non
    | Non, _ -> Non
    | (Prim Nat as t), Prim Int
    | Prim Int, (Prim Nat as t) -> t
    | Opt t1', Opt t2' ->
      Opt (glb' lubs glbs t1' t2')
    | Variant t1', Variant t2' ->
      Variant (glb_tags lubs glbs t1' t2')
    | Prim Null, Opt _
    | Opt _, Prim Null -> Prim Null
    | Tup ts1, Tup ts2 when List.(length ts1 = length ts2) ->
      Tup (List.map2 (glb' lubs glbs) ts1 ts2)
    | Array t1', Array t2' ->
      Array (glb' lubs glbs t1' t2')
    | Obj (s1, tf1), Obj (s2, tf2) when s1 = s2 ->
      Obj (s1, glb_fields lubs glbs tf1 tf2)
    | Func (s1, c1, bs1, args1, res1), Func (s2, c2, bs2, args2, res2) when
        s1 = s2 && c1 = c2 && List.(length bs1 = length bs2) &&
        List.(length args1 = length args2 && length res1 = length res2) ->
      combine_func_parts s1 c1 bs1 args1 res1 bs2 args2 res2 lubs glbs lub' glb'
    | Async t1', Async t2' ->
      Async (glb' lubs glbs t1' t2')
    | _, Free _ ->
      glb' lubs glbs t1 (Con (t2, []))
    | Free _, _ ->
      glb' lubs glbs (Con (t1, [])) t2
    | Con _, _
    | _, Con _ ->
      (* TODO(rossberg): fix handling of bounds *)
      combine_con_parts t1 t2 "glb" glbs (glb' lubs glbs)
    | _ when eq t1 t2 -> t1
    | _ -> Non

and glb_fields lubs glbs fs1 fs2 = match fs1, fs2 with
  | fs1, [] -> fs1
  | [], fs2 -> fs2
  | f1::fs1', f2::fs2' ->
    match compare_field f1 f2 with
    | -1 -> f1::glb_fields lubs glbs fs1' fs2
    | +1 -> f2::glb_fields lubs glbs fs1 fs2'
    | _ -> {f1 with typ = glb' lubs glbs f1.typ f2.typ}::glb_fields lubs glbs fs1' fs2'

and glb_tags lubs glbs fs1 fs2 = match fs1, fs2 with
  | fs1, [] -> []
  | [], fs2 -> []
  | f1::fs1', f2::fs2' ->
    match compare_field f1 f2 with
    | -1 -> glb_tags lubs glbs fs1' fs2
    | +1 -> glb_tags lubs glbs fs1 fs2'
    | _ -> {f1 with typ = glb' lubs glbs f1.typ f2.typ}::glb_tags lubs glbs fs1' fs2'

and combine_func_parts s c bs1 args1 res1 bs2 args2 res2 lubs glbs contra co =
  let open List in
  let ts1 = open_binds bs1 in
  let op = map (open_ ts1) in
  let get_con = function | Free c -> c | _ -> assert false in
  let cs = map get_con ts1 in
  let cl = map (close cs) in
  let combine_binds =
    map2 (fun b1 b2 -> {b1 with bound = contra lubs glbs b1.bound b2.bound}) in
  Func
    (s, c, combine_binds bs1 bs2,
     cl (map2 (contra lubs glbs) (op args1) (op args2)),
     cl (map2 (co lubs glbs) (op res1) (op res2)))

and combine_con_parts t1 t2 naming re how =
  let s1, s2 = !str t1, !str t2 in
  if s1 = s2 then t1 else
  let c = Con.fresh (Printf.sprintf "@%s(%s, %s)" naming s1 s2) (Abs ([], Pre)) in
  (*  Printf.printf "\n combine con parts %s %s %s" naming (debug_string_of_typ t1) (debug_string_of_typ t2); *)
  let t = Con (Free c, []) in
  re := M.add (t2, t1) t (M.add (t1, t2) t !re);
  let inner = how (normalize t1) (normalize t2) in
  set_kind c (Def ([], inner));
  (* check for short-circuiting opportunities *)
  if eq inner t1
  then (re := M.add (t2, t1) t1 (M.add (t1, t2) t1 !re); t1)
  else if eq inner t2
  then (re := M.add (t2, t1) t2 (M.add (t1, t2) t2 !re); t2)
  else inner
in
 (lub',glb')

let lub t1 t2 =
  let (lub',glb') = lubglb() in
  lub' (ref M.empty) (ref M.empty) t1 t2
let glb t1 t2 =
  let (lub',glb') = lubglb() in
  glb' (ref M.empty) (ref M.empty) t1 t2


(* Environments *)

module Env = Env.Make(String)


(* Pretty printing *)

open Printf

let string_of_prim = function
  | Null -> "Null"
  | Bool -> "Bool"
  | Nat -> "Nat"
  | Nat8 -> "Nat8"
  | Nat16 -> "Nat16"
  | Nat32 -> "Nat32"
  | Nat64 -> "Nat64"
  | Int -> "Int"
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Float -> "Float"
  | Word8 -> "Word8"
  | Word16 -> "Word16"
  | Word32 -> "Word32"
  | Word64 -> "Word64"
  | Char -> "Char"
  | Text -> "Text"

let string_of_var (x, i) =
  if i = 0 then sprintf "%s" x else sprintf "%s.%d" x i

let string_of_con' vs c =
  let s = Con.to_string c in
  if List.mem (s, 0) vs then s ^ "/0" else s  (* TBR *)

let string_of_obj_sort = function
  | Object -> ""
  | Module -> "module "
  | Actor -> "actor "

let string_of_func_sort = function
  | Local -> ""
  | Shared -> "shared "

let rec string_of_typ_nullary vs = function
  | Pre -> "???"
  | Any -> "Any"
  | Non -> "None"
  | Prim p -> string_of_prim p
  | Var (s, i) -> string_of_var (List.nth vs i)
  | Free c -> string_of_con' vs c
  | Con (t, []) ->
    string_of_typ_nullary vs t
  | Con (t, ts) ->
    sprintf "%s<%s>" (string_of_typ_nullary vs t)
      (String.concat ", " (List.map (string_of_typ' vs) ts))
  | Tup ts ->
    sprintf "(%s%s)"
      (String.concat ", " (List.map (string_of_typ' vs) ts))
      (if List.length ts = 1 then "," else "")
  | Array (Mut t) ->
    sprintf "[var %s]" (string_of_typ_nullary vs t)
  | Array t ->
    sprintf "[%s]" (string_of_typ_nullary vs t)
  | Obj (Object, fs) ->
    sprintf "{%s}" (String.concat "; " (List.map (string_of_field vs) fs))
  | Variant [] -> "{#}"
  | Variant fs ->
    sprintf "{%s}" (String.concat "; " (List.map (string_of_tag vs) fs))
  | Typ c ->
    sprintf "= {%s}" (string_of_con' vs c)
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
    sprintf "%s%s -> %s" (string_of_func_sort s)
      (string_of_dom vs ts1)
      (string_of_cod c vs ts2)
  | Func (s, c, tbs, ts1, ts2) ->
    let vs' = vars_of_binds vs tbs in
    sprintf "%s%s%s -> %s"
      (string_of_func_sort s) (string_of_binds (vs' @ vs) vs' tbs)
      (string_of_dom (vs' @ vs) ts1) (string_of_cod c (vs' @ vs) ts2)
  | Opt t ->
    sprintf "?%s"  (string_of_typ_nullary vs t)
  | Async t ->
    sprintf "async %s" (string_of_typ_nullary vs t)
  | Obj (s, fs) ->
    sprintf "%s%s" (string_of_obj_sort s) (string_of_typ_nullary vs (Obj (Object, fs)))
  | Typ c ->
    let op, sbs, st = strings_of_con' vs c in
    sprintf "typ %s%s %s %s" (Con.to_string c) sbs op st
  | Mut t ->
    sprintf "var %s" (string_of_typ' vs t)
  | Serialized t ->
    sprintf "serialized %s" (string_of_typ' vs t)
  | t -> string_of_typ_nullary vs t

and string_of_field vs {lab; typ} =
  match typ with
  | Typ c ->
    let op, sbs, st = strings_of_con' vs c in
    sprintf "type %s%s %s %s" lab sbs op st
  | _ ->
    sprintf "%s : %s" lab (string_of_typ' vs typ)

and string_of_tag vs {lab; typ} =
  match typ with
  | Tup [] -> sprintf "#%s" lab
  | _ -> sprintf "#%s : %s" lab (string_of_typ' vs typ)

and vars_of_binds vs bs =
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

and strings_of_con' vs c =
  let k = Con.kind c in
  let op, tbs, t, cs =
    match k with
    | Def (tbs, t) -> "=", tbs, t, [(string_of_con' vs c,0)]
    | Abs (tbs, t) -> "<:", tbs, t, []
  in
  let vs' = vars_of_binds vs tbs in
  let csvs'v = cs @ vs' @ vs in
  op, string_of_binds csvs'v vs' tbs, string_of_typ' csvs'v t

let string_of_con : con -> string = string_of_con' []

let strings_of_con : con -> (string * string * string) = strings_of_con' []

let string_of_typ : typ -> string = string_of_typ' []

let rec string_of_typ_expand t =
  let s = string_of_typ t in
  match t with
  | Con (Free c, ts) ->
    (match Con.kind c with
    | Abs _ -> s
    | Def _ ->
      match normalize t with
      | Prim _ | Any | Non -> s
      | t' -> s ^ " = " ^ string_of_typ_expand t'
    )
  | _ -> s

let _ = str := string_of_typ
