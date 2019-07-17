(* Representation *)

(* TODO: revisit all uses of polymorphic comparison involving typ(e)s
         & replace by custom comparison *)

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
  | Con of con * typ list                     (* constructor *)
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

(* Forward declare
   TODO: haul string_of_typ before the lub/glb business, if possible *)
let str = ref (fun _ -> failwith "")

let debug_string_of_typ t =
  match t with
  | Con (c, _) when match Con.kind c with Def _ -> true | _ -> false ->
    Printf.sprintf "%s where %s" (!str t) (!str (Typ c))
  | _ -> !str t

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
  | Prim _ | Any | Non -> cs
  | Pre -> assert false
  | Con (c, ts) ->
    List.fold_right cons ts  (cons_con c cs)
  | Opt t | Async t | Mut t | Serialized t | Array t ->
    cons t cs
  | Tup ts -> List.fold_right cons ts cs
  | Func (s, c, tbs, ts1, ts2) ->
    let cs = List.fold_right cons_bind tbs  cs in
    let cs = List.fold_right cons ts1 cs in
    List.fold_right cons ts2 cs
  | (Obj (_, fs) | Variant fs) ->
    List.fold_right cons_field fs cs
  | Typ c -> ConSet.add c cs

and cons_con c cs =
  if ConSet.mem c cs
  then cs
  else cons_kind (Con.kind c) (ConSet.add c cs)

and cons_bind {var; bound} cs =
  cons bound cs

and cons_field {lab; typ} cs =
  cons typ cs

and cons_kind k cs =
  match k with
  | Def (tbs, t) ->
    cons t (List.fold_right cons_bind tbs cs)
  | Abs (tbs, t) ->
    (* ignore the bound *)
    cs

let rec is_closed seen i t =
  match t with
  | Var (_, j) ->  j < i
  | Con (c, ts) -> is_closed_con seen i c && List.for_all (is_closed seen i) ts
  | Tup ts -> List.for_all (is_closed seen i) ts
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    List.for_all (fun {var;bound} -> is_closed seen i' bound) tbs &&
    List.for_all (is_closed seen i') ts1 &&
    List.for_all (is_closed seen i') ts2
  | Obj (_, fs)
  | Variant fs -> List.for_all (fun {typ;_} -> is_closed seen i typ) fs
  | Array t | Opt t | Async t | Mut t | Serialized t -> is_closed seen i t
  | Prim _ | Any | Non -> true
  | Pre -> assert false
  | Typ c -> is_closed_con seen i c

and is_closed_kind seen i k =
  match k with
  | Abs (tbs, t) -> (* TBR *)
    assert
      (List.for_all (fun {var; bound} -> is_closed seen 0 (*!*) bound) tbs &&
         is_closed seen 0 (*!*) t);
    true
  | Def (tbs, t) ->
    let i' = i + List.length tbs in
    List.for_all (fun {var; bound} -> is_closed seen i' bound) tbs &&
    is_closed seen i' t

and is_closed_con seen i c =
  let k = Con.kind c in
  ConSet.mem c seen || is_closed_kind (ConSet.add c seen) i k

(* Shifting *)

let is_abs c = match Con.kind c with Abs _ -> true | Def _ -> false

module ShiftMap = Map.Make
  (struct type t = int * int * con (* (i, n, c) *)
          let compare (i1, n1, c1) (i2, n2, c2) =
            (match compare i1 i2 with
             | 0 ->
               (match compare n1 n2 with
                | 0 -> Con.compare c1 c2
                | j -> j)
             | i -> i)
   end)

type shift_env = con ShiftMap.t ref

let rec shift (shift_env : shift_env) i n t =
  if n = 0 then t else
  match t with
  | Prim _ -> t
  | Var (s, j) -> Var (s, if j < i then j else j + n)
  | Con (c, ts) -> Con (shift_con shift_env i n c, List.map (shift shift_env i n) ts)
  | Array t -> Array (shift shift_env i n t)
  | Tup ts -> Tup (List.map (shift shift_env i n) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (shift_bind shift_env i' n) tbs, List.map (shift shift_env i' n) ts1, List.map (shift shift_env i' n) ts2)
  | Opt t -> Opt (shift shift_env i n t)
  | Async t -> Async (shift shift_env i n t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field shift_env n i) fs)
  | Variant fs -> Variant (List.map (shift_field shift_env n i) fs)
  | Mut t -> Mut (shift shift_env i n t)
  | Serialized t -> Serialized (shift shift_env i n t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c -> Typ (shift_con shift_env i n c)

and shift_con shift_env i n c =
  match Con.kind c with
  | Abs _ -> c
  | Def _ ->
  match ShiftMap.find_opt (i, n, c) !shift_env with
  | Some c' -> c'
  | None ->
    let c' = Con.fresh (Con.name c) (Abs([], Pre)) in
    shift_env := ShiftMap.add (i, n, c) c' !shift_env;
    Con.unsafe_set_kind c' (shift_kind shift_env i n (Con.kind c));
    c'

and shift_bind shift_env i n {var; bound} =
  {var; bound = shift shift_env i n bound}

and shift_field shift_env i n {lab; typ} =
  {lab; typ = shift shift_env i n typ}

and shift_kind shift_env i n k =
  match k with
  | Def (tbs, t) ->
    let i' = i + List.length tbs in
    Def (List.map (shift_bind shift_env i' n) tbs, shift shift_env i' n t)
  | Abs (tbs, t) ->
    let i' = i + List.length tbs in
    Abs (List.map (shift_bind shift_env i' n) tbs, shift shift_env i' n t)

(* First-order substitution *)


module SubstMap = Map.Make
  (struct
    type t = int * con (* (m, c) where m is cumulative shift *)
    let compare (m1, c1) (m2, c2) =
      (match compare m1 m2 with
       | 0 -> Con.compare c1 c2
       | i -> i)
   end)

type subst_env = {
  dom : ConSet.t;                 (* domain of the substitution (sigma) *)
  self : (con * con) option;      (* renaming of nearest enclosing constructor, if any *)
  shifts : int;                   (* cumulative shifts (of sigma) *)
  subst_map : con SubstMap.t ref; (* map of substituted cons, indexed by (shifts, c), ie. (cumulative shift,con) *)
  shift_env : con ShiftMap.t ref; (* map of shifted cons, indexed by (i, n, c) ie. (depth, shift, con) *)
}

let rec subst subst_env sigma t =
  if sigma = ConEnv.empty then t else
  match t with
  | Prim _ | Var _ -> t
  | Con (c, []) when is_abs c ->
    (match ConEnv.find_opt c sigma with
     | Some t' -> t'
     | None -> t)
  | Con (c, ts) ->
    Con (subst_con subst_env sigma c, List.map (subst subst_env sigma) ts)
  | Array t -> Array (subst subst_env sigma t)
  | Tup ts -> Tup (List.map (subst subst_env sigma) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let n = List.length tbs in
    let sigma' =
      if n = 0 then sigma else
      ConEnv.map (shift subst_env.shift_env 0 n) sigma in
    let subst_env' = {subst_env with shifts = subst_env.shifts + n} in
    Func (s, c,
      List.map (subst_bind subst_env' sigma') tbs,
      List.map (subst subst_env' sigma') ts1, List.map (subst subst_env' sigma') ts2)
  | Opt t -> Opt (subst subst_env sigma t)
  | Async t -> Async (subst subst_env sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field subst_env sigma) fs)
  | Variant fs -> Variant (List.map (subst_field subst_env sigma) fs)
  | Mut t -> Mut (subst subst_env sigma t)
  | Serialized t -> Serialized (subst subst_env sigma t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c ->
    (assert (not (ConSet.mem c subst_env.dom));
     Typ (subst_con subst_env sigma c))

and subst_con ({dom; self; subst_map; shift_env; shifts} as subst_env) sigma c =
  match Con.kind c with
  | Abs _ -> c
  | Def (tbs, t) ->
    match SubstMap.find_opt (shifts, c) (!subst_map) with
    | Some c' -> c'
    | None ->
      match self with
      | Some (c_self, c') when Con.eq c c_self -> c'
      | _ ->
        if ConSet.is_empty (ConSet.inter dom (cons_con c ConSet.empty)) then
          (subst_map := SubstMap.add (shifts, c) c !subst_map;
           c)
        else
          let c' = Con.fresh (Con.name c) (Abs ([], Pre)) in
          subst_map := SubstMap.add (shifts, c) c' !subst_map;
          let subst_env' = { subst_env with self = Some (c, c') } in
          Con.unsafe_set_kind c' (subst_kind subst_env' sigma (Con.kind c));
          c'

and subst_bind subst_env sigma {var; bound} =
  {var; bound = subst subst_env sigma bound}

and subst_field subst_env sigma {lab; typ} =
  {lab; typ = subst subst_env sigma typ}

and subst_kind subst_env sigma (k:kind) =
  match k with
  | Def (tbs, t) ->
    let n = List.length tbs in
    let sigma' = ConEnv.map (shift (subst_env.shift_env) 0 n) sigma in
    let subst_env' = { subst_env with shifts = subst_env.shifts + n } in
    Def (List.map (subst_bind subst_env' sigma') tbs, subst subst_env' sigma' t)
  | Abs (tbs, t) ->
    let n = List.length tbs in
    let sigma' = ConEnv.map (shift (subst_env.shift_env) 0 n) sigma in
    let subst_env' = { subst_env with shifts = subst_env.shifts + n } in
    Abs (List.map (subst_bind subst_env' sigma') tbs, subst subst_env' sigma' t)

(* Handling binders *)

let close cs t =
  if cs = [] then t else
  let ts = List.mapi (fun i c -> Var (Con.name c, i)) cs in
  let sigma = List.fold_right2 ConEnv.add cs ts ConEnv.empty in
  let subst_env = {
      dom = ConEnv.dom sigma;
      self = None;
      subst_map = ref SubstMap.empty;
      shift_env = ref ShiftMap.empty;
      shifts = 0
  } in
  subst subst_env sigma t

let close_binds cs tbs =
  if cs = [] then tbs else
  List.map (fun {var; bound} -> {var; bound = close cs bound}) tbs

module OpenMap = Map.Make
  (struct type t = int * con (* (i, c) *)
    let compare (i1, c1) (i2, c2) =
      (match compare i1 i2 with
       | 0 -> Con.compare c1 c2
       | i -> i)
   end)

type open_env = {
    self : (con * con) option;    (* renaming of nearest enclosing constructor, if any *)
    open_map : con OpenMap.t ref  (* map of opened constructors, indexed by (i,c) *)
  }

let rec open' open_env i ts t =
  match t with
  | Prim _ -> t
  | Var (_, j) -> if j < i then t else List.nth ts (j - i)
  | Con (c, ts') -> Con (open_con open_env i ts c, List.map (open' open_env i ts) ts')
  | Array t -> Array (open' open_env i ts t)
  | Tup ts' -> Tup (List.map (open' open_env i ts) ts')
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (open_bind open_env i' ts) tbs, List.map (open' open_env i' ts) ts1, List.map (open' open_env i' ts) ts2)
  | Opt t -> Opt (open' open_env i ts t)
  | Async t -> Async (open' open_env i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field open_env i ts) fs)
  | Variant fs -> Variant (List.map (open_field open_env i ts) fs)
  | Mut t -> Mut (open' open_env i ts t)
  | Serialized t -> Serialized (open' open_env i ts t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre
  | Typ c -> Typ (open_con open_env i ts c)

and open_con ({self; open_map} as open_env) i ts c =
  match Con.kind c with
  | Abs _ -> c
  | Def _ ->
  match OpenMap.find_opt (i, c) (!open_map) with
  | Some c' -> c'
  | None ->
    match self with
    | Some (c_, c') when Con.eq c c_ -> c'
    | _ ->
      if is_closed_con ConSet.empty i c then
        (open_map := OpenMap.add (i, c) c !open_map;
         c)
      else
        let c' = Con.fresh (Con.name c) (Abs([], Pre)) in
        open_map := OpenMap.add (i, c) c' !open_map;
        Con.unsafe_set_kind c' (open_kind {open_env with self = Some (c, c') } i ts (Con.kind c));
        c'

and open_bind open_env i ts {var; bound} =
  {var; bound = open' open_env i ts bound}

and open_field open_env i ts {lab; typ} =
  {lab; typ = open' open_env i ts typ}

and open_kind open_env i ts k =
  match k with
  | Def (tbs, t) ->
    let i' = i + List.length tbs in
    Def (List.map (open_bind open_env i' ts) tbs, open' open_env i' ts t)
  | Abs (tbs, t) ->
    let i' = i + List.length tbs in
    Abs (List.map (open_bind open_env i' ts) tbs, open' open_env i' ts t)

let open_ ts t =
  if ts = [] then t else
  let open_env = { self = None; open_map = ref OpenMap.empty } in
  open' open_env 0 ts t

let open_binds tbs =
  if tbs = [] then [] else
  let cs = List.map (fun {var; _} -> Con.fresh var (Abs ([], Pre))) tbs in
  let ts = List.map (fun c -> Con (c, [])) cs in
  let ks = List.map (fun {bound; _} -> Abs ([], open_ ts bound)) tbs in
  List.iter2 set_kind cs ks;
  ts

(* Normalization and Classification *)

let reduce tbs t ts =
  assert (List.length ts = List.length tbs);
  open_ ts t

let rec normalize = function
  | Con (c, ts) as t ->
    (match Con.kind c with
    | Def (tbs, t') -> normalize (reduce tbs t' ts)
    | _ -> t
    )
  | Mut t -> Mut (normalize t)
  | t -> t

let rec promote = function
  | Con (c, ts) ->
    let Def (tbs, t) | Abs (tbs, t) = Con.kind c
    in promote (reduce tbs t ts)
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

let clone c f =
  let k = Con.kind c in
  match k with
  | Abs(tbs,t) -> c
  | Def(tbs,t) ->
    let c' = Con.fresh (Con.name c) (f k) in
    c'

let rec avoid' cons seen = function
  | (Prim _ | Var _ | Any | Non | Pre) as t -> t
  | Con (c, ts) ->
    if ConSet.mem c seen then raise (Unavoidable c) else
    if ConSet.mem c cons
    then match Con.kind c with
      | Abs _ -> raise (Unavoidable c)
      | Def (tbs, t) -> avoid' cons (ConSet.add c seen) (reduce tbs t ts)
    else
      begin try
        Con (c, List.map (avoid' cons seen) ts)
      with Unavoidable d ->
        match Con.kind c with
        | Def (tbs, t) -> avoid' cons seen (reduce tbs t ts)
        | Abs _ -> raise (Unavoidable d)
      end
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
      | Var _ | Pre -> assert false
      | Prim _ | Any | Non -> true
      | Con (c, ts) ->
        (match Con.kind c with
        | Abs _ -> false
        | Def (_, t) -> go (open_ ts t) (* TBR this may fail to terminate *)
        )
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
      | Con (c, ts) ->
        (match Con.kind c with
        | Abs _ -> false
        | Def (_, t) -> go (open_ ts t) (* TBR this may fail to terminate *)
        )
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


module RelEnv = Env.Make(struct type t = con * typ list
                                let compare = compare
                         end)

let rels () =

let opened = ref RelEnv.empty in

let unfold c ts =
  match Con.kind c with
  | Abs _ -> assert false
  | Def (tbs, t) ->
  match RelEnv.find_opt (c, ts) !opened with
  | Some t' -> t'
  | None ->
    let t' = open_ ts t in
    opened := RelEnv.add (c, ts) t' !opened;
    t'
in

let rel_list p rel eq xs1 xs2 =
try List.for_all2 (p rel eq) xs1 xs2 with Invalid_argument _ -> false in

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
  | Con (con1, ts1), Con (con2, ts2) ->
    (match Con.kind con1, Con.kind con2 with
     | Def _, _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (unfold con1 ts1) t2
     | _, Def _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (unfold con2 ts2)
    | _ when Con.eq con1 con2 ->
      rel_list eq_typ rel eq ts1 ts2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ ->
      false
    )
  | Con (con1, ts1), t2 ->
    (match Con.kind con1, t2 with
     | Def _, _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq (unfold con1 ts1) t2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ rel eq (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (con2, ts2) ->
    (match Con.kind con2 with
     | Def _ -> (* TBR this may fail to terminate *)
      rel_typ rel eq t1 (unfold con2 ts2)
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

and eq_kind eq k1 k2 : bool =
  match k1, k2 with
  | Def (tbs1, t1), Def (tbs2, t2)
  | Abs (tbs1, t1), Abs (tbs2, t2) ->
    (match rel_binds eq eq tbs1 tbs2 with
    | Some ts -> eq_typ eq eq (open_ ts t1) (open_ ts t2)
    | None -> false
    )
  | _ -> false

and eq_con rel eq c1 c2 =
  match Con.kind c1, Con.kind c2 with
  | Def (tbs1, t1), Def (tbs2, t2)
  | Abs (tbs1, t1), Abs (tbs2, t2) ->
    (match rel_binds eq eq tbs1 tbs2 with
    | Some ts -> eq_typ eq eq (open_ ts t1) (open_ ts t2)
    | None -> false
    )
  | _ -> false

and eq_typ rel eq t1 t2 = rel_typ eq eq t1 t2
  in
    (rel_typ,
     eq_typ,
     eq_kind)


let sub t1 t2 : bool =
  let (rel_typ, _, _) = rels () in
  rel_typ (ref SS.empty) (ref SS.empty) t1 t2

let eq t1 t2 : bool =
  let (_, eq_typ, _) = rels () in
  let eq = ref SS.empty in eq_typ eq eq t1 t2

let eq_kind k1 k2 : bool =
  let (_, _, eq_kind) = rels () in
  let eq = ref SS.empty in
  eq_kind eq k1 k2

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
  | Con (c, ts) ->
    match Con.kind c with
    | Def (tbs, t') -> (* TBR this may fail to terminate *)
      inhabited_typ co (open_ ts t')
    | Abs (tbs, t') ->
      inhabited_typ co t'
  end

and inhabited_field co tf = inhabited_typ co tf.typ

and inhabited t : bool = inhabited_typ (ref S.empty) t


(* Least upper bound and greatest lower bound *)

module M = Map.Make (struct type t = typ * typ let compare = compare end)

(* Forward declare
   TODO: haul string_of_typ before the lub/glb business, if possible *)
let str = ref (fun _ -> failwith "")

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
  let get_con = function | Con (c, []) -> c | _ -> assert false in
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
  let t = Con (c, []) in
  re := M.add (t2, t1) t (M.add (t1, t2) t !re);
  let inner = how (normalize t1) (normalize t2) in
  set_kind c (Def ([], inner));
  (* check for short-circuiting opportunities *)
  if eq inner t1
  then (re := M.add (t2, t1) t1 (M.add (t1, t2) t1 !re); t1)
  else if eq inner t2
  then (re := M.add (t2, t1) t2 (M.add (t1, t2) t2 !re); t2)
  else inner

let lub t1 t2 = lub' (ref M.empty) (ref M.empty) t1 t2
let glb t1 t2 = glb' (ref M.empty) (ref M.empty) t1 t2


(* Environments *)

module Env = Env.Make(String)


(* Pretty printing *)

open Printf

let debug_con = false

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
  | Var (s, i) ->
    if debug_con then
      Printf.sprintf "%s[%i]" (try string_of_var (List.nth vs i) with Invalid_argument _ -> s ^ "?") i
    else
      (try string_of_var (List.nth vs i) with Invalid_argument _ -> s ^ "?")
  | Con (c, []) ->
    if debug_con then
      Printf.sprintf "(%s where %s)"
        (string_of_con' vs c)
        (string_of_typ' vs (Typ c))
    else
      string_of_con' vs c
  | Con (c, ts) ->
    if debug_con then
      sprintf "(%s<%s> where %s)"
        (string_of_con' vs c)
        (String.concat ", " (List.map (string_of_typ' vs) ts))
        (string_of_typ' vs (Typ c))
    else
      sprintf "%s<%s>" (string_of_con' vs c)
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
    sprintf "= (type %s)" (string_of_con' vs c)
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
    sprintf "type %s%s %s %s" (Con.to_string c) sbs op st
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
  let op, tbs, t, cs =
    match Con.kind c with
    | Def (tbs, t) -> "=", tbs, t, [(string_of_con' vs c, 0)]
    | Abs (tbs, t) -> "<:", tbs, t, []
  in
  let vs' = vars_of_binds vs tbs in
  let vs'v = vs' @ vs in
  op, string_of_binds vs'v vs' tbs, string_of_typ' vs'v t

let string_of_con : con -> string = string_of_con' []

let strings_of_con : con -> string * string * string = strings_of_con' []

let string_of_typ : typ -> string = string_of_typ' []

let rec string_of_typ_expand t =
  let s = string_of_typ t in
  match t with
  | Con (c, _) ->
    (match Con.kind c with
    | Abs _ -> s
    | Def _ ->
      match normalize t with
      | Prim _ | Any | Non -> s
      | t' -> s ^ " = " ^ string_of_typ_expand t'
    )
  | _ -> s

let _ = str := string_of_typ
