(* Representation *)

type lab = string
type var = string

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
  | Var of var * int                          (* variable *)
  | Con of con * typ list                     (* constructor *)
  | Prim of prim                              (* primitive *)
  | Obj of obj_sort * field list              (* object *)
  | Array of typ                              (* array *)
  | Opt of typ                                (* option *)
  | Variant of (lab * typ) list               (* variant *)
  | Tup of typ list                           (* tuple *)
  | Func of sharing * control * bind list * typ list * typ list  (* function *)
  | Async of typ                              (* future *)
  | Mut of typ                                (* mutable type *)
  | Shared                                    (* sharable *)
  | Serialized of typ                         (* a serialized value *)
  | Any                                       (* top *)
  | Non                                       (* bottom *)
  | Pre                                       (* pre-type *)

and bind = {var : var; bound : typ}
and field = {lab : lab; typ : typ}

and con = kind Con.t
and kind =
  | Def of bind list * typ
  | Abs of bind list * typ

(* typ' is a semanic subtype of typ and mostly representationally
   (heap layout-wise) compatible. It is needed for knot-tying operations
   like lub and glb. In contrast to typ, typ' contains lazy fields, which
   enable delaying computation of subobject lub/glb which may refer to the
   outer result due to recursion.
   To obtain the final result, the graph in OCaml runtime heap can be
   fixed-up after building, patching the lazy fields with strict ones.
   In the process of patching, the heap layout of typ' changes to become
   a legal typ via coercion. *)
and typ' =
  | Var' of unit                              (* variable (unused) *)
  | Con' of unit                              (* constructor (unused) *)
  | Prim' of unit                             (* primitive (unused) *)
  | Obj' of obj_sort * field list Lazy.t      (* object *)
  | Array' of typ Lazy.t                      (* array *)
  | Opt' of typ Lazy.t                        (* option *)
  | Variant' of (lab * typ) list Lazy.t       (* variant *)
  | Tup' of typ list Lazy.t                   (* tuple *)
  | Func' of sharing * control * bind list * typ list Lazy.t * typ list Lazy.t  (* function *)
  | Async' of typ Lazy.t                      (* future *)
  (*| Mut' of typ Lazy.t                         mutable type *)

let coerced_typ (l : typ') : typ = Obj.magic l

                            (* It is forbidden to (deeply) inspect the result of coerced_typ, so this is MOOT!!
(* A type is looping when it's directed graph references the same subgraph more than once
   (pointer equality), and there is a path between them that doesn't contain a Con. *)
let _is_loop_free t =
  let rec check_no_pointer ts followers = (List.length followers = 0 ||
    List.for_all (fun p -> not (List.exists (fun p' -> p' == p) ts)) followers
    && (Printf.printf "DEEPER %d  --> %d\n" (List.length ts) (List.length followers);check_no_pointer (ts @ followers) List.(flatten (map references followers))))
  and references = function
    | Tup ts -> Printf.printf "Typ children %d\n" (List.length ts); ts
    | Opt t | Array t | Async t -> Printf.printf "Other children 1\n";[t]
    | Obj (_, fts) -> let ts = List.map (fun {lab; typ} -> typ) fts in Printf.printf "Obj children %d\n" (List.length ts); ts
    | Variant vts -> let ts = List.map snd vts in Printf.printf "Variant children %d\n" (List.length ts); ts
    | _ -> [] in
  check_no_pointer [t] (references t)
                             *)
let fixup = function
  | Var' _
  | Con' _
  | Prim' _ ->
    ignore [Var' (); Con' (); Prim' ()]; assert false; (* these are placeholders for the sake of layout compatibility *)
  | Obj' (s, lazy fs) as o -> Obj.(set_field (repr o) 1 (repr fs)); coerced_typ o
  | Array' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o
  | Opt' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o
  | Variant' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o
  | Tup' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o
  | Func' (s, c, bs, lazy args, lazy res) as o ->
    Obj.(set_field (repr o) 3 (repr args));
    Obj.(set_field (repr o) 4 (repr res));
    coerced_typ o
  | Async' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o
(*| Mut' (lazy t) as o -> Obj.(set_field (repr o) 0 (repr t)); coerced_typ o*)

(* Helper for variant constructors *)
let map_constr_typ f = List.map (fun (c, t) -> c, f t)

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

let seq = function [t] -> t | ts -> Tup ts

let compare_field {lab = l1; _} {lab = l2; _} = compare l1 l2
let compare_summand (c1, _) (c2, _) = compare c1 c2

let iter_obj t =
  Obj (Object Local,
    [{lab = "next"; typ = Func (Local, Returns, [], [], [Opt t])}])

let array_obj t =
  let immut t =
    [ {lab = "get";  typ = Func (Local, Returns, [], [Prim Nat], [t])};
      {lab = "len";  typ = Func (Local, Returns, [], [], [Prim Nat])};
      {lab = "keys"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat)])};
      {lab = "vals"; typ = Func (Local, Returns, [], [], [iter_obj t])};
    ] in
  let mut t = immut t @
    [ {lab = "set"; typ = Func (Local, Returns, [], [Prim Nat; t], [])} ] in
  match t with
  | Mut t' -> Obj (Object Local, List.sort compare_field (mut t'))
  | t -> Obj (Object Local, List.sort compare_field (immut t))

let text_obj =
  let immut =
    [ {lab = "chars"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Char)])};
      {lab = "len";  typ = Func (Local, Returns, [], [], [Prim Nat])};
    ] in
  Obj (Object Local, List.sort compare_field immut)

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
  | Variant cts -> Variant (map_constr_typ (shift i n) cts)
  | Async t -> Async (shift i n t)
  | Obj (s, fs) -> Obj (s, List.map (shift_field n i) fs)
  | Mut t -> Mut (shift i n t)
  | Shared -> Shared
  | Serialized t -> Serialized (shift i n t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and shift_bind i n {var; bound} =
  {var; bound = shift i n bound}

and shift_field i n {lab; typ} =
  {lab; typ = shift i n typ}


(* First-order substitution *)

let rec subst sigma t =
  if sigma = ConEnv.empty then t else
  match t with
  | Prim _
  | Var _ -> t
  | Con (c, ts) ->
    (match ConEnv.find_opt c sigma with
    | Some t -> assert (List.length ts = 0); t
    | None -> Con (c, List.map (subst sigma) ts)
    )
  | Array t -> Array (subst sigma t)
  | Tup ts -> Tup (List.map (subst sigma) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    let sigma' = ConEnv.map (shift 0 (List.length tbs)) sigma in
    Func (s, c, List.map (subst_bind sigma') tbs,
          List.map (subst sigma') ts1, List.map (subst sigma') ts2)
  | Opt t -> Opt (subst sigma t)
  | Async t -> Async (subst sigma t)
  | Obj (s, fs) -> Obj (s, List.map (subst_field sigma) fs)
  | Variant cts -> Variant (map_constr_typ (subst sigma) cts)
  | Mut t -> Mut (subst sigma t)
  | Shared -> Shared
  | Serialized t -> Serialized (subst sigma t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and subst_bind sigma {var; bound} =
  {var; bound = subst sigma bound}

and subst_field sigma {lab; typ} =
  {lab; typ = subst sigma typ}


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
  | Con (c, ts') -> Con (c, List.map (open' i ts) ts')
  | Array t -> Array (open' i ts t)
  | Tup ts' -> Tup (List.map (open' i ts) ts')
  | Func (s, c, tbs, ts1, ts2) ->
    let i' = i + List.length tbs in
    Func (s, c, List.map (open_bind i' ts) tbs, List.map (open' i' ts) ts1, List.map (open' i' ts) ts2)
  | Opt t -> Opt (open' i ts t)
  | Async t -> Async (open' i ts t)
  | Obj (s, fs) -> Obj (s, List.map (open_field i ts) fs)
  | Variant cts -> Variant (map_constr_typ (open' i ts) cts)
  | Mut t -> Mut (open' i ts t)
  | Shared -> Shared
  | Serialized t -> Serialized (open' i ts t)
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and open_bind i ts {var; bound} =
  {var; bound = open' i ts bound}

and open_field i ts {lab; typ} =
  {lab; typ = open' i ts typ}

let open_ ts t =
  if ts = [] then t else
  open' 0 ts t

let open_binds tbs =
  if tbs = [] then [] else
  let cs = List.map (fun {var; _} -> Con.fresh var (Abs ([], Pre))) tbs in
  let ts = List.map (fun c -> Con (c, [])) cs in
  let ks = List.map (fun {bound; _} -> Abs ([], open_ ts bound)) tbs in
  List.iter2 (fun c k -> set_kind c k) cs ks;
  ts


(* Normalization and Classification *)

let reduce tbs t ts =
  assert (List.length ts = List.length tbs);
  open_ ts t

let rec normalize = function
  | Con (con, ts) as t ->
    (match Con.kind con with
    | Def (tbs, t) -> normalize (reduce tbs t ts)
    | _ -> t
    )
  | Mut t -> Mut (normalize t)
  | t -> t

let rec promote = function
  | Con (con, ts) ->
    let Def (tbs, t) | Abs (tbs, t) = Con.kind con
    in promote (reduce tbs t ts)
  | t -> t


(* Projections *)

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

let invalid s = raise (Invalid_argument ("Type." ^ s))

let as_prim p = function Prim p' when p = p' -> () | _ -> invalid "as_prim"
let as_obj = function Obj (s, tfs) -> s, tfs | _ -> invalid "as_obj"
let as_array = function Array t -> t | _ -> invalid "as_array"
let as_opt = function Opt t -> t | _ -> invalid "as_opt"
let as_variant = function Variant cts -> cts | _ -> invalid "as_variant"
let as_tup = function Tup ts -> ts | _ -> invalid "as_tup"
let as_unit = function Tup [] -> () | _ -> invalid "as_unit"
let as_pair = function Tup [t1; t2] -> t1, t2 | _ -> invalid "as_pair"
let as_func = function Func (s, c, tbs, ts1, ts2) -> s, c, tbs, ts1, ts2 | _ -> invalid "as_func"
let as_async = function Async t -> t | _ -> invalid "as_async"
let as_mut = function Mut t -> t | _ -> invalid "as_mut"
let as_immut = function Mut t -> t | t -> t
let as_serialized = function Serialized t -> t | _ -> invalid "as_serialized"

let as_seq = function Tup ts -> ts | t -> [t]

let as_prim_sub p t = match promote t with
  | Prim p' when p = p' -> ()
  | Non -> ()
  | _ -> invalid "as_prim_sub"
let rec as_obj_sub lab t = match promote t with
  | Obj (s, tfs) -> s, tfs
  | Array t -> as_obj_sub lab (array_obj t)
  | Prim Text -> as_obj_sub lab text_obj
  | Non -> Object Sharable, [{lab; typ = Non}]
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


let inst_func_type fun_ty sort typs =
    let _, tbs, t2, t3 = as_func_sub sort (List.length typs) fun_ty in
    let t_arg = open_ typs t2 in
    let t_ret = open_ typs t3 in
    t_arg, t_ret

let lookup_field lab' tfs =
  match List.find_opt (fun {lab; _} -> lab = lab') tfs with
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
  | Variant cts -> Some (List.length cts)
  | Mut t -> span t
  | Serialized t -> None
  | Non -> Some 0


(* Avoiding local constructors *)

exception Unavoidable of con

let rec avoid' cons = function
  | (Prim _ | Var _ | Any | Non | Shared | Pre) as t -> t
  | Con (c, ts) ->
    if ConSet.mem c cons
    then match Con.kind c with
      | Abs _ -> raise (Unavoidable c)
      | Def (tbs, t) -> avoid' cons (reduce tbs t ts)
    else
      begin try
        Con (c, List.map (avoid' cons) ts)
      with Unavoidable d ->
        match Con.kind c with
        | Def (tbs, t) -> avoid' cons (reduce tbs t ts)
        | Abs _ -> raise (Unavoidable d)
      end
  | Array t -> Array (avoid' cons t)
  | Tup ts -> Tup (List.map (avoid' cons) ts)
  | Func (s, c, tbs, ts1, ts2) ->
    Func (s,
          c,
          List.map (avoid_bind cons) tbs,
          List.map (avoid' cons) ts1, List.map (avoid' cons) ts2)
  | Opt t -> Opt (avoid' cons t)
  | Variant cts -> Variant (map_constr_typ (avoid' cons) cts)
  | Async t -> Async (avoid' cons t)
  | Obj (s, fs) -> Obj (s, List.map (avoid_field cons) fs)
  | Mut t -> Mut (avoid' cons t)
  | Serialized t -> Serialized (avoid' cons t)

and avoid_bind cons {var; bound} =
  {var; bound = avoid' cons bound}

and avoid_field cons {lab; typ} =
  {lab; typ = avoid' cons typ}

let avoid cons t =
  if cons = ConSet.empty then t else
  avoid' cons t

(* Checking for concrete types *)

module TS = Set.Make (struct type t = typ let compare = compare end)

(*
This check is a stop-gap measure until we have an IDL strategy that
allows polymorphic types, see #250. It is not what we desire for ActorScript.
*)

let is_concrete t =
  let seen = ref TS.empty in (* break the cycles *)
  let rec go t =
    TS.mem t !seen ||
    begin
      seen := TS.add t !seen;
      match t with
      | Var _ -> assert false
      | (Prim _ | Any | Non | Shared | Pre) -> true
      | Con (c, ts) ->
        begin match Con.kind c with
        | Abs _ -> false
        | Def (tbs,t) -> go (open_ ts t) (* TBR this may fail to terminate *)
        end
      | Array t -> go t
      | Tup ts -> List.for_all go ts
      | Func (s, c, tbs, ts1, ts2) ->
        let ts = open_binds tbs in
        List.for_all go (List.map (open_ ts) ts1) &&
        List.for_all go (List.map (open_ ts) ts2)
      | Opt t -> go t
      | Variant cts -> List.for_all (fun (_, t) -> go t) cts
      | Async t -> go t
      | Obj (s, fs) -> List.for_all (fun f -> go f.typ) fs
      | Mut t -> go t
      | Serialized t -> go t
    end
  in go t

(* TEMPORARY HOME FOR SOME STUFF related to lub/glb *)

let is_loop_breaker = function
  | Con (c, []) ->
    begin match Con.kind c with
    | Def ([], _) ->
      let name = Con.name c in
      String.length name > 4 && name.[3] = ' '
    | _ -> false
    end
  | _ -> false

       
module M = Map.Make (struct type t = typ * typ let compare = compare end)

(* Equivalence & Subtyping *)

module S = Set.Make (struct type t = typ * typ let compare = compare end)

let rel_list p rel eq xs1 xs2 =
  try List.for_all2 (p rel eq) xs1 xs2 with Invalid_argument _ -> false

let str = ref (fun _ -> failwith "")

let rec rel_typ rel eq t1 t2 = match rel_typ' rel eq t1 t2, rel != eq with
  | true, true -> (*let l = lub t1 t2 in
                  (*let g = glb t1 t2 in*)
                  if not (rel_typ' eq eq t2 l) then Printf.eprintf "bad LUB: %s     (%s     <:     %s) \n%!" (!str l) (!str t1) (!str t2);*)
                  (*if not (rel_typ' eq eq t1 g) then Printf.eprintf "bad GLB: %s     (%s     <:     %s) \n%!" (!str g) (!str t1) (!str t2);*)
                  true
  | r, _ -> r (* TODO: remove this temporary check *)
(*Printf.printf "[sub] %s == %s\n%!" (!str t1) (!str t2);*)


and rel_typ' rel eq t1 t2 =
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
    (match Con.kind con1, Con.kind con2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ' rel eq (open_ ts1 t) t2
    | _, Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ' rel eq t1 (open_ ts2 t)
    | _ when Con.eq con1 con2 ->
      rel_list eq_typ rel eq ts1 ts2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ' rel eq (open_ ts1 t) t2
    | _ ->
      false
    )
  | Con (con1, ts1), t2 ->
    (match Con.kind con1, t2 with
    | Def (tbs, t), _ -> (* TBR this may fail to terminate *)
      rel_typ' rel eq (open_ ts1 t) t2
    | Abs (tbs, t), _ when rel != eq ->
      rel_typ' rel eq (open_ ts1 t) t2
    | _ -> false
    )
  | t1, Con (con2, ts2) ->
    (match Con.kind con2 with
    | Def (tbs, t) -> (* TBR this may fail to terminate *)
      rel_typ' rel eq t1 (open_ ts2 t)
    | _ -> false
    )
  | Prim p1, Prim p2 when p1 = p2 ->
    true
  | Prim p1, Prim p2 when rel != eq ->
    p1 = Nat && p2 = Int
  | Prim p1, Shared when rel != eq ->
    true
  | Prim Text, Obj _ when rel != eq ->
    rel_typ' rel eq text_obj t2
  | Obj (s1, tfs1), Obj (s2, tfs2) ->
    s1 = s2 &&
    rel_fields rel eq tfs1 tfs2
  | Obj (s, _), Shared when rel != eq ->
    s <> Object Local
  | Array t1', Array t2' ->
    rel_typ' rel eq t1' t2'
  | Array t1', Obj _ when rel != eq ->
    rel_typ' rel eq (array_obj t1') t2
  | Array t, Shared when rel != eq ->
    rel_typ' rel eq t Shared
  | Opt t1', Opt t2' ->
    rel_typ' rel eq t1' t2'
  | Opt t1', Shared ->
    rel_typ' rel eq t1' Shared
  | Variant cts1, Variant cts2 ->
    rel_summands rel eq cts1 cts2
  | Variant cts1, Shared ->
    rel_list rel_typ' rel eq (List.map snd cts1) (List.map (fun _ -> Shared) cts1)
  | Prim Null, Opt t2' when rel != eq ->
    true
  | Tup ts1, Tup ts2 ->
    rel_list rel_typ' rel eq ts1 ts2
  | Tup ts1, Shared ->
    rel_list rel_typ' rel eq ts1 (List.map (fun _ -> Shared) ts1)
  | Func (s1, c1, tbs1, t11, t12), Func (s2, c2, tbs2, t21, t22) ->
    c1 = c2 && s1 = s2 &&
    (* subtyping on shared functions needs to imply subtyping of the serialized
       arguments, i.e. the IDL. Until we have a real IDL, we are conservative
       here and assume no subtyping in the IDL. This makes shared functions invariant. *)
    let rel_param =
      if s1 = Sharable then eq_typ else rel_typ' in
    (match rel_binds rel eq tbs1 tbs2 with
    | Some ts ->
      rel_list rel_param rel eq (List.map (open_ ts) t21) (List.map (open_ ts) t11) &&
      rel_list rel_param rel eq (List.map (open_ ts) t12) (List.map (open_ ts) t22)
    | None -> false
    )
  | Func (Sharable, _,  _, _, _), Shared when rel != eq ->
    true
  | Shared, Shared ->
    true
  | Async t1', Async t2' ->
    rel_typ' rel eq t1' t2'
  | Mut t1', Mut t2' ->
    eq_typ rel eq t1' t2'
  | Serialized t1', Serialized t2' ->
    eq_typ rel eq t1' t2' (* TBR: eq or sub? Does it matter? *)
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
      rel_typ' rel eq tf1.typ tf2.typ &&
      rel_fields rel eq tfs1' tfs2'
    | -1 when rel != eq ->
      rel_fields rel eq tfs1' tfs2
    | _ -> false
    )
  | _, _ -> false

and rel_summands rel eq tcs1 tcs2 =
  (* Assume that tcs1 and tcs2 are sorted. *)
  match tcs1, tcs2 with
  | [], [] ->
    true
  | [], _ when rel != eq ->
    true
  | (c1, t1)::tcs1', (c2, t2)::tcs2' ->
    (match compare c1 c2 with
    | 0 ->
      rel_typ' rel eq t1 t2 &&
      rel_summands rel eq tcs1' tcs2'
    | 1 when rel != eq ->
      rel_summands rel eq tcs1 tcs2'
    | _ -> false
    )
  | _, _ -> false

and rel_binds rel eq tbs1 tbs2 =
  let ts = open_binds tbs2 in
  if rel_list (rel_bind ts) rel eq tbs2 tbs1
  then Some ts
  else None

and rel_bind ts rel eq tb1 tb2 =
  rel_typ' rel eq (open_ ts tb1.bound) (open_ ts tb2.bound)

and eq_typ rel eq t1 t2 = rel_typ' eq eq t1 t2

and eq t1 t2 : bool =
  let eq = ref S.empty in eq_typ eq eq t1 t2

and sub t1 t2 : bool =
  let res = rel_typ' (ref S.empty) (ref S.empty) t1 t2 in
  (*let l = lub t1 t2 in
  let g = glb t1 t2 in
  if res && not (eq t2 l) then Printf.printf "bad LUB: %s     (%s     <:     %s) \n%!" (!str l) (!str t1) (!str t2);
  if res && not (eq t1 g) then Printf.printf "bad GLB: %s     (%s     <:     %s) \n%!" (!str g) (!str t1) (!str t2);
   *)res
and sub' t1 t2 : bool =
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

(* Least upper bound and greatest lower bound *)
and lub t1 t2 = lub' (ref M.empty) (ref M.empty) t1 t2
and glb t1 t2 = glb' (ref M.empty) (ref M.empty) t1 t2

and lub' lubs glbs t1 t2 =
  if t1 == t2 then t1 else
  if M.mem (t1, t2) !lubs then Lazy.force (M.find (t1, t2) !lubs) else
  if M.mem (t2, t1) !lubs then Lazy.force (M.find (t2, t1) !lubs) else
    (* TBR: this is just a quick hack *)
  let add_loop_breaker o = let lb = Lazy.from_val (loop_breaker t1 t2 "lub" o) in lubs := M.add (t1, t2) lb !lubs in
  let tr = lazy begin match normalize t1, normalize t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any
  | Any, _ -> Any
  | _, Non -> t1
  | Non, _ -> t2
  | Var (v1, i1), Var (v2, i2) when i1 = i2 && v1 = v2 -> t1
  | Shared, t2 when sub' t2 Shared -> Shared
  | t1, Shared when sub' t1 Shared -> Shared
  | Mut (Var _ as t1'), Mut (Var _ as t2') when t1' = t2' -> t1
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Int
  | Prim Null, Opt t' -> t2
  | Opt t', Prim Null -> t1
  | Array t1', (Obj _ as t2') -> lub' lubs glbs (array_obj t1') t2'
  | (Obj _ as t1'), Array t2' -> lub' lubs glbs t1' (array_obj t2')
  | Prim Text, (Obj _ as t2') -> lub' lubs glbs text_obj t2'
  | (Obj _ as t1'), Prim Text -> lub' lubs glbs t1' text_obj
  | Prim Text, Array t2' -> lub' lubs glbs text_obj (array_obj t2')
  | Array t1', Prim Text -> lub' lubs glbs (array_obj t1') text_obj
  | t1', t2' when eq t1' t2' -> t1
  (* Potentially recursive types follow *)
  | Opt t1', Opt t2' ->
    let rec o = Opt' i
    and i = lazy (add_loop_breaker o; lub' lubs glbs t1' t2') in
    fixup o
  | Variant t1', Variant t2' ->
    let rec o = Variant' i
    and i = lazy (add_loop_breaker o; lub_variant lubs glbs t1' t2') in
    fixup o
  | Tup ts1, Tup ts2 when List.(length ts1 = length ts2) ->
    let rec o = Tup' i
    and i = lazy (add_loop_breaker o; List.map2 (lub' lubs glbs) ts1 ts2) in
    fixup o
  | Array t1', Array t2' ->
    let rec o = Array' i
    and i = lazy (add_loop_breaker o; lub' lubs glbs t1' t2') in
    fixup o
  | Obj (s1, tf1), Obj (s2, tf2) when s1 = s2 ->
    let rec o = Obj' (s1, i)
    and i = lazy (add_loop_breaker o; lub_object lubs glbs tf1 tf2) in
    fixup o
  | Func (s1, c1, bs1, args1, res1), Func (s2, c2, bs2, args2, res2)
    when s1 = s2 && c1 = c2 && bs1 = bs2 && (* TBR: alpha-equivalence, bounds *)
      List.(length args1 = length args2 && length res1 = length res2) ->
    let rec o = Func' (s1, c1, bs1, args, results)
    and lb = lazy (add_loop_breaker o)
    and args = lazy (Lazy.force lb; List.map2 (glb' lubs glbs) args1 args2)
    and results = lazy (Lazy.force lb; List.map2 (lub' lubs glbs) res1 res2) in
    fixup o
  | Async t1', Async t2' ->
    let rec o = Async' i
    and i = lazy (lub' lubs glbs t1' t2') in
    fixup o
  | _ -> Any
  end in
  lubs := M.add (t1, t2) tr !lubs;
  ignore (Lazy.force tr); (* kickstart evaluation *)
  Lazy.force (M.find (t1, t2) !lubs) (* fetch potential loop-breaker *)

(* The presence of Con can potentially lead to direct type recursion
   in the lub/glb result. To avoid introducing a loop into the type,
   wrap the putative result by a descriptively named Con that redirects to it.
 *)
and loop_breaker t1 t2 how cand = match t1, t2 with
  | Con _, _
  | _, Con _ -> (* TODO(gabor) only when Con is recursive! *)
    let c = Con.fresh (Printf.sprintf "%s (%s, %s)" how (!str t1) (!str t2)) (Def ([], coerced_typ cand)) in
    let wrap = Con (c, []) in
    assert (normalize wrap == coerced_typ cand);
    wrap
  | _, _ -> coerced_typ cand

and lub_object lubs glbs fs1 fs2 = match fs1, fs2 with
  | _, [] -> []
  | [], _ -> []
  | f1::fs1', f2::fs2' ->
    begin match compare_field f1 f2 with
    | 0 -> {lab = f1.lab; typ = lub' lubs glbs f1.typ f2.typ}::lub_object lubs glbs fs1' fs2'
    | 1 -> lub_object lubs glbs fs1 fs2'
    | _ -> lub_object lubs glbs fs1' fs2
    end

and lub_variant lubs glbs fs1 fs2 = match fs1, fs2 with
  | fs1, [] -> fs1
  | [], fs2 -> fs2
  | f1::fs1', f2::fs2' ->
    begin match compare_summand f1 f2 with
    | 0 -> (fst f1, lub' lubs glbs (snd f1) (snd f2))::lub_variant lubs glbs fs1' fs2'
    | 1 -> f2::lub_variant lubs glbs fs1 fs2'
    | _ -> f1::lub_variant lubs glbs fs1' fs2
    end

and glb' lubs glbs t1 t2 =
  let add_loop_breaker o = glbs := M.add (t1, t2) (Lazy.from_val (loop_breaker t1 t2 "glb" o)) !glbs in
  if t1 == t2 then t1 else
  if M.mem (t1, t2) !glbs then Lazy.force (M.find (t1, t2) !glbs) else
  if M.mem (t2, t1) !glbs then Lazy.force (M.find (t2, t1) !glbs) else
  (* TBR: this is just a quick hack *)
  let tr = lazy begin match normalize t1, normalize t2 with
  | _, Pre
  | Pre, _ -> Pre
  | _, Any -> t1
  | Any, _ -> t2
  | _, Non -> Non
  | Non, _ -> Non
  | Var (v1, i1), Var (v2, i2) when i1 = i2 && v1 = v2 -> t1
  | Mut (Var _ as t1'), Mut (Var _ as t2') when t1' = t2' -> t1 (* TODO(gabor) not general enough *)
  | Shared, t2 when sub' t2 Shared -> t2
  | t1, Shared when sub' t1 Shared -> t1
  | Prim Nat, Prim Int
  | Prim Int, Prim Nat -> Prim Nat
  | Prim Null, Opt _
  | Opt _, Prim Null -> Prim Null
  | Array t1', (Obj _ as t2') when sub' (array_obj t1') t2' -> t1
  | (Obj _ as t1'), Array t2' when sub' (array_obj t2') t1' -> t2
  | Prim Text, (Obj _ as t2') when sub' text_obj t2' -> t1
  | (Obj _ as t1'), Prim Text when sub' text_obj t1' -> t2

  | t1', t2' when eq t1' t2' -> t1
  (* Potentially recursive types follow *)
  | Opt t1', Opt t2' ->
    let rec o = Opt' i
    and i = lazy (add_loop_breaker o; glb' lubs glbs t1' t2') in
    fixup o
  | Variant t1', Variant t2' ->
    let rec o = Variant' i
    and i = lazy (add_loop_breaker o; glb_variant lubs glbs t1' t2')
    in fixup o
  | Tup ts1, Tup ts2 when List.(length ts1 = length ts2) ->
    let rec o = Tup' i
    and i = lazy (add_loop_breaker o; List.map2 (glb' lubs glbs) ts1 ts2) in
    fixup o
  | Array t1', Array t2' ->
    let rec o = Array' i
    and i = lazy (add_loop_breaker o; glb' lubs glbs t1' t2') in
    fixup o
  | Obj (s1, tf1), Obj (s2, tf2) when s1 = s2 ->
    let rec o = Obj' (s1, i)
    and i = lazy (add_loop_breaker o; glb_object lubs glbs tf1 tf2) in
    fixup o
  | Func (s1, c1, bs1, args1, res1), Func (s2, c2, bs2, args2, res2)
    when s1 = s2 && c1 = c2 && bs1 = bs2 && (* TBR: alpha-equivalence, bounds *)
      List.(length args1 = length args2 && length res1 = length res2) ->
    let rec o = Func' (s1, c1, bs1, args, results)
    and lb = lazy (add_loop_breaker o)
    and args = lazy (Lazy.force lb; List.map2 (lub' lubs glbs) args1 args2)
    and results = lazy (Lazy.force lb; List.map2 (glb' lubs glbs) res1 res2) in
    fixup o
  | Async t1', Async t2' ->
    let rec o = Async' i
    and i = lazy (glb' lubs glbs t1' t2') in
    fixup o
  | _ -> Non
  end in
  glbs := M.add (t1, t2) tr !glbs;
  ignore (Lazy.force tr); (* kickstart evaluation *)
  Lazy.force (M.find (t1, t2) !glbs) (* fetch potential loop-breaker *)

and glb_object lubs glbs fs1 fs2 = match fs1, fs2 with
  | fs1, [] -> fs1
  | [], fs2 -> fs2
  | f1::fs1', f2::fs2' ->
    begin match compare_field f1 f2 with
    | 0 -> {lab = f1.lab; typ = glb' lubs glbs f1.typ f2.typ}::glb_object lubs glbs fs1' fs2'
    | 1 -> f2::glb_object lubs glbs fs1 fs2'
    | _ -> f1::glb_object lubs glbs fs1' fs2
    end

and glb_variant lubs glbs fs1 fs2 = match fs1, fs2 with
  | _, [] -> []
  | [], _ -> []
  | f1::fs1', f2::fs2' ->
    begin match compare_summand f1 f2 with
    | 0 -> (fst f1, glb' lubs glbs (snd f1) (snd f2))::glb_variant lubs glbs fs1' fs2'
    | 1 -> glb_variant lubs glbs fs1 fs2'
    | _ -> glb_variant lubs glbs fs1' fs2
    end

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
  | Con (c, []) -> string_of_con vs c
  | Con (c, ts) ->
    sprintf "%s<%s>" (string_of_con vs c)
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
  | Variant [] -> "{#}"
  | Variant cts ->
    sprintf "{%s}" (String.concat "; " (List.map (string_of_summand vs) cts))
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
    let vs' = vars_of_binds vs tbs in
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
  | Serialized t ->
    sprintf "serialized %s" (string_of_typ' vs t)
  | t -> string_of_typ_nullary vs t

and string_of_field vs {lab; typ} =
  sprintf "%s : %s" lab (string_of_typ' vs typ)

and string_of_summand vs = function
  | (tag, Tup []) -> sprintf "#%s" tag
  | (tag, typ) -> sprintf "#%s : %s" tag (string_of_typ' vs typ)

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

let string_of_typ = string_of_typ' []
let _ = str := string_of_typ

let strings_of_kind k =
  let op, tbs, t =
    match k with
    | Def (tbs, t) -> "=", tbs, t
    | Abs (tbs, t) -> "<:", tbs, t
  in
  let vs = vars_of_binds [] tbs in
  op, string_of_binds vs vs tbs, string_of_typ' vs t

let string_of_kind k =
  let op, sbs, st = strings_of_kind k in
  sprintf "%s %s%s" op sbs st


let rec string_of_typ_expand t =
  let s = string_of_typ t in
  match t with
  | Con (c, ts) ->
    (match Con.kind c with
    | Abs _ -> s
    | Def _ ->
      match normalize t with
      | Prim _ | Any | Non -> s
      | t' -> s ^ " = " ^ string_of_typ_expand t'
    )
  | _ -> s


(* Environments *)

module Env = Env.Make(String)
