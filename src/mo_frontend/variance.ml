(* Variance (of type variables) in a type *)
open Mo_types
open Type

(* NB: Polarities form a simple lattice with
      Invariant
     /        \
    Covariant  Contravariant
     \        /
      Bivariant
*)
type t = Bivariant | Covariant | Contravariant | Invariant


(* Least upper bound of two polarities *)
let join p1 p2 =
  match p1, p2 with
  | other, Bivariant
  | Bivariant, other -> other
  | Covariant, Contravariant
  | Contravariant, Covariant -> Invariant
  | other, Invariant -> Invariant
  | Invariant, other -> Invariant
  | Covariant, Covariant ->  Covariant
  | Contravariant, Contravariant -> Contravariant

let flip p =
    match p with
    | Bivariant -> Bivariant
    | Covariant -> Contravariant
    | Contravariant -> Covariant
    | Invariant -> Invariant

module PS = Set.Make
 (struct
    type pol = t (* rename to avoid capture below *)
    type t = pol * typ
    let compare (p1,t1) (p2, t2) =
      match compare p1 p2 with
      | 0 -> Ord.compare t1 t2
      | o -> o
  end)

let variances cons t =
  let map = ref
    (ConSet.fold (fun c ce -> ConEnv.add c Bivariant ce) cons ConEnv.empty)
  in
  let seen = ref PS.empty in
  let rec go p t =
    if PS.mem (p,t) !seen then ()
    else begin
      seen := PS.add (p,t) !seen;
      match t with
      | Var _ | Pre -> assert false
      | Prim _ | Any | Non -> ()
      | Con (c, []) when ConSet.mem c cons ->
        map := ConEnv.add c (join p (ConEnv.find c !map)) (!map)
      | Con (c, ts) ->
        (match Cons.kind c with
        | Abs _ -> ()
        | Def (_, t) -> go p (open_ ts t)) (* TBR this may fail to terminate *)
      | Array t | Opt t -> go p t
      | Mut t -> go Invariant t
      | Async (t1, t2) ->
        go Invariant t1;
        go p t2
      | Tup ts -> List.iter (go p) ts
      | Obj (_, fs) | Variant fs -> List.iter (fun f -> go p f.typ) fs
      | Func (s, c, tbs, ts1, ts2) ->
        let ts = open_binds tbs in
        List.iter (fun tb ->
          go Invariant (open_ ts tb.bound)) tbs; (* bounds are invariant *)
        List.iter (go (flip p)) (List.map (open_ ts) ts1);
        List.iter (go p) (List.map (open_ ts) ts2)
      | Typ c -> () (* TBR  assumed closed *)
    end
  in
  go Covariant t;
  !map
