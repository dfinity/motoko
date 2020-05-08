module IM = Map.Make (struct type t = int let compare = compare end)
module IS = Set.Make (struct type t = int let compare = compare end)

(* The root is at node 0 *)
type 'a t = ('a * int list) IM.t

module Unfold(M : Map.S) = struct

type e = M.key

let unfold node (root : e) : 'a t =
  let seen = ref M.empty in
  let counter = ref 0 in
  let graph = ref IM.empty in
  let rec go e : int =
    match M.find_opt e !seen with
    | Some i -> i
    | None ->
      let i = !counter in
      counter := !counter + 1;
      seen := M.add e i !seen;
      let (k, args) = node e in
      let args' = List.map go args in
      graph := IM.add i (k, args') !graph;
      i
  in
  let i = go root in
  assert (i == 0);
  !graph

end (* Unfold *)

(* Maps an index mapping over the graph. If not injective, will combine nodes *)
let rename graph (lookup : int -> int) =
  let lookups = List.map lookup in
  let r = ref IM.empty in
  IM.iter (fun i (k, args) ->
    r := IM.add (lookup i) (k, lookups args) !r
  ) graph;
  !r

(* Finds a minimal graph by finding the smallest index mapping that is consistent *)
let combine graph =
  let m = ref IM.empty in
  (* map all types to the same initially *)
  IM.iter (fun i _ -> m := IM.add i 0 !m) graph;
  let counter = ref 1 in (* first free index *)

  let any_change = ref true in

  let lookup i = IM.find i !m in
  let lookups = List.map lookup in
  let change j =
    any_change := true;
    m := IM.add j !counter !m;
    counter := !counter + 1
  in

  (* Now iterate, separating the equivalence classes if we find evidence *)
  while !any_change do
    any_change := false;
    (* stupid quadradric algorithm for now *)
    (* could be improved by maintianing a list of equivalence classes *)
    IM.iter (fun i (k1, args1) ->
      IM.iter (fun j (k2, args2) ->
        if i < j then
        if lookup i = lookup j then
        if not (k1 = k2 && lookups args1 = lookups args2) then
        (* i and j must not be mapped to each other, so change j*)
        change j
      ) graph
    ) graph
  done;

  assert (lookup 0 = 0);
  rename graph lookup

(* Changes the number to be canonical (depth first) *)
let renumber graph =
  let m = ref IM.empty in
  let counter = ref 0 in

  let rec go i = match IM.find_opt i !m with
    | None -> (* no seen before *)
      m := IM.add i !counter !m;
      counter := !counter + 1;
      let (k, args) = IM.find i graph in
      List.iter go args
    | Some _ -> ()
  in
  go 0;

  assert (IM.find 0 !m = 0);
  rename graph (fun i -> IM.find i !m)

(* Find a canonical graph *)
let canonicalize graph = renumber (combine graph)

(* Folds over the graph *)
let fold
  (of_con : 'a -> 'b list -> 'b)
  (of_def : int -> 'b -> 'b)
  (of_ref : int -> 'b)
  (graph : 'a t) : 'b =

  (* Find which entries are referenced more than once *)
  let tally : int IM.t =
    let tally = ref IM.empty in
    let succ = function | None -> Some 1 | Some i -> Some (i + 1) in
    let bump i = tally := IM.update i succ !tally in
    bump 0;
    IM.iter (fun _ (_, args) -> List.iter bump args) graph;
    !tally
  in

  (* Now fold the graph using the user-provided combinators *)
  let seen = ref IS.empty in
  let rec go_con i : 'b =
    (* This node is only printed once *)
    let (k, args) = IM.find i graph in
    of_con k (List.map go args)
  and go i : 'b =
    (* This node is only printed once: *)
    if IM.find i tally = 1 then go_con i else
    (* We have seen this before: *)
    if IS.mem i !seen then of_ref i
    (* This is a shared node, first visit: *)
    else (seen := IS.add i !seen; of_def i (go_con i))
  in
  go 0
