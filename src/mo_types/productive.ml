open Type

(*
  Maintain a map from con to info = {Nonproductive, Productive, Param n}`.
  After the analysis, this map says, for all type constructors, whether they are  really unproductive, productive, or if their unfolding is equal to one of their parameters.
* Maintain a set of type definitions under scrutiny
* For each type definition:
  * If it is already in the memo map, use that result. Else analyze it as follows, updating the memo map afterwards.
  * If it is already in the set, we have found a loop. Mark it as `unproductive`. Else add to the setduring the next step.
  * Consider its RHS:
    * If it is a concrete type, return `Productive`.
    * If it is the nth type parameter, return `Param n`
    * If it is a type application `T<t1,…,tn>`, recurse.
      * If recursion returns `Productive` or `Nonproductive`, return that.
      * If recursion returns `Param n`, loop to “Consider its RHS”, as if `tn`
        is the RHS.
*)

type info =
  | Nonproductive
  | Productive
  | Param of int

let rec rhs cs ce t : (info ConEnv.t * info) = match t with
  | Pre
  | Mut _ | Typ _ ->
    assert false (* body of a Def shouldn't be 2nd class *)
  | Var (s, j) ->
    (ce, Param j)
  | Con (d, ts) ->
    begin
      let (ce', info) = productive_con cs d ce in
      match info with
      | (Nonproductive | Productive) -> (ce', info)
      | Param n ->
        match Con.kind d with
        | Def (tbs, t) ->
          assert (n < List.length tbs); (* assume types are arity-correct *)
          rhs cs ce' (List.nth ts n)
        | Abs (tbs, t) ->
          (* we could assert here since Defs should be closed *)
          (ce', Productive)
    end
  | _ ->  (* anything else is productive *)
    (ce, Productive)

and productive_con cs c ce =
  match ConEnv.find_opt c ce with
  | Some info -> (ce, info)
  | None ->
    if ConSet.mem c cs then
      (ConEnv.add c Nonproductive ce, Nonproductive)
    else
      let cs' = ConSet.add c cs in
      let (ce', info) = rhs cs' ce (match Con.kind c with Def (_, t) -> t | _ -> assert false) in
      (ConEnv.add c info ce', info)

let non_productive cs =
  let ce = ConSet.fold
    (fun c ce -> let (ce', _) = productive_con ConSet.empty c ce in ce')
    cs ConEnv.empty
  in
  ConSet.filter (fun c -> ConEnv.find c ce = Nonproductive) cs
