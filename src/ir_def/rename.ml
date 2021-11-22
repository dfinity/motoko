open Source
open Ir

module Renaming = Map.Make(String)

(* One traversal for each syntactic category, named by that category *)

let fresh_id id = Construct.fresh_id id ()

let id rho i =
  try Renaming.find i rho
  with Not_found -> i

let id_bind rho i =
  let i' = fresh_id i in
  (i', Renaming.add i i' rho)

let arg_bind rho a =
  let i' = fresh_id a.it in
  ({a with it = i'}, Renaming.add a.it i' rho)

let rec prim rho p =
  Ir.map_prim (fun t -> t) (id rho) p (* rename BreakPrim id etc *)

and exp rho e  =  {e with it = exp' rho e.it}
and exp' rho e  = match e with
  | VarE i              -> VarE (id rho i)
  | LitE l              -> e
  | PrimE (p, es)       -> PrimE (prim rho p, List.map (exp rho) es)
  | ActorE (ds, fs, { meta; pre; post }, t) ->
    let ds', rho' = decs rho ds in
    ActorE
      (ds',
       fields rho' fs,
       {meta; pre = exp rho' pre; post = exp rho' post},
       t)
  | AssignE (e1, e2)    -> AssignE (lexp rho e1, exp rho e2)
  | BlockE (ds, e1)     -> let ds', rho' = decs rho ds
                           in BlockE (ds', exp rho' e1)
  | IfE (e1, e2, e3)    -> IfE (exp rho e1, exp rho e2, exp rho e3)
  | SwitchE (e, cs)     -> SwitchE (exp rho e, cases rho cs)
  | LoopE e1            -> LoopE (exp rho e1)
  | LabelE (i, t, e)    -> let i',rho' = id_bind rho i in
                           LabelE(i', t, exp rho' e)
  | AsyncE (tb, e, t)   -> AsyncE (tb, exp rho e, t)
  | DeclareE (i, t, e)  -> let i',rho' = id_bind rho i in
                           DeclareE (i', t, exp rho' e)
  | DefineE (i, m, e)   -> DefineE (id rho i, m, exp rho e)
  | FuncE (x, s, c, tp, p, ts, e) ->
     let p', rho' = args rho p in
     let e' = exp rho' e in
     FuncE (x, s, c, tp, p', ts, e')
  | NewObjE (s, fs, t)  -> NewObjE (s, fields rho fs, t)
  | TryE (e, cs)        -> TryE (exp rho e, cases rho cs)
  | SelfCallE (ts, e1, e2, e3) ->
     SelfCallE (ts, exp rho e1, exp rho e2, exp rho e3)

and lexp rho le = {le with it = lexp' rho le.it}
and lexp' rho = function
  | VarLE i  -> VarLE (id rho i)
  | DotLE (e, i) -> DotLE (exp rho e, i)
  | IdxLE (e1, e2) -> IdxLE (exp rho e1, exp rho e2)

and exps rho es  = List.map (exp rho) es

and fields rho fs =
  List.map (fun f -> { f with it = { f.it with var = id rho f.it.var } }) fs

and args rho as_ =
  match as_ with
  | [] -> ([],rho)
  | a::as_ ->
     let (a', rho') = arg_bind rho a in
     let (as_', rho'') = args rho' as_ in
     (a'::as_', rho'')

and pat rho p =
    let p',rho = pat' rho p.it in
    {p with it = p'}, rho

and pat' rho p = match p with
  | WildP         -> (p, rho)
  | VarP i        ->
    let i, rho' = id_bind rho i in
     (VarP i, rho')
  | TupP ps       -> let (ps, rho') = pats rho ps in
                     (TupP ps, rho')
  | ObjP pfs      ->
    let (pats, rho') = pats rho (pats_of_obj_pat pfs) in
    (ObjP (replace_obj_pat pfs pats), rho')
  | LitP l        -> (p, rho)
  | OptP p        -> let (p', rho') = pat rho p in
                     (OptP p', rho')
  | TagP (i, p)   -> let (p', rho') = pat rho p in
                     (TagP (i, p'), rho')
  | AltP (p1, p2) -> assert(Freevars.(M.is_empty (pat p1)));
                     assert(Freevars.(M.is_empty (pat p2)));
                     let (p1', _) = pat rho p1 in
                     let (p2' ,_) = pat rho p2 in
                     (AltP (p1', p2'), rho)

and pats rho ps  =
  match ps with
  | [] -> ([],rho)
  | p::ps ->
     let (p', rho') = pat rho p in
     let (ps', rho'') = pats rho' ps in
     (p'::ps', rho'')

and case rho (c : case) =
  {c with it = case' rho c.it}
and case' rho { pat = p; exp = e} =
  let (p', rho') = pat rho p in
  let e' = exp rho' e in
  {pat=p'; exp=e'}

and cases rho cs = List.map (case rho) cs

and dec rho d =
  let (mk_d, rho') = dec' rho d.it in
  ({d with it = mk_d}, rho')

and dec' rho d = match d with
  | LetD (p, e) ->
     let p', rho = pat rho p in
     (fun rho' -> LetD (p',exp rho' e)),
     rho
  | VarD (i, t, e) ->
     let i', rho = id_bind rho i in
     (fun rho' -> VarD (i', t, exp rho' e)),
     rho

and decs rho ds =
  let rec decs_aux rho ds =
    match ds with
    | [] -> ([], rho)
    | d::ds ->
       let (mk_d, rho') = dec rho d in
       let (mk_ds, rho'') = decs_aux rho' ds in
       (mk_d::mk_ds, rho'')
  in
  let mk_ds, rho' = decs_aux rho ds in
  let ds' = List.map (fun mk_d -> { mk_d with it = mk_d.it rho' } ) mk_ds in
  (ds', rho')

let comp_unit rho cu = match cu with
  | ProgU ds ->
    let ds', _ = decs rho ds in
    ProgU ds'
  | LibU (ds, e) ->
    let ds', rho' = decs rho ds
    in LibU (ds', exp rho' e)
  | ActorU (as_opt, ds, fs, { meta; pre; post }, t) ->
    let as_opt', rho' = match as_opt with
      | None -> None, rho
      | Some as_ ->
        let as_', rho' = args rho as_ in
        Some as_', rho'
    in
    let ds', rho'' = decs rho' ds in
    ActorU (as_opt', ds', fields rho'' fs,
      { meta; pre = exp rho'' pre; post = exp rho'' post }, t)
