open Source
module S = Syntax
open Ir

module Renaming = Map.Make(String)

(* One traversal for each syntactic category, named by that category *)

let stamp = ref 0

let fresh_id id =
  let i' = Printf.sprintf "%s@%i" id.it (!stamp) in
  stamp := !stamp+1;
  i'

let id rho i =
  try {i with it = Renaming.find i.it rho}
  with Not_found -> i

let id_bind rho i =
  let i' = fresh_id i in
  ({i with it = i'}, Renaming.add i.it i' rho)

let rec exp rho e  =
    {e with it = exp' rho e.it}

and exp' rho e  = match e with
  | VarE i              -> VarE (id rho i)
  | LitE l              -> e
  | PrimE _             -> e
  | UnE (ot, uo, e)      -> UnE (ot, uo, exp rho e)
  | BinE (ot, e1, bo, e2)-> BinE (ot, exp rho e1, bo, exp rho e2)
  | RelE (ot, e1, ro, e2)-> RelE (ot, exp rho e1, ro, exp rho e2)
  | TupE es             -> TupE (List.map (exp rho) es)
  | ProjE (e, i)        -> ProjE (exp rho e, i)
  | ActorE (i, ds, fs, t)-> let i',rho' = id_bind rho i in
                            let ds', rho'' = decs rho' ds
                            in ActorE (i', ds', fields rho'' fs, t)
  | DotE (e, i)         -> DotE (exp rho e, i)
  | ActorDotE (e, i)    -> ActorDotE (exp rho e, i)
  | AssignE (e1, e2)    -> AssignE (exp rho e1, exp rho e2)
  | ArrayE (m, t, es)   -> ArrayE (m, t, exps rho es)
  | IdxE (e1, e2)       -> IdxE (exp rho e1, exp rho e2)
  | CallE (cc, e1, ts, e2)
                        -> CallE  (cc, exp rho e1, ts, exp rho e2)
  | BlockE (ds, e1)     -> let ds', rho' = decs rho ds
                           in BlockE (ds', exp rho' e1)
  | IfE (e1, e2, e3)    -> IfE (exp rho e1, exp rho e2, exp rho e3)
  | SwitchE (e, cs)     -> SwitchE (exp rho e, cases rho cs)
  | LoopE e1            -> LoopE (exp rho e1)
  | LabelE (i, t, e)    -> let i',rho' = id_bind rho i in
                           LabelE(i', t, exp rho' e)
  | BreakE (i, e)       -> BreakE(id rho i,exp rho e)
  | RetE e              -> RetE (exp rho e)
  | AsyncE e            -> AsyncE (exp rho e)
  | AwaitE e            -> AwaitE (exp rho e)
  | AssertE e           -> AssertE (exp rho e)
  | OptE e              -> OptE (exp rho e)
  | DeclareE (i, t, e)  -> let i',rho' = id_bind rho i in
                           DeclareE (i', t, exp rho' e)
  | DefineE (i, m, e)   -> DefineE (id rho i, m, exp rho e)
  | FuncE (x, s, tp, p, t, e) ->
     let p', rho' = pat rho p in
     let e' = exp rho' e in
     FuncE (x, s, tp, p', t, e')
  | NewObjE (s, fs, t)  -> NewObjE (s, fields rho fs, t)

and exps rho es  = List.map (exp rho) es

and fields rho fs =
  List.map (fun f -> { f with it = { f.it with var = id rho f.it.var } }) fs

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
  | LitP l        -> (p, rho)
  | OptP p        -> let (p', rho') = pat rho p in
                     (OptP p', rho')
  | AltP (p1, p2) -> assert(Freevars.S.is_empty (snd (Freevars.pat p1)));
                     assert(Freevars.S.is_empty (snd (Freevars.pat p2)));
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
  | VarD (i, e) ->
     let i', rho = id_bind rho i in
     (fun rho' -> VarD (i',exp rho' e)),
     rho
  | TypD c -> (* we don't rename type names *)
     (fun rho -> d),
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
