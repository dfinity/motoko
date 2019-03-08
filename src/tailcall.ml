open Source
open Ir
open Effect
open Type
open Construct
module S = Syntax

(* Optimize (self) tail calls to jumps, avoiding stack overflow
   in a single linear pass *)

(*
This is simple tail call optimizer that replaces tail calls to the current function by jumps.
It can  easily be extended to non-self tail calls, once supported by wasm.

For each function `f` whose `body[...]` has at least one self tailcall to `f<Ts>(es)`, apply the transformation:
```
    func f<Ts>(pat) = body[f<Ts>(es)+]
    ~~>
    func f<Ts>(args) = {
       var temp = args;
       loop {
         label l {
           let pat = temp;
           return body[{temp := es;break l;}+]
        }
      }
    }
```


It's implemented by a recursive traversal that maintains an environment recording whether the current term is in tail position,
and what its enclosing function (if any) is.

The enclosing function is forgotten when shadowed by a local binding (we don't assume all variables are distinct) and when
entering a function, class or actor constructor.

On little gotcha for functional programmers: the argument `e` to an early `return e` is *always* in tail position,
regardless of `return e`s own tail position.

TODO: optimize for multiple arguments using multiple temps (not a tuple).

*)

type func_info = { func: S.id;
                   typ_binds: typ_bind list;
                   temp: var;
                   label: S.id;
                   tail_called: bool ref;
                 }

type env = { tail_pos:bool;          (* is the expression in tail position *)
             info: func_info option; (* the innermost enclosing func, if any *)
           }


let bind env i (info:func_info option) : env =
  match info with
  | Some _ ->
    { env with info = info; }
  | None ->
    match env.info with
    | Some { func; _}  when i.it = func.it ->
      { env with info = None } (* remove shadowed func info *)
    | _ -> env (* preserve existing, non-shadowed info *)


let are_generic_insts (tbs : typ_bind list) insts =
  List.for_all2 (fun (tb : typ_bind) inst ->
      match inst with
      | Con(c2,[]) -> Con.eq tb.it.con c2 (* conservative, but safe *)
      |  _ -> false
      ) tbs insts

let rec tailexp env e =
  {e with it = exp' env e}

and exp env e  : exp =
  {e with it = exp' {env with tail_pos = false}  e}

and exp' env e  : exp' = match e.it with
  | VarE _
    | LitE _
    | PrimE _             -> e.it
  | UnE (ot, uo, e)      -> UnE (ot, uo, exp env e)
  | BinE (ot, e1, bo, e2)-> BinE (ot, exp env e1, bo, exp env e2)
  | RelE (ot, e1, ro, e2)-> RelE (ot, exp env e1, ro, exp env e2)
  | TupE es             -> TupE (List.map (exp env) es)
  | ProjE (e, i)        -> ProjE (exp env e, i)
  | DotE (e, sn)        -> DotE (exp env e, sn)
  | ActorDotE (e, sn)   -> ActorDotE (exp env e, sn)
  | AssignE (e1, e2)    -> AssignE (exp env e1, exp env e2)
  | ArrayE (m,t,es)     -> ArrayE (m,t,(exps env es))
  | IdxE (e1, e2)       -> IdxE (exp env e1, exp env e2)
  | CallE (cc, e1, insts, e2)  ->
    begin
      match e1.it, env with
      | VarE f1, { tail_pos = true;
                   info = Some { func; typ_binds; temp; label; tail_called } }
           when f1.it = func.it && are_generic_insts typ_binds insts  ->
        tail_called := true;
        (blockE [expD (assignE temp (exp env e2))]
                 (breakE label (tupE []))).it
      | _,_-> CallE(cc, exp env e1, insts, exp env e2)
    end
  | BlockE (ds, e)      -> BlockE (block env ds e)
  | IfE (e1, e2, e3)    -> IfE (exp env e1, tailexp env e2, tailexp env e3)
  | SwitchE (e, cs)     -> SwitchE (exp env e, cases env cs)
  | WhileE (e1, e2)     -> WhileE (exp env e1, exp env e2)
  | LoopE (e1, None)    -> LoopE (exp env e1, None)
  | LoopE (e1, Some e2) -> LoopE (exp env e1, Some (exp env e2))
  | ForE (p, e1, e2)    -> let env1 = pat env p in
                           ForE (p, exp env e1, exp env1 e2)
  | LabelE (i, t, e)    -> let env1 = bind env i None in
                           LabelE(i, t, exp env1 e)
  | BreakE (i, e)       -> BreakE(i,exp env e)
  | RetE e              -> RetE (tailexp { env with tail_pos = true } e)
  (* NB:^ e is always in tailposition, regardless of fst env *)
  | AsyncE e            -> AsyncE (exp { tail_pos = true; info = None } e)
  | AwaitE e            -> AwaitE (exp env e)
  | AssertE e           -> AssertE (exp env e)
  | OptE e              -> OptE (exp env e)
  | DeclareE (i, t, e)  -> let env1 = bind env i None in
                           DeclareE (i, t, tailexp env1 e)
  | DefineE (i, m, e)   -> DefineE (i, m, exp env e)
  | FuncE (x, cc, tbs, p, typT, exp0) ->
    let env1 = pat {tail_pos = true; info = None} p in
    let exp0' = tailexp env1 exp0 in
    FuncE (x, cc, tbs, p, typT, exp0')
  | ActorE (i, ds, fs, t) -> ActorE (i, ds, fs, t) (* TODO: decent into ds *)
  | NewObjE (s,is,t)    -> NewObjE (s, is, t)

and exps env es  = List.map (exp env) es

and pat env p =
  let env = pat' env p.it in
  env

and pat' env p = match p with
  | WildP          ->  env
  | VarP i        ->
    let env1 = bind env i None in
    env1
  | TupP ps       -> pats env ps
  | LitP l        -> env
  | OptP p        -> pat env p
  | AltP (p1, p2) -> assert(Freevars.S.is_empty (snd (Freevars.pat p1)));
                     assert(Freevars.S.is_empty (snd (Freevars.pat p2)));
                     env

and pats env ps  =
  match ps with
  | [] -> env
  | p :: ps ->
    let env1 = pat env p in
    pats env1 ps

and case env (c : case) =
  { c with it = case' env c.it }
and case' env {pat=p;exp=e} =
  let env1 = pat env p in
  let e' = tailexp env1 e in
  { pat=p; exp=e' }


and cases env cs = List.map (case env) cs

and dec env d =
  let (mk_d,env1) = dec' env d in
  ({d with it = mk_d}, env1)

and dec' env d =
  match d.it with
  (* A local let bound function, this is what we are looking for *)
  | LetD (({it = VarP id;_} as id_pat),
          ({it = FuncE (x, ({ Value.sort = Local; _} as cc), tbs, p, typT, exp0);_} as funexp)) ->
    let env = bind env id None in
    begin fun env1 ->
      let temp = fresh_var (Mut p.note) in
      let l = fresh_id () in
      let tail_called = ref false in
      let env2 = { tail_pos = true;
                   info = Some { func = id;
                                 typ_binds = tbs;
                                 temp = temp;
                                 label = l;
                                 tail_called = tail_called } }
      in
      let env3 = pat env2 p  in (* shadow id if necessary *)
      let exp0' = tailexp env3 exp0 in
      let cs = List.map (fun (tb : typ_bind) -> Con (tb.it.con, [])) tbs in
      if !tail_called then
        let ids = match typ funexp with
          | Func( _, _, _, dom, _) -> List.map (fun t -> fresh_var (open_ cs t)) dom
          | _ -> assert false
        in
        let args = seqP (List.map varP ids) in
        let l_typ = Type.unit in
        let body =
          blockE [varD (id_of_exp temp) (seqE ids)]
            (loopE
              (labelE l l_typ
                 (blockE [letP p (immuteE temp)] (retE exp0'))) None)
        in
        LetD (id_pat, {funexp with it = FuncE (x, cc, tbs, args, typT, body)})
      else
        LetD (id_pat, {funexp with it = FuncE (x, cc, tbs, p, typT, exp0')})
    end,
    env
  | LetD (p, e) ->
    let env = pat env p in
    (fun env1 -> LetD(p,exp env1 e)),
    env
  | VarD (i, e) ->
    let env = bind env i None in
    (fun env1 -> VarD(i,exp env1 e)),
    env
  | TypD _ ->
    (fun env -> d.it),
    env

and block env ds exp =
  let rec decs_aux env ds =
    match ds with
    | [] -> ([],env)
    | d::ds ->
      let (mk_d, env1) = dec env d in
      let (mk_ds, env2) = decs_aux env1 ds in
      (mk_d :: mk_ds,env2)
  in
  let mk_ds,env1 = decs_aux env ds in
  ( List.map
      (fun mk_d ->
        let env2 = { env1 with tail_pos = false } in
        { mk_d with it = mk_d.it env2 })
      mk_ds
  , tailexp env1 exp)

and prog ((ds, exp), flavor) = (block { tail_pos = false; info = None } ds exp, flavor)

(* validation *)

let transform p = prog p
