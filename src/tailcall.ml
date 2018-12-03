open Syntax
open Source
open Effect
open Type   
open Syntaxops   

(* Optimize (self) tail calls to jumps, avoiding stack overflow 
   in a single linear pass *)

(*
This is simple tail call optimizer that replaces tail calls to the current function by jumps. 
It can  easily be extended to non-self-tail calls.

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

*)


type func_info = {func:id;
                  typ_binds: typ_bind list;
                  temp: var;
                  label: id;
                  tail_called: bool ref;
                 }

type env = {tail_pos:bool; (* is the expression in tail position *)
            info: func_info option; (* the innermost enclosing func, if any *)
           }
         
   
let bind env i info =
  match info with
  | Some _ ->
     {env with info = info; }
  | None ->
    match env.info with
    | Some {func;_} when i.it = func.it ->
      {env with info = None} (* remove shadowed func info *)
    | _ -> env         (* preserve existing, non-shadowed info *)                  
         

let are_generic_insts tbs insts =
  List.for_all2 (fun tb inst ->
      match !(tb.note),!(inst.note) with
      | Some c1, Con(c2,[]) -> c1 = c2 (* conservative, but safe *)
      | Some c1, _ -> false                                          
      | None,_ -> assert false) tbs insts
                            
let rec tailexp env e =
    {e with it = exp' env e}

and exp env e  =
    {e with it = exp' {env with tail_pos = false}  e}

and exp' env e  = match e.it with
  | VarE _              
  | LitE _              
  | PrimE _             -> e.it
  | UnE (uo, e)         -> UnE (uo, exp env e)
  | BinE (e1, bo, e2)   -> BinE (exp env e1, bo, exp env e2)
  | RelE (e1, ro, e2)   -> RelE (exp env e1, ro, exp env e2)
  | TupE es             -> TupE (List.map (exp env) es)
  | ProjE (e, i)        -> ProjE (exp env e, i)
  | ObjE (s, i, efs)    -> ObjE (s, i, exp_fields env efs)
  | DotE (e, i)         -> DotE (exp env e, i)
  | AssignE (e1, e2)    -> AssignE (exp env e1, exp env e2)
  | ArrayE es           -> ArrayE (exps env es)
  | IdxE (e1, e2)       -> IdxE (exp env e1, exp env e2)
  | CallE (e1, insts, e2)  -> 
    begin
      match e1.it, env with
      | VarE f1, {tail_pos = true;
                  info = Some {func; typ_binds; temp; label; tail_called}}
          when f1.it = func.it && are_generic_insts typ_binds insts  ->
        tail_called := true;
        (blockE [expD (assignE temp (exp env e2));
                 expD (breakE label (tupE []) (typ e))]).it
      | _,_-> CallE(exp env e1, insts, exp env e2)
    end
  | BlockE ds           -> BlockE (decs env ds)
  | NotE e              -> NotE (exp env e)
  | AndE (e1, e2)       -> AndE (exp env e1, tailexp env e2)
  | OrE (e1, e2)        -> OrE (exp env e1, tailexp env e2)
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
  | RetE e              -> RetE (tailexp {env with tail_pos = true} e)
   (* NB:^ e is always in tailposition, regardless of fst env *)
  | AsyncE e            -> AsyncE (exp {tail_pos = true; info = None} e)
  | AwaitE e            -> AwaitE (exp env e)
  | AssertE e           -> AssertE (exp env e)
  | IsE (e, t)          -> IsE (exp env e, t)
  | AnnotE (e, t)       -> AnnotE (exp env e, t)
  | DecE d              -> let mk_d, env1 = dec env d in
                           DecE ({mk_d with it = mk_d.it env1})
  | OptE e              -> OptE (exp env e)
  | DeclareE (i, t, e)  -> let env1 = bind env i None in
                           DeclareE (i, t, tailexp env1 e)
  | DefineE (i, m, e)   -> DefineE (i, m, exp env e)
  | NewObjE (s,is)      -> NewObjE (s, is)
                                   
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
  | AnnotP (p, t) -> pat env p 
  | LitP l        -> env
  | SignP (uo, l) -> env
  | OptP p        -> pat env p 
  | AltP (p1, p2) -> assert(Freevars.S.is_empty (snd (Freevars.pat p1)));
                     assert(Freevars.S.is_empty (snd (Freevars.pat p2)));
                     env

and pats env ps  =
  match ps with
  | [] -> env
  | p::ps ->
    let env1 = pat env p in
    pats env1 ps 

and case env (c : case) =
  {c with it = case' env c.it}
and case' env {pat=p;exp=e} =
  let env1 = pat env p in
  let e' = tailexp env1 e in
  {pat=p; exp=e'}
  

and cases env cs = List.map (case env) cs

and exp_field env (ef : exp_field) =
  let (mk_ef,env) = exp_field' env ef.it in
  ({ef with it = mk_ef}, env)

and exp_field' env {name = n; id = i; exp = e; mut; priv} =
  let env = bind env i None in
  ((fun env1-> {name = n; id = i; exp = exp env1 e; mut; priv}),
   env)            

and exp_fields env efs  = 
  let rec exp_fields_aux env efs =
    match efs with
    | [] -> ([],env)
    | ef::efs ->
      let (mk_ef,env) = exp_field env ef in
      let (mk_efs,env) = exp_fields_aux env efs in
      (mk_ef::mk_efs,env) in
  let mk_efs,env = exp_fields_aux env efs in                           
  List.map (fun mk_ef -> {mk_ef with it = mk_ef.it env}) mk_efs

and dec env d =
  let (mk_d,env1) = dec' env d in
  ({d with it = mk_d}, env1)
                 
and dec' env d =
  match d.it with
  | ExpD e ->
    (fun env1 -> ExpD (tailexp env1 e)),
    env
  | LetD (p, e) ->
    let env = pat env p in
    (fun env1 -> LetD(p,exp env1 e)),
    env              
  | VarD (i, e) ->
    let env = bind env i None in
    (fun env1 -> VarD(i,exp env1 e)),
    env
  | FuncD (({it=Local;_} as s), id, tbs, p, typT, exp0) ->
    let env = bind env id None in
    (fun env1 ->
      let temp = fresh_id (Mut (typ p)) in
      let l = fresh_lab () in
      let tail_called = ref false in
      let env2 = {tail_pos = true;
                   info = Some {func = id;
                                typ_binds = tbs;
                                temp = temp;
                                label = l;
                                tail_called = tail_called}}
      in
      let env3 = pat env2 p  in (* shadow id if necessary *)
      let exp0' = tailexp env3 exp0 in
      if !tail_called then
        let ids = match typ d with
          | Func(_,_,_,dom,_) -> List.map fresh_id dom         
          | _ -> assert false
        in
        let args = seqP (List.map varP ids) in
        let body =
          blockE [ varD (id_of_exp temp) (seqE ids);
                   expD (loopE
                           (labelE l typT
                              (blockE [letP p temp;
                                       expD (retE exp0' unit)])) None)
          ] in
        FuncD (s, id, tbs, args, typT, body)
      else
        FuncD (s, id, tbs, p, typT, exp0'))
    ,
      env
  | FuncD ({it=Sharable;_} as s, i, tbs, p, t, e) ->
    (* don't optimize self-tail calls for shared functions otherwise
       we won't go through the scheduler  *) 
    let env = bind env i None in
    (fun env1 ->
      let env2 = pat {tail_pos = true; info = None} p in
      let e' = tailexp env2 e in
      FuncD(s, i, tbs, p, t, e')),
    env
  | TypD (i, tbs, t) -> 
    (fun env -> d.it),
    env
  | ClassD (i, l, tbs, s, p, i2, efs) ->
    let env = bind env i None in
    (fun env1 ->
      let env2 = pat {tail_pos = false; info = None}  p in
      let env3 = bind env2 i2 None in
      let efs' = exp_fields env3 efs in
      ClassD(i, l, tbs, s, p, i2, efs')),
    env
       
and decs env ds =
  let rec tail_posns ds =
    match ds with
    | [] -> (true,[])
    | {it=TypD _;_}::ds ->
      let (b,bs) = tail_posns ds in
      (b,b::bs)
    | d::ds ->
      let (b,bs) = tail_posns ds in
      (false,b::bs)
  in
  let _,tail_posns = tail_posns ds in
  let rec decs_aux env ds =
    match ds with
    | [] -> ([],env)
    | d::ds ->
      let (mk_d,env1) = dec env d in
      let (mk_ds,env2) = decs_aux env1 ds in
      (mk_d::mk_ds,env2)
  in
  let mk_ds,env1 = decs_aux env ds in                           
  List.map2 (fun mk_d in_tail_pos ->
      let env2 = if in_tail_pos
                  then env1
                  else {env1 with tail_pos = false} in
      {mk_d with it = mk_d.it env2}) mk_ds tail_posns

 
and prog p:prog = {p with it = decs {tail_pos = false; info = None;} p.it}




