open Syntax
open Source
open Effect
open Type   
open Syntaxops   

let breakE l exp typ =
  { it = BreakE (l, exp);
    at = no_region;
    note = {note_eff = eff exp;
            note_typ = typ}
  }

let retE exp typ =
  { it = RetE exp;
    at = no_region;
    note = {note_eff = eff exp;
            note_typ = typ}
  }
  

let assignE exp1 exp2 =
  { it = AssignE (exp1,exp2);
    at = no_region;
    note = {note_eff = Effect.max_eff (eff exp1) (eff exp2);
            note_typ = Type.unit}
  }

let labelE l typT exp =  
  { exp with it = LabelE(l,typT,exp) }

let loopE exp1 exp2Opt =
  { it = LoopE(exp1,exp2Opt);
    at = no_region;
    note = {note_eff = Effect.max_eff (eff exp1)
                         (match exp2Opt with
                          | Some exp2 -> eff exp2
                          | None -> Type.Triv);
            note_typ = Type.unit}
  }
   

let rec t_exp env inTailPos (exp:Syntax.exp) =
 { exp with it = t_exp' env inTailPos exp }

and t_exp' env inTailPos (exp:Syntax.exp) =
  let exp' = exp.it in
  match exp' with
  | PrimE _
  | LitE _ -> exp'
  | VarE id -> exp'
  | UnE (op, exp1) ->
    UnE (op, t_exp env None exp1)
  | BinE (exp1, op, exp2) ->
    BinE (t_exp env None exp1, op, t_exp env None exp2)
  | RelE (exp1, op, exp2) ->
    RelE (t_exp env None exp1, op, t_exp env None exp2)
  | TupE exps ->
    TupE (List.map (t_exp env None) exps)
  | OptE exp1 ->
    OptE (t_exp env None exp1)
  | ProjE (exp1, n) ->
    ProjE (t_exp  env None exp1, n)
  | ObjE (sort, id, fields) ->
    let fields' = t_fields env None fields in                    
    ObjE (sort, id, fields')
  | DotE (exp1, id) ->
    DotE (t_exp env None exp1, id)
  | AssignE (exp1, exp2) ->
    AssignE (t_exp env None exp1, t_exp env None exp2)
  | ArrayE exps ->
    ArrayE (List.map (t_exp env None) exps)
  | IdxE (exp1, exp2) ->
    IdxE (t_exp env None exp1, t_exp  env None exp2)
  | CallE (exp1, typs, exp2)  ->
    begin
      match exp1.it, typs, inTailPos with
      | VarE f1, [], Some (f2,x,l,tailCalled) when f1.it = f2.it ->
        tailCalled := true;
        (blockE [expD (assignE x (t_exp env None exp2));
                expD (breakE l (tupE []) (typ exp))]).it
      | _,_ ,_-> CallE(t_exp env None exp1, typs, t_exp env None exp2)
    end
  | BlockE decs ->
    BlockE (t_decs env inTailPos decs)
  | NotE exp1 ->
    NotE (t_exp env None exp1)     
  | AndE (exp1, exp2) ->
    AndE (t_exp env None exp1, t_exp env inTailPos exp2)
  | OrE (exp1, exp2) ->
    OrE (t_exp env None exp1, t_exp env inTailPos exp2)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp env None exp1, t_exp env inTailPos exp2, t_exp env inTailPos exp3)
  | SwitchE (exp1, cases) ->
    let cases' = List.map
                  (fun {it = {pat;exp}; at; note} ->
                    {it = {pat = pat ;exp = t_exp env inTailPos exp}; at; note})
                  cases
    in
    SwitchE (t_exp env None exp1, cases')
  | WhileE (exp1, exp2) ->
    WhileE (t_exp env None exp1, t_exp env None exp2)
  | LoopE (exp1, exp2_opt) ->
    LoopE (t_exp env None exp1, Lib.Option.map (t_exp env None) exp2_opt)
  | ForE (pat, exp1, exp2) ->
    ForE (pat, t_exp env None exp1, t_exp env None exp2)
  | LabelE (id, _typ, exp1) ->
    LabelE (id, _typ, t_exp env inTailPos exp1)
  | BreakE (id, exp1) ->
    BreakE (id, t_exp env None exp1)
  | RetE exp1 ->
    RetE (t_exp env inTailPos exp1)
  | AsyncE _ -> failwith "unexpected asyncE" 
  | AwaitE _ -> failwith "unexpected awaitE" 
  | AssertE exp1 ->
    AssertE (t_exp env None exp1)
  | IsE (exp1, exp2) ->
    IsE (t_exp env None exp1, t_exp env None exp2) 
  | AnnotE (exp1, typ) ->
    AnnotE (t_exp env inTailPos exp1, typ)
  | DecE dec ->
    DecE (t_dec env inTailPos dec)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, typ, t_exp env inTailPos exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp env None exp1)
  | NewObjE (sort, ids) -> exp' 

and t_dec env inTailPos dec =
  { dec with it = t_dec' env inTailPos dec }
 
and t_dec' env inTailPos dec =
  match dec.it with
  | ExpD exp -> ExpD (t_exp env inTailPos exp)
  | TypD _ -> dec.it
  | LetD (pat,exp) -> LetD (pat,t_exp env None exp)
  | VarD (id,exp) -> VarD (id,t_exp env None exp)
  | FuncD (({it=Local;_} as s), id, ([] as typbinds), pat, typT, exp) ->
    let pat,rho = Rename.pat Rename.Renaming.empty pat in
    let exp = Rename.exp rho exp in
    let temp = fresh_id (Mut (typ pat)) in
    let l = fresh_lab () in
    let tailCalled = ref false in
    let inTailPos = Some(id,temp,l,tailCalled) in
    let exp' = t_exp env inTailPos exp in
    if !tailCalled then
      let ids = match typ dec with
        | Func(_,_,_,dom,_) -> List.map fresh_id dom         
        | _ -> assert false
      in
      let args = seqP (List.map varP ids) in
      let body =
        blockE [ varD (id_of_exp temp) (seqE ids);
                 expD (loopE
                         (labelE l typT
                            (blockE [letP pat temp;
                                     expD (retE exp' unit)])) None)
               ] in
      FuncD (s, id, typbinds, args, typT, body)
    else
      FuncD (s, id, typbinds, pat, typT, exp')
  | FuncD (s, id, typbinds, pat, typT, exp) ->
      FuncD (s, id, typbinds, pat, typT, t_exp env None exp)
  | ClassD (id, lab, typbinds, sort, pat, id', fields) ->
    let fields' = t_fields env None fields in             
    ClassD (id, lab, typbinds, sort, pat, id', fields')

and t_decs env inTailPos decs =
  let tailPos dec inTailPos =
    match dec.it with
    | TypD _ -> inTailPos
    | _ -> None
  in         
  let rec go decs =
    match decs with
    | [] -> inTailPos,[]
    | dec::decs ->
      let (inTailPos',decs') = go decs in
      (tailPos dec inTailPos',
       t_dec env inTailPos' dec::decs')
  in snd (go decs)


and t_fields env inTailPos fields = 
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = t_exp env None field.it.exp }})
    fields

and t_prog env prog:prog = {prog with it = t_decs env None prog.it}
