open Syntax
open Source
module T = Type
module R = Rename             
open T
open Awaitopt    (* TBD *)
(* lower the async type itself *)



(*
  foo(t1,t2):async t = e -> 
  foo(t1,t2,k) -> let foo(t1,t2)=e in @await (foo(t1,t2),k)); -
 
  foo(t1,t2):async t = @async (\k.e) 
  ->
  foo(t1,t2,k') =  let foo(t1,t2) = @async (\k.e) in @await (foo(t1,t2),k')); -
 *)

let localS =
  {it=T.Call T.Local;
   at=no_region;
   note=()}
let sharableS =
  {it=T.Call T.Sharable;
   at=no_region;
   note=()}

let tupT ts = {it = TupT ts;
               at = no_region;
               note = ()}
             
let unitT = tupT []

let funcT(s,bds,t1,t2) =
  {it = FuncT (s, bds, t1, t2);
   at = no_region;
   note = ()}

let contTT t = funcT(localS,[],t,unitT)
             
let tupP pats =
  {it = TupP pats; 
   note = {note_typ = T.Tup (List.map typ pats);
           note_eff = T.Triv};
   at = no_region}     

let t_async t =  T.Func (T.Call T.Local, [], T.Func(T.Call T.Local,[],t,T.unit), T.unit)
  
let rec t_typ (t:T.typ) =
  match t with
  | T.Prim _
  | Var _ -> t
  | Con (c, ts) ->
    Con (c, List.map t_typ ts)
  | Array t -> Array (t_typ t)
  | Tup ts -> Tup (List.map t_typ ts)
  | Func (s, tbs, t1, t2) ->
     begin
       match s with
       |  T.Call T.Sharable ->
         begin
           match t2 with
           | Tup [] ->
              Func(s, List.map t_bind tbs, t_typ t1, t_typ t2)
           | Async t2 ->
              Func (T.Call T.Local, List.map t_bind tbs,
                    Tup [t_typ t1; contT (t_typ t2)], T.unit)
           | _ -> failwith "t_typT'"
         end
       | _ ->
          Func (s, List.map t_bind tbs, t_typ t1, t_typ t2)
     end
  | Opt t -> Opt (t_typ t)
  | Async t -> t_async (t_typ t)
  | Like t -> Like (t_typ t)
  | Obj (s, fs) -> Obj (s, List.map t_field  fs)
  | Mut t -> Mut (t_typ t)
  | Class -> Class
  | Shared -> Shared
  | Any -> Any
  | Non -> Non
  | Pre -> Pre

and t_bind {var; bound} =
  {var; bound = t_typ bound}

and t_field {name; typ} =
  {name; typ = t_typ typ}
let rec t_exp exp =
  { exp with it = t_exp' exp.it }
and t_exp' exp' =
  match exp' with
  | PrimE _
  | LitE _ -> exp'
  | VarE _ -> exp'
  (*     
     begin
     match typ exp1 with
     | T.Func (T.Call T.Sharable,_,_,_) ->
     | _ -> exp'
     end
 *)
  | UnE (op, exp1) ->
    UnE (op, t_exp exp1)
  | BinE (exp1, op, exp2) ->
    BinE (t_exp exp1, op, t_exp exp2)
  | RelE (exp1, op, exp2) ->
    RelE (t_exp exp1, op, t_exp exp2)
  | TupE exps ->
    TupE (List.map t_exp exps)
  | OptE exp1 ->
    OptE (t_exp exp1)
  | ProjE (exp1, n) ->
    ProjE (t_exp exp1, n)
  | ObjE (sort, id, fields) ->
    let fields' = t_fields fields in                    
    ObjE (sort, id, fields')
  | DotE (exp1, id) ->
    DotE (t_exp exp1, id)
  | AssignE (exp1, exp2) ->
    AssignE (t_exp exp1, t_exp exp2)
  | ArrayE exps ->
    ArrayE (List.map t_exp exps)
  | IdxE (exp1, exp2) ->
     IdxE (t_exp exp1, t_exp exp2)
  | CallE (exp1, typs, exp2) ->
     begin
     match typ exp1 with
     | T.Func (T.Call T.Sharable,_,_,_) ->
        exp'
     | _ -> exp'
     end
  | BlockE decs ->
     BlockE (t_decs decs)
  | NotE exp1 ->
    NotE (t_exp exp1)     
  | AndE (exp1, exp2) ->
    AndE (t_exp exp1, t_exp exp2)
  | OrE (exp1, exp2) ->
    OrE (t_exp exp1, t_exp exp2)
  | IfE (exp1, exp2, exp3) ->
    IfE (t_exp exp1, t_exp exp2, t_exp exp3)
  | SwitchE (exp1, cases) ->
    let cases' = List.map
                  (fun {it = {pat;exp}; at; note} ->
                    {it = {pat = t_pat pat ;exp = t_exp exp}; at; note})
                  cases
    in
    SwitchE (t_exp exp1, cases')
  | WhileE (exp1, exp2) ->
    WhileE (t_exp exp1, t_exp exp2)
  | LoopE (exp1, exp2_opt) ->
    LoopE (t_exp exp1, Lib.Option.map t_exp exp2_opt)
  | ForE (pat, exp1, exp2) ->
    ForE (t_pat pat, t_exp exp1, t_exp exp2)
  | LabelE (id, _typ, exp1) ->
    LabelE (id, t_typT _typ, t_exp exp1)
  | BreakE (id, exp1) ->
     BreakE (id, t_exp exp1)
  | RetE exp1 ->
     RetE (t_exp exp1)
  | AsyncE _ -> failwith "unexpected asyncE" 
  | AwaitE _ -> failwith "unexpected awaitE" 
  | AssertE exp1 ->
    AssertE (t_exp exp1)
  | IsE (exp1, exp2) ->
    IsE (t_exp exp1, t_exp exp2) 
  | AnnotE (exp1, typ) ->
    AnnotE (t_exp exp1,typ)
  | DecE dec ->
    DecE (t_dec dec)
  | DeclareE (id, typ, exp1) ->
    DeclareE (id, t_typ typ, t_exp exp1)
  | DefineE (id, mut ,exp1) ->
    DefineE (id, mut, t_exp exp1)
  | NewObjE (sort, ids) -> exp' 

and t_block decs : dec list= 
  List.map t_dec decs

and t_dec dec =
  {dec with it = t_dec' dec.it}
and t_dec' dec' =
  match dec' with
  | ExpD exp -> ExpD (t_exp exp)
  | TypD _ -> dec'
  | LetD (pat,exp) -> LetD (t_pat pat,t_exp exp)
  | VarD (id,exp) -> VarD (id,t_exp exp)
  | FuncD (s, id, typbinds, pat, typT, exp) ->
    begin
      match s.it with
      | T.Local ->
         FuncD (s, id, t_typbinds typbinds, t_pat pat, t_typT typT, t_exp exp)
      | T.Sharable ->
         begin
           match typ exp with
           | T.Tup [] ->
              FuncD (s, id, t_typbinds typbinds, t_pat pat, t_typT typT, t_exp exp)
           | T.Async res_typ ->
              let res_typ = t_typ res_typ in
              let pat = t_pat pat in
              let typT = t_typT typT in 
              let cont_typ = contT res_typ in
              let cont_typT = funcT(sharableS,[],typT,unitT) in
              let typT' = tupT [typT; cont_typT] in
              let k = fresh_id cont_typ in
              let pat' = tupP [pat;varP k] in
              let typbinds' = t_typbinds typbinds in                   
              let x = fresh_id res_typ in
              let exp' =
                match exp.it with
                | CallE(async,[_],cps) ->
                   begin
                     match async.it with
                     | VarE({it="@await";_}) ->
                        (t_exp cps)
                        -*- (x --> (k -*- x))
                  | _ -> failwith "async.ml t_dec': funcD1"
                   end
                | _ -> failwith "async.ml t_dec': funcD2"
              in
              FuncD ({s with it = T.Local}, id, typbinds', pat', typT', exp')
           | _ -> failwith "async.ml t_dec': funcD2"
         end
    end
  | ClassD (id, lab, typbinds, sort, pat, fields) ->
     let fields' = t_fields fields in             
     ClassD (id, lab, t_typbinds typbinds, sort, t_pat pat, fields')

and t_decs decs = List.map t_dec decs           

and t_fields fields = 
  List.map (fun (field:exp_field) ->
      { field with it = { field.it with exp = t_exp field.it.exp }})
    fields

and t_pat pat =
  { pat with it = t_pat' pat.it;
             note = {note_typ = t_typ pat.note.note_typ;
                     note_eff = pat.note.note_eff}}

and t_pat' pat = 
  match pat with
  | WildP 
  | LitP _
  | SignP _
  | VarP _ ->
     pat
  | TupP pats ->
    TupP (List.map t_pat pats)
  | OptP pat1 ->
    OptP (t_pat pat1)
  | AltP (pat1, pat2) ->
    AltP (t_pat pat1, t_pat pat2) 
  | AnnotP (pat1, _typ) ->
    AnnotP (t_pat pat1, t_typT _typ)

(* translate syntactic types *)
 
and t_asyncT t =
  FuncT (localS,
         [],
         funcT(localS,[],t,unitT),
         unitT)
  
and t_typT t =
  { t with it = t_typT' t.it }
and t_typT' t =
  match t with
  | VarT (s, ts) ->
     VarT (s,List.map t_typT ts)
  | PrimT p ->
     PrimT p
  | ObjT (s, ts) ->
     ObjT (s, List.map t_typ_fieldT ts)
  | ArrayT (m, t) ->
     ArrayT(m, t_typT t)
  | OptT t ->
     OptT (t_typT t)
  | TupT ts ->
     TupT (List.map t_typT ts)
  | FuncT (s, tbs, t1, t2) ->
     begin
       match s.it with
       |  T.Call T.Sharable ->
         begin
           match t2.it with
           | TupT [] ->
              FuncT (s, t_typbinds tbs, t_typT t1, t_typT t2)
           | AsyncT t2 ->
              FuncT (localS, t_typbinds tbs,
                     tupT [t_typT t1; contTT (t_typT t2)], unitT)
           | _ -> failwith "t_typT'"
         end
       | _ ->
          FuncT (s, t_typbinds tbs, t_typT t1, t_typT t2)
     end
  | AsyncT t ->
     t_asyncT (t_typT t)
  | LikeT  t ->
     LikeT (t_typT t)
and t_typ_fieldT fld =
   { fld with it = {fld.it with typ = t_typT fld.it.typ}}
and t_typ_bindT bnd =
   { bnd with it = {bnd.it with Syntax.bound = t_typT bnd.it.Syntax.bound}}

and t_typbinds typbinds = List.map t_typ_bindT typbinds 
and t_prog prog:prog = {prog with it = t_decs prog.it}
