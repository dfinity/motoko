open Source
open Syntax
open Effect   
   
module T = Type

type var = exp

(* Mutabilities *)
         
let varM = Var@@no_region
let constM = Const@@no_region
                      
(* Field names *)

let nameN s = (Name s)@@no_region

let nextN = nameN "next"
            
(* Identifiers *)

let idE id typ =
  {it = VarE id;
   at = no_region;
   note = {note_typ = typ;
           note_eff = T.Triv}
  }

let id_of_exp x =
  match x.it with
  | VarE x -> x
  | _ -> failwith "Impossible: id_of_exp"

(* Fresh id generation *)       

let id_stamp = ref 0

let fresh_id typ =
  let name = Printf.sprintf "$%i" (!id_stamp) in
  let exp = idE (name@@no_region) typ in
  (id_stamp := !id_stamp + 1;
   exp)

(* Patterns *)

let varP x = {x with it=VarP (id_of_exp x)}  
let tupP pats =
  {it = TupP pats; 
   note = {note_typ = T.Tup (List.map typ pats);
           note_eff = T.Triv};
   at = no_region}
  

(* Primitives *)

let primE name typ =
  {it = PrimE name;
   at = no_region;
   note = {note_typ = typ;
           note_eff = T.Triv}
  }

(* tuples *)
  

let projE e n =
  match typ e with
  | T.Tup ts ->
     {it = ProjE(e,n);
      note = {note_typ = List.nth ts n;
              note_eff = T.Triv};
      at = no_region;
     }
  | _ -> failwith "projE"
  
let decE exp = {exp with it = DecE exp}

let rec typ_decs decs =
  match decs with
     | [] -> T.unit
     | [dec] -> typ dec
     | _::decs -> typ_decs decs 
  
let blockE decs = 
  let es = List.map eff decs in
  let typ = typ_decs decs in
  let e =  List.fold_left max_eff Type.Triv es in
  { it = BlockE decs;
    at = no_region;
    note = {note_typ = typ;
            note_eff = e}
  }
                       
let textE s =
  { it = LitE (ref (TextLit s));
    at = no_region;
    note = {note_typ = T.Prim T.Text;
            note_eff = T.Triv;}
  }
    

let unitE =
  { it = TupE [];
    at = no_region;
    note = {note_typ = T.Tup [];
            note_eff = T.Triv}
  }
  
let boolE b =
  { it = LitE (ref (BoolLit b));
    at = no_region;
    note = {note_typ = T.bool;
            note_eff = T.Triv}
  }
  
let callE e1 ts e2 t =
  { it = CallE(e1,ts,e2);
    at = no_region;
    note = {note_typ = t;
            note_eff = T.Triv}
  }
  
let ifE exp1 exp2 exp3 typ =
  { it = IfE (exp1, exp2, exp3);
    at = no_region;
    note = {note_typ = typ;
            note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }
  
let dotE exp name typ =
  { it = DotE (exp,name);
    at = no_region;
    note = {note_typ = typ;
            note_eff = eff exp}
  }

let switch_optE exp1 exp2 pat exp3 typ =
  { it = SwitchE (exp1,
                  [{it = {pat = {it = LitP (ref NullLit); 
                                 at = no_region;
                                 note = {note_typ = exp1.note.note_typ;
                                         note_eff = T.Triv}};
                          exp = exp2};
                    at = no_region;
                    note = ()};
                   {it = {pat = {it = OptP pat; 
                                 at = no_region;
                                 note = {note_typ = exp1.note.note_typ;
                                         note_eff = T.Triv}};
                          exp = exp3};
                    at = no_region;
                    note = ()}]
           );
    at = no_region;
    note = {note_typ = typ;
            note_eff = max_eff (eff exp1) (max_eff (eff exp2) (eff exp3))
           }
  }
    
let tupE exps =
  let effs = List.map effect_exp exps in
  let eff = List.fold_left max_eff Type.Triv effs in
  {it = TupE exps;
   at = no_region;
   note = {note_typ = T.Tup (List.map typ exps);
           note_eff = eff}
  }
  
let declare_idE x typ exp1 =
  { it = DeclareE (x, typ, exp1);
    at = no_region;
    note = exp1.note;
  }
  
let define_idE x mut exp1 =
  { it = DefineE (x, mut, exp1);
    at = no_region;
    note = { note_typ = T.unit;
             note_eff =T.Triv}
  }
  
let newObjE typ sort ids =
  { it = NewObjE (sort, ids);
    at = no_region;
    note = { note_typ = typ;
             note_eff = T.Triv}
  }


(* Declarations *)
  
let letD x exp = { exp with it = LetD (varP x,exp) }

let varD x exp = { exp with it = VarD (x,exp) }

let expD exp =  {exp with it = ExpD exp}

(* let expressions (derived) *)              

let letE x exp1 exp2 = 
  { it = BlockE [letD x exp1;
                 {exp2 with it = ExpD exp2}];
    at = no_region;
    note = {note_typ = typ exp2;
            note_eff = max_eff (eff exp1) (eff exp2)}           
  }
             
           
(* mono-morphic function declaration *)
let funcD f x e =
  match f.it,x.it with
  | VarE _, VarE _ ->
     let note = {note_typ = T.Func(T.Call T.Local, T.Returns, [], T.as_seq (typ x), T.as_seq (typ e));
                 note_eff = T.Triv} in
     {it=FuncD(T.Local @@ no_region, (id_of_exp f),
               [],
               {it=VarP (id_of_exp x);at=no_region;note=x.note},
               PrimT "Any"@@no_region, (* bogus,  but we shouldn't use it anymore *)
               e);
            at = no_region;
            note;}
  | _ -> failwith "Impossible: funcD"


(* Continuation types *)
  
let answerT = T.unit

let contT typ = T.Func(T.Call T.Local, T.Returns, [], T.as_seq typ, [])
let cpsT typ = T.Func(T.Call T.Local, T.Returns, [], [contT typ], [])

let fresh_cont typ = fresh_id (contT typ)

                   
(* Lambdas & continuations *)

(* Lambda abstraction *)

let  (-->) x e =
  match x.it with
  | VarE _ ->
     let f = idE ("$lambda"@@no_region) (T.Func(T.Call T.Local, T.Returns, [], T.as_seq (typ x), T.as_seq (typ e))) in
     decE (funcD f x e)
  | _ -> failwith "Impossible: -->"
            
(* Lambda application (monomorphic) *)
    
let ( -*- ) exp1 exp2 =
  match exp1.note.note_typ with
  | Type.Func(_, _, [], _, ts) ->
     {it = CallE(exp1, [], exp2);
      at = no_region;
      note = {note_typ = T.seq ts;
              note_eff = max_eff (eff exp1) (eff exp2)}
     }
  | typ1 -> failwith
           (Printf.sprintf "Impossible: \n func: %s \n : %s arg: \n %s"
              (Wasm.Sexpr.to_string 80 (Arrange.exp exp1))
              (Type.string_of_typ typ1)
              (Wasm.Sexpr.to_string 80 (Arrange.exp exp2)))


(* intermediate, cps-based @async and @await primitives, 
   introduced by await(opt).ml, removed by async.ml
*)
                   
let prim_async typ =
  primE "@async" (T.Func(T.Call T.Local, T.Returns, [], [cpsT typ], [T.Async typ]))

let prim_await typ = 
  primE "@await" (T.Func(T.Call T.Local, T.Returns, [], [T.Async typ; contT typ], []))
                       
