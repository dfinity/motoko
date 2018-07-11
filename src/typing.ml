open Syntax
open Source
open Types

module I32 = Wasm.I32
module I64 = Wasm.I64


exception KindError of Source.region * string
exception TypeError of Source.region * string

let kindError region fmt = 
     Printf.ksprintf (fun s -> raise (KindError(region,s))) fmt

let typeError region fmt = 
     Printf.ksprintf (fun s -> raise (TypeError(region,s))) fmt


(* TBR *)
let nat_width = 31
let int_width = 31

(* todo: compute refutability of pats and enforce accordingly (refutable in all cases but last, irrefutable elsewhere) *)
(* todo: type ObjE expressions (anonymous objects)*)

(* unique constructors, stamped *)

module Con =
struct
    type con = {name:string;stamp:int}
    type t = con
    let compare (c1:con) (c2:con) = compare c1.stamp c2.stamp
    let stamp = ref 0
    let fresh var =
      stamp := (!stamp) + 1;
      {name = var; stamp = !stamp}
    let to_string con = Printf.sprintf "%s/%i" con.name con.stamp
end


type con = Con.t

type typ =
  | VarT of con * typ list                     (* constructor *)
  | PrimT of Types.prim                        (* primitive *)
  | ObjT of actor' * typ_field list             (* object *)
  | ArrayT of mut' * typ                        (* array *)
  | OptT of typ                                (* option *)
  | TupT of typ list                           (* tuple *)
  | FuncT of typ_bind list * typ * typ         (* function *)
  | AsyncT of typ                              (* future *)
  | LikeT of typ                               (* expansion *)
  | AnyT                                       (* top *)
(*
  | UnionT of type * typ                       (* union *)
  | AtomT of string                            (* atom *)
*)
and typ_bind = {var:con; bound: typ }
and typ_field = {var:string; typ: typ; mut: mut'}
and kind =
     | DefK of typ_bind list * typ
     | ObjK of typ_bind list * actor' * typ_field list
     | ParK of typ_bind list * typ


module Env = Map.Make(String)

let lookup map k = try Some (Env.find k map)  with _ -> None (* TODO: use find_opt in 4.05 *)

module ConEnv = Map.Make(Con)

let lookup_con map k = try Some (ConEnv.find k map)  with _ -> None (* TODO: use find_opt in 4.05 *)

type context = {values: (typ*mut') Env.t; constructors: con Env.t; kinds: kind ConEnv.t; label: string option;  breaks: typ Env.t; continues: unit Env.t ; returns: typ option; awaitable: bool}

let union env1 env2 = Env.union (fun k v1 v2 -> Some v2) env1 env2
let union_conenv env1 env2 = ConEnv.union (fun k v1 v2 -> Some v2) env1 env2

let union_values c ve = {c with values = union c.values ve}
let add_value c v tm = {c with values = Env.add v tm c.values}
let union_constructors c ce = {c with constructors = union c.constructors ce}

let union_kinds c ke = {c with kinds = union_conenv c.kinds ke}
let add_constructor c v con kind =  {{c with constructors = Env.add v con c.constructors}
                                        with kinds = ConEnv.add con kind c.kinds}

(* raises TypeError on duplicate entries *)
let disjoint_union at fmt env1 env2 = Env.union (fun k v1 v2 -> typeError at fmt k) env1 env2
let disjoint_add_field at c v f = disjoint_union at "duplicate field %s" c (Env.singleton v f)

(* TBD
let disjoint_union_values at c ve =   {c with values = disjoint_union at "duplicate value %s"  c.values ve }
let disjoint_add_value at c v tm = disjoint_union_values at c (Env.singleton v tm)
let disjoint_union_constructors at c ce = {c with constructors = disjoint_union at "duplicate constructor %s" c.constructors ce}
let disjoint_add_constructor at c d = disjoint_union_constructors at c (Env.singleton v d)
*)
let prelude = {values = Env.empty;
    	       constructors = Env.empty;
	       kinds = ConEnv.empty;
               label = None;
	       breaks = Env.empty;
	       continues = Env.empty;
	       returns = None;
	       awaitable = false}

let addBreak context labelOpt t =
    match labelOpt with
    | None -> context
    | Some label -> { context with breaks = Env.add label t context.breaks }

let addBreakAndContinue context labelOpt t =
    match labelOpt with
    | None -> context
    | Some label -> {{ context with breaks = Env.add label t context.breaks } with continues = Env.add label () context.continues }

let sprintf = Printf.sprintf

let check_bounds region tys bounds = 
     if List.length bounds = List.length tys
     then ()
     else kindError region "constructor expecting %i arguments used with %i arguments" (List.length bounds) (List.length tys)




(* Poor man's pretty printing - replace with Format client *)
let mut_to_string m = (match m with VarMut -> " var " |  ConstMut -> "")

let rec atomic_typ_to_string t =
    match t with
    | AnyT -> "Any"
    | PrimT p ->
      (match p with
      | NullT -> "Null"
      | IntT -> "Int"
      | BoolT -> "Bool"
      | FloatT -> "Float"
      | NatT -> "Nat"
      | CharT -> "Char"
      | WordT w ->
        (match w with
        | Width8 -> "Word8"
        | Width16 -> "Word16"
        | Width32 -> "Word32"
        | Width64 -> "Word64")
      | TextT -> "Text")
    | VarT (c,[]) ->
       Con.to_string c
    | VarT (c,ts) ->
       sprintf "%s<%s>" (Con.to_string c) (String.concat "," (List.map typ_to_string ts))
    | TupT ts ->
      sprintf "(%s)"  (String.concat "," (List.map typ_to_string ts))
    | ObjT(Object,fs) ->
      sprintf "{%s}" (String.concat ";" (List.map (fun {var;mut;typ} ->
                        sprintf "%s:%s %s" var (mut_to_string mut) (typ_to_string typ))
                        fs))
    | _ ->
      sprintf "(%s)" (typ_to_string t)

and typ_to_string t =
    match t with
    | ArrayT (m,t) ->
      sprintf "%s%s[]" (match m with VarMut -> " var " |  ConstMut -> "") (atomic_typ_to_string t)  
    | FuncT([],dom,rng) ->
      sprintf "%s->%s" (atomic_typ_to_string dom) (typ_to_string rng)
    | FuncT(ts,dom,rng) ->
      sprintf "<%s>%s->%s"  (String.concat "," (List.map (fun {var;bound} -> Con.to_string var) ts)) (atomic_typ_to_string dom) (typ_to_string rng)
    | OptT t ->
      sprintf "%s?"  (atomic_typ_to_string t)
    | AsyncT t -> 
      sprintf "async %s" (atomic_typ_to_string t)
    | LikeT t -> 
      sprintf "like %s" (atomic_typ_to_string t)
    | ObjT(Actor,fs) ->
      sprintf "actor%s" (atomic_typ_to_string (ObjT(Object,fs)))
    | _ -> atomic_typ_to_string t

let kind_to_string k =
    match k with
    | DefK(ts,t) ->
      sprintf "= <%s>%s"  (String.concat "," (List.map (fun {var;bound} -> Con.to_string var) ts)) (typ_to_string t)
    | ObjK(ts,actor,ftys) -> 
      sprintf ":= <%s>%s"  (String.concat "," (List.map (fun {var;bound} -> Con.to_string var) ts))
       (typ_to_string (ObjT(actor,ftys)))
    | ParK(ts,t) -> 
      sprintf ":: <%s>%s"  (String.concat "," (List.map (fun {var;bound} -> Con.to_string var) ts)) (typ_to_string t) 
    

let unitT = TupT[]
let boolT = PrimT(BoolT)
let intT = PrimT(IntT)

(* checking literal values are in bounds *)
let check_I32_u p bits =
    let module I = I32 in
    let max = I.shl (I.of_int_s 1) (I.of_int_s bits) in
    fun at s ->
    try  let i = I.of_string s in
    	 if  (I.gt_u max I.zero) && not (I.lt_u i max)
	 then typeError at "literal overflow for type %s" (typ_to_string (PrimT p))
	 else I.to_bits i
    with _ -> typeError at "bad literal for type %s" (typ_to_string (PrimT p))

let check_I64_u p bits =
    let module I = I64 in
    let max = I.shl (I.of_int_s 1) (I.of_int_s bits) in
    fun at s ->
    try  let i = I.of_string s in
    	 if  (I.gt_u max I.zero) && not (I.lt_u i max)
	 then typeError at "literal overflow for type %s" (typ_to_string (PrimT p))
	 else I.to_bits i
    with _ -> typeError at "bad literal for type %s" (typ_to_string (PrimT p))

let check_nat    = check_I32_u NatT nat_width
let check_word8  = check_I32_u (WordT Width8) 8
let check_word16 = check_I32_u (WordT Width16) 16
let check_word32 = check_I32_u (WordT Width32) 32
let check_word64 = check_I64_u (WordT Width64) 64

let check_I32_s p bits =
    let module I = I32 in
    let max = I.sub (I.shl (I.of_int_u 1) (I.of_int_u (bits-1)))
                    (I.of_int_s 1) in
    let min = I.sub (I.sub I.zero max) (I.of_int_s 1) in
    fun at s ->
    try  let i = I.of_string s in
    	 if not (I.le_s min i && I.le_s i max)
	 then typeError at "literal under/overflow for type %s" (typ_to_string (PrimT p))
	 else I.to_bits i
    with _ -> typeError at "bad literal for type %s" (typ_to_string (PrimT p))

let check_int = check_I32_s IntT int_width

(* begin sanity test *)

let pow2 n = 1 lsl n

(* text check_nat *)

let true = (check_nat no_region (string_of_int ((pow2 31) - 1))) = I32.of_int_u ((pow2 31) - 1)
let true = (check_nat no_region (string_of_int 0) = I32.of_int_u 0)
let true = try check_nat no_region (string_of_int (pow2 31));
    	       false
           with _ -> true
let true = try check_nat no_region (string_of_int ((pow2 31)+1));
    	       false
           with _ -> true	   

(* test check_word16 *)
let true = (check_word16 no_region (string_of_int ((pow2 16) - 1))) = I32.of_int_u ((pow2 16) - 1)
let true = (check_word16 no_region (string_of_int 0) = I32.of_int_u 0)
let true = try check_word16 no_region (string_of_int (pow2 16));
    	       false
           with _ -> true
let true = try check_word16 no_region (string_of_int ((pow2 16)+1));
    	       false
           with _ -> true	   
	   
(* test check_int *)
let true = (check_int no_region (string_of_int (pow2 (int_width-1) - 1))) = I32.of_int_s (pow2 (int_width-1) - 1)
let true = (check_int no_region (string_of_int (-(pow2 (int_width-1))))) = I32.of_int_s (-(pow2 (int_width-1)))
let true = try check_int no_region (string_of_int (pow2 (int_width-1) - 1 + 1));
    	       false
           with _ -> true
let true = try check_int no_region (string_of_int (-(pow2 (int_width-1)) - 1));
	       false
	   with _ -> true

(* end sanity test *)

(* first-order substitutions *)
type subst = typ ConEnv.t

let rec rename_binds sigma (binds:typ_bind list) =
    match binds with
    | [] -> sigma,[]
    | {var=con;bound=typ}::binds ->
      let con' = Con.fresh con.name in
      let sigma' = ConEnv.add con (VarT(con',[])) sigma in
      let (rho,binds') = rename_binds sigma' binds in
      rho,{var=con';bound=subst rho typ}::binds'
and subst sigma t =
    match t with
    | PrimT p -> t
    | VarT (c,ts) ->
      begin
       match lookup_con sigma c with
       | Some t -> assert (List.length ts = 0);
                   t
       | None -> VarT(c,List.map (subst sigma) ts)
      end
    | ArrayT (m,t) ->
      ArrayT (m, subst sigma t)
    | TupT ts ->
      TupT (List.map (subst sigma) ts)
    | FuncT(ts,dom,rng) ->
      let (rho,ts') = rename_binds sigma ts in
      FuncT(ts',subst rho dom, subst rho rng)
    | OptT t ->
      OptT (subst sigma t)
    | AsyncT t -> 
      AsyncT (subst sigma t)
    | LikeT t -> 
      LikeT (subst sigma t)
    | ObjT(a,fs) ->
      ObjT(a,subst_fields sigma fs)
and subst_fields sigma fs = 
    List.map (fun {var;mut;typ} -> {var;mut;typ = subst sigma typ}) fs

let substitute =
    let rec substitute_aux sigma us ts =
      match us,ts with
      | [],[] -> sigma
      | u::us, {bound;var}::ts ->
        substitute_aux (ConEnv.add var u sigma) us ts
      | _ -> raise (Invalid_argument "substitute")
    in
    substitute_aux ConEnv.empty


let rec eq context eqs t1 t2 =
    match t1,t2 with
    | (VarT(con1,ts1),VarT(con2,ts2)) ->
       if List.mem (con1,con2) eqs
       then eq_all context eqs ts1 ts2
       else
       begin
       match lookup_con context.kinds con1, lookup_con context.kinds con2 with
       | Some (ObjK _), Some (ObjK _) -> con1 = con2 && eq_all context eqs ts1 ts2
       | Some (ParK _), Some (ParK _) -> con1 = con2 && eq_all context eqs ts1 ts2
       | Some (DefK(ts,t)), _ -> (* TBR this may fail to terminate *)
         (con1 = con2 && eq_all context eqs ts1 ts2) ||
       	 eq context eqs (subst (substitute ts1 ts) t) t2
       | _, Some (DefK(us,u)) -> (* TBR this may fail to terminate *)
         (con1 = con2 && eq_all context eqs ts1 ts2) ||
       	 eq context eqs t1 (subst (substitute ts2 us) u)
       | _,_ -> false
       end
    | (VarT(con1,ts1),t2) ->
       begin
       match lookup_con context.kinds con1 with
       | Some (ObjK _) -> false
       | Some (ParK _) -> false
       | Some (DefK(ts,t)) -> (* TBR this may fail to terminate *)
       	 eq context eqs (subst (substitute ts1 ts) t) t2
       | None -> false
       end
    | (t1,VarT(con2,ts2)) ->
       begin
       match lookup_con context.kinds con2 with
       | Some (ObjK _) -> false (* only equal if equal constructors *)
       | Some (ParK _) -> false (* ditto *)
       | Some (DefK(us,u)) -> (* TBR this may fail to terminate *)
       	 eq context eqs t1 (subst (substitute ts2 us) u)
       | None -> false
       end
    | PrimT p1 ,PrimT p2 ->
       p1 = p2
    | ObjT(a1,tfs1),ObjT(a2,tfs2) ->
      a1 = a2 &&
      (* assuming tf1 and tf2 are sorted by var *)
      (try List.for_all2 (fun (tf1:typ_field) (tf2:typ_field) ->
      		          tf1.var = tf2.var &&
		          tf1.mut = tf2.mut &&
		          eq context eqs tf1.typ tf2.typ) tfs1 tfs2
       with _ -> false)
    | ArrayT(m1,t1),ArrayT(m2,t2) ->
      m1 = m2 && eq context eqs t1 t2
    | OptT(t1),OptT(t2) ->
      eq context eqs t1 t2
    | TupT(ts1),TupT(ts2) ->
      eq_all context eqs ts1 ts2
    | FuncT(ts,t1,t2),FuncT(us,u1,u2) ->
      (match eq_typ_binds context eqs [] [] ts us with
      | Some eqs' -> eq context eqs' t1 u1 &&
                     eq context eqs' t2 u2 
      | None -> false)
    | AsyncT(t1),AsyncT(t2) ->
      eq context eqs t1 t2
    | LikeT(t1),LikeT(t2) ->
      eq context eqs t1 t2
    | AnyT,AnyT -> true
    | _,_ -> false

and eq_typ_binds context eqs bds1 bds2 tbs1 tbs2 =
    match tbs1,tbs2 with
    | [],[] ->
      if eq_all context eqs bds1 bds2
      then Some eqs
      else None
    | (tb1::tbs1,tb2::tbs2) ->
      eq_typ_binds context ((tb1.var,tb2.var)::eqs) (tb1.bound::bds1) (tb2.bound::bds2) tbs1 tbs2
    | _,_ -> None
      
and eq_all context eqs ts1 ts2 =
    match ts1,ts2 with
    | [],[] -> true
    | (t1::ts1,t2::ts2) ->
      eq context eqs t1 t2
      &&
      eq_all context eqs ts1 ts2
    | _,_ -> false
    

let eq_typ (context:context) ty1 ty2 : bool =
    eq context [] ty1 ty2 

let rec norm_typ context t =
    match t with
    | VarT(con,ts) ->
      (match lookup_con context.kinds con with
      | Some kind ->
      	(match kind with
	| DefK(us,u) -> norm_typ context (subst (substitute ts us) u)
	| ObjK(_,actor,ftys) -> t
	| ParK(_,bound) -> t) 
      | None -> t)
    | t -> t

let rec obj_typ context t =
    match norm_typ context t with
    | VarT(con,ts) ->
      (match lookup_con context.kinds con with
      | Some kind ->
      	(match kind with
	| DefK(us,u) -> (assert(false);failwith "obj_typ")
	| ObjK([],actor,ftys) -> ObjT(actor,ftys)
	| ParK([],bound) -> t)
      | None -> t)
    | ObjT(_,_) -> t
    | LikeT t -> norm_typ context t (*TBR*)
    | _ -> t  

(* types one can switch on - all primitives except floats *)

(* TBR - switch on option type? *)
let switchable_typ context t =
    match norm_typ context t with
    | PrimT p ->
      (match p with
       | FloatT -> false
       | _ -> true)
    | _ -> false

(* types one can iterate over using `for`  *)
let iterable_typ context t =
    match norm_typ context t with
    | ArrayT(_,_) -> true
    | _ -> false

(* element type of iterable_type  *)
let element_typ context t =
    match norm_typ context t with
    | ArrayT(mut,t) -> t
    | _ -> assert(false)

let numeric_typ context t =
    match norm_typ context t with
    | PrimT p ->
      (match p with
       | NatT
       | IntT
       | WordT _ 
       | FloatT -> true
       | _ -> false)
    | _ -> false

let logical_typ context t =
    match norm_typ context t  with
    | PrimT (WordT _) -> true
    | _ -> false

let equatable_typ context t =
    match norm_typ context t with
    | PrimT p ->
      (match p with
       | BoolT
       | NatT
       | IntT
       | WordT _ 
       | TextT  
       | CharT -> true
       | FloatT -> true (* TBR do we really want = and != on floats ?*)
       | _ -> false)
    | _ -> false

let comparable_typ context t =
    match norm_typ context t with
    | PrimT p ->
      (match p with
       | NatT
       | IntT
       | WordT _
       | FloatT
       | TextT 
       | CharT -> true
       | _ -> false)
    | _ -> false



let rec check_typ_binds context ts =
    let bind_context = context in (* TBR: allow parameters in bounds? - not for now*)
    let ts = List.map (fun (bind:Syntax.typ_bind) ->
    	               let v = bind.it.Syntax.var.it in
    	               let con = Con.fresh v in
                       {var=con;bound=check_typ bind_context bind.it.Syntax.bound}
                      ) ts in
    let ce  =
          List.fold_left (fun c bind -> Env.add bind.var.name bind.var c) context.constructors ts in
    let ke  =
          List.fold_left (fun c bind -> ConEnv.add bind.var (ParK([],bind.bound)) c) context.kinds ts in
    ts,ce,ke

(*TBD do we want F-bounded checking with mutually recursive bounds? *)

and check_typ context t = match t.it with
    | Syntax.VarT (c,tys) ->
      let ts = List.map (check_typ context) tys in
      begin
         match lookup context.constructors c.it with
         | Some con ->
	   begin
	     match lookup_con context.kinds con with
             | Some kind ->
               begin match kind with
	        | DefK(bounds,u) ->
	          check_bounds t.at ts bounds;
                  (* subst ts bounds u  ?*)
		  VarT(con,ts)
	        | ObjK(bounds,actor,ftys) ->
	          check_bounds t.at ts bounds;
                  VarT(con,ts)
	        | ParK(bounds,bound) ->
       	          check_bounds t.at ts bounds;
                  VarT(con,ts)
	       end
	     | None ->
	       assert(false);
	       kindError c.at "unbound constructor %s " (Con.to_string con)
	    end
          | None -> kindError c.at "unbound type identifier %s" c.it
      end
    | Syntax.PrimT p -> PrimT p      
    | Syntax.ArrayT (m,t) ->
      ArrayT(m.it,check_typ context t)
    | Syntax.TupT ts ->
        let ts = List.map (check_typ context) ts in
        TupT ts
    | Syntax.FuncT(ts,dom,rng) ->
        let ts,ce,ke = check_typ_binds context ts in
        let context = union_kinds (union_constructors context ce) ke in
        FuncT(ts,check_typ context dom, check_typ context rng)
    | Syntax.OptT t -> OptT (check_typ context t)
    | Syntax.AsyncT t -> AsyncT (check_typ context t)
    | Syntax.LikeT t -> LikeT (check_typ context t)
    | Syntax.ObjT(a,fs) ->
      (* fields distinct? *)
      let _ = List.fold_left (fun (dom:string list) ({it={var;typ;mut};at}:Syntax.typ_field)->
                               if List.mem var.it dom
                               then kindError var.at "duplicate field name %s in object type" var.it
                               else (var.it::dom)) ([]:string list) fs in
      let fs = List.map (fun (f:Syntax.typ_field) ->
      	  {var=f.it.var.it;typ=check_typ context f.it.typ;mut=f.it.mut.it}) fs in
      (* sort by name (for indexed access *)
      let fs_sorted = List.sort (fun (f:typ_field)(g:typ_field) -> String.compare f.var g.var) fs in
      ObjT(a.it,fs_sorted)
    | Syntax.AnyT -> AnyT

    
and inf_lit context rl =
  match !rl with
    | NullLit -> PrimT NullT (* TBR *)
    | BoolLit _ -> PrimT BoolT
    | NatLit _ -> PrimT NatT
    | IntLit _ -> PrimT IntT
    | WordLit w ->
        PrimT (WordT (match w with 
	              | Word8 _ -> Width8
                      | Word16 _ -> Width16
                      | Word32 _ -> Width32
                      | Word64 _ -> Width64))
    | FloatLit _ -> PrimT FloatT
    | CharLit _ -> PrimT CharT
    | TextLit _ -> PrimT TextT
    | PreLit s ->
      rl := IntLit (Int32.to_int (Wasm.I32.of_string s)); (* default *)
      PrimT IntT


and check_lit at context t rl =
  let trap of_string s = try of_string s with _ -> typeError at "bad literal %s for type %s" s (typ_to_string t) in
  let unexpected() = typeError at "expected literal of type %s" (typ_to_string t) in
  let l = !rl in
  match norm_typ context t with
    | OptT t ->
      if l = NullLit then ()
      else check_lit at context t rl 
    | PrimT p ->
      begin
      match p with
      | NullT ->
        if l = NullLit then ()
        else unexpected()
      | NatT ->
      (match l with
       | NatLit _ -> ()
       | PreLit s ->
         let v = check_nat at s in 
	 rl := NatLit (Int32.to_int v)
       | _ -> unexpected())
      | IntT ->
       (match l with
       | IntLit _ -> ()
       | PreLit s ->
         let v = check_int at s in
	 rl := IntLit (Int32.to_int v)
       | _ -> unexpected())
     | WordT Width8 ->
       (match l with
       | WordLit (Word8 _) -> ()
       | PreLit s ->
         let v = check_word8 at s in
	 rl := WordLit (Word8 (Int32.to_int v))
       | _ -> unexpected())
     | WordT Width16 ->
       (match l with
       | WordLit (Word16 _) -> ()
       | PreLit s ->
         let v = check_word16 at s in
	 rl := WordLit (Word16 (Int32.to_int v))
       | _ -> unexpected())
     | WordT Width32 ->
       (match l with
       | WordLit (Word32 _) -> ()
       | PreLit s ->
         let v = check_word32 at s in
	 rl := WordLit (Word32 v)
       | _ -> unexpected())
     | WordT Width64 ->
       (match l with
       | WordLit (Word64 _) -> ()
       | PreLit s ->
         let v = check_word64 at s in
	 rl := WordLit (Word64 v)
       | _ -> unexpected())
     | _ ->
       let u = inf_lit context rl in
       if eq_typ context t u
       then ()
       else typeError at "expect literal of type %s found literal of type %s" (typ_to_string t) (typ_to_string u)
     end
   | _ -> typeError at "type %s has no literals" (typ_to_string t)


and inf_binop context at e1 bop e2 =
    let t1 = inf_exp context e1 in
    let t2 = inf_exp context e2 in
    match bop with
    | CatOp ->
      if eq_typ context t1 (PrimT TextT) && eq_typ context t1 t2 then
         t1
      else typeError at "arguments to concatenation operator must have Text type"
    | AddOp | SubOp | MulOp | DivOp | ModOp ->
      if numeric_typ context t1 && eq_typ context t1 t2 then
         t1
      else typeError at "arguments to numeric operator must have equivalent numeric types"
    | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
      if logical_typ context t1 && t1 = t2 then
         t1
      else typeError at "arguments to logical operator must have equivalent logical types"
    | _ -> typeError at "operator doesn't take operands of types %s and %s" (typ_to_string t1) (typ_to_string t2)

and check_binop context at t e1 bop e2 =
   (match bop with
    | CatOp ->
      if eq_typ context t (PrimT TextT)
      then ()
      else typeError at "expecting value of type %s, but concatenation returns a value of type Text" (typ_to_string t)
    | AddOp | SubOp | MulOp | DivOp | ModOp ->
      if numeric_typ context t
      then ()
      else typeError at "expecting value of type non-numeric type %s, operator returns a value of numeric type" (typ_to_string t)
    | AndOp | OrOp | XorOp | ShiftLOp | ShiftROp | RotLOp | RotROp ->
      if logical_typ context t
      then ()
      else typeError at "expecting value of type non-logical type %s, operator returns a value of logical type" (typ_to_string t)
    );
    check_exp context t e1;
    check_exp context t e2

and inf_relop context at e1 rop e2 =
    let t1 = inf_exp context e1 in
    let t2 = inf_exp context e2 in
    match rop with
    | EqOp 
    | NeqOp ->
      if equatable_typ context t1 && eq_typ context t1 t2
      then boolT
      else typeError at "arguments to an equality operator must have the same, equatable type"
    | _ ->
      if comparable_typ context t1 && eq_typ context t1 t2
      then boolT
      else typeError at "arguments to a relational operator must have the same, comparable type"

and check_relop context at t e1 rop e2 =
    if eq_typ context t boolT
    then ()
    else typeError at "expecting value of non-boolean type %s, relational operator returns a value of Bool type" (typ_to_string t);
    let _ = inf_relop context at e1 rop e2 in
    ()

and inf_uop context at uop e =
    let t = inf_exp context e in
    match uop with
    | PosOp 
    | NegOp ->
      if numeric_typ context t 
      then t
      else typeError at "argument to negation operator must have numeric type"
    | NotOp ->
      if logical_typ context t 
      then t
      else typeError at "arguments to a bitwise negation operator must have logical type"

and check_uop context at t uop e =
    match uop with
    | PosOp 
    | NegOp ->
      if numeric_typ context t 
      then check_exp context t e
      else typeError at "argument to negation operator must have numeric type"
    | NotOp ->
      if logical_typ context t 
      then check_exp context t e
      else typeError at "arguments to a bitwise negation operator must have logical type"

and inf_exp context e =
    let t = inf_exp' context e in
    (*TODO: record t in e *)
    t
and inf_exp' context e =
let labelOpt = context.label in
let context = {context with label = None} in
match e.it with
| VarE x ->
  (match lookup context.values x.it with
    | Some (ty,_) -> ty
    | None -> typeError x.at "unbound identifier %s" x.it)
| LitE rl ->
   inf_lit context rl
| UnE(uop,e1) ->
   inf_uop context e.at uop e1
| BinE (e1,bop,e2) ->
   inf_binop context e.at e1 bop e2
| RelE (e1,rop,e2) ->
   inf_relop context e.at e1 rop e2    
| TupE es ->
   let ts = List.map (inf_exp context) es in
   TupT ts
| ProjE(e,n) ->
  (match norm_typ context (inf_exp context e) with
   | TupT(ts) ->
     (try List.nth ts n
      with Failure _ -> typeError e.at "tuple projection %i >= %n is out-of-bounds" n (List.length ts))
   | t -> typeError e.at "expecting tuple type, found %s" (typ_to_string t))
| DotE(e,v) ->
  (match obj_typ context (inf_exp context e) with
   |(ObjT(a,fts) as t) ->
     (try let ft = List.find (fun (fts:typ_field) -> fts.var = v.it) fts in
         ft.typ
      with  _ -> typeError e.at "object of type %s has no field named %s" (typ_to_string t) v.it)
   | t -> typeError e.at "expecting object type, found %s" (typ_to_string t))   
| AssignE(e1,e2) ->
  begin
  match e1.it with
  (*TBC: array and object update *)
  | VarE v ->
    begin
    match lookup context.values v.it with
       | Some (t1,VarMut) ->
          check_exp context t1 e2;
	  unitT
       | Some (_,ConstMut) ->
          typeError e.at "cannot assign to immutable location"
       | None ->
   	  typeError e1.at "unbound mutable identifier %s" v.it
    end
  | DotE(o,v) ->
    begin
      match obj_typ context (inf_exp context o) with
      | (ObjT(a,fts) as t) ->
	 begin
	   try let ft = List.find (fun (fts:typ_field) -> fts.var = v.it) fts in
             match ft.mut with 
             | VarMut -> check_exp context ft.typ e2;
	       	      	 unitT
             | ConstMut ->  typeError e.at "cannot assign to immutable field %s"  v.it
	   with  _ -> typeError e.at "object of type %s has no field named %s" (typ_to_string t) v.it
	 end 
      | t -> typeError e.at "expecting object type, found %s" (typ_to_string t)
    end
  | IdxE(ea,ei) ->
    begin
      match norm_typ context (inf_exp context ea) with
      |  ArrayT(VarMut,t) ->
         check_exp context intT ei;
         check_exp context t e2;
	 unitT
      | ArrayT(ConstMut,_) as t1  -> 
        typeError e.at "cannot assign to immutable array of type %s" (typ_to_string t1) 
      | t -> typeError e.at "expecting array type, found %s" (typ_to_string t)
    end
  | _ ->
    typeError e.at "illegal assignment: expecting variable, mutable object field or mutable array element"
  end
| ArrayE [] ->
  typeError e.at "cannot infer type of empty array (use a type annotation)"
| ArrayE ((_::_) as es) ->
  let t1::ts = List.map (inf_exp context) es in
  if List.for_all (eq_typ context t1) ts
  then ArrayT(VarMut,t1) (* TBR how do we create immutable arrays? *)
  else typeError e.at "array contains elements of distinct types"
| IdxE(e1,e2) ->
  begin
  match norm_typ context (inf_exp context e1) with
  | ArrayT(_,t1) -> 
    check_exp context intT e2;
    t1
  | t -> typeError e.at "illegal indexing: expected an array, found" (typ_to_string t)
  end
| CallE(e1,e2) ->
 (match norm_typ context (inf_exp context e1) with
  | FuncT([],dom,rng) -> (* TBC polymorphic instantiation, perhaps by matching? *)
    let t2 = inf_exp context e2 in
    if eq_typ context t2 dom
    then rng
    else typeError e.at "illegal function application: expecting argument of type %s found argument of type %s" (typ_to_string dom) (typ_to_string t2)
  | _ -> typeError e.at "illegal application: not a function")
| BlockE es ->
  let context = addBreak context labelOpt unitT in
  check_block e.at context unitT es;
  unitT
| NotE(e) ->
  check_exp context boolT e;
  boolT
| AndE(e1,e2) ->
  check_exp context boolT e1;
  check_exp context boolT e2;
  boolT
| OrE(e1,e2) ->
  check_exp context boolT e1;
  check_exp context boolT e2;
  boolT
| IfE(e0,e1,e2) ->
  check_exp context boolT e0;
  let t1 = inf_exp context e1 in
  let t2 = inf_exp context e2 in
  if eq_typ context t1 t2 
  then t1
  else typeError e.at "branches of if have different types"
| SwitchE(e,cs) ->
  let t = inf_exp context e in
  if switchable_typ context t
  then match inf_cases context t cs None with
       | Some t -> t
       | None -> (* assert(false); *)
                 typeError e.at "couldn't infer type of case"
  else typeError e.at "illegal type for switch"
| WhileE(e0,e1) ->
  check_exp context boolT e0;
  let context' = addBreakAndContinue context labelOpt unitT in
  check_exp context' unitT e1;
  unitT
| LoopE(e,None) ->
  let context' = addBreakAndContinue context labelOpt unitT in
  check_exp context' unitT e;
  unitT (* absurdTy? *)
| LoopE(e0,Some e1) ->
  let context' = addBreakAndContinue context labelOpt unitT in
  check_exp context' unitT e0;
  (* TBR currently can't break or continue from guard *)
  check_exp context boolT e1;
  unitT
| ForE(p,e0,e1)->
  let t = inf_exp context e0 in (*TBR is this for arrays only? If so, what about mutability*)
  if iterable_typ context t
  then 
    let ve = check_pat context p (element_typ context t) in
    let context' = addBreakAndContinue {context with values = union context.values ve} labelOpt unitT in
    check_exp context' unitT e1;
    unitT
  else typeError e.at "cannot iterate over this type"
(* labels *)
| LabelE(l,e) ->
  let context = {context with label = Some l.it} in
  inf_exp context e
| BreakE(l,e) ->
  (match lookup context.breaks l.it  with
   | Some t -> 
     (* todo: check type of e against ts! *)
     check_exp context t e ;
     unitT (*TBR actually, this could be polymorphic at least in a checking context*)
   | None -> typeError e.at "break to unknown label %s" l.it)
| ContE l ->
  (match lookup context.continues l.it  with
   | Some _ -> 
     unitT (*TBR actually, this could be polymorphic at least in a checking context*)
   | None -> typeError e.at "continue to unknown label %s" l.it)
| RetE e0 ->
  (match context.returns with
   | Some t ->
     check_exp context t e0;
     unitT (*TBR actually, this could be polymorphic at least in a checking context*)
   | None -> typeError e.at "illegal return")
| AsyncE e0 ->
    let context = {values = context.values;
                   constructors = context.constructors;
		   kinds = context.kinds;
		   breaks = Env.empty;
		   label = context.label;
		   continues = Env.empty;
		   returns = Some unitT; (* TBR *)
		   awaitable = true} in
    let t = inf_exp context e0 in
    AsyncT t
| AwaitE e0 ->
    if context.awaitable
    then
      match norm_typ context (inf_exp context e0) with
      | AsyncT t -> t
      | t -> typeError e0.at "expecting expression of async type, found expression of type %s" (typ_to_string t)
    else typeError e.at "illegal await in synchronous context"
| AssertE e ->
    check_exp context boolT e;
    unitT
| IsE(e,t) ->
    let _ = inf_exp context e in
    let _ = check_typ context t in (*TBR what if T has free type variables? How will we check this, sans type passing *) 
    boolT
| AnnotE(e,t) ->
    let t = check_typ context t in 
    check_exp context t e;
    t
| DecE d ->
    let _ = check_decs context [d] in
    unitT
    
and inf_cases context pt cs t_opt  =
  match cs with
  | [] -> t_opt
  | {it={pat=p;exp=e};at}::cs ->
    let ve = check_pat context p pt in
    let t = inf_exp (union_values context ve) e in
    let t_opt' = match t_opt with
    	      | None -> Some t
 	      | Some t' ->
	         if eq_typ context t t'
    		 then Some t'
		 else typeError at "illegal case of different type from preceeding cases" in
    inf_cases context pt cs t_opt'
and check_exp context t e =
  let labelOpt = context.label in
  let context = {context with label = None} in
  match e.it with
  | LitE rl -> check_lit e.at context t rl
  | UnE (uop,e1) ->
    check_uop context e.at t uop e1
  | BinE (e1,bop,e2) ->
    check_binop context e.at t e1 bop e2
  | RelE (e1,rop,e2) ->
    check_relop context e.at t e1 rop e2
  | ArrayE es ->
    begin
      match norm_typ context t with
      | ArrayT (mut,t) ->
        List.iter (check_exp context t) es
      | _ -> typeError e.at "array expression cannot produce expected type %s" (typ_to_string t)
    end
(* | IdxE(e1,e2) ->
   TBR, unfortunately, we can't easily do IdxE in checking mode because we don't know whether to expect
   a mutable or immutable array type (we could do better on assignment of course),
   so we rely on inference instead
*)
  | AsyncE e0 ->
    (match norm_typ context t with
     | AsyncT t ->
     let context = {values = context.values;
                    constructors = context.constructors;
		    kinds = context.kinds;
		    breaks = Env.empty;
		    label = context.label;
		    continues = Env.empty;
		    returns = Some t; (* TBR *)
		    awaitable = true} in
     check_exp context t e0
     |_ -> typeError e.at "async expression cannot produce expected type %s" (typ_to_string t))
  | LoopE(e,None) ->
    let context' = addBreakAndContinue context labelOpt t in
    check_exp context' unitT e; (*TBR do we want to allow any type for the body? *)
  | BlockE es ->
    let context = addBreak context labelOpt t in
    check_block e.at context t es
  | BreakE _ ->
    ignore(inf_exp context e)
  | ContE _ ->
    ignore(inf_exp context e)
  | RetE _ ->
    ignore(inf_exp context e)

  
  | _ ->
    let t' = inf_exp context e in
    if (eq_typ context t t')
    then ()
    else typeError e.at "expecting expression of type %s found expression of type %s" (typ_to_string t) (typ_to_string t')
    
    
and inf_block context es =
  match es with
  | [] -> unitT
  | {it = DecE d;at}::es ->
    let ve,ce,ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    inf_block (union_kinds (union_constructors (union_values context ve) ce) ke) es
  | [e] -> inf_exp context e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    inf_block context es

and check_block r context t es =
  match es with
  | [] ->
    if eq_typ context t unitT
    then ()
    else typeError r "block  must end with expression of type" (typ_to_string t) 
  | {it = DecE d;at}::es ->
    let ve,ce,ke = check_decs context [d] in (* TBR: we currently check local decs sequentially, not recursively *)
    check_block r (union_kinds (union_constructors (union_values context ve) ce) ke) t es
  | [e] -> check_exp context t e
  | e::es ->
    check_exp context unitT e; (* TBR: is this too strict? do we want to allow implicit discard? *)
    check_block r context t es 

and check_dec pass context d =
    let ve,ce,ke = check_dec' pass context d in    
    (* TBC store ve *)
    ve,ce,ke

and check_dec' pass context d =     
    match d.it with
    | LetD (p,e) ->
      if pass < 3 then
	 Env.empty, Env.empty, ConEnv.empty
      else      
         let t = inf_exp context e in
         let ve = check_pat context p t in 
	 ve, Env.empty, ConEnv.empty
    | VarD (v,t,None) ->
      if pass < 3 then
      	 Env.empty, Env.empty, ConEnv.empty
      else
      let t = check_typ context t in
      Env.singleton v.it (t,VarMut), Env.empty, ConEnv.empty
    | VarD (v,t,Some e) ->
      if pass < 3 then
         Env.empty, Env.empty, ConEnv.empty
      else
	let t = check_typ context t in
      	check_exp context t e;
      	Env.singleton v.it (t,VarMut),
	Env.empty,
	ConEnv.empty
    | TypD(v,ts,t) ->
      let ts,ce_ts,ke_ts = check_typ_binds context ts in
      let con = if pass = 0
                then Con.fresh v.it
		else
                match lookup context.constructors v.it with
		| Some con -> con
		| None -> assert(false);failwith "Impossible"
      in
      let kind0 = ParK(ts,VarT(con,[])) (* dummy abstract type *) in 
      let ce0 = Env.singleton v.it con in
      let ke0 = ConEnv.singleton con kind0 in
      if pass = 0 then
      	 Env.empty, ce0, ke0
      else
      let context_ts = union_kinds (union_constructors context ce_ts) ke_ts in
      let t = check_typ context_ts t in
      let kind1 = DefK(ts,t) (* dummy type *) in 
      let ce1 = Env.singleton v.it con in
      let ke1 = ConEnv.singleton con kind1 in
         Env.empty, ce1, ke1
    | FuncD(v,ts,p,t,e) ->
      if pass < 3 then
      	 Env.empty, Env.empty, ConEnv.empty
      else
      let ts,ce,ke = check_typ_binds context ts in
      let context_ce = union_kinds (union_constructors context ce) ke in
      let ve,dom = inf_pat context_ce p in
      let rng = check_typ context_ce t in
      let funcT= FuncT(ts,dom,rng) (* TBR: we allow polymorphic recursion *) in
      let context_ce_ve_v = 
           let {values;constructors;kinds} = add_value (union_values context_ce ve) v.it (funcT,ConstMut) in
            {values;
             constructors;
	     kinds;
             label = None;
  	     breaks = Env.empty;
	     continues = Env.empty;
	     returns = Some rng;
	     awaitable = false}
      in
      check_exp context_ce_ve_v rng e;
      Env.singleton v.it (funcT,ConstMut), Env.empty, ConEnv.empty
    | ClassD(a,v,ts,p,efs) ->
      let ts,ce_ts,ke_ts = check_typ_binds context ts in
      let con = if pass = 0
                then Con.fresh v.it
		else
                match lookup context.constructors v.it with
		| Some con -> con
		| None -> assert(false);failwith "Impossible"
      in
      let kind0 = ObjK(ts,a.it,[]) in
      let ce0 = Env.singleton v.it con in
      let ke0 = ConEnv.singleton con kind0 in
      if pass = 0 then
      	 Env.empty, ce0, ke0
      else
      let context_ts = union_kinds (union_constructors context ce_ts) ke_ts in
      let context_ts_v = add_constructor context_ts v.it con kind0 in
      let ve,dom = inf_pat context_ts_v p in
      let classT = VarT(con,List.map (fun t -> VarT(t.var,[])) ts) in
      let consT = FuncT(ts,dom,classT) (* TBR: we allow polymorphic recursion *) in
      let ve1ce1ke1 = Env.singleton v.it (consT,ConstMut),ce0,ke0 in
      if pass = 1 then
          ve1ce1ke1
      else	  
      let context_ts_v_dom =
          {context_ts_v with values = union context_ts_v.values ve} in
      let rec pre_members context field_env efs =
      	  match efs with
	  | [] -> field_env
	  | {it={var;mut;priv;exp={it=AnnotE(e,t);at}}}::efs ->
	    let t = check_typ context t in
	    let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,t)  in
	    pre_members context field_env efs
	  | {it={var;mut;priv;exp={it=DecE({it=FuncD(v,us,p,t,e);at=_});at=_}}}::efs ->
	    let us,ce_us,ke_us = check_typ_binds context us in
	    let context_us = union_kinds (union_constructors context ce_us) ke_us in
	    let _,dom = inf_pat context_us p in
	    let rng = check_typ context_us t in
	    let funcT = FuncT(us,dom,rng) in
	    let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,funcT) in
    	    pre_members context field_env efs
          | {it={var;mut;priv;exp=e}}::efs ->
	    let t = inf_exp context e in (* TBR: this feels wrong as we don't know all field types yet, just the ones to the left *)
	    let field_env = disjoint_add_field var.at field_env var.it (mut.it,priv.it,t) in
	    pre_members context field_env efs
      in
      let pre_members = pre_members context_ts_v_dom Env.empty efs in
      let private_context = Env.map (fun (m,p,t) -> (t,m)) pre_members in
      let bindings = Env.bindings pre_members in
      let public_fields =
      	  let public_bindings = List.filter (fun (v,(m,p,t)) -> p = Public) bindings  in
	  List.map (fun (v,(m,p,t)) -> {var=v;typ=t;mut=m}) public_bindings
      in
      let kind2 = ObjK(ts,a.it,public_fields) in
      let ve2,ce2,ke2 = Env.singleton v.it (consT,ConstMut),Env.singleton v.it con, ConEnv.singleton con kind2 in
      if pass = 2 then
      	  ve2,ce2,ke2
      else (* pass = 3 *)
      	  let _ = assert(pass = 3) in
          let all_fields = List.map (fun (v,(m,p,t)) -> {var=v;typ=t;mut=m}) bindings in
	  let kind3 = ObjK(ts,a.it,all_fields) in
	  let field_context = add_constructor (add_value (union_values context_ts_v_dom private_context) v.it (consT,ConstMut))
	      		                                 v.it con kind3
	  in
	  (* infer the fields *)
      	  let _ = List.map (fun {it={var;mut;exp}} -> inf_exp field_context exp) efs in
          ve2,ce2,ke2


and inf_pats at context ve ts ps =
   match ps with
   | [] -> ve, TupT(List.rev ts)
   | p::ps ->
     let ve',t = inf_pat context p in
     inf_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') (t::ts) ps

and inf_pat context p =
   match p.it with
   | WildP ->  typeError p.at "can't infer type of pattern"
   | VarP v -> typeError p.at "can't infer type of pattern"
   | LitP l ->
     Env.empty,inf_lit context l
   | TupP ps ->
     inf_pats p.at context Env.empty [] ps
   | AnnotP(p,t) ->
     let t = check_typ context t in
     check_pat context p t,t
     
and check_pat context p t =
   match p.it with
   | WildP -> Env.empty
   | VarP v -> Env.singleton v.it (t,ConstMut)
   | LitP rl ->
      check_lit p.at context t rl;
      Env.empty
   | TupP ps ->
     (match norm_typ context t with
      | TupT ts ->
        check_pats p.at context Env.empty ps ts 
      | _ -> typeError p.at "expected pattern of non-tuple type, found pattern of tuple type")
   | AnnotP(p',t') ->
     let t' = check_typ context t' in
     if eq_typ context t t'
     then check_pat context p' t'
     else typeError p.at "expected pattern of one type, found pattern of unequal type"
and check_pats at context ve ps ts =
   match ps,ts with
   | [],[] -> ve
   | p::ps,t::ts ->
     let ve' = check_pat context p t in
     check_pats at context (disjoint_union at "duplicate binding for %s in pattern" ve ve') ps ts  (*TBR reject shadowing *)
   | [],ts -> typeError at "tuple pattern has %i fewer components than expected type" (List.length ts)
   | ts,[] -> typeError at "tuple pattern has %i more components than expected type" (List.length ts)
         
      
and check_decs_aux pass context ve ce ke ds  = match ds with
   |  [] ->  ve,ce,ke
   |  d::ds ->
      let ve1,ce1,ke1 = check_dec pass context d in
      check_decs_aux pass (union_kinds (union_constructors (union_values context ve1) ce1) ke1) (union ve ve1) (union ce ce1) (union_conenv ke ke1) ds
      

and check_decs context ds =
      (* declare type constructors *)
      let ve0,ce0,ke0 = check_decs_aux 0 context Env.empty Env.empty ConEnv.empty ds in
      (* declare instance constructors, given type constructors *)
      let ve1,ce1,ke1 = check_decs_aux 1 (union_kinds (union_constructors (union_values context ve0) ce0) ke0) Env.empty Env.empty ConEnv.empty ds in
      (* define type constructors (declare public member types) *)
      let ve2,ce2,ke2 = check_decs_aux 2 (union_kinds (union_constructors (union_values context ve1) ce1) ke1) Env.empty Env.empty ConEnv.empty ds in
      (* check classes definitions (check public and private member expressions *)
      let ve3,ce3,ke3 = check_decs_aux 3 (union_kinds (union_constructors (union_values context ve2) ce2) ke2) Env.empty Env.empty ConEnv.empty ds in
      ve3,ce3,ke3


let check_prog p =
    check_decs prelude p.it
     
    
    





