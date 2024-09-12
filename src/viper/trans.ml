open Source

open Common
open Syntax

module T = Mo_types.Type
module M = Mo_def.Syntax
module Arrange = Mo_def.Arrange

module String_map = Env.Make(String)
module Stamps = String_map

module Type_map = Env.Make (T.Ord)

(* symbol generation *)

let stamps : int Stamps.t ref = ref Stamps.empty

let reset_stamps () = stamps := Stamps.empty

let fresh_stamp name =
  let n = Lib.Option.get (Stamps.find_opt name !stamps) 0 in
  stamps := Stamps.add name (n + 1) !stamps;
  n

let fresh_id name =
  let n = fresh_stamp name in
  if n = 0 then
    name
  else Printf.sprintf "%s_%i" name (fresh_stamp name)

(* helpers for constructing annotated syntax *)

let (^^^) at it note = { it; at; note}

let (!!!) at it = (^^^) at it NoInfo


let intLitE at i =
  !!! at (IntLitE (Mo_values.Numerics.Int.of_int i))

let accE at ?(perm=FullP) fldacc =
  !!! at
    (AccE(
         fldacc,
         !!! at (PermE (!!! at perm))))

let rec conjoin es at =
  match es with
  | [] -> !!! at (BoolLitE true)
  | e::[] -> e
  | e::es' -> !!! at (AndE(e, conjoin es' at))

let rec adjoin ctxt e = function
  | [] -> e
  | f :: fs -> f ctxt (adjoin ctxt e fs)

let locE at arr_e ix_e field_name =
  !!! at (CallE ("$loc", [arr_e; ix_e])), !!! at field_name

let sizeE at lhs =
  !!! at (CallE ("$size", [lhs]))

let arrayAccE at lhs field_name perm =
  let (!!) p = !!! at p in
  !! (CallE ("$array_acc", [lhs; !!(FldE field_name); !!(PermE(!! perm))]))

let someE at x_e =
  !!! at (CallE ("Some", [x_e]))
let noneE at =
  !!! at (CallE ("None", []))
let isSomeE at opt_e =
  !!! at (FldAcc (opt_e, !!! at "isSome"))
let isNoneE at opt_e =
  !!! at (FldAcc (opt_e, !!! at "isNone"))
let fromSomeE at opt_e =
  !!! at (FldAcc (opt_e, !!! at "some$0"))

let (|:) (x_opt : 'a option) (xs : 'a list) : 'a list =
  match x_opt with
  | None -> xs
  | Some(x) -> x :: xs

type sort = Field | Local | Method

(* the LHS of an assignment *)
type lvalue =
  | LValueVar of id         (* a variable *)
  | LValueUninitVar of id   (* an uninitialized variable *)
  | LValueFld of fldacc     (* field access *)

let lvalue_str (lval : lvalue) : string =
  match lval with
  | LValueVar x -> x.it
  | LValueUninitVar x -> x.it
  | LValueFld fld -> (snd fld).it

type imported_module =
  | IM_Prim         (* mo:⛔ *)
  | IM_base_Array   (* mo:base/Array *)

module Env = T.Env
module Imports = Map.Make(String)

type ctxt =
  { self : string option;
    imports : imported_module Imports.t;
    ids : (sort * T.t) T.Env.t;
    label_to_tmp_var : id String_map.t; (* Motoko label -> associated tmp variable *)
    label_to_vpr_label : string String_map.t; (* Motoko label -> Viper label *)
    reqs : reqs; (* requirements for the prelude *)
    type_to_record_ctor : id Type_map.t;
    ghost_items : (ctxt -> item) list ref;
    ghost_inits : (ctxt -> seqn') list ref;
    ghost_perms : (ctxt -> Source.region -> exp) list ref;
    ghost_conc : (ctxt -> exp -> exp) list ref;
  }

(* All text literals are injectively mapped into int values *)
let text_literals : (string, int) Hashtbl.t = Hashtbl.create 32

let add_locals ctxt (locals : (id * T.typ) list) =
  let add (x, t) = Env.add x.it (Local, t) in
  { ctxt with ids = List.fold_right add locals ctxt.ids }

let self ctxt at =
  match ctxt.self with
  | Some id -> !!! at (LocalVar (!!! at id,!!! at RefT))
  | _ -> failwith "no self"

let tr_string_literal at str =
  let str_id =
    match Hashtbl.find_opt text_literals str with
    | None ->
      let fresh_str_id = Hashtbl.length text_literals in
      Hashtbl.add text_literals str fresh_str_id;
      fresh_str_id
    | Some id -> id
  in
  intLitE at str_id

let rec extract_invariants : item list -> (par -> invariants -> invariants) = function
  | [] -> fun _ x -> x
  | { it = InvariantI (s, e); at; _ } :: p ->
      fun self es ->
        !!! at (CallE(s, [ !!! at (LocalVar (fst self, snd self)) ]))
        :: extract_invariants p self es
  | _ :: p -> extract_invariants p

let rec extract_loop_invariants (e : M.exp) : (M.exp list * M.exp) =
  match e with
  | { it = M.BlockE ds; _ } ->
    let (invs, ds') = extract_loop_invariants' ds [] in
    (invs, { e with it = M.BlockE ds' })
  | _ -> ([], e)
and extract_loop_invariants' (ds : M.dec list) (acc : M.exp list) : (M.exp list * M.dec list) =
  match ds with
  | M.({ it = ExpD ({ it = AssertE (Loop_invariant, inv); at = assert_at; _ }); _ }) :: ds ->
      extract_loop_invariants' ds ({ inv with at = assert_at } :: acc)
  | _ -> (List.rev acc, ds)

let rec extract_concurrency (seq : seqn) : stmt' list * seqn =
  let open List in
  let extr (concs, stmts) s : stmt' list * stmt list =
    match s.it with
    | ConcurrencyS _ -> s.it :: concs, stmts
    | SeqnS seq ->
      let concs', seq = extract_concurrency seq in
      rev_append concs' concs, { s with it = SeqnS seq } :: stmts
    | WhileS (e, inv, seq) ->
      let concs', seq = extract_concurrency seq in
      rev_append concs' concs, { s with it = WhileS (e, inv, seq) } :: stmts
    | IfS (e, the, els) ->
      let the_concs, the = extract_concurrency the in
      let els_concs, els = extract_concurrency els in
      rev_append els_concs (rev_append the_concs concs), { s with it = IfS (e, the, els) } :: stmts
    | _ -> concs, s :: stmts in

  let stmts = snd seq.it in
  let conc, stmts = List.fold_left extr ([], []) stmts in
  rev conc, { seq with it = fst seq.it, rev stmts }

let concat_map_seqn' : ('a -> seqn') -> 'a list -> seqn' =
  fun f xs ->
    let ds, stmts = List.split (List.map f xs) in
    List.concat ds, List.concat stmts

let rec strip_par_p (p : M.pat) : M.pat =
  match p.it with
  | M.ParP p' -> strip_par_p p'
  | _         -> p

let rec strip_mut_t (t : T.typ) : T.typ =
  match t with
  | T.Mut t' -> strip_mut_t t'
  | _        -> t

let record_ctor_tag = "$RecordCtor_"

let mk_record_ctor (typ_id : M.typ_id) : id =
  let name = Format.asprintf "%s%s" record_ctor_tag typ_id.it in
  !!! Source.no_region name

let get_record_name ctxt (typ : T.typ) : string =
  let tag_len = String.length record_ctor_tag in
  let record_ctor_name = (Type_map.find typ ctxt.type_to_record_ctor).it in
  String.sub record_ctor_name tag_len (String.length record_ctor_name - tag_len)

let mk_record_field ~record_name ~fld_name =
  Format.asprintf "$%s$%s" record_name fld_name

let get_record_field ctxt (typ : T.typ) (fld : M.id) : M.id =
  let record_name = get_record_name ctxt typ in
  let fld_name = mk_record_field ~record_name ~fld_name:fld.it in
  { fld with it = fld_name }

let rec unit reqs (u : M.comp_unit) : prog Diag.result =
  Diag.(
    reset_stamps();
    try return (unit' reqs u) with
    | Unsupported (at, desc) -> error at "0" "viper" ("translation to viper failed:\n"^desc)
    | exn ->
      error
        u.it.M.body.at
        "1"
        "viper"
        (Format.asprintf
          "translation to viper failed. Reason: %s"
          (Printexc.to_string exn))
  )

and unit' reqs (u : M.comp_unit) : prog =
  let { M.imports; M.body } = u.it in
  match body.it with
  | M.ActorU(id_opt, decs) ->
    let ctxt = {
      self = None;
      imports = tr_imports imports;
      ids = Env.empty;
      label_to_tmp_var = String_map.empty;
      label_to_vpr_label = String_map.empty;
      reqs = reqs;
      type_to_record_ctor = Type_map.empty;
      ghost_items = ref [];
      ghost_inits = ref [];
      ghost_perms = ref [];
      ghost_conc = ref []
    } in
    let ctxt', perms, inits, mk_is = dec_fields ctxt decs in
    let is' = List.map (fun mk_i -> mk_i ctxt') mk_is in
    (* given is', compute ghost_is *)
    let ghost_is = List.map (fun mk_i -> mk_i ctxt') !(ctxt.ghost_items) in
    let init_id = !!! (Source.no_region) "__init__" in
    let self_id = !!! (Source.no_region) "$Self" in
    let self_typ = !!! (self_id.at) RefT in
    let ctxt'' = { ctxt' with self = Some self_id.it } in
    let perm =
      fun (at : region) ->
       List.fold_left
         (fun pexp -> fun p_fn ->
           !!! at (AndE(pexp, p_fn ctxt'' at)))
         (!!! at (BoolLitE true))
         (perms @ !(ctxt.ghost_perms))
    in
    (* Add initializer *)
    let init_list = concat_map_seqn' (fun mk_s -> mk_s ctxt'') (inits @ !(ctxt.ghost_inits)) in
    let init_body =
      !!! (body.at) init_list (* ATG: Is this the correct position? *)
    in
    let init_m =
      (^^^) (body.at) (MethodI(init_id, [self_id, self_typ], [], [], [], Some init_body)) ActorInit
    in
    let is'' = init_m :: is' in
    (* Add permissions *)
    let is''' = List.map (function
    | {it = MethodI (id, ins, outs, pres, posts, body); at; note: info} ->
      (^^^)
        at
        (MethodI (id, ins, outs,
          !!! at (CallE("$Perm", [self ctxt'' at]))::pres,
          !!! at (CallE("$Perm", [self ctxt'' at]))::posts,
          body))
        note
      | x -> x) is'' in
    (* Add functional invariants to public functions *)
    let invs = extract_invariants is''' (self_id, self_typ) [] in
    let is4 = List.map (function
      | {
        it = MethodI (id, ins, outs, pres, posts, body);
        at;
        note = ActorInit
      } -> ((^^^)
        at
        (MethodI(id, ins, outs,
          pres,
          posts @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          body))
        ActorInit
      )
      | {
        it = MethodI (id, ins, outs, pres, posts, body);
        at;
        note = PublicFunction x
      } -> ((^^^)
        at
        (MethodI(id, ins, outs,
          pres @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          posts @ [!!! at (CallE("$Inv", [self ctxt'' at]))],
          body))
        (PublicFunction x)
      )
      | x -> x
    ) is''' in
    let perm_def = !!! (body.at) (InvariantI("$Perm", perm body.at)) in
    let inv_def = !!! (body.at) (InvariantI("$Inv", adjoin ctxt'' (conjoin invs body.at) !(ctxt.ghost_conc))) in
    let is = ghost_is @ (perm_def :: inv_def :: is4) in
    !!! (body.at) is
  | _ -> assert false

and tr_imports (imps : M.import list) : imported_module Imports.t =
  List.fold_left
    (fun acc imp ->
      let k, v = tr_import imp in
      Imports.add k v acc)
    Imports.empty
    imps

and tr_import (imp : M.import) : (string * imported_module) =
  match imp.it with
  | ({it=M.VarP s;_}, "mo:⛔", _) -> (s.it, IM_Prim)
  | ({it=M.VarP s;_}, "mo:base/Array", _) -> (s.it, IM_base_Array)
  | (p, _, _) -> unsupported p.at (Arrange.pat p)

and dec_fields (ctxt : ctxt) (ds : M.dec_field list) =
  match ds with
  | [] ->
    (ctxt, [], [], [])
  | d :: ds ->
    let ctxt, perm, init, mk_i = dec_field ctxt d in
    let ctxt, perms, inits, mk_is = dec_fields ctxt ds in
    (ctxt, perm |: perms, init |: inits, mk_i::mk_is)

and dec_field ctxt d =
  let ctxt, perm, init, mk_i = dec_field' ctxt d.it in
   (ctxt,
    perm,
    init,
    fun ctxt' ->
      let (i, info) = mk_i ctxt' in
      (^^^) (d.at) i info)

and dec_field' ctxt d =
  match d.M.dec.it with
  (* type declarations*)
  | M.(TypD (typ_id, typ_binds, {note = T.Variant cons;_})) ->
    ctxt, None, None, fun ctxt ->
      let adt_param tb = id tb.it.M.var in
      let adt_con con = begin
        let con_name = !!! Source.no_region con.T.lab in
        let mk_field_name i = !!! Source.no_region (Format.asprintf "%s$%i" con.T.lab i) in
        { con_name;
          con_fields = match con.T.typ with
            | T.Tup ts -> List.mapi (fun i t -> mk_field_name i, tr_typ ctxt t) ts
            | t -> [mk_field_name 0, tr_typ ctxt t]
        }
      end in
      AdtI ({ typ_id with note = NoInfo },
            List.map adt_param typ_binds,
            List.map adt_con cons),
      NoInfo
  | M.(TypD (typ_id, typ_binds, { note = T.Obj (T.Object, flds) as typ; _ })) ->
    let con_name = mk_record_ctor typ_id in
    { ctxt with type_to_record_ctor = Type_map.add typ con_name ctxt.type_to_record_ctor },
    None,
    None,
    fun ctxt ->
      let adt_param tb = id tb.it.M.var in
      let adt_field field =
        let field_name = !!! Source.no_region (mk_record_field ~record_name:typ_id.it ~fld_name:field.T.lab) in
        let field_typ = tr_typ ctxt field.T.typ in
        field_name, field_typ
      in
      let adt_con =
        { con_name;
          con_fields = List.map adt_field flds
        }
      in
      AdtI ({ typ_id with note = NoInfo },
            List.map adt_param typ_binds,
            [ adt_con ]),
      NoInfo
  (* async functions *)
  | M.(LetD ({it=VarP f;note;_},
             {it=FuncE(x, sp, tp, p, t_opt, sugar,
             {it = AsyncE (T.Fut, _, e); _} );_}, None)) -> (* ignore async *)
      { ctxt with ids = Env.add f.it (Method, note) ctxt.ids },
      None, (* no perm *)
      None, (* no init *)
      fun ctxt ->
        let open Either in
        let self_id = !!! (Source.no_region) "$Self" in
        let method_args = args p in
        let method_args' = List.map (fun (id, t) -> id, tr_typ ctxt t) method_args in
        let ctxt = { ctxt
          with self = Some self_id.it;
               ids = List.fold_left (fun env (id, t) -> Env.add id.it (Local, t) env) ctxt.ids method_args }
        in
        let stmts = stmt ctxt e in
        let _, stmts = extract_concurrency stmts in
        let pres, stmts' = List.partition_map (function { it = PreconditionS exp; at; _ } -> Left { exp with at } | s -> Right s) (snd stmts.it) in
        let posts, stmts' = List.partition_map (function { it = PostconditionS exp; at; _ } -> Left { exp with at } | s -> Right s) stmts' in
        let arg_preds = local_access_preds ctxt in
        let ret_preds, ret = rets ctxt t_opt in
        let pres = arg_preds @ pres in
        let posts = arg_preds @ ret_preds @ posts in
        let stmts'' = stmts' @ [!!! Source.no_region (LabelS(!!! (Source.no_region) "$Ret"))] in
        (MethodI(id f, (self_id, !!! Source.no_region RefT)::method_args', ret, pres, posts, Some { stmts with it = fst stmts.it, stmts'' } ),
        PublicFunction f.it)
  (* private sync functions *)
  | M.(LetD ({it=VarP f; note;_},
             {it=FuncE(x, sp, tp, p, t_opt, sugar, e );_},
             None)) ->
      { ctxt with ids = Env.add f.it (Method, note) ctxt.ids },
      None, (* no perm *)
      None, (* no init *)
      fun ctxt ->
        let open Either in
        let self_id = !!! (Source.no_region) "$Self" in
        let method_args = args p in
        let method_args' = List.map (fun (id, t) -> id, tr_typ ctxt t) method_args in
        let ctxt = { ctxt
          with self = Some self_id.it;
               ids = List.fold_left (fun env (id, t) -> Env.add id.it (Local, t) env) ctxt.ids method_args }
        in
        let stmts = stmt ctxt e in
        let _, stmts = extract_concurrency stmts in
        let pres, stmts' = List.partition_map (function { it = PreconditionS exp; at; _ } -> Left { exp with at } | s -> Right s) (snd stmts.it) in
        let posts, stmts' = List.partition_map (function { it = PostconditionS exp; at; _ } -> Left { exp with at } | s -> Right s) stmts' in
        let arg_preds = local_access_preds ctxt in
        let ret_preds, ret = rets ctxt t_opt in
        let pres = arg_preds @ pres in
        let posts = arg_preds @ ret_preds @ posts in
        let stmts'' = stmts' @ [!!! Source.no_region (LabelS(!!! (Source.no_region) "$Ret"))] in
        (MethodI(id f, (self_id, !!! Source.no_region RefT)::method_args', ret, pres, posts, Some { stmts with it = fst stmts.it, stmts'' } ),
        PrivateFunction f.it)
  (* fields *)
  | M.LetD ({it=M.VarP x;_}, e, None)
  | M.VarD (x, e) ->
      let t = e.note.M.note_typ in
      let fldacc = fun ctxt' -> (self ctxt' e.at, id x) in
      let lhs = fun ctxt' -> !!! Source.no_region (FldAcc (fldacc ctxt')) in
      let perms ctxt' at =
            conjoin ([ accE at (self ctxt' at, id x) ]
                     @ (access_pred ctxt' (lhs ctxt') t |: [])) at in
      { ctxt with ids = Env.add x.it (Field, t) ctxt.ids },
      Some perms, (* perm *)
      Some (fun ctxt' -> (* init *)
          let at = span x.at e.at in
          assign_stmts ctxt' at (LValueFld (fldacc ctxt')) e),
      (fun ctxt ->
        (FieldI(id x, tr_typ ctxt t),
        NoInfo))
  (* invariants *)
  | M.(ExpD { it = AssertE (Invariant, e); at; _ }) ->
      ctxt,
      None, (* no perm *)
      None, (* no init *)
      fun ctxt ->
        (InvariantI (Printf.sprintf "invariant_%d" at.left.line, exp { ctxt with self = Some "$Self" }  e), NoInfo)
  | _ ->
     unsupported d.M.dec.at (Arrange.dec d.M.dec)

and args p = match p.it with
  | M.TupP ps -> List.map arg ps
  | M.ParP p  -> [arg p]
  | _ -> unsupported p.at (Arrange.pat p)
and arg p = match p.it with
  | M.AnnotP (p, t) ->
      (match p.it with
        | M.VarP x -> (id x, t.note)
        | _ -> unsupported p.at (Arrange.pat p))
  | _ -> unsupported p.at (Arrange.pat p)

and access_pred ctxt lhs t =
  match T.normalize t with
  | T.Array elem_t -> Some (array_acc Source.no_region ctxt lhs elem_t)
  | _ -> None

(* Get access predicates for all local variables in current scope *)
and local_access_preds ctxt =
  let (!!) p = !!! Source.no_region p in
  let preds = Env.fold (fun id info preds ->
      match info with
      | (Local, t) ->
        let pred = access_pred ctxt !!(LocalVar (!!id, tr_typ ctxt t)) t in
        pred |: preds
      | _ -> preds)
    ctxt.ids []
  in preds

and block ctxt at ds =
  let ctxt, mk_ss = decs ctxt ds in
  !!! at (mk_ss ctxt)

and decs ctxt ds =
  match ds with
  | [] -> (ctxt, fun ctxt' -> ([],[]))
  | d::ds' ->
    let (ctxt1, mk_s) = dec ctxt d in
    let (ctxt2, mk_ss) = decs ctxt1 ds' in
    (ctxt2,
     fun ctxt' ->
       let (l, s) = mk_s ctxt' in
       let (ls, ss) = mk_ss ctxt' in
       (l @ ls, s @ ss))

and dec ctxt d =
  let (!!) p = !!! (d.at) p in
  match d.it with
  | M.LetD ({it=M.VarP x;_}, e, None)
  | M.VarD (x, e) ->
    { ctxt with ids = Env.add x.it (Local, e.note.M.note_typ) ctxt.ids },
    fun ctxt' ->
      let lval = LValueUninitVar (id x) in
      let d = !!(id x, tr_typ ctxt' e.note.M.note_typ) in
      let ds, stmts = assign_stmts ctxt' d.at lval e in
      (d :: ds, stmts)
  | M.LetD ({it=M.TupP _;_} as p, scrut, None) ->
    add_locals ctxt (unwrap_tup_vars_pat p),
    fun ctxt' ->
      let conds, _, (ds, stmts) = pat_match ctxt' scrut p in
      let irrefutable = (conds = []) in
      if not irrefutable then failwith "impossible: tuple patterns must be irrefutable" else
      ds, stmts
  | M.(ExpD e) -> (* TODO: restrict to e of unit type? *)
     (ctxt,
      fun ctxt' ->
        let s = stmt ctxt' e in
        s.it)
  | _ ->
     unsupported d.at (Arrange.dec d)

and compile_while
  ctxt
  ?(label_id : M.id option)
  ~(at : region)
  ~(pred : M.exp)
  ~(body : M.exp)
  () : seqn =
  let (!!) p = !!! at p in
  let (invs, body) = extract_loop_invariants body in
  let invs = (* TODO: automatic adding invariant into loop require more pondering*)
             (* [!!(AndE(!!(CallE("$Perm", [self ctxt at])), *)
             (*          !!(CallE("$Inv",  [self ctxt at]))))] *)
             [!!(CallE("$Perm", [self ctxt at]))]
             @ local_access_preds ctxt
             @ List.map (exp ctxt) invs in
  let pred = exp ctxt pred in
  let body =
    match label_id with
    | Some label_id ->
      let label, ctxt = loop_label ctxt label_id in
      let stmts = stmt ctxt body in
      let decls, stmts = stmts.it in
      !!(decls, stmts @ [ !!(LabelS label) ])
    | None -> stmt ctxt body
  in
  !!([], [ !!(WhileS(pred, invs, body)) ])

and stmt ctxt (s : M.exp) : seqn =
  let (!!) p = !!! (s.at) p in
  match s.it with
  | M.TupE [] ->
     block ctxt s.at []
  | M.BlockE ds ->
     block ctxt s.at ds
  | M.IfE(e, s1, s2) ->
    !!([],
       [ !!(IfS(exp ctxt e, stmt ctxt s1, stmt ctxt s2))])
  | M.(AwaitE(T.Fut, { it = AsyncE (T.Fut, _, e); at; _ })) -> (* gross hack *)
     let id = fresh_id "$message_async" in
     let (!!) p = !!! (s.at) p in
     let (!@) p = !!! at p in
     ctxt.ghost_items :=
       (fun ctxt ->
         !!(FieldI (!!id, !!IntT))) ::
       !(ctxt.ghost_items);
     let mk_s = fun ctxt ->
       [],
       [!@ (FieldAssignS ((self  ctxt s.at, !!id),
            intLitE (s.at) 0))]
     in
     ctxt.ghost_inits := mk_s :: !(ctxt.ghost_inits);
     let mk_p = fun ctxt at ->
       accE at (self ctxt at, !!! at id)
     in
     ctxt.ghost_perms := mk_p :: !(ctxt.ghost_perms);
     let stmts = stmt ctxt e in
     (* assume that each `async {...}` has an assertion *)
     let conc, _ = extract_concurrency stmts in
     let mk_c = match conc with
       | [] ->
         fun _ x -> x
       | ConcurrencyS ("1", _, cond) :: _ ->
         let (!?) p = !!! (cond.at) p in
         let zero, one = intLitE Source.no_region 0, intLitE Source.no_region 1 in
         fun ctxt x ->
           let ghost_fld () = !?(FldAcc (self ctxt cond.at, !?id)) in
           let between = !?(AndE (!?(LeCmpE (zero, ghost_fld ())), !?(LeCmpE (ghost_fld (), one)))) in
           let is_one = !?(EqCmpE (ghost_fld (), one)) in
           !?(AndE (x, !?(AndE (between, !?(Implies (is_one, cond.it (exp ctxt)))))))
       | _ -> unsupported e.at (Arrange.exp e) in
     ctxt.ghost_conc := mk_c :: !(ctxt.ghost_conc);
     !!([],
        [ !!(FieldAssignS(
            (self ctxt Source.no_region, !!id),
            (!!(AddE(!!(FldAcc (self ctxt (s.at), !!id)),
                     intLitE Source.no_region 1)))));
          !@(ExhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                              !@(CallE("$Inv", [self ctxt at]))))));
          !@(SeqnS (
              !@([],
                 [
                   !@(InhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                                  !@(AndE(!@(CallE("$Inv", [self ctxt at])),
                                          !@(GtCmpE(!@(FldAcc (self ctxt at, !@id)),
                                               intLitE Source.no_region 0))))))));
                   !@(FieldAssignS(
                          (self ctxt at, !@id),
                          (!@(SubE(!@(FldAcc (self ctxt at, !@id)),
                                   intLitE at 1)))));
                   !!! (e.at) (SeqnS stmts);
                   !@(ExhaleS (!@(AndE(!@(CallE("$Perm", [self ctxt at])),
                                       !@(CallE("$Inv", [self ctxt at])))))) ])));
          !!(InhaleS (!!(AndE(!!(CallE("$Perm", [self ctxt at])),
                              !!(CallE("$Inv", [self ctxt at]))))));
        ])
  | M.WhileE(e, { it = M.LabelE (cont_label_id, _typ, s1); _}) ->
      compile_while ~label_id:cont_label_id ~at:s.at ~pred:e ~body:s1 ctxt ()
  | M.WhileE(e, s1) -> compile_while ~at:s.at ~pred:e ~body:s1 ctxt ()
  | M.(SwitchE(scrut, cs)) ->
      !!(switch_stmts ctxt s.at scrut cs)
  | M.(AssignE({it = VarE x; _}, e2)) ->
      let lval =
        match fst (Env.find x.it ctxt.ids) with
        | Local -> LValueVar (!!! (x.at) (x.it))
        | Field -> LValueFld (self ctxt x.at, id_ref x)
        | _ -> unsupported s.at (Arrange.exp s)
      in !!(assign_stmts ctxt s.at lval e2)
  | M.(AssignE({it = IdxE (e1, e2);_}, e3)) ->
      let lval = LValueFld (array_loc ctxt s.at e1 e2 e3.note.M.note_typ) in
      !!(assign_stmts ctxt s.at lval e3)
  | M.AssertE (M.Precondition, e) ->
    !!( [],
        [ !!(PreconditionS (exp ctxt e)) ])
  | M.AssertE (M.Postcondition, e) ->
    !!([],
       [ !!(PostconditionS (exp ctxt e)) ])
  | M.AssertE (M.Concurrency n, e) ->
    !!([],
       [ !!(ConcurrencyS (n, exp ctxt e, !! ((|>) e))) ])
  | M.AssertE (M.Static, e) ->
    !!([],
       [ !!(AssertS (exp ctxt e)) ])
  | M.AssertE (M.Runtime, e) ->
    !!([],
       [ !!(AssumeS (exp ctxt e)) ])
  | M.(CallE({it = VarE m; _}, inst, args)) ->
    !!([],
       [ !!(MethodCallS ([], id_ref m,
       let self_var = self ctxt m.at in
       self_var :: call_args ctxt args))])
  | M.RetE e ->
      let lval = (LValueUninitVar (!!! (Source.no_region) "$Res")) in
      let ds, stmts = assign_stmts ctxt s.at lval e in
      let stmt = !!(GotoS(!!! (Source.no_region) "$Ret")) in
      !!(ds, stmts @ [stmt])
  | M.LabelE (label_id, typ, e) ->
      let label_id, ctxt = loop_label ctxt label_id in
      let label = !!! Source.no_region (LabelS label_id) in
      let stmts = stmt ctxt e in
      let decls, stmts = stmts.it in
      let stmts = stmts @ [ label ] in
      !!(decls, stmts)
  | M.BreakE (label_id, { it = M.TupE []; _ }) ->
    (* Loop case *)
    let label_name = String_map.find label_id.it ctxt.label_to_vpr_label in
    !!([], [ !!(GotoS !!label_name) ])
  | M.BreakE (label_id, ret) ->
    (* Expression case *)
    let stmts =
      match String_map.find_opt label_id.it ctxt.label_to_tmp_var with
      | None -> stmt ctxt ret
      | Some x ->
        let lvalue = LValueVar x in
        let stmts = assign_stmts ctxt ret.at lvalue ret in
        !!stmts
    in
    let decls, stmts = stmts.it in
    let label_name = String_map.find label_id.it ctxt.label_to_vpr_label in
    !!(decls, stmts @ [ !!(GotoS !!label_name) ])
  | _ ->
     unsupported s.at (Arrange.exp s)

(* Translate switch(scrut){...}. *)
and switch_stmts ctxt at (scrut : M.exp) (cases : M.case list) : seqn' =
  match cases with
  | [] -> [], []
  | c :: cs -> begin
      let (!!) a = !!! (c.at) a in
      let {M.pat = p; M.exp = e} = c.it in
      let conds, p_scope, (p_ds, p_stmts) = pat_match ctxt scrut p in
      let local_ctxt = add_locals ctxt p_scope in
      let {it = (e_ds, e_stmts);_} = stmt local_ctxt e in
      let the = (p_ds @ e_ds, p_stmts @ e_stmts) in
      let els = switch_stmts ctxt at scrut cs in
      [], [!!(IfS(conjoin conds at, !!the, !!els))]
    end

and pat_match ctxt (scrut : M.exp) (p : M.pat) : exp list * (id * T.typ) list * seqn' =
  let (!!) a = !!! (p.at) a in
  match (strip_par_p p).it with
  | M.AnnotP (p, _typ) -> pat_match ctxt scrut p
  | M.LitP lit ->
    begin match !lit with
    | M.NullLit -> [isNoneE (p.at) (exp ctxt scrut)], [], ([], [])
    | M.BoolLit b ->
      let e_scrut = exp ctxt scrut in
      let rhs = !!(BoolLitE b) in
      [ !!(EqCmpE (e_scrut, rhs)) ], [], ([], [])
    | M.NatLit n | M.IntLit n ->
      let e_scrut = exp ctxt scrut in
      let rhs = !!(IntLitE n) in
      [ !!(EqCmpE (e_scrut, rhs)) ], [], ([], [])
    | M.TextLit str ->
      let e_scrut = exp ctxt scrut in
      let rhs = tr_string_literal p.at str in
      [ !!(EqCmpE (e_scrut, rhs)) ], [], ([], [])
    | M.PreLit _ -> failwith "Expected PreLit to be eliminated after typing"
    | _          -> unsupported (p.at) (Arrange.lit !lit)
    end
  | M.OptP p' ->
    let e_scrut = exp ctxt scrut in
    let cond = isSomeE (p.at) e_scrut in
    let x, t = unwrap_var_pat p' in
    let ds = [!!(x, tr_typ ctxt t)] in
    let stmts = [!!(assign_stmt (LValueUninitVar x) (fromSomeE (scrut.at) e_scrut))] in
    [cond], [(x, t)], (ds, stmts)
  | M.(TupP ps) ->
    let p_scope = unwrap_tup_vars_pat p in
    let ds = List.map (fun (x, t) -> !!(x, tr_typ ctxt t)) p_scope in
    let tup_prj_assign_stmt i lval t = begin
      let rhs = prjE ctxt (p.at) scrut i in
      !!(assign_stmt lval rhs)
    end in
    let stmts = List.mapi (fun i (x, t) -> tup_prj_assign_stmt i (LValueUninitVar x) t) p_scope in
    [], p_scope, (ds, stmts)
  | M.(TagP(l, p')) ->
    let e_scrut = exp ctxt scrut in
    let cond = !!(FldAcc (e_scrut, !!("is" ^ l.it))) in
    let p_scope = unwrap_tup_vars_pat p' in
    let ds = List.map (fun (x, t) -> !!(x, tr_typ ctxt t)) p_scope in
    let fld_assign_stmt i lval = begin
      let rhs = !!(FldAcc (e_scrut, !!(l.it ^ "$" ^ string_of_int i))) in
      !!(assign_stmt lval rhs)
    end in
    let stmts = List.mapi (fun i (x, t) -> fld_assign_stmt i (LValueUninitVar x)) p_scope in
    [cond], p_scope, (ds, stmts)
  | M.ObjP pat_fields ->
    let e_scrut = exp ctxt scrut in
    let p_scope = List.map (fun M.{ it = { id = _; pat }; _ } -> unwrap_var_pat pat) pat_fields in
    let ds = List.map (fun (x, t) -> !!(x, tr_typ ctxt t)) p_scope in
    let fld_assign_stmt lval fld_name =
      let rhs = !!(FldAcc (e_scrut, id fld_name)) in
      !!(assign_stmt lval rhs)
    in
    let stmts =
      List.map2
        (fun M.{ it = { id = fld_name; pat = _ }; _ } (x, _) ->
          let fld_name = get_record_field ctxt (T.normalize p.note) fld_name in
          fld_assign_stmt (LValueUninitVar x) fld_name)
        pat_fields p_scope
    in
    [], p_scope, (ds, stmts)
  | M.WildP -> [], [], ([], [])
  | _ -> unsupported p.at (Arrange.pat p)

and unwrap_tup_vars_pat (p : M.pat) : (id * T.typ) list =
  match (strip_par_p p).it with
  | M.TupP ps -> List.map unwrap_var_pat ps
  | _         -> [unwrap_var_pat p]

and unwrap_var_pat (p : M.pat) : (id * T.typ) =
  match (strip_par_p p).it with
  | M.VarP x         -> id x, p.note
  | M.AnnotP (p', t) -> fst (unwrap_var_pat p'), t.note
  | _                -> unsupported p.at (Arrange.pat p)

(* Translate assignment a:=b or initialization. May create temporary variables
   if the LHS is a field or if the RHS is an array. *)
and assign_stmts ctxt at (lval : lvalue) (e : M.exp) : seqn' =
  let (!!) p = !!! at p in
  let t = e.note.M.note_typ in
  match e with
  | M.({it=TupE [];_}) -> [], []
  | M.({it=AnnotE (e, _);_}) -> assign_stmts ctxt at lval e
  | M.({it=CallE ({it=M.DotE ({it=M.VarE(m);_}, {it="init";_});_}, _inst, args);_})
      when Imports.find_opt (m.it) ctxt.imports = Some(IM_base_Array)
      ->
    begin match args with
    | M.({it=TupE([e1;e2]); _}) ->
      fld_via_tmp_var ctxt lval t (fun x ->
        let lhs = !!(LocalVar (x, tr_typ ctxt t)) in
        [], [ !!(AssertS !!(GeCmpE (exp ctxt e1, intLitE at 0)))
            ; !!(InhaleS (array_acc at ctxt lhs (array_elem_t t)))
            ; !!(InhaleS (array_size_inv at lhs (exp ctxt e1)))
            ; !!(InhaleS (array_init_const at ctxt lhs (array_elem_t t) (exp ctxt e2))) ]
      )
    | _ -> unsupported args.at (Arrange.exp args)
    end
  | M.({it = CallE({it = VarE m; _}, inst, args); _}) ->
    fld_via_tmp_var ctxt lval t (fun x ->
      let self_var = self ctxt m.at in
      [], [ !!(MethodCallS ([x], id_ref m, self_var :: call_args ctxt args)) ])
  | M.({it=ArrayE(mut, es); _}) ->
    via_tmp_var ctxt lval t (fun x ->
      let lhs = !!(LocalVar (x, tr_typ ctxt t)) in
      [], array_alloc at ctxt lhs (array_elem_t t) es)
  | M.{ it = LabelE (label_id, ret_typ, e); note; _ } ->
    via_tmp_var ctxt lval t (fun x ->
      label_expr_alloc ~label_id ~label_type:ret_typ ~label_rhs:e ~label_note:note at ctxt x)
  | _ ->
    [], [!!(assign_stmt lval (exp ctxt e))]

and assign_stmt (lval : lvalue) (e : exp) : stmt' =
  match lval with
    | LValueVar x       -> VarAssignS(x, e)
    | LValueUninitVar x -> VarAssignS(x, e)
    | LValueFld fld     -> FieldAssignS(fld, e)

and fld_via_tmp_var ctxt (lval : lvalue) (t : T.typ) (f : id -> seqn') : seqn' =
  match lval with
  | LValueVar x       -> f x
  | LValueUninitVar x -> f x
  | LValueFld _       -> via_tmp_var ctxt lval t f

and via_tmp_var ctxt (lval : lvalue) (t : T.typ) (f : id -> seqn') : seqn' =
  match lval with
  | LValueUninitVar x -> f x  (* initialization never needs a tmp variable *)
  | _ ->
    let (!!) p = !!! Source.no_region p in
    let tmp_id = !! (fresh_id ("$t_" ^ lvalue_str lval)) in
    let tmp_typ = tr_typ ctxt t in
    let tmp_e = !! (LocalVar (tmp_id, tmp_typ)) in
    let d = !! (tmp_id, tmp_typ) in
    let ds, stmts = f tmp_id in
    let stmt = !!(assign_stmt lval tmp_e) in
    d :: ds, stmts @ [stmt]

and call_args ctxt e =
  match e with
  | {it = M.TupE args; _} -> List.map (fun arg -> exp ctxt arg) args
  | arg -> [exp ctxt arg]

and exp ctxt e =
  let open Mo_values.Operator in
  let (!!) p = !!! (e.at) p in
  let e_t = e.note.M.note_typ in
  match e.it with
  | M.VarE x ->
    begin
     match Env.find x.it ctxt.ids with
     | Local, t ->
        !!(LocalVar (id_ref x, tr_typ ctxt t))
     | Field, _ ->
        !!(FldAcc (self ctxt x.at, id_ref x))
     | _ ->
        unsupported e.at (Arrange.exp e)
    end
  | M.AnnotE(a, b) ->
    exp ctxt a
  | M.CallE ({it=M.DotE (e1, {it="size";_});_}, _inst, {it=M.TupE ([]);at;_})
      -> sizeE at (exp ctxt e1)
  | M.LitE r ->
    begin match !r with
    | M.BoolLit b ->
       !!(BoolLitE b)
    | M.IntLit i ->
       !!(IntLitE i)
    | M.NatLit i ->
       !!(IntLitE i)
    | M.NullLit ->
       noneE (e.at)
    | M.TextLit str ->
      tr_string_literal e.at str
    | _ ->
       unsupported e.at (Arrange.exp e)
    end
  | M.NotE e ->
     !!(NotE (exp ctxt e))
  | M.RelE (ot, e1, op, e2) ->
     let e1, e2 = exp ctxt e1, exp ctxt e2 in
     !!(match op with
      | EqOp -> EqCmpE (e1, e2)
      | NeqOp -> NeCmpE (e1, e2)
      | GtOp -> GtCmpE (e1, e2)
      | GeOp -> GeCmpE (e1, e2)
      | LtOp -> LtCmpE (e1, e2)
      | LeOp -> LeCmpE (e1, e2))
  | M.BinE (ot, e1, op, e2) ->
     let e1, e2 = exp ctxt e1, exp ctxt e2 in
     !!(match op with
      | AddOp -> AddE (e1, e2)
      | SubOp -> SubE (e1, e2)
      | MulOp -> MulE (e1, e2)
      | DivOp -> DivE (e1, e2)
      | ModOp -> ModE (e1, e2)
      | CatOp -> CallE ("$concat", [e1; e2])
      | _ -> unsupported e.at (Arrange.exp e))
  | M.OrE (e1, e2) ->
     !!(OrE (exp ctxt e1, exp ctxt e2))
  | M.AndE (e1, e2) ->
     !!(AndE (exp ctxt e1, exp ctxt e2))
  | M.ImpliesE (e1, e2) ->
     !!(Implies (exp ctxt e1, exp ctxt e2))
  | M.OldE e ->
    !!(Old (exp ctxt e))
  | M.IdxE (e1, e2) ->
     !!(FldAcc (array_loc ctxt e.at e1 e2 e_t))
  | M.ProjE (e, i) ->
     prjE ctxt e.at e i
  | M.OptE e ->
     someE (e.at) (exp ctxt e)
  | M.TagE (tag, e) ->
     !!(match e.it with
        | M.TupE es -> CallE (tag.it, List.map (exp ctxt) es)
        | _ -> CallE (tag.it, [exp ctxt e]))
  | M.ObjE ([], flds) ->
    (match T.normalize e.note.M.note_typ with
    | T.Obj (T.Object, typ_flds) as t ->
      let record_ctor_name = Type_map.find t ctxt.type_to_record_ctor in
      let flds = List.map (fun M.{ it = { mut; id; exp }; _} ->
        match mut.it with
        | M.Const -> id.it, exp
        | M.Var -> unsupported mut.at (Arrange.exp e)) flds
      in
      let args = List.map (fun T.{ lab; _ } ->
        let rhs_exp = List.assoc lab flds in
        exp ctxt rhs_exp) typ_flds
      in
      !!(CallE (record_ctor_name.it, args))
    | T.Obj _ -> unsupported e.at (Arrange.exp e)
    | _ -> assert false)
  | M.DotE (proj, fld) when Type_map.mem (T.normalize proj.note.M.note_typ) ctxt.type_to_record_ctor ->
    let proj_t = T.normalize proj.note.M.note_typ in
    let proj = exp ctxt proj in
    let fld = id (get_record_field ctxt proj_t fld) in
    !!(FldAcc (proj, fld))
  | M.TupE es ->
      let n = List.length es in
      ctxt.reqs.tuple_arities := IntSet.add n !(ctxt.reqs.tuple_arities);
      !!(CallE (tup_con_name n, List.map (exp ctxt) es))
  | M.CallE ({ it = M.DotE ({it=M.VarE(m);_}, {it=predicate_name;_}); _ }, _inst, { it = M.FuncE (_, _, _, pattern, _, _, e); note; _ })
    when Imports.find_opt (m.it) ctxt.imports = Some(IM_Prim)
      && (predicate_name = "forall" || predicate_name = "exists")
    ->
    let binders = extract_binders pattern in
    let typs =
      match M.(note.note_typ) with
      | T.Func (_, _, _, [ T.Tup args ], _) -> args
      | T.Func (_, _, _, [ arg ], _) -> [ arg ]
      | _ -> []
    in
    let typed_binders =
      List.fold_left2
        (fun acc b t -> (id b, t) :: acc)
        [] binders typs
    in
    let ctxt = add_locals ctxt typed_binders in
    let typed_binders = List.map (fun (b, t) -> (b, tr_typ ctxt t)) typed_binders in
    let e = exp ctxt e in
    (match predicate_name with
    | "forall" -> !!(ForallE (typed_binders, e))
    | "exists" -> !!(ExistsE (typed_binders, e))
    | _ -> assert false)
  | M.CallE ({ it = M.DotE ({it=M.VarE(m);_}, {it="Ret";_}); _ }, _, _)
    when Imports.find_opt (m.it) ctxt.imports = Some(IM_Prim) -> !!(FldE "$Res")
  | _ ->
     unsupported e.at (Arrange.exp e)

and extract_binders pattern =
  match pattern.it with
  | M.AnnotP ({ it = M.VarP binder; _ }, _) | M.VarP binder ->
    [ binder ]
  | M.ParP pattern -> extract_binders pattern
  | M.TupP pats -> List.concat_map extract_binders pats
  | _ -> []

and rets ctxt t_opt =
  let (!!) p = !!! Source.no_region p in
  match t_opt with
  | None -> [], []
  | Some t ->
     (match t.note with
     | T.Tup [] -> [], []
     | T.Async (T.Fut, _, _) -> [], []
     | typ ->
        let pred = access_pred ctxt !!(LocalVar (!!"$Res", tr_typ ctxt typ)) typ in
        pred |: [], [(!!"$Res", tr_typ ctxt typ)]
    )

and id id = Source.annotate NoInfo id.it id.at
and id_ref id = Source.annotate NoInfo id.it id.at

and loop_label ctxt loop_label =
  let label_name =
    match String.split_on_char ' ' loop_label.it with
    | ["continue"; name] -> "continue$" ^ name
    | _ -> loop_label.it
  in
  let label_name = fresh_id ("$lbl$" ^ label_name) in
  let ctxt =
    { ctxt with label_to_vpr_label = String_map.add loop_label.it label_name ctxt.label_to_vpr_label }
  in
  id { loop_label with it = label_name }, ctxt

and prjE ctxt at e i =
  let n = List.length (tuple_elem_ts e.note.M.note_typ) in
  ctxt.reqs.tuple_arities := IntSet.add n !(ctxt.reqs.tuple_arities);
  !!! at (FldAcc (exp ctxt e, !!! at (tup_prj_name n i)))

and tr_typ ctxt typ =
  { it = tr_typ' ctxt typ;
    at = Source.no_region;
    note = NoInfo }
and tr_typ' ctxt typ =
  let norm_typ = T.normalize typ in
  match typ, norm_typ with
  | _, T.Prim T.Int -> IntT
  | _, T.Prim T.Nat -> IntT    (* Viper has no native support for Nat, so translate to Int *)
  | _, T.Prim T.Text -> IntT   (* Viper has no native support for Text, so translate to uninterpreted Int values *)
  | _, T.Prim T.Bool -> BoolT
  | _, T.Array _ -> ArrayT     (* Viper arrays are not parameterised by element type *)
  | _, T.Opt   t -> OptionT (tr_typ ctxt t)
  | _, T.Tup  ts ->
    ctxt.reqs.tuple_arities := IntSet.add (List.length ts) !(ctxt.reqs.tuple_arities);
    TupleT (List.map (tr_typ ctxt) ts)
  | _, T.Obj (T.Object, flds) ->
    let record_name = get_record_name ctxt norm_typ in
    ConT (!!! Source.no_region record_name, [])
  | T.Con (con, ts), _ -> ConT (!!! Source.no_region (Mo_types.Cons.name con), List.map (tr_typ ctxt) ts)
  | _, t -> unsupported Source.no_region (Mo_types.Arrange_type.typ t)

and is_mut t =
  match T.normalize t with
  | T.Mut _ -> true
  | _       -> false

and array_elem_t t =
  match T.normalize t with
  | T.Array elem_t -> elem_t
  | t -> failwith "array_elem_t: expected array type"

and tuple_elem_ts t =
  match T.normalize t with
  | T.Tup ts -> ts
  | t -> failwith "tuple_elem_ts: expected tuple type"

(* name of field of typed reference *)
and typed_field ctxt t =
  let t' = tr_typ ctxt (strip_mut_t t) in
  let name = type_field_name t' in
  ctxt.reqs.typed_fields := StrMap.add name t' !(ctxt.reqs.typed_fields);
  name

and type_field_name t =
  match t.it with
  | IntT  -> "$int"
  | BoolT -> "$bool"
  | RefT  -> "$ref"
  | ArrayT -> "$array"
  | TupleT ts  -> "$tuple" ^ string_of_int (List.length ts) ^ String.concat "" (List.map type_field_name ts)
  | OptionT t' -> "$option" ^ type_field_name t'
  | ConT (con, ts) -> "$c_" ^ con.it ^ String.concat "" (List.map type_field_name ts)

and array_size_inv at lhs n =
  !!! at (EqCmpE (sizeE at lhs, n))

and array_acc at ctxt lhs t =
  match T.normalize t with
  | T.Mut _-> arrayAccE at lhs (typed_field ctxt t) FullP
  | _      -> arrayAccE at lhs (typed_field ctxt t) WildcardP

(* Allocate array on the LHS expression.
   Note: array_alloc assumes that the array is uninitialized. Assignment to
   existing arrays must be done via a temporary variable. *)
and array_alloc at ctxt lhs t es : stmt list =
  let (!!) p = !!! at p in
  let init_array = List.mapi (fun i e ->
    FieldAssignS (locE at lhs (intLitE at i) (typed_field ctxt t), exp ctxt e)) es in
  (* InhaleS (!! (FldAcc (locE at lhs (intLitE at i) (typed_field t))) === e)) es in *)
  let reset_perm =
    (match T.normalize t with
     | T.Mut _ -> []
     | _       -> [ExhaleS (array_acc at ctxt lhs t); InhaleS (array_acc at ctxt lhs t)])in
  let stmts = [ InhaleS (array_acc at ctxt lhs (T.Mut t))
              ; InhaleS (array_size_inv at lhs (intLitE at (List.length es)))]
              @ init_array
              @ reset_perm
  in List.map (!!) stmts

and array_init_const at ctxt lhs t x =
  let (!!) p = !!! at p in
  !! (CallE ("$array_init", [lhs; !!(FldE (typed_field ctxt t)); x]))

and array_loc ctxt at e1 e2 t =
  locE at (exp ctxt e1) (exp ctxt e2) (typed_field ctxt t)

and label_expr_alloc ~label_id ~label_type ~label_rhs ~label_note at ctxt lhs : seqn' =
  let ctxt =
    match label_type.it with
    | M.TupT [] -> ctxt
    | _ -> { ctxt with label_to_tmp_var = String_map.add label_id.it lhs ctxt.label_to_tmp_var }
  in
  let wrap_break exp =
    match label_type.it, exp.it with
    | M.TupT [], _ | _, M.BreakE _ ->
    (* Here is a hack: instead of assigning a unit expression
       we'll just treat them as statements. The next Motoko's code

       {[
         let x = label unit_lbl : () { break unit_lbl(v := 42); unreachable_expr; };
       ]}

       would be translated to

       {[
         v := 42;
         goto unit_lbl;
         unreachable_expr;
         label unit_lbl;
       ]} *)
      exp
    | _ -> { exp with it = M.BreakE (label_id, exp) }
  in
  let label_rhs =
    match label_rhs.it with
    | M.BlockE decs ->
      (* In case of block expression we want to wrap the last one
         with [break] if it's not wrapped yet.

         {[
          let x = label lbl : t {
            stmt1;
            stmt2;
            ret_expr
          }
         ]}

         Example above would be mapped to

         {[
          let x = label lbl : t {
            stmt1;
            stmt2;
            break lbl(ret_expr);
          }
         ]} *)
      let decs = map_last decs ~f:(fun dec ->
        match dec.it with
        | M.ExpD exp ->
          let exp = wrap_break exp in
          { dec with it = M.ExpD exp }
        | _ -> dec) in
      { label_rhs with it = M.BlockE decs }
    | _ -> wrap_break label_rhs
  in
  let label = M.{ it = LabelE (label_id, label_type, label_rhs); at; note = label_note } in
  let label_tr = stmt ctxt label in
  label_tr.it
