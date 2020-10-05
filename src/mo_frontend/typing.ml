open Mo_def
open Mo_types
open Mo_values
module Flags = Mo_config.Flags

open Syntax
open Source

module T = Type
module A = Effect
module C = Async_cap


(* Contexts  *)

(* availability, used to mark actor constructors as unavailable in compiled code
   FUTURE: mark unavailable, non-shared variables *)
type avl = Available | Unavailable

type lab_env = T.typ T.Env.t
type ret_env = T.typ option
type val_env  = (T.typ * avl) T.Env.t

let available env = T.Env.map (fun ty -> (ty, Available)) env

let initial_scope =
  { Scope.empty with
    Scope.typ_env = T.Env.singleton T.default_scope_var C.top_cap;
    Scope.con_env = T.ConSet.singleton C.top_cap;
  }


type env =
  { vals : val_env;
    libs : Scope.lib_env;
    typs : Scope.typ_env;
    cons : Scope.con_env;
    objs : Scope.obj_env;
    labs : lab_env;
    rets : ret_env;
    async : C.async_cap;
    in_actor : bool;
    in_prog : bool;
    context : exp' list;
    pre : bool;
    msgs : Diag.msg_store;
    scopes : Source.region T.ConEnv.t;
  }

let env_of_scope msgs scope =
  { vals = available scope.Scope.val_env;
    libs = scope.Scope.lib_env;
    typs = scope.Scope.typ_env;
    cons = scope.Scope.con_env;
    objs = T.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = C.initial_cap();
    in_actor = false;
    in_prog = true;
    context = [];
    pre = false;
    msgs;
    scopes = T.ConEnv.empty;
  }


(* Error bookkeeping *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y

let type_error at text : Diag.message =
  Diag.{sev = Diag.Error; at; cat = "type"; text}

let type_warning at text : Diag.message =
  Diag.{sev = Diag.Warning; at; cat = "type"; text}

let type_info at text : Diag.message =
  Diag.{sev = Diag.Info; at; cat = "type"; text}


let error env at fmt =
  Printf.ksprintf
    (fun s -> Diag.add_msg env.msgs (type_error at s); raise Recover) fmt

let local_error env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_error at s)) fmt

let warn env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_warning at s)) fmt

let info env at fmt =
  Printf.ksprintf (fun s -> Diag.add_msg env.msgs (type_info at s)) fmt

let flag_of_compile_mode mode =
  match mode with
  | Flags.ICMode -> ""
  | Flags.WASIMode -> " and flag -wasi-system-api"
  | Flags.WasmMode -> " and flag -no-system-api"
  | Flags.RefMode -> " and flag -ref-system-api"

let diag_in type_diag modes env at fmt =
  let mode = !Flags.compile_mode in
  if !Flags.compiled && List.mem mode modes then
    begin
      Printf.ksprintf
        (fun s ->
          let s =
            Printf.sprintf "%s\n  (This is a limitation of the current version%s.)"
            s
            (flag_of_compile_mode mode)
          in
          Diag.add_msg env.msgs (type_diag at s)) fmt;
      true
    end
  else false

let error_in modes env at fmt =
  if diag_in type_error modes env at fmt then
    raise Recover

let warn_in modes env at fmt =
  ignore (diag_in type_warning modes env at fmt)

(* Context extension *)

let add_lab env x t = {env with labs = T.Env.add x t env.labs}
let add_val env x t = {env with vals = T.Env.add x (t, Available) env.vals}

let add_typs env xs cs =
  { env with
    typs = List.fold_right2 T.Env.add xs cs env.typs;
    cons = List.fold_right T.ConSet.disjoint_add cs env.cons;
  }

let adjoin env scope =
  { env with
    vals = T.Env.adjoin env.vals (available scope.Scope.val_env);
    libs = T.Env.adjoin env.libs scope.Scope.lib_env;
    typs = T.Env.adjoin env.typs scope.Scope.typ_env;
    cons = T.ConSet.union env.cons scope.Scope.con_env;
    objs = T.Env.adjoin env.objs scope.Scope.obj_env;
  }

let adjoin_vals env ve = {env with vals = T.Env.adjoin env.vals (available ve)}
let adjoin_typs env te ce =
  { env with
    typs = T.Env.adjoin env.typs te;
    cons = T.ConSet.disjoint_union env.cons ce;
  }

let disjoint_union env at fmt env1 env2 =
  try T.Env.disjoint_union env1 env2
  with T.Env.Clash k -> error env at fmt k

(* Types *)

let check_ids env kind member ids = Lib.List.iter_pairs
  (fun x y ->
    if x.it = y.it
    then error env y.at "duplicate %s name %s in %s type" member y.it kind;
    if Hash.hash x.it = Hash.hash y.it
    then error env y.at "%s names %s and %s in %s type have colliding hashes" member x.it y.it kind;
  ) ids

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Const -> fun t -> t
  | Var -> fun t -> T.Mut t

(* System method types *)

let system_funcs = [
    ("preupgrade", T.Func (T.Local, T.Returns, [], [], []));
    ("postupgrade", T.Func (T.Local, T.Returns, [], [], []))
  ]

(* Imports *)

let check_import env at f ri =
  let full_path =
    match !ri with
    | Unresolved -> error env at "unresolved import %s" f
    | LibPath fp -> fp
    | IDLPath (fp, _) -> fp
    | PrimPath -> "@prim"
  in
  match T.Env.find_opt full_path env.libs with
  | Some T.Pre ->
    error env at "cannot infer type of forward import %s" f
  | Some t -> t
  | None -> error env at "imported file %s not loaded" full_path

(* Paths *)

let rec check_obj_path env path : T.obj_sort * (T.field list) =
  match T.promote (check_obj_path' env path) with
  | T.Obj (s, fs) as t ->
    path.note <- t;
    (s, fs)
  | t ->
    error env path.at
      "expected module, object, or actor type, but path expression produces type\n  %s"
      (T.string_of_typ_expand t)

and check_obj_path' env path : T.typ =
  match path.it with
  | IdH id ->
    (match T.Env.find_opt id.it env.vals with
     | Some (T.Pre, _) ->
       error env id.at "cannot infer type of forward variable reference %s" id.it
     | Some (t, Available) -> t
     | Some (t, Unavailable) ->
         error env id.at "unavailable variable %s" id.it
     | None -> error env id.at "unbound variable %s" id.it
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    match T.lookup_val_field id.it fs with
    | T.Pre ->
      error env id.at "cannot infer type of forward field reference %s" id.it
    | t -> t
    | exception Invalid_argument _ ->
      error env id.at "field %s does not exist in type\n  %s"
        id.it (T.string_of_typ_expand (T.Obj (s, fs)))

let rec check_typ_path env path : T.con =
  let c = check_typ_path' env path in
  path.note <- T.Typ c;
  c

and check_typ_path' env path : T.con =
  match path.it with
  | IdH id ->
    (match T.Env.find_opt id.it env.typs with
    | Some c -> c
    | None -> error env id.at "unbound type %s" id.it
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    try T.lookup_typ_field id.it fs with Invalid_argument _ ->
      error env id.at "type field %s does not exist in type\n  %s"
        id.it (T.string_of_typ_expand (T.Obj (s, fs)))

let error_shared env t at fmt =
  match T.find_unshared t with
  | None -> error env at fmt
  | Some t1 ->
    let s = Printf.sprintf "\ntype\n  %s\nis or contains non-shared type\n  %s"
      (T.string_of_typ_expand t) (T.string_of_typ_expand t1) in
    Printf.ksprintf (fun s1 -> Diag.add_msg env.msgs (type_error at (s1^s)); raise Recover) fmt

let as_domT t =
  match t.Source.it with
  | TupT tis -> List.map snd tis
  | _ -> [t]

let as_codomT sort t =
  match sort, t.Source.it with
  | T.Shared _,  AsyncT (_, t1) ->
    T.Promises, as_domT t1
  | _ -> T.Returns, as_domT t

let check_shared_return env at sort c ts =
  match sort, c, ts with
  | T.Shared _, T.Promises,  _ -> ()
  | T.Shared T.Write, T.Returns, [] -> ()
  | T.Shared T.Write, _, _ -> error env at "shared function must have syntactic return type `()` or `async <typ>`"
  | T.Shared T.Query, _, _ -> error env at "shared query function must have syntactic return type `async <typ>`"
  | _ -> ()

let region_of_scope env typ =
  match T.normalize typ with
  | T.Con(c,_) ->
    T.ConEnv.find_opt c env.scopes
  | _ -> None

let string_of_region r =
  let open Source in
  let { left; right } = r in
  let basename = if left.file = "" then "" else Filename.basename left.file in
  Source.string_of_region
    { left =  { left with file = basename };
      right = { right with file = basename } }

let associated_region env typ at =
  match region_of_scope env typ with
  | Some r ->
    Printf.sprintf "\n  scope %s is %s" (T.string_of_typ_expand typ) (string_of_region r);
  | None ->
    if T.eq typ (T.Con(C.top_cap,[])) then
      Printf.sprintf "\n  scope %s is the global scope" (T.string_of_typ_expand typ)
    else ""

let scope_info env typ at =
  match region_of_scope env typ with
  | Some r ->
    let s = {left = r.left; right = r.left} in
    let l = { r.right with column = r.right.column - 1 } in
    let e = {left = l; right = l} in
    info env s "start of scope %s mentioned in error at %s"
      (T.string_of_typ_expand typ) (string_of_region at);
    info env e "end of scope %s mentioned in error at %s"
      (T.string_of_typ_expand typ) (string_of_region at);
  | None -> ()

let rec infer_async_cap env sort cs tbs at =
  match sort, cs, tbs with
  | (T.Shared T.Write | T.Local) , c::_,  { T.sort = T.Scope; _ }::_ ->
    { env with typs = T.Env.add T.default_scope_var c env.typs;
               scopes = T.ConEnv.add c at env.scopes;
               async = C.AsyncCap c }
  | (T.Shared T.Query) , c::_,  { T.sort = T.Scope; _ }::_ ->
    { env with typs = T.Env.add T.default_scope_var c env.typs;
               scopes = T.ConEnv.add c at env.scopes;
               async = C.QueryCap c }
  | T.Shared _, _, _ -> assert false (* impossible given sugaring *)
  | _ -> { env with async = C.NullCap }

and check_AsyncCap env s at : T.typ * (T.con -> C.async_cap) =
   match env.async with
   | C.AwaitCap c
   | C.AsyncCap c -> T.Con(c, []), fun c' -> C.AwaitCap c'
   | C.QueryCap c -> T.Con(c, []), fun _c' -> C.ErrorCap
   | C.ErrorCap
   | C.NullCap -> error env at "misplaced %s; try enclosing in an async function" s

and check_AwaitCap env s at =
   match env.async with
   | C.AwaitCap c -> T.Con(c, [])
   | C.AsyncCap _
   | C.QueryCap _ ->
     error env at "misplaced %s; try enclosing in an async expression" s
   | C.ErrorCap
   | C.NullCap -> error env at "misplaced %s" s

and check_ErrorCap env s at =
   match env.async with
   | C.AwaitCap c -> ()
   | C.ErrorCap -> ()
   | C.AsyncCap _
   | C.QueryCap _ ->
     error env at "misplaced %s; try enclosing in an async expression or query function" s
   | C.NullCap -> error env at "misplaced %s" s

and scope_of_env env =
  match env.async with
   | C.AsyncCap c
   | C.QueryCap c
   | C.AwaitCap c -> Some (T.Con(c,[]))
   | C.ErrorCap -> None
   | C.NullCap -> None

and check_typ env (typ:typ) : T.typ =
  let t = check_typ' env typ in
  typ.note <- t;
  t

and check_typ' env typ : T.typ =
  match typ.it with
  | PathT (path, typs) ->
    let c = check_typ_path env path in
    let ts = List.map (check_typ env) typs in
    let T.Def (tbs, _) | T.Abs (tbs, _) = Con.kind c in
    let tbs' = List.map (fun tb -> { tb with T.bound = T.open_ ts tb.T.bound }) tbs in
    check_typ_bounds env tbs' ts (List.map (fun typ -> typ.at) typs) typ.at;
    T.Con (c, ts)
  | PrimT "Any" -> T.Any
  | PrimT "None" -> T.Non
  | PrimT s ->
    (try T.Prim (T.prim s) with Invalid_argument _ ->
      error env typ.at "unknown primitive type"
    )
  | ArrayT (mut, typ) ->
    let t = check_typ env typ in
    T.Array (infer_mut mut t)
  | TupT typs ->
    T.Tup (List.map (fun (_, t) -> check_typ env t) typs)
  | FuncT (sort, binds, typ1, typ2) ->
    let cs, tbs, te, ce = check_typ_binds env binds in
    let env' = infer_async_cap (adjoin_typs env te ce) sort.it cs tbs typ.at in
    let typs1 = as_domT typ1 in
    let c, typs2 = as_codomT sort.it typ2 in
    let ts1 = List.map (check_typ env') typs1 in
    let ts2 = List.map (check_typ env') typs2 in
    check_shared_return env typ2.at sort.it c ts2;
    if Type.is_shared_sort sort.it then
    if not env.pre then begin
      let t1 = T.seq ts1 in
      if not (T.shared t1) then
        error_shared env t1 typ1.at "shared function has non-shared parameter type\n  %s" (T.string_of_typ_expand t1);
      List.iter (fun t ->
        if not (T.shared t) then
          error_shared env t typ.at
            "shared function has non-shared return type\n  %s"
            (T.string_of_typ_expand t);
      ) ts2;
      match c, ts2 with
      | T.Returns, [] when sort.it = T.Shared T.Write -> ()
      | T.Promises, _ -> ()
      | _ ->
        error env typ2.at
          "shared function has non-async result type\n  %s"
          (T.string_of_typ_expand (T.seq ts2))
      end;
    T.Func (sort.it, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | OptT typ ->
    T.Opt (check_typ env typ)
  | VariantT tags ->
    check_ids env "variant" "tag"
      (List.map (fun (tag : typ_tag) -> tag.it.tag) tags);
    let fs = List.map (check_typ_tag env) tags in
    T.Variant (List.sort T.compare_field fs)
  | AsyncT (typ0, typ) ->
    let t0 = check_typ env typ0 in
    let t = check_typ env typ in
    if not env.pre && not (T.shared t) then
      error_shared env t typ.at "async has non-shared content type\n  %s"
        (T.string_of_typ_expand t);
    T.Async (t0, t)
  | ObjT (sort, fields) ->
    check_ids env "object" "field"
      (List.map (fun (field : typ_field) -> field.it.id) fields);
    let fs = List.map (check_typ_field env sort.it) fields in
    T.Obj (sort.it, List.sort T.compare_field fs)
  | ParT typ ->
    check_typ env typ
  | NamedT (_, typ) ->
    check_typ env typ

and check_typ_field env s typ_field : T.field =
  let {id; mut; typ} = typ_field.it in
  let t = infer_mut mut (check_typ env typ) in
  if not env.pre && s = T.Actor then begin
    if not (T.is_shared_func t) then
      error env typ.at "actor field %s must have shared function type, but has type\n  %s"
        id.it (T.string_of_typ_expand t)
  end;
  T.{lab = id.it; typ = t}

and check_typ_tag env typ_tag =
  let {tag; typ} = typ_tag.it in
  let t = check_typ env typ in
  T.{lab = tag.it; typ = t}

and check_typ_binds_acyclic env typ_binds cs ts  =
  let n = List.length cs in
  let ce = List.fold_right2 T.ConEnv.add cs ts T.ConEnv.empty in
  let chase typ_bind c =
    let rec chase i ts c' =
      if i > n then
        error env typ_bind.at "type parameter %s has cyclic bounds %s"
          (T.string_of_con c)
          (String.concat " <: " (List.map T.string_of_typ ts)) (List.rev ts)
      else
        match T.ConEnv.find_opt c' ce with
        | None -> ()
        | Some t ->
          (match T.normalize t with
           | T.Con (c'', []) as t' ->
             chase (i+1) (t'::ts) c''
           | _ -> ())
    in chase 0 [] c
  in List.iter2 chase typ_binds cs

and check_typ_bind_sorts env tbs =
  (* assert, don't error, since this should be a syntactic invariant of parsing *)
  List.iteri (fun i tb -> assert (i = 0 || (tb.T.sort = T.Type))) tbs;

and check_typ_binds env typ_binds : T.con list * T.bind list * Scope.typ_env * Scope.con_env =
  let xs = List.map (fun typ_bind -> typ_bind.it.var.it) typ_binds in
  let cs =
    List.map2 (fun x tb ->
      match tb.note with
      | Some c -> c
      | None -> Con.fresh x (T.Abs ([], T.Pre))) xs typ_binds in
  let te = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.it.var in
      if T.Env.mem id.it te then
        error env id.at "duplicate type name %s in type parameter list" id.it;
      T.Env.add id.it c te
    ) T.Env.empty typ_binds cs in
  let pre_env' = add_typs {env with pre = true} xs cs  in
  let tbs = List.map (fun typ_bind ->
    { T.var = typ_bind.it.var.it;
      T.sort = typ_bind.it.sort.it;
      T.bound = check_typ pre_env' typ_bind.it.bound }) typ_binds
  in
  check_typ_bind_sorts env tbs;
  let ts = List.map (fun tb -> tb.T.bound) tbs in
  check_typ_binds_acyclic env typ_binds cs ts;
  let ks = List.map (fun t -> T.Abs ([], t)) ts in
  List.iter2 (fun c k ->
    match Con.kind c with
    | T.Abs (_, T.Pre) -> T.set_kind c k
    | k' -> assert (T.eq_kind k k')
  ) cs ks;
  let env' = add_typs env xs cs in
  let _ = List.map (fun typ_bind -> check_typ env' typ_bind.it.bound) typ_binds in
  List.iter2 (fun typ_bind c -> typ_bind.note <- Some c) typ_binds cs;
  cs, tbs, te, T.ConSet.of_list cs

and check_typ_bind env typ_bind : T.con * T.bind * Scope.typ_env * Scope.con_env =
  match check_typ_binds env [typ_bind] with
  | [c], [tb], te, cs -> c, tb, te, cs
  | _ -> assert false

and check_typ_bounds env (tbs : T.bind list) (ts : T.typ list) ats at =
  let pars = List.length tbs in
  let args = List.length ts in
  if pars > args then
    error env at "too few type arguments";
  if pars < args then
    error env at "too many type arguments";
  let rec go tbs' ts' ats' =
    match tbs', ts', ats' with
    | tb::tbs', t::ts', at'::ats' ->
      if not env.pre then
        let u = T.open_ ts tb.T.bound in
        if not (T.sub t u) then
          local_error env at'
            "type argument\n  %s\ndoes not match parameter bound\n  %s"
            (T.string_of_typ_expand t)
            (T.string_of_typ_expand u);
        go tbs' ts' ats'
    | [], [], [] -> ()
    | _  -> assert false
  in go tbs ts ats

and infer_inst env tbs typs at =
  let ts = List.map (check_typ env) typs in
  let ats = List.map (fun typ -> typ.at) typs in
  match tbs,typs with
  | {T.bound; sort = T.Scope; _}::tbs', typs' ->
    assert (List.for_all (fun tb -> tb.T.sort = T.Type) tbs');
    (match env.async with
     | C.ErrorCap
     | C.QueryCap _
     | C.NullCap -> error env at "send capability required, but not available (need an enclosing async expression or function body)"
     | C.AwaitCap c
     | C.AsyncCap c ->
      (T.Con(c,[])::ts, at::ats)
    )
  | tbs', typs' ->
    assert (List.for_all (fun tb -> tb.T.sort = T.Type) tbs');
    ts, ats

and check_inst_bounds env tbs inst at =
  let ts, ats = infer_inst env tbs inst at in
  check_typ_bounds env tbs ts ats at;
  ts

(* Literals *)

let check_lit_val env t of_string at s =
  try of_string s with _ ->
    error env at "literal out of range for type %s"
      (T.string_of_typ (T.Prim t))

let check_nat env = check_lit_val env T.Nat Value.Nat.of_string
let check_nat8 env = check_lit_val env T.Nat8 Value.Nat8.of_string
let check_nat16 env = check_lit_val env T.Nat16 Value.Nat16.of_string
let check_nat32 env = check_lit_val env T.Nat32 Value.Nat32.of_string
let check_nat64 env = check_lit_val env T.Nat64 Value.Nat64.of_string
let check_int env = check_lit_val env T.Int Value.Int.of_string
let check_int8 env = check_lit_val env T.Int8 Value.Int_8.of_string
let check_int16 env = check_lit_val env T.Int16 Value.Int_16.of_string
let check_int32 env = check_lit_val env T.Int32 Value.Int_32.of_string
let check_int64 env = check_lit_val env T.Int64 Value.Int_64.of_string
let check_word8 env = check_lit_val env T.Word8 Value.Word8.of_string
let check_word16 env = check_lit_val env T.Word16 Value.Word16.of_string
let check_word32 env = check_lit_val env T.Word32 Value.Word32.of_string
let check_word64 env = check_lit_val env T.Word64 Value.Word64.of_string
let check_float env = check_lit_val env T.Float Value.Float.of_string

let check_text env at s =
  (try ignore (Wasm.Utf8.decode s)
   with Wasm.Utf8.Utf8 -> local_error env at "string literal \"%s\": is not valid utf8" (String.escaped s));
  s


let infer_lit env lit at : T.prim =
  match !lit with
  | NullLit -> T.Null
  | BoolLit _ -> T.Bool
  | NatLit _ -> T.Nat
  | Nat8Lit _ -> T.Nat8
  | Nat16Lit _ -> T.Nat16
  | Nat32Lit _ -> T.Nat32
  | Nat64Lit _ -> T.Nat64
  | IntLit _ -> T.Int
  | Int8Lit _ -> T.Int8
  | Int16Lit _ -> T.Int16
  | Int32Lit _ -> T.Int32
  | Int64Lit _ -> T.Int64
  | Word8Lit _ -> T.Word8
  | Word16Lit _ -> T.Word16
  | Word32Lit _ -> T.Word32
  | Word64Lit _ -> T.Word64
  | FloatLit _ -> T.Float
  | CharLit _ -> T.Char
  | TextLit _ -> T.Text
  | BlobLit _ -> T.Blob
  | PreLit (s, T.Nat) ->
    lit := NatLit (check_nat env at s); (* default *)
    T.Nat
  | PreLit (s, T.Int) ->
    lit := IntLit (check_int env at s); (* default *)
    T.Int
  | PreLit (s, T.Float) ->
    lit := FloatLit (check_float env at s); (* default *)
    T.Float
  | PreLit (s, T.Text) ->
    lit := TextLit (check_text env at s); (* default *)
    T.Text
  | PreLit _ ->
    assert false

let check_lit env t lit at =
  match t, !lit with
  | T.Prim T.Nat, PreLit (s, T.Nat) ->
    lit := NatLit (check_nat env at s)
  | T.Prim T.Nat8, PreLit (s, T.Nat) ->
    lit := Nat8Lit (check_nat8 env at s)
  | T.Prim T.Nat16, PreLit (s, T.Nat) ->
    lit := Nat16Lit (check_nat16 env at s)
  | T.Prim T.Nat32, PreLit (s, T.Nat) ->
    lit := Nat32Lit (check_nat32 env at s)
  | T.Prim T.Nat64, PreLit (s, T.Nat) ->
    lit := Nat64Lit (check_nat64 env at s)
  | T.Prim T.Int, PreLit (s, (T.Nat | T.Int)) ->
    lit := IntLit (check_int env at s)
  | T.Prim T.Int8, PreLit (s, (T.Nat | T.Int)) ->
    lit := Int8Lit (check_int8 env at s)
  | T.Prim T.Int16, PreLit (s, (T.Nat | T.Int)) ->
    lit := Int16Lit (check_int16 env at s)
  | T.Prim T.Int32, PreLit (s, (T.Nat | T.Int)) ->
    lit := Int32Lit (check_int32 env at s)
  | T.Prim T.Int64, PreLit (s, (T.Nat | T.Int)) ->
    lit := Int64Lit (check_int64 env at s)
  | T.Prim T.Word8, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word8Lit (check_word8 env at s)
  | T.Prim T.Word16, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word16Lit (check_word16 env at s)
  | T.Prim T.Word32, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word32Lit (check_word32 env at s)
  | T.Prim T.Word64, PreLit (s, (T.Nat | T.Int)) ->
    lit := Word64Lit (check_word64 env at s)
  | T.Prim T.Float, PreLit (s, (T.Nat | T.Int | T.Float)) ->
    lit := FloatLit (check_float env at s)
  | T.Prim T.Blob, PreLit (s, T.Text) ->
    lit := BlobLit s
  | t, _ ->
    let t' = T.Prim (infer_lit env lit at) in
    if not (T.sub t' t) then
      error env at
        "literal of type\n  %s\ndoes not have expected type\n  %s"
        (T.string_of_typ t') (T.string_of_typ_expand t)

(* Coercions *)

let array_obj t =
  let open T in
  let immut t =
    [ {lab = "get";  typ = Func (Local, Returns, [], [Prim Nat], [t])};
      {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat])};
      {lab = "keys"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat)])};
      {lab = "vals"; typ = Func (Local, Returns, [], [], [iter_obj t])};
    ] in
  let mut t = immut t @
    [ {lab = "put"; typ = Func (Local, Returns, [], [Prim Nat; t], [])} ] in
  Object,
  List.sort compare_field (match t with Mut t' -> mut t' | t -> immut t)

let blob_obj () =
  let open T in
  Object,
  [ {lab = "bytes"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Word8)])};
    {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat])};
  ]

let text_obj () =
  let open T in
  Object,
  [ {lab = "chars"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Char)])};
    {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat])};
  ]


(* Expressions *)

let rec infer_exp env exp : T.typ =
  infer_exp' T.as_immut env exp

and infer_exp_mut env exp : T.typ =
  infer_exp' Fun.id env exp

and infer_exp_promote env exp : T.typ =
  let t = infer_exp env exp in
  let t' = T.promote t in
  if t' = T.Pre then
    error env exp.at
      "cannot infer type of expression while trying to infer surrounding class type,\nbecause its type is a forward reference to type\n  %s"
      (T.string_of_typ_expand  t);
  t'

and infer_exp' f env exp : T.typ =
  assert (exp.note.note_typ = T.Pre);
  let t = infer_exp'' env exp in
  assert (t <> T.Pre);
  let t' = f t in
  if not env.pre then begin
    assert (T.normalize t' <> T.Pre);
    let e = A.infer_effect_exp exp in
    exp.note <- {note_typ = T.normalize t'; note_eff = e}
  end;
  t'

and infer_exp'' env exp : T.typ =
  let in_prog = env.in_prog in
  let in_actor = env.in_actor in
  let env = {env with in_actor = false; in_prog = false; context = exp.it::env.context} in
  match exp.it with
  | PrimE _ ->
    error env exp.at "cannot infer type of primitive"
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with
    | Some (T.Pre, _) ->
      error env id.at "cannot infer type of forward variable %s" id.it;
    | Some (t, Unavailable) ->
      if !Flags.compiled then
        error env id.at "variable %s is in scope but not available in compiled code" id.it
      else t
    | Some (t, Available) -> t
    | None ->
      error env id.at "unbound variable %s" id.it
    )
  | LitE lit ->
    T.Prim (infer_lit env lit exp.at)
  | ActorUrlE exp' ->
    if not env.pre then check_exp env T.text exp';
    error env exp.at "no type can be inferred for actor reference"
  | UnE (ot, op, exp1) ->
    let t1 = infer_exp_promote env exp1 in
    let t = Operator.type_unop op t1 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_unop op t) then
        error env exp.at "operator is not defined for operand type\n  %s"
          (T.string_of_typ_expand t);
      ot := t;
    end;
    t
  | BinE (ot, exp1, op, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    let t2 = infer_exp_promote env exp2 in
    let t = Operator.type_binop op (T.lub t1 t2) in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_binop op t) then
        error env exp.at
          "operator not defined for operand types\n  %s\nand\n  %s"
          (T.string_of_typ_expand t1)
          (T.string_of_typ_expand t2);
      ot := t
    end;
    t
  | RelE (ot, exp1, op, exp2) ->
    let t1 = T.normalize (infer_exp env exp1) in
    let t2 = T.normalize (infer_exp env exp2) in
    let t = Operator.type_relop op (T.lub (T.promote t1) (T.promote t2)) in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_relop op t) then
        error env exp.at
          "operator not defined for operand types\n  %s\nand\n  %s"
          (T.string_of_typ_expand t1)
          (T.string_of_typ_expand t2);
      if not (T.eq t t1 || T.eq t t2) then
        if T.eq t1 t2 then
          warn env exp.at
            "comparing abstract type\n  %s\nto itself at supertype\n  %s"
            (T.string_of_typ_expand t1)
            (T.string_of_typ_expand t)
        else
          warn env exp.at
            "comparing incompatible types\n  %s\nand\n  %s\nat common supertype\n  %s"
            (T.string_of_typ_expand t1)
            (T.string_of_typ_expand t2)
            (T.string_of_typ_expand t);
      ot := t;
    end;
    T.bool
  | ShowE (ot, exp1) ->
    let t = infer_exp_promote env exp1 in
    if not env.pre then begin
      if not (Show.can_show t) then
        error env exp.at "show is not defined for operand type\n  %s"
          (T.string_of_typ_expand t);
      ot := t
    end;
    T.text
  | TupE exps ->
    let ts = List.map (infer_exp env) exps in
    T.Tup ts
  | OptE exp1 ->
    let t1 = infer_exp env exp1 in
    T.Opt t1
  | TagE (id, exp1) ->
    T.Variant [T.{lab = id.it; typ = infer_exp env exp1}]
  | ProjE (exp1, n) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let ts = T.as_tup_sub n t1 in
      match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error env exp.at "tuple projection %n is out of bounds for type\n  %s"
          n (T.string_of_typ_expand t1)
    with Invalid_argument _ ->
      error env exp1.at
        "expected tuple type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | ObjE (obj_sort, fields) ->
    if obj_sort.it = T.Actor then begin
      error_in [Flags.WASIMode; Flags.WasmMode] env exp.at "actors are not supported";
      if not in_prog then
        error_in [Flags.ICMode; Flags.RefMode] env exp.at "non-toplevel actor; an actor can only be declared at the toplevel of a program"
    end;
    let env' = if obj_sort.it = T.Actor then {env with async = C.NullCap; in_actor = true} else env in
    infer_obj env' obj_sort.it fields exp.at
  | DotE (exp1, id) ->
    let t1 = infer_exp_promote env exp1 in
    let _s, tfs =
      try T.as_obj_sub [id.it] t1 with Invalid_argument _ ->
      try array_obj (T.as_array_sub t1) with Invalid_argument _ ->
      try blob_obj (T.as_prim_sub T.Blob t1) with Invalid_argument _ ->
      try text_obj (T.as_prim_sub T.Text t1) with Invalid_argument _ ->
        error env exp1.at
          "expected object type, but expression produces type\n  %s"
          (T.string_of_typ_expand t1)
    in
    (match T.lookup_val_field id.it tfs with
    | T.Pre ->
      error env exp.at "cannot infer type of forward field reference %s"
        id.it
    | t -> t
    | exception Invalid_argument _ ->
      error env exp1.at "field %s does not exist in type\n  %s"
        id.it (T.string_of_typ_expand t1)
    )
  | AssignE (exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_mut env exp1 in
      try
        let t2 = T.as_mut t1 in
        check_exp env t2 exp2
      with Invalid_argument _ ->
        error env exp.at "expected mutable assignment target";
    end;
    T.unit
  | ArrayE (mut, exps) ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.fold_left T.lub T.Non ts in
    if not env.pre && inconsistent t1 ts then
      warn env exp.at
        "this array has type %s because elements have inconsistent types"
        (T.string_of_typ (T.Array t1));
    T.Array (match mut.it with Const -> t1 | Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let t = T.as_array_sub t1 in
      if not env.pre then check_exp env T.nat exp2;
      t
    with Invalid_argument _ ->
      error env exp1.at
        "expected array type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | FuncE (_, shared_pat, typ_binds, pat, typ_opt, _sugar, exp1) ->
    if not env.pre && not in_actor && T.is_shared_sort shared_pat.it then begin
      error_in [Flags.WASIMode; Flags.WasmMode] env exp1.at "shared functions are not supported";
      if not in_actor then
        error_in [Flags.ICMode; Flags.RefMode] env exp1.at "a shared function is only allowed as a public field of an actor";
    end;
    let typ = match typ_opt with
      | Some typ -> typ
      | None -> {it = TupT []; at = no_region; note = T.Pre}
    in
    let sort, ve = check_shared_pat env shared_pat in
    let cs, tbs, te, ce = check_typ_binds env typ_binds in
    let c, ts2 = as_codomT sort typ in
    check_shared_return env typ.at sort c ts2;
    let env' = infer_async_cap (adjoin_typs env te ce) sort cs tbs exp.at in
    let t1, ve1 = infer_pat_exhaustive (if T.is_shared_sort sort then local_error else warn) env' pat in
    let ve2 = T.Env.adjoin ve ve1 in
    let ts2 = List.map (check_typ env') ts2 in
    let codom = T.codom c (fun () -> T.Con(List.hd cs,[])) ts2 in
    if not env.pre then begin
      let env'' =
        { env' with
          labs = T.Env.empty;
          rets = Some codom;
          (* async = None; *) }
      in
      check_exp (adjoin_vals env'' ve2) codom exp1;
      if Type.is_shared_sort sort then begin
        if not (T.shared t1) then
          error_shared env t1 pat.at
            "shared function has non-shared parameter type\n  %s"
            (T.string_of_typ_expand t1);
        List.iter (fun t ->
          if not (T.shared t) then
            error_shared env t typ.at
              "shared function has non-shared return type\n  %s"
              (T.string_of_typ_expand t);
        ) ts2;
        match c, ts2 with
        | T.Returns, [] when sort = T.Shared T.Write ->
          if not (is_IgnoreAsync exp1) then
            error env exp1.at
              "shared function with () result type has unexpected body:\n  the body must either be of sugared form '{ ... }' \n  or explicit form '= ignore ((async ...) : async ())'"
        | T.Promises, _ ->
          if not (is_Async exp1) then
            error env exp1.at
              "shared function with async result type has non-async body"
        | _ ->
          error env typ.at "shared function has non-async result type\n  %s"
            (T.string_of_typ_expand codom)
      end
    end;
    let ts1 = match pat.it with TupP _ -> T.seq_of_tup t1 | _ -> [t1] in
    T.Func (sort, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | CallE (exp1, inst, exp2) ->
    infer_call env exp1 inst exp2 exp.at None
  | BlockE decs ->
    let t, scope = infer_block env decs exp.at in
    (try T.avoid scope.Scope.con_env t with T.Unavoidable c ->
      error env exp.at
        "local class type %s is contained in inferred block type\n  %s"
        (Con.to_string c)
        (T.string_of_typ_expand t)
    )
  | NotE exp1 ->
    if not env.pre then check_exp env T.bool exp1;
    T.bool
  | AndE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.bool exp2
    end;
    T.bool
  | OrE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.bool exp2
    end;
    T.bool
  | IfE (exp1, exp2, exp3) ->
    if not env.pre then check_exp env T.bool exp1;
    let t2 = infer_exp env exp2 in
    let t3 = infer_exp env exp3 in
    let t = T.lub t2 t3 in
    if not env.pre && inconsistent t [t2; t3] then
      warn env exp.at
        "this if has type %s because branches have inconsistent types,\ntrue produces\n  %s\nfalse produces\n  %s"
        (T.string_of_typ t)
        (T.string_of_typ_expand t2)
        (T.string_of_typ_expand t3);
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
    let t = infer_cases env t1 T.Non cases in
    if not env.pre then begin
      match Coverage.check_cases cases t1 with
      | [] -> ()
      | ss ->
        warn env exp.at
          "the cases in this switch over type\n  %s\ndo not cover value\n  %s"
          (Type.string_of_typ_expand t1)
          (String.concat " or\n  " ss)
    end;
    t
  | TryE (exp1, cases) ->
    check_ErrorCap env "try" exp.at;
    let t1 = infer_exp env exp1 in
    let t2 = infer_cases env T.catch T.Non cases in
    if not env.pre then begin
      match Coverage.check_cases cases T.catch with
      | [] -> ()
      | ss ->
        warn env exp.at
          "the catches in this try do not cover error value\n  %s"
          (String.concat " or\n  " ss)
    end;
    T.lub t1 t2
  | WhileE (exp1, exp2) ->
    if not env.pre then begin
      check_exp env T.bool exp1;
      check_exp env T.unit exp2
    end;
    T.unit
  | LoopE (exp1, None) ->
    if not env.pre then begin
      check_exp env T.unit exp1
    end;
    T.Non
  | LoopE (exp1, Some exp2) ->
    if not env.pre then begin
      check_exp env T.unit exp1;
      check_exp env T.bool exp2
    end;
    T.unit
  | ForE (pat, exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_promote env exp1 in
      (try
        let _, tfs = T.as_obj_sub ["next"] t1 in
        let t = T.lookup_val_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub t in
        if not (T.sub T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub t2 in
        let ve = check_pat_exhaustive warn env t2' pat in
        check_exp (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ | Not_found ->
        local_error env exp1.at
          "expected iterable type, but expression has type\n  %s"
          (T.string_of_typ_expand t1)
      );
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ env typ in
    if not env.pre then check_exp (add_lab env id.it t) t exp1;
    t
  | DebugE exp1 ->
    if not env.pre then check_exp env T.unit exp1;
    T.unit
  | BreakE (id, exp1) ->
    (match T.Env.find_opt id.it env.labs with
    | Some t ->
      if not env.pre then check_exp env t exp1
    | None ->
      let name =
        match String.split_on_char ' ' id.it with
        | ["continue"; name] -> name
        | _ -> id.it
      in local_error env id.at "unbound label %s" name
    );
    T.Non
  | RetE exp1 ->
    if not env.pre then begin
      match env.rets with
      | Some T.Pre ->
        local_error env exp.at "cannot infer return type"
      | Some t ->
        check_exp env t exp1
      | None ->
        local_error env exp.at "misplaced return"
    end;
    T.Non
  | ThrowE exp1 ->
    check_ErrorCap env "throw" exp.at;
    if not env.pre then check_exp env T.throw exp1;
    T.Non
  | AsyncE (typ_bind, exp1) ->
    error_in [Flags.WASIMode; Flags.WasmMode] env exp1.at "async expressions are not supported";
    let t1, next_cap = check_AsyncCap env "async expression" exp.at in
    let c, tb, ce, cs = check_typ_bind env typ_bind in
    let ce_scope = T.Env.add T.default_scope_var c ce in (* pun scope var with c *)
    let env' =
      {(adjoin_typs env ce_scope cs) with
        labs = T.Env.empty;
        rets = Some T.Pre;
        async = next_cap c;
        scopes = T.ConEnv.add c exp.at env.scopes } in
    let t = infer_exp env' exp1 in
    let t' = T.open_ [t1] (T.close [c] t)  in
    if not (T.shared t') then
      error_shared env t' exp1.at "async type has non-shared content type\n  %s"
        (T.string_of_typ_expand t');
    T.Async (t1, t')
  | AwaitE exp1 ->
    let t0 = check_AwaitCap env "await" exp.at in
    let t1 = infer_exp_promote env exp1 in
    (try
       let (t2, t3) = T.as_async_sub t0 t1 in
       if not (T.eq t0 t2) then begin
         local_error env exp1.at "ill-scoped await: expected async type from current scope %s, found async type from other scope %s%s%s"
           (T.string_of_typ_expand t0)
           (T.string_of_typ_expand t2)
           (associated_region env t0 exp.at)
           (associated_region env t2 exp.at);
         scope_info env t0 exp.at;
         scope_info env t2 exp.at;
       end;
       t3
    with Invalid_argument _ ->
      error env exp1.at "expected async type, but expression has type\n  %s"
        (T.string_of_typ_expand t1)
    )
  | AssertE exp1 ->
    if not env.pre then check_exp env T.bool exp1;
    T.unit
  | AnnotE (exp1, typ) ->
    let t = check_typ env typ in
    if not env.pre then check_exp env t exp1;
    t
  | ImportE (f, ri) ->
    check_import env exp.at f ri

and check_exp env t exp =
  assert (not env.pre);
  assert (exp.note.note_typ = T.Pre);
  assert (t <> T.Pre);
  let t' = check_exp' env (T.normalize t) exp in
  let e = A.infer_effect_exp exp in
  exp.note <- {note_typ = t'; note_eff = e}

and check_exp' env0 t exp : T.typ =
  let env = {env0 with in_prog = false; in_actor = false; context = exp.it :: env0.context } in
  match exp.it, t with
  | PrimE s, T.Func _ ->
    t
  | LitE lit, _ ->
    check_lit env t lit exp.at;
    t
  | ActorUrlE exp', t' ->
    check_exp env T.text exp';
    begin match T.normalize t' with
    | T.(Obj (Actor, _)) -> t'
    | _ -> error env exp.at "actor reference must have an actor type"
    end
  | UnE (ot, op, exp1), _ when Operator.has_unop op t ->
    ot := t;
    check_exp env t exp1;
    t
  | BinE (ot, exp1, op, exp2), _ when Operator.has_binop op t ->
    ot := t;
    check_exp env t exp1;
    check_exp env t exp2;
    t
  | TupE exps, T.Tup ts when List.length exps = List.length ts ->
    List.iter2 (check_exp env) ts exps;
    t
  | OptE exp1, _ when T.is_opt t ->
    check_exp env (T.as_opt t) exp1;
    t
  | ArrayE (mut, exps), T.Array t' ->
    if (mut.it = Var) <> T.is_mut t' then
      local_error env exp.at
        "%smutable array expression cannot produce expected type\n  %s"
        (if mut.it = Const then "im" else "")
        (T.string_of_typ_expand (T.Array t'));
    List.iter (check_exp env (T.as_immut t')) exps;
    t
  | AsyncE (tb, exp1), T.Async (t1', t') ->
    error_in [Flags.WASIMode; Flags.WasmMode] env exp1.at "async expressions are not supported";
    let t1, next_cap = check_AsyncCap env "async expression" exp.at in
    if not (T.eq t1 t1') then begin
      local_error env exp.at "async at scope\n  %s\ncannot produce expected scope\n  %s%s%s"
        (T.string_of_typ_expand t1)
        (T.string_of_typ_expand t1')
        (associated_region env t1 exp.at)
        (associated_region env t1' exp.at);
      scope_info env t1 exp.at;
      scope_info env t1' exp.at
    end;
    let c, tb, ce, cs = check_typ_bind env tb in
    let ce_scope = T.Env.add T.default_scope_var c ce in (* pun scope var with c *)
    let env' =
      {(adjoin_typs env ce_scope cs) with
        labs = T.Env.empty;
        rets = Some t';
        async = next_cap c;
        scopes = T.ConEnv.add c exp.at env.scopes;
      } in
    check_exp env' t' exp1;
    t
  | BlockE decs, _ ->
    ignore (check_block env t decs exp.at);
    t
  | IfE (exp1, exp2, exp3), _ ->
    check_exp env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3;
    t
  | SwitchE (exp1, cases), _ ->
    let t1 = infer_exp_promote env exp1 in
    check_cases env t1 t cases;
    (match Coverage.check_cases cases t1 with
    | [] -> ()
    | ss ->
      warn env exp.at
        "the cases in this switch over type\n  %s\ndo not cover value\n  %s"
        (Type.string_of_typ_expand t1)
        (String.concat " or\n  " ss)
    );
    t
  | TryE (exp1, cases), _ ->
    check_ErrorCap env "try" exp.at;
    check_exp env t exp1;
    check_cases env T.catch t cases;
    (match Coverage.check_cases cases T.catch with
    | [] -> ()
    | ss ->
      warn env exp.at
        "the catches in this try do not cover value\n  %s"
        (String.concat " or\n  " ss)
    );
    t
  (* TODO: allow shared with one scope par *)
  | FuncE (_, shared_pat,  [], pat, typ_opt, _sugar, exp), T.Func (s, c, [], ts1, ts2) ->
    let sort, ve = check_shared_pat env shared_pat in
    if not env.pre && not env0.in_actor && T.is_shared_sort sort then
      error_in [Flags.ICMode; Flags.RefMode] env exp.at "a shared function is only allowed as a public field of an actor";
    let ve1 = check_pat_exhaustive (if T.is_shared_sort sort then local_error else warn) env (T.seq ts1) pat in
    let ve2 = T.Env.adjoin ve ve1 in
    let codom = T.codom c (fun () -> assert false) ts2 in
    let t2 = match typ_opt with
      | None -> codom
      | Some typ -> check_typ env typ
    in
    if sort <> s then
      error env exp.at
        "%sshared function does not match expected %sshared function type"
        (if sort = T.Local then "non-" else "")
        (if s = T.Local then "non-" else "");
    if not (T.sub t2 codom) then
      error env exp.at
        "function return type\n  %s\ndoes not match expected return type\n  %s"
        (T.string_of_typ_expand t2) (T.string_of_typ_expand codom);
    let env' =
      { env with
        labs = T.Env.empty;
        rets = Some t2;
        async = C.NullCap; }
    in
    check_exp (adjoin_vals env' ve2) t2 exp;
    t
  | CallE (exp1, inst, exp2), _ ->
    let t' = infer_call env exp1 inst exp2 exp.at (Some t) in
    if not (T.sub t' t) then
      local_error env0 exp.at
        "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t);
    t'
  | TagE (id, exp1), T.Variant fs when List.exists (fun T.{lab; _} -> lab = id.it) fs ->
    let {T.typ; _} = List.find (fun T.{lab; typ} -> lab = id.it) fs in
    check_exp env typ exp1 ;
    t
  | _ ->
    let t' = infer_exp env0 exp in
    if not (T.sub t' t) then
      local_error env0 exp.at
        "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t);
    t'

and infer_call env exp1 inst exp2 at t_expect_opt =
  let t = Lib.Option.get t_expect_opt T.Any in
  let n = match inst.it with None -> 0 | Some typs ->  List.length typs in
  let t1 = infer_exp_promote env exp1 in
  let sort, tbs, t_arg, t_ret =
    try T.as_func_sub T.Local n t1
    with Invalid_argument _ ->
      local_error env exp1.at
        "expected function type, but expression produces type\n  %s"
        (T.string_of_typ_expand t1);
      if inst.it = None then
        info env (Source.between exp1.at exp2.at)
          "this looks like an unintended function call, perhaps a missing ';'?";
      T.as_func_sub T.Local n T.Non
  in
  let ts, t_arg', t_ret' =
    match tbs, inst.it with
    | [], (None | Some [])  (* no inference required *)
    | [{T.sort = T.Scope;_}], _  (* special case to allow t_arg driven overload resolution *)
    | _, Some _ ->
      (* explicit instantiation, check argument against instantiated domain *)
      let typs = match inst.it with None -> [] | Some typs -> typs in
      let ts = check_inst_bounds env tbs typs at in
      let t_arg' = T.open_ ts t_arg in
      let t_ret' = T.open_ ts t_ret in
      if not env.pre then check_exp env t_arg' exp2;
      ts, t_arg', t_ret'
    | _::_, None -> (* implicit, infer *)
      let t2 = infer_exp env exp2 in
      try
        (* i.e. exists_unique ts . t2 <: open_ ts t_arg /\ open ts_ t_ret <: t] *)
        let ts =
          Bi_match.bi_match_subs (scope_of_env env) tbs
            [(t2, t_arg); (t_ret, t)] in
        let t_arg' = T.open_ ts t_arg in
        let t_ret' = T.open_ ts t_ret in
        ts, t_arg', t_ret'
      with Failure msg ->
        error env at
          "cannot implicitly instantiate function of type\n  %s\nto argument of type\n  %s%s\n%s"
          (T.string_of_typ t1)
          (T.string_of_typ t2)
          (if Option.is_none t_expect_opt then ""
           else  Printf.sprintf "\nto produce result of type\n  %s" (T.string_of_typ t))
          msg
  in
  inst.note <- ts;
  if not env.pre then begin
    if Type.is_shared_sort sort then begin
      if not (T.concrete t_arg') then
        error env exp1.at
          "shared function argument contains abstract type\n  %s"
          (T.string_of_typ_expand t_arg');
      if not (T.concrete t_ret') then
        error env exp2.at
          "shared function call result contains abstract type\n  %s"
          (T.string_of_typ_expand t_ret');
    end
  end;
  (* note t_ret' <: t checked by caller if necessary *)
  t_ret'

(* Cases *)

and infer_cases env t_pat t cases : T.typ =
  List.fold_left (infer_case env t_pat) t cases

and infer_case env t_pat t case =
  let {pat; exp} = case.it in
  let ve = check_pat env t_pat pat in
  let t' = recover_with T.Non (infer_exp (adjoin_vals env ve)) exp in
  let t'' = T.lub t t' in
  if not env.pre && inconsistent t'' [t; t'] then
    warn env case.at
      "the switch has type %s because branches have inconsistent types,\nthis case produces type\n  %s\nthe previous produce type\n  %s"
      (T.string_of_typ t'')
      (T.string_of_typ_expand t)
      (T.string_of_typ_expand t');
  t''

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t case =
  let {pat; exp} = case.it in
  let ve = check_pat env t_pat pat in
  recover (check_exp (adjoin_vals env ve) t) exp

and inconsistent t ts =
  T.opaque t && not (List.exists T.opaque ts)


(* Patterns *)

and infer_pat_exhaustive warnOrError env pat : T.typ * Scope.val_env =
  let t, ve = infer_pat env pat in
  if not env.pre then begin
    match Coverage.check_pat pat t with
    | [] -> ()
    | ss ->
      warnOrError env pat.at
        "this pattern consuming type\n  %s\ndoes not cover value\n  %s"
        (Type.string_of_typ_expand t)
        (String.concat " or\n  " ss)
  end;
  t, ve

and infer_pat env pat : T.typ * Scope.val_env =
  assert (pat.note = T.Pre);
  let t, ve = infer_pat' env pat in
  if not env.pre then
    pat.note <- T.normalize t;
  t, ve

and infer_pat' env pat : T.typ * Scope.val_env =
  match pat.it with
  | WildP ->
    error env pat.at "cannot infer type of wildcard"
  | VarP _ ->
    error env pat.at "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit env lit pat.at), T.Env.empty
  | SignP (op, lit) ->
    let t1 = T.Prim (infer_lit env lit pat.at) in
    let t = Operator.type_unop op t1 in
    if not (Operator.has_unop op t) then
      error env pat.at "operator is not defined for operand type\n  %s"
        (T.string_of_typ_expand t);
    t, T.Env.empty
  | TupP pats ->
    let ts, ve = infer_pats pat.at env pats [] T.Env.empty in
    T.Tup ts, ve
  | ObjP pfs ->
    let (s, tfs), ve = infer_pat_fields pat.at env pfs [] T.Env.empty in
    T.Obj (s, tfs), ve
  | OptP pat1 ->
    let t1, ve = infer_pat env pat1 in
    T.Opt t1, ve
  | TagP (id, pat1) ->
    let t1, ve = infer_pat env pat1 in
    T.Variant [T.{lab = id.it; typ = t1}], ve
  | AltP (pat1, pat2) ->
    let t1, ve1 = infer_pat env pat1 in
    let t2, ve2 = infer_pat env pat2 in
    let t = T.lub t1 t2 in
    if not (T.compatible t1 t2) then
      error env pat.at
        "pattern branches have incompatible types,\nleft consumes\n  %s\nright consumes\n  %s"
        (T.string_of_typ_expand t1)
        (T.string_of_typ_expand t2);
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    t, T.Env.empty
  | AnnotP (pat1, typ) ->
    let t = check_typ env typ in
    t, check_pat env t pat1
  | ParP pat1 ->
    infer_pat env pat1

and infer_pats at env pats ts ve : T.typ list * Scope.val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat env pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'

and infer_pat_fields at env pfs ts ve : (T.obj_sort * T.field list) * Scope.val_env =
  match pfs with
  | [] -> (T.Object, List.sort T.compare_field ts), ve
  | pf::pfs' ->
    let typ, ve1 = infer_pat env pf.it.pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    infer_pat_fields at env pfs' (T.{ lab = pf.it.id.it; typ }::ts) ve'

and check_shared_pat env shared_pat : T.func_sort * Scope.val_env =
  match shared_pat.it with
  | T.Local -> T.Local, T.Env.empty
  | T.Shared (ss, pat) ->
    if pat.it <> WildP then
      error_in [Flags.WASIMode; Flags.WasmMode] env pat.at "shared function cannot take a context pattern";
    T.Shared ss, check_pat_exhaustive local_error env T.ctxt pat

and check_class_shared_pat env shared_pat obj_sort : Scope.val_env =
  match shared_pat.it, obj_sort.it with
  | T.Local, (T.Module | T.Object) -> T.Env.empty
  | T.Local, T.Actor ->
    T.Env.empty (* error instead? That's a breaking change *)
  | T.Shared (mode, pat), sort ->
    if sort <> T.Actor then
      error env pat.at "non-actor class cannot take a context pattern";
    if pat.it <> WildP then
      error_in [Flags.WASIMode; Flags.WasmMode] env pat.at "actor class cannot take a context pattern";
    if mode = T.Query then
      error env shared_pat.at "class cannot be a query";
    check_pat_exhaustive local_error env T.ctxt pat
  | _, T.Memory -> assert false


and check_pat_exhaustive warnOrError env t pat : Scope.val_env =
  let ve = check_pat env t pat in
  if not env.pre then begin
    match Coverage.check_pat pat t with
    | [] -> ()
    | ss ->
      warnOrError env pat.at
        "this pattern consuming type\n  %s\ndoes not cover value\n  %s"
        (Type.string_of_typ_expand t)
        (String.concat " or\n  " ss)
  end;
  ve

and check_pat env t pat : Scope.val_env =
  assert (pat.note = T.Pre);
  if t = T.Pre then snd (infer_pat env pat) else
  let t' = T.normalize t in
  let ve = check_pat' env t' pat in
  if not env.pre then pat.note <- t';
  ve

and check_pat' env t pat : Scope.val_env =
  assert (t <> T.Pre);
  match pat.it with
  | WildP ->
    T.Env.empty
  | VarP id ->
    T.Env.singleton id.it t
  | LitP lit ->
    if not env.pre then begin
      if T.opaque t then
        error env pat.at "literal pattern cannot consume expected type\n  %s"
          (T.string_of_typ_expand t);
      if T.sub t T.Non
      then ignore (infer_lit env lit pat.at)
      else check_lit env t lit pat.at
    end;
    T.Env.empty
  | SignP (op, lit) ->
    if not env.pre then begin
      if not (Operator.has_unop op (T.promote t)) then
        error env pat.at "operator cannot consume expected type\n  %s"
          (T.string_of_typ_expand t);
      if T.sub t T.Non
      then ignore (infer_lit env lit pat.at)
      else check_lit env t lit pat.at
    end;
    T.Env.empty
  | TupP pats ->
    let ts = try T.as_tup_sub (List.length pats) t with Invalid_argument _ ->
      error env pat.at "tuple pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand t)
    in check_pats env ts pats T.Env.empty pat.at
  | ObjP pfs ->
    let pfs' = List.stable_sort compare_pat_field pfs in
    let s, tfs =
      try T.as_obj_sub (List.map (fun (pf : pat_field) -> pf.it.id.it) pfs') t
      with Invalid_argument _ ->
        error env pat.at "object pattern cannot consume expected type\n  %s"
          (T.string_of_typ_expand t)
    in
    if not env.pre && s = T.Actor then
      local_error env pat.at "object pattern cannot consume actor type\n  %s"
        (T.string_of_typ_expand t);
    check_pat_fields env s tfs pfs' T.Env.empty pat.at
  | OptP pat1 ->
    let t1 = try T.as_opt_sub t with Invalid_argument _ ->
      error env pat.at "option pattern cannot consume expected type\n  %s"
        (T.string_of_typ_expand t)
    in check_pat env t1 pat1
  | TagP (id, pat1) ->
    let t1 =
      try T.lookup_val_field id.it (T.as_variant_sub id.it t)
      with Invalid_argument _ | Not_found ->
        error env pat.at "variant pattern cannot consume expected type\n  %s"
          (T.string_of_typ_expand t)
    in check_pat env t1 pat1
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if ve1 <> T.Env.empty || ve2 <> T.Env.empty then
      error env pat.at "variables are not allowed in pattern alternatives";
    T.Env.empty
  | AnnotP (pat1, typ) ->
    let t' = check_typ env typ in
    if not (T.sub t t') then
      error env pat.at
        "pattern of type\n  %s\ncannot consume expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t);
    check_pat env t pat1
  | ParP pat1 ->
    check_pat env t pat1

(*
Consider:

  case (P : A) : B


(P : A) :<= B  iff
1: B <: A   P :<= B
2: A <: B   P :<= A
3: B <: A   P :<= A
4: A <: B   P :<= B

1 is implemented, allows

  case ({x} : {}) : {x}  // type annotations are reversed for patterns
  case (1 : Int) : Nat   // type annotations are reversed for patterns
  case (x : Int) : Nat   // type annotations are reversed for patterns

2 would allow

  case ({x} : {x}) : {}  // unsound, x does not exist

3 would allow

  case (-1 : Int) : Nat  // breaks coverage checking

4 would allow

  case (x : Nat) : Int  // x is Int, harmless but misleading

Alternative: pass in two types?
*)


and check_pats env ts pats ve at : Scope.val_env =
  match ts, pats with
  | [], [] -> ve
  | t::ts', pat::pats' ->
    let ve1 = check_pat env t pat in
    let ve' = disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
    check_pats env ts' pats' ve' at
  | ts, [] ->
    local_error env at "tuple pattern has %i fewer components than expected type"
      (List.length ts); ve
  | [], ts ->
    error env at "tuple pattern has %i more components than expected type"
      (List.length ts)

and check_pat_fields env s tfs pfs ve at : Scope.val_env =
  match tfs, pfs with
  | _, [] -> ve
  | [], pf::_ ->
    error env pf.at
      "object field %s is not contained in expected type\n  %s"
      pf.it.id.it (T.string_of_typ (T.Obj (s, tfs)))
  | T.{lab; typ = Typ _}::tfs', _ ->  (* TODO: remove the namespace hack *)
    check_pat_fields env s tfs' pfs ve at
  | T.{lab; typ}::tfs', pf::pfs' ->
    match compare pf.it.id.it lab with
    | -1 -> check_pat_fields env s [] pfs ve at
    | +1 -> check_pat_fields env s tfs' pfs ve at
    | _ ->
      if T.is_mut typ then
        error env pf.at "cannot pattern match mutable field %s" lab;
      let ve1 = check_pat env typ pf.it.pat in
      let ve' =
        disjoint_union env at "duplicate binding for %s in pattern" ve ve1 in
      match pfs' with
      | pf'::_ when pf'.it.id.it = lab ->
        error env pf'.at "duplicate field %s in object pattern" lab
      | _ -> check_pat_fields env s tfs' pfs' ve' at

and compare_pat_field pf1 pf2 = compare pf1.it.id.it pf2.it.id.it


(* Objects *)

and pub_fields fields : region T.Env.t * region T.Env.t =
  List.fold_right pub_field fields (T.Env.empty, T.Env.empty)

and pub_field field xs : region T.Env.t * region T.Env.t =
  match field.it with
  | {vis; dec; _} when vis.it = Public -> pub_dec dec xs
  | _ -> xs

and pub_dec dec xs : region T.Env.t * region T.Env.t =
  match dec.it with
  | ExpD _ | IgnoreD _ -> xs
  | LetD (pat, _) -> pub_pat pat xs
  | VarD (id, _) -> pub_val_id id xs
  | ClassD (_, id, _, _, _, _, _, _) ->
    pub_val_id {id with note = ()} (pub_typ_id id xs)
  | TypD (id, _, _) -> pub_typ_id id xs

and pub_pat pat xs : region T.Env.t * region T.Env.t =
  match pat.it with
  | WildP | LitP _ | SignP _ -> xs
  | VarP id -> pub_val_id id xs
  | TupP pats -> List.fold_right pub_pat pats xs
  | ObjP pfs -> List.fold_right pub_pat_field pfs xs
  | AltP (pat1, _)
  | OptP pat1
  | TagP (_, pat1)
  | AnnotP (pat1, _)
  | ParP pat1 -> pub_pat pat1 xs

and pub_pat_field pf xs =
  pub_pat pf.it.pat xs

and pub_typ_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (T.Env.add id.it id.at xs, ys)

and pub_val_id id (xs, ys) : region T.Env.t * region T.Env.t =
  (xs, T.Env.add id.it id.at ys)

(* Object/Scope transformations *)

and gather_typ con_env t =
  match t with
  | T.Obj (s, tfs) -> List.fold_right gather_typ_field tfs con_env
  | _ -> con_env

and gather_typ_field T.{lab; typ} con_env =
  match typ with
  | T.Typ  c -> T.ConSet.add c con_env
  | t -> gather_typ con_env t

(* TODO: remove by merging conenv and valenv or by separating typ_fields *)

and object_of_scope env sort fields scope at =
  let pub_typ, pub_val = pub_fields fields in
  let tfs =
    T.Env.fold
      (fun id c tfs ->
        if T.Env.mem id pub_typ
        then T.{lab = id; typ = T.Typ c}::tfs
        else tfs
      ) scope.Scope.typ_env  []
  in
  let tfs' =
    T.Env.fold
      (fun id t tfs ->
        if T.Env.mem id pub_val
        then T.{lab = id; typ = t}::tfs
        else tfs
      ) scope.Scope.val_env tfs
  in

  Lib.List.iter_pairs
    (fun x y ->
      if not (T.is_typ x.T.typ) && not (T.is_typ y.T.typ) &&
         Hash.hash x.T.lab = Hash.hash y.T.lab
      then error env at "field names %s and %s in %sobject type have colliding hashes"
        x.T.lab y.T.lab (T.string_of_obj_sort sort);
    ) tfs';

  let t = T.Obj (sort, List.sort T.compare_field tfs') in
  let accessible_cons = gather_typ T.ConSet.empty t in
  let inaccessible_cons = T.ConSet.diff scope.Scope.con_env accessible_cons in
  try
    T.avoid_cons inaccessible_cons accessible_cons;
    T.avoid inaccessible_cons t
  with T.Unavoidable c ->
    error env at "local class type %s is contained in object or actor type\n  %s"
      (Con.to_string c)
      (T.string_of_typ_expand t)

and is_actor_method dec : bool = match dec.it with
  | LetD ({it = VarP _; _}, {it = FuncE (_, shared_pat, _, _, _, _, _); _}) ->
    T.is_shared_sort shared_pat.it
  | _ -> false

and is_typ_dec dec : bool = match dec.it with
  | TypD _ -> true
  | _ -> false


and infer_obj env s fields at : T.typ =
  let env = {env with in_actor = s = T.Actor} in
  let decs = List.map (fun (field : exp_field) -> field.it.dec) fields in
  let _, scope = infer_block env decs at in
  let t = object_of_scope env s fields scope at in
  let (_, tfs) = T.as_obj t in
  if not env.pre then begin
    if s = T.Actor then begin
      List.iter (fun T.{lab; typ} ->
        if not (T.is_typ typ) && not (T.is_shared_func typ) then
          let _, pub_val = pub_fields fields in
          error env (T.Env.find lab pub_val)
            "public actor field %s has non-shared function type\n  %s"
            lab (T.string_of_typ_expand typ)
      ) tfs;
      List.iter (fun ef ->
        if ef.it.vis.it = Syntax.Public && not (is_actor_method ef.it.dec) && not (is_typ_dec ef.it.dec) then
          local_error env ef.it.dec.at
            "public actor field needs to be a manifest function"
      ) fields;
      List.iter (fun ef ->
        if ef.it.vis.it = Syntax.Private && is_actor_method ef.it.dec then
          error_in [Flags.ICMode; Flags.RefMode] env ef.it.dec.at
            "a shared function cannot be private"
      ) fields;
    end;
    if s = T.Module then Static.fields env.msgs fields;
    check_system_fields env s scope fields;
    check_stab env s scope fields;
  end;
  t

and check_system_fields env sort scope fields =
  List.iter (fun ef ->
    match sort, ef.it.vis.it, ef.it.dec.it with
    | T.Actor, vis,
      LetD({ it = VarP id; _ },
           { it = FuncE _; _ }) ->
      begin
        match List.assoc_opt id.it system_funcs with
        | Some t ->
          (* TBR why does Stable.md require this to be a manifest function, not just any expression of appropriate type?  *)
          if vis = System then
            begin
              let t1 = T.Env.find id.it scope.Scope.val_env in
              if not (T.sub t1 t) then
                local_error env ef.at "system function %s is declared with type %s, expecting type %s" id.it
                  (T.string_of_typ t1) (T.string_of_typ t)
            end
          else warn env id.at "this function has the name of a system method, but is declared without system visibility and will not be called by the system"
        | None ->
          if vis = System then
            local_error env id.at "unexpected system method named %s: expected %s"
              id.it (String.concat " or " (List.map fst system_funcs))
          else ()
      end
    | _, System, _ ->
      local_error env ef.it.vis.at "misplaced system visibility, did you mean private?"
    | _ -> ())
  fields

and stable_pat pat =
  match pat.it with
  | VarP _ -> true
  | ParP pat'
  | AnnotP (pat', _) -> stable_pat pat'
  | _ -> false

and check_stab env sort scope fields =
  let check_stable id at =
    match T.Env.find_opt id scope.Scope.val_env with
    | None -> assert false
    | Some t ->
      let t1 = T.as_immut t in
      if not (T.stable t1) then
        local_error env at "variable %s is declared stable but has non-stable type %s" id (T.string_of_typ t1)
  in
  let idss = List.map (fun ef ->
    match sort, ef.it.stab, ef.it.dec.it with
    | (T.Object | T.Module), None, _ -> []
    | (T.Object | T.Module), Some stab, _ ->
      local_error env stab.at
        "misplaced stability declaration on field of non-actor";
      []
    | T.Actor, Some {it = Stable; _}, VarD (id, _) ->
      check_stable id.it id.at;
      [id]
    | T.Actor, Some {it = Stable; _}, LetD (pat, _) when stable_pat pat ->
      let ids = T.Env.keys (gather_pat env T.Env.empty pat) in
      List.iter (fun id -> check_stable id pat.at) ids;
      List.map (fun id -> {it = id; at = pat.at; note = ()}) ids;
    | T.Actor, Some {it = Flexible; _} , (VarD _ | LetD _) -> []
    | T.Actor, Some stab, _ ->
      local_error env stab.at "misplaced stability modifier: expected on var or simple let declarations only";
      []
    | _ -> []) fields
  in
  check_ids env "actor" "stable variable" (List.concat idss)


(* Blocks and Declarations *)

and infer_block env decs at : T.typ * Scope.scope =
  let scope = infer_block_decs env decs in
  let env' = adjoin env scope in
  (* HACK: when compiling to IC, mark class constructors as unavailable *)
  let ve = match !Flags.compile_mode with
    | (Flags.ICMode | Flags.RefMode) ->
      List.fold_left (fun ve' dec ->
        match dec.it with
        | ClassD(_, id, _, _, _, { it = T.Actor; _}, _, _) ->
          T.Env.mapi (fun id' (typ, avl) ->
            (typ, if id' = id.it then Unavailable else avl)) ve'
        | _ -> ve') env'.vals decs
    | _ -> env'.vals
  in
  let t = infer_block_exps { env' with vals = ve } decs in
  t, scope

and infer_block_decs env decs : Scope.t =
  let scope = gather_block_decs env decs in
  let env' = adjoin {env with pre = true} scope in
  let scope_ce = infer_block_typdecs env' decs in
  let env'' = adjoin {env' with pre = env.pre} scope_ce in
  let _scope_ce = infer_block_typdecs env'' decs in
  (* TBR: assertion does not work for types with binders, due to stamping *)
  (* assert (scope_ce = _scope_ce); *)
  infer_block_valdecs (adjoin env'' scope_ce) decs scope_ce

and infer_block_exps env decs : T.typ =
  match decs with
  | [] -> T.unit
  | [dec] -> infer_dec env dec
  | dec::decs' ->
    if not env.pre then recover (check_dec env T.unit) dec;
    recover_with T.Non (infer_block_exps env) decs'

and infer_dec env dec : T.typ =
  let t =
  match dec.it with
  | ExpD exp
  | LetD (_, exp) ->
    infer_exp env exp
  | IgnoreD exp ->
    if not env.pre then begin
      check_exp env T.Any exp;
      if T.sub exp.note.note_typ T.unit then
        warn env dec.at "redundant ignore, operand already has type ()"
    end;
    T.unit
  | VarD (_, exp) ->
    if not env.pre then ignore (infer_exp env exp);
    T.unit
  | ClassD (shared_pat, id, typ_binds, pat, typ_opt, obj_sort, self_id, fields) ->
    let (t, _) = T.Env.find id.it env.vals in
    if not env.pre then begin
      let c = T.Env.find id.it env.typs in
      let ve0 = check_class_shared_pat env shared_pat obj_sort in
      let cs, _ts, te, ce = check_typ_binds env typ_binds in
      let env' = adjoin_typs env te ce in
      let t_pat, ve =
        infer_pat_exhaustive (if obj_sort.it = T.Actor then error else warn) env' pat in
      if obj_sort.it = T.Actor && not (T.shared t_pat) then
        error_shared env t_pat pat.at "shared constructor has non-shared parameter type\n  %s" (T.string_of_typ_expand t_pat);
      let env'' = adjoin_vals (adjoin_vals env' ve0) ve in
      let self_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
      let env''' =
        { (add_val env'' self_id.it self_typ) with
          labs = T.Env.empty;
          rets = None;
          async = C.NullCap;
          in_actor = obj_sort.it = T.Actor;
        }
      in
      let t' = infer_obj env''' obj_sort.it fields dec.at in
      match typ_opt with
      | None -> ()
      | Some typ ->
        let t'' = check_typ env'' typ in
        if not (T.sub t' t'') then
          local_error env dec.at
            "class body of type\n  %s\ndoes not match expected type\n  %s"
            (T.string_of_typ_expand t')
            (T.string_of_typ_expand t'')
    end;
    T.normalize t
  | TypD _ ->
    T.unit
  in
  let eff = A.infer_effect_dec dec in
  dec.note <- {note_typ = t; note_eff = eff};
  t


and check_block env t decs at : Scope.t =
  let scope = infer_block_decs env decs in
  check_block_exps (adjoin env scope) t decs at;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    if not (T.sub T.unit t) then
      local_error env at "empty block cannot produce expected type\n  %s"
        (T.string_of_typ_expand t)
  | [dec] ->
    check_dec env t dec
  | dec::decs' ->
    recover (check_dec env T.unit) dec;
    recover (check_block_exps env t decs') at

and check_dec env t dec =
  match dec.it with
  | ExpD exp ->
    check_exp env t exp;
    dec.note <- exp.note
  | _ ->
    let t' = infer_dec env dec in
    if not (T.eq t T.unit || T.sub t' t) then
      local_error env dec.at "expression of type\n  %s\ncannot produce expected type\n  %s"
        (T.string_of_typ_expand t')
        (T.string_of_typ_expand t)

and infer_val_path env exp : T.typ option =
  match exp.it with
  | ImportE (f, ri) ->
    Some (check_import env exp.at f ri)
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with (* TBR: return None for Unavailable? *)
     | Some (t, _) -> Some t
     | _ -> None)
  | DotE (path, id) ->
    (match infer_val_path env path with
     | None -> None
     | Some t ->
       match T.promote t with
       | T.Obj ( _, flds) ->
         (try Some (T.lookup_val_field id.it flds)
         with Invalid_argument _ -> None)
       | _ -> None
    )
  | AnnotE (_, typ) ->
    Some (check_typ {env with pre = true}  typ)
  | _ -> None


(* Pass 1: collect:
   * type identifiers and their arity,
   * object identifiers and their fields (if known) (recursively)
   * other value identifiers at type T.Pre
*)
and gather_block_decs env decs : Scope.t =
  List.fold_left (gather_dec env) Scope.empty decs

and gather_dec env scope dec : Scope.t =
  match dec.it with
  | ExpD _ | IgnoreD _ -> scope
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _},
      {it = ObjE (obj_sort, fields); at; _}
    ) ->
    let decs = List.map (fun ef -> ef.it.dec) fields in
    let open Scope in
    if T.Env.mem id.it scope.val_env then
      error env dec.at "duplicate definition for value %s in block" id.it;
    let scope' = gather_block_decs env decs in
    let ve' = T.Env.add id.it (object_of_scope env obj_sort.it fields scope' at) scope.val_env in
    let obj_env = T.Env.add id.it scope' scope.obj_env in
    { val_env = ve';
      typ_env = scope.typ_env;
      lib_env = scope.lib_env;
      con_env = scope.con_env;
      obj_env = obj_env
    }
  | LetD (pat, _) -> Scope.adjoin_val_env scope (gather_pat env scope.Scope.val_env pat)
  | VarD (id, _) -> Scope.adjoin_val_env scope (gather_id env scope.Scope.val_env id)
  | TypD (id, binds, _) | ClassD (_, id, binds, _, _, _, _, _) ->
    let open Scope in
    if T.Env.mem id.it scope.typ_env then
      error env dec.at "duplicate definition for type %s in block" id.it;
    let pre_tbs = List.map (fun bind ->
                      {T.var = bind.it.var.it;
                       T.sort = T.Type;
                       T.bound = T.Pre}
                    ) binds in
    let pre_k = T.Abs (pre_tbs, T.Pre) in
    let c = match id.note with
      | None -> let c = Con.fresh id.it pre_k in id.note <- Some c; c
      | Some c -> c
    in
    let val_env = match dec.it with
      | ClassD _ ->
        if T.Env.mem id.it scope.val_env then
          error env id.at "duplicate definition for %s in block" id.it;
        T.Env.add id.it T.Pre scope.val_env
      | _ -> scope.val_env
    in
    { val_env;
      typ_env = T.Env.add id.it c scope.typ_env;
      con_env = T.ConSet.disjoint_add c scope.con_env;
      lib_env = scope.lib_env;
      obj_env = scope.obj_env;
    }

and gather_pat env ve pat : Scope.val_env =
  match pat.it with
  | WildP | LitP _ | SignP _ -> ve
  | VarP id -> gather_id env ve id
  | TupP pats -> List.fold_left (gather_pat env) ve pats
  | ObjP pfs -> List.fold_left (gather_pat_field env) ve pfs
  | TagP (_, pat1) | AltP (pat1, _) | OptP pat1
  | AnnotP (pat1, _) | ParP pat1 -> gather_pat env ve pat1

and gather_pat_field env ve pf : Scope.val_env =
  gather_pat env ve pf.it.pat

and gather_id env ve id : Scope.val_env =
  if T.Env.mem id.it ve then
    error env id.at "duplicate definition for %s in block" id.it;
  T.Env.add id.it T.Pre ve

(* Pass 2 and 3: infer type definitions *)
and infer_block_typdecs env decs : Scope.t =
  let _env', scope =
    List.fold_left (fun (env, scope) dec ->
      let scope' = infer_dec_typdecs env dec in
      adjoin env scope', Scope.adjoin scope scope'
    ) (env, Scope.empty) decs
  in scope

and infer_dec_typdecs env dec : Scope.t =
  match dec.it with
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _},
      {it = ObjE (obj_sort, fields); at; _}
    ) ->
    let decs = List.map (fun {it = {vis; dec; _}; _} -> dec) fields in
    let scope = T.Env.find id.it env.objs in
    let env' = adjoin env scope in
    let obj_scope_typs = infer_block_typdecs env' decs in
    let obj_scope = Scope.adjoin scope obj_scope_typs in
    Scope.{ empty with
      con_env = obj_scope.con_env;
      val_env = T.Env.singleton id.it (object_of_scope env obj_sort.it fields obj_scope at);
      obj_env = T.Env.singleton id.it obj_scope
    }
  (* TODO: generalize beyond let <id> = <valpath> *)
  | LetD ({it = VarP id; _}, exp) ->
    (match infer_val_path env exp with
     | None -> Scope.empty
     | Some t ->
       let open Scope in
       match T.promote t with
       | T.Obj (_, _) as t' -> { Scope.empty with val_env = T.Env.singleton id.it t' }
       | _ -> { Scope.empty with val_env = T.Env.singleton id.it T.Pre }
    )
  | LetD _ | ExpD _ | IgnoreD _ | VarD _ ->
    Scope.empty
  | TypD (id, binds, typ) ->
    let c = T.Env.find id.it env.typs in
    let cs, tbs, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs env te ce in
    let t = check_typ env' typ in
    let k = T.Def (T.close_binds cs tbs, T.close cs t) in
    begin
      let is_typ_param c =
        match Con.kind c with
        | T.Def _ -> false
        | T.Abs( _, T.Pre) -> false (* an approximated type constructor *)
        | T.Abs( _, _) -> true in
      let typ_params = T.ConSet.filter is_typ_param env.cons in
      let cs_k = T.cons_kind k in
      let free_params = T.ConSet.inter typ_params cs_k in
      if not (T.ConSet.is_empty free_params) then
        error env dec.at
          "type definition %s %s references type parameter(s) %s from an outer scope"
          id.it
          (T.string_of_kind k)
          (String.concat ", " (T.ConSet.fold (fun c cs -> T.string_of_con c::cs) free_params []))
    end;
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = infer_id_typdecs id c k;
    }
  | ClassD (shared_pat, id, binds, pat, _typ_opt, obj_sort, self_id, fields) ->
    let c = T.Env.find id.it env.typs in
    let ve0 = check_class_shared_pat {env with pre = true} shared_pat obj_sort in
    let cs, tbs, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs (adjoin_vals {env with pre = true} ve0) te ce in
    let _, ve = infer_pat env' pat in
    let self_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let env'' = add_val (adjoin_vals env' ve) self_id.it self_typ in
    let t = infer_obj env'' obj_sort.it fields dec.at in
    let k = T.Def (T.close_binds cs tbs, T.close cs t) in
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = infer_id_typdecs id c k;
    }

and infer_id_typdecs id c k : Scope.con_env =
  assert (match k with T.Abs (_, T.Pre) -> false | _ -> true);
  (match Con.kind c with
  | T.Abs (_, T.Pre) -> T.set_kind c k; id.note <- Some c
  | k' -> assert (T.eq_kind k' k)
  );
  T.ConSet.singleton c

(* Pass 4: infer value types *)
and infer_block_valdecs env decs scope : Scope.t =
  let _, scope' =
    List.fold_left (fun (env, scope) dec ->
      let scope' = infer_dec_valdecs env dec in
      adjoin env scope', Scope.adjoin scope scope'
    ) (env, scope) decs
  in scope'

and infer_dec_valdecs env dec : Scope.t =
  match dec.it with
  | ExpD _ | IgnoreD _ ->
    Scope.empty
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _} as pat,
      {it = ObjE (obj_sort, fields); at; _}
) ->
    let decs = List.map (fun ef -> ef.it.dec) fields in
    let obj_scope = T.Env.find id.it env.objs in
    let obj_scope' =
      infer_block_valdecs
        (adjoin {env with pre = true} obj_scope)
        decs obj_scope
    in
    let obj_typ = object_of_scope env obj_sort.it fields obj_scope' at in
    let _ve = check_pat env obj_typ pat in
    Scope.{empty with val_env = T.Env.singleton id.it obj_typ}
  | LetD (pat, exp) ->
    let t = infer_exp {env with pre = true} exp in
    let ve' = check_pat_exhaustive warn env t pat in
    Scope.{empty with val_env = ve'}
  | VarD (id, exp) ->
    let t = infer_exp {env with pre = true} exp in
    Scope.{empty with val_env = T.Env.singleton id.it (T.Mut t)}
  | TypD (id, _, _) ->
    let c = Option.get id.note in
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = T.ConSet.singleton c ;
    }
  | ClassD (_shared_pat, id, typ_binds, pat, _, obj_sort, _, _) ->
    if obj_sort.it = T.Actor then begin
      error_in [Flags.WASIMode; Flags.WasmMode] env dec.at "actor classes are not supported";
      if not env.in_prog then
        error_in [Flags.ICMode; Flags.RefMode] env dec.at
          "inner actor classes are not supported yet; any actor class must come last in your program";
      if not (is_anonymous id) then
        warn_in [Flags.ICMode; Flags.RefMode] env dec.at
          "actor classes should be anonymous: the constructor of this class will not be available to compiled code";
      if not (typ_binds = []) then
        error env dec.at
          "actor classes with type parameters are not supported yet";
    end;
    let cs, tbs, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find id.it env.typs in
    let t1, _ = infer_pat {env' with pre = true} pat in
    let ts1 = match pat.it with TupP _ -> T.seq_of_tup t1 | _ -> [t1] in
    let t2 = T.Con (c, List.map (fun c -> T.Con (c, [])) cs) in
    let t = T.Func (T.Local, T.Returns, T.close_binds cs tbs, List.map (T.close cs) ts1, [T.close cs t2]) in
    Scope.{ empty with
      val_env = T.Env.singleton id.it t;
      typ_env = T.Env.singleton id.it c;
      con_env = T.ConSet.singleton c;
    }


(* Programs *)

let infer_prog scope prog : (T.typ * Scope.t) Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun prog ->
          let env = env_of_scope msgs scope in
          let res = infer_block env prog.it prog.at in
          res
        ) prog
    )

let is_actor_dec d =
  match d.it with
  | LetD (_, {it = ObjE ({it = T.Actor; _}, _); _}) -> true
  | ClassD (shared_pat, id, typ_binds, pat, typ_opt, obj_sort, self_id, fields) ->
    obj_sort.it = T.Actor
  | _ -> false

let is_import d =
  match d.it with
  | LetD ({it = VarP n; _}, {it = ImportE _; _}) -> true
  | _ -> false

let check_actors scope progs : unit Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt (fun progs ->
        let prog = Lib.List.concat_map (fun prog -> prog.Source.it) progs in
        let env = env_of_scope msgs scope in
        let rec go ds = function
          | [] -> ()
          | (d::ds') when is_actor_dec d ->
            if ds <> [] || ds' <> []  then
              recover (error_in [Flags.ICMode; Flags.RefMode] env d.at)
                "an actor or actor class must be the only non-imported declaration in a program"
          | (d::ds') when is_import d -> go ds ds'
          | (d::ds') -> go (d::ds) ds'
        in
        go [] prog
      ) progs
    )

let check_lib scope lib : Scope.t Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun lib ->
          let env = env_of_scope msgs scope in
          let (imports, cub) = lib.it in
          let (imp_ds, ds) = Syntax.decs_of_comp_unit lib in
          let typ, _ = infer_block env (imp_ds @ ds) lib.at in
          List.iter2 (fun import imp_d -> import.note <- imp_d.note.note_typ) imports imp_ds;
          cub.note <- {note_typ = typ; note_eff = T.Triv};
          let imp_typ = match cub.it with
            | ModuleU _ ->
              if cub.at = no_region then begin
                let r = Source.({
                  left = { no_pos with file = lib.note };
                  right = { no_pos with file = lib.note }})
                in
                warn env r "deprecated syntax: an imported library should be a module or named actor class, not a sequence of declarations"
              end;
              typ
            | ActorClassU  (sp, id, p, _, self_id, fields) ->
              if Syntax.is_anonymous id then
                error env cub.at "bad import: imported actor class cannot be anonymous";
              let class_typ, fun_typ = begin
                match T.normalize typ with
                | T.Func (sort, control, [], ts1, [t2]) ->
                  t2,
                  T.Func (sort, control, [T.scope_bind],
                    ts1,
                    [T.Async (T.Var (T.default_scope_var, 0), t2)])
                | _ -> assert false
                end
              in
              let con = Con.fresh id.it (T.Def([], class_typ)) in
              T.Obj(T.Module, List.sort T.compare_field [
                { T.lab = id.it; T.typ = T.Typ con };
                { T.lab = id.it; T.typ = fun_typ }
              ])
            | ActorU _ ->
              error env cub.at "bad import: expected a module or actor class but found an actor"
            | ProgU _ ->
              (* this shouldn't really happen, as an imported program should be rewritten to a module *)
              error env cub.at "compiler bug: expected a module or actor class but found a program, i.e. a sequence of declarations"
          in
          Scope.lib lib.note imp_typ
        ) lib
    )
