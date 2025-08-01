open Mo_def
open Mo_types
open Mo_values
module Flags = Mo_config.Flags

open Syntax
open Source

module T = Type
module A = Effect
module C = Async_cap

module S = Set.Make(String)

(* Contexts  *)

(* availability, used to mark actor constructors as unavailable in compiled code
   FUTURE: mark unavailable, non-shared variables *)
type avl = Available | Unavailable

type lab_env = T.typ T.Env.t
type ret_env = T.typ option
type val_env  = (T.typ * Source.region * Scope.val_kind * avl) T.Env.t

(* separate maps for values and types; entries only for _public_ elements *)
type visibility_src = {depr : string option; id_region : Source.region; field_region : Source.region}
type visibility_env = visibility_src T.Env.t * visibility_src T.Env.t

let available env = T.Env.map (fun (ty, at, kind) -> (ty, at, kind, Available)) env

let initial_scope =
  { Scope.empty with
    Scope.typ_env = T.Env.singleton T.default_scope_var C.top_cap;
    Scope.con_env = T.ConSet.singleton C.top_cap;
  }

type unused_warnings = (string * Source.region * Scope.val_kind) List.t

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
    weak : bool;
    msgs : Diag.msg_store;
    scopes : Source.region T.ConEnv.t;
    check_unused : bool;
    used_identifiers : S.t ref;
    unused_warnings : unused_warnings ref;
    reported_stable_memory : bool ref;
    viper_mode : bool;
    errors_only : bool;
    srcs : Field_sources.t;
  }

let env_of_scope ?(viper_mode=false) msgs scope =
  { vals = available scope.Scope.val_env;
    libs = scope.Scope.lib_env;
    typs = scope.Scope.typ_env;
    cons = scope.Scope.con_env;
    objs = T.Env.empty;
    labs = T.Env.empty;
    rets = None;
    async = Async_cap.NullCap;
    in_actor = false;
    in_prog = true;
    context = [];
    pre = false;
    weak = false;
    msgs;
    scopes = T.ConEnv.empty;
    check_unused = true;
    used_identifiers = ref S.empty;
    unused_warnings = ref [];
    reported_stable_memory = ref false;
    errors_only = false;
    viper_mode;
    srcs = Field_sources.of_immutable_map scope.Scope.fld_src_env;
  }

let use_identifier env id =
  env.used_identifiers := S.add id !(env.used_identifiers)

let is_unused_identifier env id =
  not (S.mem id !(env.used_identifiers))

let get_identifiers identifiers =
  T.Env.fold (fun id _ set -> S.add id set) identifiers S.empty

let equal_unused_warning first second = first = second

let add_unused_warning env warning =
  if List.find_opt (equal_unused_warning warning) !(env.unused_warnings) = None then
    env.unused_warnings := warning::!(env.unused_warnings)
  else ()

let compare_unused_warning first second =
  let (first_id, {left = first_left; right = first_right}, _) = first in
  let (second_id, {left = second_left; right = second_right}, _) = second in
  match compare first_left second_left with
  | 0 ->
    (match compare first_right second_right with
     | 0 -> compare first_id second_id
     | other -> other)
  | other -> other

let sorted_unused_warnings list = List.sort compare_unused_warning list

let kind_of_field_pattern pf = match pf.it with
  | ValPF(id, { it = VarP pat_id; _ }) when id = pat_id -> Scope.FieldReference
  | _ -> Scope.Declaration

(* Error bookkeeping *)

exception Recover

let recover_with (x : 'a) (f : 'b -> 'a) (y : 'b) = try f y with Recover -> x
let recover_opt f y = recover_with None (fun y -> Some (f y)) y
let recover f y = recover_with () f y

let display_lab = Lib.Format.display T.pp_lab

let display_typ = Lib.Format.display T.pp_typ

let display_typ_expand = Lib.Format.display T.pp_typ_expand

let display_obj fmt (s, fs) =
  if !Flags.ai_errors || (List.length fs) < 16 then
    Format.fprintf fmt "type:%a" display_typ (T.Obj(s, fs))
  else
    Format.fprintf fmt "%s." (String.trim(T.string_of_obj_sort s))

let display_vals fmt vals =
  if !Flags.ai_errors then
    let tfs = T.Env.fold (fun x (t, _, _, _) acc ->
      if x = "Prim" || Syntax.is_privileged x
      then acc
      else T.{lab = x; src = empty_src; typ = t}::acc)
      vals []
    in
    let ty = T.Obj(T.Object, List.rev tfs) in
    Format.fprintf fmt " in environment:%a" display_typ ty
  else
    Format.fprintf fmt ""

let display_labs fmt labs =
  if !Flags.ai_errors then
    let tfs = T.Env.fold (fun x t acc ->
      T.{lab = x; src = empty_src; typ = t}::acc)
      labs []
    in
    let ty = T.Obj(T.Object, List.rev tfs) in
    Format.fprintf fmt " in label environment:%a" display_typ ty
  else
    Format.fprintf fmt ""

let display_typs fmt typs =
  if !Flags.ai_errors then
    let tfs = T.Env.fold (fun x c acc ->
      if Syntax.(is_privileged x || is_scope x) ||
        T.(match Cons.kind c with
          | Def ([], Prim _)
          | Def ([], Any)
          | Def ([], Non) -> string_of_con c = x
          | _ -> false)
      then acc
      else T.{lab = x; src = empty_src; typ = T.Typ c}::acc)
      typs []
    in
    let ty = T.Obj(T.Object, List.rev tfs) in
    Format.fprintf fmt " in type environment:%a" display_typ ty
  else
    Format.fprintf fmt ""

let type_error at code text : Diag.message =
  Diag.error_message at code "type" text

let type_warning at code text : Diag.message =
  Diag.warning_message at code "type" text

let type_info at text : Diag.message =
  Diag.info_message at "type" text

let error env at code fmt =
  Format.kasprintf
    (fun s -> Diag.add_msg env.msgs (type_error at code s); raise Recover) fmt

let local_error env at code fmt =
  Format.kasprintf (fun s -> Diag.add_msg env.msgs (type_error at code s)) fmt

let warn env at code fmt =
  Format.kasprintf (fun s ->
    if not env.errors_only then Diag.add_msg env.msgs (type_warning at code s)) fmt

let info env at fmt =
  Format.kasprintf (fun s ->
    if not env.errors_only then Diag.add_msg env.msgs (type_info at s)) fmt

let check_deprecation env at desc id depr =
  match depr with
  | Some ("M0199" as code) ->
    if !(env.reported_stable_memory) then ()
    else begin
      env.reported_stable_memory := true;
      (match compare !Flags.experimental_stable_memory 0 with
       | -1 -> error
       | 0 -> warn
       | _ -> fun _ _ _ _ -> ())
       env at code
       "this code is (or uses) the deprecated library `ExperimentalStableMemory`.\nPlease use the `Region` library instead: https://internetcomputer.org/docs/current/motoko/main/stable-memory/stable-regions/#the-region-library or compile with flag `--experimental-stable-memory 1` to suppress this message."
    end
  | Some msg ->
    warn env at "M0154" "%s %s is deprecated:\n%s" desc id msg
  | None -> ()

let flag_of_compile_mode mode =
  match mode with
  | Flags.ICMode -> ""
  | Flags.WASIMode -> " and flag -wasi-system-api"
  | Flags.WasmMode -> " and flag -no-system-api"
  | Flags.RefMode -> " and flag -ref-system-api"

let diag_in type_diag modes env at code fmt =
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
          Diag.add_msg env.msgs (type_diag at code s)) fmt;
      true
    end
  else false

let error_in modes env at code fmt =
  if diag_in type_error modes env at code fmt then
    raise Recover

let plural cs = if T.ConSet.cardinal cs = 1 then "" else "s"

let warn_lossy_bind_type env at bind t1 t2 =
  if not T.(sub ~src_fields:env.srcs t1 t2 || sub ~src_fields:env.srcs t2 t1) then
    warn env at "M0190" "pattern variable %s has larger type%a\nbecause its types in the pattern alternatives are unrelated smaller types:\ntype in left pattern is%a\ntype in right pattern is%a"
      bind
      display_typ_expand (T.lub ~src_fields:env.srcs t1 t2)
      display_typ_expand t1
      display_typ_expand t2

(* Currently unused *)
let _warn_in modes env at code fmt =
  ignore (diag_in type_warning modes env at code fmt)

(* Unused identifier detection *)

let emit_unused_warnings env =
  let emit (id, region, kind) = match kind with
    | Scope.Declaration -> warn env region "M0194" "unused identifier %s (delete or rename to wildcard `_` or `_%s`)" id id
    | Scope.FieldReference -> warn env region "M0198" "unused field %s in object pattern (delete or rewrite as `%s = _`)" id id
  in
  let list = sorted_unused_warnings !(env.unused_warnings) in
  List.iter emit list

let ignore_warning_for_id id =
  Syntax.(is_underscored id || is_privileged id)

let detect_unused env inner_identifiers =
  if not env.pre && env.check_unused then
    T.Env.iter (fun id (_, at, kind) ->
      if (not (ignore_warning_for_id id)) && (is_unused_identifier env id) then
        add_unused_warning env (id, at, kind)
    ) inner_identifiers

let enter_scope env : S.t =
  !(env.used_identifiers)

let leave_scope env inner_identifiers initial_usage =
  detect_unused env inner_identifiers;
  let inner_identifiers = get_identifiers inner_identifiers in
  let unshadowed_usage = S.diff !(env.used_identifiers) inner_identifiers in
  let final_usage = S.union initial_usage unshadowed_usage in
  env.used_identifiers := final_usage

(* Value environments *)

let singleton id t = T.Env.singleton id.it (t, id.at, Scope.Declaration)
let add_id val_env id t = T.Env.add id.it (t, id.at, Scope.Declaration) val_env

(* Context extension *)

let add_lab env x t = {env with labs = T.Env.add x t env.labs}

let add_val env id t =
  { env with vals = T.Env.add id.it (t, id.at, Scope.Declaration, Available) env.vals }

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

let disjoint_union env at code fmt env1 env2 =
  try T.Env.disjoint_union env1 env2
  with T.Env.Clash k -> error env at code fmt k


let sub env at t1 t2 =
  try T.sub ~src_fields:env.srcs t1 t2 with T.Undecided ->
    error env at "M0200" "cannot decide subtyping between type%a\nand%a"
      display_typ_expand t1
      display_typ_expand t2

let eq env at t1 t2 =
  try T.eq ~src_fields:env.srcs t1 t2 with T.Undecided ->
    error env at "M0200" "cannot decide equality between type%a\nand%a"
      display_typ_expand t1
      display_typ_expand t2


let eq_kind env at k1 k2 =
  try T.eq_kind ~src_fields:env.srcs k1 k2 with T.Undecided ->
    error env at "M0200" "cannot decide type constructor equality"

(* Coverage *)

let coverage' warnOrError category env f x t at =
  let uncovered, unreached = f x t in
  List.iter (fun at -> warn env at "M0146" "this pattern is never matched") unreached;
  if uncovered <> [] then
    warnOrError env at "M0145"
      ("this %s of type%a\ndoes not cover value\n  %s" : (_, _, _, _) format4 )
      category
      display_typ_expand t
      (String.concat " or\n  " uncovered)

let coverage_cases category env cases t at =
  coverage' warn category env Coverage.check_cases cases t at

let coverage_pat warnOrError env pat t =
  coverage' warnOrError "pattern" env Coverage.check_pat pat t pat.at

(* Types *)

let check_ids env kind member ids = Lib.List.iter_pairs
  (fun x y ->
    if x.it = y.it
    then error env y.at "M0018" "duplicate %s name %s in %s" member y.it kind;
    if Hash.hash x.it = Hash.hash y.it
    then error env y.at "M0019" "%s names %s and %s in %s have colliding hashes" member x.it y.it kind;
  ) ids

let infer_mut mut : T.typ -> T.typ =
  match mut.it with
  | Const -> Fun.id
  | Var -> fun t -> T.Mut t


(* System method types *)

let system_funcs tfs =
  [
    ("heartbeat", T.heartbeat_type);
    ("timer", T.timer_type);
    T.("preupgrade", Func (Local, Returns, [scope_bind], [], []));
    T.("postupgrade", Func (Local, Returns, [scope_bind], [], []));
    ("lowmemory", T.low_memory_type);
    ("inspect",
     (let msg_typ = T.decode_msg_typ tfs in
      let record_typ =
        T.(Obj (Object, List.sort compare_field
           [{lab = "caller"; typ = principal; src = empty_src};
            {lab = "arg"; typ = blob; src = empty_src};
            {lab = "msg"; typ = msg_typ; src = empty_src}]))
      in
        T.(Func (Local, Returns, [],  [record_typ], [bool]))))
  ]


let check_closed env id k at =
  let is_typ_param c =
    match Cons.kind c with
    | T.Def _
    | T.Abs( _, T.Pre) -> false (* an approximated type constructor *)
    | T.Abs( _, _) -> true in
  let typ_params = T.ConSet.filter is_typ_param env.cons in
  let cs_k = T.cons_kind k in
  let free_params = T.ConSet.inter typ_params cs_k in
  if not (T.ConSet.is_empty free_params) then
    let op, sbs, st = T.strings_of_kind k in
    error env at "M0137"
      "type %s%s %s %s references type parameter%s %s from an outer scope"
      id.it sbs op st
      (plural free_params)
      (String.concat ", " (T.ConSet.fold (fun c cs -> T.string_of_con c::cs) free_params []))

(* Imports *)

let check_import env at f ri =
  let full_path =
    match !ri with
    | Unresolved -> error env at "M0020" "unresolved import %s" f
    | LibPath {path=fp; _} -> fp
    | IDLPath (fp, _) -> fp
    | PrimPath -> "@prim"
  in
  match T.Env.find_opt full_path env.libs with
  | Some T.Pre ->
    error env at "M0021" "cannot infer type of forward import %s" f
  | Some t -> t
  | None -> error env at "M0022" "imported file %s not loaded" full_path


(* Paths *)

let rec check_obj_path env path : T.obj_sort * (T.field list) =
  match T.promote (check_obj_path' env path) with
  | T.Obj (s, fs) as t ->
    path.note <- t;
    (s, fs)
  | t ->
    error env path.at "M0023"
      "expected module, object, or actor type, but path expression produces type%a"
      display_typ_expand t

and check_obj_path' env path : T.typ =
  match path.it with
  | IdH id ->
    use_identifier env id.it;
    (match T.Env.find_opt id.it env.vals with
     | Some (T.Pre, _, _, _) ->
       error env id.at "M0024" "cannot infer type of forward variable reference %s" id.it
     | Some (t, _, _, Available) -> t
     | Some (t, _, _, Unavailable) ->
       error env id.at "M0025" "unavailable variable %s" id.it
     | None ->
       error env id.at "M0026" "unbound variable %s%a%s" id.it
         display_vals env.vals
         (Suggest.suggest_id "variable" id.it (T.Env.keys env.vals))
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    match T.lookup_val_field id.it fs with
    | T.Pre ->
      error env id.at "M0027" "cannot infer type of forward field reference %s" id.it
    | t -> t
    | exception Invalid_argument _ ->
      error env id.at "M0028" "field %s does not exist in %a%s"
        id.it
        display_obj (s, fs)
        (Suggest.suggest_id "field" id.it
          (List.filter_map
            (function
              {T.typ=T.Typ _;_} -> None
            | {T.lab;_} -> Some lab) fs))

let rec check_typ_path env path : T.con =
  let c = check_typ_path' env path in
  path.note <- T.Typ c;
  c

and check_typ_path' env path : T.con =
  match path.it with
  | IdH id ->
    use_identifier env id.it;
    (match T.Env.find_opt id.it env.typs with
    | Some c -> c
    | None ->
      error env id.at "M0029" "unbound type %s%a%s" id.it
        display_typs env.typs
        (Suggest.suggest_id "type" id.it (T.Env.keys env.typs))
    )
  | DotH (path', id) ->
    let s, fs = check_obj_path env path' in
    match T.lookup_typ_field id.it fs with
      | c ->
        check_deprecation env path.at "type field" id.it (T.lookup_typ_deprecation id.it fs);
        c
      | exception Invalid_argument _ ->
        error env id.at "M0030" "type field %s does not exist in %a%s"
          id.it display_obj (s, fs)
          (Suggest.suggest_id "type field" id.it
             (List.filter_map
               (function { T.lab; T.typ=T.Typ _;_ } -> Some lab
               |  _ -> None) fs))

(* Type helpers *)

let error_shared env t at code fmt =
  match T.find_unshared t with
  | None -> error env at code fmt
  | Some t1 ->
    let s =
      Format.asprintf "\ntype%a\nis or contains non-shared type%a"
        display_typ_expand t
        display_typ_expand t1
    in
    Format.kasprintf (fun s1 -> Diag.add_msg env.msgs (type_error at code (s1^s)); raise Recover) fmt

let as_domT t =
  match t.Source.it with
  | TupT tis -> tis
  | _ -> [(None, t)]

let as_codomT sort t =
  match sort, t.Source.it with
  | T.Shared _,  AsyncT (T.Fut, _, t1) ->
    T.Promises, as_domT t1
  | _ -> T.Returns, as_domT t

let check_shared_binds env at tbs =
  (* should be ensured by desugaring parser *)
  assert (List.length tbs > 0 &&
            (List.hd(tbs)).T.sort = T.Scope);
  (* shared functions can't have user declared type parameters *)
  if List.length tbs > 1 then
    error env at "M0180"
      "shared function has unexpected type parameters"

let check_shared_return env at sort c ts =
  match sort, c, ts with
  | T.Shared _, T.Promises,  _ -> ()
  | T.Shared T.Write, T.Returns, [] -> ()
  | T.Shared T.Write, _, _ -> error env at "M0035" "shared function must have syntactic return type '()' or 'async <typ>'"
  | T.Shared T.Query, _, _ -> error env at "M0036" "shared query function must have syntactic return type 'async <typ>'"
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
    if eq env at typ (T.Con(C.top_cap,[])) then
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

let infer_async_cap env sort cs tbs body_opt at =
  let open T in
  match sort, cs, tbs with
  | Shared Write, c::_,  { T.sort = Scope; _ }::_ ->
    { env with typs = Env.add default_scope_var c env.typs;
               scopes = ConEnv.add c at env.scopes;
               async = C.AsyncCap c }
  | Shared Query, c::_,  { sort = Scope; _ }::_ ->
    { env with typs = Env.add default_scope_var c env.typs;
               scopes = ConEnv.add c at env.scopes;
               async = C.QueryCap c }
  | Shared Composite, c::_,  { sort = Scope; _ }::_ ->
    { env with typs = Env.add default_scope_var c env.typs;
               scopes = ConEnv.add c at env.scopes;
               async = C.CompositeCap c }
  | Shared _, _, _ -> assert false (* impossible given sugaring *)
  | Local, c::_,  { sort = Scope; _ }::_ ->
    let async = match body_opt with
      | Some exp when not (is_asyncE exp) -> C.SystemCap c
      | _ -> C.AsyncCap c
    in
    { env with typs = Env.add default_scope_var c env.typs;
               scopes = ConEnv.add c at env.scopes;
               async }
  | _ -> { env with async = C.NullCap }

let check_AsyncCap env s at : T.typ * (T.con -> C.async_cap) =
   match env.async with
   | C.AwaitCap c
   | C.AsyncCap c -> T.Con(c, []), fun c' -> C.AwaitCap c'
   | C.CompositeCap c -> T.Con(c, []), fun c' -> C.CompositeAwaitCap c'
   | C.QueryCap c -> T.Con(c, []), fun _c' -> C.ErrorCap
   | C.ErrorCap ->
      local_error env at "M0037" "misplaced %s; a query cannot contain an %s" s s;
      T.Con(C.bogus_cap,[]), fun c -> C.NullCap
   | C.(NullCap | SystemCap _) ->
      local_error env at "M0037" "misplaced %s; try enclosing in an async function" s;
      T.Con(C.bogus_cap,[]), fun c -> C.NullCap
   | C.CompositeAwaitCap _ ->
      local_error env at "M0037" "misplaced %s; a composite query cannot contain an %s" s s;
      T.Con(C.bogus_cap,[]), fun c -> C.NullCap

let check_AwaitCap env s at =
   match env.async with
   | C.(AwaitCap c
        | CompositeAwaitCap c) -> T.Con(c, [])
   | C.AsyncCap _
   | C.QueryCap _
   | C.CompositeCap _
     ->
      local_error env at "M0038" "misplaced %s; try enclosing in an async expression" s;
      T.Con(C.bogus_cap,[])
   | C.(ErrorCap | NullCap | SystemCap _) ->
      local_error env at "M0038" "misplaced %s" s;
      T.Con(C.bogus_cap,[])

let check_ErrorCap env s at =
   match env.async with
   | C.AwaitCap c -> ()
   | C.ErrorCap -> ()
   | C.CompositeAwaitCap c -> ()
   | C.AsyncCap _
   | C.QueryCap _
   | C.CompositeCap _ ->
     local_error env at "M0039" "misplaced %s; try enclosing in an async expression or query function" s
   | C.(NullCap | SystemCap _) ->
     local_error env at "M0039" "misplaced %s" s

and scope_of_env env =
  C.(match env.async with
     | AsyncCap c
     | QueryCap c
     | CompositeCap c
     | CompositeAwaitCap c
     | AwaitCap c
     | SystemCap c -> Some (T.Con(c, []))
     | ErrorCap | NullCap -> None)

let infer_class_cap env obj_sort (tbs : T.bind list) cs =
  match tbs, cs with
  | T.{sort = T.Scope; _} :: tbs', c :: cs' ->
    (* HACK:
       choosing top_cap just to support compilation of actor classes
       which currently won't have any binding for c
    *)
    let c = if obj_sort = T.Actor then C.top_cap else c in
    C.SystemCap c,
    tbs',
    cs'
  | _ ->
    C.NullCap, tbs, cs

(* Types *)

let rec check_typ env (typ : typ) : T.typ =
  let t = check_typ' env typ in
  typ.note <- t;
  t

and check_typ_item env typ_item =
  match typ_item with
  | (None, typ) -> check_typ env typ
  | (Some id, typ) -> T.Named (id.it, check_typ env typ)

and check_typ' env typ : T.typ =
  match typ.it with
  | PathT (path, typs) ->
    let c = check_typ_path env path in
    let ts = List.map (check_typ env) typs in
    let T.Def (tbs, _) | T.Abs (tbs, _) = Cons.kind c in
    let tbs' = List.map (fun tb -> { tb with T.bound = T.open_ ts tb.T.bound }) tbs in
    check_typ_bounds env tbs' ts (List.map (fun typ -> typ.at) typs) typ.at;
    T.Con (c, ts)
  | PrimT "Any" -> T.Any
  | PrimT "None" -> T.Non
  | PrimT s ->
    (try T.Prim (T.prim s) with Invalid_argument _ ->
      error env typ.at "M0040" "unknown primitive type"
    )
  | ArrayT (mut, typ) ->
    let t = check_typ env typ in
    T.Array (infer_mut mut t)
  | TupT typ_items ->
    T.Tup (List.map (check_typ_item env) typ_items)
  | FuncT (sort, binds, typ1, typ2) ->
    let cs, tbs, te, ce = check_typ_binds env binds in
    let env' = infer_async_cap (adjoin_typs env te ce) sort.it cs tbs None typ.at in
    let typs1 = as_domT typ1 in
    let c, typs2 = as_codomT sort.it typ2 in
    let ts1 = List.map (check_typ_item env') typs1 in
    let ts2 = List.map (check_typ_item env') typs2 in
    check_shared_return env typ2.at sort.it c ts2;
    if not env.pre && Type.is_shared_sort sort.it then begin
      check_shared_binds env typ.at tbs;
      let t1 = T.seq ts1 in
      if not (T.shared t1) then
        error_shared env t1 typ1.at "M0031" "shared function has non-shared parameter type%a"
          display_typ_expand t1;
      List.iter (fun t ->
        if not (T.shared t) then
          error_shared env t typ.at "M0032"
            "shared function has non-shared return type%a"
            display_typ_expand t;
      ) ts2;
      match c, ts2 with
      | T.Returns, [] when sort.it = T.Shared T.Write -> ()
      | T.Promises, _ -> ()
      | _ ->
        error env typ2.at "M0041"
          "shared function has non-async result type%a"
          display_typ_expand (T.seq ts2)
      end;
    T.Func (sort.it, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | OptT typ ->
    T.Opt (check_typ env typ)
  | VariantT tags ->
    check_ids env "variant type" "tag"
      (List.map (fun (tag : typ_tag) -> tag.it.tag) tags);
    let fs = List.map (check_typ_tag env) tags in
    T.Variant (List.sort T.compare_field fs)
  | AsyncT (s, typ0, typ) ->
    let t0 = check_typ env typ0 in
    let t = check_typ env typ in
    if not env.pre && not (T.shared t) then
      error_shared env t typ.at
        "M0033" "async has non-shared content type%a"
        display_typ_expand t;
    T.Async (s, t0, t)
  | ObjT (sort, fields) ->
    check_ids env "object type" "field"
      (List.filter_map (fun (field : typ_field) ->
        match field.it with ValF (x, _, _) -> Some x | _ -> None
      ) fields);
    check_ids env "object type" "type field"
      (List.filter_map (fun (field : typ_field) ->
        match field.it with TypF (x, _, _) -> Some x | _ -> None
      ) fields);
    let fs = List.map (check_typ_field env sort.it) fields in
    T.Obj (sort.it, List.sort T.compare_field fs)
  | AndT (typ1, typ2) ->
    let t1 = check_typ env typ1 in
    let t2 = check_typ env typ2 in
    let t = try T.glb ~src_fields:env.srcs t1 t2 with T.PreEncountered ->
      error env typ2.at "M0168"
        "cannot compute intersection of types containing recursive or forward references to other type definitions"
    in
    if not env.pre && sub env typ.at t T.Non && not (sub env typ1.at t1 T.Non || sub env typ2.at t2 T.Non) then
      warn env typ.at "M0166"
        "this intersection results in type%a\nbecause operand types are inconsistent,\nleft operand is%a\nright operand is%a"
        display_typ t
        display_typ_expand t1
        display_typ_expand t2;
    t
  | OrT (typ1, typ2) ->
    let t1 = check_typ env typ1 in
    let t2 = check_typ env typ2 in
    let t = try T.lub ~src_fields:env.srcs t1 t2 with T.PreEncountered ->
      error env typ2.at "M0168"
        "cannot compute union of types containing recursive or forward references to other type definitions"
    in
    if not env.pre && sub env typ.at T.Any t && not (sub env typ1.at T.Any t1 || sub env typ2.at T.Any t2) then
      warn env typ.at "M0167"
        "this union results in type%a\nbecause operand types are inconsistent,\nleft operand is%a\nright operand is%a"
        display_typ t
        display_typ_expand t1
        display_typ_expand t2;
    t
  | ParT typ ->
    check_typ env typ
  | NamedT (name, typ) ->
    T.Named (name.it, check_typ env typ)

and check_typ_def env at (id, typ_binds, typ) : T.kind =
  let cs, tbs, te, ce = check_typ_binds {env with pre = true} typ_binds in
  let env' = adjoin_typs env te ce in
  let t = check_typ env' typ in
  let k = T.Def (T.close_binds cs tbs, T.close cs t) in
  check_closed env id k at;
  k

and check_typ_field env s typ_field : T.field = match typ_field.it with
  | ValF (id, typ, mut) ->
    let t = infer_mut mut (check_typ env typ) in
    if not env.pre && s = T.Actor then begin
      if not (T.is_shared_func t) then
        error env typ.at "M0042" "actor field %s must have shared function type, but has type\n  %s"
          id.it (T.string_of_typ_expand t)
    end;
    Field_sources.add_src env.srcs id.at;
    T.{lab = id.it; typ = t; src = {empty_src with track_region = id.at}}
  | TypF (id, typ_binds, typ) ->
    let k = check_typ_def env typ_field.at (id, typ_binds, typ) in
    let c = Cons.fresh id.it k in
    Field_sources.add_src env.srcs id.at;
    T.{lab = id.it; typ = Typ c; src = {empty_src with track_region = id.at}}

and check_typ_tag env typ_tag =
  let {tag; typ} = typ_tag.it in
  let t = check_typ env typ in
  Field_sources.add_src env.srcs tag.at;
  T.{lab = tag.it; typ = t; src = {empty_src with track_region = tag.at}}

and check_typ_binds_acyclic env typ_binds cs ts  =
  let n = List.length cs in
  let ce = List.fold_right2 T.ConEnv.add cs ts T.ConEnv.empty in
  let chase typ_bind c =
    let rec chase i ts c' =
      if i > n then
        error env typ_bind.at "M0043" "type parameter %s has cyclic bounds %s"
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
      | None -> Cons.fresh x (T.Abs ([], T.Pre))) xs typ_binds in
  let te = List.fold_left2 (fun te typ_bind c ->
      let id = typ_bind.it.var in
      if T.Env.mem id.it te then
        error env id.at "M0044" "duplicate type name %s in type parameter list" id.it;
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
    match Cons.kind c with
    | T.Abs (_, T.Pre) -> T.set_kind c k
    | k' -> assert (eq_kind env Source.no_region k k')
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
  if pars <> args then begin
    let consider_scope x = match tbs with
      | hd :: _ when hd.T.sort = T.Scope -> x - 1
      | _ -> x in
    error env at "M0045"
      "wrong number of type arguments: expected %d but got %d"
      (consider_scope pars)
      (consider_scope args)
    end;
  let rec go tbs' ts' ats' =
    match tbs', ts', ats' with
    | tb::tbs', t::ts', at'::ats' ->
      if not env.pre then
        let u = T.open_ ts tb.T.bound in
        if not (sub env at' t u) then
          local_error env at' "M0046"
            "type argument%a\ndoes not match parameter bound%a"
            display_typ_expand t
            display_typ_expand u;
        go tbs' ts' ats'
    | [], [], [] -> ()
    | _  -> assert false
  in go tbs ts ats

(* Check type definitions productive and non-expansive *)
and check_con_env env at ce =
  let cs = Productive.non_productive ce in
  if not (T.ConSet.is_empty cs) then
    error env at "M0157" "block contains non-productive definition%s %s"
      (plural cs)
      (String.concat ", " (List.sort compare (List.map Cons.name (T.ConSet.elements cs))));

  begin match Mo_types.Expansive.is_expansive ce with
  | None -> ()
  | Some msg ->
    error env at "M0156" "block contains expansive type definitions%s" msg
  end;

and infer_inst env sort tbs typs t_ret at =
  let ts = List.map (check_typ env) typs in
  let ats = List.map (fun typ -> typ.at) typs in
  match tbs, typs with
  | {T.bound; sort = T.Scope; _}::tbs', typs' ->
    assert (List.for_all (fun tb -> tb.T.sort = T.Type) tbs');
    (match env.async with
     | cap when sort = T.Local && not (T.is_async t_ret) ->
       begin
         match cap with
         | C.(SystemCap c | AwaitCap c | AsyncCap c) ->
           (T.Con(c, [])::ts, at::ats)
         | _ ->
          if not env.pre then
            local_error env at "M0197"
              "`system` capability required, but not available\n (need an enclosing async expression or function body or explicit `system` type parameter)";
          (T.Con(C.bogus_cap, [])::ts, at::ats)
       end
     | C.(AwaitCap c | AsyncCap c) when T.(sort = Shared Query || sort = Shared Write || sort = Local) ->
        (T.Con(c, [])::ts, at::ats)
     | C.(AwaitCap c | AsyncCap c) when sort = T.(Shared Composite) ->
        error env at "M0186"
         "composite send capability required, but not available\n  (cannot call a `composite query` function from a non-`composite query` function)"
     | C.(CompositeAwaitCap c | CompositeCap c) ->
       begin
         match sort with
         | T.(Shared (Composite | Query)) ->
           (T.Con(c, [])::ts, at::ats)
         | T.(Shared Write | Local) ->
           error env at "M0187"
             "send capability required, but not available\n  (cannot call a `shared` function from a `composite query` function; only calls to `query` and `composite query` functions are allowed)"
       end
     | C.ErrorCap
     | C.QueryCap _ ->
        error env at "M0188"
         "send capability required, but not available\n  (cannot call a `shared` function from a `query` function)"
     | C.NullCap
     | _ ->
        error env at "M0047"
          "send capability required, but not available\n (need an enclosing async expression or function body)"
    )
  | tbs', typs' ->
    assert (List.for_all (fun tb -> tb.T.sort = T.Type) tbs');
    ts, ats

and check_inst_bounds env sort tbs inst t_ret at =
  let ts, ats = infer_inst env sort tbs inst t_ret at in
  check_typ_bounds env tbs ts ats at;
  ts


(* Subgrammar of explicitly typed expressions *)

(* Roughly, this defines the sublanguage of expressions whose inferred type
   is determined by explicit type annotations or previously defined identifiers,
   or by expressions whose type is unambiguous and can be weakened only to Any
   or via lossy width subtyping on records.

   The intuition is that for an explicit expression, the inferred type is a
   "good enough" choice to resolve overloading of operators that have it as
   an operand.

   Specifically, this excludes expression forms that are either overloaded
   or have a principal type like None or Null, that are subtypes of other
   non-trivial types. These must be excluded so that examples like the
   following do not run into checking mode with a type that is too small:

     null == ?0
     [] == [0]
     (break) == 0
*)

let is_explicit_lit l =
  match l with
  | BoolLit _ -> true
  | _ -> false

let rec is_explicit_pat p =
  match p.it with
  | WildP | VarP _ -> false
  | LitP l | SignP (_, l) -> is_explicit_lit !l
  | OptP p1 | TagP (_, p1) | ParP p1 -> is_explicit_pat p1
  | TupP ps -> List.for_all is_explicit_pat ps
  | ObjP pfs -> List.for_all is_explicit_pat_field pfs
  | AltP (p1, p2) -> is_explicit_pat p1 && is_explicit_pat p2
  | AnnotP _ -> true

and is_explicit_pat_field pf =
  match pf.it with
  | ValPF(_, p) -> is_explicit_pat p
  (* NOTE(Christoph): Being conservative here, it might be fine to allow type pattern fields *)
  | TypPF(_) -> false

let rec is_explicit_exp e =
  match e.it with
  | PrimE _ | ActorUrlE _
  | TagE _
  | BreakE _ | RetE _ | ThrowE _ ->
    false
  | VarE _
  | RelE _ | NotE _ | AndE _ | OrE _ | ImpliesE _ | OldE _ | ShowE _ | ToCandidE _ | FromCandidE _
  | AssignE _ | IgnoreE _ | AssertE _ | DebugE _
  | WhileE _ | ForE _
  | AnnotE _ | ImportE _ ->
    true
  | LitE l -> is_explicit_lit !l
  | UnE (_, _, e1) | OptE e1 | DoOptE e1
  | ProjE (e1, _) | DotE (e1, _) | BangE e1 | IdxE (e1, _) | CallE (_, e1, _, _)
  | LabelE (_, _, e1) | AsyncE (_, _, _, e1) | AwaitE (_, e1) ->
    is_explicit_exp e1
  | BinE (_, e1, _, e2) | IfE (_, e1, e2) ->
    is_explicit_exp e1 || is_explicit_exp e2
  | TupE es -> List.for_all is_explicit_exp es
  | ObjE (bases, efs) ->
    List.(for_all is_explicit_exp bases
          && for_all (fun (ef : exp_field) -> is_explicit_exp ef.it.exp) efs)
  | ObjBlockE (_, _e_opt, _, dfs) ->
    List.for_all (fun (df : dec_field) -> is_explicit_dec df.it.dec) dfs
  | ArrayE (_, es) -> List.exists is_explicit_exp es
  | SwitchE (e1, cs) ->
    is_explicit_exp e1 &&
    List.exists (fun (c : case) -> is_explicit_exp c.it.exp) cs
  | TryE (e1, cs, _) ->
    is_explicit_exp e1 &&
    (cs = [] || List.exists (fun (c : case) -> is_explicit_exp c.it.exp) cs)
  | BlockE ds -> List.for_all is_explicit_dec ds
  | FuncE (_, _, _, p, t_opt, _, _) -> is_explicit_pat p && t_opt <> None
  | LoopE (_, e_opt) -> e_opt <> None

and is_explicit_dec d =
  match d.it with
  | ExpD e | LetD (_, e, _) | VarD (_, e) -> is_explicit_exp e
  | TypD _ -> true
  | ClassD (_, _, _, _, _, p, _, _, dfs) ->
    is_explicit_pat p &&
    List.for_all (fun (df : dec_field) -> is_explicit_dec df.it.dec) dfs


(* Literals *)

let check_lit_val env t of_string at s =
  try of_string s with Invalid_argument _ ->
    error env at "M0048" "literal out of range for type %s"
      (T.string_of_typ (T.Prim t))

let check_nat env = check_lit_val env T.Nat Numerics.Nat.of_string
let check_nat8 env = check_lit_val env T.Nat8 Numerics.Nat8.of_string
let check_nat16 env = check_lit_val env T.Nat16 Numerics.Nat16.of_string
let check_nat32 env = check_lit_val env T.Nat32 Numerics.Nat32.of_string
let check_nat64 env = check_lit_val env T.Nat64 Numerics.Nat64.of_string
let check_int env = check_lit_val env T.Int Numerics.Int.of_string
let check_int8 env = check_lit_val env T.Int8 Numerics.Int_8.of_string
let check_int16 env = check_lit_val env T.Int16 Numerics.Int_16.of_string
let check_int32 env = check_lit_val env T.Int32 Numerics.Int_32.of_string
let check_int64 env = check_lit_val env T.Int64 Numerics.Int_64.of_string
let check_float env = check_lit_val env T.Float Numerics.Float.of_string

let check_text env at s =
  if not (Lib.Utf8.is_valid s) then
    local_error env at "M0049" "string literal \"%s\": is not valid utf8" (String.escaped s);
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

let check_lit env t lit at suggest =
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
  | T.Prim T.Float, PreLit (s, (T.Nat | T.Int | T.Float)) ->
    lit := FloatLit (check_float env at s)
  | T.Prim T.Blob, PreLit (s, T.Text) ->
    lit := BlobLit s
  | t, _ ->
    let t' = T.Prim (infer_lit env lit at) in
    if not (sub env at t' t) then
    error env at "M0050"
      "literal of type%a\ndoes not have expected type%a%s"
      display_typ t'
      display_typ_expand t
      (if suggest then Suggest.suggest_conversion env.libs env.vals t' t else "")

(* Coercions *)

let array_obj t =
  let open T in
  let immut t =
    [ {lab = "get";  typ = Func (Local, Returns, [], [Prim Nat], [t]); src = empty_src};
      {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat]); src = empty_src};
      {lab = "keys"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat)]); src = empty_src};
      {lab = "vals"; typ = Func (Local, Returns, [], [], [iter_obj t]); src = empty_src};
      {lab = "values"; typ = Func (Local, Returns, [], [], [iter_obj t]); src = empty_src};
    ] in
  let mut t = immut t @
    [ {lab = "put"; typ = Func (Local, Returns, [], [Prim Nat; t], []); src = empty_src} ] in
  Object,
  List.sort compare_field (match t with Mut t' -> mut t' | t -> immut t)

let blob_obj () =
  let open T in
  Object,
  [ {lab = "get";  typ = Func (Local, Returns, [], [Prim Nat], [Prim Nat8]); src = empty_src};
    {lab = "vals"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat8)]); src = empty_src};
    {lab = "values"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat8)]); src = empty_src};
    {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat]); src = empty_src};
    {lab = "keys"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Nat)]); src = empty_src};
  ]

let text_obj () =
  let open T in
  Object,
  [ {lab = "chars"; typ = Func (Local, Returns, [], [], [iter_obj (Prim Char)]); src = empty_src};
    {lab = "size";  typ = Func (Local, Returns, [], [], [Prim Nat]); src = empty_src};
  ]


(* Expressions *)

let error_duplicate env kind id =
  error env id.at "M0051" "duplicate definition for %s%s in block" kind id.it


let error_bin_op env at t1 t2 =
  error env at "M0060"
    "operator is not defined for operand types%a\nand%a"
    display_typ_expand t1
    display_typ_expand t2

(* NOTE: Keep in sync with mo_types/type.ml:compare_field *)
let compare_pat_field pf1 pf2 = match pf1.it, pf2.it with
  | TypPF(id1), TypPF(id2) -> compare id1.it id2.it
  | TypPF(_), _ -> -1
  | _, TypPF(_) -> 1
  | ValPF(id1, _), ValPF(id2, _) -> compare id1.it id2.it

let compare_pat_typ_field tf pf = match tf, pf.it with
  | T.{lab; typ = Typ t'; _}, TypPF(id) -> compare lab id.it
  | T.{typ = Typ t'; _}, _ -> -1
  | _, TypPF(_) -> 1
  | T.{lab; _}, ValPF(id, _) -> compare lab id.it

let rec combine_pat_fields_srcs env t tfs (pfs : pat_field list) : unit =
  match tfs, pfs with
  | _, [] | [], _ -> ()
  | T.{lab; typ = Typ _; _}::tfs', _ ->  (* TODO: remove the namespace hack *)
     combine_pat_fields_srcs env t tfs' pfs
  | _, {it = TypPF(_); _}::pfs' ->
     combine_pat_fields_srcs env t tfs pfs'
  | T.{lab; typ; src}::tfs', {it = ValPF(id, pat); _}::pfs' ->
    match compare id.it lab with
    | -1 -> combine_pat_fields_srcs env t [] pfs
    | +1 -> combine_pat_fields_srcs env t tfs' pfs
    | _ ->
      combine_pat_srcs env typ pat;
      combine_pat_fields_srcs env t tfs' pfs';

and combine_id_srcs env t id : unit =
  match T.Env.find_opt id.it env.vals with
  | None -> ()
  | Some (t', _, _, _) ->
    (* Use [sub] to merge the fields of sources, if one type is indeed a subtype
       of the other. *)
    try ignore (T.sub ~src_fields:env.srcs t t') with
    | T.Undecided | T.PreEncountered -> ()

and combine_pat_srcs env t pat : unit =
  match pat.it with
  | WildP -> ()
  | VarP id -> combine_id_srcs env t id
  | LitP _ -> ()
  | SignP _ -> ()
  | TupP pats ->
    let ts = T.as_tup_sub (List.length pats) t in
    List.iter2 (combine_pat_srcs env) ts pats
  | ObjP pfs ->
    let pfs' = List.stable_sort compare_pat_field pfs in
    let _s, tfs =
      T.as_obj_sub (List.filter_map (fun pf ->
        match pf.it with
        | TypPF(_) -> None
        | ValPF(id, _) -> Some(id.it)) pfs') t
    in
    combine_pat_fields_srcs env t tfs pfs'
  | OptP pat1 ->
    let t1 = T.as_opt_sub t in
    combine_pat_srcs env t1 pat1
  | TagP (id, pat1) ->
    let t1 =
      match T.lookup_val_field_opt id.it (T.as_variant_sub id.it t) with
      | Some t1 -> t1
      | None -> T.Non
    in
    combine_pat_srcs env t1 pat1
  | AltP (pat1, pat2) ->
    combine_pat_srcs env t pat1;
    combine_pat_srcs env t pat2;
  | AnnotP (pat1, _typ) -> combine_pat_srcs env t pat1
  | ParP pat1 -> combine_pat_srcs env t pat1

let rec infer_exp env exp : T.typ =
  infer_exp' T.as_immut env exp

and infer_exp_mut env exp : T.typ =
  infer_exp' Fun.id env exp

and infer_exp_promote env exp : T.typ =
  let t = infer_exp env exp in
  let t' = T.promote t in
  if t' = T.Pre then
    error env exp.at "M0053"
      "cannot infer type of expression while trying to infer surrounding class type,\nbecause its type is a forward reference to type%a"
      display_typ_expand t;
  t'

and infer_exp_wrapper inf f env exp : T.typ =
  assert (exp.note.note_typ = T.Pre);
  let t = inf env exp in
  assert (t <> T.Pre);
  let t' = f t in
  if not env.pre then begin
    let t'' = T.normalize t' in
    assert (t'' <> T.Pre);
    let note_eff = A.infer_effect_exp exp in
    exp.note <- {note_typ = if env.viper_mode then t' else t''; note_eff}
  end;
  t'

and infer_exp' f env exp : T.typ = infer_exp_wrapper infer_exp'' f env exp

and infer_exp'' env exp : T.typ =
  let context = env.context in
  let in_actor = env.in_actor in
  let env = {env with in_actor = false; in_prog = false; context = exp.it::env.context} in
  match exp.it with
  | PrimE _ ->
    error env exp.at "M0054" "cannot infer type of primitive"
  | VarE id ->
    use_identifier env id.it;
    (match T.Env.find_opt id.it env.vals with
    | Some (T.Pre, _, _, _) ->
      error env id.at "M0055" "cannot infer type of forward variable %s" id.it;
    | Some (t, _, _, Unavailable) ->
      if !Flags.compiled then
        error env id.at "M0056" "variable %s is in scope but not available in compiled code" id.it
      else t
    | Some (t, _, _, Available) -> id.note <- (if T.is_mut t then Var else Const); t
    | None ->
      error env id.at "M0057" "unbound variable %s%a%s" id.it
        display_vals env.vals
        (Suggest.suggest_id "variable" id.it (T.Env.keys env.vals))
    )
  | LitE lit ->
    T.Prim (infer_lit env lit exp.at)
  | ActorUrlE exp' ->
    if not env.pre then check_exp_strong env T.text exp';
    error env exp.at "M0058" "no type can be inferred for actor reference"
  | UnE (ot, op, exp1) ->
    let t1 = infer_exp_promote env exp1 in
    let t = Operator.type_unop op t1 in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_unop op t) then
        error env exp.at "M0059" "operator is not defined for operand type%a"
          display_typ_expand t;
      ot := t;
    end;
    t
  | BinE (ot, exp1, op, exp2) ->
    let t1, t2 = infer_bin_exp env exp1 exp2 in
    let t = Operator.type_binop op (T.lub ~src_fields:env.srcs (T.promote t1) (T.promote t2)) in
    if not env.pre then begin
      assert (!ot = Type.Pre);
      if not (Operator.has_binop op t) then
        error_bin_op env exp.at t1 t2
      else if op = Operator.SubOp && eq env exp.at t T.nat then
        warn env exp.at "M0155" "operator may trap for inferred type%a"
          display_typ_expand t;
      ot := t
    end;
    t
  | RelE (ot, exp1, op, exp2) ->
    if not env.pre then begin
      assert (!ot = Type.Pre);
      let t1, t2 = infer_bin_exp env exp1 exp2 in
      let t = Operator.type_relop op (T.lub ~src_fields:env.srcs (T.promote t1) (T.promote t2)) in
      if not (Operator.has_relop op t) then
        error_bin_op env exp.at t1 t2;
      if not (eq env exp1.at t t1 || eq env exp2.at t t2) && not (sub env exp1.at T.nat t1 && sub env exp2.at T.nat t2) then
        if eq env exp.at t1 t2 then
          warn env exp.at "M0061"
            "comparing abstract type%a\nto itself at supertype%a"
            display_typ_expand t1
            display_typ_expand t
        else
          warn env exp.at "M0062"
            "comparing incompatible types%a\nand%a\nat common supertype%a"
            display_typ_expand t1
            display_typ_expand t2
            display_typ_expand t;
      ot := t;
    end;
    T.bool
  | ShowE (ot, exp1) ->
    if not env.pre then begin
      let t = infer_exp_promote env exp1 in
      if not (Show.can_show t) then
        error env exp.at "M0063" "show is not defined for operand type%a"
          display_typ_expand t;
      ot := t
    end;
    T.text
  | ToCandidE exps ->
    if not env.pre then begin
        let ts = List.map (infer_exp env) exps in
        if not (T.shared (T.seq ts)) then
          error env exp.at "M0175" "to_candid argument must have shared type, but instead has non-shared type%a"
            display_typ_expand (T.seq ts);
      end;
    T.Prim T.Blob
  | FromCandidE exp1 ->
    error env exp.at "M0176" "from_candid requires but is missing a known type (from context)"
  | TupE exps ->
    let ts = List.map (infer_exp env) exps in
    T.Tup ts
  | OptE exp1 ->
    let t1 = infer_exp env exp1 in
    T.Opt t1
  | DoOptE exp1 ->
    let env' = add_lab env "!" (T.Prim T.Null) in
    let t1 = infer_exp env' exp1 in
    T.Opt t1
  | BangE exp1 ->
    begin
      let t1 = infer_exp_promote env exp1 in
      if Option.is_none (T.Env.find_opt "!" env.labs) then
        local_error env exp.at "M0064" "misplaced '!' (no enclosing 'do ? { ... }' expression)";
      try
        T.as_opt_sub t1
      with Invalid_argument _ ->
        error env exp1.at "M0065"
          "expected option type before '!', but expression produces type%a"
          display_typ_expand t1
    end
  | TagE (id, exp1) ->
    let t1 = infer_exp env exp1 in
    Field_sources.add_src env.srcs id.at;
    T.Variant [T.{lab = id.it; typ = t1; src = {empty_src with track_region = id.at}}]
  | ProjE (exp1, n) ->
    let t1 = infer_exp_promote env exp1 in
    (try
      let ts = T.as_tup_sub n t1 in
      match List.nth_opt ts n with
      | Some t -> t
      | None ->
        error env exp.at "M0066" "tuple projection %n is out of bounds for type%a"
          n
          display_typ_expand t1
    with Invalid_argument _ ->
      error env exp1.at "M0067"
        "expected tuple type, but expression produces type%a"
        display_typ_expand t1
    )
  | ObjBlockE (exp_opt, obj_sort, typ_opt, dec_fields) as e ->
    let _typ_opt = infer_migration env obj_sort exp_opt in
    if obj_sort.it = T.Actor then begin
      error_in [Flags.WASIMode; Flags.WasmMode] env exp.at "M0068"
        "actors are not supported";
      match context with
      | (AsyncE _ :: AwaitE _ :: _ :: _ ) ->
         error_in [Flags.ICMode; Flags.RefMode] env exp.at "M0069"
           "non-toplevel actor; an actor can only be declared at the toplevel of a program"
      | _ -> ()
      end;
    let env' =
      if obj_sort.it = T.Actor then
        { env with
          in_actor = true;
          async = C.SystemCap C.top_cap }
      else env
    in
    let t = infer_obj env' obj_sort exp_opt dec_fields exp.at in
    begin match env.pre, typ_opt with
      | false, (_, Some typ) ->
        let t' = check_typ env' typ in
        if not (sub env exp.at t t') then
          local_error env exp.at "M0192"
            "body of type%a\ndoes not match expected type%a"
            display_typ_expand t
            display_typ_expand t'
        else detect_lost_fields env t' e;
      | _ -> ()
    end;
    t
  | ObjE (exp_bases, exp_fields) ->
    infer_check_bases_fields env [] exp.at exp_bases exp_fields
  | DotE (exp1, id) ->
    let t1 = infer_exp_promote env exp1 in
    let s, tfs =
      try T.as_obj_sub [id.it] t1 with Invalid_argument _ ->
      try array_obj (T.as_array_sub t1) with Invalid_argument _ ->
      try blob_obj (T.as_prim_sub T.Blob t1) with Invalid_argument _ ->
      try text_obj (T.as_prim_sub T.Text t1) with Invalid_argument _ ->
        error env exp1.at "M0070"
          "expected object type, but expression produces type%a"
          display_typ_expand t1
    in
    (match T.lookup_val_field id.it tfs with
    | T.Pre ->
      error env exp.at "M0071"
        "cannot infer type of forward field reference %s"
        id.it
    | t ->
      if not env.pre then
        check_deprecation env exp.at "field" id.it (T.lookup_val_deprecation id.it tfs);
      t
    | exception Invalid_argument _ ->
      error env id.at "M0072"
        "field %s does not exist in %a%s"
        id.it
        display_obj (s, tfs)
        (Suggest.suggest_id "field" id.it
          (List.filter_map
             (function
               { T.typ=T.Typ _;_} -> None
             | {T.lab;_} -> Some lab) tfs))
    )
  | AssignE (exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_mut env exp1 in
      try
        let t2 = T.as_mut t1 in
        check_exp_strong env t2 exp2
      with Invalid_argument _ ->
        error env exp.at "M0073" "expected mutable assignment target";
    end;
    T.unit
  | ArrayE (mut, exps) ->
    let ts = List.map (infer_exp env) exps in
    let t1 = List.fold_left (T.lub ~src_fields:env.srcs) T.Non ts in
    if not env.pre && inconsistent t1 ts then
      warn env exp.at "M0074"
        "this array has type%a\nbecause elements have inconsistent types"
        display_typ (T.Array t1);
    T.Array (match mut.it with Const -> t1 | Var -> T.Mut t1)
  | IdxE (exp1, exp2) ->
    let t1 = infer_exp_promote env exp1 in
    begin match t1 with
    | T.(Prim Blob) ->
      if not env.pre then check_exp_strong env T.nat exp2;
      T.(Prim Nat8)
    | _ ->
      try
        let t = T.as_array_sub t1 in
        if not env.pre then check_exp_strong env T.nat exp2;
        t
      with Invalid_argument _ ->
        error env exp1.at "M0075"
          "expected array type or Blob, but expression produces type%a"
          display_typ_expand t1
    end
  | FuncE (_, shared_pat, typ_binds, pat, typ_opt, _sugar, exp1) ->
    if not env.pre && not in_actor && T.is_shared_sort shared_pat.it then begin
      error_in [Flags.WASIMode; Flags.WasmMode] env exp1.at "M0076"
        "shared functions are not supported";
      if not in_actor then
        error_in [Flags.ICMode; Flags.RefMode] env exp1.at "M0077"
          "a shared function is only allowed as a public field of an actor";
    end;
    let typ = match typ_opt with
      | Some typ -> typ
      | None -> {it = TupT []; at = no_region; note = T.Pre}
    in
    begin match exp1.it with
    | AsyncE (Some par, _, _, _) when not env.pre && T.is_shared_sort shared_pat.it ->
      local_error env par.at "M0213"
        "parenthetical notes aren't allowed on shared functions"
    | _ -> ()
    end;
    let sort, ve = check_shared_pat env shared_pat in
    let cs, tbs, te, ce = check_typ_binds env typ_binds in
    let c, ts2 = as_codomT sort typ in
    check_shared_return env typ.at sort c ts2;
    let env' = infer_async_cap (adjoin_typs env te ce) sort cs tbs (Some exp1) exp.at in
    let t1, ve1 = infer_pat_exhaustive (if T.is_shared_sort sort then local_error else warn) env' pat in
    let ve2 = T.Env.adjoin ve ve1 in
    let ts2 = List.map (check_typ_item env') ts2 in
    typ.note <- T.seq ts2; (* HACK *)
    let codom = T.codom c (fun () -> T.Con(List.hd cs,[])) ts2 in
    if not env.pre then begin
      let env'' =
        { env' with
          labs = T.Env.empty;
          rets = Some codom;
          (* async = None; *) }
      in
      let initial_usage = enter_scope env'' in
      check_exp_strong (adjoin_vals env'' ve2) codom exp1;
      leave_scope env ve2 initial_usage;
      if Type.is_shared_sort sort then begin
        check_shared_binds env exp.at tbs;
        if not (T.shared t1) then
          error_shared env t1 pat.at "M0031"
            "shared function has non-shared parameter type%a"
            display_typ_expand t1;
        List.iter (fun t ->
          if not (T.shared t) then
            error_shared env t typ.at "M0032"
              "shared function has non-shared return type%a"
              display_typ_expand t;
        ) ts2;
        match c, ts2 with
        | T.Returns, [] when sort = T.Shared T.Write ->
          if not (is_ignore_asyncE exp1) then
            error env exp1.at "M0078"
              "shared function with () result type has unexpected body:\n  the body must either be of sugared form '{ ... }' \n  or explicit form '= ignore ((async ...) : async ())'"
        | T.Promises, _ ->
          if not (is_asyncE exp1) then
            error env exp1.at "M0079"
              "shared function with async result type has non-async body"
        | _ ->
          error env typ.at "M0041" "shared function has non-async result type%a"
            display_typ_expand codom
      end
    end;
    let ts1 = match pat.it with TupP _ -> T.seq_of_tup t1 | _ -> [t1] in
    T.Func (sort, c, T.close_binds cs tbs, List.map (T.close cs) ts1, List.map (T.close cs) ts2)
  | CallE (par_opt, exp1, inst, exp2) ->
    let t = infer_call env exp1 inst exp2 exp.at None in
    if not env.pre then check_parenthetical env (Some exp1.note.note_typ) par_opt;
    t
  | BlockE decs ->
    let t, _ = infer_block env decs exp.at false in
    t
  | NotE exp1 ->
    if not env.pre then check_exp_strong env T.bool exp1;
    T.bool
  | AndE (exp1, exp2) ->
    if not env.pre then begin
      check_exp_strong env T.bool exp1;
      check_exp_strong env T.bool exp2
    end;
    T.bool
  | OrE (exp1, exp2) ->
    if not env.pre then begin
      check_exp_strong env T.bool exp1;
      check_exp_strong env T.bool exp2
    end;
    T.bool
  | ImpliesE (exp1, exp2) ->
    if not env.pre then begin
      check_exp_strong env T.bool exp1;
      check_exp_strong env T.bool exp2
    end;
    T.bool
  | OldE exp1 ->
    infer_exp_promote env exp1
  | IfE (exp1, exp2, exp3) ->
    if not env.pre then check_exp_strong env T.bool exp1;
    let t2 = infer_exp env exp2 in
    let t3 = infer_exp env exp3 in
    let t = T.lub ~src_fields:env.srcs t2 t3 in
    if not env.pre && inconsistent t [t2; t3] then
      warn env exp.at "M0081"
        "this if has type%a\nbecause branches have inconsistent types,\ntrue produces%a\nfalse produces%a"
        display_typ t
        display_typ_expand t2
        display_typ_expand t3;
    t
  | SwitchE (exp1, cases) ->
    let t1 = infer_exp_promote env exp1 in
    let t = infer_cases env t1 T.Non cases in
    if not env.pre then
      coverage_cases "switch" env cases t1 exp.at;
    t
  | TryE (exp1, cases, exp2_opt) ->
    let t1 = infer_exp env exp1 in
    let t2 = infer_cases env T.catch T.Non cases in
    if not env.pre then begin
      check_ErrorCap env "try" exp.at;
      if cases <> [] then
        coverage_cases "try handler" env cases T.catch exp.at;
      Option.iter (check_exp_strong { env with async = C.NullCap; rets = None; labs = T.Env.empty } T.unit) exp2_opt
    end;
    T.lub ~src_fields:env.srcs t1 t2
  | WhileE (exp1, exp2) ->
    if not env.pre then begin
      check_exp_strong env T.bool exp1;
      check_exp_strong env T.unit exp2
    end;
    T.unit
  | LoopE (exp1, None) ->
    if not env.pre then begin
      check_exp_strong env T.unit exp1
    end;
    T.Non
  | LoopE (exp1, Some exp2) ->
    if not env.pre then begin
      check_exp_strong env T.unit exp1;
      check_exp_strong env T.bool exp2
    end;
    T.unit
  | ForE (pat, exp1, exp2) ->
    if not env.pre then begin
      let t1 = infer_exp_promote env exp1 in
      (try
        let _, tfs = T.as_obj_sub ["next"] t1 in
        let t = T.lookup_val_field "next" tfs in
        let t1, t2 = T.as_mono_func_sub t in
        if not (sub env exp1.at T.unit t1) then raise (Invalid_argument "");
        let t2' = T.as_opt_sub t2 in
        let ve = check_pat_exhaustive warn env t2' pat in
        check_exp_strong (adjoin_vals env ve) T.unit exp2
      with Invalid_argument _ | Not_found ->
        local_error env exp1.at "M0082"
          "expected iterable type, but expression has type%a"
          display_typ_expand t1
      );
    end;
    T.unit
  | LabelE (id, typ, exp1) ->
    let t = check_typ env typ in
    if not env.pre then check_exp (add_lab env id.it t) t exp1;
    t
  | DebugE exp1 ->
    if not env.pre then check_exp_strong env T.unit exp1;
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
      in local_error env id.at "M0083" "unbound label %s%a%s" name
         display_labs env.labs
         (Suggest.suggest_id "label" id.it (T.Env.keys env.labs))
    );
    T.Non
  | RetE exp1 ->
    if not env.pre then begin
      match env.rets with
      | Some T.Pre ->
        local_error env exp.at "M0084" "cannot infer return type"
      | Some t ->
        check_exp_strong env t exp1
      | None ->
        local_error env exp.at "M0085" "misplaced return"
    end;
    T.Non
  | ThrowE exp1 ->
    if not env.pre then begin
      check_ErrorCap env "throw" exp.at;
      check_exp_strong env T.throw exp1
    end;
    T.Non
  | AsyncE (par_opt, s, typ_bind, exp1) ->
    error_in Flags.[WASIMode; WasmMode] env exp1.at "M0086"
      "async expressions are not supported";
    if not env.pre then check_parenthetical env None par_opt;
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
      error_shared env t' exp1.at "M0033" "async type has non-shared content type%a"
        display_typ_expand t';
    T.Async (s, t1, t')
  | AwaitE (s, exp1) ->
    let t0 = check_AwaitCap env "await" exp.at in
    let t1 = infer_exp_promote env exp1 in
    let s1 = match s with
      | T.AwaitFut _ -> T.Fut (* we can await/await? an async *)
      | T.AwaitCmp -> T.Cmp in (* we can await* an async* *)
    (try
       let (t2, t3) = T.as_async_sub s1 t0 t1 in
       if not (eq env exp.at t0 t2) then begin
          local_error env exp1.at "M0087"
            "ill-scoped await: expected async type from current scope %s, found async type from other scope %s%s%s"
           (T.string_of_typ_expand t0)
           (T.string_of_typ_expand t2)
           (associated_region env t0 exp.at)
           (associated_region env t2 exp.at);
         scope_info env t0 exp.at;
         scope_info env t2 exp.at;
       end;
       t3
     with Invalid_argument _ ->
       error env exp1.at "M0088"
         "expected async%s type, but expression has type%a%s"
         (if s1 = T.Fut then "" else "*")
         display_typ_expand t1
         (if T.is_async t1 then
            (if s1 = T.Fut then
              "\nUse keyword 'await*' (not 'await' or 'await?') to consume this type."
            else
              "\nUse keyword 'await' or 'await?' (not 'await*') to consume this type.")
          else "")
    )
  | AssertE (_, exp1) ->
    if not env.pre then check_exp_strong env T.bool exp1;
    T.unit
  | AnnotE (exp1, typ) ->
    let t = check_typ env typ in
    if not env.pre then check_exp_strong env t exp1;
    t
  | IgnoreE exp1 ->
    if not env.pre then begin
      check_exp_strong env T.Any exp1;
      if sub env exp1.at exp1.note.note_typ T.unit then
        warn env exp.at "M0089" "redundant ignore, operand already has type ()"
    end;
    T.unit
  | ImportE (f, ri) ->
    check_import env exp.at f ri

and infer_bin_exp env exp1 exp2 =
  match is_explicit_exp exp1, is_explicit_exp exp2 with
  | true, false ->
    let t1 = T.normalize (infer_exp env exp1) in
    if not env.pre then check_exp_weak env t1 exp2;
    t1, t1
  | false, true ->
    let t2 = T.normalize (infer_exp env exp2) in
    if not env.pre then check_exp_weak env t2 exp1;
    t2, t2
  | _ ->
    let t1 = T.normalize (infer_exp env exp1) in
    let t2 = T.normalize (infer_exp env exp2) in
    t1, t2

and infer_exp_field env rf =
  let { mut; id; exp } = rf.it in
  let t = infer_exp env exp in
  let t1 = if mut.it = Syntax.Var then T.Mut t else t in
  Field_sources.add_src env.srcs id.at;
  T.{lab = id.it; typ = t1; src = {empty_src with track_region = id.at}}

and infer_check_bases_fields env (check_fields : T.field list) exp_at exp_bases exp_fields =
  let open List in
  check_ids env "object" "field"
    (map (fun (ef : exp_field) -> ef.it.id) exp_fields);

  let infer_or_check rf =
    let { mut; id; exp } = rf.it in
    match List.find_opt (fun ft -> ft.T.lab = id.it) check_fields with
    | Some exp_field ->
      check_exp_field env rf [exp_field];
      exp_field
    | _ -> infer_exp_field env rf in

  let fts = map infer_or_check exp_fields in
  let bases = map (fun b -> infer_exp_promote env b, b) exp_bases in
  let homonymous_fields ft1 ft2 = T.compare_field ft1 ft2 = 0 in

  (* removing explicit fields from the bases *)
  let strip (base_t, base) =
    let s, base_fts =
      try T.as_obj base_t with Invalid_argument _ ->
        error env base.at "M0093"
          "expected object type, but expression produces type%a"
          display_typ_expand base_t in
    (* forbid actors as bases *)
    if s = T.Actor then
      error env base.at "M0178"
        "actors cannot serve as bases in record extensions";
    T.(Obj (Object, filter (fun ft -> not (exists (homonymous_fields ft) fts)) base_fts))
  in
  let stripped_bases = map strip bases in

  let ambiguous_fields ft1 ft2 =
    homonymous_fields ft1 ft2 &&
    (* allow equivalent type fields *)
    match ft1.T.typ, ft2.T.typ with
    (* homonymous type fields are ambiguous when unequal *)
    | T.Typ c1, T.Typ c2 ->  not (eq env exp_at ft1.T.typ ft2.T.typ)
    (* homonymous value fields are always ambiguous *)
    | _ -> true
  in

  (* field disjointness of stripped bases *)
  let rec disjoint = function
    | [] | [_] -> ()
    | (h, h_exp) :: t ->
       let avoid ft =
         let avoid_fields b b_fts =
           if exists (ambiguous_fields ft) b_fts then
             begin
               let frag_typ, frag_sug = match ft.T.typ with
                 | T.Typ c -> "type ", ""
                 | _ -> "", " (consider overwriting)" in
               info env h_exp.at "%sfield also present in base, here%s" frag_typ frag_sug;
               error env b.at "M0177"
                 "ambiguous %sfield in base%a"
                 frag_typ
                 display_lab ft.T.lab
             end in
         iter (fun (b_t, b) -> avoid_fields b (T.as_obj b_t |> snd)) t in
       iter avoid (T.as_obj h |> snd);
       disjoint t in
  disjoint (map2 (fun b_t b -> b_t, b) stripped_bases exp_bases);

  (* do not allow var fields for now (to avoid aliasing) *)
  begin if not (!Flags.experimental_field_aliasing) then
          let immutable_base b_typ b_exp =
            let constant_field (ft : T.field) =
              if T.(is_mut ft.typ) then
                begin
                  info env b_exp.at "overwrite field to resolve error";
                  error env b_exp.at "M0179"
                    "base has non-aliasable var field%a"
                    display_lab ft.T.lab
                end
            in
            iter constant_field (T.as_obj b_typ |> snd)
          in
          iter2 immutable_base stripped_bases exp_bases
  end;
  let t_base = T.(fold_left (glb ~src_fields:env.srcs) (Obj (Object, [])) stripped_bases) in
  T.(glb ~src_fields:env.srcs t_base (Obj (Object, sort T.compare_field fts)))

and check_exp_strong env t exp =
  check_exp {env with weak = false} t exp

and check_exp_weak env t exp =
  check_exp {env with weak = true} t exp

and check_exp env t exp =
  assert (not env.pre);
  assert (exp.note.note_typ = T.Pre);
  assert (t <> T.Pre);
  let t' = check_exp' env (T.normalize t) exp in
  let e = A.infer_effect_exp exp in
  exp.note <- {exp.note with note_typ = t'; note_eff = e}

and check_exp' env0 t exp : T.typ =
  let env = {env0 with in_prog = false; in_actor = false; context = exp.it :: env0.context } in
  match exp.it, t with
  | PrimE s, T.Func _ ->
    t
  | LitE lit, _ ->
    check_lit env t lit exp.at true;
    t
  | ActorUrlE exp', t' ->
    check_exp_strong env T.text exp';
    begin match T.normalize t' with
    | T.(Obj (Actor, _)) -> t'
    | _ -> error env exp.at "M0090" "actor reference must have an actor type"
    end
  | UnE (ot, op, exp1), _ when Operator.has_unop op t ->
    ot := t;
    check_exp env t exp1;
    t
  | BinE (ot, exp1, op, exp2), _ when Operator.has_binop op t ->
    ot := t;
    check_exp env t exp1;
    check_exp env t exp2;
    if env.weak && op = Operator.SubOp && eq env exp.at t T.nat then
      warn env exp.at "M0155" "operator may trap for inferred type%a"
        display_typ_expand t;
    t
  | ToCandidE exps, _ ->
    if not env.pre then begin
      let ts = List.map (infer_exp env) exps in
      if not (sub env exp.at (T.Prim T.Blob) t) then
        error env exp.at "M0172" "to_candid produces a Blob that is not a subtype of%a"
          display_typ_expand t;
      if not (T.shared (T.seq ts)) then
          error env exp.at "M0173" "to_candid argument must have shared type, but instead has non-shared type%a"
          display_typ_expand (T.seq ts);
      end;
    T.Prim T.Blob
  | FromCandidE exp1, t when T.shared t && T.is_opt t ->
    check_exp env (T.Prim T.Blob) exp1;
    t
  | FromCandidE _, t ->
    error env exp.at "M0174" "from_candid produces an optional shared type, not type%a"
      display_typ_expand t
  | TupE exps, T.Tup ts when List.length exps = List.length ts ->
    List.iter2 (check_exp env) ts exps;
    t
  | ObjE ([], exp_fields) as e, T.Obj(T.Object, fts) -> (* TODO: infer bases? Default does a decent job. *)
    check_ids env "object" "field"
      (List.map (fun (ef : exp_field) -> ef.it.id) exp_fields);
    List.iter (fun ef -> check_exp_field env ef fts) exp_fields;
    if List.for_all (fun ft ->
      if not (List.exists (fun (ef : exp_field) -> ft.T.lab = ef.it.id.it) exp_fields)
      then begin
      local_error env exp.at "M0151"
        "object literal is missing field %s from expected type%a"
        ft.T.lab
        display_typ_expand t;
        false
      end else true
    ) fts
    then detect_lost_fields env t e;
    t
  | OptE exp1, _ when T.is_opt t ->
    check_exp env (T.as_opt t) exp1;
    t
  | DoOptE exp1, _ when T.is_opt t ->
    let env' = add_lab env "!" (T.Prim T.Null) in
    check_exp env' (T.as_opt t) exp1;
    t
  | BangE exp1, t ->
    if Option.is_none (T.Env.find_opt "!" env.labs) then
      local_error env exp.at "M0064" "misplaced '!' (no enclosing 'do ? { ... }' expression)";
    check_exp env (T.Opt t) exp1;
    t
  | ArrayE (mut, exps), T.Array t' ->
    if (mut.it = Var) <> T.is_mut t' then
      local_error env exp.at "M0091"
        "%smutable array expression cannot produce expected type%a"
        (if mut.it = Const then "im" else "")
        display_typ_expand (T.Array t');
    List.iter (check_exp env (T.as_immut t')) exps;
    t
  | AsyncE (par, s1, tb, exp1), T.Async (s2, t1', t') ->
    error_in Flags.[WASIMode; WasmMode] env exp1.at "M0086"
      "async expressions are not supported";
    let t1, next_cap = check_AsyncCap env "async expression" exp.at in
    if s1 <> s2 then begin
      local_error env exp.at "M0183"
        "async expression cannot produce expected async type %a.\n%s"
        display_typ_expand t
        (if s2 = T.Fut then
          "Use keyword 'async' (not 'async*') to produce the expected type."
         else
          "Use keyword 'async*' (not 'async') to produce the expected type.")
    end;
    if not (eq env exp.at t1 t1') then begin
      local_error env exp.at "M0092"
        "async at scope%a\ncannot produce expected scope%a%s%s"
        display_typ_expand t1
        display_typ_expand t1'
        (associated_region env t1 exp.at) (*FIX ME?*)
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
    if not env.pre then check_parenthetical env None par;
    t
  | BlockE decs, _ ->
    ignore (check_block env t decs exp.at);
    t
  | IfE (exp1, exp2, exp3), _ ->
    check_exp_strong env T.bool exp1;
    check_exp env t exp2;
    check_exp env t exp3;
    t
  | SwitchE (exp1, cases), _ ->
    let t1 = infer_exp_promote env exp1 in
    check_cases env t1 t cases;
    coverage_cases "switch" env cases t1 exp.at;
    t
  | TryE (exp1, cases, exp2_opt), _ ->
    check_ErrorCap env "try" exp.at;
    check_exp env t exp1;
    check_cases env T.catch t cases;
    if cases <> []
    then coverage_cases "try handler" env cases T.catch exp.at;
    if not env.pre then
      Option.iter (check_exp_strong { env with async = C.NullCap; rets = None; labs = T.Env.empty; } T.unit) exp2_opt;
    t
  (* TODO: allow shared with one scope par *)
  | FuncE (_, shared_pat,  [], pat, typ_opt, _sugar, exp), T.Func (s, c, [], ts1, ts2) ->
    let sort, ve = check_shared_pat env shared_pat in
    if not env.pre && not env0.in_actor && T.is_shared_sort sort then
      error_in [Flags.ICMode; Flags.RefMode] env exp.at "M0077"
        "a shared function is only allowed as a public field of an actor";
    let ve1 = check_pat_exhaustive (if T.is_shared_sort sort then local_error else warn) env (T.seq ts1) pat in
    let ve2 = T.Env.adjoin ve ve1 in
    let codom = T.codom c (fun () -> assert false) ts2 in
    let t2 = match typ_opt with
      | None -> codom
      | Some typ -> check_typ env typ
    in
    if sort <> s then
      error env exp.at "M0094"
        "%sshared function does not match expected %sshared function type"
        (if sort = T.Local then "non-" else "")
        (if s = T.Local then "non-" else "");
    if not (sub env Source.no_region t2 codom) then
      error env exp.at "M0095"
        "function return type%a\ndoes not match expected return type%a"
        display_typ_expand t2
        display_typ_expand codom;
    let env' =
      { env with
        labs = T.Env.empty;
        rets = Some t2;
        async = C.NullCap; }
    in
    check_exp_strong (adjoin_vals env' ve2) t2 exp;
    t
  | CallE (par_opt, exp1, inst, exp2), _ ->
    let t' = infer_call env exp1 inst exp2 exp.at (Some t) in
    if not (sub env exp1.at t' t) then
      local_error env0 exp.at "M0096"
        "expression of type%a\ncannot produce expected type%a"
        display_typ_expand t'
        display_typ_expand t;
    if not env.pre then check_parenthetical env (Some exp1.note.note_typ) par_opt;
    t'
  | TagE (id, exp1), T.Variant fs when List.exists (fun T.{lab; _} -> lab = id.it) fs ->
    let {T.typ; _} = List.find (fun T.{lab; typ;_} -> lab = id.it) fs in
    check_exp env typ exp1 ;
    t
  | e, _ ->
    let t' = infer_exp env0 exp in
    if not (sub env exp.at t' t) then
    begin
      local_error env0 exp.at "M0096"
        "expression of type%a\ncannot produce expected type%a%s"
        display_typ_expand t'
        display_typ_expand t
        (Suggest.suggest_conversion env.libs env.vals t' t)
    end
    else detect_lost_fields env t e;
    t'

and check_exp_field env (ef : exp_field) fts =
  let { mut; id; exp } = ef.it in
  let ft_opt = List.find_opt (fun ft -> ft.T.lab = id.it) fts in
  match ft_opt with
  | Some { T.typ = T.Mut t; _ } ->
    if mut.it <> Syntax.Var then
      error env ef.at "M0149" "expected mutable 'var' field %s of type%a\nbut found immutable field (insert 'var'?)"
        id.it
        display_typ t;
    check_exp env t exp
  | Some { T.typ = t; _ } ->
    if mut.it = Syntax.Var then
      error env ef.at "M0150" "expected immutable field %s of type%a\nbut found mutable 'var' field (delete 'var'?)"
        id.it
        display_typ t;
    check_exp env t exp
  | None ->
    ignore (infer_exp env exp)

and detect_lost_fields env t = function
  | _ when env.pre || not (T.is_obj t) -> ()
  | ObjE (bs, flds) ->
    let [@warning "-8"] T.Obj (_, fts) = t in
    List.iter
      (fun (fld : exp_field) ->
         let id = fld.it.id.it in
         match T.lookup_val_field_opt id fts with
         | Some _ -> ()
         | None ->
            warn env fld.at "M0215"
              "field `%s` is provided but not expected in record%s of type%a"
              id (if bs = [] then "" else " extension")
              display_typ t)
      flds
  | ObjBlockE (_exp_opt, { it = Type.Object; _}, _typ_opt, dec_fields) ->
    let pub_types, pub_fields = pub_fields dec_fields in
    let [@warning "-8"] T.Obj (_, fts) = t in
    List.iter
      (fun id ->
        match T.lookup_val_field_opt id fts with
        | Some _ -> ()
        | None ->
           warn env ((T.Env.find id pub_fields).id_region) "M0215"
             "public field `%s` is provided but not expected in object of type%a"
             id
             display_typ t)
      (T.Env.keys pub_fields);
    List.iter
      (fun id ->
        match T.lookup_typ_field_opt id fts with
        | Some _ -> ()
        | None ->
           warn env ((T.Env.find id pub_types).id_region) "M0215"
             "public type `%s` is provided but not expected in object of type%a"
             id
             display_typ t)
      (T.Env.keys pub_types)
  | _ -> ()

and infer_call env exp1 inst exp2 at t_expect_opt =
  let n = match inst.it with None -> 0 | Some (_, typs) -> List.length typs in
  let t1 = infer_exp_promote env exp1 in
  let sort, tbs, t_arg, t_ret =
    try T.as_func_sub T.Local n t1
    with Invalid_argument _ ->
      local_error env exp1.at "M0097"
        "expected function type, but expression produces type%a"
        display_typ_expand t1;
      if inst.it = None then
        info env (Source.between exp1.at exp2.at)
          "this looks like an unintended function call, perhaps a missing ';'?";
      T.as_func_sub T.Local n T.Non
  in
  let ts, t_arg', t_ret' =
    match tbs, inst.it with
    | [], (None | Some (_, []))  (* no inference required *)
    | [T.{sort = Scope;_}], _  (* special case to allow t_arg driven overload resolution *)
    | _, Some _ ->
      (* explicit instantiation, check argument against instantiated domain *)
      let typs = match inst.it with None -> [] | Some (_, typs) -> typs in
      let ts = check_inst_bounds env sort tbs typs t_ret at in
      let t_arg' = T.open_ ts t_arg in
      let t_ret' = T.open_ ts t_ret in
      if not env.pre then check_exp_strong env t_arg' exp2;
      ts, t_arg', t_ret'
    | _::_, None -> (* implicit, infer *)
      let t2 = infer_exp env exp2 in
      try
        (* i.e. exists minimal ts .
                t2 <: open_ ts t_arg /\
                t_expect_opt == Some t -> open ts_ t_ret <: t *)
        let ts =
          Bi_match.bi_match_call
            (scope_of_env env)
            (tbs, t_arg, t_ret)
            t2
            t_expect_opt
        in
        let t_arg' = T.open_ ts t_arg in
        let t_ret' = T.open_ ts t_ret in
(*
        if not env.pre then
          info env at "inferred instantiation <%s>"
            (String.concat ", " (List.map T.string_of_typ ts));
*)
        ts, t_arg', t_ret'
      with Bi_match.Bimatch msg ->
        error env at "M0098"
          "cannot implicitly instantiate function of type%a\nto argument of type%a%s\nbecause %s"
          display_typ t1
          display_typ t2
          (match t_expect_opt with
           | None -> ""
           | Some t ->
             Format.asprintf "\nto produce result of type%a" display_typ t)
          msg
  in
  inst.note <- ts;
  if not env.pre then begin
    if Type.is_shared_sort sort then begin
      if not (T.concrete t_arg') then
        error env exp1.at "M0099"
          "shared function argument contains abstract type%a"
          display_typ_expand t_arg';
      if not (T.concrete t_ret') then
        error env exp2.at "M0100"
          "shared function call result contains abstract type%a"
          display_typ_expand t_ret';
    end;
    match T.(is_shared_sort sort || is_async t_ret'), inst.it, tbs with
    | false, Some (true, _), ([] | T.{ sort = Type; _ } :: _) ->
       local_error env inst.at "M0196" "unexpected `system` capability (try deleting it)"
    | false, (None | Some (false, _)), T.{ sort = Scope; _ } :: _ ->
       warn env at "M0195" "this function call implicitly requires `system` capability and may perform undesired actions (please review the call and provide a type instantiation `<system%s>` to suppress this warning)" (if List.length tbs = 1 then "" else ", ...")
    | _ -> ()
  end;
  (* note t_ret' <: t checked by caller if necessary *)
  t_ret'


(* Cases *)

and infer_cases env t_pat t cases : T.typ =
  List.fold_left (infer_case env t_pat) t cases

and infer_case env t_pat t case =
  let {pat; exp} = case.it in
  let ve = check_pat env t_pat pat in
  let initial_usage = enter_scope env in
  let t' = recover_with T.Non (infer_exp (adjoin_vals env ve)) exp in
  leave_scope env ve initial_usage;
  let t'' = T.lub ~src_fields:env.srcs t t' in
  if not env.pre && inconsistent t'' [t; t'] then
    warn env case.at "M0101"
      "the switch has type%a\nbecause branches have inconsistent types,\nthis case produces type%a\nthe previous produce type%a"
      display_typ t''
      display_typ_expand t
      display_typ_expand t';
  t''

and check_cases env t_pat t cases =
  List.iter (check_case env t_pat t) cases

and check_case env t_pat t case =
  let {pat; exp} = case.it in
  let initial_usage = enter_scope env in
  let ve = check_pat env t_pat pat in
  let t' = recover (check_exp (adjoin_vals env ve) t) exp in
  leave_scope env ve initial_usage;
  t'

and inconsistent t ts =
  T.opaque t && not (List.exists T.opaque ts)


(* Patterns *)

and infer_pat_exhaustive warnOrError env pat : T.typ * Scope.val_env =
  let t, ve = infer_pat true env pat in
  if not env.pre then
    coverage_pat warnOrError env pat t;
  t, ve

and infer_pat name_types env pat : T.typ * Scope.val_env =
  assert (pat.note = T.Pre);
  let t, ve = infer_pat' name_types env pat in
  if not env.pre then
    pat.note <- T.normalize t;
  t, ve

and infer_pat' name_types env pat : T.typ * Scope.val_env =
  match pat.it with
  | WildP ->
    error env pat.at "M0102" "cannot infer type of wildcard"
  | VarP _ ->
    error env pat.at "M0103" "cannot infer type of variable"
  | LitP lit ->
    T.Prim (infer_lit env lit pat.at), T.Env.empty
  | SignP (op, lit) ->
    let t1 = T.Prim (infer_lit env lit pat.at) in
    let t = Operator.type_unop op t1 in
    if not (Operator.has_unop op t) then
      error env pat.at "M0059" "operator is not defined for operand type%a"
        display_typ_expand t;
    t, T.Env.empty
  | TupP pats ->
    let ts, ve = infer_pats pat.at env pats [] T.Env.empty in
    T.Tup ts, ve
  | ObjP pfs ->
    let (s, tfs), ve = infer_pat_fields pat.at env pfs [] T.Env.empty in
    T.Obj (s, tfs), ve
  | OptP pat1 ->
    let t1, ve = infer_pat false env pat1 in
    T.Opt t1, ve
  | TagP (id, pat1) ->
    let t1, ve = infer_pat false env pat1 in
    Field_sources.add_src env.srcs id.at;
    T.Variant [T.{lab = id.it; typ = t1; src = {empty_src with track_region = id.at}}], ve
  | AltP (pat1, pat2) ->
    error env pat.at "M0184"
        "cannot infer the type of this or-pattern, please add a type annotation";
    (*let t1, ve1 = infer_pat env pat1 in
    let t2, ve2 = infer_pat env pat2 in
    let t = T.lub ~src_fields:env.srcs t1 t2 in
    if not (T.compatible t1 t2) then
      error env pat.at "M0104"
        "pattern branches have incompatible types,\nleft consumes%a\nright consumes%a"
        display_typ_expand t1
        display_typ_expand t2;
    if T.Env.keys ve1 <> T.Env.keys ve2 then
      error env pat.at "M0189" "different set of bindings in pattern alternatives";
    if not env.pre then T.Env.(iter (fun k t1 -> warn_lossy_bind_type env pat.at k t1 (find k ve2))) ve1;
    t, T.Env.merge (fun _ -> Lib.Option.map2 (T.lub ~src_fields:env.srcs)) ve1 ve2*)
  | AnnotP ({it = VarP id; _} as pat1, typ) when name_types ->
    let t = check_typ env typ in
    T.Named (id.it, t),  check_pat env t pat1
  | AnnotP (pat1, typ) ->
    let t = check_typ env typ in
    t, check_pat env t pat1
  | ParP pat1 ->
    infer_pat name_types env pat1

and infer_pats at env pats ts ve : T.typ list * Scope.val_env =
  match pats with
  | [] -> List.rev ts, ve
  | pat::pats' ->
    let t, ve1 = infer_pat true env pat in
    let ve' = disjoint_union env at "M0017" "duplicate binding for %s in pattern" ve ve1 in
    infer_pats at env pats' (t::ts) ve'

and infer_pat_fields at env pfs ts ve : (T.obj_sort * T.field list) * Scope.val_env =
  match pfs with
  | [] -> (T.Object, List.sort T.compare_field ts), ve
  | { it = TypPF(id); _}::pfs' ->
    (* NOTE(Christoph): see check_pat_fields *)
    if Option.is_none id.note then
      error env at "M0221" "failed to determine type for type pattern field";
    infer_pat_fields at env pfs' ts ve
  | { it = ValPF(id, pat); _}::pfs' ->
    let typ, ve1 = infer_pat false env pat in
    let ve' = disjoint_union env id.at "M0017" "duplicate binding for %s in pattern" ve ve1 in
    Field_sources.add_src env.srcs id.at;
    infer_pat_fields at env pfs' (T.{lab = id.it; typ; src = {empty_src with track_region = id.at}}::ts) ve'

and check_shared_pat env shared_pat : T.func_sort * Scope.val_env =
  match shared_pat.it with
  | T.Local -> T.Local, T.Env.empty
  | T.Shared (ss, pat) ->
    if pat.it <> WildP then
      error_in [Flags.WASIMode; Flags.WasmMode] env pat.at "M0106" "shared function cannot take a context pattern";
    T.Shared ss, check_pat_exhaustive local_error env T.ctxt pat

and check_class_shared_pat env shared_pat obj_sort : Scope.val_env =
  match shared_pat.it, obj_sort.it with
  | T.Local, (T.Module | T.Object) -> T.Env.empty
  | T.Local, T.Actor ->
    T.Env.empty (* error instead? That's a breaking change *)
  | T.Shared (mode, pat), sort ->
    if sort <> T.Actor then
      error env pat.at "M0107" "non-actor class cannot take a context pattern";
    if pat.it <> WildP then
      error_in [Flags.WASIMode; Flags.WasmMode] env pat.at "M0108" "actor class cannot take a context pattern";
    if mode = T.Query then
      error env shared_pat.at "M0109" "class cannot be a query";
    check_pat_exhaustive local_error env T.ctxt pat
  | _, T.Memory -> assert false


and check_pat_exhaustive warnOrError env t pat : Scope.val_env =
  let ve = check_pat env t pat in
  if not env.pre then
    coverage_pat warnOrError env pat t;
  ve

and check_pat env t pat : Scope.val_env =
  check_pat_aux env t pat Scope.Declaration

and check_pat_aux env t pat val_kind : Scope.val_env =
  assert (pat.note = T.Pre);
  if t = T.Pre then snd (infer_pat false env pat) else
  let t' = T.normalize t in
  let ve = check_pat_aux' env t' pat val_kind in
  if not env.pre then pat.note <- t';
  ve

and check_pat_aux' env t pat val_kind : Scope.val_env =
  assert (t <> T.Pre);
  match pat.it with
  | WildP ->
    T.Env.empty
  | VarP id ->
    T.Env.singleton id.it (t, id.at, val_kind)
  | LitP lit ->
    if not env.pre then begin
      let t' = if eq env pat.at t T.nat then T.int else t in  (* account for Nat <: Int *)
      if T.opaque t' then
        error env pat.at "M0110" "literal pattern cannot consume expected type%a"
          display_typ_expand t;
      if sub env pat.at t' T.Non
      then ignore (infer_lit env lit pat.at)
      else check_lit env t' lit pat.at false
    end;
    T.Env.empty
  | SignP (op, lit) ->
    if not env.pre then begin
      let t' = if eq env pat.at t T.nat then T.int else t in  (* account for Nat <: Int *)
      if not (Operator.has_unop op (T.promote t)) then
        error env pat.at "M0111" "operator pattern cannot consume expected type%a"
          display_typ_expand t;
      if sub env pat.at t' T.Non
      then ignore (infer_lit env lit pat.at)
      else check_lit env t' lit pat.at false
    end;
    T.Env.empty
  | TupP pats ->
    let ts = try T.as_tup_sub (List.length pats) t with Invalid_argument _ ->
      error env pat.at "M0112" "tuple pattern cannot consume expected type%a"
         display_typ_expand t
    in check_pats env ts pats T.Env.empty pat.at
  | ObjP pfs ->
    let pfs' = List.stable_sort compare_pat_field pfs in
    let s, tfs =
      try T.as_obj_sub (List.filter_map (fun pf ->
        match pf.it with
        | TypPF(_) -> None
        | ValPF(id, _) -> Some(id.it)) pfs') t
      with Invalid_argument _ ->
        error env pat.at "M0113" "object pattern cannot consume expected type%a"
          display_typ_expand t
    in
    if not env.pre && s = T.Actor then
      local_error env pat.at "M0114" "object pattern cannot consume actor type%a"
        display_typ_expand t;
    check_pat_fields env t tfs pfs' T.Env.empty pat.at
  | OptP pat1 ->
    let t1 = try T.as_opt_sub t with Invalid_argument _ ->
      error env pat.at "M0115" "option pattern cannot consume expected type%a"
        display_typ_expand t
    in check_pat env t1 pat1
  | TagP (id, pat1) ->
    let t1 =
      try
        match T.lookup_val_field_opt id.it (T.as_variant_sub id.it t) with
        | Some t1 -> t1
        | None -> T.Non
      with Invalid_argument _ ->
        error env pat.at "M0116" "variant pattern cannot consume expected type%a"
          display_typ_expand t
    in check_pat env t1 pat1
  | AltP (pat1, pat2) ->
    let ve1 = check_pat env t pat1 in
    let ve2 = check_pat env t pat2 in
    if T.Env.keys ve1 <> T.Env.keys ve2 then
      error env pat.at "M0189" "different set of bindings in pattern alternatives";
    T.Env.(iter (fun k (t1, _, _) ->
      let (t2, _, _) = find k ve2 in
      warn_lossy_bind_type env pat.at k t1 t2)
    ) ve1;
    let merge_entries (t1, at1, kind1) (t2, at2, kind2) = (T.lub ~src_fields:env.srcs t1 t2, at1, kind1) in
    T.Env.merge (fun _ -> Lib.Option.map2 merge_entries) ve1 ve2
  | AnnotP (pat1, typ) ->
    let t' = check_typ env typ in
    if not (sub env pat.at t t') then
      error env pat.at "M0117"
        "pattern of type%a\ncannot consume expected type%a"
        display_typ_expand t'
        display_typ_expand t;
    check_pat env t' pat1
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
  let ts_len = List.length ts in
  let pats_len = List.length pats in
  let rec go ts pats ve =
    match ts, pats with
    | [], [] -> ve
    | t::ts', pat::pats' ->
        let ve1 = check_pat env t pat in
        let ve' = disjoint_union env at "M0017" "duplicate binding for %s in pattern" ve ve1 in
        go ts' pats' ve'
    | _, _ ->
        error env at "M0118" "tuple pattern has %i components but expected type has %i"
          pats_len ts_len
  in
  go ts pats ve

and check_pat_fields env t tfs pfs ve at : Scope.val_env =
  match tfs, pfs with
  | _, [] -> ve
  | _, {it = TypPF(id); at; _}::pfs' ->
    (* NOTE(Christoph): We check the note to see if we were able to
       resolve this type field in the "types-only" pass. *)
    if Option.is_none id.note then
      error env at "M0221" "failed to determine type for type pattern field";
    check_pat_fields env t tfs pfs' ve at
  | [], {it = ValPF(id, _); at; _}::_ ->
     error env at "M0119"
       "object field %s is not contained in expected type%a"
       id.it
       display_typ_expand t
  | tf::tfs', pf::pfs' ->
     match compare_pat_typ_field tf pf with
     | -1 -> check_pat_fields env t tfs' pfs ve at
     | 1 -> (match pf.it with
            | ValPF(_) -> check_pat_fields env t [] pfs ve at
            | _ -> check_pat_fields env t tfs pfs' ve at)
     | _ -> match tf, pf with
        | T.{lab; typ; src}, {it = ValPF(id, pat); _} ->
           if T.is_mut typ then
             error env pf.at "M0120" "cannot pattern match mutable field %s" lab;
           check_deprecation env pf.at "field" lab src.T.depr;
           let val_kind = kind_of_field_pattern pf in
           let ve1 = check_pat_aux env typ pat val_kind in
           let ve' =
             disjoint_union env at "M0017" "duplicate binding for %s in pattern" ve ve1 in
           (match pfs' with
           | {it = ValPF(id', _); at = at';_}::_ when id'.it = lab ->
              error env at' "M0121" "duplicate field %s in object pattern" lab
           | _ -> check_pat_fields env t tfs' pfs' ve' at)
        | _, _ -> assert false

and check_pat_typ_dec env t pat : Scope.typ_env =
  match pat.it, T.promote t with
  | (WildP, _) | (SignP _, _) | (LitP _, _) ->
    T.Env.empty
  | ObjP pfs, T.Obj (s, tfs) ->
    let pfs' = List.stable_sort compare_pat_field pfs in
    check_pat_fields_typ_dec env t tfs pfs' T.Env.empty pat.at
  | TagP (id, pat1), T.Variant tfs ->
    begin match T.lookup_val_field_opt id.it tfs with
      | Some t1 -> check_pat_typ_dec env t1 pat1
      | None -> T.Env.empty
    end
  | TupP pats, T.Tup ts ->
    check_pats_typ_dec env ts pats T.Env.empty pat.at
  | ParP pat1, _ ->
    check_pat_typ_dec env t pat1
  | OptP pat1, T.Opt t_opt ->
    check_pat_typ_dec env t_opt pat1
  | AltP (pat1, pat2), _ ->
    let te1 = check_pat_typ_dec env t pat1 in
    let te2 = check_pat_typ_dec env t pat2 in
    if T.Env.keys te1 <> T.Env.keys te2 then
      error env pat.at "M0189" "different set of type bindings in pattern alternatives";
    let compare_cons (c1 : T.con) (c2 : T.con) = eq_kind env pat.at (Mo_types.Cons.kind c1) (Mo_types.Cons.kind c2) in
    let _ = T.Env.merge (fun s con1 con2 ->
      if not (compare_cons (Option.get con1) (Option.get con2)) then
        (* TODO Actually report the mismatched types *)
        error env pat.at "M0189" "mismatched types for type %s in patterns" s
      else None) te1 te2 in
    te1
  | _, _ -> T.Env.empty

and check_pats_typ_dec env ts pats te at : Scope.typ_env =
  let ts_len = List.length ts in
  let pats_len = List.length pats in
  let rec go ts pats te =
    match ts, pats with
    | [], [] -> te
    | t::ts', pat::pats' ->
        let te1 = check_pat_typ_dec env t pat in
        let te' = disjoint_union env at "M0017" "duplicate binding for type %s in pattern" te te1 in
        go ts' pats' te'
    | _, _ ->
        error env at "M0118" "tuple pattern has %i components but expected type has %i"
          pats_len ts_len
  in
  go ts pats te

and check_pat_fields_typ_dec env t tfs pfs te at : Scope.typ_env = match tfs, pfs with
  | _, [] -> te
  | [], {it = TypPF(id); at; _}::_ ->
    error env at "M0119"
      "object type field %s is not contained in expected type%a"
      id.it
      display_typ_expand t
  | [], {it = ValPF(_); _}::pfs' ->
     check_pat_fields_typ_dec env t tfs pfs' te at
  | tf::tfs', pf::pfs' ->
     match compare_pat_typ_field tf pf with
     | -1 -> check_pat_fields_typ_dec env t tfs' pfs te at
     | 1 ->
        (match pf.it with
        | TypPF(_) -> check_pat_fields_typ_dec env t [] pfs te at
        | _ -> check_pat_fields_typ_dec env t tfs pfs' te at)
     | _ ->
        match tf, pf with
        | T.{lab; typ = Typ t'; _}, { it = TypPF(id); _ } ->
           id.note <- Some t';
           let te' = T.Env.add id.it t' te in
           begin match pfs' with
           | {it = TypPF(id'); at = at';_}::_ when id'.it = lab ->
              error env at' "M0121" "duplicate type field %s in object pattern" lab
           | _ -> check_pat_fields_typ_dec env t tfs' pfs' te' at
           end
        | T.{lab; typ; _}, { it = ValPF(id, p); _ } ->
           let te1 = check_pat_typ_dec env typ p in
           disjoint_union env at "M0017" "duplicate binding for %s in pattern" te te1
        | _ -> assert false

(* Objects *)

and nonpub_fields dec_fields : visibility_env =
  List.fold_right nonpub_field dec_fields T.Env.(empty, empty)

and nonpub_field dec_field xs : visibility_env =
  match dec_field.it with
  | {vis = { it = Private | System; _}; dec; _} ->
    vis_dec T.{depr = None; track_region = no_region; region = dec_field.at} dec xs
  | _ -> xs

and pub_fields dec_fields : visibility_env =
  List.fold_right pub_field dec_fields T.Env.(empty, empty)

and pub_field dec_field xs : visibility_env =
  match dec_field.it with
  | {vis = { it = Public depr; _}; dec; _} ->
    vis_dec T.{depr = depr; track_region = no_region; region = dec_field.at} dec xs
  | _ -> xs

and vis_dec src dec xs : visibility_env =
  match dec.it with
  | ExpD _ -> xs
  | LetD (pat, _, _) -> vis_pat src pat xs
  | VarD (id, _) -> vis_val_id src id xs
  | ClassD (_, _, _, id, _, _, _, _, _) ->
    vis_val_id src {id with note = ()} (vis_typ_id src id xs)
  | TypD (id, _, _) -> vis_typ_id src id xs

and vis_pat src pat xs : visibility_env =
  match pat.it with
  | WildP | LitP _ | SignP _ -> xs
  | VarP id -> vis_val_id src id xs
  | TupP pats -> List.fold_right (vis_pat src) pats xs
  | ObjP pfs -> List.fold_right (vis_pat_field src) pfs xs
  | AltP (pat1, _)
  | OptP pat1
  | TagP (_, pat1)
  | AnnotP (pat1, _)
  | ParP pat1 -> vis_pat src pat1 xs

and vis_pat_field src pf xs =
  match pf.it with
  | ValPF(_, pat) -> vis_pat src pat xs
  | TypPF(id) -> (* TODO? *) xs

and vis_typ_id src id (xs, ys) : visibility_env =
  (T.Env.add id.it T.{depr = src.depr; id_region = id.at; field_region = src.region} xs, ys)

and vis_val_id src id (xs, ys) : visibility_env =
  (xs, T.Env.add id.it T.{depr = src.depr; id_region = id.at; field_region = src.region} ys)


(* Object/Scope transformations *)

(* TODO: remove by merging conenv and valenv or by separating typ_fields *)

and object_of_scope env sort dec_fields scope at =
  let pub_typ, pub_val = pub_fields dec_fields in
  let tfs =
    T.Env.fold
      (fun id c tfs ->
        match T.Env.find_opt id pub_typ with
        | Some src ->
          Field_sources.add_src env.srcs src.id_region;
          T.{lab = id; typ = T.Typ c; src = {depr = src.depr; track_region = src.id_region; region = src.field_region}}::tfs
        | _ -> tfs
      ) scope.Scope.typ_env  []
  in
  let tfs' =
    T.Env.fold
      (fun id (t, _, _) tfs ->
        match T.Env.find_opt id pub_val with
        | Some src ->
          Field_sources.add_src env.srcs src.id_region;
          T.{lab = id; typ = t; src = {depr = src.depr; track_region = src.id_region; region = src.field_region}}::tfs
        | _ -> tfs
      ) scope.Scope.val_env tfs
  in

  (* Add sources for private fields. *)
  if !Flags.typechecker_combine_srcs then begin
    let nonpub_typ, nonpub_val = nonpub_fields dec_fields in
    let add_srcs ids =
      T.Env.iter (fun id _env ->
        match T.Env.find_opt id ids with
        | None -> ()
        | Some src -> Field_sources.add_src env.srcs src.id_region)
    in
    add_srcs nonpub_typ scope.Scope.typ_env;
    add_srcs nonpub_val scope.Scope.val_env;
  end;

  Lib.List.iter_pairs
    (fun x y ->
      if not (T.is_typ x.T.typ) && not (T.is_typ y.T.typ) &&
         Hash.hash x.T.lab = Hash.hash y.T.lab
      then error env at "M0122" "field names %s and %s in %sobject type have colliding hashes"
        x.T.lab y.T.lab (T.string_of_obj_sort sort);
    ) tfs';

  T.Obj (sort, List.sort T.compare_field tfs')

and is_actor_method dec : bool = match dec.it with
  | LetD ({it = VarP _; _}, {it = FuncE (_, shared_pat, _, _, _, _, _); _}, _) ->
    T.is_shared_sort shared_pat.it
  | _ -> false

and is_typ_dec dec : bool = match dec.it with
  | TypD _ -> true
  | _ -> false

and infer_obj env obj_sort exp_opt dec_fields at : T.typ =
  let s = obj_sort.it in
  let private_fields =
    let scope = List.filter (fun field -> is_private field.it.vis) dec_fields
    |> List.map (fun field -> field.it.dec)
    |> gather_block_decs env in
    get_identifiers scope.Scope.val_env
  in
  let private_identifiers identifiers =
    T.Env.filter (fun id _ -> S.mem id private_fields) identifiers
  in
  let env =
    if s <> T.Actor then
      { env with in_actor = false }
    else
      { env with
        in_actor = true;
        labs = T.Env.empty;
        rets = None;
      }
  in
  let decs = List.map (fun (df : dec_field) -> df.it.dec) dec_fields in
  let initial_usage = enter_scope env in
  let _, scope = infer_block env decs at false in
  let t = object_of_scope env s dec_fields scope at in
  leave_scope env (private_identifiers scope.Scope.val_env) initial_usage;
  let (_, tfs) = T.as_obj t in
  if not env.pre then begin
    if s = T.Actor then begin
      List.iter (fun T.{lab; typ; _} ->
        if not (T.is_typ typ) && not (T.is_shared_func typ) then
          let _, pub_val = pub_fields dec_fields in
          error env ((T.Env.find lab pub_val).id_region) "M0124"
            "public actor field %s has non-shared function type%a"
            lab
            display_typ_expand typ
      ) tfs;
      List.iter (fun df ->
        if is_public df.it.vis && not (is_actor_method df.it.dec) && not (is_typ_dec df.it.dec) then
          local_error env df.it.dec.at "M0125"
            "public actor field needs to be a manifest function"
      ) dec_fields;
      List.iter (fun df ->
        if df.it.vis.it = Syntax.Private && is_actor_method df.it.dec then
          error_in [Flags.ICMode; Flags.RefMode] env df.it.dec.at "M0126"
            "a shared function cannot be private"
      ) dec_fields;
    end;
    if s = T.Module then Static.dec_fields env.msgs dec_fields;
    check_system_fields env s scope tfs dec_fields;
    let stab_tfs = check_stab env obj_sort scope dec_fields in
    check_migration env stab_tfs exp_opt
  end;
  t

and check_parenthetical env typ_opt = function
  | None -> ()
  | Some par ->
     let env = { env with async = C.NullCap } in
     begin match typ_opt with
     | Some fun_ty when T.is_func fun_ty ->
       let s, _, _, _, ts2 = T.as_func fun_ty in
       begin match ts2 with
       | _ when T.is_shared_sort s -> ()
       | [cod] when T.is_async cod -> ()
       | _ -> warn env par.at "M0210" "misplaced parenthetical (this call does not send a message)"
       end
     | _ -> ()
     end;
     let checked = T.[ cycles_fld; timeout_fld ] in
     let [@warning "-8"] par_infer env { it = ObjE (bases, fields); _ } =
       infer_check_bases_fields env checked par.at bases fields in
     let attrs = infer_exp_wrapper par_infer T.as_immut env par in
     let [@warning "-8"] T.Object, attrs_flds = T.as_obj attrs in
     if attrs_flds = [] then warn env par.at "M0211" "redundant empty parenthetical note";
     let check_lab { T.lab; typ; _ } =
       let check want =
         if not (sub env par.at typ want)
         then local_error env par.at "M0214" "field %s in parenthetical is declared with type%a\ninstead of expected type%a" lab
                display_typ typ
                display_typ want in
       match List.find_opt (fun { T.lab = l; _} -> l = lab) checked with
       | Some { T.typ; _} -> check typ
       | None -> () in
     List.iter check_lab attrs_flds;
     let unrecognised = List.(filter T.(fun {lab; _} -> lab <> cycles_lab && lab <> timeout_lab) attrs_flds |> map (fun {T.lab; _} -> lab)) in
     if unrecognised <> [] then warn env par.at "M0212" "unrecognised attribute %s in parenthetical" (List.hd unrecognised);

and check_system_fields env sort scope tfs dec_fields =
  List.iter (fun df ->
    match sort, df.it.vis.it, df.it.dec.it with
    | T.Actor, vis,
      LetD({ it = VarP id; _ },
           { it = FuncE _; _ },
           _) ->
      begin
        match List.assoc_opt id.it (system_funcs tfs) with
        | Some t ->
          (* TBR why does Stable.md require this to be a manifest function, not just any expression of appropriate type?  *)
          if vis = System then
            begin
              let (t1, _, _) = T.Env.find id.it scope.Scope.val_env in
              if not (sub env id.at t1 t) then
                local_error env df.at "M0127" "system function %s is declared with type%a\ninstead of expected type%a" id.it
                   display_typ t1
                   display_typ t
              else if id.it = "timer" && not !Mo_config.Flags.global_timer then
                local_error env df.at "M0182" "system function timer is present but -no-timer flag is specified"
            end
         else warn env id.at "M0128" "this function has the name of a system method, but is declared without system visibility and will not be called by the system"
        | None ->
          if vis = System then
            local_error env id.at "M0129" "unexpected system method named %s, expected %s"
              id.it (String.concat " or " (List.map fst (system_funcs tfs)))
          else ()
      end
    | _, System, _ ->
      local_error env df.it.vis.at "M0130" "misplaced system visibility, did you mean private?"
    | _ -> ())
  dec_fields

and stable_pat pat =
  match pat.it with
  | VarP _ -> true
  | ParP pat'
  | AnnotP (pat', _) -> stable_pat pat'
  | _ -> false

and infer_migration env obj_sort exp_opt =
  Option.map
    (fun exp ->
      if obj_sort.it <> T.Actor then
        local_error env exp.at "M0209"
          "misplaced actor migration expression on module or object";
      infer_exp_promote { env with async = C.NullCap; rets = None; labs = T.Env.empty } exp)
    exp_opt

and check_migration env (stab_tfs : T.field list) exp_opt =
  match exp_opt with
  | None -> ()
  | Some exp ->
    let focus = match exp.it with
      | ObjE(_, flds) ->
        (match List.find_opt (fun ({it = {id; _}; _} : exp_field) -> id.it = T.migration_lab) flds with
         | Some fld -> fld.at
         | None -> exp.at)
      | _ -> exp.at in
    Static.exp env.msgs exp; (* preclude side effects *)
    let check_fields desc typ =
      match typ with
      | T.Obj(T.Object, tfs) ->
         if not (T.stable typ) then
           local_error env focus "M0201"
             "expected stable type, but migration expression %s non-stable type%a"
             desc
             display_typ_expand typ;
         tfs
      | _ ->
         local_error env focus "M0202"
           "expected object type, but migration expression %s non-object type%a"
           desc
           display_typ_expand typ;
         []
   in
   let typ =
     try
       let s, tfs = T.as_obj_sub [T.migration_lab] exp.note.note_typ in
       if s = T.Actor then raise (Invalid_argument "");
       T.lookup_val_field T.migration_lab tfs
     with Invalid_argument _ ->
       error env focus "M0208"
         "expected expression with field `migration`, but expression has type%a"
         display_typ_expand exp.note.note_typ
   in
   let dom_tfs, rng_tfs =
     try
      let sort, tbs, t_dom, t_rng = T.as_func_sub T.Local 0 typ in
      if sort <> T.Local || tbs <> [] then raise (Invalid_argument "");
      check_fields "consumes" (T.normalize t_dom),
      check_fields "produces" (T.promote t_rng)
     with Invalid_argument _ ->
       local_error env focus "M0203"
         "expected non-generic, local function type, but migration expression produces type%a"
         display_typ_expand typ;
       [], []
   in
   List.iter
     (fun tf ->
       match T.lookup_val_field_opt tf.T.lab rng_tfs with
       | None -> ()
       | Some typ ->
         if not (T.stable_sub ~src_fields:env.srcs (T.as_immut typ) (T.as_immut tf.T.typ)) then
           local_error env focus "M0204"
             "migration expression produces field `%s` of type%a\n, not the expected type%a"
              tf.T.lab
              display_typ_expand typ
              display_typ_expand tf.T.typ) stab_tfs;
   (* Construct the pre signature *)
   let pre_tfs = List.sort T.compare_field
      dom_tfs @
        (List.filter_map
           (fun tf ->
             match T.lookup_val_field_opt tf.T.lab dom_tfs, T.lookup_val_field_opt tf.T.lab rng_tfs with
             | _, Some _  (* ignore consumed (overridden) *)
             | Some _, _ -> (* ignore produced (provided) *)
               None
             | None, None ->
               (* retain others *)
               Some tf)
           stab_tfs)
   in
   (* Check for duplicates and hash collisions in pre-signature *)
   let pre_ids = List.map (fun tf -> T.{it = tf.lab; at = tf.src.region; note = ()}) pre_tfs in
   check_ids env "pre actor type" "stable variable" pre_ids;
   (* Reject any fields in range not in post signature (unintended data loss) *)
   let stab_ids = List.map (fun tf -> tf.T.lab) stab_tfs in
   List.iter (fun T.{lab;typ;src} ->
     match typ with
     | T.Typ c -> ()
     | _ ->
       match T.lookup_val_field_opt lab stab_tfs with
       | Some _ -> ()
       | None ->
         local_error env focus "M0205"
           "migration expression produces unexpected field `%s` of type%a\n%s\n%s"
            lab
            display_typ_expand typ
            (Suggest.suggest_id "field" lab stab_ids)
           "The actor should declare a corresponding `stable` field.")
     rng_tfs;
   (* Warn about any field in domain, not in range, and declared stable in actor *)
   (* This may indicate unintentional data loss. *)
   List.iter (fun T.{lab;typ;src} ->
     match typ with
     | T.Typ c -> ()
     | _ ->
       match T.lookup_val_field_opt lab rng_tfs with
       | Some _ -> ()
       | None ->
         if List.mem lab stab_ids then
           (* re-initialized *)
           warn env focus "M0206"
             "migration expression consumes field `%s` of type%a\nbut does not produce it, yet the field is declared in the actor.\n%s\n%s"
             lab
             display_typ_expand typ
             "The declaration in the actor will be reinitialized, discarding its consumed value."
             "If reinitialization is unintended, and you want to preserve the consumed value, either remove this field from the parameter of the migration function or add it to the result of the migration function."
         else
           (* dropped *)
           warn env focus "M0207"
             "migration expression consumes field `%s` of type%a\nbut does not produce it. The field is not declared in the actor.\n%s\n%s"
             lab
             display_typ_expand typ
             "This field will be removed from the actor, discarding its consumed value."
             "If this removal is unintended, declare the field in the actor and either remove the field from the parameter of the migration function or add it to the result of the migration function."
     )
     dom_tfs;
   (* Warn the user about unrecognised attributes. *)
   let [@warning "-8"] T.Object, attrs_flds = T.as_obj exp.note.note_typ in
   let unrecognised = List.(filter (fun {T.lab; _} -> lab <> T.migration_lab) attrs_flds |> map (fun {T.lab; _} -> lab)) in
   if unrecognised <> [] then warn env exp.at "M0212" "unrecognised attribute %s in parenthetical note" (List.hd unrecognised);


and check_stable_defaults env sort dec_fields =
  if sort.it <> T.Actor then () else
  let declared_persistent = sort.note.it in
  if declared_persistent then
    begin
      if !Flags.actors = Flags.DefaultPersistentActors && sort.note.at <> no_region then
        warn env sort.note.at "M0217" "with flag --default-persistent-actors, the `persistent` keyword is redundant and can be removed";
      List.iter (fun dec_field ->
        match dec_field.it.stab, dec_field.it.dec.it with
        | Some {it = Stable; at; _}, (LetD _ | VarD _) ->
          if at <> Source.no_region then
            warn env at "M0218" "redundant `stable` keyword, this declaration is implicitly stable"
        | _ -> ())
      dec_fields
    end
  else
    (* non-`persistent` *)
    if !Flags.actors = Flags.RequirePersistentActors then
    let has_implicit_flexible =
      List.fold_left (fun acc dec_field ->
        match dec_field.it.stab, dec_field.it.dec.it with
        | Some {it = Flexible; at; _}, (LetD _ | VarD _) ->
           if at = Source.no_region
           then
             (local_error env dec_field.it.dec.at "M0219" "this declaration is currently implicitly transient, please declare it explicitly `transient`";
              true)
           else acc
        | _ -> acc)
        false dec_fields
    in
    if not has_implicit_flexible then
      local_error env sort.at "M0220" "this actor or actor class should be declared `persistent`"

and check_stab env sort scope dec_fields =
  let check_stable id at =
    match T.Env.find_opt id scope.Scope.val_env with
    | None -> assert false
    | Some (t, _, _) ->
      let t1 = T.as_immut t in
      if not (T.stable t1) then
        local_error env at "M0131"
          "variable %s is declared stable but has non-stable type%a" id
          display_typ t1;
  in
  let idss = List.map (fun df ->
    match sort.it, df.it.stab, df.it.dec.it with
    | (T.Object | T.Module), None, _ -> []
    | (T.Object | T.Module), Some stab, _ ->
      local_error env stab.at "M0132"
        "misplaced stability declaration on field of non-actor";
      []
    | T.Actor, Some {it = Stable; _}, VarD (id, _) ->
      check_stable id.it id.at;
      [id]
    | T.Actor, Some {it = Stable; _}, LetD (pat, _, _) when stable_pat pat ->
      let ids = T.Env.keys (gather_pat env Scope.empty pat).Scope.val_env in
      List.iter (fun id -> check_stable id pat.at) ids;
      List.map (fun id -> {it = id; at = pat.at; note = ()}) ids;
    | T.Actor, Some {it = Flexible; _} , (VarD _ | LetD _) -> []
    | T.Actor, Some stab, _ ->
      local_error env stab.at "M0133"
        "misplaced stability modifier: allowed on var or simple let declarations only";
      []
    | _ -> []) dec_fields
  in
  let ids = List.concat idss in
  check_ids env "actor type" "stable variable" ids;
  check_stable_defaults env sort dec_fields;
  List.sort T.compare_field
    (List.map
      (fun id ->
         let typ, _, _ = T.Env.find id.it scope.Scope.val_env in
         Field_sources.add_src env.srcs id.at;
         T.{ lab = id.it;
             typ;
             src = {depr = None; track_region = id.at; region = id.at}})
      ids)

(* Blocks and Declarations *)

and infer_block env decs at check_unused : T.typ * Scope.scope =
  let initial_usage = enter_scope env in
  let scope = infer_block_decs env decs at in
  let env' = adjoin env scope in
  (* HACK: when compiling to IC, mark class constructors as unavailable *)
  let ve = match !Flags.compile_mode with
    | Flags.(ICMode | RefMode) ->
      List.fold_left (fun ve' dec ->
        match dec.it with
        | ClassD(_, _, { it = T.Actor; _}, id, _, _, _,  _, _) ->
          T.Env.mapi (fun id' (typ, at, kind, avl) ->
            (typ, at, kind, if id' = id.it then Unavailable else avl)) ve'
        | _ -> ve') env'.vals decs
    | _ -> env'.vals
  in
  let t = infer_block_exps { env' with vals = ve } decs in
  if check_unused then
    leave_scope env scope.Scope.val_env initial_usage
  else ();
  t, scope

and infer_block_decs env decs at : Scope.t =
  let scope = gather_block_decs env decs in
  let env' = adjoin {env with pre = true} scope in
  let scope_ce = infer_block_typdecs env' decs in
  check_con_env env' at scope_ce.Scope.con_env;
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
    infer_block_exps env decs'

and infer_dec env dec : T.typ =
  let t =
  match dec.it with
  | ExpD exp -> infer_exp env exp
  | LetD (pat, exp, None) ->
    (* For developer convenience, ignore top-level actor and module identifiers in unused detection. *)
    (if env.in_prog && (CompUnit.is_actor_def exp || CompUnit.is_module_def exp) then
      match pat.it with
      | VarP id -> use_identifier env id.it
      | _ -> ());
    let t = infer_exp env exp in
    if !Flags.typechecker_combine_srcs then
      combine_pat_srcs env t pat;
    t
  | LetD (pat, exp, Some fail) ->
    if not env.pre then
      check_exp env T.Non fail;
    let t = infer_exp env exp in
    if !Flags.typechecker_combine_srcs then
      combine_pat_srcs env t pat;
    t
  | VarD (id, exp) ->
    if not env.pre then begin
      let t = infer_exp env exp in
      if !Flags.typechecker_combine_srcs then
        combine_id_srcs env t id
    end;
    T.unit
  | ClassD (exp_opt, shared_pat, obj_sort, id, typ_binds, pat, typ_opt, self_id, dec_fields) ->
    let (t, _, _, _) = T.Env.find id.it env.vals in
    if not env.pre then begin
      let c = T.Env.find id.it env.typs in
      let _typ_opt = infer_migration env obj_sort exp_opt in
      let ve0 = check_class_shared_pat env shared_pat obj_sort in
      let cs, tbs, te, ce = check_typ_binds env typ_binds in
      let env' = adjoin_typs env te ce in
      let in_actor = obj_sort.it = T.Actor in
      (* Top-level actor class identifier is implicitly public and thus considered used. *)
      if env.in_prog && in_actor then use_identifier env id.it;
      let t_pat, ve =
        infer_pat_exhaustive (if in_actor then error else warn) env' pat
      in
      if in_actor && not (T.shared t_pat) then
        error_shared env t_pat pat.at "M0034"
          "shared constructor has non-shared parameter type%a"
          display_typ_expand t_pat;
      let env'' = adjoin_vals (adjoin_vals env' ve0) ve in
      let async_cap, _, class_cs = infer_class_cap env obj_sort.it tbs cs in
      let self_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) class_cs) in
      let env''' =
        { (add_val env'' self_id self_typ) with
          labs = T.Env.empty;
          rets = None;
          async = async_cap;
          in_actor;
        }
      in
      let initial_usage = enter_scope env''' in
      let t' = infer_obj { env''' with check_unused = true } obj_sort exp_opt dec_fields dec.at in
      leave_scope env ve initial_usage;
      match typ_opt, obj_sort.it with
      | None, _ -> ()
      | Some { it = AsyncT (T.Fut, _, typ); at; _ }, T.Actor
      | Some ({ at; _ } as typ), T.(Module | Object) ->
        if at = Source.no_region then
          warn env dec.at "M0135"
            "actor classes with non non-async return types are deprecated; please declare the return type as 'async ...'";
        let t'' = check_typ env'' typ in
        if not (sub env dec.at t' t'') then
          local_error env dec.at "M0134"
            "class body of type%a\ndoes not match expected type%a"
            display_typ_expand t'
            display_typ_expand t''
        else ObjBlockE (exp_opt, obj_sort, (None, typ_opt), dec_fields) |> detect_lost_fields env t''
      | Some typ, T.Actor ->
         local_error env dec.at "M0193" "actor class has non-async return type"
      | _, T.Memory -> assert false
    end;
    T.normalize t
  | TypD _ ->
    T.unit
  in
  let eff = A.infer_effect_dec dec in
  dec.note <- {empty_typ_note with note_typ = t; note_eff = eff};
  t


and check_block env t decs at : Scope.t =
  let initial_usage = enter_scope env in
  let scope = infer_block_decs env decs at in
  check_block_exps (adjoin env scope) t decs at;
  leave_scope env scope.Scope.val_env initial_usage;
  scope

and check_block_exps env t decs at =
  match decs with
  | [] ->
    if not (sub env at T.unit t) then
      local_error env at "M0136" "empty block cannot produce expected type%a"
        display_typ_expand t
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
    if not (eq env dec.at t T.unit || sub env dec.at t' t) then
      local_error env dec.at "M0096"
        "expression of type%a\ncannot produce expected type%a"
        display_typ_expand t'
        display_typ_expand t

and infer_val_path env exp : T.typ option =
  match exp.it with
  | ImportE (f, ri) ->
    Some (check_import env exp.at f ri)
  | VarE id ->
    (match T.Env.find_opt id.it env.vals with (* TBR: return None for Unavailable? *)
     | Some (t, _, _, _) -> Some t
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
    Some (check_typ {env with pre = true} typ)
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
  | ExpD _ -> scope
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _},
      ( {it = ObjBlockE (_, obj_sort, _, dec_fields); at; _}
      | {it = AwaitE (_, { it = AsyncE (_, _, _, { it = ObjBlockE (_, ({ it = Type.Actor; _} as obj_sort), _, dec_fields); at; _ }) ; _  }); _ }),
       _
    ) ->
    let decs = List.map (fun df -> df.it.dec) dec_fields in
    let open Scope in
    if T.Env.mem id.it scope.val_env then
      error_duplicate env "" id;
    let scope' = gather_block_decs env decs in
    let ve' = add_id scope.val_env id (object_of_scope env obj_sort.it dec_fields scope' at) in
    let obj_env = T.Env.add id.it scope' scope.obj_env in
    { val_env = ve';
      typ_env = scope.typ_env;
      lib_env = scope.lib_env;
      con_env = scope.con_env;
      obj_env = obj_env;
      fld_src_env = scope.fld_src_env;
    }
  | LetD (pat, _, _) -> gather_pat env scope pat
  | VarD (id, _) -> Scope.adjoin_val_env scope (gather_id env scope.Scope.val_env id Scope.Declaration)
  | TypD (id, binds, _) | ClassD (_, _, _, id, binds, _, _, _, _) ->
    let open Scope in
    if T.Env.mem id.it scope.typ_env then
      error_duplicate env "type " id;
    let binds' = match binds with
      | bind::binds when bind.it.sort.it = T.Scope ->
        binds
      | _ -> binds
    in
    let pre_tbs = List.map (fun bind ->
      { T.var = bind.it.var.it;
        T.sort = T.Type;
        T.bound = T.Pre })
      binds'
    in
    let pre_k = T.Abs (pre_tbs, T.Pre) in
    let c = match id.note with
      | None -> let c = Cons.fresh id.it pre_k in id.note <- Some c; c
      | Some c -> c
    in
    let val_env = match dec.it with
      | ClassD _ ->
        if T.Env.mem id.it scope.val_env then
          error_duplicate env "" id;
        add_id scope.val_env id T.Pre
      | _ -> scope.val_env
    in
    { val_env;
      typ_env = T.Env.add id.it c scope.typ_env;
      con_env = T.ConSet.disjoint_add c scope.con_env;
      lib_env = scope.lib_env;
      obj_env = scope.obj_env;
      fld_src_env = scope.fld_src_env;
    }

and gather_pat env (scope : Scope.t) pat : Scope.t =
   gather_pat_aux env Scope.Declaration scope pat

and gather_pat_aux env val_kind scope pat : Scope.t =
  match pat.it with
  | WildP | LitP _ | SignP _ -> scope
  | VarP id -> Scope.adjoin_val_env scope (gather_id env scope.Scope.val_env id val_kind)
  | TupP pats -> List.fold_left (gather_pat env) scope pats
  | ObjP pfs -> List.fold_left (gather_pat_field env) scope pfs
  | TagP (_, pat1) | AltP (pat1, _) | OptP pat1
  | AnnotP (pat1, _) | ParP pat1 -> gather_pat env scope pat1

and gather_pat_field env scope pf : Scope.t =
  let val_kind = kind_of_field_pattern pf in
  match pf.it with
  | ValPF (id, pat) -> gather_pat_aux env val_kind scope pat
  | TypPF id -> gather_typ_id env scope id

and gather_id env ve id val_kind : Scope.val_env =
  if T.Env.mem id.it ve then
    error_duplicate env "" id;
  T.Env.add id.it (T.Pre, id.at, val_kind) ve

and gather_typ_id env scope id : Scope.t =
  let open Scope in
  if T.Env.mem id.it scope.typ_env then
    error_duplicate env "type " id;
  (* NOTE: If we decide to require specifying the arity and bounds of
  type constructors on type pattern fields we need to record them here *)
  let pre_k = T.Def ([], T.Pre) in
  let c = Cons.fresh id.it pre_k in
  { scope with
    typ_env = T.Env.add id.it c scope.typ_env;
    con_env = T.ConSet.disjoint_add c scope.con_env;
  }

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
      ( {it = ObjBlockE (_exp_opt, obj_sort, _t, dec_fields); at; _}
      | {it = AwaitE (_, { it = AsyncE (_, _, _, { it = ObjBlockE (_exp_opt, ({ it = Type.Actor; _} as obj_sort), _t, dec_fields); at; _ }) ; _ }); _ }),
        _
    ) ->
    let decs = List.map (fun {it = {vis; dec; _}; _} -> dec) dec_fields in
    let scope = T.Env.find id.it env.objs in
    let env' = adjoin env scope in
    let obj_scope_typs = infer_block_typdecs env' decs in
    let obj_scope = Scope.adjoin scope obj_scope_typs in
    Scope.{ empty with
      con_env = obj_scope.con_env;
      val_env = singleton id (object_of_scope env obj_sort.it dec_fields obj_scope at);
      obj_env = T.Env.singleton id.it obj_scope
    }
  (* TODO: generalize beyond let <id> = <valpath> *)
  | LetD ({it = VarP id; _}, exp, _) ->
    (match infer_val_path env exp with
     | None -> Scope.empty
     | Some t ->
       let open Scope in
       match T.promote t with
       | T.Obj (_, _) as t' -> { Scope.empty with val_env = singleton id t' }
       | _ -> { Scope.empty with val_env = singleton id T.Pre }
    )
  | LetD (pat, exp, _) ->
       begin match infer_val_path env exp with
       | Some t ->
          let te = check_pat_typ_dec {env with pre = true} t pat in
          Scope.{empty with typ_env = te}
       | None -> Scope.empty
       end
  | ExpD _ | VarD _ ->
    Scope.empty
  | TypD (id, typ_binds, typ) ->
    let k = check_typ_def env dec.at (id, typ_binds, typ) in
    let c = T.Env.find id.it env.typs in
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = infer_id_typdecs env dec.at id c k;
    }
  | ClassD (exp_opt, shared_pat, obj_sort, id, binds, pat, _typ_opt, self_id, dec_fields) ->
     (*TODO exp_opt *)
    let c = T.Env.find id.it env.typs in
    let ve0 = check_class_shared_pat {env with pre = true} shared_pat obj_sort in
    let cs, tbs, te, ce = check_typ_binds {env with pre = true} binds in
    let env' = adjoin_typs (adjoin_vals {env with pre = true} ve0) te ce in
    let _, ve = infer_pat true env' pat in
    let in_actor = obj_sort.it = T.Actor in
    let async_cap, class_tbs, class_cs = infer_class_cap env obj_sort.it tbs cs in
    let self_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) class_cs) in
    let env'' =
     { (add_val (adjoin_vals env' ve) self_id self_typ) with
          labs = T.Env.empty;
          rets = None;
          async = async_cap;
          in_actor}
    in
    let t = infer_obj { env'' with check_unused = false } obj_sort exp_opt dec_fields dec.at in
    let k = T.Def (T.close_binds class_cs class_tbs, T.close class_cs t) in
    check_closed env id k dec.at;
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = infer_id_typdecs env dec.at id c k;
    }

and infer_id_typdecs env at id c k : Scope.con_env =
  assert (match k with T.Abs (_, T.Pre) -> false | _ -> true);
  (match Cons.kind c with
  | T.Abs (_, T.Pre) -> T.set_kind c k; id.note <- Some c
  | k' -> assert (eq_kind env at k' k) (* may diverge on expansive types *)
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

and is_import d =
  match d.it with
  | LetD (_, {it = ImportE _; _}, None) -> true
  | _ -> false

and infer_dec_valdecs env dec : Scope.t =
  match dec.it with
  | ExpD _ ->
    Scope.empty
  (* TODO: generalize beyond let <id> = <obje> *)
  | LetD (
      {it = VarP id; _} as pat,
      ( {it = ObjBlockE (_exp_opt, obj_sort, _t, dec_fields); at; _}
      | {it = AwaitE (_, { it = AsyncE (_, _, _, { it = ObjBlockE (_exp_opt, ({ it = Type.Actor; _} as obj_sort), _t, dec_fields); at; _ }) ; _ }); _ }),
        _
    ) ->
    let decs = List.map (fun df -> df.it.dec) dec_fields in
    let obj_scope = T.Env.find id.it env.objs in
    let obj_scope' =
      infer_block_valdecs
        (adjoin {env with pre = true} obj_scope)
        decs obj_scope
    in
    let obj_typ = object_of_scope env obj_sort.it dec_fields obj_scope' at in
    let _ve = check_pat env obj_typ pat in
    Scope.{empty with val_env = singleton id obj_typ}
  | LetD (pat, exp, fail) ->
    let t = infer_exp {env with pre = true; check_unused = false} exp in
    let ve' = match fail with
      | None -> check_pat_exhaustive (if is_import dec then local_error else warn) env t pat
      | Some _ -> check_pat env t pat
    in
    Scope.{empty with val_env = ve'}
  | VarD (id, exp) ->
    let t = infer_exp {env with pre = true} exp in
    Scope.{empty with val_env = singleton id (T.Mut t)}
  | TypD (id, _, _) ->
    let c = Option.get id.note in
    Scope.{ empty with
      typ_env = T.Env.singleton id.it c;
      con_env = T.ConSet.singleton c;
    }
  | ClassD (_exp_opt, _shared_pat, obj_sort, id, typ_binds, pat, _, _, _) ->
    if obj_sort.it = T.Actor then begin
      error_in Flags.[WASIMode; WasmMode] env dec.at "M0138" "actor classes are not supported";
      if not env.in_prog then
        error_in Flags.[ICMode; RefMode] env dec.at "M0139"
          "inner actor classes are not supported yet; any actor class must come last in your program";
      if not (List.length typ_binds = 1) then
        local_error env dec.at "M0140"
          "actor classes with type parameters are not supported yet";
    end;
    let cs, tbs, te, ce = check_typ_binds env typ_binds in
    let env' = adjoin_typs env te ce in
    let c = T.Env.find id.it env.typs in
    let t1, _ = infer_pat true {env' with pre = true} pat in
    let ts1 = match pat.it with TupP _ -> T.seq_of_tup t1 | _ -> [t1] in
    let class_tbs, _,  class_cs = infer_class_cap env obj_sort.it tbs cs in
    let obj_typ = T.Con (c, List.map (fun c -> T.Con (c, [])) class_cs) in
    let t2 =
      if obj_sort.it = T.Actor then
        T.Async (T.Fut, T.Con (List.hd cs, []), obj_typ)
      else obj_typ
    in
    let t = T.Func (T.Local, T.Returns, T.close_binds cs tbs,
      List.map (T.close cs) ts1,
      [T.close cs t2])
    in
    Scope.{ empty with
      val_env = singleton id t;
      typ_env = T.Env.singleton id.it c;
      con_env = T.ConSet.singleton c;
    }

(* Programs *)
let infer_prog ?(viper_mode=false) scope pkg_opt async_cap prog
    : (T.typ * Scope.t) Diag.result
  =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun prog ->
          let env0 = env_of_scope ~viper_mode msgs scope in
          let env = {
             env0 with async = async_cap;
          } in
          let t, sscope = infer_block env prog.it prog.at true in
          if pkg_opt = None && Diag.is_error_free msgs then emit_unused_warnings env;
          let fld_src_env = Field_sources.of_mutable_tbl env.srcs in
          t, {sscope with Scope.fld_src_env}
        ) prog
    )

let is_actor_dec d =
  match d.it with
  | ExpD e
  | LetD (_, e, _) -> CompUnit.is_actor_def e
  | ClassD (exp_opt, shared_pat, obj_sort, id, typ_binds, pat, typ_opt, self_id, dec_fields) ->
    obj_sort.it = T.Actor
  | _ -> false

let check_actors ?(viper_mode=false) ?(check_actors=false) scope progs : unit Diag.result =
  if not check_actors then Diag.return () else
  Diag.with_message_store
    (fun msgs ->
      recover_opt (fun progs ->
        let prog = (CompUnit.combine_progs progs).it in
        let env = env_of_scope ~viper_mode msgs scope in
        let report ds =
          match ds with
            [] -> ()
          | d :: _ ->
            let r = { d.at with right = (Lib.List.last ds).at.right } in
            local_error env r "M0141" "move these declarations into the body of the main actor or actor class"
        in
        let rec go ds = function
          | [] -> ()
          | (d::ds') when is_actor_dec d ->
            if ds <> [] || ds' <> [] then begin
              report (List.rev ds);
              report ds';
              error_in [Flags.ICMode; Flags.RefMode] env d.at "M0141"
                "an actor or actor class must be the only non-imported declaration in a program"
            end
          | (d::ds') when is_import d -> go ds ds'
          | (d::ds') -> go (d::ds) ds'
        in
        go [] prog
        ) progs
    )

let check_lib scope pkg_opt lib : Scope.t Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun lib ->
          let env = { (env_of_scope msgs scope) with errors_only = (pkg_opt <> None) } in
          let { imports; body = cub; _ } = lib.it in
          let (imp_ds, ds) = CompUnit.decs_of_lib lib in
          let typ, _ = infer_block env (imp_ds @ ds) lib.at false in
          List.iter2 (fun import imp_d -> import.note <- imp_d.note.note_typ) imports imp_ds;
          cub.note <- {empty_typ_note with note_typ = typ};
          let imp_typ = match cub.it with
            | ModuleU _ ->
              if cub.at = no_region then begin
                let r = Source.({
                  left = { no_pos with file = lib.note.filename };
                  right = { no_pos with file = lib.note.filename }})
                in
                warn env r "M0142" "deprecated syntax: an imported library should be a module or named actor class"
              end;
              typ
            | ActorClassU (_persistence, sp, exp_opt, id, tbs, p, _, self_id, dec_fields) ->
              if is_anon_id id then
                error env cub.at "M0143" "bad import: imported actor class cannot be anonymous";
              let cs = List.map (fun tb -> Option.get tb.note) tbs in
              let ts = List.map (fun c -> T.Con(c, [])) cs in
              let fun_typ = typ in
              let ts1, class_typ =
                match T.normalize fun_typ with
                | T.Func (sort, control, _, ts1, [t2]) ->
                  let t2 = T.normalize (T.open_ ts t2) in
                  (match t2 with
                   | T.Async (_, _, class_typ) -> List.map (T.open_ ts) ts1, class_typ
                   | _ -> assert false)
                | _ -> assert false
              in
              let con = Cons.fresh id.it (T.Def([], class_typ)) in
              T.(obj Module [
                (id.it, Typ con);
                (id.it, fun_typ);
                ("system", obj Module [id.it, install_typ (List.map (close cs) ts1) class_typ])
              ])
            | ActorU _ ->
              error env cub.at "M0144" "bad import: expected a module or actor class but found an actor"
            | ProgU _ ->
              (* this shouldn't really happen, as an imported program should be rewritten to a module *)
              error env cub.at "M0000" "compiler bug: expected a module or actor class but found a program, i.e. a sequence of declarations"
          in
          if pkg_opt = None && Diag.is_error_free msgs then emit_unused_warnings env;
          let fld_src_env = Field_sources.of_mutable_tbl env.srcs in
          {(Scope.lib lib.note.filename imp_typ) with Scope.fld_src_env}
        ) lib
    )

let check_stab_sig scope sig_ : T.stab_sig  Diag.result =
  Diag.with_message_store
    (fun msgs ->
      recover_opt
        (fun (decs, sfs) ->
          let env = env_of_scope msgs scope in
          let scope = infer_block_decs env decs sig_.at in
          let env1 = adjoin env scope in
          let check_fields sfs =
            check_ids env "object type" "field"
              (List.filter_map (fun (field : typ_field) ->
                   match field.it with ValF (id, _, _) -> Some id | _ -> None)
                 sfs);
            check_ids env "object type" "type field"
              (List.filter_map (fun (field : typ_field) ->
                   match field.it with TypF (id, _, _) -> Some id | _ -> None)
                 sfs);
            let _ = List.map (check_typ_field {env1 with pre = true} T.Object) sfs in
            let fs = List.map (check_typ_field {env1 with pre = false} T.Object) sfs in
            List.iter (fun (field : Syntax.typ_field) ->
                match field.it with
                | TypF _ -> ()
                | ValF (id, typ, _) ->
                  if not (T.stable typ.note) then
                     error env id.at "M0131" "variable %s is declared stable but has non-stable type%a"
                     id.it
                     display_typ typ.note)
              sfs;
            List.sort T.compare_field fs
          in
          match sfs.it with
          | Single sfs -> T.Single (List.sort T.compare_field (check_fields sfs))
          | PrePost (pre, post) ->
            let reqs = List.map (fun f -> (fst f).it) pre in
            let pres = List.map snd pre in
            T.PrePost (List.sort
                         (fun (r1, tf1) (r2, tf2) -> T.compare_field tf1 tf2)
                         (List.combine reqs (check_fields pres)),
                       List.sort T.compare_field (check_fields post))
        ) sig_.it
    )
