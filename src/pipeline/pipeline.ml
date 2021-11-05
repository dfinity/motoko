open Mo_def
open Mo_frontend
open Mo_types
open Mo_values
open Mo_interpreter
open Ir_def
open Ir_interpreter
open Ir_passes
open Mo_config

open Printf

module ResolveImport = Resolve_import

type stat_env = Scope.t
type dyn_env = Interpret.scope
type env = stat_env * dyn_env


(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let print_ce =
  Type.ConSet.iter (fun c ->
    let eq, params, typ = Type.pps_of_kind (Con.kind c) in
    Format.printf "@[<hv 2>type %s%a %s@ %a@]@."
      (Con.to_string c)
      params ()
      eq
      typ ()
  )

let print_stat_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.as_immut t in
    Format.printf "@[<hv 2>%s %s :@ %a@]@."
      (if t == t' then "let" else "var") x
      Type.pp_typ t'
  )

let print_dyn_ve scope =
  Value.Env.iter (fun x d ->
    let open Type in
    let t = Env.find x scope.Scope.val_env in
    let t' = as_immut t in
    match normalize t' with
    | Obj (Module, fs) ->
      Format.printf "@[<hv 2>%s %s : module {...}@]@."
        (if t == t' then "let" else "var") x
    | _ ->
      Format.printf "@[<hv 2>%s %s :@ %a =@ %a@]@."
        (if t == t' then "let" else "var") x
        Type.pp_typ t'
        (Value.pp_def !Flags.print_depth) d
  )

let print_scope senv scope dve =
  print_ce scope.Scope.con_env;
  print_dyn_ve senv dve

let print_val _senv v t =
  Format.printf "@[<hv 2>%a :@ %a@]@."
    (Value.pp_val !Flags.print_depth) v
    Type.pp_typ t


(* Dumping *)

let dump_prog flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange.prog prog)

let dump_ir flag prog_ir =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange_ir.prog prog_ir)


(* Parsing *)

type rel_path = string

type parse_result = (Syntax.prog * rel_path) Diag.result

type no_region_parse_fn = string -> parse_result
type parse_fn = Source.region -> no_region_parse_fn

let generic_parse_with mode lexer parser name : _ Diag.result =
  phase "Parsing" name;
  let open Diag.Syntax in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  (* a back door to enable the `prim` syntax, for our test suite *)
  let tokenizer, triv_table = Lexer.tokenizer mode lexer in
  let* mk_syntax =
    try
      Parser_lib.triv_table := triv_table;
      Parsing.parse (!Flags.error_detail) (parser lexer.Lexing.lex_curr_p) tokenizer lexer
    with Lexer.Error (at, msg) -> Diag.error at"M0002" "syntax" msg
  in
  let phrase = mk_syntax name in
  Diag.return phrase

let parse_with mode lexer parser name : Syntax.prog Diag.result =
  let open Diag.Syntax in
  let* prog = generic_parse_with mode lexer parser name in
  dump_prog Flags.dump_parse prog;
  Diag.return prog

let parse_string' mode name s : parse_result =
  let open Diag.Syntax in
  let lexer = Lexing.from_string s in
  let parse = Parser.Incremental.parse_prog in
  let* prog = parse_with mode lexer parse name in
  Diag.return (prog, name)

let parse_string = parse_string' Lexer.mode

let parse_file' mode at filename : (Syntax.prog * rel_path) Diag.result =
  let ic, messages = Lib.FilePath.open_in filename in
  Diag.finally (fun () -> close_in ic) (
    let open Diag.Syntax in
    let* _ =
      Diag.traverse_
        (Diag.warn at "M0005" "import")
        messages in
    let lexer = Lexing.from_channel ic in
    let parse = Parser.Incremental.parse_prog in
    let* prog = parse_with mode lexer parse filename in
    Diag.return (prog, filename)
  )

let parse_file = parse_file' Lexer.mode

(* Import file name resolution *)

type resolve_result = (Syntax.prog * ResolveImport.resolved_imports) Diag.result

let resolve_flags () =
  ResolveImport.{
    package_urls = !Flags.package_urls;
    actor_aliases = !Flags.actor_aliases;
    actor_idl_path = !Flags.actor_idl_path
  }

let resolve_prog (prog, base) : resolve_result =
  Diag.map
    (fun libs -> (prog, libs))
    (ResolveImport.resolve (resolve_flags ()) prog base)

let resolve_progs =
  Diag.traverse resolve_prog


(* Printing dependency information *)

let print_deps (file : string) : unit =
  let (prog, _) =  Diag.run (parse_file Source.no_region file) in
  let imports = Diag.run (ResolveImport.collect_imports prog file) in
  List.iter (fun (url, path) ->
      match path with
      | None -> Printf.printf "%s\n" url
      | Some path -> Printf.printf "%s %s\n" url path
    ) imports

(* Checking *)

let infer_prog senv prog : (Type.typ * Scope.scope) Diag.result =
  let filename = prog.Source.note.Syntax.filename in
  phase "Checking" filename;
  let r = Typing.infer_prog senv prog in
  if !Flags.trace && !Flags.verbose then begin
    match r with
    | Ok ((_, scope), _) ->
      print_ce scope.Scope.con_env;
      print_stat_ve scope.Scope.val_env;
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  phase "Definedness" filename;
  let open Diag.Syntax in
  let* t_sscope = r in
  let* () = Definedness.check_prog prog in
  Diag.return t_sscope

let rec check_progs senv progs : Scope.scope Diag.result =
  match progs with
  | [] -> Diag.return senv
  | p::ps ->
    let open Diag.Syntax in
    let* _t, sscope = infer_prog senv p in
    let senv' = Scope.adjoin senv sscope in
    check_progs senv' ps

let check_lib senv lib : Scope.scope Diag.result =
  let filename = lib.Source.note.Syntax.filename in
  phase "Checking" (Filename.basename filename);
  let open Diag.Syntax in
  let* sscope = Typing.check_lib senv lib in
  phase "Definedness" (Filename.basename filename);
  let* () = Definedness.check_lib lib in
  Diag.return sscope

let lib_of_prog f prog : Syntax.lib  =
  let lib = CompUnit.comp_unit_of_prog true prog in
  { lib with Source.note = { lib.Source.note with Syntax.filename = f } }


(* Prelude and internals *)

let builtin_error phase what (msgs : Diag.messages) =
  Printf.eprintf "%s %s failed\n" phase what;
  Diag.print_messages msgs;
  exit 1

let check_builtin what src senv0 : Syntax.prog * stat_env =
  let lexer = Lexing.from_string src in
  let parse = Parser.Incremental.parse_prog in
  match parse_with Lexer.mode_priv lexer parse what with
  | Error es -> builtin_error "parsing" what es
  | Ok (prog, _ws) ->
    match infer_prog senv0 prog with
    | Error es -> builtin_error "checking" what es
    | Ok ((_t, sscope), _ws) ->
      let senv1 = Scope.adjoin senv0 sscope in
      prog, senv1

let prelude, initial_stat_env0 =
  check_builtin "prelude" Prelude.prelude Typing.initial_scope
let internals, initial_stat_env =
  check_builtin "internals" Prelude.internals initial_stat_env0

(* Compatibility *)

let _parse_sig s name  =
  let open Diag.Syntax in
  let mode = {Lexer.privileged = false} in
  let lexer = Lexing.from_string s in
  let parse = Parser.Incremental.parse_sig in
  let* sig_ = generic_parse_with mode lexer parse name in
  Diag.return sig_

let parse_sig_from_file filename : Syntax.sig_ Diag.result =
  let ic = Stdlib.open_in filename in
  Diag.finally (fun () -> close_in ic) (
    let open Diag.Syntax in
    let mode = {Lexer.privileged = false} in
    let lexer = Lexing.from_channel ic in
    let parse = Parser.Incremental.parse_sig in
    let* sig_ = generic_parse_with mode lexer parse filename in
    Diag.return sig_
  )

let compatible pre post : unit Diag.result =
  let open Diag.Syntax in
  let* p1 = parse_sig_from_file pre in
  let* p2 = parse_sig_from_file post in
  let* s1 = Typing.check_sig initial_stat_env0 p1 in
  let* s2 = Typing.check_sig initial_stat_env0 p2 in
  Stability.match_sig s1 s2

(* The prim module *)

let prim_name = "prim"

let prim_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prim failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prim () : Syntax.lib * stat_env =
  let lexer = Lexing.from_string Prelude.prim_module in
  let parse = Parser.Incremental.parse_prog in

  match parse_with Lexer.mode_priv lexer parse prim_name with
  | Error es -> prim_error "parsing" es
  | Ok (prog, _ws) ->
    let open Syntax in
    let open Source in
    let senv0 = initial_stat_env in
    let fs = List.map (fun d -> {vis = Public None @@ no_region; dec = d; stab = None} @@ d.at) prog.it in
    let body = {it = ModuleU (None, fs); at = no_region; note = empty_typ_note} in
    let lib = {
      it = { imports = []; body };
      at = no_region;
      note = { filename = "@prim"; trivia = Trivia.empty_triv_table }
    } in
    match check_lib senv0 lib with
    | Error es -> prim_error "checking" es
    | Ok (sscope, _ws) ->
      let senv1 = Scope.adjoin senv0 sscope in
      lib, senv1


(* Imported file loading *)

(*
Loading a file (or string) implies lexing, parsing, resolving imports to
libraries, and typechecking.
The resulting prog is typechecked.
The Typing.scope field in load_result is the accumulated scope.
When we load a declaration (i.e from the REPL), we also care about the type
and the newly added scopes, so these are returned separately.
*)


type load_result =
  (Syntax.lib list * Syntax.prog list * Scope.scope) Diag.result

type load_decl_result =
  (Syntax.lib list * Syntax.prog * Scope.scope * Type.typ * Scope.scope) Diag.result

let chase_imports parsefn senv0 imports : (Syntax.lib list * Scope.scope) Diag.result =
  (*
  This function loads and type-checkes the files given in `imports`,
  including any further dependencies.

  The resulting `Syntax.libraries` list is in dependency order. To achieve this,
  the function go below does an depth-first traversal of the import DAG.
  * To detected illegal cycles, pending is a set of filenames that we started
    processing, but did not add yet.
  * To avoid duplicates, i.e. load each file at most once, we check the
    senv.
  * We accumulate the resulting libraries in reverse order, for O(1) appending.
  *)

  let open ResolveImport.S in
  let pending = ref empty in
  let senv = ref senv0 in
  let libs = ref [] in

  let rec go ri = match ri.Source.it with
    | Syntax.PrimPath ->
      (* a bit of a hack, lib_env should key on resolved_import *)
      if Type.Env.mem "@prim" !senv.Scope.lib_env then
        Diag.return ()
      else
        let lib, sscope = check_prim () in
        libs := lib :: !libs; (* NB: Conceptually an append *)
        senv := Scope.adjoin !senv sscope;
        Diag.return ()
    | Syntax.Unresolved -> assert false
    | Syntax.LibPath f ->
      if Type.Env.mem f !senv.Scope.lib_env then
        Diag.return ()
      else if mem ri.Source.it !pending then
        Diag.error
          ri.Source.at
          "M0003"
          "import"
          (Printf.sprintf "file %s must not depend on itself" f)
      else begin
        pending := add ri.Source.it !pending;
        let open Diag.Syntax in
        let* prog, base = parsefn ri.Source.at f in
        let* () = Static.prog prog in
        let* more_imports = ResolveImport.resolve (resolve_flags ()) prog base in
        let* () = go_set more_imports in
        let lib = lib_of_prog f prog in
        let* sscope = check_lib !senv lib in
        libs := lib :: !libs; (* NB: Conceptually an append *)
        senv := Scope.adjoin !senv sscope;
        pending := remove ri.Source.it !pending;
        Diag.return ()
      end
    | Syntax.IDLPath (f, _) ->
      let open Diag.Syntax in
      let* prog, idl_scope, actor_opt = Idllib.Pipeline.check_file f in
      if actor_opt = None then
        Diag.error
          ri.Source.at
          "M0004"
          "import"
          (Printf.sprintf "file %s does not define a service" f)
      else
        match Mo_idl.Idl_to_mo.check_prog idl_scope actor_opt with
        | exception Idllib.Exception.UnsupportedCandidFeature error_message ->
          Stdlib.Error [
            Diag.error_message
              ri.Source.at
              "M0153"
              "import"
              (Printf.sprintf "file %s uses Candid types without corresponding Motoko type" f);
            error_message ]
        | actor ->
          let sscope = Scope.lib f actor in
          senv := Scope.adjoin !senv sscope;
          Diag.return ()
  and go_set todo = Diag.traverse_ go todo
  in
  Diag.map (fun () -> (List.rev !libs, !senv)) (go_set imports)

let load_progs parsefn files senv : load_result =
  let open Diag.Syntax in
  let* parsed = Diag.traverse (parsefn Source.no_region) files in
  let* rs = resolve_progs parsed in
  let progs' = List.map fst rs in
  let libs = List.concat_map snd rs in
  let* libs, senv' = chase_imports parsefn senv libs in
  let* senv'' = check_progs senv' progs' in
  Diag.return (libs, progs', senv'')

let load_decl parse_one senv : load_decl_result =
  let open Diag.Syntax in
  let* parsed = parse_one in
  let* prog, libs = resolve_prog parsed in
  let* libs, senv' = chase_imports parse_file senv libs in
  let* t, sscope = infer_prog senv' prog in
  let senv'' = Scope.adjoin senv' sscope in
  Diag.return (libs, prog, senv'', t, sscope)


(* Interpretation (Source) *)

let interpret_prog denv prog : (Value.value * Interpret.scope) option =
  let open Interpret in
  phase "Interpreting" prog.Source.note.Syntax.filename;
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  let result = Interpret.interpret_prog flags denv prog in
  Profiler.process_prog_result result ;
  result

let rec interpret_libs denv libs : Interpret.scope =
  let open Interpret in
  match libs with
  | [] -> denv
  | lib::libs' ->
     phase "Interpreting" (Filename.basename lib.Source.note.Syntax.filename);
    let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
    let dscope = interpret_lib flags denv lib in
    let denv' = adjoin_scope denv dscope in
    interpret_libs denv' libs'

let rec interpret_progs denv progs : Interpret.scope option =
  match progs with
  | [] -> Some denv
  | p::ps ->
    match interpret_prog denv p with
    | Some (_v, dscope) ->
      let denv' = Interpret.adjoin_scope denv dscope in
      interpret_progs denv' ps
    | None -> None

let interpret_files (senv0, denv0) files : (Scope.scope * Interpret.scope) option =
  Option.bind
    (Diag.flush_messages (load_progs parse_file files senv0))
    (fun (libs, progs, senv1) ->
      let denv1 = interpret_libs denv0 libs in
      match interpret_progs denv1 progs with
      | None -> None
      | Some denv2 -> Some (senv1, denv2)
    )

let run_builtin prog denv : dyn_env =
  match interpret_prog denv prog with
  | None -> builtin_error "initializing" prog.Source.note.Syntax.filename []
  | Some (_v, dscope) ->
    Interpret.adjoin_scope denv dscope

let initial_dyn_env = run_builtin internals (run_builtin prelude Interpret.empty_scope)

let initial_env = (initial_stat_env, initial_dyn_env)


(* Only checking *)

type check_result = unit Diag.result

let check_files' parsefn files : check_result =
  Diag.map ignore (load_progs parsefn files initial_stat_env)

let check_files files : check_result =
  check_files' parse_file files

(* Generate IDL *)

let generate_idl files : Idllib.Syntax.prog Diag.result =
  let open Diag.Syntax in
  let* libs, progs, senv = load_progs parse_file files initial_stat_env in
  let* () = Typing.check_actors senv progs in
  Diag.return (Mo_idl.Mo_to_idl.prog (progs, senv))

(* Running *)

let run_files files : unit option =
  Option.map ignore (interpret_files initial_env files)

(* Interactively *)

let continuing = ref false

let lexer_stdin buf len =
  let prompt = if !continuing then "  " else "> " in
  printf "%s" prompt; flush_all ();
  continuing := true;
  let rec loop i =
    if i = len then i else
    let ch = input_char stdin in
    Bytes.set buf i ch;
    if ch = '\n' then i + 1 else loop (i + 1)
  in loop 0

let parse_lexer lexer : parse_result =
  let open Lexing in
  if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
  match parse_with Lexer.mode lexer Parser.Incremental.parse_prog_interactive "stdin" with
  | Error es ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    Error es
  | Ok (prog, ws) -> Ok ((prog, Filename.current_dir_name), ws)

let is_exp dec = match dec.Source.it with Syntax.ExpD _ -> true | _ -> false

let output_scope (senv, _) t v sscope dscope =
  print_scope senv sscope dscope.Interpret.val_env;
  if v <> Value.unit then print_val senv v t

let run_stdin lexer (senv, denv) : env option =
  match Diag.flush_messages (load_decl (parse_lexer lexer) senv) with
  | None ->
    if !Flags.verbose then printf "\n";
    None
  | Some (libs, prog, senv', t, sscope) ->
    let denv' = interpret_libs denv libs in
    match interpret_prog denv' prog with
    | None ->
      if !Flags.verbose then printf "\n";
      None
    | Some (v, dscope) ->
      phase "Finished" "stdin";
      let denv' = Interpret.adjoin_scope denv dscope in
      let env' = (senv', denv') in
      (* TBR: hack *)
      let t', v' =
        if Option.fold ~none:false ~some:is_exp (Lib.List.last_opt prog.Source.it)
        then t, v
        else Type.unit, Value.unit
      in
      output_scope env' t' v' sscope dscope;
      if !Flags.verbose then printf "\n";
      Some env'

let run_stdin_from_file files file =
  let open Lib.Option.Syntax in
  let* (senv, denv) = interpret_files initial_env files in
  let* (libs, prog, senv', t, sscope) =
    Diag.flush_messages (load_decl (parse_file Source.no_region file) senv) in
  let denv' = interpret_libs denv libs in
  let* (v, dscope) = interpret_prog denv' prog in
  Format.printf "@[<hv 2>%a :@ %a@]@."
    (Value.pp_val 10) v
    Type.pp_typ t;
  Some ()

let run_files_and_stdin files =
  let open Lib.Option.Syntax in
  let lexer = Lexing.from_function lexer_stdin in
  let* env = interpret_files initial_env files in
  let rec loop env = loop (Lib.Option.get (run_stdin lexer env) env) in
  try loop env with End_of_file ->
    printf "\n%!";
    Some ()

(* Desugaring *)

let desugar_unit imports u name : Ir.prog =
  phase "Desugaring" name;
  let open Lowering.Desugar in
  let prog_ir' : Ir.prog = link_declarations
    (import_prelude prelude @ import_prelude internals @ imports)
    (transform_unit u) in
  dump_ir Flags.dump_lowering prog_ir';
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose "Desugaring" prog_ir';
  prog_ir'

(* IR transforms *)

let transform transform_name trans prog name =
  phase transform_name name;
  let prog_ir' : Ir.prog = trans prog in
  dump_ir Flags.dump_lowering prog_ir';
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose transform_name prog_ir';
  prog_ir'

let transform_if transform_name trans flag prog name =
  if flag then transform transform_name trans prog name
  else prog

let await_lowering =
  transform_if "Await Lowering" Await.transform

let async_lowering mode =
  transform_if "Async Lowering" (Async.transform mode)

let tailcall_optimization =
  transform_if "Tailcall optimization" Tailcall.transform

let typ_field_translation =
  transform_if "Erase type components" Erase_typ_field.transform

let show_translation =
  transform_if "Translate show" Show.transform

let eq_translation =
  transform_if "Translate polymorphic equality" Eq.transform

let analyze analysis_name analysis prog name =
  phase analysis_name name;
  analysis prog;
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose analysis_name prog

let ir_passes mode prog_ir name =
  (* erase typ components from objects *)
  let prog_ir = typ_field_translation true prog_ir name in
  (* translations that extend the progam and must be done before await/cps conversion *)
  let prog_ir = show_translation true prog_ir name in
  let prog_ir = eq_translation true prog_ir name in
  (* cps conversion and local transformations *)
  let prog_ir = await_lowering !Flags.await_lowering prog_ir name in
  let prog_ir = async_lowering mode !Flags.async_lowering prog_ir name in
  let prog_ir = tailcall_optimization true prog_ir name in
  analyze "constness analysis" Const.analyze prog_ir name;
  prog_ir


(* Compilation *)

let load_as_rts () =
  let rts = if !Flags.sanity then Rts.wasm_debug else Rts.wasm in
  Wasm_exts.CustomModuleDecode.decode "rts.wasm" (Lazy.force rts)

type compile_result = Wasm_exts.CustomModule.extended_module Diag.result

(* This transforms the flat list of libs (some of which are classes)
   into a list of imported libs and (compiled) classes *)
let rec compile_libs mode libs : Lowering.Desugar.import_declaration =
  let open Source in
  let rec go imports = function
    | [] -> imports
    | l :: libs ->
      let { Syntax.body = cub; _ } = l.it in
      match cub.it with
      | Syntax.ActorClassU _ ->
        let wasm = compile_unit_to_wasm mode imports l in
        go (imports @ Lowering.Desugar.import_compiled_class l wasm) libs
      | _ ->
        go (imports @ Lowering.Desugar.import_unit l) libs
  in go [] libs

and compile_unit mode do_link imports u : Wasm_exts.CustomModule.extended_module =
  let name = u.Source.note.Syntax.filename in
  let prog_ir = desugar_unit imports u name in
  let prog_ir = ir_passes mode prog_ir name in
  phase "Compiling" name;
  let rts = if do_link then Some (load_as_rts ()) else None in
  Codegen.Compile.compile mode rts prog_ir

and compile_unit_to_wasm mode imports (u : Syntax.comp_unit) : string =
  let wasm_mod = compile_unit mode true imports u in
  let (_source_map, wasm) = Wasm_exts.CustomModuleEncode.encode wasm_mod in
  wasm

and compile_progs mode do_link libs progs : Wasm_exts.CustomModule.extended_module =
  let imports = compile_libs mode libs in
  let prog = CompUnit.combine_progs progs in
  let u = CompUnit.comp_unit_of_prog false prog in
  compile_unit mode do_link imports u

let compile_files mode do_link files : compile_result =
  let open Diag.Syntax in
  let* libs, progs, senv = load_progs parse_file files initial_stat_env in
  let* () = Typing.check_actors senv progs in
  Diag.return (compile_progs mode do_link libs progs)


(* Interpretation (IR) *)

(*
   This transforms the flat list of libs into a list of imported units,
   Unlike, `compile_libs`, classes are imported as IR for interpretation,
   not compiled to wasm
*)
let import_libs libs : Lowering.Desugar.import_declaration =
  List.concat_map Lowering.Desugar.import_unit libs

let interpret_ir_progs libs progs =
  let prog = CompUnit.combine_progs progs in
  let name = prog.Source.note.Syntax.filename in
  let imports = import_libs libs in
  let u = CompUnit.comp_unit_of_prog false prog in
  let prog_ir = desugar_unit imports u name in
  let prog_ir = ir_passes (!Flags.compile_mode) prog_ir name in
  phase "Interpreting" name;
  let open Interpret_ir in
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  interpret_prog flags prog_ir

let interpret_ir_files files =
  Option.map
    (fun (libs, progs, senv) -> interpret_ir_progs libs progs)
    (Diag.flush_messages (load_progs parse_file files initial_stat_env))
