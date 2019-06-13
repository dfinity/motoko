open As_frontend
open As_types
open As_values
open As_ir
open Interpreter
open Printf

type stat_env = Typing.scope
type dyn_env = Interpret.scope
type env = stat_env * dyn_env


(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let error at cat text =
  Error { Diag.sev = Diag.Error; at; cat; text }

let print_ce =
  Type.ConSet.iter (fun c ->
    let eq, params, typ = Type.strings_of_kind (Con.kind c) in
    printf "type %s%s %s %s\n" (Con.to_string c) params eq typ
  )

let print_stat_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.as_immut t in
    printf "%s %s : %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
  )

let print_dyn_ve scope =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x scope.Typing.val_env in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def !Flags.print_depth d)
  )

let print_scope senv scope dve =
  print_ce scope.Typing.con_env;
  print_dyn_ve senv dve

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val !Flags.print_depth v) (Type.string_of_typ t)

(* Dumping *)

let dump_prog flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange.prog prog)
    else ()

let dump_ir flag prog_ir =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange_ir.prog prog_ir)
    else ()

(* Parsing *)

type rel_path = string

type parse_result = (Syntax.prog * rel_path) Diag.result

let parse_with mode lexer parse name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parse (Lexer.token mode) lexer name in
    dump_prog Flags.dump_parse prog;
    Ok prog
  with
    | Lexer.Error (at, msg) ->
      error at "syntax" msg
    | Parser.Error ->
      error (Lexer.region lexer) "syntax" "unexpected token"

let parse_string s name : parse_result =
  let lexer = Lexing.from_string s in
  let parse = Parser.parse_prog in
  match parse_with Lexer.Normal lexer parse name with
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)
  | Error e -> Error [e]

let parse_file filename : parse_result =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parse = Parser.parse_prog in
  let name = Filename.basename filename in
  let result = parse_with Lexer.Normal lexer parse name in
  close_in ic;
  match result with
  | Ok prog -> Diag.return (prog, filename)
  | Error e -> Error [e]

let parse_files =
  Diag.traverse parse_file

(* Import file name resolution *)

type resolve_result = (Syntax.prog * Resolve_import.S.t) Diag.result

let resolve_prog (prog, base) : resolve_result =
  Diag.map
    (fun libraries -> (prog, libraries))
    (Resolve_import.resolve prog base)

let resolve_progs =
  Diag.traverse resolve_prog


(* Typechecking *)

let infer_prog senv prog
  : (Type.typ * Typing.scope) Diag.result =
  phase "Checking" prog.Source.note;
  let r = Typing.infer_prog senv prog in
  if !Flags.trace && !Flags.verbose then begin
    match r with
    | Ok ((_, scope), _) ->
      print_ce scope.Typing.con_env;
      print_stat_ve scope.Typing.val_env;
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  r

let rec typecheck_progs senv progs : Typing.scope Diag.result =
  match progs with
  | [] -> Diag.return senv
  | p::ps ->
    Diag.bind (infer_prog senv p) (fun (_t, sscope) ->
      let senv' = Typing.adjoin_scope senv sscope in
      typecheck_progs senv' ps
    )

let typecheck_library senv filename prog : Typing.scope Diag.result =
  phase "Checking" prog.Source.note;
  Typing.check_library senv (filename, prog)

(* Definedness checking *)

let defindeness_prog prog : unit Diag.result =
  phase "Definedness" prog.Source.note;
  Definedness.check_prog prog

let defindeness_progs progs : unit Diag.result =
  Diag.traverse_ defindeness_prog progs

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
  (Syntax.libraries * Syntax.prog list * Typing.scope) Diag.result

type load_decl_result =
  (Syntax.libraries * Syntax.prog * Typing.scope * Type.typ * Typing.scope) Diag.result

let chase_imports senv0 imports : (Syntax.libraries * Typing.scope) Diag.result =
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

  let open Resolve_import.S in
  let pending = ref empty in
  let senv = ref senv0 in
  let libraries = ref [] in

  let rec go f =
    if Type.Env.mem f !senv.Typing.lib_env then
      Diag.return ()
    else if mem f !pending then
      Error [{
        Diag.sev = Diag.Error; at = Source.no_region; cat = "import";
        text = Printf.sprintf "file %s must not depend on itself" f
      }]
    else begin
      pending := add f !pending;
      Diag.bind (parse_file f) (fun (prog, base) ->
      Diag.bind (Static.prog prog) (fun () ->
      Diag.bind (Resolve_import.resolve prog base) (fun more_imports ->
      Diag.bind (go_set more_imports) (fun () ->
      Diag.bind (typecheck_library !senv f prog) (fun sscope ->
      Diag.bind (defindeness_prog prog) (fun () ->
      libraries := (f, prog) :: !libraries; (* NB: Conceptually an append *)
      senv := Typing.adjoin_scope !senv sscope;
      pending := remove f !pending;
      Diag.return ()
      ))))))
    end
    and go_set todo = Diag.traverse_ go (elements todo)
  in
  Diag.map (fun () -> (List.rev !libraries, !senv)) (go_set imports)

let load_progs parse senv : load_result =
  Diag.bind parse (fun parsed ->
  Diag.bind (resolve_progs parsed) (fun rs ->
  let progs' = List.map fst rs in
  let libraries =
    List.fold_left Resolve_import.S.union Resolve_import.S.empty
    (List.map snd rs) in
  Diag.bind (chase_imports senv libraries) (fun (libraries, senv') ->
  Diag.bind (typecheck_progs senv' progs') (fun senv'' ->
  Diag.bind (defindeness_progs progs') (fun _ ->
  Diag.return (libraries, progs', senv'')
  )))))

let load_decl parse_one senv : load_decl_result =
  Diag.bind parse_one (fun parsed ->
  Diag.bind (resolve_prog parsed) (fun (prog, libraries) ->
  Diag.bind (chase_imports senv libraries) (fun (libraries, senv') ->
  Diag.bind (infer_prog senv' prog) (fun (t, sscope) ->
  let senv'' = Typing.adjoin_scope senv' sscope in
  Diag.return (libraries, prog, senv'', t, sscope)
  ))))


(* Interpretation (Source) *)

let interpret_prog denv prog : (Value.value * Interpret.scope) option =
  let open Interpret in
  phase "Interpreting" prog.Source.note;
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  interpret_prog flags denv prog

let rec interpret_libraries denv libraries : Interpret.scope =
  let open Interpret in
  match libraries with
  | [] -> denv
  | (f, p)::libs ->
    phase "Interpreting" p.Source.note;
    let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
    let dscope = interpret_library flags denv (f, p) in
    let denv' = adjoin_scope denv dscope in
    interpret_libraries denv' libs

let rec interpret_progs denv progs : Interpret.scope option =
  match progs with
  | [] -> Some denv
  | p::ps ->
    match interpret_prog denv p with
    | Some (_v, dscope) ->
      let denv' = Interpret.adjoin_scope denv dscope in
      interpret_progs denv' ps
    | None -> None

let interpret_files (senv0, denv0) files : (Typing.scope * Interpret.scope) option =
  Lib.Option.bind
    (Diag.flush_messages (load_progs (parse_files files) senv0))
    (fun (libraries, progs, senv1) ->
      let denv1 = interpret_libraries denv0 libraries in
      match interpret_progs denv1 progs with
      | None -> None
      | Some denv2 -> Some (senv1, denv2)
    )


(* Prelude *)

let prelude_name = "prelude"

let prelude_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prelude failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let typecheck_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parse = Parser.parse_prog in
  match parse_with Lexer.Privileged lexer parse prelude_name with
  | Error e -> prelude_error "parsing" [e]
  | Ok prog ->
    let senv0 = Typing.empty_scope  in
    match infer_prog senv0 prog with
    | Error es -> prelude_error "checking" es
    | Ok ((_t, sscope), msgs) ->
      let senv1 = Typing.adjoin_scope senv0 sscope in
      prog, senv1

let prelude, initial_stat_env = typecheck_prelude ()

let run_prelude () : dyn_env =
  match interpret_prog Interpret.empty_scope prelude with
  | None -> prelude_error "initializing" []
  | Some (_v, dscope) ->
    Interpret.adjoin_scope Interpret.empty_scope dscope

let initial_dyn_env = run_prelude ()

let initial_env = (initial_stat_env, initial_dyn_env)


(* Only checking *)

type check_result = unit Diag.result

let check_files files : check_result =
  Diag.map ignore (load_progs (parse_files files) initial_stat_env)

let check_string s name : check_result =
  Diag.map ignore (load_decl (parse_string s name) initial_stat_env)


(* Running *)

let run_files files : unit option =
  Lib.Option.map ignore (interpret_files initial_env files)


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
  match parse_with Lexer.Normal lexer Parser.parse_prog_interactive "stdin" with
  | Error e ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    Error [e]
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)

let is_exp dec = match dec.Source.it with Syntax.ExpD _ -> true | _ -> false

let output_scope (senv, _) t v sscope dscope =
  print_scope senv sscope dscope.Interpret.val_env;
  if v <> Value.unit then print_val senv v t

let run_stdin lexer (senv, denv) : env option =
  match load_decl (parse_lexer lexer) senv with
  | Error msgs ->
    Diag.print_messages msgs;
    if !Flags.verbose then printf "\n";
    None
  | Ok ((libraries, prog, senv', t, sscope), msgs) ->
    Diag.print_messages msgs;
    let denv' = interpret_libraries denv libraries in
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
        if prog.Source.it <> [] && is_exp (Lib.List.last prog.Source.it)
        then t, v
        else Type.unit, Value.unit
      in
      output_scope env' t' v' sscope dscope;
      if !Flags.verbose then printf "\n";
      Some env'

let run_files_and_stdin files =
  let lexer = Lexing.from_function lexer_stdin in
  Lib.Option.bind (interpret_files initial_env files) (fun env ->
    let rec loop env = loop (Lib.Option.get (run_stdin lexer env) env) in
    try loop env with End_of_file ->
      printf "\n%!";
      Some ()
  )


(* IR transforms *)

let transform transform_name trans env prog name =
  phase transform_name name;
  let prog_ir' : Ir.prog = trans env prog in
  dump_ir Flags.dump_lowering prog_ir';
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose env transform_name prog_ir';
  prog_ir'

let transform_if transform_name trans flag env prog name =
  if flag then transform transform_name trans env prog name
  else prog

let desugar env lib_env libraries progs name =
  phase "Desugaring" name;
  let prog_ir' : Ir.prog = Lowering.Desugar.transform_graph lib_env libraries progs in
  dump_ir Flags.dump_lowering prog_ir';
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose env "Desugaring" prog_ir';
  prog_ir'

let await_lowering =
  transform_if "Await Lowering" (fun _ -> Ir_passes.Await.transform)

let async_lowering =
  transform_if "Async Lowering" Ir_passes.Async.transform

let serialization =
  transform_if "Synthesizing serialization code" Ir_passes.Serialization.transform

let tailcall_optimization =
  transform_if "Tailcall optimization" (fun _ -> Ir_passes.Tailcall.transform)

let show_translation =
  transform_if "Translate show" Ir_passes.Show.transform


(* Compilation *)

let load_as_rts () =

  let load_file f =
    let ic = open_in f in
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s in

  (*
  The RTS can be found via environment (in particular when built via nix),
  or relative to the directory of the invoked asc (when developing)
  *)
  let wasm_filename =
    match Sys.getenv_opt "ASC_RTS" with
    | Some filename -> filename
    | None -> Filename.(concat (dirname Sys.argv.(0)) "../rts/as-rts.wasm") in
  let wasm = load_file wasm_filename in
  Wasm_exts.CustomModuleDecode.decode "rts.wasm" wasm

type compile_mode = Codegen.Compile.mode = WasmMode | DfinityMode
type compile_result = Wasm_exts.CustomModule.extended_module Diag.result

let name_progs progs =
  if progs = []
  then "empty"
  else (Lib.List.last progs).Source.note

let lower_prog senv lib_env libraries progs name =
  let prog_ir = desugar senv lib_env libraries progs name in
  let prog_ir = await_lowering !Flags.await_lowering initial_stat_env prog_ir name in
  let prog_ir = async_lowering !Flags.async_lowering initial_stat_env prog_ir name in
  let prog_ir = serialization !Flags.await_lowering initial_stat_env prog_ir name in
  let prog_ir = tailcall_optimization true initial_stat_env prog_ir name in
  let prog_ir = show_translation true initial_stat_env prog_ir name in
  prog_ir

let compile_prog mode do_link lib_env libraries progs : Wasm_exts.CustomModule.extended_module =
  let prelude_ir = Lowering.Desugar.transform prelude in
  let name = name_progs progs in
  let prog_ir = lower_prog initial_stat_env lib_env libraries progs name in
  phase "Compiling" name;
  let rts = if do_link then Some (load_as_rts ()) else None in
  Codegen.Compile.compile mode name rts prelude_ir [prog_ir]

let compile_files mode do_link files : compile_result =
  Diag.bind (load_progs (parse_files files) initial_stat_env)
    (fun (libraries, progs, senv) ->
    Diag.return (compile_prog mode do_link senv.Typing.lib_env libraries progs))

let compile_string mode s name : compile_result =
  Diag.bind (load_decl (parse_string s name) initial_stat_env)
    (fun (libraries, prog, senv, _t, _sscope) ->
    Diag.return (compile_prog mode false senv.Typing.lib_env libraries [prog]))

(* Interpretation (IR) *)

let interpret_ir_prog inp_env libraries progs =
  let prelude_ir = Lowering.Desugar.transform prelude in
  let name = name_progs progs in
  let prog_ir = lower_prog initial_stat_env inp_env libraries progs name in
  phase "Interpreting" name;
  let open Ir_interpreter.Interpret_ir in
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  let denv0 = empty_scope in
  let dscope = interpret_prog flags denv0 prelude_ir in
  let denv1 = adjoin_scope denv0 dscope in
  let _ = interpret_prog flags denv1 prog_ir in
  ()

let interpret_ir_files files =
  Lib.Option.map
    (fun (libraries, progs, senv) -> interpret_ir_prog senv.Typing.lib_env libraries progs)
    (Diag.flush_messages (load_progs (parse_files files) initial_stat_env))
