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
    let open Type in
    let t = Env.find x scope.Scope.val_env in
    let t' = as_immut t in
    match normalize t' with
    | Obj (Module, fs) ->
      printf "%s %s : module {...}\n"
        (if t == t' then "let" else "var") x
    | _ ->
      printf "%s %s : %s = %s\n"
        (if t == t' then "let" else "var") x
        (Type.string_of_typ t')
        (Value.string_of_def !Flags.print_depth d)
  )

let print_scope senv scope dve =
  print_ce scope.Scope.con_env;
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

type parse_fn = rel_path -> parse_result

let parse_with mode lexer parser name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let tokenizer, _ = Lexer.tokenizer mode lexer in
    let prog = Parsing.parse (!Flags.error_detail) (parser lexer.Lexing.lex_curr_p) tokenizer lexer name in
    dump_prog Flags.dump_parse prog;
    Ok prog
  with
    | Lexer.Error (at, msg) ->
      error at "syntax" msg
    | Parsing.Error (msg, start, end_) ->
      error Source.{
        left = Lexer.convert_pos start;
        right = Lexer.convert_pos end_;
      } "syntax" msg

let parse_string name s : parse_result =
  let lexer = Lexing.from_string s in
  let parse = Parser.Incremental.parse_prog in
  match parse_with Lexer.Normal lexer parse name with
  | Ok prog -> Diag.return (prog, name)
  | Error e -> Error [e]

let parse_file filename : parse_result =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parse = Parser.Incremental.parse_prog in
  let result = parse_with Lexer.Normal lexer parse filename in
  close_in ic;
  match result with
  | Ok prog -> Diag.return (prog, filename)
  | Error e -> Error [e]

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
  let (prog, _) =  Diag.run (parse_file file) in
  let imports = Diag.run (ResolveImport.collect_imports prog file) in
  List.iter (fun (url, path) ->
      match path with
      | None -> Printf.printf "%s\n" url
      | Some path -> Printf.printf "%s %s\n" url path
    ) imports

(* Checking *)

let infer_prog senv prog
  : (Type.typ * Scope.scope) Diag.result =
  phase "Checking" prog.Source.note;
  let r = Typing.infer_prog senv prog in
  if !Flags.trace && !Flags.verbose then begin
    match r with
    | Ok ((_, scope), _) ->
      print_ce scope.Scope.con_env;
      print_stat_ve scope.Scope.val_env;
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  phase "Definedness" prog.Source.note;
  Diag.bind r (fun t_sscope ->
    Diag.bind (Definedness.check_prog prog) (fun () -> Diag.return t_sscope)
  )

let rec check_progs senv progs : Scope.scope Diag.result =
  match progs with
  | [] -> Diag.return senv
  | p::ps ->
    Diag.bind (infer_prog senv p) (fun (_t, sscope) ->
      let senv' = Scope.adjoin senv sscope in
      check_progs senv' ps
    )

let check_lib senv lib : Scope.scope Diag.result =
  phase "Checking" (Filename.basename lib.Source.note);
  Diag.bind (Typing.check_lib senv lib) (fun sscope ->
    phase "Definedness" (Filename.basename lib.Source.note);
    Diag.bind (Definedness.check_lib lib) (fun () -> Diag.return sscope)
  )

(* Parsing libraries *)

let is_import dec =
  let open Source in let open Syntax in
  match dec.it with
  | ExpD e | LetD (_, e) -> (match e.it with ImportE _ -> true | _ -> false)
  | _ -> false

let is_module dec =
  let open Source in let open Syntax in
  match dec.it with
  | ExpD e | LetD (_, e) ->
    (match e.it with ObjE (s, _) -> s.it = Type.Module | _ -> false)
  | _ -> false

let rec lib_of_prog' imps at = function
  | [d] when is_module d -> imps, d
  | d::ds when is_import d -> lib_of_prog' (d::imps) at ds
  | ds ->
    let open Source in let open Syntax in
    let fs = List.map (fun d -> {vis = Public @@ at; dec = d; stab = None} @@ d.at) ds in
    let obj = {it = ObjE (Type.Module @@ at, fs); at; note = empty_typ_note} in
    imps, {it = ExpD obj; at; note = empty_typ_note}

let lib_of_prog f prog =
  let open Source in let open Syntax in
  let imps, dec = lib_of_prog' [] prog.at prog.it in
  let exp = {it = BlockE (List.rev imps @ [dec]); at = prog.at; note = empty_typ_note} in
  {it = exp; at = prog.at; note = f}


(* Prelude *)

let prelude_name = "prelude"

let prelude_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prelude failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parse = Parser.Incremental.parse_prog in
  match parse_with Lexer.Privileged lexer parse prelude_name with
  | Error e -> prelude_error "parsing" [e]
  | Ok prog ->
    let senv0 = Typing.initial_scope in
    match infer_prog senv0 prog with
    | Error es -> prelude_error "checking" es
    | Ok ((_t, sscope), msgs) ->
      let senv1 = Scope.adjoin senv0 sscope in
      prog, senv1

let prelude, initial_stat_env = check_prelude ()

(* The prim module *)

let prim_name = "prim"

let prim_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prim failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prim () : Syntax.lib * stat_env =
  let lexer = Lexing.from_string Prelude.prim_module in
  let parse = Parser.Incremental.parse_prog in
  match parse_with Lexer.Privileged lexer parse prim_name with
  | Error e -> prim_error "parsing" [e]
  | Ok prog ->
    let senv0 = initial_stat_env in
    let lib = lib_of_prog "@prim" prog in
    match check_lib senv0 lib with
    | Error es -> prim_error "checking" es
    | Ok (sscope, msgs) ->
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
        Error [{
          Diag.sev = Diag.Error; at = ri.Source.at; cat = "import";
          text = Printf.sprintf "file %s must not depend on itself" f
        }]
      else begin
        pending := add ri.Source.it !pending;
        Diag.bind (parsefn f) (fun (prog, base) ->
        Diag.bind (Static.prog prog) (fun () ->
        Diag.bind (ResolveImport.resolve (resolve_flags ()) prog base) (fun more_imports ->
        Diag.bind (go_set more_imports) (fun () ->
        let lib = lib_of_prog f prog in
        Diag.bind (check_lib !senv lib) (fun sscope ->
        libs := lib :: !libs; (* NB: Conceptually an append *)
        senv := Scope.adjoin !senv sscope;
        pending := remove ri.Source.it !pending;
        Diag.return ()
        )))))
      end
    | Syntax.IDLPath (f, _) ->
       Diag.bind (Idllib.Pipeline.check_file f) (fun (prog, idl_scope, actor_opt) ->
       if actor_opt = None then
         Error [{
           Diag.sev = Diag.Error; at = ri.Source.at; cat = "import";
           text = Printf.sprintf "file %s does not define a service" f
         }]
       else
         let actor = Mo_idl.Idl_to_mo.check_prog idl_scope actor_opt in
         let sscope = Scope.lib f actor in
         senv := Scope.adjoin !senv sscope;
         Diag.return ()
       )

  and go_set todo = Diag.traverse_ go todo
  in
  Diag.map (fun () -> (List.rev !libs, !senv)) (go_set imports)

let load_progs parsefn files senv : load_result =
  Diag.bind (Diag.traverse parsefn files) (fun parsed ->
  Diag.bind (resolve_progs parsed) (fun rs ->
  let progs' = List.map fst rs in
  let libs = Lib.List.concat_map snd rs in
  Diag.bind (chase_imports parsefn senv libs) (fun (libs, senv') ->
  Diag.bind (check_progs senv' progs') (fun senv'' ->
  Diag.return (libs, progs', senv'')
  ))))

let load_decl parse_one senv : load_decl_result =
  Diag.bind parse_one (fun parsed ->
  Diag.bind (resolve_prog parsed) (fun (prog, libs) ->
  Diag.bind (chase_imports parse_file senv libs) (fun (libs, senv') ->
  Diag.bind (infer_prog senv' prog) (fun (t, sscope) ->
  let senv'' = Scope.adjoin senv' sscope in
  Diag.return (libs, prog, senv'', t, sscope)
  ))))


(* Interpretation (Source) *)

let interpret_prog denv prog : (Value.value * Interpret.scope) option =
  let open Interpret in
  phase "Interpreting" prog.Source.note;
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  let result = Interpret.interpret_prog flags denv prog in
  Profiler.process_prog_result result ;
  result

let rec interpret_libs denv libs : Interpret.scope =
  let open Interpret in
  match libs with
  | [] -> denv
  | lib::libs' ->
    phase "Interpreting" (Filename.basename lib.Source.note);
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


let run_prelude () : dyn_env =
  match interpret_prog Interpret.empty_scope prelude with
  | None -> prelude_error "initializing" []
  | Some (_v, dscope) ->
    Interpret.adjoin_scope Interpret.empty_scope dscope

let initial_dyn_env = run_prelude ()

let initial_env = (initial_stat_env, initial_dyn_env)


(* Only checking *)

type check_result = unit Diag.result

let check_files' parsefn files : check_result =
  Diag.map ignore (load_progs parsefn files initial_stat_env)

let check_files files : check_result =
  check_files' parse_file files

let check_string s name : check_result =
  Diag.map ignore (load_decl (parse_string name s) initial_stat_env)

(* Generate IDL *)

let generate_idl files : Idllib.Syntax.prog Diag.result =
  Diag.bind (load_progs parse_file files initial_stat_env)
    (fun (libs, progs, senv) ->
      Diag.return (Mo_idl.Mo_to_idl.prog (progs, senv)))

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
  match parse_with Lexer.Normal lexer Parser.Incremental.parse_prog_interactive "stdin" with
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
  | Ok ((libs, prog, senv', t, sscope), msgs) ->
    Diag.print_messages msgs;
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
        if prog.Source.it <> [] && is_exp (Lib.List.last prog.Source.it)
        then t, v
        else Type.unit, Value.unit
      in
      output_scope env' t' v' sscope dscope;
      if !Flags.verbose then printf "\n";
      Some env'

let run_files_and_stdin files =
  let lexer = Lexing.from_function lexer_stdin in
  Option.bind (interpret_files initial_env files) (fun env ->
    let rec loop env = loop (Lib.Option.get (run_stdin lexer env) env) in
    try loop env with End_of_file ->
      printf "\n%!";
      Some ()
  )


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

let desugar imports prog name =
  phase "Desugaring" name;
  let open Lowering.Desugar in
  let prog_ir' : Ir.prog = link_declarations
    (transform_prelude prelude @ imports)
    (transform_prog prog) in
  dump_ir Flags.dump_lowering prog_ir';
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose "Desugaring" prog_ir';
  prog_ir'

let await_lowering =
  transform_if "Await Lowering" Await.transform

let async_lowering mode =
  transform_if "Async Lowering" (Async.transform mode)

let tailcall_optimization =
  transform_if "Tailcall optimization" Tailcall.transform

let show_translation =
  transform_if "Translate show" Show.transform

let analyze analysis_name analysis prog name =
  phase analysis_name name;
  analysis prog;
  if !Flags.check_ir
  then Check_ir.check_prog !Flags.verbose analysis_name prog


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
  or relative to the directory of the invoked moc (when developing)
  *)
  let wasm_filename =
    match Sys.getenv_opt "MOC_RTS" with
    | Some filename -> filename
    | None -> Filename.(concat (dirname Sys.argv.(0)) "../rts/mo-rts.wasm") in
  let wasm = load_file wasm_filename in
  Wasm_exts.CustomModuleDecode.decode "rts.wasm" wasm

type compile_result = Wasm_exts.CustomModule.extended_module Diag.result

(* a hack to support compiling multiple files *)
let combine_progs progs : Syntax.prog =
  let open Source in
  if progs = []
  then { it = []; at = no_region; note = "empty" }
  else { it = Lib.List.concat_map (fun p -> p.it) progs
       ; at = (Lib.List.last progs).at
       ; note = (Lib.List.last progs).note
       }

let lower_prog mode libs progs name =
  let prog_ir = desugar libs progs name in
  let prog_ir = await_lowering !Flags.await_lowering prog_ir name in
  let prog_ir = async_lowering mode !Flags.async_lowering prog_ir name in
  let prog_ir = tailcall_optimization true prog_ir name in
  let prog_ir = show_translation true prog_ir name in
  analyze "constness analysis" Const.analyze prog_ir name;
  prog_ir

let lower_libs libs : Lowering.Desugar.import_declaration =
  Lib.List.concat_map (fun l -> Lowering.Desugar.transform_lib l) libs

let compile_prog mode do_link libs progs : Wasm_exts.CustomModule.extended_module =
  let prog = combine_progs progs in
  let name = prog.Source.note in
  let imports = lower_libs libs in
  let prog_ir = lower_prog mode imports prog name in
  phase "Compiling" name;
  let rts = if do_link then Some (load_as_rts ()) else None in
  Codegen.Compile.compile mode name rts prog_ir

let compile_files mode do_link files : compile_result =
  Diag.bind (load_progs parse_file files initial_stat_env)
    (fun (libs, progs, senv) ->
    Diag.bind (Typing.check_actors senv progs) (fun () ->
    Diag.return (compile_prog mode do_link libs progs)))

let compile_string mode s name : compile_result =
  Diag.bind (load_decl (parse_string name s) initial_stat_env)
    (fun (libs, prog, senv, _t, _sscope) ->
    Diag.return (compile_prog mode false libs [prog]))

(* Interpretation (IR) *)

let interpret_ir_prog libs progs =
  let prog = combine_progs progs in
  let name = prog.Source.note in
  let libs' = lower_libs libs in
  let prog_ir = lower_prog (!Flags.compile_mode) libs' prog name in
  phase "Interpreting" name;
  let open Interpret_ir in
  let flags = { trace = !Flags.trace; print_depth = !Flags.print_depth } in
  interpret_prog flags prog_ir

let interpret_ir_files files =
  Option.map
    (fun (libs, progs, senv) -> interpret_ir_prog libs progs)
    (Diag.flush_messages (load_progs parse_file files initial_stat_env))
