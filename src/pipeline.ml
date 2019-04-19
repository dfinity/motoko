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
    if Syntax.is_import_id x then () else
    let t = Type.Env.find x scope.Typing.val_env in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let print_scope senv scope dve =
  print_ce scope.Typing.con_env;
  print_dyn_ve senv dve

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)

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

let parse_with mode lexer parser name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parser (Lexer.token mode) lexer in
    dump_prog Flags.dump_parse prog;
    Ok prog
  with
    | Lexer.Error (at, msg) ->
      error at "syntax" msg
    | Parser.Error ->
      error (Lexer.region lexer) "syntax" "unexpected token"

let parse_string s name : parse_result =
  let lexer = Lexing.from_string s in
  let parser = Parser.parse_prog in
  match parse_with Lexer.Normal lexer parser name with
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)
  | Error e -> Error [e]

let parse_file filename : parse_result =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parser = Parser.parse_prog in
  let result = parse_with Lexer.Normal lexer parser filename in
  close_in ic;
  match result with
  | Ok prog -> Diag.return (prog, filename)
  | Error e -> Error [e]

(* Import file name resolution *)

type resolve_result = (Syntax.prog * Resolve_import.S.t) Diag.result

let resolve (prog, base) : resolve_result =
  Diag.map_result (fun imports -> (prog, imports)) (Resolve_import.resolve prog base)


(* Imported file loading *)

type imports = (rel_path * Syntax.prog) list
type load_imports_result = (imports * Syntax.prog list) Diag.result

let rec chase_imports todo seen imported : imports Diag.result =
  let open Resolve_import.S in
  match min_elt_opt todo with
  | None -> Diag.return imported
  | Some f ->
    let todo = remove f todo in
    let seen = add f seen in
    Diag.bind (parse_file f) (fun (prog, base) ->
      Diag.bind (Static.prog prog) (fun () ->
        Diag.bind (Resolve_import.resolve prog base) (fun more_imports ->
          let todo = union todo (diff more_imports seen) in
          chase_imports todo seen ((f, prog) :: imported)
        )
      )
    )

let load_imports progs : load_imports_result =
  Diag.bind (Diag.traverse resolve progs) (fun rs ->
    let progs' = List.map fst rs in
    let imports =
      List.fold_left Resolve_import.S.union Resolve_import.S.empty
      (List.map snd rs) in
    Diag.map_result
      (fun imported -> (imported, progs'))
      (chase_imports imports Resolve_import.S.empty [])
  )


(* Checking *)

type check_result = (Syntax.prog * Type.typ * Typing.scope) Diag.result

let check_prog infer senv name prog
  : (Type.typ * Typing.scope) Diag.result =
  phase "Checking" name;
  let r = infer senv prog in
  if !Flags.trace && !Flags.verbose then begin
    match r with
    | Ok ((_, scope), _) ->
      print_ce scope.Typing.con_env;
      print_stat_ve scope.Typing.val_env;
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  r


let declare_import (f, (prog:Syntax.prog)) =
  let open Source in
  match prog.it with
  |  [{it = Syntax.ExpD _;_}] ->
     { it = Syntax.LetD
              ( { it = Syntax.VarP (Syntax.id_of_full_path f)
                ; at = no_region
                ; note = Type.Pre
                }
              , { it = Syntax.BlockE prog.it
                ; at = no_region
                ; note = Syntax.empty_typ_note
                }
              )
     ; at = no_region
     ; note = Syntax.empty_typ_note
     }
  |  ds ->
     { it = Syntax.ModuleD
              (  Syntax.id_of_full_path f
              , ds
              )
     ; at = no_region
     ; note = Syntax.empty_typ_note
     }


let combine_files imports progs : Syntax.prog =
    let open Source in
    ( List.map declare_import imports
      @ List.concat (List.map (fun p -> p.it) progs)
    ) @@ no_region

let check_with parse infer senv name : check_result =
  Diag.bind parse (fun parse_result ->
    Diag.bind (load_imports parse_result) (fun (imports, progs) ->
      let prog = combine_files imports progs in
      Diag.map_result (fun (t, scope) -> (prog, t, scope))
        (check_prog infer senv name prog)
    )
  )
let check_with_one parse_one infer senv name : check_result =
  let parse = Diag.map_result (fun r -> [r]) parse_one in
  check_with parse infer senv name

let infer_prog_unit senv prog =
  Diag.map_result (fun scope -> (Type.unit, scope))
    (Typing.check_prog senv prog)

let check_string senv s n =
  check_with_one (parse_string s n) Typing.infer_prog senv n
let check_files senv ns =
  check_with (Diag.traverse parse_file ns) Typing.infer_prog senv "all"

(* IR transforms *)

let transform transform_name trans env prog name =
  phase transform_name name;
  let prog_ir' : Ir.prog = trans env prog in
  dump_ir Flags.dump_lowering prog_ir';
  Check_ir.check_prog env transform_name prog_ir';
  prog_ir'

let transform_if transform_name trans flag env prog name =
  if flag then transform transform_name trans env prog name
  else prog

let desugar =
  transform "Desugaring" Desugar.transform

let await_lowering =
  transform_if "Await Lowering" (fun _ -> Await.transform)

let async_lowering =
  transform_if "Async Lowering" Async.transform

let serialization =
  transform_if "Synthesizing serialization code" Serialization.transform

let tailcall_optimization =
  transform_if "Tailcall optimization" (fun _ -> Tailcall.transform)

let show_translation =
  transform_if "Translate show" Show.transform

(* Interpretation *)

type interpret_result =
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option

let interpret_prog (senv,denv) name prog : (Value.value * Interpret.scope) option =
  try
    phase "Interpreting" name;
    let vo, scope =
      if !Flags.interpret_ir
      then
        let prog_ir = desugar senv prog name in
        let prog_ir = await_lowering (!Flags.await_lowering) senv prog_ir name in
        let prog_ir = async_lowering (!Flags.await_lowering && !Flags.async_lowering) senv prog_ir name in
        let prog_ir = serialization (!Flags.await_lowering && !Flags.async_lowering) senv prog_ir name in
        let prog_ir = tailcall_optimization true senv prog_ir name in
        let prog_ir = show_translation true senv prog_ir name in
        Interpret_ir.interpret_prog denv prog_ir
      else Interpret.interpret_prog denv prog in
    match vo with
    | None -> None
    | Some v -> Some (v, scope)
  with exn ->
    (* For debugging, should never happen. *)
    Interpret.print_exn exn;
    None

let interpret_with check (senv, denv) name : interpret_result =
  match check senv with
  | Error msgs ->
    Diag.print_messages msgs;
    None
  | Ok ((prog, t, sscope), msgs) ->
    Diag.print_messages msgs;
(*  let prog = await_lowering (!Flags.await_lowering) prog name in
    let prog = async_lowering (!Flags.await_lowering && !Flags.async_lowering) prog name in *)
    match interpret_prog (senv,denv) name prog with
    | None -> None
    | Some (v, dscope) -> Some (prog, t, v, sscope, dscope)

let interpret_files env ns =
  interpret_with (fun senv -> check_files senv ns) env "all"


(* Prelude *)

let prelude_name = "prelude"

let prelude_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prelude failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parser = Parser.parse_prog in
  match parse_with Lexer.Privileged lexer parser prelude_name with
  | Error e -> prelude_error "parsing" [e]
  | Ok prog ->
    match check_prog infer_prog_unit Typing.empty_scope prelude_name prog with
    | Error es -> prelude_error "checking" es
    | Ok ((_t, sscope), msgs) ->
      let senv = Typing.adjoin_scope Typing.empty_scope sscope in
      prog, senv

let prelude, initial_stat_env = check_prelude ()

let run_prelude () : dyn_env =
  match interpret_prog (Typing.empty_scope,Interpret.empty_scope) prelude_name prelude with
  | None -> prelude_error "initializing" []
  | Some (_v, dscope) ->
    Interpret.adjoin_scope Interpret.empty_scope dscope

let initial_dyn_env = run_prelude ()
let initial_env = (initial_stat_env, initial_dyn_env)


(* Running *)

type run_result = env option

let output_dscope (senv, _) t v sscope dscope =
  if !Flags.trace then print_dyn_ve senv dscope

let output_scope (senv, _) t v sscope dscope =
  print_scope senv sscope dscope;
  if v <> Value.unit then print_val senv v t

let is_exp dec = match dec.Source.it with Syntax.ExpD _ -> true | _ -> false

let run_with interpret output ((senv, denv) as env) name : run_result =
  let result = interpret env in
  let env' =
    match result with
    | None ->
      phase "Aborted" name;
      None
    | Some (prog, t, v, sscope, dscope) ->
      phase "Finished" name;
      let senv' = Typing.adjoin_scope senv sscope in
      let denv' = Interpret.adjoin_scope denv dscope in
      let env' = (senv', denv') in
      (* TBR: hack *)
      let t', v' =
        if prog.Source.it <> [] && is_exp (Lib.List.last prog.Source.it)
        then t, v
        else Type.unit, Value.unit
      in
      output env' t' v' sscope dscope;
      Some env'
  in
  if !Flags.verbose then printf "\n";
  env'

let run_files env ns =
    run_with (fun env -> interpret_files env ns) output_dscope env "all"


(* Compilation *)

type compile_mode = Compile.mode = WasmMode | DfinityMode
type compile_result = (CustomModule.extended_module, Diag.messages) result

let compile_with check mode name : compile_result =
  match check initial_stat_env name with
  | Error msgs -> Error msgs
  | Ok ((prog, _t, scope), msgs) ->
    Diag.print_messages msgs;
    let prelude_ir = Desugar.transform Typing.empty_scope prelude in
    let prog_ir = desugar initial_stat_env prog name in
    let prog_ir = await_lowering true initial_stat_env prog_ir name in
    let prog_ir = async_lowering true initial_stat_env prog_ir name in
    let prog_ir = serialization true initial_stat_env prog_ir name in
    let prog_ir = tailcall_optimization true initial_stat_env prog_ir name in
    let prog_ir = show_translation true initial_stat_env prog_ir name in
    phase "Compiling" name;
    let module_ = Compile.compile mode name prelude_ir [prog_ir] in
    Ok module_

let compile_string mode s name =
  compile_with (fun senv name -> check_string senv s name) mode name
let compile_files mode files name =
  compile_with (fun senv _name -> check_files senv files) mode name


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

let parse_lexer lexer name : parse_result =
  let open Lexing in
  if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
  match parse_with Lexer.Normal lexer Parser.parse_prog_interactive name with
  | Error e ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    Error [e]
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)

let check_lexer senv lexer name =
  check_with_one (parse_lexer lexer name) Typing.infer_prog senv name
let interpret_lexer env lexer name =
  interpret_with (fun senv -> check_lexer senv lexer name) env name
let run_lexer env lexer name  =
  run_with (fun env -> interpret_lexer env lexer name) output_scope env name

let run_stdin env =
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop env = loop (Lib.Option.get (run_lexer env lexer "stdin") env) in
  try loop env with End_of_file ->
    printf "\n%!"
