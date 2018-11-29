open Printf

module Await = Awaitopt   (* for more naive cps translation, use Await *)
module Async = Async 
type stat_env = Typing.env
type dyn_env = Interpret.env
type env = stat_env * dyn_env


(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

type message = Source.region * string * Severity.t * string
type messages = message list

let error at category msg =
  Error (at, category, Severity.Error, msg)

let print_message (at, category, severity, msg) =
  match severity with
  | Severity.Error -> eprintf "%s: %s error, %s\n%!" (Source.string_of_region at) category msg
  | Severity.Warning -> eprintf "%s: warning, %s\n%!" (Source.string_of_region at) msg

let print_messages = List.iter print_message

let print_ce =
  Con.Env.iter (fun c k ->
    let eq, params, typ = Type.strings_of_kind k in
    printf "type %s%s %s %s\n" (Con.to_string c) params eq typ
  )

let print_stat_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.as_immut t in
    printf "%s %s : %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
  )

let print_dyn_ve senv =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x senv.Typing.vals in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let eprint_dyn_ve_untyped =
  Value.Env.iter (fun x d ->
    eprintf "%s = %s\n%!" x (Value.string_of_def d)
  )

let print_scope senv (sve, te, ce) dve =
  print_ce ce;
  print_dyn_ve senv dve

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)


(* Parsing *)

type parse_result = (Syntax.prog, message) result

let dump_prog flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange.prog prog)
    else ()

let parse_with mode lexer parser name : parse_result =
  try
    (*phase "Parsing" name;*)
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

let parse_string s name =
  let lexer = Lexing.from_string s in
  let parser = Parser.parse_prog in
  parse_with Lexer.Normal lexer parser name

let parse_file filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parser = Parser.parse_prog in
  let result = parse_with Lexer.Normal lexer parser filename in
  close_in ic;
  result

let parse_files filenames =
  let open Source in
  let rec loop progs = function
    | [] -> Ok (List.concat (List.rev progs) @@ no_region)
    | filename::filenames' ->
      match parse_file filename with
      | Error e -> Error e
      | Ok prog -> loop (prog.it::progs) filenames'
  in loop [] filenames


(* Checking *)

type check_result = (Syntax.prog * Type.typ * Typing.scope * messages, messages) result

let messages_of_typing_messages = List.map (fun (at, sev, msg) -> (at, "type", sev, msg))

let check_prog infer senv name prog
  : (Type.typ * Typing.scope * messages, messages) result =
  phase "Checking" name;
  match infer senv prog with
  | Ok (t, ((ve, te, ce) as scope), msgs) ->
    if !Flags.trace && !Flags.verbose then begin
      print_ce ce;
      print_stat_ve ve
    end;
    Ok (t, scope, messages_of_typing_messages msgs)
  | Error msgs -> Error (messages_of_typing_messages msgs)

let await_lowering flag prog name =
  if flag then
    begin
      phase "Await Lowering" name;
      let prog' = Await.t_prog prog in
      dump_prog Flags.dump_lowering prog';
      prog'
    end
  else prog

let async_lowering flag prog name =
  if flag then
    begin
      phase "Async Lowering" name;
      let prog' = Async.t_prog prog in
      dump_prog Flags.dump_lowering prog';
      prog'
    end
  else prog

let check_with parse infer senv name : check_result =
  match parse name with
  | Error e -> Error [e]
  | Ok prog ->
    match check_prog infer senv name prog with
    | Error msgs -> Error msgs
    | Ok (t, scope, msgs) -> Ok (prog, t, scope, msgs)

let infer_prog_unit senv prog =
  match Typing.check_prog senv prog with
  | Error msgs -> Error msgs
  | Ok (scope, msgs) -> Ok (Type.unit, scope, msgs)

let check_string senv s = check_with (parse_string s) Typing.infer_prog senv
let check_file senv n = check_with parse_file infer_prog_unit senv n
let check_files senv = function
  | [n] -> check_file senv n
  | ns -> check_with (fun _n -> parse_files ns) infer_prog_unit senv "all"


(* Interpretation *)

type interpret_result =
  (Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope) option

let interpret_prog denv name prog : (Value.value * Interpret.scope) option =
  try
    phase "Interpreting" name;
    let vo, scope = Interpret.interpret_prog denv prog in
    match vo with
    | None -> None
    | Some v -> Some (v, scope)
  with exn ->
    (* For debugging, should never happen. *)
    print_message (Interpret.get_last_region (), "fatal", Severity.Error, Printexc.to_string exn);
    eprintf "\nLast environment:\n%!";
    eprint_dyn_ve_untyped Interpret.((get_last_env ()).vals);
    eprintf "\n";
    Printexc.print_backtrace stderr;
    eprintf "%!";
    None

let interpret_with check (senv, denv) name : interpret_result =
  match check senv name with
  | Error msgs ->
    print_messages msgs;
    None
  | Ok (prog, t, sscope, msgs) ->
    print_messages msgs;
    let prog = await_lowering (!Flags.await_lowering) prog name in
    let prog = async_lowering (!Flags.await_lowering && !Flags.async_lowering) prog name in
    match interpret_prog denv name prog with
    | None -> None
    | Some (v, dscope) -> Some (prog, t, v, sscope, dscope)

let interpret_string env s =
  interpret_with (fun senv name -> check_string senv s name) env
let interpret_file env n = interpret_with check_file env n
let interpret_files env = function
  | [n] -> interpret_file env n
  | ns -> interpret_with (fun senv _name -> check_files senv ns) env "all"


(* Prelude *)

let prelude_name = "prelude"

let prelude_error phase (at, _, is_err, msg) =
  print_message (at, "fatal", is_err, phase ^ " prelude failed: " ^ msg);
  exit 1

let check_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parser = Parser.parse_prog in
  match parse_with Lexer.Privileged lexer parser prelude_name with
  | Error e -> prelude_error "parsing" e
  | Ok prog ->
    match check_prog infer_prog_unit Typing.empty_env prelude_name prog with
    | Error msgs -> prelude_error "checking" (List.hd msgs)
    | Ok (_t, sscope, msgs) ->
      let senv = Typing.adjoin Typing.empty_env sscope in
      prog, senv

let prelude, initial_stat_env = check_prelude ()

let run_prelude () : dyn_env =
  match interpret_prog Interpret.empty_env prelude_name prelude with
  | None -> prelude_error "initializing" (Source.no_region, "", Severity.Error, "crashed")
  | Some (_v, dscope) ->
    Interpret.adjoin Interpret.empty_env dscope

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
  let result = interpret env name in
  let env' =
    match result with
    | None ->
      phase "Aborted" name;
      None
    | Some (prog, t, v, sscope, dscope) ->
      phase "Finished" name;
      let senv' = Typing.adjoin senv sscope in
      let denv' = Interpret.adjoin denv dscope in
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

let run_string env s =
  run_with (fun env name -> interpret_string env s name) output_dscope env
let run_file env n = run_with interpret_file output_dscope env n
let run_files env = function
  | [n] -> run_file env n
  | ns ->
    run_with (fun env _name -> interpret_files env ns) output_dscope env "all"


(* Compilation *)

type compile_mode = Compile.mode = WasmMode | DfinityMode
type compile_result = (CustomModule.extended_module, messages) result

let compile_with check mode name : compile_result =
  match check initial_stat_env name with
  | Error msgs -> Error msgs
  | Ok (prog, _t, _scope, msgs) ->
    print_messages msgs;
    let prog = await_lowering true prog name in
    let prog = async_lowering true prog name in
    phase "Compiling" name;
    let module_ = Compile.compile mode prelude [prog] in
    Ok module_

let compile_string mode s =
  compile_with (fun senv name -> check_string senv s name) mode
let compile_file mode n = compile_with check_file mode n
let compile_files mode = function
  | [n] -> compile_file mode n
  | ns -> compile_with (fun senv _name -> check_files senv ns) mode "all"


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

let parse_lexer lexer name =
  let open Lexing in
  if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
  match parse_with Lexer.Normal lexer Parser.parse_prog_interactive name with
  | Error e ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    Error e
  | some -> some

let check_lexer senv lexer = check_with (parse_lexer lexer) Typing.infer_prog senv
let interpret_lexer env lexer =
  interpret_with (fun senv name -> check_lexer senv lexer name) env
let run_lexer env lexer =
  run_with (fun env name -> interpret_lexer env lexer name) output_scope env

let run_stdin env =
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop env = loop (Lib.Option.get (run_lexer env lexer "stdin") env) in
  try loop env with End_of_file ->
    printf "\n%!"
