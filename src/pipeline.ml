open Printf

type stat_env = Typing.env
type dyn_env = Interpret.env
type env = stat_env * dyn_env


(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let error at category msg =
  eprintf "%s: %s error, %s\n%!" (Source.string_of_region at) category msg


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

type parse_result = Syntax.prog

let parse_with lexer parser name : parse_result option =
  try
    (*phase "Parsing" name;*)
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    Some (parser Lexer.token lexer)
  with
    | Lexer.Error (at, msg) -> error at "syntax" msg; None
    | Parser.Error ->
      error (Lexer.region lexer) "syntax" "unexpected token"; None

let parse_string s name =
  let lexer = Lexing.from_string s in
  let parser = Parser.parse_prog in
  parse_with lexer parser name

let parse_file filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parser = Parser.parse_prog in
  let result = parse_with lexer parser filename in
  close_in ic;
  result


(* Checking *)

type check_result = Syntax.prog * Type.typ * Typing.scope

let check_prog infer senv prog name : (Type.typ * Typing.scope) option =
  try
    phase "Checking" name;
    let t, ((ve, te, ce) as scope) = infer senv prog in
    if !Flags.trace then begin
      print_ce ce;
      print_stat_ve ve
    end;
    Some (t, scope)
  with Typing.Error (at, msg) ->
    error at "type" msg; None

let check_with parse infer senv name : check_result option =
  match parse name with
  | None -> None
  | Some prog ->
    match check_prog infer senv prog name with
    | None -> None
    | Some (t, scope) -> Some (prog, t, scope)

let check_string s = check_with (parse_string s) Typing.infer_prog
let check_file =
  check_with parse_file (fun env p -> Type.unit, Typing.check_prog env p)


(* Interpretation *)

type interpret_result =
  Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope

let interpret_prog denv prog name : (Value.value * Interpret.scope) option =
  try
    phase "Interpreting" name;
    let vo, scope = Interpret.interpret_prog denv prog in
    match vo with
    | None -> None
    | Some v -> Some (v, scope)
  with exn ->
    (* For debugging, should never happen. *)
    error (Interpret.get_last_region ()) "fatal" (Printexc.to_string exn);
    eprintf "\nLast environment:\n%!";
    eprint_dyn_ve_untyped Interpret.((get_last_env ()).vals);
    eprintf "\n";
    Printexc.print_backtrace stderr;
    eprintf "%!";
    None

let interpret_with check (senv, denv) name : interpret_result option =
  match check senv name with
  | None -> None
  | Some (prog, t, sscope) ->
    match interpret_prog denv prog name with
    | None -> None
    | Some (v, dscope) -> Some (prog, t, v, sscope, dscope)

let interpret_string s = interpret_with (check_string s)
let interpret_file = interpret_with check_file


(* Running *)

type run_result = env

let output_dscope (senv, _) t v sscope dscope =
  if !Flags.trace then print_dyn_ve senv dscope

let output_scope (senv, _) t v sscope dscope =
  print_scope senv sscope dscope;
  if v <> Value.unit then print_val senv v t


let run_with interpret output ((senv, denv) as env) name : run_result option =
  let result = interpret env name in
  let env' =
    match result with
    | None ->
      phase "Aborted" name;
      None
    | Some (_prog, t, v, sscope, dscope) ->
      phase "Finished" name;
      let senv' = Typing.adjoin senv sscope in
      let denv' = Interpret.adjoin denv dscope in
      let env' = (senv', denv') in
      output env' t v sscope dscope;
      Some env'
  in
  if !Flags.verbose then printf "\n";
  env'

let run_string s = run_with (interpret_string s) output_dscope
let run_file = run_with interpret_file output_dscope


(* Compilation *)

type compile_result = Wasm.Ast.module_ * Typing.scope

let compile_prog prog name : Wasm.Ast.module_ =
  phase "Compiling" name;
  Compile.compile prog

let compile_with check senv name : compile_result option =
  match check senv name with
  | None -> None
  | Some (prog, t, scope) ->
    let module_ = compile_prog prog name in
    Some (module_, scope)

let compile_string s = compile_with (check_string s)
let compile_file = compile_with check_file


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
  match parse_with lexer Parser.parse_prog_interactive name with
  | None ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    None
  | some -> some

let check_lexer lexer = check_with (parse_lexer lexer) Typing.infer_prog
let interpret_lexer lexer = interpret_with (check_lexer lexer)
let run_lexer lexer = run_with (interpret_lexer lexer) output_scope

let run_stdin env =
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop env = loop (Lib.Option.get (run_lexer lexer env "stdin") env) in
  try loop env with End_of_file ->
    printf "\n%!"


(* Prelude *)

let init () =
  let empty_env = (Typing.empty_env, Interpret.empty_env) in
  Flags.privileged := true;
  match run_string Prelude.prelude empty_env "prelude" with
  | None ->
    error Source.no_region "fatal" "initializing prelude failed";
    exit 1
  | Some env ->
    Flags.privileged := false;
    env
