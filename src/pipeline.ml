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

let parse_files filenames =
  let open Source in
  let rec loop progs = function
    | [] -> Some (List.concat (List.rev progs) @@ no_region)
    | filename::filenames' ->
      match parse_file filename with
      | None -> None
      | Some prog -> loop (prog.it::progs) filenames'
  in loop [] filenames


(* Checking *)

type check_result = Syntax.prog * Type.typ * Typing.scope

let check_prog infer senv name prog : (Type.typ * Typing.scope) option =
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
    match check_prog infer senv name prog with
    | None -> None
    | Some (t, scope) -> Some (prog, t, scope)

let infer_prog_unit senv prog = Type.unit, Typing.check_prog senv prog

let check_string senv s = check_with (parse_string s) Typing.infer_prog senv
let check_file senv n = check_with parse_file infer_prog_unit senv n
let check_files senv = function
  | [n] -> check_file senv n
  | ns -> check_with (fun _n -> parse_files ns) infer_prog_unit senv "all"


(* Interpretation *)

type interpret_result =
  Syntax.prog * Type.typ * Value.value * Typing.scope * Interpret.scope

let interpret_prog denv name prog : (Value.value * Interpret.scope) option =
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
    match interpret_prog denv name prog with
    | None -> None
    | Some (v, dscope) -> Some (prog, t, v, sscope, dscope)

let interpret_string env s =
  interpret_with (fun senv name -> check_string senv s name) env
let interpret_file env n = interpret_with check_file env n
let interpret_files env = function
  | [n] -> interpret_file env n
  | ns -> interpret_with (fun senv _name -> check_files senv ns) env "all"


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

let run_string env s =
  run_with (fun env name -> interpret_string env s name) output_dscope env
let run_file env n = run_with interpret_file output_dscope env n
let run_files env = function
  | [n] -> run_file env n
  | ns ->
    run_with (fun env _name -> interpret_files env ns) output_dscope env "all"


(* Compilation *)

type compile_result = Wasm.Ast.module_

let prelude = ref []

let compile_prog name prog : Wasm.Ast.module_ =
  phase "Compiling" name;
  Compile.compile [prog]

let compile_with check senv name : compile_result option =
  match check senv name with
  | None -> None
  | Some (prog, _t, _scope) ->
    let open Source in
    let open Syntax in
    let (@?) it at = {it; at; note = empty_typ_note} in
    let block = ExpD (BlockE prog.it @? prog.at) @? prog.at in
    let prog' = (!prelude @ [block]) @@ prog.at in
    let module_ = compile_prog name prog' in
    Some module_

let compile_string senv s =
  compile_with (fun senv name -> check_string senv s name) senv
let compile_file senv n = compile_with check_file senv n
let compile_files senv = function
  | [n] -> compile_file senv n
  | ns -> compile_with (fun env _name -> check_files senv ns) senv "all" 


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


(* Prelude *)

let init () =
  try
    let prel_source =
      if !Flags.dfinity_mode
      then Prelude.dfinity_prelude
      else Prelude.prelude in
    let priv_env = { Typing.empty_env with Typing.privileged = true } in
    let prog, _t, sscope = Lib.Option.value
      (check_string priv_env prel_source "prelude") in
    let _v, dscope = Lib.Option.value
      (interpret_prog Interpret.empty_env "prelude" prog) in
    prelude := prog.Source.it;
    Typing.adjoin Typing.empty_env sscope,
      Interpret.adjoin Interpret.empty_env dscope
  with Not_found ->
    error Source.no_region "fatal" "initializing prelude failed";
    exit 1
