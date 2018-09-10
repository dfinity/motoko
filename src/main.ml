open Printf

let name = "asc"
let version = "0.1"
let banner = "ActorScript " ^ version ^ " interpreter"
let usage = "Usage: " ^ name ^ " [option] [file ...]"


(*
let print_te =
  Type.Env.iter (fun x c ->
    printf "  type %s := %s\n" x (Con.to_string con)
  )
*)

let rec map_fold_left f x = function
  | [] -> (x, [])
  | (y::ys) ->
      let (x1, z)  = f x y in
      let (x2, zs) = map_fold_left f x1 ys in
      (x2, z::zs)

let print_ce =
  Con.Env.iter (fun c k ->
    let eq, params, typ = Type.strings_of_kind k in
    printf "type %s%s %s %s\n" (Con.to_string c) params eq typ
  )

let print_stat_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.immutable t in
    printf "%s %s : %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
  )

let print_dyn_ve env =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x env.Typing.vals in
    let t' = Type.immutable t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let print_dyn_ve_untyped =
  Value.Env.iter (fun x d ->
    printf "%s = %s\n" x (Value.string_of_def d)
  )

let print_scope env (ve, te, ce) dyn_ve =
  print_ce ce;
  print_dyn_ve env dyn_ve

let print_val env v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)

let phase heading filename =
  if !Flags.verbose then printf "-- %s %s:\n" heading filename


(* Typechecking *)

let typecheck to_stdout env lexer name =
  let infer env prog = Type.unit, Typing.check_prog env prog in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try
    let prog = Parser.parse_prog Lexer.token lexer in
    phase "Checking" name;
    let t, ((ve, te, ce) as stat_scope) = infer env prog in
    let env' = Typing.adjoin env stat_scope in
    if !Flags.trace then begin
      print_ce ce;
      print_stat_ve ve
    end;
    Some (env', (name, t, env', stat_scope, prog))
  with exn ->
    let r, sort, msg, dump =
      match exn with
      | Lexer.Error (at, msg) -> at, "syntax", msg, false
      | Parser.Error -> Lexer.region lexer, "syntax", "unexpected token", false
      | Typing.Error (at, msg) -> at, "type", msg, false
      | End_of_file -> raise exn
      | _ ->
        Interpret.get_last_region (), "fatal", Printexc.to_string exn, true
    in
    if dump then printf "\n";
    if to_stdout then begin
      printf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
      if !Flags.verbose then printf "\n";
    end else begin
      eprintf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
      if !Flags.verbose then eprintf "\n";
    end;
    None

let typecheck_string env s name =
  let lexer = Lexing.from_string s in
  typecheck false env lexer name

let typecheck_file env filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  match typecheck false env lexer filename with
  | Some r -> r
  | None   -> exit 1


(* Execute program *)

let run_mod dyn_env (name, t, stat_env, stat_scope, prog) =
  try
    phase "Interpreting" name;
    let vo, dyn_scope = Interpret.interpret_prog dyn_env prog in
    let dyn_env' = Interpret.adjoin dyn_env dyn_scope in
    phase "Finished" name;
    if !Flags.interactive then
      print_scope stat_env stat_scope dyn_scope
    else if !Flags.trace then
      print_dyn_ve stat_env dyn_scope;
    if !Flags.interactive && vo <> None && vo <> Some Value.unit then
      print_val stat_env (Lib.Option.value vo) t;
    if !Flags.verbose then printf "\n";
    Some dyn_env'
  with exn ->
    let r, sort, msg, dump =
      match exn with
      | _ ->
        Interpret.get_last_region (), "fatal", Printexc.to_string exn, true
    in
    if dump then printf "\n";
    printf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
    if dump then begin
      printf "\n";
      Printexc.print_backtrace stderr; flush_all ();
      printf "\nLast environment:\n";
      print_dyn_ve_untyped (Interpret.get_last_env ()).Interpret.vals
    end;
    if !Flags.verbose then printf "\n";
    None

let run_mod_or_fail dyn_env (name, t, stat_env, stat_scope, prog) =
    match run_mod dyn_env (name, t, stat_env, stat_scope, prog) with
    | Some r -> r
    | None -> exit 1


(* There is some duplication with `run` below. But here we want errors to stderr! *)
let compile_mod (name, _t, _stat_env, _stat_scope, prog) =
  phase "Compiling" name;
  Compile.compile prog

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

let parse_and_run_stdin (static_env, dynamic_env) lexer =
    match typecheck true static_env lexer "stdin" with
    | Some (senv', mod_closure) ->
      begin match run_mod dynamic_env mod_closure with
      | Some denv' -> Some (senv', denv')
      | None -> None
      end
    | None -> None

let loop_stdin envs =
  let open Lexing in
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop envs =
    match parse_and_run_stdin envs lexer with
    | None ->
      Lexing.flush_input lexer;
      (* Reset beginning-of-line, too, to sync consecutive positions. *)
      lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
      if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
      loop envs
    | Some envs' ->
      if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
      loop envs'
  in
  try loop envs with End_of_file ->
    printf "\n"

(* Prelude *)

let run_prelude (_: unit) =
  (* Run the prelude privileged, and before parsing the flags (to have Flags.verbose = false) *)
  Flags.privileged := true;
  match typecheck_string Typing.empty_env Prelude.prelude "prelude" with
  | Some (env1, prel) ->
    begin match run_mod Interpret.empty_env prel with
    | Some denv1 ->
      Flags.privileged := false;
      (env1, denv1)
    | None -> eprintf "Internal error: Prelude failed to run"; exit 1
    end
  | None -> eprintf "Internal error: Prelude failed to type-check"; exit 1

(* Argument handling *)

let args = ref []
let add_arg source = args := !args @ [source]

let argspec = Arg.align
[
  "-c", Arg.Set Flags.compile, " compile programs to WebAssembly";
  "-r", Arg.Set Flags.run, " interpret programs";
  "-i", Arg.Set Flags.interactive, " run interactive REPL (implies -r)";
  "--typecheck-only", Arg.Set Flags.tc_only, " typecheck only";
  "-t", Arg.Set Flags.trace, " activate tracing";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner),
    " show version"
]

let () =
  Printexc.record_backtrace true;

  let (env0, denv0) = run_prelude () in

  Arg.parse argspec add_arg usage;
  if !Flags.interactive then begin Flags.run := true end;

  if !Flags.interactive && !Flags.compile then begin
    eprintf "-i and -c are mutually exclusive"; exit 1
  end;
  if !Flags.run && !Flags.compile then begin
    eprintf "-r and -c are mutually exclusive"; exit 1
  end;
  if !Flags.run && !Flags.tc_only then begin
    eprintf "-r and --typecheck-only are mutually exclusive"; exit 1
  end;
  if !Flags.compile && !Flags.tc_only then begin
    eprintf "-c and --typecheck-only are mutually exclusive"; exit 1
  end;

  let (env1, tc'ed_mods) = map_fold_left typecheck_file env0 !args in

  if not !Flags.tc_only then begin
    if !Flags.run then begin
      if !Flags.interactive then printf "%s\n" banner;
      let denv1 = List.fold_left run_mod_or_fail denv0 (tc'ed_mods) in
      if !Flags.interactive then loop_stdin (env1, denv1);
    end else if !Flags.compile then begin
      List.iter compile_mod tc'ed_mods
    end else begin
      eprintf "Please select one of -c, -r, -i or --typecheck-only\n"; exit 1
    end
  end
