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


(* Execute program *)

let run (stat_env, dyn_env) lexer parse infer name =
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try
    let prog = parse Lexer.token lexer in
    phase "Checking" name;
    let t, ((ve, te, ce) as stat_scope) = infer stat_env prog in
    let stat_env' = Typing.adjoin stat_env stat_scope in
    if !Flags.trace then begin
      print_ce ce;
      print_stat_ve ve
    end;
    phase "Interpreting" name;
    let vo, dyn_scope = Interpret.interpret_prog dyn_env prog in
    let dyn_env' = Interpret.adjoin dyn_env dyn_scope in
    phase "Finished" name;
    if !Flags.interactive then
      print_scope stat_env' stat_scope dyn_scope
    else if !Flags.trace then
      print_dyn_ve stat_env' dyn_scope;
    if !Flags.interactive && vo <> None && vo <> Some Value.unit then
      print_val stat_env' (Lib.Option.value vo) t;
    Some (stat_env', dyn_env')
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
    printf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
    if dump then begin
      printf "\n";
      Printexc.print_backtrace stderr; flush_all ();
      printf "\nLast environment:\n";
      print_dyn_ve_untyped (Interpret.get_last_env ()).Interpret.vals
    end;
    if !Flags.verbose then printf "\n";
    if not !Flags.interactive then exit 1;
    None


let update_envs envs = function
  | Some envs' -> envs'
  | None -> envs

let run_string envs s name =
  let lexer = Lexing.from_string s in
  let infer env prog = Type.unit, Typing.check_prog env prog in
  let result = run envs lexer Parser.parse_prog infer name in
  if !Flags.verbose then printf "\n";
  update_envs envs result

let run_file envs filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let infer env prog = Type.unit, Typing.check_prog env prog in
  let result = run envs lexer Parser.parse_prog infer filename in
  close_in ic;
  if !Flags.verbose then printf "\n";
  update_envs envs result


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

let run_stdin envs =
  let open Lexing in
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop envs =
    let result = run envs lexer Parser.parse_prog_interactive Typing.infer_prog "stdin" in
    if result = None then begin
      Lexing.flush_input lexer;
      (* Reset beginning-of-line, too, to sync consecutive positions. *)
      lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0}
    end;
    if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
    loop (update_envs envs result)
  in
  try loop envs with End_of_file ->
    printf "\n"


(* Argument handling *)

let args = ref []
let add_arg source = args := !args @ [source]

let argspec = Arg.align
[
  "-", Arg.Set Flags.interactive,
    " run interactively (default if no files given)";
  "-t", Arg.Set Flags.trace, " activate tracing";
  "-v", Arg.Set Flags.verbose, " verbose output";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "--version",
    Arg.Unit (fun () -> printf "%s\n" banner; Flags.interactive := false),
    " show version"
]

let () =
  Printexc.record_backtrace true;
  Flags.privileged := true;
  let envs0 = (Typing.empty_env, Interpret.empty_env) in
  let envs = run_string envs0 Prelude.prelude "prelude" in
  Flags.privileged := false;
  Arg.parse argspec add_arg usage;
  if !args = [] then Flags.interactive := true;
  if !Flags.interactive then printf "%s\n" banner;
  let envs' = List.fold_left run_file envs !args in
  if !Flags.interactive then run_stdin envs'
