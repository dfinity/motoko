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

let print_ve =
  Type.Env.iter (fun x t ->
    let t' = Type.immutable t in
    printf "%s %s : %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
  )

let print_dyn_ve {Typing.cons = ce; vals = ve; _} dyn_ve =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x ve in
    let t' = Type.immutable t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x (Type.string_of_typ t')
      (match Lib.Promise.value_opt d with
      | None -> "_"
      | Some v -> Value.string_of_val ce t' v
      )
  ) dyn_ve

let print_debug_ve =
  Value.Env.iter (fun x d ->
    printf "%s = %s\n" x (Value.debug_string_of_def d)
  )

let print_scope context (ve, te, ce) dyn_ve =
  print_ce ce;
  print_dyn_ve context dyn_ve

let trace heading filename =
  if !Flags.trace then printf "-- %s %s:\n" heading filename


(* Execute program *)

let run (stat_context, dyn_context) lexer parse name =
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try
    let prog = parse Lexer.token lexer in 
    trace "Checking" name;
    let (ve, te, ce) as stat_scope = Typing.check_prog stat_context prog in
    let stat_context' = Typing.adjoin stat_context stat_scope in
    if !Flags.trace then begin
      print_ce ce;
      print_ve ve
    end;
    trace "Interpreting" name;
    let dyn_scope = Interpret.interpret_prog dyn_context prog in
    let dyn_context' = Interpret.adjoin dyn_context dyn_scope in
    trace "Finished" name;
    if !Flags.interactive then
      print_scope stat_context' stat_scope dyn_scope
    else if !Flags.trace then
      print_dyn_ve stat_context' dyn_scope;
    Some (stat_context', dyn_context')
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
      printf "\nLast context:\n";
      print_debug_ve (Interpret.get_last_context ()).Interpret.vals
    end;
    if !Flags.trace then printf "\n";
    if not !Flags.interactive then exit 1;
    None


let update_contexts contexts = function
  | Some contexts' -> contexts'
  | None -> contexts

let run_file contexts filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let result = run contexts lexer Parser.parse_prog filename in
  close_in ic;
  if !Flags.trace then printf "\n";
  update_contexts contexts result

(* There is some duplication with `run` below. But here we want errors to stderr! *)
let compile filename =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parse = Parser.parse_prog in
  let name = filename in
  let stat_context = Typing.empty_context in
  lexer.Lexing.lex_curr_p <-
    {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
  try
    let prog = parse Lexer.token lexer in 
    trace "Checking" name;
    let (ve, te, ce) as stat_scope = Typing.check_prog stat_context prog in
    if !Flags.trace then begin
      print_ce ce;
      print_ve ve
    end;
    trace "Compiling" name;
    Compile.compile prog
  with exn ->
    let r, sort, msg, dump =
      match exn with
      | Lexer.Error (at, msg) -> at, "syntax", msg, false
      | Parser.Error -> Lexer.region lexer, "syntax", "unexpected token", false
      | Typing.Error (at, msg) -> at, "type", msg, false
      | _ -> raise exn
    in
    eprintf "%s: %s error, %s\n" (Source.string_of_region r) sort msg;
    exit 1

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

let run_stdin contexts =
  let open Lexing in
  let lexer = Lexing.from_function lexer_stdin in
  let rec loop contexts =
    let result = run contexts lexer Parser.parse_prog_interactive "stdin" in
    if result = None then begin
      Lexing.flush_input lexer;
      (* Reset beginning-of-line, too, to sync consecutive positions. *)
      lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0}
    end;
    if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
    loop (update_contexts contexts result)
  in
  try loop contexts with End_of_file ->
    printf "\n"


(* Argument handling *)

let args = ref []
let add_arg source = args := !args @ [source]; Flags.interactive := false

let argspec = Arg.align
[
  "-", Arg.Set Flags.interactive,
    " run interactively (default if no files given)";
  "-c", Arg.Set Flags.compile, "compile to .wat";
  "-t", Arg.Set Flags.trace, " trace phases";
  "-d", Arg.Set Flags.debug, " debug, trace calls";
  "-p", Arg.Set_int Flags.print_depth, " set print depth";
  "-v", Arg.Unit (fun () -> printf "%s\n" banner; Flags.interactive := false), " show version"
]

let initial_contexts = (Typing.empty_context, Interpret.empty_context)

let () =
  Printexc.record_backtrace true;
  Arg.parse argspec add_arg usage;
  if !Flags.compile then begin
    List.iter compile !args
  end else begin
    let contexts = List.fold_left run_file initial_contexts !args in
    if !Flags.interactive then begin
      printf "%s\n" banner;
      run_stdin contexts
    end
  end
