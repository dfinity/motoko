open Printf
module Typing = Typing_idl

type stat_env = Typing.scope
type env = stat_env              


(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let error at cat text =
  Error { Diag.sev = Diag.Error; at; cat; text }

let print_stat_ve =
  Typing.Env.iter (fun x t ->
    printf "%s %s : %s\n"
      "var" x (Arrange_idl.string_of_typ t)
  )

let dump_prog flag prog =
    if flag then
      Wasm.Sexpr.print 80 (Arrange_idl.prog prog)
    else ()
    
(* Parsing *)

type parse_result = Syntax_idl.prog Diag.result

let parse_with lexer parser name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parser Lexer.token lexer in
    dump_prog !Flags.dump_parse prog;
    Ok prog
  with
    | Lexer.Error (at, msg) ->
      error at "syntax" msg
    | Parser.Error ->
      error (Lexer.region lexer) "syntax" "unexpected token"

let parse_file filename : parse_result =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parser = Parser.parse_prog in
  let result = parse_with lexer parser filename in
  close_in ic;
  match result with
  | Ok prog -> Diag.return prog
  | Error e -> Error [e]


let initial_stat_env = Typing.empty_scope
let initial_env = initial_stat_env                     
   
(* Checking *)

type check_result = (Syntax_idl.prog * Typing.scope) Diag.result

let check_prog check senv name prog
  : Typing.scope Diag.result =
  phase "Checking" name;
  let r = check senv prog in
  (match r with
   | Ok (scope, _) ->
      print_stat_ve scope;
   | Error _ -> ());
  r

let check_with parse check senv name : check_result =
  Diag.bind (parse name) (fun prog ->
      Diag.bind (check_prog check senv name prog) (fun senv' ->
          Diag.return (prog, senv')
    ))

let check_file' senv name = check_with parse_file Typing.check_prog senv name
let check_file name = check_file' initial_stat_env name

(* Compilation *)

type compile_result = (Buffer.t, Diag.messages) result

let compile_js_file name : compile_result =
  match check_file name with
  | Error msgs -> Error msgs
  | Ok ((prog, scope), msgs) ->
     Diag.print_messages msgs;
     phase "Compiling" name;
     let buf = Compile_js.compile scope prog in
     Ok buf    
