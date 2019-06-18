open Printf

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

type rel_path = string
  
type parse_result = (Syntax.prog * rel_path) Diag.result

let parse_with lexer parser name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parser Lexer.token lexer name in
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
  let name = Filename.basename filename in
  let result = parse_with lexer parser name in
  close_in ic;
  match result with
  | Ok prog -> Diag.return (prog, filename)
  | Error e -> Error [e]

(* import file name resolution *)

type resolve_result = (Syntax.prog * Resolve_import.S.t) Diag.result

let resolve_prog (prog, base) : resolve_result =
  Diag.map (fun libraries -> (prog, libraries)) (Resolve_import.resolve prog base)

(* Type checking *)

let check_prog senv prog
  : Typing.scope Diag.result =
  phase "Checking" prog.Source.note;
  let r = Typing.check_prog senv prog in
  (match r with
   | Ok (scope, _) ->
      if !Flags.verbose then print_stat_ve scope;
   | Error _ -> ());
  r

(* Imported file loading *)

type load_result = (Syntax.prog * Typing.scope) Diag.result

let chase_imports senv0 imports : Typing.scope Diag.result =
  let open Resolve_import.S in
  let pending = ref empty in
  let senv = ref senv0 in
  let libraries = ref [] in
  let rec go f =
    if Typing.Env.mem f !senv then
      Diag.return ()
    else if mem f !pending then
      Error [{
                Diag.sev = Diag.Error; at = Source.no_region; cat = "import";
                text = sprintf "file %s must not depend on itself" f
        }]
    else begin
        pending := add f !pending;
        Diag.bind (parse_file f) (fun (prog, base) ->
            Diag.bind (Resolve_import.resolve prog base) (fun more_imports ->
                Diag.bind (go_set more_imports) (fun () ->
                    Diag.bind (check_prog !senv prog) (fun scope ->
                        libraries := (f, prog) :: !libraries;
                        senv := Typing.Env.adjoin !senv scope;
                        pending := remove f !pending;
                        Diag.return ()
          ))))
      end
  and go_set todo = Diag.traverse_ go (elements todo)
  in Diag.map (fun () -> !senv) (go_set imports)

let load_prog parse senv : load_result =
  Diag.bind parse (fun parsed ->
      Diag.bind (resolve_prog parsed) (fun (prog, libraries) ->
          Diag.bind (chase_imports senv libraries) (fun senv' ->
              Diag.bind (check_prog senv' prog) (fun senv'' ->
                  Diag.return (prog, senv'')
    ))))
  
(* Only type checking *)

let initial_stat_env = Typing.empty_scope

let check_file file : load_result = load_prog (parse_file file) initial_stat_env  
  
(* JS Compilation *)

type compile_result = Buffer.t Diag.result
                    
let compile_js_file file : compile_result =
  Diag.bind (load_prog (parse_file file) initial_stat_env)
    (fun (prog, senv) ->
      phase "JS Compiling" file;
      Diag.return (Compile_js.compile senv prog))
