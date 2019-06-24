open Printf

(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let error at cat text =
  Error [{ Diag.sev = Diag.Error; at; cat; text }]

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
  | Error e -> Error e

let parse_file filename : parse_result =
  try parse_file filename
  with Sys_error s ->
    error Source.no_region "file" (sprintf "cannot open \"%s\"" filename)
             
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

module LibEnv = Env.Make(String)

let merge_env imports init_env lib_env =
  let disjoint_union env1 env2 : Typing.typ_env Diag.result =
    try Diag.return (Typing.Env.union (fun k v1 v2 ->
        (* TODO Add proper type equivalence check for env *)
        if v1 = v2 then Some v1 else raise (Typing.Env.Clash k)
      ) env1 env2)
    with Typing.Env.Clash k ->
      error Source.no_region "import" (sprintf "conflict type definition for %s" k) in
  let env_list = List.map (fun import -> LibEnv.find import lib_env) imports in
  Diag.fold disjoint_union init_env env_list

let chase_imports senv imports =
  let module S = Resolve_import.Set in
  let pending = ref S.empty in
  let lib_env = ref LibEnv.empty in
  let rec go file =
    if S.mem file !pending then
      error Source.no_region "import" (sprintf "file %s must not depend on itself" file)
    else if LibEnv.mem file !lib_env then
      Diag.return ()
    else begin
        pending := S.add file !pending;
        Diag.bind (parse_file file) (fun (prog, base) ->
            Diag.bind (Resolve_import.resolve prog base) (fun imports ->
                Diag.bind (go_set imports) (fun () ->
                    Diag.bind (merge_env imports senv !lib_env) (fun base_env ->
                        Diag.bind (check_prog base_env prog) (fun scope ->
                            lib_env := LibEnv.add file scope !lib_env;
                            pending := S.remove file !pending;
                            Diag.return ()
          )))))
      end
  and go_set todo = Diag.traverse_ go todo
  in Diag.map (fun () -> !lib_env) (go_set imports)

let load_prog parse senv =
  Diag.bind parse (fun (prog, base) ->
      Diag.bind (Resolve_import.resolve prog base) (fun imports ->
          Diag.bind (chase_imports senv imports) (fun lib_env ->
              Diag.bind (merge_env imports senv lib_env) (fun base_env ->
                  Diag.bind (check_prog base_env prog) (fun scope ->
                      Diag.return (prog, scope))))))
   
(* Only type checking *)

let initial_stat_env = Typing.empty_scope

let check_file file : load_result = load_prog (parse_file file) initial_stat_env
  
(* JS Compilation *)

type compile_result = Buffer.t Diag.result
                    
let compile_js_file file : compile_result =
  Diag.bind (check_file file)
    (fun (prog, senv) ->
      phase "JS Compiling" file;
      Diag.return (Compile_js.compile senv prog))
