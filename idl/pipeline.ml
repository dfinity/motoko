open Printf

type stat_env = Typing.scope
type env = stat_env              


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

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)


let dump_prog flag prog =
    printf "Dump prog\n";
    if flag then
      Wasm.Sexpr.print 80 (Arrange_idl.prog prog)
    else ()
(*
let dump_ir flag prog_ir =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange_ir.prog prog_ir)
    else ()
 *)
    
(* Parsing *)

type parse_result = Syntax_idl.prog Diag.result

let parse_with lexer parser name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parser Lexer.token lexer in
    dump_prog true prog;
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
(*
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
    (*dump_prog !Flags.dump_tc prog;*)
    | Error _ -> ()
  end;
  r
 *)
(*
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

let run_string env s =
  run_with (fun env name -> interpret_string env s name) output_dscope env
let run_file env n = run_with interpret_file output_dscope env n
let run_files env = function
  | [n] -> run_file env n
  | ns ->
    run_with (fun env _name -> interpret_files env ns) output_dscope env "all"


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
    phase "Compiling" name;
    let module_ = Compile.compile mode name prelude_ir [prog_ir] in
    Ok module_

let compile_string mode s name =
  compile_with (fun senv name -> check_string senv s name) mode name
let compile_file mode file name = compile_with check_file mode name
let compile_files mode files name =
  compile_with (fun senv _name -> check_files senv files) mode name
 *)

