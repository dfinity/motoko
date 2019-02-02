open Printf


module Await = Awaitopt   (* for more naive cps translation, use Await *)
module Async = Async

(*TBD module Tailcall = Tailcall *)

type stat_env = Typing.scope
type dyn_env = Interpret.env
type env = stat_env * dyn_env

(*TBD*)
module Await_ir = Awaitopt_ir
module Tailcall_ir = Tailcall_ir
module Async_ir = Async_ir

(* Diagnostics *)

let phase heading name =
  if !Flags.verbose then printf "-- %s %s:\n%!" heading name

let error at cat text =
  Error { Diag.sev = Diag.Error; at; cat; text }

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

let print_dyn_ve scope =
  Value.Env.iter (fun x d ->
    let t = Type.Env.find x scope.Typing.val_env in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let eprint_dyn_ve_untyped =
  Value.Env.iter (fun x d ->
    eprintf "%s = %s\n%!" x (Value.string_of_def d)
  )

let print_scope senv scope dve =
  print_ce scope.Typing.con_env;
  print_dyn_ve senv dve

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)


(* Parsing *)

type parse_result = (Syntax.prog, Diag.message) result

let dump_prog flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange.prog prog)
    else ()

let dump_ir flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange_ir.prog prog)
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
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  r

let transform transform_name transform flag prog name  =
  if flag then
    begin
      phase transform_name name;
      let prog' = transform prog in
      dump_prog Flags.dump_lowering prog';
      prog'
    end
  else prog

let transform_ir transform_name transform flag prog name  =
  if flag then
    begin
      phase transform_name name;
      let prog' = transform prog in
      dump_ir Flags.dump_lowering prog';
      prog'
    end
  else prog


let await_lowering =
  transform "Await Lowering" Await.t_prog

let await_ir_lowering =
  transform_ir "Await IR Lowering" Await_ir.t_prog

let async_lowering =
  transform "Async Lowering" Async.t_prog

let async_ir_lowering =
  transform_ir "Async IR Lowering" Async_ir.t_prog

let tailcall_optimization =
  transform_ir "Tailcall optimization" Tailcall_ir.prog

let check_with parse infer senv name : check_result =
  match parse name with
  | Error e -> Error [e]
  | Ok prog ->
    Diag.map_result (fun (t, scope) -> (prog, t, scope))
      (check_prog infer senv name prog)

let infer_prog_unit senv prog =
  Diag.map_result (fun (scope) -> (Type.unit, scope))
    (Typing.check_prog senv prog)

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
    Diag.print_message (Diag.fatal_error (Interpret.get_last_region ()) (Printexc.to_string exn));
    eprintf "\nLast environment:\n%!";
    eprint_dyn_ve_untyped Interpret.((get_last_env ()).vals);
    eprintf "\n";
    Printexc.print_backtrace stderr;
    eprintf "%!";
    None

let interpret_with check (senv, denv) name : interpret_result =
  match check senv name with
  | Error msgs ->
    Diag.print_messages msgs;
    None
  | Ok ((prog, t, sscope), msgs) ->
    Diag.print_messages msgs;
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

let prelude_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prelude failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parser = Parser.parse_prog in
  match parse_with Lexer.Privileged lexer parser prelude_name with
  | Error e -> prelude_error "parsing" [e]
  | Ok prog ->
    match check_prog infer_prog_unit Typing.empty_scope prelude_name prog with
    | Error es -> prelude_error "checking" es
    | Ok ((_t, sscope), msgs) ->
      let senv = Typing.adjoin_scope Typing.empty_scope sscope in
      prog, senv

let prelude, initial_stat_env = check_prelude ()

let run_prelude () : dyn_env =
  match interpret_prog Interpret.empty_env prelude_name prelude with
  | None -> prelude_error "initializing" []
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
      let senv' = Typing.adjoin_scope senv sscope in
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
type compile_result = (CustomModule.extended_module, Diag.messages) result

let compile_with check mode name : compile_result =
  match check initial_stat_env name with
  | Error msgs -> Error msgs
  | Ok ((prog, _t, scope), msgs) ->
    Diag.print_messages msgs;
    let prelude = Desugar.prog prelude in
    let prog = Desugar.prog prog in
    ignore (Check_ir.check_prog initial_stat_env prog);
    let prog = await_ir_lowering true prog name in
    ignore (Check_ir.check_prog initial_stat_env prog);
    let prog = async_ir_lowering true prog name in
    ignore (Check_ir.check_prog initial_stat_env prog);
    let prog = tailcall_optimization true prog name in
    ignore (Check_ir.check_prog initial_stat_env prog);
    phase "Compiling" name;
    let module_ = Compile.compile mode name prelude [prog] in
    Ok module_

let compile_string mode s name =
  compile_with (fun senv name -> check_string senv s name) mode name
let compile_file mode file name = compile_with check_file mode name
let compile_files mode files name =
  compile_with (fun senv _name -> check_files senv files) mode name


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
