open Printf

type stat_env = Typing.scope
type dyn_env = Interpret.scope
type env = stat_env * dyn_env


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

let print_dyn_ve scope =
  Value.Env.iter (fun x d ->
    if Syntax.is_import_id x then () else
    let t = Type.Env.find x scope.Typing.val_env in
    let t' = Type.as_immut t in
    printf "%s %s : %s = %s\n"
      (if t == t' then "let" else "var") x
      (Type.string_of_typ t') (Value.string_of_def d)
  )

let print_scope senv scope dve =
  print_ce scope.Typing.con_env;
  print_dyn_ve senv dve

let print_val _senv v t =
  printf "%s : %s\n" (Value.string_of_val v) (Type.string_of_typ t)

(* Dumping *)

let dump_prog flag prog =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange.prog prog)
    else ()

let dump_ir flag prog_ir =
    if !flag then
      Wasm.Sexpr.print 80 (Arrange_ir.prog prog_ir)
    else ()

(* Parsing *)

type rel_path = string

type parse_result = (Syntax.prog * rel_path) Diag.result

let parse_with mode lexer parse name =
  try
    phase "Parsing" name;
    lexer.Lexing.lex_curr_p <-
      {lexer.Lexing.lex_curr_p with Lexing.pos_fname = name};
    let prog = parse (Lexer.token mode) lexer name in
    dump_prog Flags.dump_parse prog;
    Ok prog
  with
    | Lexer.Error (at, msg) ->
      error at "syntax" msg
    | Parser.Error ->
      error (Lexer.region lexer) "syntax" "unexpected token"

let parse_string s name : parse_result =
  let lexer = Lexing.from_string s in
  let parse = Parser.parse_prog in
  match parse_with Lexer.Normal lexer parse name with
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)
  | Error e -> Error [e]

let parse_file filename : parse_result =
  let ic = open_in filename in
  let lexer = Lexing.from_channel ic in
  let parse = Parser.parse_prog in
  let name = Filename.basename filename in
  let result = parse_with Lexer.Normal lexer parse name in
  close_in ic;
  match result with
  | Ok prog -> Diag.return (prog, filename)
  | Error e -> Error [e]

(* Import file name resolution *)

type resolve_result = (Syntax.prog * Resolve_import.S.t) Diag.result

let resolve (prog, base) : resolve_result =
  Diag.map_result (fun imports -> (prog, imports)) (Resolve_import.resolve prog base)


(* Checking *)

let infer_prog senv prog
  : (Type.typ * Typing.scope) Diag.result =
  phase "Checking" prog.Source.note;
  let r = Typing.infer_prog senv prog in
  if !Flags.trace && !Flags.verbose then begin
    match r with
    | Ok ((_, scope), _) ->
      print_ce scope.Typing.con_env;
      print_stat_ve scope.Typing.val_env;
      dump_prog Flags.dump_tc prog;
    | Error _ -> ()
  end;
  r

let rec check_progs senv progs : Typing.scope Diag.result =
  match progs with
  | [] -> Diag.return senv
  | p::ps ->
    Diag.bind (infer_prog senv p) (fun (_t, sscope) ->
      let senv' = Typing.adjoin_scope senv sscope in
      check_progs senv' ps
    )

let check_import senv filename prog : Typing.scope Diag.result =
  phase "Checking" prog.Source.note;
  Typing.check_import senv (filename, prog)

(* Imported file loading *)

type imports = (rel_path * Syntax.prog) list

(*
Loading a file (or string) implies lexing, parsing, resolving imports, and
typechecking. The resulting prog is typechecked.
The Typing.scope field in load_result is the accumulated scope.
When we load a declaration (i.e from the REPL), we also care about the type
and the newly added scopes, these are returned separately.
*)
type load_result =
  (imports * Syntax.prog list * Typing.scope) Diag.result
type load_decl_result =
  (imports * Syntax.prog * Typing.scope * Type.typ * Typing.scope) Diag.result

let chase_imports senv0 to_load : (imports * Typing.scope) Diag.result =
  let open Resolve_import.S in
  let todo = ref to_load in
  let seen = ref empty in
  let senv = ref senv0 in
  let imports = ref [] in

  let rec go () =
    match min_elt_opt !todo with
    | None ->
      Diag.return ()
    | Some f when mem f !seen->
      Error [ { Diag.sev = Diag.Error; at = Source.no_region; cat = "import";
              text = Printf.sprintf "file %s must not depend on itself" f } ]
    | Some f ->
      todo := remove f !todo;
      seen := add f !seen;
      Diag.bind (parse_file f) (fun (prog, base) ->
      Diag.bind (Static.prog prog) (fun () ->
      Diag.bind (Resolve_import.resolve prog base) (fun more_imports ->
      todo := union !todo more_imports;
      imports := (f, prog) :: !imports; (* NB: Do this before recursing *)
      Diag.bind (go ()) (fun () ->
      Diag.bind (check_import !senv f prog) (fun sscope ->
      senv := Typing.adjoin_scope !senv sscope;
      Diag.return ()
      )))))
  in
  Diag.bind (go ()) (fun () -> Diag.return (!imports, !senv))


let load_many parse senv : load_result =
  Diag.bind parse (fun parsed ->
  Diag.bind (Diag.traverse resolve parsed) (fun rs ->
  let progs' = List.map fst rs in
  let imports =
    List.fold_left Resolve_import.S.union Resolve_import.S.empty
    (List.map snd rs) in
  Diag.bind (chase_imports senv imports) (fun (imports, senv') ->
  Diag.bind (check_progs senv' progs') (fun senv'' ->
  Diag.return (imports, progs', senv'')
  ))))

let load_one parse_one senv : load_decl_result =
  Diag.bind parse_one (fun parsed ->
  Diag.bind (resolve parsed) (fun (prog, imports) ->
  Diag.bind (chase_imports senv imports) (fun (imports, senv') ->
  Diag.bind (infer_prog senv' prog) (fun (t, sscope) ->
  let senv'' = Typing.adjoin_scope senv' sscope in
  Diag.return (imports, prog, senv'', t, sscope)
  ))))


(* IR transforms *)

let transform transform_name trans env prog name =
  phase transform_name name;
  let prog_ir' : Ir.prog = trans env prog in
  dump_ir Flags.dump_lowering prog_ir';
  Check_ir.check_prog env transform_name prog_ir';
  prog_ir'

let transform_if transform_name trans flag env prog name =
  if flag then transform transform_name trans env prog name
  else prog

let desugar =
  transform "Desugaring" Desugar.transform

let await_lowering =
  transform_if "Await Lowering" (fun _ -> Await.transform)

let async_lowering =
  transform_if "Async Lowering" Async.transform

let serialization =
  transform_if "Synthesizing serialization code" Serialization.transform

let tailcall_optimization =
  transform_if "Tailcall optimization" (fun _ -> Tailcall.transform)

let show_translation =
  transform_if "Translate show" Show.transform

(* Interpretation (Source) *)

let interpret_prog denv prog : (Value.value * Interpret.scope) option =
  try
    phase "Interpreting" prog.Source.note;
    match Interpret.interpret_prog denv prog with
    | None, _ -> None
    | Some v, scope -> Some (v, scope)
  with exn ->
    (* For debugging, should never happen. *)
    Interpret.print_exn exn;
    None

let rec interpret_progs denv progs : Interpret.scope option =
  match progs with
  | [] -> Some denv
  | p::ps ->
    match interpret_prog denv p with
    | Some (_v, dscope) ->
      let denv' = Interpret.adjoin_scope denv dscope in
      interpret_progs denv' ps
    | None -> None

(* Prelude *)

let prelude_name = "prelude"

let prelude_error phase (msgs : Diag.messages) =
  Printf.eprintf "%s prelude failed\n" phase;
  Diag.print_messages msgs;
  exit 1

let check_prelude () : Syntax.prog * stat_env =
  let lexer = Lexing.from_string Prelude.prelude in
  let parse = Parser.parse_prog in
  match parse_with Lexer.Privileged lexer parse prelude_name with
  | Error e -> prelude_error "parsing" [e]
  | Ok prog ->
    let senv0 = Typing.empty_scope  in
    match infer_prog senv0 prog with
    | Error es -> prelude_error "checking" es
    | Ok ((_t, sscope), msgs) ->
      let senv1 = Typing.adjoin_scope senv0 sscope in
      prog, senv1

let prelude, initial_stat_env = check_prelude ()

let run_prelude () : dyn_env =
  match interpret_prog Interpret.empty_scope prelude with
  | None -> prelude_error "initializing" []
  | Some (_v, dscope) ->
    Interpret.adjoin_scope Interpret.empty_scope dscope

let initial_dyn_env = run_prelude ()

(* Only checking *)

type check_only_result = unit Diag.result

let check_only_files files : check_only_result =
  Diag.map_result (fun _ -> ())
    (load_many (Diag.traverse parse_file files) initial_stat_env)

let check_only_string s name : check_only_result =
  Diag.map_result (fun _ -> ())
    (load_one (parse_string s name) initial_stat_env)

(* Running *)

let output_scope (senv, _) t v sscope dscope =
  print_scope senv sscope dscope;
  if v <> Value.unit then print_val senv v t

let is_exp dec = match dec.Source.it with Syntax.ExpD _ -> true | _ -> false

let rec interpret_imports denv imports : Interpret.scope =
  match imports with
  | [] -> denv
  | (f, p)::is ->
    phase "Interpreting" p.Source.note;
    let dscope = Interpret.interpret_import denv (f, p) in
    let denv' = Interpret.adjoin_scope denv dscope in
    interpret_imports denv' is

let interpret_files files : Interpret.scope Diag.result =
  Diag.bind (Diag.flush_messages
    (load_many (Diag.traverse parse_file files) initial_stat_env))
    (fun (imports, progs, _sscope) ->
  let denv0 = initial_dyn_env in
  let denv1 = interpret_imports denv0 imports in
  match interpret_progs denv1 progs with
  | None -> Error []
  | Some denv2 -> Diag.return denv2
  )

let run_files files : unit Diag.result =
  Diag.map_result (fun _ -> ()) (interpret_files files)

(* Compilation *)

type compile_mode = Compile.mode = WasmMode | DfinityMode
type compile_result = (CustomModule.extended_module, Diag.messages) result

let combine_files senv imports progs : Syntax.prog =
  (* This is a hack until the backend has explicit support for imports *)
  let open Source in
  { it = List.map (fun (f, prog) ->
      let t = Type.Env.find (Syntax.id_of_full_path f).it senv.Typing.val_env in
      { it = Syntax.LetD
        ( { it = Syntax.VarP (Syntax.id_of_full_path f)
          ; at = no_region
          ; note = t
          }
        , { it = Syntax.BlockE prog.it
          ; at = no_region
          ; note = { Syntax.empty_typ_note with Syntax.note_typ = t }
          }
        )
      ; at = no_region
      ; note = { Syntax.empty_typ_note with Syntax.note_typ = t }
      }) imports
    @ List.concat (List.map (fun p -> p.it) progs)
  ; at = no_region
  ; note = match progs with
    | [prog] -> prog.Source.note
    | _ -> "all"
  }

let lower_prog prog =
  let name = prog.Source.note in
  let prog_ir = desugar initial_stat_env prog name in
  let prog_ir = await_lowering true initial_stat_env prog_ir name in
  let prog_ir = async_lowering true initial_stat_env prog_ir name in
  let prog_ir = serialization true initial_stat_env prog_ir name in
  let prog_ir = tailcall_optimization true initial_stat_env prog_ir name in
  let prog_ir = show_translation true initial_stat_env prog_ir name in
  prog_ir

let compile_prog mode prog : compile_result =
  let prelude_ir = Desugar.transform Typing.empty_scope prelude in
  let prog_ir = lower_prog prog in
  phase "Compiling" prog.Source.note;
  let module_name = Filename.remove_extension prog.Source.note in
  let module_ = Compile.compile mode module_name prelude_ir [prog_ir] in
  Ok module_

let compile_files mode files : compile_result =
  match load_many (Diag.traverse parse_file files) initial_stat_env with
  | Error msgs -> Error msgs
  | Ok ((imports, progs, senv), msgs) ->
    Diag.print_messages msgs;
    let prog = combine_files senv imports progs in
    compile_prog mode prog

let compile_string mode s name : compile_result =
  match load_one (parse_string s name) initial_stat_env with
  | Error msgs -> Error msgs
  | Ok ((imports, prog, senv, _t, _sscope), msgs) ->
    Diag.print_messages msgs;
    let prog = combine_files senv imports [prog] in
    compile_prog mode prog

(* Interpretation (IR) *)

let interpret_ir_prog prog =
  let prelude_ir = Desugar.transform Typing.empty_scope prelude in
  let prog_ir = lower_prog prog in
  phase "Compiling" prog.Source.note;
  let denv0 = Interpret_ir.empty_scope in
  let _, dscope = Interpret_ir.interpret_prog denv0 prelude_ir in
  let denv1 = Interpret_ir.adjoin_scope denv0 dscope in
  let _, _ = Interpret_ir.interpret_prog denv1 prog_ir in
  ()


let interpret_ir_files files =
  match load_many (Diag.traverse parse_file files) initial_stat_env with
  | Error msgs -> Error msgs
  | Ok ((imports, progs, senv), msgs) ->
    Diag.print_messages msgs;
    let prog = combine_files senv imports progs in
    interpret_ir_prog prog;
    Diag.return ()

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

let parse_lexer lexer : parse_result =
  let open Lexing in
  if lexer.lex_curr_pos >= lexer.lex_buffer_len - 1 then continuing := false;
  match parse_with Lexer.Normal lexer Parser.parse_prog_interactive "stdin" with
  | Error e ->
    Lexing.flush_input lexer;
    (* Reset beginning-of-line, too, to sync consecutive positions. *)
    lexer.lex_curr_p <- {lexer.lex_curr_p with pos_bol = 0};
    Error [e]
  | Ok prog -> Diag.return (prog, Filename.current_dir_name)

let run_stdin lexer (senv, denv) : env option =
  match load_one (parse_lexer lexer) senv with
  | Error msgs ->
    Diag.print_messages msgs;
    if !Flags.verbose then printf "\n";
    None
  | Ok ((imports, prog, senv', t, sscope), msgs) ->
    Diag.print_messages msgs;
    let denv' = interpret_imports denv imports in
    match interpret_prog denv' prog with
    | None ->
      if !Flags.verbose then printf "\n";
      None
    | Some (v, dscope) ->
      phase "Finished" "stdin";
      let denv' = Interpret.adjoin_scope denv dscope in
      let env' = (senv', denv') in
      (* TBR: hack *)
      let t', v' =
        if prog.Source.it <> [] && is_exp (Lib.List.last prog.Source.it)
        then t, v
        else Type.unit, Value.unit
      in
      output_scope env' t' v' sscope dscope;
      if !Flags.verbose then printf "\n";
      Some env'

let run_files_and_stdin files =
  match load_many (Diag.traverse parse_file files) initial_stat_env with
  | Error msgs -> Error msgs
  | Ok ((imports, progs, senv), msgs) ->
    let lexer = Lexing.from_function lexer_stdin in
    Diag.bind (interpret_files files) (fun denv ->
      let rec loop env = loop (Lib.Option.get (run_stdin lexer env) env) in
      try loop (senv, denv) with End_of_file ->
        printf "\n%!";
        Diag.return ()
    )
