(** Maintenance note:
    Update of the expected values could be done via [dune runtest --auto-promote].
*)

let parse_from_lexbuf lexer_mode filename lexbuf : Mo_def.Syntax.prog Diag.result =
  let open Mo_frontend in
  let open Diag.Syntax in
  let lexer, triv_table = Lexer.tokenizer lexer_mode lexbuf in
  let () = Parser_lib.triv_table := triv_table in
  let start =  Parser.Incremental.parse_prog lexbuf.Lexing.lex_start_p in
  let error_details = 4 in
  let* mk_syntax =
    Mo_frontend.Parsing.parse
      ~recovery:false
      lexer_mode
      error_details
      start
      lexer
      lexbuf
  in
  Diag.return @@ mk_syntax filename

let parse_from_string lexer_mode filename source : Mo_def.Syntax.prog Diag.result =
  let lexbuf = Lexing.from_string source in
  parse_from_lexbuf lexer_mode filename lexbuf

let infer_prog prog senv async_cap : (Mo_def.Syntax.prog * Mo_types.Scope.t) Diag.result =
  let open Diag.Syntax in
  let filename = prog.Source.note.Mo_def.Syntax.filename in
  let* _typ, sscope =
    Mo_types.Cons.session ~scope:filename (fun () ->
      Mo_frontend.Typing.infer_prog
        ~viper_mode:false
        senv
        None
        async_cap
        prog)
  in
  Diag.return (prog, sscope)

let check_builtin what src senv0 : Mo_def.Syntax.prog * Mo_types.Scope.t =
  let builtin_error phase what (msgs : Diag.messages) =
    Printf.eprintf "%s %s failed\n" phase what;
    Diag.print_messages msgs;
    exit 1
  in
  match parse_from_string Mo_frontend.Lexer.mode_priv what src with
  | Error es -> builtin_error "parsing" what es
  | Ok (prog, _ws) ->
    match infer_prog prog senv0 Mo_types.Async_cap.NullCap with
    | Error es -> builtin_error "checking" what es
    | Ok ((_t, sscope), _ws) ->
      let senv1 = Mo_types.Scope.adjoin senv0 sscope in
      prog, senv1

let prelude, initial_stat_env0 =
  check_builtin "prelude" Prelude.prelude Mo_frontend.Typing.initial_scope
let internals, initial_stat_env =
  check_builtin "internals" Prelude.internals initial_stat_env0

module Sexpr_set = Set.Make (struct
  type t = Wasm.Sexpr.sexpr

  let compare =
    let open Wasm.Sexpr in
    let rec compare_sexprs a b = List.compare compare_sexpr a b
    and compare_sexpr a b =
      match a, b with
      | Atom n1, Atom n2 -> String.compare n1 n2
      | Node (n1, as1), Node (n2, as2) ->
        (match String.compare n1 n2 with
        | 0 -> compare_sexprs as1 as2
        | o -> o)
      | Atom _, Node _ -> -1
      | Node _, Atom _ -> +1
    in
    compare_sexpr
end)

let gather_let_srcs sexpr =
  let open Wasm.Sexpr in
  let srcs_of_labs name args =
    match name, args with
    | "Obj", Atom "Object" :: fields ->
      List.find_map
        (function
          | Node (lab, _typ :: _depr :: _region :: srcs) -> Some (lab, srcs)
          | _ -> None)
        fields
    | _, _ -> None
  in
  let rec go acc = function
    | Atom _ -> acc
    | Node (name, args) ->
      List.fold_left
        (fun acc arg ->
          let acc =
            match srcs_of_labs name args with
            | None -> acc
            | Some (lab, srcs) -> Sexpr_set.add (Node (lab, srcs)) acc
          in
          go acc arg)
        acc
        args
  in
  List.of_seq @@ Sexpr_set.to_seq @@ go Sexpr_set.empty sexpr

let show (prog : Mo_def.Syntax.prog Diag.result) : unit =
  let show_msgs msgs =
    String.concat "\n" (List.map Diag.string_of_message msgs)
  in
  match prog with
  | Error msgs -> Format.printf "Diagnostics:\n%s\n" (show_msgs msgs)
  | Ok (prog, msgs) ->
    let filename = prog.Source.note.Mo_def.Syntax.filename in
    let module Arrange = Mo_def.Arrange.Make (struct
      let include_sources = false
      let include_type_rep = true
      let include_types = true
      let include_parenthetical = false
      let include_docs = None
      let main_file = Some filename
    end) in
    Format.printf "Ok:\n";
    List.iter
      (fun srcs -> Format.printf "%s" (Wasm.Sexpr.to_string 80 srcs))
      (gather_let_srcs @@ Arrange.prog prog);
    match msgs with
    | [] -> ()
    | _ :: _ -> Format.printf "With diagnostics:\n%s\n" (show_msgs msgs)

let run_test source =
  Fun.protect
    (fun () ->
      Mo_config.Flags.typechecker_combine_srcs := true;
      let filename = "test-field-srcs.mo" in
      show begin
        let open Diag.Syntax in
        let* prog = parse_from_string Mo_frontend.Lexer.mode filename source in
        let* prog, _sscope =
          infer_prog prog initial_stat_env (Mo_types.Async_cap.initial_cap ())
        in
        Diag.return prog
      end)
    ~finally:(fun () -> Mo_config.Flags.typechecker_combine_srcs := false)

let%expect_test "" =
  let s = {|actor {
  class Class1() = self {
    public func meth(_ : Int) : Nat {
      return 1
    }
  };

  class Class2() = self {
    public func meth(_ : Nat) : Int {
      return 2
    }
  };

  public func test() : async Int {
    let c1 : Class2 = Class1();
    return c1.meth(42)
  }
}
|}
  in
  run_test s;
  [%expect {|
    Ok:
    (caller)
    (meth (@@ (Pos  3 16) (Pos  3 20)) (@@ (Pos  9 16) (Pos  9 20))) |}]
