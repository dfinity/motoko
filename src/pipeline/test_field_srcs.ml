(** Maintenance note:
    Update of the expected values could be done via [dune runtest --auto-promote].
*)

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

let rec remove_srcs =
  let open Wasm.Sexpr in
  let is_not_src = function
    | Atom _ -> true
    | Node (name, _) -> name <> "@@"
  in
  function
  | Atom _ as atom -> atom
  | Node (name, args) ->
    Node (name, List.map remove_srcs @@ List.filter is_not_src args)

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

let arrange filename : (module Mo_def.Arrange.S) =
  (module
    (Mo_def.Arrange.Make (struct
      let include_sources = false
      let include_type_rep = true
      let include_types = true
      let include_parenthetical = false
      let include_docs = None
      let main_file = Some filename
    end)))

let show_msgs msgs = String.concat "\n" (List.map Diag.string_of_message msgs)

let show (prog : Mo_def.Syntax.prog Diag.result) : unit =
  match prog with
  | Error msgs -> Format.printf "Diagnostics:\n%s\n" (show_msgs msgs)
  | Ok (prog, msgs) ->
    let filename = prog.Source.note.Mo_def.Syntax.filename in
    let module Arrange = (val arrange filename) in
    Format.printf "Ok:\n";
    List.iter
      (fun srcs -> Format.printf "%s" (Wasm.Sexpr.to_string 80 srcs))
      (gather_let_srcs @@ Arrange.prog prog);
    match msgs with
    | [] -> ()
    | _ :: _ -> Format.printf "With diagnostics:\n%s\n" (show_msgs msgs)

let run_get_sources_test source =
  let open Diag.Syntax in
  let infer_prog prog senv async_cap : Mo_def.Syntax.prog Diag.result =
    let filename = prog.Source.note.Mo_def.Syntax.filename in
    let* _typ, _sscope =
      Mo_types.Cons.session ~scope:filename (fun () ->
        Mo_frontend.Typing.infer_prog
          ~viper_mode:false
          senv
          None
          async_cap
          prog)
    in
    Diag.return prog
  in
  let filename = "test-field-srcs.mo" in
  Fun.protect
    (fun () ->
      Mo_config.Flags.typechecker_combine_srcs := true;
      show begin
        let* prog, _name = Pipeline.parse_string filename source in
        infer_prog prog Pipeline.initial_stat_env (Mo_types.Async_cap.initial_cap ())
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
  run_get_sources_test s;
  [%expect {|
    Ok:
    (caller)
    (meth
      (@@ (Pos test-field-srcs.mo 3 16) (Pos test-field-srcs.mo 3 20))
      (@@ (Pos test-field-srcs.mo 9 16) (Pos test-field-srcs.mo 9 20))
    ) |}]

let run_compare_typed_asts_test filename =
  let open Diag.Syntax in
  let load_prog () =
    let* _libs, progs_no_srcs, _sscope =
      Mo_types.Cons.session ~scope:filename (fun () ->
        Pipeline.load_progs
          ~viper_mode:false
          ~check_actors:false
          Pipeline.parse_file
          [filename]
          Pipeline.initial_stat_env)
    in
    Diag.return (List.hd progs_no_srcs)
  in
  (* Have two fresh ASTs, since [infer_prog] will mutate the AST with types. *)
  let* prog_no_srcs = load_prog () in
  let* prog_srcs =
    Fun.protect
      (fun () ->
        Mo_config.Flags.typechecker_combine_srcs := true;
        load_prog ())
      ~finally:(fun () -> Mo_config.Flags.typechecker_combine_srcs := false)
  in
  let module Arrange = (val arrange filename) in
  let sexpr_prog_no_srcs = remove_srcs @@ Arrange.prog prog_no_srcs in
  let sexpr_prog_srcs = remove_srcs @@ Arrange.prog prog_srcs in
  if sexpr_prog_no_srcs <> sexpr_prog_srcs then
    failwith
      (Format.sprintf
        "Testing %s failed with an AST mismatch:\nAST 1:\n%sAST 2:\n%s\n" filename
        (Wasm.Sexpr.to_string 80 sexpr_prog_no_srcs)
        (Wasm.Sexpr.to_string 80 sexpr_prog_srcs));
  Diag.return ()

let get_mo_files_from_dir dir =
  let files = Sys.readdir dir in
  List.filter_map
    (fun file ->
      if Filename.extension file = ".mo"
      then Some (Filename.concat dir file)
      else None)
    (Array.to_list files)

let run_compare_typed_asts_tests_on_dir dir =
  let run_files = get_mo_files_from_dir dir in
  List.iter (fun file -> ignore @@ run_compare_typed_asts_test file) run_files

let%test_unit "ASTs in test match with and without combining sources" =
  List.iter
    run_compare_typed_asts_tests_on_dir
    ["run"; "run-drun"; "perf"; "bench"]
