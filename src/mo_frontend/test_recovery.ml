(** Maintenance note:
    Update of the expected values could be done via [dune runtest --auto-promote].
*)

let parse_from_lexbuf lexbuf : Mo_def.Syntax.prog Diag.result =
  let open Mo_frontend in
  let open Diag.Syntax in
  let name = "test.mo" in
  let lexer_mode = Lexer.mode in
  let lexer, triv_table = Lexer.tokenizer lexer_mode lexbuf in
  let () = Parser_lib.triv_table := triv_table in
  let start =  Parser.Incremental.parse_prog lexbuf.Lexing.lex_start_p in
  let error_details = 0 in
  let* mk_syntax = Mo_frontend.Parsing.parse ~recovery:true lexer_mode error_details start lexer lexbuf
  in Diag.return @@ mk_syntax name

let parse_from_string s : Mo_def.Syntax.prog Diag.result =
  let lexbuf = Lexing.from_string s in
  parse_from_lexbuf lexbuf

let show  (r: Mo_def.Syntax.prog Diag.result) : String.t =
  let show_msgs msgs =
    String.concat "\n" (List.map Diag.string_of_message msgs) in
  match r with
  | Ok (prog, msgs) ->
    "Ok: " ^ Wasm.Sexpr.to_string 80 (Mo_def.Arrange.prog prog)
    ^ "\n with errors:\n" ^ show_msgs msgs
  | Error msgs -> "Errors:\n" ^ show_msgs msgs


let _parse_test input (expected : string) =
  let actual = parse_from_string input in
  if String.equal (show actual) expected then
    true
  else
    (Printf.printf "\nExpected:\n  %s\nbut got:\n  %s\n" (expected) (show actual); false)

let%expect_test "test1" =
  let s = "actor {
  let x : Int = 1

  let y : Int = 2;

  let z : Int = 3

  let t : Int = 4
}" in
  Printf.printf "%s" @@ show (parse_from_string s);
  [%expect {|
    Ok: (Prog
      (ExpD
        (AwaitE
          (AsyncE
            _
            ($@anon-async-1.1 (PrimT Any))
            (ObjBlockE
              _
              Actor
              _
              (DecField
                (LetD (VarP x) (AnnotE (LitE (PreLit 1 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
              (DecField
                (LetD (VarP y) (AnnotE (LitE (PreLit 2 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
              (DecField
                (LetD (VarP z) (AnnotE (LitE (PreLit 3 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
              (DecField
                (LetD (VarP t) (AnnotE (LitE (PreLit 4 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
            )
          )
        )
      )
    )

     with errors:
    (unknown location): syntax error [M0001], unexpected token 'let'

    (unknown location): syntax error [M0001], unexpected token 'let' |}]

let%expect_test "test2" =
  let s = "actor {
  let x : Int = 1 +

  let y : Int = 2;
}" in
  Printf.printf "%s" @@ show (parse_from_string s);
  [%expect {|
    Ok: (Prog
      (ExpD
        (AwaitE
          (AsyncE
            _
            ($@anon-async-1.1 (PrimT Any))
            (ObjBlockE
              _
              Actor
              _
              (DecField
                (LetD
                  (VarP x)
                  (AnnotE
                    (BinE ??? (LitE (PreLit 1 Nat)) AddOp (LoopE (BlockE)))
                    (PathT (IdH Int))
                  )
                )
                Private
                Flexible
              )
              (DecField
                (LetD (VarP y) (AnnotE (LitE (PreLit 2 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
            )
          )
        )
      )
    )

     with errors:
    (unknown location): syntax error [M0001], unexpected token 'let' |}]

let%expect_test "test3" =
  let s = "actor {
  private func(a Int) {
    return
  }

  private func bar(y: Int) : Int { return 2; }

  let y : Int = 2;
}" in
  Printf.printf "%s" @@ show (parse_from_string s);
  [%expect {|
    Ok: (Prog
      (ExpD
        (AwaitE
          (AsyncE
            _
            ($@anon-async-1.1 (PrimT Any))
            (ObjBlockE
              _
              Actor
              _
              (DecField
                (ExpD
                  (FuncE
                    ???
                    Local
                    @anon-func-2.11
                    (TupP (VarP a) (VarP Int))
                    _

                    (BlockE (ExpD (RetE (TupE))))
                  )
                )
                Private
                (Flexible)
              )
              (DecField
                (LetD
                  (VarP bar)
                  (FuncE
                    ???
                    Local Stable
                    bar
                    (ParP (AnnotP (VarP y) (PathT (IdH Int))))
                    (PathT (IdH Int))

                    (BlockE (ExpD (RetE (LitE (PreLit 2 Nat)))))
                  )
                )
                Private
                Flexible
              )
              (DecField
                (LetD (VarP y) (AnnotE (LitE (PreLit 2 Nat)) (PathT (IdH Int))))
                Private
                Flexible
              )
            )
          )
        )
      )
    )

     with errors:
    (unknown location): syntax error [M0001], unexpected token 'Int'

    (unknown location): syntax error [M0001], unexpected token 'private'

    (unknown location): syntax error [M0001], unexpected token 'let' |}] 


let%expect_test "test4" =
  let s = "import { print } \"mo:base/Debug\";

actor Main {

    let x = 1

    public query test() : async Nat {
        123
    }
};" in
  Printf.printf "%s" @@ show (parse_from_string s);
  [%expect{|
    Ok: (Prog
      (LetD (ObjP (print (VarP print))) (ImportE mo:base/Debug))
      (LetD
        (VarP Main)
        (AwaitE
          (AsyncE
            _
            ($@anon-async-3.1 (PrimT Any))
            (ObjBlockE
              _
              Actor
              Main
              (DecField (LetD (VarP x) (LitE (PreLit 1 Nat))) Private Flexible)
              (DecField
                (ExpD
                  (FuncE
                    ???
                    (Query (VarP test))
                    @anon-func-7.12
                    ($ (PrimT Any))
                    (TupP)
                    (AsyncT (PathT (IdH $)) (PathT (IdH Nat)))

                    (AsyncE
                      _
                      ($@anon-func-7.12 (PrimT Any))
                      (BlockE (ExpD (LitE (PreLit 123 Nat))))
                    )
                  )
                )
                Public
                (Flexible)
              )
            )
          )
        )
      )
    )

     with errors:
    (unknown location): syntax error [M0001], unexpected token 'public'

    (unknown location): syntax error [M0001], unexpected token '(' |}]

let%expect_test "test5" =
  let s = "module {

    let x =

    public query test() : async Nat {
        123
    }
};" in
  Printf.printf "%s" @@ show (parse_from_string s);
  [%expect{|
    Ok: (Prog
      (ExpD
        (ObjBlockE
          _
          Module
          _
          (DecField (LetD (VarP x) (LoopE (BlockE))) Private (Flexible))
          (DecField
            (ExpD
              (FuncE
                ???
                (Query (VarP test))
                @anon-func-5.12
                ($ (PrimT Any))
                (TupP)
                (AsyncT (PathT (IdH $)) (PathT (IdH Nat)))

                (AsyncE
                  _
                  ($@anon-func-5.12 (PrimT Any))
                  (BlockE (ExpD (LitE (PreLit 123 Nat))))
                )
              )
            )
            Public
            (Flexible)
          )
        )
      )
    )

     with errors:
    (unknown location): syntax error [M0001], unexpected token 'public'

    (unknown location): syntax error [M0001], unexpected token '(' |}]
