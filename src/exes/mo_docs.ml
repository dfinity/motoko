open Mo_frontend
open Mo_def

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let file = "doctest.mo"

let conv_token (tkn, _, _) = tkn

let conv_start (_, start, _) = start

let conv_end (_, _, end_) = end_

(* TODO Allow imports *)
let un_module = function
  | Source.
      { it = [ { it = Syntax.ExpD { it = Syntax.ObjE (_, m); _ }; _ } ]; _ } ->
      m
  | _ -> assert false

let rec un_varp : Syntax.pat -> (string * Source.region) option = function
  | Source.{ it = Syntax.VarP { it = n; at; _ }; _ } -> Some (n, at)
  | Source.{ it = Syntax.AnnotP (p, _); _ } -> un_varp p
  | Source.{ it = Syntax.OptP p; at; _ } ->
      Option.map (fun (n, _) -> (n, at)) (un_varp p)
  | pat ->
      Wasm.Sexpr.print 80 (Arrange.pat pat);
      None

let un_obj_typ : Syntax.typ -> (string * Source.region) list option = function
  | Source.{ it = Syntax.ObjT (_, fields); _ } ->
      Some
        (List.map
           (fun Source.{ it = (tf : Syntax.typ_field'); at; _ } ->
             (tf.Syntax.id.Source.it, at))
           fields)
  | _ -> None

let un_func_dec : Syntax.dec -> (string * (string * Source.region) list) option
    = function
  | Source.
      {
        it =
          Syntax.LetD
            ( { it = Syntax.VarP { it = name; _ }; _ },
              {
                it =
                  Syntax.FuncE (_, _, _, { it = Syntax.TupP args; _ }, _, _, _);
                _;
              } );
        _;
      } ->
      Some (name, List.filter_map un_varp args)
  | _ -> None

let un_typ_dec : Syntax.dec -> (string * Source.region * Syntax.typ) option =
  function
  | Source.{ it = Syntax.TypD ({ it = name; _ }, _, typ); at; _ } ->
      Some (name, at, typ)
  | _ -> None

let print_leading : Lexer_conv.trivia_info -> unit =
 fun info ->
  List.iter
    (* (fun t -> Printf.printf "%s\n" (Source_token.string_of_trivia_lf t)) *)
      (function Source_token.Comment s -> Printf.printf "%s\n" s | _ -> ())
    info.Lexer_conv.leading_trivia

let simplistic_docs : Lexing.lexbuf -> unit =
 fun lexbuf ->
  let lookup_trivia, tknzr = Lexer_conv.tokenizer Lexer_new.Normal lexbuf in
  let find_trivia (parser_pos : Source.region) : Lexer_conv.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
    |> Option.get
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
  in
  let prog = parser tknzr file in
  let un = un_module prog in
  List.iter
    (fun Source.{ it = Syntax.{ dec; vis }; at; _ } ->
      print_endline "";
      let info = find_trivia at in
      match un_func_dec dec with
      | Some (name, args) ->
          print_leading info;
          Printf.printf "func %s\n" name;
          List.iter
            (fun (name, pos) ->
              Printf.printf "  ";
              print_leading (find_trivia pos);
              Printf.printf "  arg %s\n" name)
            args
      | None -> (
          match un_typ_dec dec with
          | Some (name, pos, typ) -> (
              print_leading info;
              Printf.printf "type %s\n" name;
              match un_obj_typ typ with
              | Some fields ->
                  List.iter
                    (fun (name, pos) ->
                      Printf.printf "  ";
                      print_leading (find_trivia pos);
                      Printf.printf "  field %s\n" name)
                    fields
              | None -> () )
          | None ->
              print_leading info;
              Wasm.Sexpr.print 80 (Arrange.dec dec) ))
    un

let run_docs () = simplistic_docs (Lexing.from_channel (open_in file))

let string_of_lex_pos pos =
  Lexing.(Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol))

let compare_tokens =
  List.iter2 (fun (t1, s1, e1) (t2, s2, e2) ->
      if t1 = t2 then ()
      else
        Printf.printf "MISMATCH\n%s %s-%s\n%s %s-%s\n"
          (Source_token.string_of_parser_token t1)
          (string_of_lex_pos s1) (string_of_lex_pos e1)
          (Source_token.string_of_parser_token t2)
          (string_of_lex_pos s2) (string_of_lex_pos e2))

let compare_lexers_priv mk_input =
  let lexbuf1 = mk_input () in
  let tknzr1 () =
    let t = Lexer.token Lexer.Privileged lexbuf1 in
    let start = Lexing.lexeme_start_p lexbuf1 in
    let end_ = Lexing.lexeme_end_p lexbuf1 in
    (t, start, end_)
  in
  let lexbuf2 = mk_input () in
  let _, tknzr2 = Lexer_conv.tokenizer Lexer_new.Privileged lexbuf2 in
  let lex_all lexer =
    let rec go acc =
      match lexer () with
      | (Parser.EOF, _, _) as t -> List.rev (t :: acc)
      | t -> go (t :: acc)
    in
    go []
  in
  let tokens1 = lex_all tknzr1 in
  let tokens2 = lex_all tknzr2 in

  compare_tokens tokens1 tokens2

let compare_lexers mk_input =
  let lexbuf1 = mk_input () in
  let tknzr1 () =
    let t = Lexer.token Lexer.Normal lexbuf1 in
    let start = Lexing.lexeme_start_p lexbuf1 in
    let end_ = Lexing.lexeme_end_p lexbuf1 in
    (t, start, end_)
  in
  let lexbuf2 = mk_input () in
  let _, tknzr2 = Lexer_conv.tokenizer Lexer_new.Normal lexbuf2 in
  let lex_all lexer =
    let rec go acc =
      match lexer () with
      | (Parser.EOF, _, _) as t -> List.rev (t :: acc)
      | t -> go (t :: acc)
    in
    go []
  in
  let tokens1 = lex_all tknzr1 in
  let tokens2 = lex_all tknzr2 in

  compare_tokens tokens1 tokens2

let compare_lexers_file file =
  compare_lexers (fun () -> Lexing.from_channel (open_in file))

let compare_lexers_string s = compare_lexers (fun () -> Lexing.from_string s)

let list_files_recursively dir =
  let rec loop result = function
    | f :: fs when Sys.is_directory f ->
        Sys.readdir f |> Array.to_list
        |> List.map (Filename.concat f)
        |> List.append fs |> loop result
    | f :: fs -> loop (f :: result) fs
    | [] -> result
  in
  loop [] [ dir ]

let run_lex_diff () =
  let files =
    list_files_recursively "../test/run"
    |> List.filter (fun f -> Filename.extension f = ".mo")
  in
  let test_cases =
    [
      {|#bar;
#foo(#bar);
[#Monday, #Tuesday, #Wednesday, #Thursday, #Friday, #Saturday, #Sunday];
|};
      "+127\n  -127;\n";
      "";
    ]
  in

  List.iter
    (fun file ->
      Printf.printf "\nChecking %s\n" file;
      compare_lexers_file file)
    files;
  (* let repl_files =
   *   list_files_recursively "../test/repl"
   *   |> List.filter (fun f -> Filename.extension f = ".sh") in
   * List.iter
   *   (fun file ->
   *     Printf.printf "\nChecking %s\n" file;
   *     compare_lexers_file file)
   *   repl_files; *)
  List.iter
    (fun s ->
      Printf.printf "\nTestcase %s\n" (String.trim s);
      compare_lexers_string s)
    test_cases;

  compare_lexers_priv (fun () -> Lexing.from_string Prelude.prelude);
  compare_lexers_priv (fun () -> Lexing.from_string Prelude.prim_module)

(* let () = run_docs () *)

let () = run_lex_diff ()
