open Mo_frontend
open Mo_def

let file = "doctest.mo"

let conv_token (tkn, _) = tkn

let conv_start (_, ann) = fst ann.Source_token.range

let conv_end (_, ann) = snd ann.Source_token.range

let un_module = function
  | Source.
      { it = [ { it = Syntax.ExpD { it = Syntax.ObjE (_, m); _ }; _ } ]; _ } ->
      m
  | _ -> assert false

let rec un_varp : Syntax.pat -> (string * Source.region) option = function
  | Source.{ it = Syntax.VarP { it = n; at; _ }; _ } -> Some (n, at)
  | Source.{ it = Syntax.AnnotP(p,_); _ } -> un_varp p
  | pat ->
      Wasm.Sexpr.print 80 (Arrange.pat pat);
      None

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

let find_trivia : Source.region -> Lexer_conv.trivia_info =
 fun parser_pos ->
  let pos =
    {
      Lexer_conv.line = Source.(parser_pos.left.line);
      Lexer_conv.column = Source.(parser_pos.left.column);
    }
  in
  (* Lexer_conv.TrivTable.iter
   *   (fun k v -> Printf.printf "%d:%d\n" k.line k.column)
   *   !Lexer_conv.trivia_table;
   * Printf.printf "%d:%d\n" Source.(f.at.left.line) Source.(f.at.left.column); *)
  Lexer_conv.TrivTable.find pos !Lexer_conv.trivia_table

let print_leading : Lexer_conv.trivia_info -> unit =
 fun info ->
  List.iter
    (* (fun t -> Printf.printf "%s\n" (Source_token.string_of_trivia_lf t)) *)
      (function Source_token.Comment s -> Printf.printf "%s\n" s | _ -> ())
    info.Lexer_conv.leading_trivia

let simplistic_docs : Lexing.lexbuf -> unit =
 fun lexbuf ->
  let tknzr = Lexer_conv.tokenizer Lexer_new.Normal lexbuf in
  let parser =
    MenhirLib.Convert.traditional2revised conv_token conv_start conv_end
      Parser.parse_prog
  in
  let prog = parser tknzr file in
  let un = un_module prog in
  List.iter
    (fun Source.{ it = Syntax.{ dec; vis }; at; _ } ->
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
      | None ->
          print_leading info;
          Wasm.Sexpr.print 80 (Arrange.dec dec))
    un

let () = simplistic_docs (Lexing.from_channel (open_in file))
