open Mo_frontend
open Mo_def

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

let print_leading : Lexer_conv.trivia_info -> unit =
 fun info ->
  List.iter
    (* (fun t -> Printf.printf "%s\n" (Source_token.string_of_trivia_lf t)) *)
      (function Source_token.Comment s -> Printf.printf "%s\n" s | _ -> ())
    info.Lexer_conv.leading_trivia

let simplistic_docs : Lexing.lexbuf -> unit =
 fun lexbuf ->
  let lookup_trivia, tknzr = Lexer_conv.tokenizer Lexer_new.Normal lexbuf in
  let find_trivia (parser_pos : Source.region): Lexer_conv.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column) |> Option.get in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
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
