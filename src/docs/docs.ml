open Mo_frontend
open Mo_def
open Source

type doc = { doc_comment : string option; declaration : declaration_doc }

and declaration_doc =
  | Function of function_doc
  | Type of type_doc
  | Unknown of string

and function_doc = {
  name : string;
  typ : Syntax.typ option;
  args : function_arg_doc list;
}

and function_arg_doc = {
  name : string;
  typ : Syntax.typ option;
  doc : string option;
}

and type_doc = { name : string; type_args : string list; typ : Syntax.typ }

and link = {
    location : link_location;
    title : string;
    namespace : namespace
  }

and namespace = Value | Type
and package_name = string
and module_name = string

and link_location =
  | Local of module_name
  | Dependency of (package_name * module_name)


let opt_typ : Syntax.typ option -> string =
  Option.fold ~none:"" ~some:(fun ty ->
      " : " ^ Wasm.Sexpr.to_string 1000 (Arrange.typ ty))

let function_arg : function_arg_doc -> string =
 fun arg -> Printf.sprintf "%s%s" arg.name (opt_typ arg.typ)

let declaration_header : declaration_doc -> string = function
  | Function function_doc ->
      let args = String.concat ", " (List.map function_arg function_doc.args) in
      let typ = opt_typ function_doc.typ in
      Printf.sprintf "Function %s\n========\nfunc %s(%s)%s" function_doc.name
        function_doc.name args typ
  | Type type_doc ->
      Printf.sprintf "Type %s\n========\ntype %s%s = %s" type_doc.name
        type_doc.name
        ( if type_doc.type_args = [] then ""
        else "<" ^ String.concat ", " type_doc.type_args ^ ">" )
        (Wasm.Sexpr.to_string 1000 (Arrange.typ type_doc.typ))
  | Unknown _ -> "Unknown\n========\n"

let render_doc : doc -> string =
 fun { doc_comment; declaration } ->
  declaration_header declaration
  ^ Option.value ~default:"No documentation comment" doc_comment
  ^ "\n"

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let conv_token (tkn, _, _) = tkn

let conv_start (_, start, _) = start

let conv_end (_, _, end_) = end_

let un_prog prog =
  let rec go = function
    | { it = Syntax.ExpD { it = Syntax.ObjE (_, m); _ }; _ } :: _ -> m
    | _ :: tail -> go tail
    | [] -> assert false
  in
  go prog.Source.it

let rec un_varp :
    Syntax.pat -> (string * Syntax.typ option * Source.region) option = function
  | Source.{ it = Syntax.VarP { it = n; at; _ }; _ } -> Some (n, None, at)
  | Source.{ it = Syntax.AnnotP (p, ty); _ } ->
      Option.map (fun (n, _, at) -> (n, Some ty, at)) (un_varp p)
  | Source.{ it = Syntax.OptP p; at; _ } ->
      Option.map (fun (n, ty, _) -> (n, ty, at)) (un_varp p)
  | pat ->
      Printf.printf "UNVARP:\n";
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

let un_func_dec :
    Syntax.dec ->
    ( string
    * Syntax.typ option
    * (string * Syntax.typ option * Source.region) list )
    option = function
  | Source.
      {
        it =
          Syntax.LetD
            ( { it = Syntax.VarP { it = name; _ }; _ },
              {
                it =
                  Syntax.FuncE (_, _, _, { it = Syntax.TupP args; _ }, typ, _, _);
                _;
              } );
        _;
      } ->
      Some (name, typ, List.filter_map un_varp args)
  | _ -> None

let un_typ_dec : Syntax.dec -> (string * Source.region * Syntax.typ) option =
  function
  | Source.{ it = Syntax.TypD ({ it = name; _ }, _, typ); at; _ } ->
      Some (name, at, typ)
  | _ -> None

let string_of_leading : Lexer.trivia_info -> string =
 fun info ->
  String.concat ""
    (List.filter_map
       (function Source_token.Comment s -> Some s | _ -> None)
       info.Lexer.leading_trivia)

let print_leading : Lexer.trivia_info -> unit =
 fun info -> print_endline (string_of_leading info)

let rec extract_args find_trivia = function
  | Source.{ it = Syntax.VarP { it = name; at; _ }; _ } ->
      Some { name; typ = None; doc = Some (string_of_leading (find_trivia at)) }
  | Source.{ it = Syntax.AnnotP (p, ty); at; _ } ->
      Option.map
        (fun x ->
          {
            x with
            typ = Some ty;
            doc = Some (string_of_leading (find_trivia at));
          })
        (extract_args find_trivia p)
  | pat ->
      Wasm.Sexpr.print 80 (Arrange.pat pat);
      None

let extract_func_args find_trivia = function
  | { it = Syntax.ParP arg; _ } -> Option.to_list (extract_args find_trivia arg)
  | { it = Syntax.TupP args; _ } ->
      List.filter_map (extract_args find_trivia) args
  | _ -> []

let rec extract_let_doc (find_trivia : Source.region -> Lexer.trivia_info) :
    Syntax.exp -> string -> Syntax.typ option -> declaration_doc option =
  function
  | Source.{ it = Syntax.FuncE (_, _, _, args, typ, _, _); _ } ->
      fun name _ ->
        let args_doc = extract_func_args find_trivia args in
        Some (Function { name; typ; args = args_doc })
  | Source.{ it = Syntax.AnnotE (e, ty); _ } ->
      fun name _ -> extract_let_doc find_trivia e name (Some ty)
  | _ -> fun name ty -> None

let extract_doc find_trivia = function
  | Source.
      { it = Syntax.LetD ({ it = Syntax.VarP { it = name; _ }; _ }, rhs); _ } ->
      extract_let_doc find_trivia rhs name None
  | Source.{ it = Syntax.TypD (name, ty_args, typ); _ } ->
      Some (Type { name = name.it; type_args = []; typ })
  | unknown ->
      Wasm.Sexpr.print 80 (Arrange.dec unknown);
      None

(* Some (Unknown "Unknown") *)

let extract_docs : Syntax.prog -> Lexer.triv_table -> doc list =
 fun prog trivia_table ->
  let lookup_trivia (line, column) =
    Lexer.PosHashtbl.find_opt trivia_table Lexer.{ line; column }
  in
  let find_trivia (parser_pos : Source.region) : Lexer.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
    |> Option.get
  in
  (* Skip the module header *)
  let module_ = un_prog prog in
  let public_decls =
    List.filter (fun x -> x.it.Syntax.vis.it = Syntax.Public) module_
  in
  let docs =
    List.filter_map
      (fun exp_field ->
        extract_doc find_trivia exp_field.it.Syntax.dec
        |> Option.map (fun decl_doc ->
               {
                 doc_comment =
                   Some (string_of_leading (find_trivia exp_field.at));
                 declaration = decl_doc;
               }))
      public_decls
  in

  docs

let simplistic_docs : string -> unit =
 fun file ->
  Printf.printf "Figuring out docs for %s:\n" file;
  let tokenizer, get_trivia_table =
    Lexer.tokenizer Lexer.NormalWithTrivia (Lexing.from_channel (open_in file))
  in
  let parser =
    MenhirLib.Convert.Simplified.traditional2revised Parser.parse_prog
  in
  let prog = parser tokenizer file in
  let trivia_table = get_trivia_table () in
  let result = extract_docs prog trivia_table in
  (* Wasm.Sexpr.print 80 (Arrange.prog prog); *)
  List.iter (fun doc -> print_endline (render_doc doc)) result

(* let lookup_trivia (l, c) =
 *   Lexer.TrivTable.find_opt Lexer.{ line = l; column = c } trivia_table
 * in
 * let find_trivia (parser_pos : Source.region) : Lexer.trivia_info =
 *   lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
 *   |> Option.get
 * in
 * let un = un_prog prog in
 * List.iter
 *   (fun Source.{ it = Syntax.{ dec; vis; _ }; at; _ } ->
 *     print_endline "";
 *     let info = find_trivia at in
 *     match un_func_dec dec with
 *     | Some (name, typ, args) ->
 *         print_leading info;
 *         Printf.printf "func %s\n" name;
 *         List.iter
 *           (fun (name, ty, pos) ->
 *             Printf.printf "  ";
 *             print_leading (find_trivia pos);
 *             Printf.printf "  arg %s\n" name)
 *           args
 *     | None -> (
 *         match un_typ_dec dec with
 *         | Some (name, pos, typ) -> (
 *             print_leading info;
 *             Printf.printf "type %s\n" name;
 *             match un_obj_typ typ with
 *             | Some fields ->
 *                 List.iter
 *                   (fun (name, pos) ->
 *                     Printf.printf "  ";
 *                     print_leading (find_trivia pos);
 *                     Printf.printf "  field %s\n" name)
 *                   fields
 *             | None -> () )
 *         | None ->
 *             print_leading info;
 *             Wasm.Sexpr.print 80 (Arrange.dec dec) ))
 *   un *)

(* let file = "/home/creek/code/motoko/src/mytest.mo" *)
let file = "/home/creek/code/mo-libs/motoko-base/src/List.mo"

let start () = simplistic_docs file
