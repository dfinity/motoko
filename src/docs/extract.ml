open Mo_frontend
open Mo_def
open Source

type doc = { doc_comment : string option; declaration : declaration_doc }

and declaration_doc =
  | Function of function_doc
  | Type of type_doc
  | Class of class_doc
  | Unknown of string

and function_doc = {
  name : string;
  typ : Syntax.typ option;
  type_args : Syntax.typ_bind list;
  args : function_arg_doc list;
}

and function_arg_doc = {
  name : string;
  typ : Syntax.typ option;
  doc : string option;
}

and type_doc = { name : string; type_args : string list; typ : Syntax.typ }

and class_doc = {
  name : string;
  type_args : Syntax.typ_bind list;
  fields : doc list;
}

and link = { location : link_location; title : string; namespace : namespace }

and namespace = Value | Type

and package_name = string

and module_name = string

and link_location =
  | Local of module_name
  | Dependency of (package_name * module_name)

let un_prog prog =
  let rec go acc = function
    | { it = Syntax.ExpD { it = Syntax.ObjE (_, m); _ }; _ } :: _ -> (acc, m)
    | {
        it =
          Syntax.LetD
            ({ it = Syntax.VarP v; _ }, { it = Syntax.ImportE (path, _); _ });
        _;
      }
      :: tail ->
        (* TODO Gotta resolve this path so it makes sense somehow *)
        go ((v.it, path) :: acc) tail
    | _ :: tail -> go acc tail
    | [] -> assert false
  in
  go [] prog.Source.it

let string_of_leading : Lexer.trivia_info -> string =
 fun info ->
  String.concat "\n"
    (List.filter_map
       (function Source_token.Comment s -> Some s | _ -> None)
       info.Lexer.leading_trivia)

let _print_leading : Lexer.trivia_info -> unit =
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
  | Source.{ it = Syntax.FuncE (_, _, type_args, args, typ, _, _); _ } ->
      fun name _ ->
        let args_doc = extract_func_args find_trivia args in
        Some (Function { name; typ; type_args; args = args_doc })
  | Source.{ it = Syntax.AnnotE (e, ty); _ } ->
      fun name _ -> extract_let_doc find_trivia e name (Some ty)
  | _ -> fun name ty -> None

let rec extract_doc find_trivia = function
  | Source.
      { it = Syntax.LetD ({ it = Syntax.VarP { it = name; _ }; _ }, rhs); _ } ->
      extract_let_doc find_trivia rhs name None
  | Source.{ it = Syntax.TypD (name, ty_args, typ); _ } ->
      Some (Type { name = name.it; type_args = []; typ })
  | Source.
      { it = Syntax.ClassD (name, type_args, _ctor_pat, _, _, _, fields); _ } ->
      Some
        (Class
           {
             name = name.it;
             type_args;
             fields = List.filter_map (extract_exp_field find_trivia) fields;
           })
  | unknown ->
      Wasm.Sexpr.print 80 (Arrange.dec unknown);
      None

and extract_exp_field find_trivia exp_field =
  if exp_field.it.Syntax.vis.it <> Syntax.Public then None
  else
    extract_doc find_trivia exp_field.it.Syntax.dec
    |> Option.map (fun decl_doc ->
           {
             doc_comment = Some (string_of_leading (find_trivia exp_field.at));
             declaration = decl_doc;
           })

(* Some (Unknown "Unknown") *)

type imports = (string * string) list

let extract_docs : Syntax.prog -> Lexer.triv_table -> imports * doc list =
 fun prog trivia_table ->
  let lookup_trivia (line, column) =
    Lexer.PosHashtbl.find_opt trivia_table Lexer.{ line; column }
  in
  let find_trivia (parser_pos : Source.region) : Lexer.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
    |> Option.get
  in
  (* Skip the module header *)
  let imports, decls = un_prog prog in
  let docs = List.filter_map (extract_exp_field find_trivia) decls in
  (imports, docs)
