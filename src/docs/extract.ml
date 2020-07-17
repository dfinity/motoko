open Mo_frontend
open Mo_def
open Source

type doc = { doc_comment : string option; declaration : declaration_doc }

and declaration_doc =
  | Function of function_doc
  | Value of value_doc
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

and value_doc = { name : string; typ : Syntax.typ option }

and type_doc = {
  name : string;
  type_args : Syntax.typ_bind list;
  typ : doc_type;
}

and doc_type =
  | DTPlain of Syntax.typ
  (* One level unwrapping of an object type with documentation on its fields *)
  | DTObj of Syntax.typ * (Syntax.typ_field * string) list

(* TODO We'll also want to unwrap variants here *)
and class_doc = {
  name : string;
  type_args : Syntax.typ_bind list;
  constructor : function_arg_doc list;
  fields : doc list;
  sort : Syntax.obj_sort;
}

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
       (function
         | Source_token.Comment s -> (
             match Lib.String.chop_prefix "///" s with
             | Some "" -> Some ""
             | Some line_comment ->
                 (* We expect a documentation line comment to start with a space
                  *  (which we remove here) *)
                 Lib.String.chop_prefix " " line_comment
             | None ->
                 Option.bind
                   (Lib.String.chop_prefix "/**" s)
                   (Lib.String.chop_suffix "*/")
                 |> Option.map String.trim )
         | _ -> None)
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
  | Source.{ it = Syntax.WildP; _ } -> None
  | pat ->
      Wasm.Sexpr.print 80 (Arrange.pat pat);
      None

let extract_func_args find_trivia = function
  | { it = Syntax.ParP arg; _ } -> Option.to_list (extract_args find_trivia arg)
  | { it = Syntax.TupP args; _ } ->
      List.filter_map (extract_args find_trivia) args
  | _ -> []

let extract_let_doc (find_trivia : Source.region -> Lexer.trivia_info) :
    Syntax.exp -> string -> declaration_doc = function
  | Source.{ it = Syntax.FuncE (_, _, type_args, args, typ, _, _); _ } ->
      fun name ->
        let args_doc = extract_func_args find_trivia args in
        Function { name; typ; type_args; args = args_doc }
  | Source.{ it = Syntax.AnnotE (e, ty); _ } ->
      fun name -> Value { name; typ = Some ty }
  | _ -> fun name -> Value { name; typ = None }

let extract_obj_field_doc find_trivia :
    Syntax.typ_field -> Syntax.typ_field * string =
 fun ({ at; _ } as tf) -> (tf, string_of_leading (find_trivia at))

let rec extract_doc find_trivia = function
  | Source.
      { it = Syntax.LetD ({ it = Syntax.VarP { it = name; _ }; _ }, rhs); _ } ->
      Some (extract_let_doc find_trivia rhs name)
  | Source.{ it = Syntax.TypD (name, ty_args, typ); _ } ->
      let doc_typ =
        match typ.it with
        | Syntax.ObjT (_, fields) ->
            let doc_fields =
              List.map (extract_obj_field_doc find_trivia) fields
            in
            (* TODO Only unwrap the ObjT if at least one field is documented *)
            DTObj (typ, doc_fields)
        | _ -> DTPlain typ
      in
      Some (Type { name = name.it; type_args = ty_args; typ = doc_typ })
  | Source.
      {
        it = Syntax.ClassD (name, type_args, ctor, _, obj_sort_pat, _, fields);
        _;
      } ->
      Some
        (Class
           {
             name = name.it;
             type_args;
             constructor = extract_func_args find_trivia ctor;
             fields = List.filter_map (extract_exp_field find_trivia) fields;
             sort = { obj_sort_pat with it = Syntax.obj_sort obj_sort_pat };
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

let extract_docs :
    Syntax.prog -> Lexer.triv_table -> string * imports * doc list =
 fun prog trivia_table ->
  let lookup_trivia (line, column) =
    Lexer.PosHashtbl.find_opt trivia_table Lexer.{ line; column }
  in
  let find_trivia (parser_pos : Source.region) : Lexer.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
    |> Option.get
  in
  let module_docs = find_trivia prog.at in
  (* Skip the module header *)
  let imports, decls = un_prog prog in
  let docs = List.filter_map (extract_exp_field find_trivia) decls in
  (string_of_leading module_docs, imports, docs)
