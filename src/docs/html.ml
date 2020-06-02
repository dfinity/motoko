open Extract
open Mo_def
open Cow.Html

let rec join_with : t -> t list -> t =
 fun sep -> function
  | [] -> empty
  | [ x ] -> x
  | x :: xs -> x ++ sep ++ join_with sep xs

let space : t = string "\u{00A0}"

let cls_span : string -> string -> t = fun cls s -> span ~cls (string s)

let fn_name : string -> t = cls_span "fnname"

let class_name : string -> t = cls_span "classname"

let keyword : string -> t = cls_span "keyword"

let parameter : string -> t = cls_span "parameter"

let html_type : string -> t = cls_span "type"

let rec string_of_path : Syntax.path -> string =
 fun path ->
  match path.Source.it with
  | Syntax.IdH id -> id.Source.it
  | Syntax.DotH (path, id) -> string_of_path path ^ "." ^ id.Source.it

let html_of_comment : string -> t = Cow.Markdown.of_string

let html_of_mut : Syntax.mut -> t =
 fun mut ->
  match mut.Source.it with
  | Syntax.Var -> keyword "var "
  | Syntax.Const -> string ""

let html_of_func_sort : Syntax.func_sort -> t =
 fun sort ->
  Mo_types.Type.(
    match sort.Source.it with
    | Local -> nil
    | Shared Query -> keyword "shared query "
    | Shared Write -> keyword "shared ")

let html_of_obj_sort : Syntax.obj_sort -> t =
 fun sort ->
  Mo_types.Type.(
    match sort.Source.it with
    | Object -> nil
    | Actor -> keyword "actor "
    | Module -> keyword "module "
    | Memory -> assert false)

let rec html_of_type : Syntax.typ -> t =
 fun typ ->
  match typ.Source.it with
  | Syntax.PathT (path, typs) -> (
      html_type (string_of_path path)
      ++
      match typs with
      | [] -> empty
      | xs ->
          string "<"
          ++ join_with (string ", ") (List.map html_of_type xs)
          ++ string ">" )
  | Syntax.PrimT typ -> html_type typ
  | Syntax.ParT typ -> string "(" ++ html_of_type typ ++ string ")"
  | Syntax.OptT typ ->
      if Common.is_type_atom typ then string "?" ++ html_of_type typ
      else string "?(" ++ html_of_type typ ++ string ")"
  | Syntax.TupT typ_list ->
      string "("
      ++ join_with (string ", ") (List.map html_of_type typ_list)
      ++ string ")"
  | Syntax.VariantT typ_tags ->
      string "{"
      ++ join_with (string "; ")
           (List.map
              (fun typ_tag ->
                string
                  (Printf.sprintf "#%s : "
                     typ_tag.Source.it.Syntax.tag.Source.it)
                ++ html_of_type typ_tag.Source.it.Syntax.typ)
              typ_tags)
      ++ string "}"
  | Syntax.FuncT (func_sort, typ_binders, arg, res) ->
      let ty_args = html_of_typ_binders typ_binders in
      let ty_arg =
        if Common.is_tuple_type arg then html_of_type arg
        else string "(" ++ html_of_type arg ++ string ")"
      in
      html_of_func_sort func_sort
      ++ ty_args
      ++ ty_arg
      ++ string " -> "
      ++ html_of_type res
  | Syntax.ArrayT (mut, ty) ->
      string "[" ++ html_of_mut mut ++ html_of_type ty ++ string "]"
  | Syntax.AsyncT (_scope, typ) -> keyword "async " ++ html_of_type typ
  | Syntax.ObjT (obj_sort, fields) ->
      html_of_obj_sort obj_sort
      ++ string "{ "
      ++ join_with (string "; ") (List.map html_of_typ_field fields)
      ++ string " }"

and html_of_typ_bind : Syntax.typ_bind -> t =
 fun typ_bind ->
  let bound = typ_bind.Source.it.Syntax.bound in
  let bound_html =
    if Syntax.is_any bound then nil else string " <: " ++ html_of_type bound
  in
  html_type typ_bind.Source.it.Syntax.var.Source.it ++ bound_html

and html_of_typ_binders : Syntax.typ_bind list -> t =
 fun typ_binders ->
  match List.filter (fun b -> not (Common.is_scope_bind b)) typ_binders with
  | [] -> []
  | xs ->
      string "<"
      ++ join_with (string ", ") (List.map html_of_typ_bind xs)
      ++ string ">"

and html_of_typ_field : Syntax.typ_field -> t =
 fun field ->
  (* TODO mut might be wrong here *)
  html_of_mut field.Source.it.Syntax.mut
  ++ string (field.Source.it.Syntax.id.Source.it ^ " : ")
  ++ html_of_type field.Source.it.Syntax.typ

let html_of_type_doc : Extract.type_doc -> t =
 fun type_doc ->
  let ty_args = html_of_typ_binders type_doc.type_args in
  match type_doc.typ with
  | DTPlain ty ->
      h4 ~cls:"type-declaration" ~id:("type." ^ type_doc.name)
        ( keyword "type "
        ++ html_type type_doc.name
        ++ ty_args
        ++ string " = "
        ++ html_of_type ty )
  | DTObj (ty, fields) ->
      (* TODO Figure out a layout for showing the documentation on individual
       *  fields *)
      (* let header =
       *   h4 ~cls:"type-declaration" ~id:("type." ^ type_doc.name)
       *     ( keyword "type "
       *     ++ html_type type_doc.name
       *     ++ ty_args
       *     ++ string " = {" )
       * in
       * let html_field = code ~cls:"type-field" in
       * let indent = space ++ space in
       * let br_indent = br empty ++ indent in
       * header
       * ++ list
       *      (List.map
       *         (fun (ty_field, doc) ->
       *           let doc_string =
       *             if doc <> "" then br_indent ++ string doc else []
       *           in
       *           html_field (indent ++ html_of_typ_field ty_field ++ string ";")
       *           ++ doc_string)
       *         fields)
       * ++ br empty
       * ++ string "}" *)
      h4 ~cls:"type-declaration" ~id:("type." ^ type_doc.name)
        ( keyword "type "
        ++ html_type type_doc.name
        ++ ty_args
        ++ string " = "
        ++ html_of_type ty )

let html_of_arg : Extract.function_arg_doc -> t =
 fun arg ->
  parameter arg.name
  ++ Option.fold ~none:empty
       ~some:(fun arg -> string " : " ++ html_of_type arg)
       arg.typ

let rec html_of_declaration : Extract.declaration_doc -> t = function
  | Function function_doc ->
      let is_multiline = List.length function_doc.args > 2 in
      let br' = if is_multiline then br empty else empty in
      let br_indent =
        if is_multiline then br empty ++ space ++ space else empty
      in
      let ty_args = html_of_typ_binders function_doc.type_args in
      (* TODO: Figure out a layout to show documentation for individual
       *  arguments *)
      let args =
        join_with
          (string ", " ++ br_indent)
          (List.map html_of_arg function_doc.args)
      in
      let return_typ =
        Option.fold ~none:[]
          ~some:(fun typ -> string " : " ++ html_of_type typ)
          function_doc.typ
      in
      h4 ~cls:"function"
        ~id:("value." ^ function_doc.name)
        (code
           ( keyword "public func "
           ++ fn_name function_doc.name
           ++ ty_args
           ++ string "("
           ++ br_indent
           ++ args
           ++ br'
           ++ string ")"
           ++ return_typ ))
  | Class class_doc ->
      let ty_args = html_of_typ_binders class_doc.type_args in
      h4 ~cls:"class-declaration"
        ~id:("class." ^ class_doc.name)
        (keyword "class " ++ class_name class_doc.name ++ ty_args)
      ++ list (List.map html_of_doc class_doc.fields)
  | Type type_doc -> html_of_type_doc type_doc
  | Value value_doc ->
      h4 ~cls:"value-declaration"
        ~id:("value." ^ value_doc.name)
        (code
           ( keyword "public let "
           ++ fn_name value_doc.name
           ++ string " : "
           ++ Option.fold ~none:empty ~some:html_of_type value_doc.typ ))
  | Unknown s -> code (string "Unknown: " ++ string s)

and html_of_doc : Extract.doc -> t =
 fun { doc_comment; declaration } ->
  div ~cls:"declaration"
    ( html_of_declaration declaration
    ++ p (html_of_comment (doc_comment |> Option.value ~default:"")) )

let html_of_docs : string -> doc list -> Cow.Html.t =
 fun module_docs docs ->
  let header =
    head ~attrs:[ ("title", "Doc") ]
      ( meta ~charset:"UTF-8" []
      ++ link ~rel:"stylesheet" ~href:(Uri.of_string "styles.css") empty )
  in
  let bdy =
    body
      ( div ~cls:"module-docs" (html_of_comment module_docs)
      ++ list (List.map html_of_doc docs) )
  in
  html (header ++ bdy)

let render_docs : string -> doc list -> string =
 fun module_docs docs ->
  Format.asprintf "%s" (Cow.Html.to_string (html_of_docs module_docs docs))
