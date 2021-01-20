open Extract
open Mo_def
open Cow.Html
open Common
open Ic

type env = { imports : (string * string) list; local_types : string list }

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

let rec split_path : Syntax.path -> string list * string =
 fun path ->
  match path.Source.it with
  | Syntax.IdH id -> ([], id.Source.it)
  | Syntax.DotH (path, id) ->
      let xs, x = split_path path in
      (List.append xs [ x ], id.Source.it)

let lookup_import : env -> string -> string option =
 fun { imports; _ } search ->
  List.find_map
    (fun (qualifier, url) -> if qualifier = search then Some url else None)
    imports

let html_of_path : env -> Syntax.path -> t =
 fun env path ->
  let link =
    match split_path path with
    | [], name ->
        if List.exists (fun x -> x = name) env.local_types then
          Some ("#type." ^ name)
        else None
    | qualifiers, name -> (
        match lookup_import env (String.concat "." qualifiers) with
        | Some url -> (
            match Url.parse url with
            | Ok (Url.Relative path) -> Some (path ^ ".html#type." ^ name)
            | _ -> None )
        | None -> None )
  in
  match link with
  | Some link -> a ~href:(Uri.of_string link) (html_type (string_of_path path))
  | None -> html_type (string_of_path path)

let html_of_comment : string -> t = function
  | "" -> empty
  | s -> Cow.Markdown.of_string s

let html_of_mut : Syntax.mut -> t =
 fun mut ->
  match mut.Source.it with
  | Syntax.Var -> keyword "var "
  | Syntax.Const -> string ""

let html_of_func_sort : Syntax.func_sort -> t =
 fun sort ->
  Mo_types.Type.(
    match sort.Source.it with
    | Local -> empty
    | Shared Query -> keyword "shared query "
    | Shared Write -> keyword "shared ")

let html_of_obj_sort : Syntax.obj_sort -> t =
 fun sort ->
  Mo_types.Type.(
    match sort.Source.it with
    | Object -> empty
    | Actor -> keyword "actor "
    | Module -> keyword "module "
    | Memory -> keyword "memory ")

let rec html_of_type : env -> Syntax.typ -> t =
 fun env typ ->
  match typ.Source.it with
  | Syntax.PathT (path, typs) -> (
      html_of_path env path
      ++
      match typs with
      | [] -> empty
      | xs ->
          string "<"
          ++ join_with (string ", ") (List.map (html_of_type env) xs)
          ++ string ">" )
  | Syntax.PrimT typ -> html_type typ
  | Syntax.ParT typ -> string "(" ++ html_of_type env typ ++ string ")"
  | Syntax.NamedT (id, t) ->
      string "(" ++ html_of_typ_item env (Some id, t) ++ string ")"
  | Syntax.OptT typ -> string "?" ++ html_of_type env typ
  | Syntax.TupT typ_list ->
      string "("
      ++ join_with (string ", ") (List.map (html_of_typ_item env) typ_list)
      ++ string ")"
  | Syntax.VariantT typ_tags ->
      string "{"
      ++ join_with (string "; ") (List.map (html_of_typ_tag env) typ_tags)
      ++ string "}"
  | Syntax.FuncT (func_sort, typ_binders, arg, res) ->
      let ty_args = html_of_typ_binders env typ_binders in
      let ty_arg = html_of_type env arg in
      html_of_func_sort func_sort
      ++ ty_args
      ++ ty_arg
      ++ string " -> "
      ++ html_of_type env res
  | Syntax.ArrayT (mut, ty) ->
      string "[" ++ html_of_mut mut ++ html_of_type env ty ++ string "]"
  | Syntax.AsyncT (_scope, typ) -> keyword "async " ++ html_of_type env typ
  | Syntax.ObjT (obj_sort, fields) ->
      html_of_obj_sort obj_sort
      ++ string "{ "
      ++ join_with (string "; ") (List.map (html_of_typ_field env) fields)
      ++ string " }"

and html_of_typ_tag : env -> Syntax.typ_tag -> t =
 fun env typ_tag ->
  string (Printf.sprintf "#%s" typ_tag.Source.it.Syntax.tag.Source.it)
  ++
  match typ_tag.Source.it.Syntax.typ.Source.it with
  | Syntax.TupT [] -> nil
  | _ -> string " : " ++ html_of_type env typ_tag.Source.it.Syntax.typ

and html_of_typ_bind : env -> Syntax.typ_bind -> t =
 fun env typ_bind ->
  let bound = typ_bind.Source.it.Syntax.bound in
  let bound_html =
    if Syntax.is_any bound then empty
    else string " <: " ++ html_of_type env bound
  in
  html_type typ_bind.Source.it.Syntax.var.Source.it ++ bound_html

and html_of_typ_binders : env -> Syntax.typ_bind list -> t =
 fun env typ_binders ->
  match List.filter (fun b -> not (is_scope_bind b)) typ_binders with
  | [] -> []
  | xs ->
      string "<"
      ++ join_with (string ", ") (List.map (html_of_typ_bind env) xs)
      ++ string ">"

and html_of_typ_field : env -> Syntax.typ_field -> t =
 fun env field ->
  (* TODO mut might be wrong here *)
  html_of_mut field.Source.it.Syntax.mut
  ++ string (field.Source.it.Syntax.id.Source.it ^ " : ")
  ++ html_of_type env field.Source.it.Syntax.typ

and html_of_typ_item : env -> Syntax.typ_item -> t =
 fun env (oid, t) ->
  Option.fold ~none:empty
    ~some:(fun id -> parameter id.Source.it ++ string " : ")
    oid
  ++ html_of_type env t

let html_of_type_doc : env -> Extract.type_doc -> t =
 fun env type_doc ->
  let ty_args = html_of_typ_binders env type_doc.type_args in
  match type_doc.typ with
  | DTPlain ty ->
      h4 ~cls:"type-declaration" ~id:("type." ^ type_doc.name)
        ( keyword "type "
        ++ html_type type_doc.name
        ++ ty_args
        ++ string " = "
        ++ html_of_type env ty )
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
       * let br_indent = br ++ indent in
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
       * ++ br
       * ++ string "}" *)
      h4 ~cls:"type-declaration" ~id:("type." ^ type_doc.name)
        ( keyword "type "
        ++ html_type type_doc.name
        ++ ty_args
        ++ string " = "
        ++ html_of_type env ty )

let html_of_arg : env -> Extract.function_arg_doc -> t =
 fun env arg ->
  parameter arg.name
  ++ Option.fold ~none:empty
       ~some:(fun arg -> string " : " ++ html_of_type env arg)
       arg.typ

let rec html_of_declaration : env -> Extract.declaration_doc -> t =
 fun env dec ->
  match dec with
  | Function function_doc ->
      let is_multiline = List.length function_doc.args > 2 in
      let br' = if is_multiline then br else empty in
      let br_indent = if is_multiline then br ++ space ++ space else empty in
      let ty_args = html_of_typ_binders env function_doc.type_args in
      (* TODO: Figure out a layout to show documentation for individual
       *  arguments *)
      let args =
        join_with
          (string ", " ++ br_indent)
          (List.map (html_of_arg env) function_doc.args)
      in
      let return_typ =
        Option.fold ~none:[]
          ~some:(fun typ -> string " : " ++ html_of_type env typ)
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
      let is_multiline = List.length class_doc.constructor > 2 in
      let br' = if is_multiline then br else empty in
      let br_indent = if is_multiline then br ++ space ++ space else empty in
      let ty_args = html_of_typ_binders env class_doc.type_args in
      let args =
        join_with
          (string ", " ++ br_indent)
          (List.map (html_of_arg env) class_doc.constructor)
      in
      h4 ~cls:"class-declaration"
        ~id:("class." ^ class_doc.name)
        ( html_of_obj_sort class_doc.sort
        ++ keyword "class "
        ++ class_name class_doc.name
        ++ ty_args
        ++ string "("
        ++ br_indent
        ++ args
        ++ br'
        ++ string ")" )
      ++ list (List.map (html_of_doc env) class_doc.fields)
  | Type type_doc -> html_of_type_doc env type_doc
  | Value value_doc ->
      h4 ~cls:"value-declaration"
        ~id:("value." ^ value_doc.name)
        (code
           ( keyword "public let "
           ++ fn_name value_doc.name
           ++ string " : "
           ++ Option.fold ~none:empty ~some:(html_of_type env) value_doc.typ ))
  | Unknown s -> code (string "Unknown: " ++ string s)

and html_of_doc : env -> Extract.doc -> t =
 fun env { doc_comment; declaration } ->
  div ~cls:"declaration"
    ( html_of_declaration env declaration
    ++ p (html_of_comment (doc_comment |> Option.value ~default:"")) )

let html_of_docs : render_input -> Cow.Html.t =
 fun { all_modules; module_comment; declarations; imports; current_path } ->
  let name_of_type_dec doc =
    match doc.declaration with Type ty -> Some ty.name | _ -> None
  in
  let env =
    { imports; local_types = List.filter_map name_of_type_dec declarations }
  in
  let path_to_root =
    String.split_on_char '/' current_path
    |> List.tl
    |> List.map (fun _ -> "../")
    |> String.concat ""
  in
  let header =
    head ~attrs:[ ("title", "Doc") ]
      ( meta ~charset:"UTF-8" []
      ++ link ~rel:"stylesheet" (Uri.of_string (path_to_root ^ "styles.css")) )
  in
  let nav_of_doc doc =
    match doc.Extract.declaration with
    | Extract.Function func ->
        li (a ~href:(Uri.of_string ("#value." ^ func.name)) (string func.name))
    | Extract.Type typ ->
        li (a ~href:(Uri.of_string ("#type." ^ typ.name)) (string typ.name))
    | Extract.Class cls ->
        li (a ~href:(Uri.of_string ("#class." ^ cls.name)) (string cls.name))
    | Extract.Value val' ->
        li (a ~href:(Uri.of_string ("#value." ^ val'.name)) (string val'.name))
    | Extract.Unknown typ -> empty
  in
  let navigation =
    nav ~cls:"sidebar"
      ( h3 (string "Modules")
      ++ ul
           (List.map
              (fun path ->
                li
                  (a
                     ~href:(Uri.of_string (path_to_root ^ path ^ ".html"))
                     (string path)))
              all_modules)
      ++ h3 (string "Declarations")
      ++ ul (List.map nav_of_doc declarations) )
  in
  let bdy =
    body
      ( navigation
      ++ div ~cls:"documentation"
           ( h1 (string current_path)
           ++ html_of_comment module_comment
           ++ list (List.map (html_of_doc env) declarations) ) )
  in
  html (header ++ bdy)

let render_docs : render_input -> string =
 fun input -> Format.asprintf "%s" (Cow.Html.to_string (html_of_docs input))

let make_index : render_input list -> string =
 fun inputs ->
  let header =
    head
      ~attrs:[ ("title", "Motoko docs") ]
      ( meta ~charset:"UTF-8" []
      ++ link ~rel:"stylesheet" (Uri.of_string "styles.css") )
  in
  let make_link input =
    a ~cls:"index-item-link"
      ~href:(Uri.of_string (input.current_path ^ ".html"))
      (string input.current_path)
    ++ div ~cls:"index-item-comment" (html_of_comment input.module_comment)
  in
  let bdy =
    div ~cls:"index-container"
      ( h1 ~cls:"index-header" (string "Index of modules")
      ++ ul ~cls:"index-listing" ~licls:"index-item" (List.map make_link inputs)
      )
  in
  let index = html (header ++ bdy) in
  Format.asprintf "%s" (Cow.Html.to_string index)
