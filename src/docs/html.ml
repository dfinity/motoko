open Tyxml
open Tyxml.Html
open Extract
open Mo_def

let rec join_with sep = function
  | [] -> []
  | [ x ] -> x
  | x :: xs -> x @ sep @ join_with sep xs

let keyword s = span ~a:[ a_class [ "keyword" ] ] [ txt s ]

let parameter s = span ~a:[ a_class [ "parameter" ] ] [ txt s ]

let html_type s = span ~a:[ a_class [ "type" ] ] [ txt s ]

let rec string_of_path path =
  match path.Source.it with
  | Syntax.IdH id -> id.Source.it
  | Syntax.DotH (path, id) -> string_of_path path ^ "." ^ id.Source.it

let html_of_mut mut =
  match mut.Source.it with
  | Syntax.Var -> keyword "mut "
  | Syntax.Const -> txt ""

let html_of_typ_bind typ_bind =
  html_type typ_bind.Source.it.Syntax.var.Source.it

let rec html_of_type typ =
  match typ.Source.it with
  | Syntax.PathT (path, typs) -> (
      [ html_type (string_of_path path) ]
      @
      match typs with
      | [] -> []
      | xs ->
          (txt "<" :: join_with [ txt ", " ] (List.map html_of_type xs))
          @ [ txt ">" ] )
  | Syntax.PrimT typ -> [ html_type typ ]
  | Syntax.ParT typ -> (txt "(" :: html_of_type typ) @ [ txt ")" ]
  | Syntax.OptT typ ->
      (* TODO only parenthesize non-trivial types *)
      (txt "?(" :: html_of_type typ) @ [ txt ")" ]
  | Syntax.TupT typ_list ->
      (txt "(" :: join_with [ txt ", " ] (List.map html_of_type typ_list))
      @ [ txt ")" ]
  | Syntax.VariantT typ_tags ->
      txt "{"
      :: join_with [ txt "; " ]
           (List.map
              (fun typ_tag ->
                txt
                  (Printf.sprintf "#%s : "
                     typ_tag.Source.it.Syntax.tag.Source.it)
                :: html_of_type typ_tag.Source.it.Syntax.typ)
              typ_tags)
      @ [ txt "}" ]
  | Syntax.FuncT (func_sort, typ_binders, arg, res) ->
      let ty_args =
        match typ_binders with
        | [] -> []
        | xs ->
            txt "<"
            :: join_with [ txt ", " ]
                 (List.map (fun t -> [ html_of_typ_bind t ]) xs)
            @ [ txt ">" ]
      in
      ty_args @ html_of_type arg @ (txt " -> " :: html_of_type res)
  | Syntax.ArrayT (mut, ty) ->
      [ txt "["; html_of_mut mut ] @ html_of_type ty @ [ txt "]" ]
  | Syntax.AsyncT (_scope, typ) -> keyword "async " :: html_of_type typ
  | Syntax.ObjT (obj_sort, fields) ->
      [ txt "{" ]
      @ join_with [ txt "; " ]
          (List.map
             (fun (field : Syntax.typ_field) ->
               (* TODO mut might be wrong here *)
               [
                 html_of_mut field.Source.it.Syntax.mut;
                 txt (field.Source.it.Syntax.id.Source.it ^ " : ");
               ]
               @ html_of_type field.Source.it.Syntax.typ)
             fields)
      @ [ txt "}" ]

let html_of_arg (arg : Extract.function_arg_doc) =
  parameter arg.name
  :: Option.fold ~none:[]
       ~some:(fun arg -> txt " : " :: html_of_type arg)
       arg.typ

let rec html_of_declaration = function
  | Function function_doc ->
      let is_multiline = List.length function_doc.args > 2 in
      let br' = if is_multiline then [ br () ] else [] in
      let br_indent =
        if is_multiline then [ br (); space (); space () ] else []
      in
      let ty_args =
        match function_doc.type_args with
        | [] -> []
        | xs ->
            txt "<"
            :: join_with [ txt ", " ]
                 (List.map (fun t -> [ html_of_typ_bind t ]) xs)
            @ [ txt ">" ]
      in
      let args =
        join_with (txt ", " :: br_indent)
          (List.map html_of_arg function_doc.args)
      in
      let return_typ =
        Option.fold ~none:[]
          ~some:(fun typ -> txt " -> " :: html_of_type typ)
          function_doc.typ
      in
      code
        ( [ keyword "func "; txt function_doc.name ]
        @ ty_args
        @ (txt "(" :: br_indent)
        @ args
        @ br'
        @ [ txt ")" ]
        @ return_typ )
  | Class class_doc ->
      let ty_args =
        match class_doc.type_args with
        | [] -> []
        | xs ->
            txt "<"
            :: join_with [ txt ", " ]
                 (List.map (fun t -> [ html_of_typ_bind t ]) xs)
            @ [ txt ">" ]
      in
      div
        ~a:[ a_class [ "class-declaration" ] ]
        ( [ h3 ([ keyword "class "; html_type class_doc.name ] @ ty_args) ]
        @ List.map html_of_doc class_doc.fields )
  | Type type_doc ->
      let ty_args =
        match type_doc.type_args with
        | [] -> []
        | xs ->
            txt "<"
            :: join_with [ txt ", " ] (List.map (fun t -> [ html_type t ]) xs)
            @ [ txt ">" ]
      in
      code
        ( [ keyword "type "; html_type type_doc.name ]
        @ ty_args
        @ [ txt " = " ]
        @ html_of_type type_doc.typ )
  | Unknown s -> code [ txt "Unknown: "; txt s ]

and html_of_doc { doc_comment; declaration } =
  div
    ~a:[ a_class [ "declaration" ] ]
    [
      html_of_declaration declaration;
      p
        ~a:[ a_class [ "doc-comment" ] ]
        (join_with [ br () ]
           ( Lib.String.split (doc_comment |> Option.value ~default:"") '\n'
           |> List.map (fun s -> [ txt s ])));
    ]

let html_of_docs : doc list -> Html.doc =
 fun docs ->
  let header =
    head
      (title (txt "Doc"))
      [
        meta ~a:[ a_charset "UTF-8" ] ();
        link ~rel:[ `Stylesheet ] ~href:"styles.css" ();
      ]
  in
  html header (body (List.map html_of_doc docs))

let render_docs : doc list -> string =
 fun docs -> Format.asprintf "%a" (Html.pp ()) (html_of_docs docs)
