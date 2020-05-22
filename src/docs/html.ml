open Tyxml
open Tyxml.Html
open Extract

let html_of_declaration_doc = function
  | Function function_doc ->
      let args = String.concat ", " (List.map function_arg function_doc.args) in
      let ty_args =
        match function_doc.type_args with
        | [] -> ""
        | xs -> "<" ^ String.concat ", " (List.map string_of_typ_bind xs) ^ ">"
      in
      let typ = opt_typ function_doc.typ in
      Printf.sprintf "Function %s\n========\nfunc %s%s(%s)%s" function_doc.name
        function_doc.name ty_args args typ
  | Type type_doc ->
      Printf.sprintf "Type %s\n========\ntype %s%s = %s" type_doc.name
        type_doc.name
        ( if type_doc.type_args = [] then ""
        else "<" ^ String.concat ", " type_doc.type_args ^ ">" )
        (string_of_typ type_doc.typ)
  | Class class_doc ->
      Printf.sprintf "Class %s\n========\nbegin class %s%s\n" class_doc.name
        class_doc.name
        ( if class_doc.type_args = [] then ""
        else
          "<"
          ^ String.concat ", " (List.map string_of_typ_bind class_doc.type_args)
          ^ ">" )
      ^ String.concat "\n" (List.map render_doc_string class_doc.fields)
      ^ Printf.sprintf "\nend class %s" class_doc.name
  | Unknown _ -> "Unknown\n========\n"

let html_of_doc { doc_comment; declaration } =
  div
    [
      p
        ~a:[ a_class [ "doc-comment" ] ]
        [ txt (doc_comment |> Option.value ~default:"") ];
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
