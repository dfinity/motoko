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
  | _ -> [ txt "html_of_type" ]

(*
 * | Syntax.ObjT (obj_sort, fields) ->
 *     "{"
 *     ^ String.concat "; "
 *         (List.map
 *            (fun (field : Syntax.typ_field) ->
 *              (\* TODO mut might be wrong here *\)
 *              string_of_mut field.it.Syntax.mut
 *              ^ field.it.Syntax.id.it
 *              ^ " : "
 *              ^ string_of_typ field.it.Syntax.typ)
 *            fields)
 *     ^ "}"
 * | Syntax.ArrayT (mut, ty) -> "[" ^ string_of_mut mut ^ string_of_typ ty ^ "]"
 * | Syntax.OptT typ ->
 *     Printf.sprintf "?(%s)" (string_of_typ typ)
 *     (\* TODO only parenthesize non-trivial types *\)
 * | Syntax.AsyncT (_scope, typ) -> Printf.sprintf "async %s" (string_of_typ typ)
 *)

let html_of_arg (arg : Extract.function_arg_doc) =
  parameter arg.name
  :: Option.fold ~none:[]
       ~some:(fun arg -> txt " : " :: html_of_type arg)
       arg.typ

let html_of_declaration = function
  (* | Function function_doc ->
   *     let args = String.concat ", " (List.map function_arg function_doc.args) in
   *     let ty_args =
   *       match function_doc.type_args with
   *       | [] -> ""
   *       | xs -> "<" ^ String.concat ", " (List.map string_of_typ_bind xs) ^ ">"
   *     in
   *     let typ = opt_typ function_doc.typ in
   *     Printf.sprintf "Function %s\n========\nfunc %s%s(%s)%s" function_doc.name
   *       function_doc.name ty_args args typ *)
  | Function function_doc ->
      let ty_args =
        match function_doc.type_args with
        | [] -> []
        | xs ->
            txt "<"
            :: join_with [ txt "," ]
                 (List.map (fun t -> [ html_of_typ_bind t ]) xs)
            @ [ txt ">" ]
      in
      let args =
        join_with [ txt ", "; br (); space (); space () ] (List.map html_of_arg function_doc.args)
      in
      code
        ( [ keyword "func "; txt function_doc.name ]
        @ ty_args
        @ [ txt "("; br (); space (); space () ]
        @ args
        @ [ br (); txt ")" ] )
  | _ -> code [ txt "html_of_declaration" ]

(* | Type type_doc ->
 *     Printf.sprintf "Type %s\n========\ntype %s%s = %s" type_doc.name
 *       type_doc.name
 *       ( if type_doc.type_args = [] then ""
 *       else "<" ^ String.concat ", " type_doc.type_args ^ ">" )
 *       (string_of_typ type_doc.typ)
 * | Class class_doc ->
 *     Printf.sprintf "Class %s\n========\nbegin class %s%s\n" class_doc.name
 *       class_doc.name
 *       ( if class_doc.type_args = [] then ""
 *       else
 *         "<"
 *         ^ String.concat ", " (List.map string_of_typ_bind class_doc.type_args)
 *         ^ ">" )
 *     ^ String.concat "\n" (List.map render_doc_string class_doc.fields)
 *     ^ Printf.sprintf "\nend class %s" class_doc.name
 * | Unknown _ -> "Unknown\n========\n" *)

let html_of_doc { doc_comment; declaration } =
  div
    [
      html_of_declaration declaration;
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
