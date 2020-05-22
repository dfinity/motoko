open Mo_frontend
open Extract
open Mo_def
open Source

let rec string_of_path path =
  match path.Source.it with
  | Syntax.IdH id -> id.it
  | Syntax.DotH (path, id) -> string_of_path path ^ id.it

let string_of_mut mut =
  match mut.it with Syntax.Var -> "mut " | Syntax.Const -> ""

let string_of_typ_bind typ_bind = typ_bind.it.Syntax.var.it

let rec string_of_typ typ =
  match typ.Source.it with
  | Syntax.PathT (path, typs) ->
      string_of_path path
      ^
      if List.length typs = 0 then ""
      else "<" ^ String.concat ", " (List.map string_of_typ typs) ^ ">"
  | Syntax.PrimT typ -> typ
  | Syntax.ObjT (obj_sort, fields) ->
      "{"
      ^ String.concat "; "
          (List.map
             (fun (field : Syntax.typ_field) ->
               (* TODO mut might be wrong here *)
               string_of_mut field.it.Syntax.mut
               ^ field.it.Syntax.id.it
               ^ " : "
               ^ string_of_typ field.it.Syntax.typ)
             fields)
      ^ "}"
  | Syntax.ArrayT (mut, ty) -> "[" ^ string_of_mut mut ^ string_of_typ ty ^ "]"
  | Syntax.OptT typ ->
      Printf.sprintf "?(%s)" (string_of_typ typ)
      (* TODO only parenthesize non-trivial types *)
  | Syntax.VariantT typ_tags ->
      "{"
      ^ String.concat "; "
          (List.map
             (fun typ_tag ->
               Printf.sprintf "#%s : %s" typ_tag.it.Syntax.tag.it
                 (string_of_typ typ_tag.it.Syntax.typ))
             typ_tags)
      ^ "}"
  | Syntax.TupT typ_list ->
      "(" ^ String.concat ", " (List.map string_of_typ typ_list) ^ ")"
  | Syntax.AsyncT (_scope, typ) -> Printf.sprintf "async %s" (string_of_typ typ)
  | Syntax.ParT typ -> "(" ^ string_of_typ typ ^ ")"
  | Syntax.FuncT (func_sort, binders, arg, res) -> "TODO"

let opt_typ : Syntax.typ option -> string =
  Option.fold ~none:"" ~some:(fun ty -> " : " ^ string_of_typ ty)

let function_arg : function_arg_doc -> string =
 fun arg -> Printf.sprintf "%s%s" arg.name (opt_typ arg.typ)

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let rec declaration_header : declaration_doc -> string = function
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

and render_doc_string : doc -> string =
 fun { doc_comment; declaration } ->
  declaration_header declaration
  ^ "\n"
  ^ Option.value ~default:"No documentation comment" doc_comment
  ^ "\n"

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
  let imports, docs = extract_docs prog trivia_table in
  (* Wasm.Sexpr.print 80 (Arrange.prog prog); *)
  List.iter (fun (v, p) -> Printf.printf "import %s \"%s\";\n" v p) imports;
  List.iter (fun doc -> print_endline (render_doc_string doc)) docs

let html_docs : string -> unit =
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
  let imports, docs = extract_docs prog trivia_table in
  print_endline (Html.render_docs docs)

(* let file = "/home/creek/code/motoko/src/mytest.mo" *)
(* let file = "/home/creek/code/mo-libs/motoko-base/src/List.mo" *)
let file = "/home/creek/code/mo-libs/motoko-base/src/HashMap.mo"

(* let start () = simplistic_docs file *)
let start () = html_docs file
