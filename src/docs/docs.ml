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
      ^ String.concat "\n" (List.map render_doc class_doc.fields)
      ^ Printf.sprintf "\nend class %s" class_doc.name
  | Unknown _ -> "Unknown\n========\n"

and render_doc : doc -> string =
 fun { doc_comment; declaration } ->
  declaration_header declaration
  ^ "\n"
  ^ Option.value ~default:"No documentation comment" doc_comment
  ^ "\n"

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let conv_token (tkn, _, _) = tkn

let conv_start (_, start, _) = start

let conv_end (_, _, end_) = end_

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
  List.iter (fun doc -> print_endline (render_doc doc)) docs

(* let file = "/home/creek/code/motoko/src/mytest.mo" *)
(* let file = "/home/creek/code/mo-libs/motoko-base/src/List.mo" *)
let file = "/home/creek/code/mo-libs/motoko-base/src/HashMap.mo"

let start () = simplistic_docs file
