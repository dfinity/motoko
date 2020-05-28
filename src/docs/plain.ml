open Extract
open Mo_def
open Source
open Printf

let sep_by buf sep f = function
  | [] -> ()
  | x :: xs ->
      f x;
      List.iter
        (fun x ->
          Buffer.add_string buf sep;
          f x)
        xs

let sep_by' buf open_ sep close f = function
  | [] -> ()
  | x :: xs ->
      Buffer.add_string buf open_;
      f x;
      List.iter
        (fun x ->
          Buffer.add_string buf sep;
          f x)
        xs;
      Buffer.add_string buf close

let rec plain_of_path buf path =
  match path.Source.it with
  | Syntax.IdH id -> Buffer.add_string buf id.it
  | Syntax.DotH (path, id) ->
      plain_of_path buf path;
      Buffer.add_string buf id.it

let plain_of_mut buf mut =
  match mut.it with
  | Syntax.Var -> Buffer.add_string buf "mut "
  | Syntax.Const -> ()

let plain_of_typ_bind buf typ_bind =
  Buffer.add_string buf typ_bind.it.Syntax.var.it

let rec plain_of_typ buf typ =
  match typ.Source.it with
  | Syntax.PathT (path, typs) ->
      plain_of_path buf path;
      sep_by' buf "<" ", " ">" (plain_of_typ buf) typs
  | Syntax.PrimT typ -> Buffer.add_string buf typ
  | Syntax.ObjT (obj_sort, fields) ->
      bprintf buf "{";
      sep_by buf "; " (plain_of_typ_field buf) fields;
      bprintf buf "}"
  | Syntax.ArrayT (mut, ty) ->
      bprintf buf "[";
      plain_of_mut buf mut;
      plain_of_typ buf ty;
      bprintf buf "]"
  | Syntax.OptT typ ->
      (* TODO only parenthesize non-trivial types *)
      bprintf buf "?(";
      plain_of_typ buf typ;
      bprintf buf ")"
  | Syntax.VariantT typ_tags ->
      bprintf buf "{";
      sep_by buf "; "
        (fun (typ_tag : Syntax.typ_tag) ->
          bprintf buf "#%s : " typ_tag.it.Syntax.tag.it;
          plain_of_typ buf typ_tag.it.Syntax.typ)
        typ_tags;
      bprintf buf "}"
  | Syntax.TupT typ_list ->
      bprintf buf "(";
      sep_by buf ", " (plain_of_typ buf) typ_list;
      bprintf buf ")"
  | Syntax.AsyncT (_scope, typ) ->
      bprintf buf "async ";
      plain_of_typ buf typ
  | Syntax.ParT typ ->
      bprintf buf "(";
      plain_of_typ buf typ;
      bprintf buf ")"
  | Syntax.FuncT (func_sort, binders, arg, res) -> bprintf buf "TODO"

and plain_of_typ_field : Buffer.t -> Syntax.typ_field -> unit =
 fun buf field ->
  plain_of_mut buf field.it.Syntax.mut;
  bprintf buf "%s : " field.it.Syntax.id.it;
  plain_of_typ buf field.it.Syntax.typ

let opt_typ : Buffer.t -> Syntax.typ option -> unit =
 fun buf ->
  Option.iter (fun ty ->
      bprintf buf " : ";
      plain_of_typ buf ty)

let plain_of_doc_typ : Buffer.t -> doc_type -> unit =
 fun buf -> function
  | DTPlain ty -> plain_of_typ buf ty
  | DTObj doc_fields -> bprintf buf "TODO"

let function_arg : Buffer.t -> function_arg_doc -> unit =
 fun buf arg ->
  Buffer.add_string buf arg.name;
  opt_typ buf arg.typ

let string_of_list f xs =
  List.map f xs |> String.concat "; " |> fun x -> "[ " ^ x ^ " ]"

let rec declaration_header : Buffer.t -> declaration_doc -> unit =
 fun buf -> function
  | Function function_doc ->
      bprintf buf "Function %s\n========\nfunc %s" function_doc.name
        function_doc.name;
      sep_by' buf "<" ", " ">" (plain_of_typ_bind buf) function_doc.type_args;
      bprintf buf "(";
      sep_by buf ", " (function_arg buf) function_doc.args;
      bprintf buf ")";
      opt_typ buf function_doc.typ
  | Value value_doc ->
      bprintf buf "Value %s\n========\nlet %s" value_doc.name value_doc.name;
      opt_typ buf value_doc.typ
  | Type type_doc ->
      bprintf buf "Type %s\n========\ntype %s" type_doc.name type_doc.name;
      sep_by' buf "<" ", " ">" (plain_of_typ_bind buf) type_doc.type_args;
      bprintf buf " = ";
      plain_of_doc_typ buf type_doc.typ
  | Class class_doc ->
      bprintf buf "Class %s\n========\nbegin class %s" class_doc.name
        class_doc.name;
      sep_by' buf "<" ", " ">" (plain_of_typ_bind buf) class_doc.type_args;
      sep_by buf "\n" (plain_of_doc buf) class_doc.fields;
      bprintf buf "\nend class %s" class_doc.name
  | Unknown _ -> bprintf buf "Unknown\n========\n"

and plain_of_doc : Buffer.t -> doc -> unit =
 fun buf { doc_comment; declaration } ->
  declaration_header buf declaration;
  bprintf buf "\n";
  Buffer.add_string buf
    (Option.value ~default:"No documentation comment" doc_comment);
  bprintf buf "\n"

and render_doc_string : doc -> string =
 fun doc ->
  let buf = Buffer.create 1024 in
  plain_of_doc buf doc;
  Buffer.contents buf
