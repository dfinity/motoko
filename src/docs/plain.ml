open Extract
open Mo_def
open Source
open Printf

type level = int

let sep_by : Buffer.t -> string -> ('a -> unit) -> 'a list -> unit =
 fun buf sep f -> function
  | [] -> ()
  | x :: xs ->
      f x;
      List.iter
        (fun x ->
          Buffer.add_string buf sep;
          f x)
        xs

let sep_by' :
    Buffer.t -> string -> string -> string -> ('a -> unit) -> 'a list -> unit =
 fun buf open_ sep close f -> function
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

(** Adds a title at `level` *)
let title : Buffer.t -> level -> string -> unit =
 fun buf level txt -> bprintf buf "%s %s" (String.make level '#') txt

let rec plain_of_path : Buffer.t -> Syntax.path -> unit =
 fun buf path ->
  match path.Source.it with
  | Syntax.IdH id -> Buffer.add_string buf id.it
  | Syntax.DotH (path, id) ->
      plain_of_path buf path;
      bprintf buf ".%s" id.it

let string_of_path : Syntax.path -> string =
 fun path ->
  let buf = Buffer.create 8 in
  plain_of_path buf path;
  Buffer.contents buf

let plain_of_mut : Buffer.t -> Syntax.mut -> unit =
 fun buf mut ->
  match mut.it with
  | Syntax.Var -> Buffer.add_string buf "var "
  | Syntax.Const -> ()

let plain_of_func_sort : Buffer.t -> Syntax.func_sort -> unit =
 fun buf sort ->
  Mo_types.Type.(
    match sort.it with
    | Local -> ()
    | Shared Query -> bprintf buf "shared query "
    | Shared Write -> bprintf buf "shared ")

let plain_of_obj_sort : Buffer.t -> Syntax.obj_sort -> unit =
 fun buf sort ->
  Buffer.add_string buf
    Mo_types.Type.(
      match sort.it with
      | Object -> ""
      | Actor -> "actor "
      | Module -> "module "
      | Memory -> "memory ")

let rec plain_of_typ : Buffer.t -> (Syntax.path -> string) -> Syntax.typ -> unit
    =
 fun buf render_path typ ->
  match typ.Source.it with
  | Syntax.PathT (path, typs) ->
      bprintf buf "%s" (render_path path);
      sep_by' buf "<" ", " ">" (plain_of_typ buf render_path) typs
  | Syntax.PrimT typ -> Buffer.add_string buf typ
  | Syntax.ObjT (obj_sort, fields) ->
      plain_of_obj_sort buf obj_sort;
      bprintf buf "{ ";
      sep_by buf "; " (plain_of_typ_field buf render_path) fields;
      bprintf buf " }"
  | Syntax.ArrayT (mut, ty) ->
      bprintf buf "[";
      plain_of_mut buf mut;
      plain_of_typ buf render_path ty;
      bprintf buf "]"
  | Syntax.OptT typ ->
      bprintf buf "?";
      plain_of_typ buf render_path typ
  | Syntax.VariantT typ_tags ->
      bprintf buf "{";
      sep_by buf "; " (plain_of_typ_tag buf render_path) typ_tags;
      bprintf buf "}"
  | Syntax.TupT typ_list ->
      bprintf buf "(";
      sep_by buf ", " (plain_of_typ_item buf render_path) typ_list;
      bprintf buf ")"
  | Syntax.AsyncT (_scope, typ) ->
      bprintf buf "async ";
      plain_of_typ buf render_path typ
  | Syntax.ParT typ ->
      bprintf buf "(";
      plain_of_typ buf render_path typ;
      bprintf buf ")"
  | Syntax.NamedT (id, typ) ->
      bprintf buf "(";
      plain_of_typ_item buf render_path (Some id, typ);
      bprintf buf ")"
  | Syntax.FuncT (func_sort, typ_binders, arg, res) ->
      plain_of_func_sort buf func_sort;
      plain_of_typ_binders buf render_path typ_binders;
      plain_of_typ buf render_path arg;
      bprintf buf " -> ";
      plain_of_typ buf render_path res

and plain_of_typ_tag :
    Buffer.t -> (Syntax.path -> string) -> Syntax.typ_tag -> unit =
 fun buf render_path typ_tag ->
  bprintf buf "#%s" typ_tag.it.Syntax.tag.it;
  match typ_tag.it.Syntax.typ.it with
  | Syntax.TupT [] -> ()
  | _ ->
      bprintf buf " : ";
      plain_of_typ buf render_path typ_tag.it.Syntax.typ

and plain_of_typ_bind :
    Buffer.t -> (Syntax.path -> string) -> Syntax.typ_bind -> unit =
 fun buf render_path typ_bind ->
  let bound = typ_bind.it.Syntax.bound in
  Buffer.add_string buf typ_bind.it.Syntax.var.it;
  if not (Syntax.is_any bound) then (
    bprintf buf " <: ";
    plain_of_typ buf render_path typ_bind.it.Syntax.bound )

and plain_of_typ_binders :
    Buffer.t -> (Syntax.path -> string) -> Syntax.typ_bind list -> unit =
 fun buf render_path typ_binders ->
  let typ_binders =
    List.filter (fun b -> not (Common.is_scope_bind b)) typ_binders
  in
  sep_by' buf "<" ", " ">" (plain_of_typ_bind buf render_path) typ_binders

and plain_of_typ_field :
    Buffer.t -> (Syntax.path -> string) -> Syntax.typ_field -> unit =
 fun buf render_path field ->
  plain_of_mut buf field.it.Syntax.mut;
  bprintf buf "%s : " field.it.Syntax.id.it;
  plain_of_typ buf render_path field.it.Syntax.typ

and plain_of_typ_item :
    Buffer.t -> (Syntax.path -> string) -> Syntax.typ_item -> unit =
 fun buf render_path (oid, t) ->
  Option.iter (fun id -> bprintf buf "%s : " id.it) oid;
  plain_of_typ buf render_path t

let opt_typ : Buffer.t -> Syntax.typ option -> unit =
 fun buf ->
  Option.iter (fun ty ->
      bprintf buf " : ";
      plain_of_typ buf string_of_path ty)

let plain_of_doc_typ : Buffer.t -> doc_type -> unit =
 fun buf -> function
  | DTPlain ty -> plain_of_typ buf string_of_path ty
  | DTObj (ty, doc_fields) -> plain_of_typ buf string_of_path ty

let function_arg : Buffer.t -> function_arg_doc -> unit =
 fun buf arg ->
  Buffer.add_string buf arg.name;
  opt_typ buf arg.typ

let rec declaration_header : Buffer.t -> level -> declaration_doc -> unit =
 fun buf lvl -> function
  | Function function_doc ->
      title buf lvl (Printf.sprintf "Function `%s`" function_doc.name);
      bprintf buf "\n`func %s" function_doc.name;
      plain_of_typ_binders buf string_of_path function_doc.type_args;
      bprintf buf "(";
      sep_by buf ", " (function_arg buf) function_doc.args;
      bprintf buf ")";
      opt_typ buf function_doc.typ;
      bprintf buf "`\n\n"
  | Value value_doc ->
      title buf lvl (Printf.sprintf "Value `%s`" value_doc.name);
      bprintf buf "\n`let %s" value_doc.name;
      opt_typ buf value_doc.typ;
      bprintf buf "`\n\n"
  | Type type_doc ->
      title buf lvl (Printf.sprintf "Type `%s`" type_doc.name);
      bprintf buf "\n`type %s" type_doc.name;
      plain_of_typ_binders buf string_of_path type_doc.type_args;
      bprintf buf " = ";
      plain_of_doc_typ buf type_doc.typ;
      bprintf buf "`\n\n"
  | Class class_doc ->
      title buf lvl "`";
      plain_of_obj_sort buf class_doc.sort;
      bprintf buf "class %s" class_doc.name;
      plain_of_typ_binders buf string_of_path class_doc.type_args;
      bprintf buf "`\n\n";
      sep_by buf "\n" (plain_of_doc buf (lvl + 1)) class_doc.fields
  | Unknown u -> title buf lvl (Printf.sprintf "Unknown %s" u)

and plain_of_doc : Buffer.t -> level -> doc -> unit =
 fun buf lvl { doc_comment; declaration; _ } ->
  declaration_header buf lvl declaration;
  Option.iter (bprintf buf "%s\n") doc_comment

let render_docs : Common.render_input -> string =
 fun Common.{ module_comment; declarations; current_path; _ } ->
  let buf = Buffer.create 1024 in
  bprintf buf "# %s\n" current_path;
  bprintf buf "%s\n" module_comment;
  List.iter (plain_of_doc buf 2) declarations;
  Buffer.contents buf
