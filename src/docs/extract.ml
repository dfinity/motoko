open Mo_def
open Source

type doc = {
  xref : Xref.t;
  doc_comment : string option;
  declaration : declaration_doc;
}

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
  | DTObj of Syntax.typ * (Syntax.typ_field * string option) list

(* TODO We'll also want to unwrap variants here *)
and class_doc = {
  name : string;
  type_args : Syntax.typ_bind list;
  constructor : function_arg_doc list;
  fields : doc list;
  sort : Syntax.obj_sort;
}

let un_prog prog =
  let comp_unit = Mo_def.CompUnit.comp_unit_of_prog true prog in
  let open Syntax in
  let { imports; body } = comp_unit.it in
  let imports =
    List.concat_map
      (fun i ->
        match i.it with
        | { it = VarP alias; _ }, path, _ -> [ (alias.it, path) ]
        | _ -> []) (* FIXME: explicit imports #3078 *)
      imports
  in
  match body.it with
  | ModuleU (_, decs) -> Ok (imports, decs)
  | _ -> Error "Couldn't find a module expression"

module PosTable = Trivia.PosHashtbl

type extracted = {
  module_comment : string option;
  lookup_type : Syntax.path -> Xref.t option;
  docs : doc list;
}

module MakeExtract (Env : sig
  val all_decs : Syntax.dec_field list
  val imports : (string * string) list
  val find_trivia : Source.region -> Trivia.trivia_info
end) =
struct
  let namespace : Namespace.t =
    let import_ns = Namespace.from_imports Env.imports in
    let module_ns = Namespace.from_module Env.all_decs in
    Namespace.shadow import_ns module_ns

  let lookup_type : Syntax.path -> Xref.t option =
    Namespace.lookup_type namespace

  let rec extract_args = function
    | Source.{ it = Syntax.VarP { it = name; at; _ }; _ } ->
        Some
          {
            name;
            typ = None;
            doc = Trivia.doc_comment_of_trivia_info (Env.find_trivia at);
          }
    | Source.{ it = Syntax.AnnotP (p, ty); at; _ } ->
        Option.map
          (fun x ->
            {
              x with
              typ = Some ty;
              doc = Trivia.doc_comment_of_trivia_info (Env.find_trivia at);
            })
          (extract_args p)
    | Source.{ it = Syntax.WildP; _ } -> None
    | pat ->
        (* Wasm.Sexpr.print 80 (Arrange.pat pat); *)
        None

  let extract_func_args = function
    | { it = Syntax.ParP arg; _ } -> Option.to_list (extract_args arg)
    | { it = Syntax.TupP args; _ } -> List.filter_map extract_args args
    | _ -> []

  let extract_let_doc : Syntax.exp -> string -> declaration_doc =
   fun exp name ->
    match exp.it with
    | Syntax.FuncE (_, _, type_args, args, typ, _, _) ->
        let args_doc = extract_func_args args in
        Function { name; typ; type_args; args = args_doc }
    | Syntax.AnnotE (e, ty) -> Value { name; typ = Some ty }
    | _ -> Value { name; typ = None }

  let extract_obj_field_doc :
      Syntax.typ_field -> Syntax.typ_field * string option =
   fun ({ at; _ } as tf) ->
    (tf, Trivia.doc_comment_of_trivia_info (Env.find_trivia at))

  let rec extract_doc mk_xref = function
    | Source.
        { it = Syntax.LetD ({ it = Syntax.VarP { it = name; _ }; _ }, rhs); _ }
      ->
        Some (mk_xref (Xref.XValue name), extract_let_doc rhs name)
    | Source.{ it = Syntax.TypD (name, ty_args, typ); _ } ->
        let doc_typ =
          match typ.it with
          | Syntax.ObjT (_, fields) ->
              let doc_fields = List.map extract_obj_field_doc fields in
              (* TODO Only unwrap the ObjT if at least one field is documented *)
              DTObj (typ, doc_fields)
          | _ -> DTPlain typ
        in
        Some
          ( mk_xref (Xref.XType name.it),
            Type { name = name.it; type_args = ty_args; typ = doc_typ } )
    | Source.
        {
          it =
            Syntax.ClassD
              (shared_pat, name, type_args, ctor, _, obj_sort, _, fields);
          _;
        } ->
        let mk_field_xref xref = mk_xref (Xref.XClass (name.it, xref)) in
        Some
          ( mk_xref (Xref.XType name.it),
            Class
              {
                name = name.it;
                type_args;
                constructor = extract_func_args ctor;
                fields =
                  List.filter_map (extract_dec_field mk_field_xref) fields;
                sort = obj_sort;
              } )
    | unknown ->
        print_endline "Failed to extract documentation for declaration:";
        Wasm.Sexpr.print 80 (Arrange.dec unknown);
        None

  and extract_dec_field mk_xref dec_field =
    if Syntax.is_public dec_field.it.Syntax.vis then
      extract_doc mk_xref dec_field.it.Syntax.dec
      |> Option.map (fun (xref, decl_doc) ->
             {
               xref;
               doc_comment =
                 Trivia.doc_comment_of_trivia_info
                   (Env.find_trivia dec_field.at);
               declaration = decl_doc;
             })
    else None
end

let extract_docs : Syntax.prog -> (extracted, string) result =
 fun prog ->
  let lookup_trivia (line, column) =
    PosTable.find_opt prog.note.Syntax.trivia Trivia.{ line; column }
  in
  let find_trivia (parser_pos : Source.region) : Trivia.trivia_info =
    lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
    |> Option.get
  in
  let module_docs = find_trivia prog.at in
  (* Skip the module header *)
  match un_prog prog with
  | Ok (imports, decls) ->
      let module Ex = MakeExtract (struct
        let all_decs = decls
        let imports = imports
        let find_trivia = find_trivia
      end) in
      let docs = List.filter_map (Ex.extract_dec_field Fun.id) decls in
      Ok
        {
          module_comment = Trivia.doc_comment_of_trivia_info module_docs;
          lookup_type = Ex.lookup_type;
          docs;
        }
  | Error msg -> Error msg
