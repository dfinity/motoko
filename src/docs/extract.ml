open Mo_frontend
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
  | DTObj of Syntax.typ * (Syntax.typ_field * string) list

(* TODO We'll also want to unwrap variants here *)
and class_doc = {
  name : string;
  type_args : Syntax.typ_bind list;
  constructor : function_arg_doc list;
  fields : doc list;
  sort : Syntax.obj_sort;
}

let string_of_leading : Lexer.trivia_info -> string =
 fun info ->
  String.concat "\n"
    (List.filter_map
       (function
         | Source_token.Comment s -> (
             match Lib.String.chop_prefix "///" s with
             | Some "" -> Some ""
             | Some line_comment ->
                 (* We expect a documentation line comment to start with a space
                  *  (which we remove here) *)
                 Lib.String.chop_prefix " " line_comment
             | None ->
                 Option.bind
                   (Lib.String.chop_prefix "/**" s)
                   (Lib.String.chop_suffix "*/")
                 |> Option.map String.trim )
         | _ -> None)
       info.Lexer.leading_trivia)

let _print_leading : Lexer.trivia_info -> unit =
 fun info -> print_endline (string_of_leading info)

let un_prog prog =
  let rec go acc = function
    | { it = Syntax.ExpD { it = Syntax.ObjBlockE (_, m); _ }; _ } :: _ -> Ok (acc, m)
    | {
        it =
          Syntax.LetD
            ({ it = Syntax.VarP v; _ }, { it = Syntax.ImportE (path, _); _ });
        _;
      }
      :: tail ->
        go ((v.it, path) :: acc) tail
    | _ :: tail -> go acc tail
    | [] -> Error "Couldn't find a module expression"
  in
  go [] prog.Source.it

module PosTable = Lexer.PosHashtbl

type extracted = {
  module_comment : string;
  lookup_type : Syntax.path -> Xref.t option;
  docs : doc list;
}

module MakeExtract (Env : sig
  val all_decs : Syntax.dec_field list

  val imports : (string * string) list

  val find_trivia : Source.region -> Lexer.trivia_info
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
            doc = Some (string_of_leading (Env.find_trivia at));
          }
    | Source.{ it = Syntax.AnnotP (p, ty); at; _ } ->
        Option.map
          (fun x ->
            {
              x with
              typ = Some ty;
              doc = Some (string_of_leading (Env.find_trivia at));
            })
          (extract_args p)
    | Source.{ it = Syntax.WildP; _ } -> None
    | pat ->
        Wasm.Sexpr.print 80 (Arrange.pat pat);
        None

  let extract_func_args = function
    | { it = Syntax.ParP arg; _ } -> Option.to_list (extract_args arg)
    | { it = Syntax.TupP args; _ } -> List.filter_map extract_args args
    | _ -> []

  let extract_let_doc : Syntax.exp -> string -> declaration_doc = function
    | Source.{ it = Syntax.FuncE (_, _, type_args, args, typ, _, _); _ } ->
        fun name ->
          let args_doc = extract_func_args args in
          Function { name; typ; type_args; args = args_doc }
    | Source.{ it = Syntax.AnnotE (e, ty); _ } ->
        fun name -> Value { name; typ = Some ty }
    | _ -> fun name -> Value { name; typ = None }

  let extract_obj_field_doc : Syntax.typ_field -> Syntax.typ_field * string =
   fun ({ at; _ } as tf) -> (tf, string_of_leading (Env.find_trivia at))

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
    if dec_field.it.Syntax.vis.it <> Syntax.Public then None
    else
      extract_doc mk_xref dec_field.it.Syntax.dec
      |> Option.map (fun (xref, decl_doc) ->
             {
               xref;
               doc_comment =
                 Some (string_of_leading (Env.find_trivia dec_field.at));
               declaration = decl_doc;
             })
end

module type Extract = sig
  val extract_exp_field : (Xref.t -> Xref.t) -> Syntax.dec list -> doc option
end

let extract_docs : Syntax.prog -> Lexer.triv_table -> (extracted, string) result
    =
 fun prog trivia_table ->
  let lookup_trivia (line, column) =
    Lexer.PosHashtbl.find_opt trivia_table Lexer.{ line; column }
  in
  let find_trivia (parser_pos : Source.region) : Lexer.trivia_info =
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
          module_comment = string_of_leading module_docs;
          lookup_type = Ex.lookup_type;
          docs;
        }
  | Error msg -> Error msg
