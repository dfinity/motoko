open Extract
open Printf
open Mo_def

type env = { lookup_type : Syntax.path -> Xref.t option }

let surround : Buffer.t -> string -> (unit -> unit) -> unit =
 fun buf s inner ->
  Buffer.add_string buf s;
  inner ();
  Buffer.add_string buf s

let adoc_link : Buffer.t -> Xref.t -> (unit -> unit) -> unit =
 fun buf xref adoc_text ->
  let prepend_hash is_top s = if is_top then "#" ^ s else s in
  let rec string_of_xref is_top = function
    | Xref.XClass (x, xref) ->
        prepend_hash is_top
          (Printf.sprintf "%s.%s" x (string_of_xref false xref))
    | Xref.XNested (x, xref) ->
        prepend_hash is_top
          (Printf.sprintf "%s.%s" x (string_of_xref false xref))
    | Xref.XValue x -> prepend_hash is_top x
    | Xref.XType x -> prepend_hash is_top ("type." ^ x)
    | Xref.XPackage (_, _) -> ""
    | Xref.XFile (p, None) -> p
    | Xref.XFile (p, Some xref) ->
        Printf.sprintf "%s.adoc#%s" p (string_of_xref false xref)
  in
  let link = string_of_xref true xref in
  bprintf buf "xref:%s[" link;
  adoc_text ();
  bprintf buf "]"

let adoc_of_type : Buffer.t -> env -> Syntax.typ -> unit =
 fun buf env ty ->
  let render_path buf path =
    match env.lookup_type path with
    | None ->
        Plain.plain_of_path buf path
    | Some xref ->
       adoc_link buf xref (fun () -> Plain.plain_of_path buf path)
  in
  Plain.plain_of_typ buf render_path ty

let opt_typ : Buffer.t -> env -> Syntax.typ option -> unit =
 fun buf env ->
  Option.iter (fun ty ->
      bprintf buf " : ";
      adoc_of_type buf env ty)

let adoc_of_typ_bind : Buffer.t -> Syntax.typ_bind -> unit =
  Plain.plain_of_typ_bind

let adoc_of_function_arg : Buffer.t -> env -> function_arg_doc -> unit =
 fun buf env arg ->
  Buffer.add_string buf arg.name;
  opt_typ buf env arg.typ

let adoc_header : Buffer.t -> int -> string -> unit =
 fun buf lvl s -> bprintf buf "%s %s\n\n" (String.make (lvl + 1) '=') s

let rec lvl_of_xref : Xref.t -> int = function
  | Xref.XType _ | Xref.XValue _ -> 1
  | Xref.XNested (_, xref) | Xref.XClass (_, xref) -> lvl_of_xref xref + 1
  | Xref.XFile (file, xref) -> (* TODO *) 0
  | Xref.XPackage (pkg, xref) -> (* TODO *) 0

let adoc_xref : Buffer.t -> Xref.t -> unit =
 fun buf xref ->
  let rec go = function
    | Xref.XType t -> bprintf buf "type.%s" t
    | Xref.XValue t -> bprintf buf "%s" t
    | Xref.XNested (x, xref) ->
        bprintf buf "%s." x;
        go xref
    | Xref.XClass (x, xref) ->
        bprintf buf "%s." x;
        go xref
    | Xref.XFile (file, xref) -> (* TODO *) ()
    | Xref.XPackage (pkg, xref) -> (* TODO *) ()
  in
  bprintf buf "[[";
  go xref;
  bprintf buf "]]\n"

let adoc_signature : Buffer.t -> (unit -> unit) -> unit =
 fun buf f ->
  bprintf buf "[source.no-repl,motoko,subs=+macros]\n----\n";
  f ();
  bprintf buf "\n----\n\n"


let adoc_of_doc_type : Buffer.t -> env -> Extract.doc_type -> unit =
 fun buf env dt ->
  match dt with
  | DTPlain t -> adoc_of_type buf env t
  | DTObj (t, fields) -> adoc_of_type buf env t

let rec adoc_of_declaration :
    Buffer.t -> env -> Xref.t -> (unit -> unit) -> declaration_doc -> unit =
 fun buf env xref doc_comment doc ->
  let header = adoc_header buf (lvl_of_xref xref) in
  let signature = adoc_signature buf in
  adoc_xref buf xref;
  match doc with
  | Function function_doc ->
      header function_doc.name;
      signature (fun _ ->
          bprintf buf "func %s" function_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            function_doc.type_args;
          bprintf buf "(";
          Plain.sep_by buf ", " (adoc_of_function_arg buf env) function_doc.args;
          bprintf buf ")";
          opt_typ buf env function_doc.typ);
      doc_comment ()
  | Value value_doc ->
      header value_doc.name;
      signature (fun _ ->
          bprintf buf "let %s" value_doc.name;
          opt_typ buf env value_doc.typ);
      doc_comment ()
  | Type type_doc ->
      header type_doc.name;
      signature (fun _ ->
          bprintf buf "type %s" type_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            type_doc.type_args;
          bprintf buf " = ";
          adoc_of_doc_type buf env type_doc.typ);
      doc_comment ()
  | Class class_doc ->
      header class_doc.name;
      signature (fun _ ->
          bprintf buf "class %s" class_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            class_doc.type_args;
          bprintf buf "(";
          Plain.sep_by buf ", " (adoc_of_function_arg buf env) class_doc.constructor;
          bprintf buf ")");
      doc_comment ();
      bprintf buf "\n\n";
      List.iter (adoc_of_doc buf env) class_doc.fields
  | Unknown unknown ->
      signature (fun _ -> bprintf buf "Unknown %s" unknown);
      doc_comment ()

and adoc_of_doc : Buffer.t -> env -> doc -> unit =
 fun buf env { doc_comment; declaration; xref } ->
  let doc_comment () =
    Option.iter (Buffer.add_string buf) doc_comment;
    bprintf buf "\n\n"
  in
  adoc_of_declaration buf env xref doc_comment declaration

let render_docs : Common.render_input -> string =
 fun Common.{ module_comment; declarations; current_path; lookup_type; _ } ->
  let buf = Buffer.create 1024 in
  bprintf buf "[[module.%s]]\n= %s\n\n" current_path current_path;
  bprintf buf "%s\n\n" module_comment;
  let env = { lookup_type } in
  List.iter (adoc_of_doc buf env) declarations;
  Buffer.contents buf
