open Extract
open Printf

let surround : Buffer.t -> string -> (unit -> unit) -> unit =
 fun buf s inner ->
  Buffer.add_string buf s;
  inner ();
  Buffer.add_string buf s

let adoc_of_typ_bind : Buffer.t -> Mo_def.Syntax.typ_bind -> unit =
  Plain.plain_of_typ_bind

let adoc_of_function_arg : Buffer.t -> function_arg_doc -> unit =
 fun buf arg ->
  Buffer.add_string buf arg.name;
  Plain.opt_typ buf arg.typ

let adoc_header : Buffer.t -> int -> string -> unit =
 fun buf lvl s -> bprintf buf "%s %s\n\n" (String.make (lvl + 1) '=') s

let adoc_signature : Buffer.t -> (unit -> unit) -> unit =
 fun buf f ->
  bprintf buf "[source.no-repl,motoko]\n----\n";
  f ();
  bprintf buf "\n----\n\n"

let adoc_of_doc_type : Buffer.t -> Extract.doc_type -> unit =
 fun buf dt ->
  match dt with
  | DTPlain t -> Plain.plain_of_typ buf t
  | DTObj (t, fields) -> Plain.plain_of_typ buf t

let rec adoc_of_declaration :
    Buffer.t -> int -> (unit -> unit) -> declaration_doc -> unit =
 fun buf lvl doc_comment doc ->
  let header = adoc_header buf lvl in
  let signature = adoc_signature buf in
  match doc with
  | Function function_doc ->
      bprintf buf "[[value.%s]]\n" function_doc.name;
      header function_doc.name;
      signature (fun _ ->
          bprintf buf "func %s" function_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            function_doc.type_args;
          bprintf buf "(";
          Plain.sep_by buf ", " (adoc_of_function_arg buf) function_doc.args;
          bprintf buf ")";
          Plain.opt_typ buf function_doc.typ);
      doc_comment ()      
  | Value value_doc ->
      bprintf buf "[[value.%s]]\n" value_doc.name;
      header value_doc.name;
      signature (fun _ ->
          bprintf buf "let %s" value_doc.name;
          Plain.opt_typ buf value_doc.typ);
      doc_comment ()      
  | Type type_doc ->
      bprintf buf "[[type.%s]]\n" type_doc.name;
      header type_doc.name;
      signature (fun _ ->
          bprintf buf "type %s" type_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            type_doc.type_args;
          bprintf buf " = ";
          adoc_of_doc_type buf type_doc.typ);
      doc_comment ()      
  | Class class_doc ->
      bprintf buf "[[class.%s]]\n" class_doc.name;
      header class_doc.name;
      signature (fun _ ->
          bprintf buf "class %s" class_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            class_doc.type_args;
          bprintf buf "(";
          Plain.sep_by buf ", " (adoc_of_function_arg buf) class_doc.constructor;
          bprintf buf ")");
      doc_comment ();      
      bprintf buf "\n\n";
      List.iter (adoc_of_doc buf (lvl + 1)) class_doc.fields
  | Unknown unknown ->
      signature (fun _ -> bprintf buf "Unknown %s" unknown);
      doc_comment ()

and adoc_of_doc : Buffer.t -> int -> doc -> unit =
 fun buf lvl { doc_comment; declaration } ->
  let doc_comment () =
    Option.iter (Buffer.add_string buf) doc_comment;
    bprintf buf "\n\n"
  in
  adoc_of_declaration buf lvl doc_comment declaration

let render_docs : Common.render_input -> string =
 fun Common.{ module_comment; declarations; current_path; _ } ->
  let buf = Buffer.create 1024 in
  bprintf buf "[[module.%s]]\n= %s\n\n" current_path current_path;
  bprintf buf "%s\n\n" module_comment;
  List.iter (adoc_of_doc buf 1) declarations;
  Buffer.contents buf
