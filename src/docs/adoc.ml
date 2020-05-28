open Extract
open Mo_def
open Printf

let surround buf s inner =
  Buffer.add_string buf s;
  inner ();
  Buffer.add_string buf s

let adoc_of_typ_bind = Plain.plain_of_typ_bind

let adoc_of_function_arg : Buffer.t -> function_arg_doc -> unit =
 fun buf arg ->
  Buffer.add_string buf arg.name;
  Plain.opt_typ buf arg.typ

let adoc_declaration_header : Buffer.t -> int -> (unit -> unit) -> unit =
 fun buf lvl f ->
  Buffer.add_string buf (String.make (lvl + 3) '=');
  bprintf buf " ";
  surround buf "`" (fun _ -> surround buf "+" f)

let rec adoc_of_declaration : Buffer.t -> int -> declaration_doc -> unit =
 fun buf lvl doc ->
  let header = adoc_declaration_header buf lvl in
  match doc with
  | Function function_doc ->
      header (fun _ ->
          bprintf buf "func %s" function_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            function_doc.type_args;
          bprintf buf "(";
          Plain.sep_by buf ", " (adoc_of_function_arg buf) function_doc.args;
          bprintf buf ")";
          Plain.opt_typ buf function_doc.typ)
  | Value value_doc ->
      header (fun _ ->
          bprintf buf "let %s" value_doc.name;
          Plain.opt_typ buf value_doc.typ)
  | Type type_doc ->
      header (fun _ ->
          bprintf buf "type %s" type_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            type_doc.type_args;
          bprintf buf " = ";
          Plain.plain_of_doc_typ buf type_doc.typ)
  | Class class_doc ->
      header (fun _ ->
          bprintf buf "class %s" class_doc.name;
          Plain.sep_by' buf "<" ", " ">" (adoc_of_typ_bind buf)
            class_doc.type_args);
      bprintf buf "\n\n";
      List.iter (adoc_of_doc buf (lvl + 1)) class_doc.fields
  | Unknown unknown -> header (fun _ -> bprintf buf "Unknown %s" unknown)

and adoc_of_doc : Buffer.t -> int -> doc -> unit =
 fun buf lvl { doc_comment; declaration } ->
  adoc_of_declaration buf lvl declaration;
  surround buf "\n\n" (fun _ -> Option.iter (Buffer.add_string buf) doc_comment)

let render_docs : string -> doc list -> string =
 fun module_docs docs ->
  let buf = Buffer.create 1024 in
  bprintf buf "%s\n\n" module_docs;
  List.iter (adoc_of_doc buf 1) docs;
  Buffer.contents buf
