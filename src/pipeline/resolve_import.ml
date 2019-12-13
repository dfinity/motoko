open Mo_def
module Traversals = Mo_frontend.Traversals

(*
This module traverses the syntax tree. For each `import` statement, it looks
at the given relative path and tries to resolve it to a full path (where
full means relative to the current working directory, so that source
directories do not creep into the build output). If no file can be found
there, this prints an error message, otherwise it stores the real path
in the second, mutable field of the ImportE statement.

It returns a list of all imported file names.

*)

(* written as a functor so we can allocate some temporary shared state without making it global *)

type filepath = string

module S = Set.Make(String)
module M = Map.Make(String)

(* a map of type package_map will map each package name to a(n optional) package URL,
   which for now is just a filesystem path:

   e.g.,
   packages("std") = "/Users/home/username/.dfinity-sdk/src/mo-stdlib/0.1.0/"
   packages("foo") = "/Users/home/username/fooPackage/1.2.3/src"
*)

type package_map = string M.t

open Syntax
open Source

type parsed_import =
  | PackageImport of (string * string)
  | RelativeImport of string
  | ActorImport of string

(* decodes `f` according to the following URL patterns:

   parse_import "mo:std/list"    = PackageImport ("std", "list")
   parse_import "mo:std/foo/bar" = PackageImport ("std", "foo/bar")
   parse_import "mo:foo/bar"     = PackageImport ("foo", "bar")
   parse_import "mo:foo"         = PackageImport ("foo", "")

   parse_import "ic:alias"       = ActorImport "alias"
   parse_import "ic:DEADBEEF"    = ActorImport "DEADBEEF"

   parse_import "std/foo"        = RelativeImport "std/foo"
   parse_import "foo"            = RelativeImport "foo"
   parse_import "./foo"          = RelativeImport "foo"

   TODO: This could be the place to reject things like
     ic: mo: http:std/foo
   and also to do proper URL decoding.
*)
let parse_import (f: string) : parsed_import =
  match Lib.String.chop_prefix "mo:" f with
  | Some suffix ->
    begin match String.index_opt suffix '/' with
    | None -> PackageImport (suffix, "")
    | Some i ->
      let pkg = String.sub suffix 0 i in
      let path = String.sub suffix (i+1) (String.length suffix - (i+1)) in
      PackageImport (pkg, path)
    end
  | None ->
    match Lib.String.chop_prefix "ic:" f with
    | Some suffix -> ActorImport suffix
    | None ->
      (* TODO: Check and reject other URL schemas? *)
      match Lib.String.chop_prefix "./" f with
      | Some suffix -> RelativeImport suffix
      | None -> RelativeImport f

let append_lib_if_needed f =
  if Sys.file_exists f && Sys.is_directory f
  then Filename.concat f "lib.mo"
  else f

let err_file_does_not_exist msgs at full_path =
  let open Diag in
  add_msg msgs {
      sev = Error;
      at;
      cat = "import";
      text = Printf.sprintf "File \"%s\" does not exist" full_path
    }

let err_package_not_defined msgs at pkg =
  let open Diag in
  add_msg msgs {
    sev = Error;
    at;
    cat = "import";
    text = Printf.sprintf "Package \"%s\" not defined" pkg
  }

let err_package_file_does_not_exist msgs f pname =
  let open Diag in
  add_msg msgs {
    sev = Error;
    at = no_region;
    cat = "package";
    text = Printf.sprintf "File \"%s\" (for package `%s`) does not exist" f pname
  }

let err_package_already_defined msgs package_name =
  let open Diag in
  Diag.add_msg msgs {
    sev = Error;
    at = no_region;
    cat = "--package";
    text = Printf.sprintf "Package name \"%s\" already defined" package_name;
  }

let add_lib_import msgs imported ri_ref at full_path =
  let full_path = append_lib_if_needed full_path in
  if Sys.file_exists full_path
  then begin
    ri_ref := LibPath full_path;
    imported := S.add full_path !imported
  end else
    err_file_does_not_exist msgs at full_path

let add_idl_import msgs imported ri_ref at full_path =
  ri_ref := IDLPath full_path
  (*
  if Sys.file_exists full_path
  then begin
    ri_ref := IDLPath full_path;
    imported := S.add full_path !imported
  end else
    does_not_exist_error msgs at full_path
  *)



let in_base base f =
  if base = "."
  then f
  else Filename.concat base f

let resolve_import_string msgs base packages imported (f, ri_ref, at)  =
  match parse_import f with
    | RelativeImport path ->
      add_lib_import msgs imported ri_ref at (in_base base path)
    | PackageImport (pkg,path) ->
      begin match M.find_opt pkg packages with
      | Some pkg_path ->
        add_lib_import msgs imported ri_ref at (in_base pkg_path path)
      | None ->
        err_package_not_defined msgs at pkg
      end
    | ActorImport path ->
      let full_path = (* in_base actor_base *) path in
      add_idl_import msgs imported ri_ref at full_path

(* Resolve the argument to --package. These can also be relative to base *)
let resolve_package_url (msgs:Diag.msg_store) (base:filepath) (pname:string) (f: string) : string option =
  let f =
    if Filename.is_relative f
    then in_base base f
    else f in
  let f = File_path.normalise f in
  if Sys.file_exists f then
    Some f
  else
  begin
    err_package_file_does_not_exist msgs f pname;
    None
  end

let prog_imports (p : prog): (string * resolved_import ref * Source.region) list =
  let res = ref [] in
  let f e = match e.it with
    | ImportE (f, fp) -> res := (f, fp, e.at) ::!res; e
    | _ -> e in
  let _ = ignore (Traversals.over_prog f p) in
  List.rev !res

let collect_imports (p : prog): string list =
  List.map (fun (f, _, _) -> f) (prog_imports p)

type package_urls = (string * string) list

let resolve_packages : package_urls -> filepath -> package_map Diag.result = fun purls base ->
  Diag.fold (fun package_map (package_name, package_url) ->
    Diag.with_message_store (fun msgs ->
      if M.mem package_name package_map
      then begin err_package_already_defined msgs package_name; None end
      else match resolve_package_url msgs base package_name package_url with
        | None              -> None
        | Some resolved_url -> Some (M.add package_name resolved_url package_map)
    )
  )
  M.empty purls

let resolve : package_urls -> Syntax.prog -> filepath -> S.t Diag.result = fun purls p base ->
  Diag.bind (resolve_packages purls base) (fun (packages:package_map) ->
    Diag.with_message_store (fun msgs ->
      let base = if Sys.is_directory base then base else Filename.dirname base in
      let imported = ref S.empty in
      List.iter (resolve_import_string msgs base packages imported) (prog_imports p);
      Some !imported
    )
  )
