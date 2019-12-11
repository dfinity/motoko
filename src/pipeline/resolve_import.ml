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

type env = {
  msgs : Diag.msg_store;
  base : filepath;
  packages : package_map;
  imported : S.t ref;
}

open Syntax
open Source

(* match `f` against the URL pattern 'mo://package-name/path',
   optionally returning the package-name and path components as a pair of strings.

   e.g.,
   match_package_name "mo:std/list"    = Some("std", "list")
   match_package_name "mo:std/foo/bar" = Some("std", "foo/bar")
   match_package_name "mo:foo/bar"     = Some("foo", "bar")

   match_package_name "mo:"            = None
   match_package_name "mo:std"         = None
   match_package_name "std/foo"        = None
   match_package_name "std/foo/bar"    = None

*)
let match_package_name (f: string) : (string * string) option =
  let rec loop (f: string) (path_accum:string) : (string * string) option =
    let (dir, base) = (Filename.dirname f, Filename.basename f) in
    match dir with
    | "." -> if path_accum = "" then None else Some (f, path_accum)
    | _   ->
      let path_accum =
        match path_accum with
        | "" -> base
        | _  -> Filename.concat base path_accum
      in
      loop dir path_accum
  in
  if String.length f < 3 then None else
    let (prefix, suffix) = (
        String.sub f 0 3,
        String.sub f 3 ((String.length f) - 3)
      )
    in
    match prefix with
    | "mo:" -> loop suffix ""
    | _     -> None

(* using env, resolve import strings of the form "mo:package-name/mod1/mod2/item"
   into the triple ("package-name", "package-url", "mod1/mod2/item") when the package name is defined.
   Does not validate the package url or path.
 *)
let resolve_package env (f: string) : (string * string * string) option =
  match match_package_name f with
  | None -> None
  | Some (name, path) ->
    if M.mem name env.packages then
      Some (name, M.find name env.packages, path)
    else
      None

let resolve_import_string env region (f: string) (fp: string ref) =
  let f =
    match resolve_package env f with
    | None -> f
    | Some (_pname, url, path) -> Filename.concat url path
  in
  let f =
    if Filename.is_relative f
    then Filename.concat env.base f
    else f in
  let f =
    if Sys.file_exists f && Sys.is_directory f
    then Filename.concat f "lib.mo"
    else f in
  let f = File_path.normalise f in
  if Sys.file_exists f
  then begin
      fp := f;
      env.imported := S.add f !(env.imported)
    end else
    let open Diag in
    add_msg env.msgs {
        sev = Error;
        at = region;
        cat = "import";
        text = Printf.sprintf "File \"%s\" does not exist" f
      }

(* compare to the filesystem semantics of function `resolve_import_string`:
   the import-string to filesystem-path resolution semantics are related, but distinct.
 *)
let resolve_package_url (msgs:Diag.msg_store) (base:filepath) (pname:string) (f: string) : string option =
  let f =
    if Filename.is_relative f
    then Filename.concat base f
    else f in
  let f = File_path.normalise f in
  if Sys.file_exists f then
    Some f
  else
    let open Diag in
    add_msg msgs {
        sev = Error;
        at = no_region;
        cat = "package";
        text = Printf.sprintf "File \"%s\" (for package `%s`) does not exist" f pname
      };
    None

let collect_imports (p : prog): string list =
  let res = ref [] in
  let f e = match e.it with
    | ImportE (f, _) -> res := f::!res; e
    | _ -> e in
  let _ = ignore (Traversals.over_prog f p) in
  List.rev !res

let prog env p =
  let f e = match e.it with
    | ImportE (f, fp) -> resolve_import_string env e.at f fp; e
    | _ -> e in
  ignore (Traversals.over_prog f p)

type package_urls = (string * string) list

let resolve_packages : package_urls -> filepath -> package_map Diag.result = fun purls base ->
  Diag.fold (fun package_map (package_name, package_url) ->
      if M.mem package_name package_map then
        Diag.with_message_store (fun msgs ->
            let open Diag in
            Diag.add_msg msgs {
                sev = Error;
                at = no_region;
                cat = "--package";
                text = Printf.sprintf "Package name \"%s\" already defined" package_name;
              };
            None
          )
      else
        Diag.with_message_store (fun msgs ->
            match resolve_package_url msgs base package_name package_url with
            | None              -> None
            | Some resolved_url -> Some (M.add package_name resolved_url package_map)
          )
    )
    M.empty purls

let resolve : package_urls -> Syntax.prog -> filepath -> S.t Diag.result = fun purls p base ->
  Diag.bind (resolve_packages purls base) (fun (packages:package_map) ->
      Diag.with_message_store (fun msgs ->
          let base = if Sys.is_directory base then base else Filename.dirname base in
          let env = { msgs; base; imported = ref S.empty; packages } in
          prog env p;
          Some !(env.imported)
        )
    )
