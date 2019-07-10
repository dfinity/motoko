open As_def

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
   packages("std") = "/Users/home/username/.dfinity-sdk/src/as-stdlib/0.1.0/"
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

(* match `f` against the URL pattern 'as:package-name/path',
   optionally returning the package-name and path components as a pair of strings. *)
let match_package_name (f: string) : (string * string) option =
  let rec loop (f: string) (path_accum:string) : (string * string) option =
    (* loop recursively until we get to a 'self-loop', indicated by dirname '.' ;
       in each iteration, we use function pair (dirname, basename)
       to decompose the filename.
     *)
    let (dir, base) = (Filename.dirname f, Filename.basename f) in
    match dir with
    | "." -> Some (f, path_accum)
    | _   -> begin
        assert ((Filename.concat dir base) = f) ;
        loop dir (Filename.concat base path_accum)
      end
  in
  let (prefix, suffix) = (
      String.sub f 0 3,
      String.sub f 3 ((String.length f) - 3)
    )
  in
  match prefix with
  | "as:" -> loop suffix ""
  | _     -> None

(* using env, resolve import strings of the form "as:package-name/mod1/mod2/item"
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
    | Some (_, url, path) -> Filename.concat url path
  in
  let f =
    if Filename.is_relative f
    then Filename.concat env.base f
    else f in
  let f =
    if Sys.file_exists f && Sys.is_directory f
    then Filename.concat f "lib.as"
    else f in
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
   the two import-string to filesystem-path resolution semantics agree for now,
   but other API details and usage are distinct.
 *)
let resolve_package_url (msgs:Diag.msg_store) (base:filepath) (pname:string) (f: string) : string option =
  let f =
    if Filename.is_relative f
    then Filename.concat base f
    else f in
  let f =
    if Sys.file_exists f && Sys.is_directory f
    then Filename.concat f "lib.as"
    else f in
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

let rec
  exp env (e : exp) = match e.it with
  | ImportE (f, fp) -> resolve_import_string env e.at f fp
  (* The rest is just a boring syntax traversal *)
  | (PrimE _ | VarE _ | LitE _) -> ()
  | UnE (_, _, exp1)
  | ShowE (_, exp1)
  | ProjE (exp1, _)
  | OptE exp1
  | TagE (_, exp1)
  | DotE (exp1, _)
  | NotE exp1
  | AssertE exp1
  | LabelE (_, _, exp1)
  | BreakE (_, exp1)
  | RetE exp1
  | AnnotE (exp1, _)
  | AsyncE exp1
  | AwaitE exp1
  | LoopE (exp1, None) ->
    exp env exp1
  | BinE (_, exp1, _, exp2)
  | IdxE (exp1, exp2)
  | RelE (_, exp1, _, exp2)
  | AssignE (exp1, exp2)
  | CallE (exp1, _, exp2)
  | AndE (exp1, exp2)
  | OrE (exp1, exp2)
  | WhileE (exp1, exp2)
  | LoopE (exp1, Some exp2)
  | ForE (_, exp1, exp2) ->
    exp env exp1; exp env exp2
  | TupE exps
  | ArrayE (_, exps) ->
    List.iter (exp env) exps
  | BlockE ds ->
    decs env ds
  | ObjE (_, efs) ->
    List.iter (fun ef -> dec env ef.it.dec) efs
  | IfE (exp1, exp2, exp3) ->
    exp env exp1;
    exp env exp2;
    exp env exp3
  | SwitchE (exp1, cases) ->
    exp env exp1;
    List.iter (fun c -> exp env c.it.exp) cases
  | FuncE (_, _, _ , _ , _, e) ->
    exp env e

and decs env = List.iter (dec env)
and dec env d = match d.it with
  | TypD _ -> ()
  | ExpD e
  | VarD (_, e)
  | LetD (_, e) ->
    exp env e
  | ClassD (_, _, _, _ , _, efs) ->
    List.iter (fun ef -> dec env ef.it.dec) efs

let prog env p = decs env p.it

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
