(**
 * Normalises a file path
 *)
val normalise : string -> string

(**
 * Makes an absolute path relative to another absolute path.
 *
 * Examples:
 *
 * relative_to "/home/foo" "/home/foo/project" = Some "project"
 * relative_to "/home/foo" "/home/foo/project/lib" = Some "project/lib"
 * relative_to "/home/foo" "/home/bar/project" = None
 *)
val relative_to : string -> string -> string option
