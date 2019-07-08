(**
 * Normalises a file path
 *)
val normalise : string -> string

(**
 * Makes one path relative to another path.
 *
 * Examples:
 *
 * relative_to "/home/foo" "/home/foo/project" = Some "project"
 * relative_to "/home/foo" "/home/foo/project/lib" = Some "project/lib"
 * relative_to "/home/foo" "/home/bar/project" = None
 * relative_to "foo/bar" "foo/bar/project" = Some "project"
 *)
val relative_to : string -> string -> string option
