(* A list of all error codes used in the compiler. The second field in
   each tuple represents a possible long-form description of the
   error. *)
let error_codes : (string * string option) list =
  [
    "M0001", None; (* Parsing errors *)
    "M0002", None; (* Lexer errors *)
    "M0003", None; (* Self-import *)
    "M0004", None; (* IDL file doesn't define a service *)
    "M0005", None; (* Case mismatch between import and filename *)
    "M0006", None; (* Failed to parse import URL *)
    "M0007", None; (* Failed to parse actor alias *)
    "M0008", None; (* Actor import without IDL path *)
    "M0009", None; (* File not found for import *)
    "M0010", None; (* Imported package was not defined *)
    "M0011", None; (* Imported canister alias was not defined *)
    "M0012", None; (* File for package does not exist *)
    "M0013", None; (* Tried to map the prim package *)
    "M0014", None; (* Non-static expression in library or module *)
    "M0015", None; (* Non-trivial pattern in static expression *)
    "M0016", None; (* Usage before definition *)
    "M0017", None; (* Duplicate binding in pattern *)
    "M0018", None; (* Duplicate name in type *)
    "M0019", None; (* Colliding hashes in type *)
    "M0020", None; (* Unresolved import *)
    "M0021", None; (* Cannot infer type of forward import *)
    "M0022", None; (* Imported file not loaded *)
    "M0023", None; (* Path expression is not a module, object, or actor *)
    "M0024", None; (* Cannot infer type of forward variable *)
    "M0025", None; (* Unavailable variable *)
    "M0026", None; (* Unbound variable *)
    "M0027", None; (* Cannot infer type of forward field reference *)
    "M0028", None; (* Field does not exist in type *)
    "M0029", None; (* Unbound type *)
    "M0030", None; (* Type field does not exist in type *)
    "M0031", None; (* Shared function has non-shared parameter type *)
    "M0032", None; (* Shared function has non-shared return type *)
    "M0033", None; (* Async has non-shared content type *)
    "M0034", None; (* Shared constructor has non-shared parameter type *)
    "M0035", None; (* Invalid return type for shared function *)
    "M0036", None; (* Invalid return type for shared query function *)
    "M0037", None; (* Misplaced async expression *)
    "M0038", None; (* Misplaced await *)
    "M0039", None; (* Misplaced try/throw/catch *)
    "M0040", None; (* Unknowwn primitive type *)
    "M0041", None; (* Shared function has non-async result type *)
    "M0042", None; (* Actor field must have shared function type *)
  ]
