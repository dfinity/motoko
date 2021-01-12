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
  ]
