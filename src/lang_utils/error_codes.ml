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
    "M0043", None; (* Type parameter has cyclic bounds *)
    "M0044", None; (* Duplicate name in type parameter list *)
    "M0045", None; (* Wrong number of type arguments *)
    "M0046", None; (* Type argument does not match parameter bound *)
    "M0047", None; (* Send capability required *)
    "M0048", None; (* Literal out of range *)
    "M0049", None; (* String literal is not valid utf8 *)
    "M0050", None; (* Literal does not have expected type *)
    "M0051", None; (* Duplicate definition in block *)
    "M0052", None; (* Duplicate definition in object *)
    "M0053", None; (* Cannot infer expression in class using forward type reference *)
    "M0054", None; (* Cannot infer type of primitive *)
    "M0055", None; (* Cannot infer type of forward variable *)
    "M0056", None; (* Variable is not available in compiled code *)
    "M0057", None; (* Unbound variable *)
    "M0058", None; (* No type can be inferred for actor reference *)
    "M0059", None; (* Operator is not defined for operand type *)
    "M0060", None; (* Operator is not defined for operand types *)
    "M0061", None; (* Comparing abstract type to itself at supertype *)
    "M0062", None; (* Comparing incompatible type at common supertype *)
    "M0063", None; (* Show is not defined for operand type *)
    "M0064", None; (* Misplaced '!' without enclosing do block *)
    "M0065", None; (* Expected option type before '!' *)
    "M0066", None; (* Tuple projection is out of bounds *)
    "M0067", None; (* Expected tuple type *)
    "M0068", None; (* Actors are not supported *)
    "M0069", None; (* Non-toplevel actor *)
  ]
