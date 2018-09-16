val compile : Syntax.prog -> Wasm.Ast.module_
val print_wat : Wasm.Ast.module_ -> unit
val string_of_wat : Wasm.Ast.module_ -> string
