open Parser.MenhirInterpreter

(* In order to print syntax error messages and/or debugging information, we
   need a symbol printer. *)

let abstract abs con = abs

let binop = abstract "<binop>"
let relop = abstract "<relop>"
let binassign = abstract "<binassign>"

(* all unary operators are also binary operators, so keep them unary *)
let unop = abstract "<unop>"
let unassign = abstract "<unassign>"


let string_of_symbol symbol : string =
  match symbol with
  | X (T T_error) -> "error"
  | X (T T_XOROP) -> unop "^"
  | X (T T_XORASSIGN) -> unassign "^="
  | X (T T_WHILE) -> "while"
  | X (T T_VAR) -> "var"
  | X (T T_USHROP) -> binop " >>"
  | X (T T_USHRASSIGN) -> binop ">>="
  | X (T T_UNDERSCORE) -> "_"
  | X (T T_TYPE) -> "type"
  | X (T T_TRY) -> "try"
  | X (T T_THROW) -> "throw"
  | X (T T_TEXT) -> "<text>"
  | X (T T_SWITCH) -> "switch"
  | X (T T_SUBOP) -> unop "-"
  | X (T T_SUB) -> "<:"
  | X (T T_STABLE) -> "stable"
  | X (T T_SSHROP) -> binop "+>>"
  | X (T T_SSHRASSIGN) -> binassign "+>>="
  | X (T T_SHLOP) -> binop "<<"
  | X (T T_SHLASSIGN) -> binassign "<<="
  | X (T T_SHARED) -> "shared"
  | X (T T_SEMICOLON_EOL) -> ";" (* suppress the \n *)
  | X (T T_SEMICOLON) -> ";"
  | X (T T_SYSTEM) -> "system"
  | X (T T_RPAR) -> ")"
  | X (T T_ROTROP) -> binop "<>>"
  | X (T T_ROTRASSIGN) -> binassign "<>>="
  | X (T T_ROTLOP) -> binop "<<>"
  | X (T T_ROTLASSIGN) -> binassign "<<>="
  | X (T T_RETURN) -> "return"
  | X (T T_RCURLY) -> "}"
  | X (T T_RBRACKET) -> "]"
  | X (T T_QUEST) -> "?"
  | X (T T_QUERY) -> "query"
  | X (T T_PUBLIC) -> "public"
  | X (T T_PRIVATE) -> "private"
  | X (T T_PRIM) -> "prim"
  | X (T T_POWOP) -> binop "**"
  | X (T T_POWASSIGN) -> binassign "**-"
  | X (T T_PLUSASSIGN) -> unassign "+="
  | X (T T_OROP) -> binop "|"
  | X (T T_ORASSIGN) -> binassign "|="
  | X (T T_OR) -> "or"
  | X (T T_OBJECT) -> "object"
  | X (T T_NULL) -> "null"
  | X (T T_NOT) -> "not"
  | X (T T_NEQOP) -> binop "!="
  | X (T T_NAT) -> "<nat>"
  | X (T T_MULOP) -> binop "*"
  | X (T T_MULASSIGN) -> binassign "*="
  | X (T T_MODULE) -> "module"
  | X (T T_MODOP) -> binop "%"
  | X (T T_MODASSIGN) -> binassign "%="
  | X (T T_MINUSASSIGN) -> unassign "-="
  | X (T T_LTOP) -> relop " < "
  | X (T T_LT) -> "<"
  | X (T T_LPAR) -> "("
  | X (T T_LOOP) -> "loop"
  | X (T T_LET) -> "let"
  | X (T T_LEOP) -> relop "<="
  | X (T T_LCURLY) -> "{"
  | X (T T_LBRACKET) -> "["
  | X (T T_LABEL) -> "label"
  | X (T T_IN) -> "in"
  | X (T T_IMPORT) -> "import"
  | X (T T_IGNORE) -> "ignore"
  | X (T T_IF) -> "if"
  | X (T T_ID) -> "<id>"
  | X (T T_HASH) -> binop "#"
  | X (T T_GTOP) -> relop " > "
  | X (T T_GT) -> ">"
  | X (T T_GEOP) -> relop ">="
  | X (T T_FUNC) -> "func"
  | X (T T_FOR) -> "for"
  | X (T T_FLEXIBLE) -> "flexible"
  | X (T T_FLOAT) -> "<float>"
  | X (T T_EQOP) -> relop "=="
  | X (T T_EQ) -> "="
  | X (T T_EOF) -> "<eof>"
  | X (T T_ELSE) -> "else"
  | X (T T_DOT_NUM) -> ".<nat>"
  | X (T T_DOT) -> "."
  | X (T T_DIVOP) -> binop "/"
  | X (T T_DIVASSIGN) -> binassign "/="
  | X (T T_DEBUG_SHOW) -> "debug_show"
  | X (T T_DEBUG) -> "debug"
  | X (T T_CONTINUE) -> "continue"
  | X (T T_COMMA) -> ","
  | X (T T_COLON) -> ":"
  | X (T T_CLASS) -> "class"
  | X (T T_CHAR) ->  "<char>"
  | X (T T_CATCH) -> "catch"
  | X (T T_CATASSIGN) -> binassign "@="
  | X (T T_CASE) -> "case"
  | X (T T_BREAK) -> "break"
  | X (T T_BOOL) -> "<bool>"
  | X (T T_AWAIT) -> "await"
  | X (T T_ASYNC) -> "async"
  | X (T T_ASSIGN) -> binassign "assign"
  | X (T T_ASSERT) -> "assert"
  | X (T T_ARROW) -> "->"
  | X (T T_ANDOP) -> binop "&"
  | X (T T_ANDASSIGN) -> binassign "&="
  | X (T T_AND) -> "and"
  | X (T T_ADDOP) -> unop "+"
  | X (T T_ACTOR) -> "actor"
  (* non-terminals *)
  | X (N N_bl) -> "<bl>"
  | X (N N_case) -> "<case>"
  | X (N N_catch) -> "<catch>"
  | X (N N_class_body) -> "<class_body>"
  | X (N N_dec) -> "<dec>"
  | X (N N_dec_field) -> "<dec_field>"
  | X (N N_dec_nonvar) -> "<dec_nonvar>"
  | X (N N_dec_var) -> "<dec_var>"
  | X (N N_exp_bl_) -> "<exp(bl)>"
  | X (N N_exp_ob_) -> "<exp(ob)>"
  | X (N N_exp_bin_bl_) -> "<exp_bin(bl)>"
  | X (N N_exp_bin_ob_) -> "<exp_bin(ob)>"
  | X (N N_exp_block) -> "<exp_block>"
  | X (N N_exp_field) -> "<exp_field>"
  | X (N N_exp_nondec_bl_) -> "<exp_nondec(bl)>"
  | X (N N_exp_nondec_ob_) -> "<exp_nondec(ob)>"
  | X (N N_exp_nonvar_bl_) -> "<exp_nonvar(bl)>"
  | X (N N_exp_nonvar_ob_) -> "<exp_nonvar(ob)>"
  | X (N N_exp_nullary_bl_) -> "<exp_nullary(bl)>"
  | X (N N_exp_nullary_ob_) -> "<exp_nullary(ob)>"
  | X (N N_exp_plain) -> "<exp_plain>"
  | X (N N_exp_post_bl_) -> "<exp_post(bl)>"
  | X (N N_exp_post_ob_) -> "<exp_post(ob)>"
  | X (N N_exp_un_bl_) -> "<exp_un(bl)>"
  | X (N N_exp_un_ob_) -> "<exp_un(ob)>"
  | X (N N_func_body) -> "<func_body>"
  | X (N N_imp) -> "<imp>"
  | X (N N_import_list) -> "<import_list>"
  | X (N N_inst) -> "<inst>"
  | X (N N_lit) -> "<lit>"
  | X (N N_ob) -> "<ob>"
  | X (N N_obj_body) -> "<obj_body>"
  | X (N N_option_EQ_) -> "=?"
  | X (N N_option_exp_nullary_ob__) -> "<exp_nullary(ob)>?"
  | X (N N_option_typ_args_) -> "<typ_args>?"
  | X (N N_parse_module_header) -> "<parse_module_header>"
  | X (N N_parse_prog) -> "<parse_prog>"
  | X (N N_parse_prog_interactive) -> "<parse_prog_interactive>"
  | X (N N_pat) -> "<pat>"
  | X (N N_pat_bin) -> "<pat_bin>"
  | X (N N_pat_field) -> "<pat_field>"
  | X (N N_pat_nullary) -> "<pat_nullary>"
  | X (N N_pat_plain) -> "<pat_plain>"
  | X (N N_pat_un) -> "<pat_un>"
  | X (N N_path) -> "<path>"
  | X (N N_annot_opt) -> "<annot_opt>"
  | X (N N_seplist_case_semicolon_) ->  "seplist(<case>,<semicolon>)"
  | X (N N_seplist_dec_SEMICOLON_) -> "seplist(<dec>,;)"
  | X (N N_seplist_dec_semicolon_) -> "seplist(<dec>,<semicolon>)"
  | X (N N_seplist_dec_field_semicolon_) -> "seplist(<dec_field>,<semicolon>)"
  | X (N N_seplist_dec_var_semicolon_) -> "seplist(<dec_var>,<semicolon>)"
  | X (N N_seplist_exp_ob__COMMA_) -> "seplist(<exp(ob)>,,)"
  | X (N N_seplist_exp_field_semicolon_) -> "seplist(<exp_field>,<semicolon>)"
  | X (N N_seplist_exp_nonvar_ob__COMMA_) -> "seplist(<exp_nonvar(ob),,)"
  | X (N N_seplist_imp_SEMICOLON_) -> "seplist(<imp>,;)"
  | X (N N_seplist_imp_semicolon_) -> "seplist(<imp>,<semicolon>)"
  | X (N N_seplist_pat_bin_COMMA_) -> "seplist(<pat_bin>,,)"
  | X (N N_seplist_pat_field_semicolon_) -> "seplist(<pat_field>,<semicolon>)"
  | X (N N_seplist_typ_COMMA_) -> "seplist(<typ>,,)"
  | X (N N_seplist_typ_bind_COMMA_) -> "seplist(<typ_bind>,,)"
  | X (N N_seplist_typ_field_semicolon_) -> "seplist(<typ_field>,<semicolon>)"
  | X (N N_seplist_typ_item_COMMA_) -> "seplist(<typ_item>,,)"
  | X (N N_seplist_typ_tag_semicolon_) -> "seplist(<typ_tag>,<semicolon>)"
  | X (N N_seplist1_typ_tag_semicolon_) -> "seplist1(<typ_tag>,<semicolon>)"
  | X (N N_pat_opt) -> "<pat_opt>"
  | X (N N_typ) -> "<typ>"
  | X (N N_typ_args) -> "<typ_args>"
  | X (N N_typ_bind) -> "<typ_bind>"
  | X (N N_typ_field) -> "<typ_field>"
  | X (N N_typ_item) -> "<typ_item>"
  | X (N N_typ_nullary) -> "<typ_nullary>"
  | X (N N_typ_obj) -> "<typ_obj>"
  | X (N N_typ_pre) -> "<typ_pre>"
  | X (N N_typ_tag) -> "<typ_tag>"
  | X (N N_typ_un) -> "<typ_un>"
  | X (N N_typ_variant) -> "<typ_variant>"
  | X (N N_vis) -> "<vis>"
  | X (N N_stab) -> "<stab>"
  | X (N N_start) -> "<start>" (* dummy non-terminal, don't display *)
  | X (N N_deprecated_exp_nullary_bl_) -> "<deprecated_exp_nullary(bl)>"
  | X (N N_deprecated_exp_nullary_ob_) -> "<deprecated_exp_nullary(ob)>"
  | X (N N_deprecated_pat_nullary) -> "<deprecated_pat_nullary>"
  | X (N N_deprecated_exp_field_unamb) -> "<deprecated_exp_field_unamb>"
  | X (N N_deprecated_exp_field_list_unamb) -> "<deprecated_exp_field_list_unamb>"
  | X (N N_deprecated_dec_list_unamb) -> "<deprecated_dec_list_unamb>"

(* In order to print a view of the stack that includes semantic values,
   we need an element printer. (If we don't need this feature, then
   [print_symbol] above suffices.) *)

(* The public functions. *)

let buff :string list ref = ref []
let print s = buff := s::!buff
let print_symbol s = print (string_of_symbol s)
let print_element = None
let to_string() = let s = String.concat "" (List.rev (!buff)) in
                  buff := [];
                  s
