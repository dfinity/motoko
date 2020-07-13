open Mo_def

type render_input = {
  all_modules : string list;  (** Needed so we can generate a navigation *)
  current_path : string;  (** The path for the current module *)
  module_comment : string;
      (** The top-level module comment for the current module *)
  declarations : Extract.doc list;  (** The list of declarations to process *)
}

(** Does a given type need to be parenthesized? *)
let is_type_atom typ =
  match typ.Source.it with
  | Syntax.PathT _ | Syntax.PrimT _ | Syntax.ArrayT _ | Syntax.ParT _
  | Syntax.TupT _ | Syntax.ObjT _ | Syntax.VariantT _ | Syntax.NamedT _ ->
      true
  | Syntax.OptT _ | Syntax.FuncT _ | Syntax.AsyncT _ -> false

let is_scope_bind : Syntax.typ_bind -> bool =
 fun typ_bind ->
  match typ_bind.Source.it.Syntax.sort.Source.it with
  | Mo_types.Type.Scope -> true
  | _ -> false
