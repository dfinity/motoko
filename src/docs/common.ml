open Mo_def

type render_input = {
  (** Needed so we can generate a navigation *)
  all_modules : string list;
  (** The path for the current module *)
  current_path : string;
  (** The top-level module comment for the current module *)
  module_comment : string;
  (** The list of declarations to process *)
  declarations : Extract.doc list;
}

let is_tuple_type typ =
  match typ.Source.it with
  | Syntax.TupT _ | Syntax.ParT _ -> true
  | _ -> false

let is_type_atom typ =
  match typ.Source.it with
  | Syntax.PathT _ | Syntax.PrimT _ | Syntax.ArrayT _ | Syntax.ParT _
  | Syntax.TupT _ | Syntax.ObjT _ | Syntax.VariantT _ ->
      true
  | Syntax.OptT _ | Syntax.FuncT _ | Syntax.AsyncT _ -> false

let is_scope_bind : Syntax.typ_bind -> bool =
 fun typ_bind ->
  match typ_bind.Source.it.Syntax.sort.Source.it with
  | Mo_types.Type.Scope -> true
  | _ -> false
