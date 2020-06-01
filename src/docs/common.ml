open Mo_def

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
