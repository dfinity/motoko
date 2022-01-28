open Ir
open Source
let rec is_irrefutable p = match p.it with
  | TupP pats -> List.for_all is_irrefutable pats
  | ObjP pfs -> List.for_all (fun (pf : pat_field) -> is_irrefutable pf.it.pat) pfs
  | AltP (pat1, _) -> is_irrefutable pat1
  | WildP | VarP _ -> true
  | TagP _ | LitP _ | OptP _ -> false


let expected_irrefutable : dec -> bool = function
  | { it = LetD (_, { it = VarE id; _ }); _ } ->
    Lib.String.starts_with "file$" id
  | _ -> false
