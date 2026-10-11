open Mo_types

type arguments = Shared | Fixed of Type.typ list

type specialization = {
  type_arguments : Type.typ list;
  argument_types : Type.typ list;
  result_type : Type.typ;
}

type binding = {
  id : string;
  name : string;
  arguments : arguments;
  type_parameters : int;
  result_type : Type.typ list -> Type.typ list -> (Type.typ, string) result;
  evaluate : Type.typ list -> Type.typ list -> Value.value list ->
    (Value.value, string) result;
}

let bindings : binding list ref = ref []

let identifier s =
  String.length s > 0 && s <> "_" &&
  String.for_all (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false) s &&
  (match s.[0] with '0' .. '9' -> false | _ -> true)

let register binding =
  if not (identifier binding.name) || not (identifier binding.id) then
    invalid_arg "invalid compute plugin binding identifier";
  if binding.type_parameters < 0 then
    invalid_arg "negative compute plugin type parameter count";
  if List.exists (fun b -> b.id = binding.id || b.name = binding.name)
      !bindings then
    invalid_arg ("duplicate compute plugin binding: " ^ binding.name);
  bindings := !bindings @ [binding]

let find id = List.find_opt (fun b -> b.id = id) !bindings

let intrinsic_type id =
  Type.Func (Type.Local, Type.Intrinsic id, [], [], [])

let intrinsic typ = match Type.promote typ with
  | Type.Func (Type.Local, Type.Intrinsic id, [], [], []) ->
    Some id
  | _ -> None

let declarations () =
  String.concat "\n" (List.map (fun b ->
    Printf.sprintf "let %s = prim %S;" b.name ("compute:" ^ b.id)) !bindings)

let check_arguments binding types = match binding.arguments, types with
  | Shared, ts when List.for_all Type.shared ts -> Ok ()
  | Shared, _ -> Error (binding.name ^ " requires shared argument types")
  | Fixed expected, ts when List.length ts = List.length expected &&
      List.for_all2 (fun t expected -> Type.sub t expected) ts expected -> Ok ()
  | Fixed expected, _ -> Error (Printf.sprintf "%s expects arguments of type %s"
      binding.name (Type.string_of_typ (Type.seq expected)))

(* Unlike Type.concrete this also handles incomplete plugin-provided types
   without assertions. Plugins must specialize to a known runtime layout. *)
module Types = Set.Make (Type.Ord)

let concrete typ =
  let seen = ref Types.empty in
  let rec go typ =
    Types.mem typ !seen || begin
      seen := Types.add typ !seen;
      match typ with
      | Type.Var _ | Type.Pre -> false
      | Type.Prim _ | Type.Any | Type.Non -> true
      | Type.Con (con, ts) ->
        (match Cons.kind con with
         | Type.Abs _ -> false
         | Type.Def (bs, body) ->
           List.length bs = List.length ts && go (Type.open_ ts body))
      | Type.Array t | Type.Opt t | Type.Mut t | Type.Weak t
      | Type.Named (_, t) -> go t
      | Type.Async (_, _, t) -> go t
      | Type.Tup ts -> List.for_all go ts
      | Type.Obj (_, fs, _) | Type.Variant fs ->
        List.for_all (fun f -> go f.Type.typ) fs
      | Type.Func (_, _, bs, ins, outs) ->
        let ts = Type.open_binds bs in
        List.for_all (fun t -> go (Type.open_ ts t)) (ins @ outs)
    end
  in go typ

let specialize binding type_arguments argument_types =
  if List.length type_arguments <> binding.type_parameters then
    Error (Printf.sprintf "%s expects %d explicit type argument%s"
      binding.name binding.type_parameters
      (if binding.type_parameters = 1 then "" else "s"))
  else if not (List.for_all concrete type_arguments) then
    Error (binding.name ^ " requires concrete type arguments")
  else if not (List.for_all concrete argument_types) then
    Error (binding.name ^ " requires concrete argument types")
  else match check_arguments binding argument_types with
  | Error message -> Error message
  | Ok () ->
    match binding.result_type type_arguments argument_types with
    | Error message -> Error message
    | Ok result_type when concrete result_type ->
      Ok {type_arguments; argument_types; result_type}
    | Ok _ -> Error (binding.name ^ " requires a concrete result type")

let map_specialization f s = {
  type_arguments = List.map f s.type_arguments;
  argument_types = List.map f s.argument_types;
  result_type = f s.result_type;
}

let evaluate id s values = match find id with
  | None -> Error ("compute plugin is not loaded: " ^ id)
  | Some binding ->
    match specialize binding s.type_arguments s.argument_types with
    | Error e -> Error e
    | Ok checked ->
      if not (Type.eq checked.result_type s.result_type) ||
        List.length s.argument_types <> List.length values then
        Error (binding.name ^ " received inconsistent argument metadata")
      else
        binding.evaluate s.type_arguments s.argument_types values
