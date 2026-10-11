open Mo_types

type arguments = Blob | Shared

type binding = {
  id : string;
  name : string;
  arguments : arguments;
  evaluate : Type.typ list -> Value.value list -> (string, string) result;
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
  if List.exists (fun b -> b.id = binding.id || b.name = binding.name)
      !bindings then
    invalid_arg ("duplicate compute plugin binding: " ^ binding.name);
  bindings := !bindings @ [binding]

let find id = List.find_opt (fun b -> b.id = id) !bindings

let intrinsic_type id =
  Type.Func (Type.Local, Type.Intrinsic id, [], [], [Type.blob])

let intrinsic typ = match Type.promote typ with
  | Type.Func (Type.Local, Type.Intrinsic id, [], [], [Type.Prim Type.Blob]) ->
    Some id
  | _ -> None

let declarations () =
  String.concat "\n" (List.map (fun b -> match b.arguments with
    | Shared -> Printf.sprintf "let %s = prim %S;" b.name
        ("compute:" ^ b.id)
    | Blob -> Printf.sprintf
        "func %s(x : Blob) : Blob { (prim %S : Blob -> Blob)(x) };"
        b.name ("compute:" ^ b.id)) !bindings)

let check binding types = match binding.arguments, types with
  | Blob, [t] when Type.sub t Type.blob -> Ok ()
  | Blob, _ -> Error (binding.name ^ " expects one Blob argument")
  | Shared, ts when List.for_all Type.shared ts -> Ok ()
  | Shared, _ -> Error (binding.name ^ " requires shared argument types")

let evaluate id types values = match find id with
  | None -> Error ("compute plugin is not loaded: " ^ id)
  | Some binding ->
    match check binding types with
    | Error e -> Error e
    | Ok () ->
      if List.length types <> List.length values then
        Error (binding.name ^ " received inconsistent argument metadata")
      else
        binding.evaluate types values
