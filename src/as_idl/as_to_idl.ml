open As_types
open As_types.Type
open Source   
open Printf
module I = Idllib.Syntax

type label = Nat of Lib.Uint32.t | Id of string

let dec_set = ref ConSet.empty
                                       
let unescape lab : label =
  let len = String.length lab in
  try if lab.[len-1] = '_' then begin
          if lab.[0] = '_' then 
            Nat (Lib.Uint32.of_string lab)
          else Id (String.sub lab 0 (len-1))
        end else Id lab
  with _ ->
    if len >= 2 && lab.[len-1] = '_'
    then Id (String.sub lab 0 (len-1))
    else Id lab
       
let prim p =
  match p with
  | Null -> I.Null
  | Bool -> I.Bool
  | Nat -> I.Nat
  | Nat8 -> I.Nat8
  | Nat16 -> I.Nat16
  | Nat32 -> I.Nat32
  | Nat64 -> I.Nat64
  | Int -> I.Int
  | Int8 -> I.Int8
  | Int16 -> I.Int16
  | Int32 -> I.Int32
  | Int64 -> I.Int64
  | Word8 -> I.Nat8
  | Word16 -> I.Nat16
  | Word32 -> I.Nat32
  | Word64 -> I.Nat64
  | Float -> I.Float64
  | Char -> I.Nat32
  | Text -> I.Text
  
let rec typ env t =
  (match t with
  | Pre -> assert false
  | Any -> I.PrimT I.Reserved
  | Non -> I.PrimT I.Empty
  | Prim p -> I.PrimT (prim p)
  | Var (s, i) -> assert false
  | Con (c, ts) ->
     (* TODO monomorphization *)
     let id =
       (if List.length ts = 0 then string_of_con c
        else sprintf "%s<%s>" (string_of_con c) (String.concat ", " (List.map string_of_typ ts))) @@ no_region in
     dec_set := ConSet.add c !dec_set;
     I.VarT id
  | Tup ts ->
     I.RecordT (tuple env ts)
  | Array t -> I.VecT (typ env t)
  | Obj (Object, fs) ->
     I.RecordT (List.map (field env) fs)
  | Obj (Actor, fs) -> I.ServT (List.map (meth env) fs)
  | Obj (Module, _) -> assert false
  | Variant fs ->
     I.VariantT (List.map (field env) fs)
  | Typ c -> assert false
  | Func (Shared, c, [], ts1, ts2) ->
     let fs1 = tuple env ts1 in
     (match ts2 with
     | [] when c = Returns -> I.FuncT ([I.Oneway @@ no_region], fs1, [])
     | [Async t] when c = Promises -> I.FuncT ([], fs1, tuple env [t])
     | _ -> assert false)
  | Func _ -> assert false
  | Opt t -> I.OptT (typ env t)
  | Async t -> assert false
  | Mut t -> assert false
  | Serialized t -> assert false
  ) @@ no_region
and field env {lab; typ=t} =
  match unescape lab with
  | Nat nat ->
     let name = Lib.Uint32.to_string nat @@ no_region in
     I.{id = nat; name = name; typ = typ env t} @@ no_region
  | Id id -> 
     let name = id @@ no_region in
     let id = Idllib.IdlHash.idl_hash id in
     I.{id = id; name = name; typ = typ env t} @@ no_region
and tuple env ts =
  List.mapi (fun i x ->
      let id = Lib.Uint32.of_int i in
      let name = Lib.Uint32.to_string id @@ no_region in
      I.{id = id; name = name; typ = typ env x} @@ no_region
    ) ts
and meth env {lab; typ=t} =
  match unescape lab with
  | Nat nat -> I.{var = Lib.Uint32.to_string nat @@ no_region; meth = typ env t} @@ no_region
  | Id id -> I.{var = id @@ no_region; meth = typ env t} @@ no_region

let actor env =
  let set =
    ConSet.filter (fun c ->
        match Con.kind c with
        | Def ([], Obj (Actor, fs)) -> true
        | _ -> false
      ) env.Scope.con_env in
  assert (ConSet.cardinal set <= 1);
  match ConSet.choose_opt set with
  | None -> None
  | Some c ->
     (match Con.kind c with
      | Def (_, Obj (Actor, fs)) ->
         let t = I.ServT (List.map (meth env) fs) @@ no_region in
         Some (I.ActorD (Con.to_string c @@ no_region, t) @@ no_region)
      | _ -> assert false)

let decs env set =
  ConSet.fold (fun c list ->
      match Con.kind c with
      | Def ([], t) -> (I.TypD (Con.to_string c @@ no_region, typ env t) @@ no_region) :: list
      | _ -> list)
  set []

let prog env : I.prog =
  let actor = actor env in
  let decs = decs env !dec_set in
  let it = I.{decs = decs; actor = actor} in
  {it = it; at = no_region; note = ""}

