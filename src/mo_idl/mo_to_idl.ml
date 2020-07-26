open Mo_types
open Mo_types.Type
open Source
module E = Mo_def.Syntax
module I = Idllib.Syntax

let env = ref Env.empty

(* For monomorphization *)
module Stamp = Map.Make(String)
let stamp = ref Stamp.empty

module TypeMap = Map.Make (struct type t = con * typ list let compare = compare end)
let type_map = ref TypeMap.empty

let monomorphize_con vs c =
  let name = Con.name c in
  match Con.kind c with
  | Def _ ->
     let id = (c, vs) in
     let n =
       match TypeMap.find_opt id !type_map with
       | None ->
          (match Stamp.find_opt name !stamp with
           | None ->
              stamp := Stamp.add name 1 !stamp;
              type_map := TypeMap.add id 1 !type_map;
              1
           | Some n ->
              stamp := Stamp.add name (n+1) !stamp;
              type_map := TypeMap.add id (n+1) !type_map;
              n+1)
       | Some n -> n
     in
     if n == 1 then name
     else Printf.sprintf "%s_%d" name n
  | _ -> assert false

let prim p =
  match p with
  | Null -> I.PrimT I.Null
  | Bool -> I.PrimT I.Bool
  | Nat -> I.PrimT I.Nat
  | Nat8 -> I.PrimT I.Nat8
  | Nat16 -> I.PrimT I.Nat16
  | Nat32 -> I.PrimT I.Nat32
  | Nat64 -> I.PrimT I.Nat64
  | Int -> I.PrimT I.Int
  | Int8 -> I.PrimT I.Int8
  | Int16 -> I.PrimT I.Int16
  | Int32 -> I.PrimT I.Int32
  | Int64 -> I.PrimT I.Int64
  | Word8 -> I.PrimT I.Nat8
  | Word16 -> I.PrimT I.Nat16
  | Word32 -> I.PrimT I.Nat32
  | Word64 -> I.PrimT I.Nat64
  | Float -> I.PrimT I.Float64
  | Char -> I.PrimT I.Nat32
  | Text -> I.PrimT I.Text
  | Blob -> I.BlobT
  | Principal -> I.PrincipalT
  | Error -> assert false

let rec typ t =
  (match t with
  | Any -> I.PrimT I.Reserved
  | Non -> I.PrimT I.Empty
  | Prim p -> prim p
  | Var (s, i) -> assert false
  | Con (c, ts) ->
     (match Con.kind c with
      | Def (_, t) ->
         (match (open_ ts t) with
          | Prim p -> prim p
          | Any -> I.PrimT I.Reserved
          | Non -> I.PrimT I.Empty
          | t ->
             let id = monomorphize_con ts c in
             if not (Env.mem id !env) then
               begin
                 env := Env.add id (I.PreT @@ no_region) !env;
                 let t = typ t in
                 env := Env.add id t !env
               end;
             I.VarT (id @@ no_region))
      | _ -> assert false)
  | Typ c -> assert false
  | Tup ts ->
     if ts = [] then
       I.PrimT I.Null
     else
       I.RecordT (tuple ts)
  | Array t -> I.VecT (typ t)
  | Opt t -> I.OptT (typ t)
  | Obj (Object, fs) ->
     I.RecordT (fields fs)
  | Obj (Actor, fs) -> I.ServT (meths fs)
  | Obj (Module, _) -> assert false
  | Obj (Memory, _) -> assert false
  | Variant fs ->
     I.VariantT (fields fs)
  | Func (Shared s, c, tbs, ts1, ts2) ->
     let nons = List.map (fun _ -> Non) tbs in
     let ts1, ts2 =
       (List.map (open_ nons) ts1,
        List.map (open_ nons) ts2) in
     let t1 = args ts1 in
     (match ts2, c with
     | [], Returns -> I.FuncT ([I.Oneway @@ no_region], t1, [])
     | ts, Promises ->
       I.FuncT (
         (match s with
          | Query -> [I.Query @@ no_region]
          | Write -> []),
         t1, args ts)
     | _ -> assert false)
  | Func _ -> assert false
  | Async _ -> assert false
  | Mut t -> assert false
  | Pre -> assert false
  ) @@ no_region
and field {lab; typ=t} =
  let open Idllib.Escape in
  match unescape lab with
  | Nat nat ->
     I.{label = I.Id nat @@ no_region; typ = typ t} @@ no_region
  | Id id ->
     I.{label = I.Named id @@ no_region; typ = typ t} @@ no_region
and fields fs =
  List.map field
    (List.filter (fun f -> not (is_typ f.typ)) fs)
and tuple ts =
  List.mapi (fun i x ->
      let id = Lib.Uint32.of_int i in
      I.{label = I.Unnamed id @@ no_region; typ = typ x} @@ no_region
    ) ts
and args ts =
  List.map typ ts
and meths fs =
  List.fold_right (fun f list ->
      match f.typ with
      | Typ c ->
         list
      | _ ->
         let meth =
           let open Idllib.Escape in
           match unescape f.lab with
           | Nat nat ->
              I.{var = Lib.Uint32.to_string nat @@ no_region;
                 meth = typ f.typ} @@ no_region
           | Id id ->
              I.{var = id @@ no_region;
                 meth = typ f.typ} @@ no_region in
         meth :: list
    ) fs []

let is_actor_con c =
  match Con.kind c with
  | Def ([], Obj (Actor, _)) -> true
  | _ -> false

let chase_decs env =
  ConSet.iter (fun c ->
      if is_actor_con c then ignore (typ (Con (c,[])))
    ) env.Scope.con_env

let gather_decs () =
  Env.fold (fun id t list ->
      let dec = I.TypD (id @@ no_region, t) @@ no_region in
      dec::list
    ) !env []

let actor progs =
  let open E in
  let find_last_actor (prog : prog) =
    let check_dec d t def =
      let rec check_pat p =
        match p.it with
        | WildP -> Some t
        | VarP id -> Some t
        | ParP p -> check_pat p
        | _ -> def
      in
      match d.it with
      | ExpD _ -> Some t
      | LetD (pat, _) -> check_pat pat
      | _ -> def
    in
    List.fold_left
      (fun actor (d : dec) ->
        match d.note.note_typ with
        | Obj (Actor, _) as t -> check_dec d t actor
        | Con (c, []) as t when is_actor_con c -> check_dec d t actor
        | _ -> actor
      ) None prog.it in

  match progs with
  | [] -> None
  | _ ->
     let prog = Lib.List.last progs in
     match find_last_actor prog with
     | None -> None
     | Some t -> Some (typ t)

let prog (progs, senv) : I.prog =
  env := Env.empty;
  let actor = actor progs in
  if actor = None then chase_decs senv;
  let decs = gather_decs () in
  let prog = I.{decs = decs; actor = actor} in
  {it = prog; at = no_region; note = ""}
