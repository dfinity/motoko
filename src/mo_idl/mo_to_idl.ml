open Mo_def
open Mo_types

open Source
open Trivia
open Type
module E = Syntax
module I = Idllib.Syntax

(* use a functor to allocate temporary shared state *)
module MakeState() = struct

  let env = ref Env.empty

  (* For monomorphization *)
  module Stamp = Type.ConEnv
  let stamp = ref Stamp.empty

  module TypeMap = Map.Make (struct type t = con * typ list let compare = compare end)
  let type_map = ref TypeMap.empty

  let normalize_name name =
    String.map (fun c ->
        if c >= '0' && c <= '9' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'
        then c else '_'
      ) name

  let monomorphize_con vs c =
    let name = normalize_name (Cons.name c) in
    match Cons.kind c with
    | Def _ ->
      let id = (c, vs) in
      let (k, n) =
        match TypeMap.find_opt id !type_map with
        | None ->
          (match Stamp.find_opt c !stamp with
           | None ->
             let keys = Stamp.keys !stamp in
             let k = List.length (List.filter (fun d -> Cons.name c = Cons.name d) keys) in
             stamp := Stamp.add c (k, 0) !stamp;
             type_map := TypeMap.add id (k, 0) !type_map;
             (k, 0)
           | Some (k, n) ->
             stamp := Stamp.add c (k, n + 1) !stamp;
             type_map := TypeMap.add id (k, n + 1) !type_map;
             (k, n + 1))
        | Some (k, n) -> (k, n)
      in
      begin
        match (k, n) with
        | (0, 0) -> name
        | (0, n) when n > 0 -> Printf.sprintf "%s_%d" name n
        | (k, 0) when k > 0 -> Printf.sprintf "%s__%d" name k
        | (k, n) when k > 0 && n > 0 -> Printf.sprintf "%s__%d_%d" name k n
        | _ -> assert false
      end
    | _ -> assert false

  let prim = function
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
    | Float -> I.PrimT I.Float64
    | Char -> I.PrimT I.Nat32
    | Text -> I.PrimT I.Text
    | Blob -> I.BlobT
    | Principal -> I.PrincipalT
    | Region
    | Error -> assert false

  let rec typ t =
    (match t with
    | Any -> I.PrimT I.Reserved
    | Non -> I.PrimT I.Empty
    | Prim p -> prim p
    | Var (s, i) -> assert false
    | Con (c, ts) ->
       (match Cons.kind c with
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
                   let t = typ (normalize t) in
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
            | Composite -> [I.Composite @@ no_region]
            | Write -> []),
           t1, args ts)
       | _ -> assert false)
    | Named (n, t) ->
      (* drop name, Candid only allows names on function argument and return types *)
      (typ t).it
    | Func _
    | Async _
    | Mut _
    | Pre -> assert false
    ) @@ no_region
  and field {lab; typ = t; src = {region; _}} =
    let open Idllib.Escape in
    match unescape lab with
    | Nat nat ->
       I.{label = I.Id nat @@ no_region; typ = typ t} @@ region
    | Id id ->
       I.{label = I.Named id @@ no_region; typ = typ t} @@ region
  and fields fs =
    List.map field
      (List.filter (fun f -> not (is_typ f.typ)) fs)
  and tuple ts =
    List.mapi (fun i x ->
        let id = Lib.Uint32.of_int i in
        I.{label = I.Unnamed id @@ no_region; typ = typ x} @@ no_region
      ) ts
  and args ts =
    List.map arg_typ ts
  and arg_typ t =
    match t with
    | Named (name, t) ->
       let open Idllib.Escape in
       (match unescape name with
       | Nat nat ->
          I.{name= None; typ = typ t} @@ no_region
       | Id id ->
          I.{name= Some (id @@ no_region); typ = typ t} @@ no_region)
    | t ->
      I.{name= None; typ = typ t} @@ no_region
  and meths fs =
    List.fold_right (fun f list ->
        match f.typ with
        | Typ c ->
           list
        | _ ->
           let meth =
             I.{var = Idllib.Escape.unescape_method f.lab @@ no_region;
                meth = typ f.typ} @@ f.src.region in
           meth :: list
      ) fs []

  let is_actor_con c =
    match Cons.kind c with
    | Def ([], Obj (Actor, _)) -> true
    | _ -> false

  let chase_decs env =
    ConSet.iter (fun c ->
        if is_actor_con c then ignore (typ (Con (c,[])))
      ) env.Scope.con_env

  let gather_decs () =
    Env.fold (fun id t list ->
        (* TODO: pass corresponding Motoko source region? *)
        let dec = I.TypD (id @@ no_region, t) @@ no_region in
        dec::list
      ) !env []


  let actor prog =
    let open E in
    let { body = cub; _ } = (CompUnit.comp_unit_of_prog false prog).it in
    match cub.it with
    | ProgU _ | ModuleU _ -> None
    | ActorU _ -> Some (typ cub.note.note_typ)
    | ActorClassU _ ->
       (match normalize cub.note.note_typ with
        | Func (Local, Returns, [tb], ts1, [t2]) ->
          let args = List.map arg_typ (List.map (open_ [Non]) ts1) in
          let (_, _, rng) = as_async (normalize (open_ [Non] t2)) in
          let actor = typ rng in
          Some (I.ClassT (args, actor) @@ cub.at)
        | _ -> assert false
       )

end

let prog (progs, senv) : I.prog =
  let prog = CompUnit.combine_progs progs in
  let trivia = prog.note.E.trivia in
  let open MakeState() in
  let actor = actor prog in
  if actor = None then chase_decs senv;
  let decs = gather_decs () in
  let it = I.{decs; actor} in
  {it; at = prog.at; note = I.{filename = ""; trivia}}

let of_actor_type t : I.prog =
  let open MakeState() in
  let actor = Some (typ t) in
  let decs = gather_decs () in
  let prog = I.{decs; actor} in
  {it = prog; at = no_region; note = I.{filename = ""; trivia = empty_triv_table}}

let of_service_type ts t : I.arg_typ list * I.prog =
  let open MakeState() in
  let args = List.map arg_typ ts  in
  let actor = Some (typ t) in
  let decs = gather_decs () in
  let prog = I.{decs; actor} in
  args,
  {it = prog; at = no_region; note = I.{filename = ""; trivia = empty_triv_table}}
