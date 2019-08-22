open As_types
open As_types.Type
open Source
open Printf
module E = As_def.Syntax
module I = Idllib.Syntax

type label = Nat of Lib.Uint32.t | Id of string

let env = ref Env.empty
(* For monomorphization *)
let stamp = ref Env.empty
let type_map = ref Env.empty
           
let normalize str =
  let illegal_chars = ['-'; '/';] in
  String.map (fun c -> if List.mem c illegal_chars then '_' else c) str

let string_of_con vs c =
  let name = string_of_con c in
  match Con.kind c with
  | Def ([], _) -> normalize name
  | Def (tbs, _) ->
     let id = sprintf "%s<%s>" name (String.concat "," (List.map string_of_typ vs)) in
     let n =
       match Env.find_opt id !type_map with
       | None ->
          (match Env.find_opt name !stamp with
           | None -> 
              stamp := Env.add name 1 !stamp;
              type_map := Env.add id 1 !type_map;
              1
           | Some n ->
              stamp := Env.add name (n+1) !stamp;
              type_map := Env.add id (n+1) !type_map;
              n+1)
       | Some n -> n
     in Printf.sprintf "%s_%d" (normalize name) n
  | _ -> assert false
  
let unescape lab : label =
  let lab = normalize lab in
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
  
let rec typ vs t =
  (match t with
  | Any -> I.PrimT I.Reserved
  | Non -> I.PrimT I.Empty
  | Prim p -> I.PrimT (prim p)
  | Var (s, i) -> (typ vs (List.nth vs i)).it
  | Con (c, []) ->
     chase_con vs c;
     I.VarT (string_of_con vs c @@ no_region)
  | Con (c, ts) ->
     let ts =
       List.map (fun t ->
           match t with
           | Var (s, i) -> List.nth vs i
           | _ -> t
         ) ts in
     (match Con.kind c with
      | Def (tbs, t) ->
         (* use this for inlining defs, doesn't work with recursion
         (typ ts t).it
          *)
         chase_con ts c;
         I.VarT (string_of_con ts c @@ no_region)
      | _ -> assert false)
  | Typ c -> assert false
  | Tup ts ->
     I.RecordT (tuple vs ts)
  | Array t -> I.VecT (typ vs t)
  | Opt t -> I.OptT (typ vs t)                 
  | Obj (Object, fs) ->
     I.RecordT (List.map (field vs) fs)
  | Obj (Actor, fs) -> I.ServT (meths vs fs)
  | Obj (Module, _) -> assert false
  | Variant fs ->
     I.VariantT (List.map (field vs) fs)
  | Func (Shared, c, [], ts1, ts2) ->
     let fs1 = tuple vs ts1 in
     (match ts2 with
     | [] when c = Returns -> I.FuncT ([I.Oneway @@ no_region], fs1, [])
     | [Async t] when c = Promises -> I.FuncT ([], fs1, tuple vs [t])
     | _ -> assert false)
  | Func _ -> assert false
  | Async t -> assert false
  | Mut t -> assert false
  | Serialized t -> assert false
  | Pre -> assert false                  
  ) @@ no_region
and field vs {lab; typ=t} =
  match unescape lab with
  | Nat nat ->
     let name = Lib.Uint32.to_string nat @@ no_region in
     I.{id = nat; name = name; typ = typ vs t} @@ no_region
  | Id id -> 
     let name = id @@ no_region in
     let id = Idllib.IdlHash.idl_hash id in
     I.{id = id; name = name; typ = typ vs t} @@ no_region
and tuple vs ts =
  List.mapi (fun i x ->
      let id = Lib.Uint32.of_int i in
      let name = Lib.Uint32.to_string id @@ no_region in
      I.{id = id; name = name; typ = typ vs x} @@ no_region
    ) ts
and meths vs fs =
  List.fold_right (fun f list ->
      match f.typ with
      | Typ c ->
         chase_con vs c;
         list
      | _ ->
         let meth =
           match unescape f.lab with
           | Nat nat ->
              I.{var = Lib.Uint32.to_string nat @@ no_region;
                 meth = typ vs f.typ} @@ no_region
           | Id id ->
              I.{var = id @@ no_region;
                 meth = typ vs f.typ} @@ no_region in
         meth :: list
    ) fs []
and chase_con vs c =
  let id = string_of_con vs c in
  if not (Env.mem id !env) then
    (match Con.kind c with
     | Def (_, t) ->
         env := Env.add id (I.PreT @@ no_region) !env;
         let t = typ vs t in
         env := Env.add id t !env
     | _ -> assert false)

let is_actor_con c =
  match Con.kind c with
  | Def ([], Obj (Actor, _)) -> true
  | _ -> false

let chase_decs env =
  ConSet.iter (fun c ->
      if is_actor_con c then chase_con [] c
    ) env.Scope.con_env
  
let gather_decs () =
  Env.fold (fun id t list ->
      let dec = I.TypD (id @@ no_region, t) @@ no_region in
      dec::list
    ) !env []

let actor progs =
  let open E in
  let find_last_actor (prog : prog) =
    let anon = normalize ("anon_" ^ (Filename.remove_extension prog.note)) in
    let check_dec d t def =
      let rec check_pat p =
        match p.it with
        | WildP -> Some (anon, t)
        | VarP id -> Some (id.it, t)
        | ParP p -> check_pat p
        | _ -> def
      in
      match d.it with
      | ExpD _ -> Some (anon, t)
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
     | Some (id, t) -> Some (I.ActorD (id @@ no_region, typ [] t) @@ no_region)
             
let prog (progs, senv) : I.prog =
  env := Env.empty;
  let actor = actor progs in
  if actor = None then chase_decs senv;
  let decs = gather_decs () in  
  let prog = I.{decs = decs; actor = actor} in
  {it = prog; at = no_region; note = ""}

