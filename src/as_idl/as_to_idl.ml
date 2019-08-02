open As_types
open As_types.Type
open Source   
open Printf
module E = As_def.Syntax
module I = Idllib.Syntax

type label = Nat of Lib.Uint32.t | Id of string

let dec_env = ref ConEnv.empty            

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
  
let rec typ t =
  (match t with
  | Any -> I.PrimT I.Reserved
  | Non -> I.PrimT I.Empty
  | Prim p -> I.PrimT (prim p)
  | Var (s, i) ->
     printf "VAR %s.%d" s i;
     assert false
  | Con (c, ts) ->
     (* TODO monomorphization *)
     let id = 
     (if List.length ts = 0 then string_of_con c
        else sprintf "%s<%s>" (string_of_con c) (String.concat ", " (List.map string_of_typ ts))) @@ no_region in
     chase_con c;
     I.VarT id
  | Typ c -> assert false
  | Tup ts ->
     I.RecordT (tuple ts)
  | Array t -> I.VecT (typ t)
  | Opt t -> I.OptT (typ t)                 
  | Obj (Object, fs) ->
     I.RecordT (List.map field fs)
  | Obj (Actor, fs) -> I.ServT (meths fs)
  | Obj (Module, _) -> assert false
  | Variant fs ->
     I.VariantT (List.map field fs)
  | Func (Shared, c, [], ts1, ts2) ->
     let fs1 = tuple ts1 in
     (match ts2 with
     | [] when c = Returns -> I.FuncT ([I.Oneway @@ no_region], fs1, [])
     | [Async t] when c = Promises -> I.FuncT ([], fs1, tuple [t])
     | _ -> assert false)
  | Func _ -> assert false
  | Async t -> assert false
  | Mut t -> assert false
  | Serialized t -> assert false
  | Pre -> assert false                  
  ) @@ no_region
and field {lab; typ=t} =
  match unescape lab with
  | Nat nat ->
     let name = Lib.Uint32.to_string nat @@ no_region in
     I.{id = nat; name = name; typ = typ t} @@ no_region
  | Id id -> 
     let name = id @@ no_region in
     let id = Idllib.IdlHash.idl_hash id in
     I.{id = id; name = name; typ = typ t} @@ no_region
and tuple ts =
  List.mapi (fun i x ->
      let id = Lib.Uint32.of_int i in
      let name = Lib.Uint32.to_string id @@ no_region in
      I.{id = id; name = name; typ = typ x} @@ no_region
    ) ts
and meths fs =
  List.fold_right (fun f list ->
      match f.typ with
      | Typ c ->
         chase_con c;
         list
      | _ ->
         let meth =
           match unescape f.lab with
           | Nat nat ->
              I.{var = Lib.Uint32.to_string nat @@ no_region;
                 meth = typ f.typ} @@ no_region
           | Id id ->
              I.{var = id @@ no_region;
                 meth = typ f.typ} @@ no_region in
         meth :: list
    ) fs []
and chase_con c =
  if not (ConEnv.mem c !dec_env) then
    (match Con.kind c with
     | Def ([], t) ->
        dec_env := ConEnv.add c (I.PreT @@ no_region) !dec_env;
        let t = typ t in
        dec_env := ConEnv.add c t !dec_env
     | _ -> ())

let is_actor_con c =
  match Con.kind c with
  | Def ([], Obj (Actor, _)) -> true
  | _ -> false

let chase_decs env =
  ConSet.iter (fun c ->
      if is_actor_con c then chase_con c
    ) env.Scope.con_env
  
let gather_decs () =
  ConEnv.fold (fun c t list ->
      let dec = I.TypD (Con.to_string c @@ no_region, t) @@ no_region in
      dec::list
    ) !dec_env []

let actor progs =
  let open E in
  let find_last_actor (prog : prog) =
    let anon = "anon_" ^ (Filename.remove_extension prog.note) in
    let check_dec d t def =
      match d.it with
      | ExpD _ -> Some (anon, t)
      | LetD ({it=WildP;_}, _) -> Some (anon, t)                      
      | LetD ({it=VarP id;_}, _) -> Some (id.it, t)
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
     | Some (id, t) -> Some (I.ActorD (id @@ no_region, typ t) @@ no_region)
             
let prog (progs, env) : I.prog =
  dec_env := ConEnv.empty;
  let actor = actor progs in
  if actor = None then chase_decs env;
  let decs = gather_decs () in  
  let prog = I.{decs = decs; actor = actor} in
  {it = prog; at = no_region; note = ""}

