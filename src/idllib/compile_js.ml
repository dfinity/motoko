
open Format
open Syntax
open Source

module Env = Typing.Env
module TS = Set.Make(String)           

(* Gather type definitions from actor and sort the definitions in topological order *)              
let chase_env env actor =
  let new_env = ref [] in
  let seen = ref TS.empty in
  let rec chase t =
    match t.it with
    | PrimT _ -> ()
    | VarT id ->
       if not (TS.mem id.it !seen) then begin
         seen := TS.add id.it !seen;
         let t = Env.find id.it env in
         chase t;
         new_env := (id.it, t) :: !new_env;
         end
    | ServT ms -> List.iter (fun m -> chase m.it.meth) ms
    | OptT t -> chase t
    | VecT t -> chase t
    | RecordT fs -> chase_fields fs
    | VariantT fs -> chase_fields fs
    | FuncT (ms, fs1, fs2) -> chase_fields fs1; chase_fields fs2
    | PreT -> assert false
  and chase_fields fs =
    List.iter (fun (f : typ_field) -> chase f.it.typ) fs
  in
  match actor.it with
  | ActorD (_, t) ->
     chase t;
     List.rev (!new_env)

(* Given a topologically sorted type definition list, infer which types are recursive *)
let infer_rec env_list =
  let seen = ref TS.empty in
  let recs = ref TS.empty in
  let rec go t =
    match t.it with
    | PrimT _ -> ()
    | VarT id ->
       if not (TS.mem id.it !seen) then begin
         seen := TS.add id.it !seen;
         recs := TS.add id.it !recs
         end
    | ServT ms -> List.iter (fun m -> go m.it.meth) ms
    | OptT t -> go t
    | VecT t -> go t
    | RecordT fs -> go_fields fs
    | VariantT fs -> go_fields fs
    | FuncT (_, fs1, fs2) -> go_fields fs1; go_fields fs2
    | preT -> assert false
  and go_fields fs =
    List.iter (fun (f:typ_field) -> go f.it.typ) fs
  in
  List.iter (fun (x,t) -> go t; seen := TS.add x !seen) env_list;
  !recs
  
let str ppf s = pp_print_string ppf s; pp_print_cut ppf ()
let id ppf s = str ppf s.it; pp_print_cut ppf ()
let space = pp_print_space             
let kwd ppf s = str ppf s; space ppf ()
let field_name ppf s = str ppf "'"; str ppf s.it; str ppf "'"; pp_print_cut ppf ()                

let pp_prim p =
  match p with
  | Nat -> "Nat"
  | Nat8 -> "Nat8"
  | Nat16 -> "Nat16"
  | Nat32 -> "Nat32"
  | Nat64 -> "Nat64"
  | Int -> "Int"
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | Float32 -> "Float"
  | Float64 -> "Float"
  | Bool -> "Bool"
  | Text -> "Text"
  | Null -> "Unit"
  | Reserved -> "None"
                       
let rec concat ppf f sep list =
  match list with
  | [] -> ()
  | e::[] -> f ppf e; pp_print_cut ppf ()
  | e::tail -> f ppf e; str ppf sep; space ppf (); concat ppf f sep tail

let rec pp_typ ppf t =
  pp_open_box ppf 1;
  (match t.it with
  | VarT s -> id ppf s
  | PrimT p -> str ppf ("IDL."^(pp_prim p))
  | RecordT ts -> pp_fields ppf ts
  | VecT t -> str ppf "IDL.Arr("; pp_typ ppf t; str ppf ")";
  | OptT t -> str ppf "IDL.Opt("; pp_typ ppf t; str ppf ")";
  | VariantT ts -> str ppf "IDL.Variant({"; concat ppf pp_field "," ts; str ppf "})";
  | FuncT (ms, t1, t2) ->
     str ppf "IDL.message(";
     pp_fields ppf t1;
     kwd ppf ",";
     pp_fields ppf t2;
     str ppf ")";
  | ServT ts ->
     pp_open_hovbox ppf 1;
     kwd ppf "new";
     str ppf "IDL.ActorInterface({";
     concat ppf pp_meth "," ts;
     str ppf "})";
     pp_close_box ppf ();
  | PreT -> assert false
  );
  pp_close_box ppf ()

and pp_fields ppf fs =
  pp_open_box ppf 1;
  str ppf "IDL.Obj({";
  concat ppf pp_field "," fs;
  str ppf "})";
  pp_close_box ppf ()
  
and pp_field ppf tf =
  pp_open_box ppf 1;
  field_name ppf tf.it.name; kwd ppf ":"; pp_typ ppf tf.it.typ;
  pp_close_box ppf ()

and pp_meth ppf meth =
  pp_open_box ppf 1;
  field_name ppf meth.it.var;
  kwd ppf ":";
  pp_typ ppf meth.it.meth;
  pp_close_box ppf ()

let pp_dec ppf (x,t) =
  pp_open_hovbox ppf 1;
  kwd ppf "const";
  kwd ppf x;
  kwd ppf "=";
  pp_typ ppf t;
  pp_close_box ppf ();
  pp_print_cut ppf ()

let pp_rec ppf x =
  pp_open_hovbox ppf 1;
  kwd ppf "const";
  kwd ppf x;
  kwd ppf "=";
  str ppf "IDL.Rec()";
  pp_close_box ppf ();
  pp_print_cut ppf ()

let pp_actor ppf actor =
  pp_open_hovbox ppf 1;
  kwd ppf "const";
  (match actor.it with
   | ActorD (x, {it=ServT tp; _}) ->
      id ppf x; space ppf (); kwd ppf "="; kwd ppf "new";
      str ppf "IDL.ActorInterface({";
      concat ppf pp_meth "," tp;
      str ppf "})"
   | ActorD (x, {it=VarT var; _}) -> id ppf x; space ppf (); kwd ppf "="; id ppf var
   | ActorD (x, _) -> assert false
  );
  pp_close_box ppf ()

let pp_prog ppf env prog =
  match prog.it.actor with
  | None -> ()
  | Some actor ->
     let env_list = chase_env env actor in
     let recs = infer_rec env_list in
     pp_open_vbox ppf 0;
     TS.iter (pp_rec ppf) recs;
     List.iter (pp_dec ppf) env_list;
     pp_actor ppf actor;
     pp_close_box ppf ()
   
let compile (scope : Typing.scope) (prog : Syntax.prog) =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  pp_prog ppf scope prog;
  pp_print_flush ppf ();
  buf
