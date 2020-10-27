
open Format
open Syntax
open Source

module Env = Typing.Env
module TS = Set.Make(String)           

type typ_info = {
    var : string;
    typ : typ;
    is_rec : bool;
  }

let as_tuple fs =
  let open List in
  let fs2 = mapi (fun i f -> (i, f)) fs in
  let is_tuple = length fs > 0 && for_all (fun (i, f) ->
      match f.it.label.it with
      | Unnamed id -> Lib.Uint32.to_int id = i
      | _ -> false) fs2 in
  if is_tuple then
    Some (map (fun (f : typ_field) -> f.it.typ) fs)
  else
    None

(* Gather type definitions from actor and sort the definitions in topological order *)              
let chase_env env actor =
  let new_env = ref [] in
  let seen = ref TS.empty in
  let rec chase t =
    match t.it with
    | PrimT _ -> ()
    | PrincipalT -> ()
    | BlobT -> ()
    | VarT id ->
       if not (TS.mem id.it !seen) then begin
         seen := TS.add id.it !seen;
         let t = Env.find id.it env in
         chase t;
         new_env := {var = id.it; typ = t; is_rec = false} :: !new_env;
         end
    | ServT ms -> List.iter (fun m -> chase m.it.meth) ms
    | OptT t -> chase t
    | VecT t -> chase t
    | RecordT fs -> chase_fields fs
    | VariantT fs -> chase_fields fs
    | FuncT (ms, fs1, fs2) -> List.iter chase fs1; List.iter chase fs2
    | ClassT _ -> assert false
    | PreT -> assert false
  and chase_fields fs =
    List.iter (fun (f : typ_field) -> chase f.it.typ) fs
  in
  chase actor;
  List.rev (!new_env)

(* Given a topologically sorted type definition list, infer which types are recursive *)
let infer_rec env_list =
  let seen = ref TS.empty in
  let recs = ref TS.empty in
  let rec go t =
    match t.it with
    | PrimT _ -> ()
    | PrincipalT -> ()
    | BlobT -> ()
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
    | FuncT (_, fs1, fs2) -> List.iter go fs1; List.iter go fs2
    | ClassT _ -> assert false
    | PreT -> assert false
  and go_fields fs =
    List.iter (fun (f:typ_field) -> go f.it.typ) fs
  in
  List.iter (fun {var;typ;_} -> go typ; seen := TS.add var !seen) env_list;
  !recs
  
let str ppf s = pp_print_string ppf s; pp_print_cut ppf ()
let id ppf s = str ppf s.it; pp_print_cut ppf ()
let space = pp_print_space             
let kwd ppf s = str ppf s; space ppf ()
let quote_name ppf s = pp_open_hbox ppf (); str ppf "'"; str ppf (Lib.String.lightweight_escaped s); str ppf "'"; pp_close_box ppf (); pp_print_cut ppf ()

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
  | Float32 -> "Float32"
  | Float64 -> "Float64"
  | Bool -> "Bool"
  | Text -> "Text"
  | Null -> "Null"
  | Reserved -> "None"
  | Empty -> "None"

let pp_mode ppf m =
  match m.it with
  | Oneway -> str ppf "'oneway'"
  | Query -> str ppf "'query'"

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
  | PrincipalT -> str ppf "IDL.Principal"
  | RecordT ts -> pp_fields ppf ts
  | VecT t -> str ppf "IDL.Vec("; pp_typ ppf t; str ppf ")";
  | BlobT -> str ppf "IDL.Vec(IDL.Nat8)";
  | OptT t -> str ppf "IDL.Opt("; pp_typ ppf t; str ppf ")";
  | VariantT ts -> str ppf "IDL.Variant({"; concat ppf pp_field "," ts; str ppf "})";
  | FuncT (ms, t1, t2) ->
     str ppf "IDL.Func(";
     pp_args ppf t1;
     kwd ppf ",";
     pp_args ppf t2;
     kwd ppf ",";
     pp_modes ppf ms;
     str ppf ")";
  | ServT ts ->
     pp_open_hovbox ppf 1;
     str ppf "IDL.Service({";
     concat ppf pp_meth "," ts;
     str ppf "})";
     pp_close_box ppf ();
  | ClassT _ -> assert false
  | PreT -> assert false
  );
  pp_close_box ppf ()

and pp_args ppf args =
  pp_open_box ppf 1;
  str ppf "[";
  concat ppf pp_typ "," args;
  str ppf "]";
  pp_close_box ppf ()

and pp_modes ppf modes =
  pp_open_box ppf 1;
  str ppf "[";
  concat ppf pp_mode "," modes;
  str ppf "]";
  pp_close_box ppf ()  

and pp_fields ppf fs =
  pp_open_box ppf 1;
  (match as_tuple fs with
  | None ->
     str ppf "IDL.Record({";
     concat ppf pp_field "," fs;
     str ppf "})";
  | Some typs ->
     str ppf "IDL.Tuple(";
     concat ppf pp_typ "," typs;
     str ppf ")");
  pp_close_box ppf ()
  
and pp_field ppf tf =
  pp_open_box ppf 1;
  let f_name =
    match tf.it.label.it with
    | Named name -> name
    | Id n | Unnamed n -> "_" ^ (Lib.Uint32.to_string n) ^ "_"
  in quote_name ppf f_name; kwd ppf ":"; pp_typ ppf tf.it.typ;
  pp_close_box ppf ()

and pp_meth ppf meth =
  pp_open_box ppf 1;
  quote_name ppf meth.it.var.it;
  kwd ppf ":";
  pp_typ ppf meth.it.meth;
  pp_close_box ppf ()

let pp_dec ppf {var;typ;is_rec} =
  pp_open_hovbox ppf 1;
  if is_rec then begin
      str ppf var;
      str ppf ".fill(";
      pp_typ ppf typ;
      str ppf ")";
    end
  else begin
      kwd ppf "const";
      kwd ppf var;
      kwd ppf "=";
      pp_typ ppf typ;
    end;
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

let pp_actor ppf t recs =
  pp_open_hovbox ppf 1;
  kwd ppf "return";
  (match t.it with
   | ServT tp ->
      str ppf "IDL.Service({";
      concat ppf pp_meth "," tp;
      str ppf "});"
   | VarT var ->
      if TS.mem var.it recs then
        str ppf (var.it ^ ".getType();")
      else
        str ppf var.it;
   | _ -> assert false
  );
  pp_close_box ppf ()    

let pp_header ppf () =
  pp_open_vbox ppf 1;
  str ppf "export default ({ IDL }) => {"

let pp_footer ppf () =
  pp_close_box ppf ();
  pp_force_newline ppf ();
  pp_print_string ppf "};"

let pp_prog ppf env prog =
  match prog.it.actor with
  | None -> ()
  | Some actor ->
     let env_list = chase_env env actor in
     let recs = infer_rec env_list in
     let env_list =
       List.map (fun (e:typ_info) ->
           if TS.mem e.var recs then {e with is_rec = true} else e)
         env_list in
     pp_header ppf ();
     TS.iter (pp_rec ppf) recs;
     List.iter (pp_dec ppf) env_list;
     pp_actor ppf actor recs;
     pp_footer ppf ()

let compile (scope : Typing.scope) (prog : Syntax.prog) =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  pp_prog ppf scope prog;
  pp_print_flush ppf ();
  buf
