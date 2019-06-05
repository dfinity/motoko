
open Format
open Syntax_idl
open Source

let str ppf s = pp_print_string ppf s; pp_print_cut ppf ()
let id ppf s = str ppf s.it; pp_print_cut ppf ()
let space = pp_print_space             
let kwd ppf s = str ppf s; space ppf ()
let field_name ppf s = str ppf "'"; str ppf s.it; str ppf "'"; pp_print_cut ppf ()                

let pp_prim p =
  match p with
  | Nat -> "Nat"
  | Nat8 -> "Nat"
  | Nat16 -> "Nat"
  | Nat32 -> "Nat"
  | Nat64 -> "Nat"
  | Int -> "Int"
  | Int8 -> "Int"
  | Int16 -> "Int"
  | Int32 -> "Int"
  | Int64 -> "Int"
  | Float32 -> "Float"
  | Float64 -> "Float"
  | Bool -> "Bool"
  | Text -> "Text"
  | Null -> "Unit"
  | Reserved -> "None"
                       
let rec concat ppf f env sep list =
  match list with
  | [] -> ()
  | e::[] -> f ppf env e; pp_print_cut ppf ()
  | e::tail -> f ppf env e; str ppf sep; space ppf (); concat ppf f env sep tail

let rec pp_typ ppf env t =
  pp_open_box ppf 1;
  (match t.it with
  | VarT s -> id ppf s
  | PrimT p -> str ppf ("IDL."^(pp_prim p))
  | RecordT ts -> pp_fields ppf env ts
  | VecT t -> str ppf "IDL.Arr("; pp_typ ppf env t; str ppf ")";
  | OptT t -> str ppf "IDL.Opt("; pp_typ ppf env t; str ppf ")";
  | VariantT ts -> str ppf "IDL.Variant({"; concat ppf pp_field env "," ts; str ppf "})";
  | FuncT (ms, t1, t2) ->
     str ppf "IDL.message(";
     pp_fields ppf env t1;
     kwd ppf ",";
     pp_fields ppf env t2;
     str ppf ")";
  | ServT ts ->
     pp_open_hovbox ppf 1;
     str ppf "IDL.ActorInterface({";
     concat ppf pp_meth env "," ts;
     str ppf "})";
     pp_close_box ppf ();
  | PreT -> ()
  );
  pp_close_box ppf ()

and pp_fields ppf env fs =
  pp_open_box ppf 1;
  str ppf "IDL.Obj({";
  concat ppf pp_field env "," fs;
  str ppf "})";
  pp_close_box ppf ()
  
and pp_field ppf env tf =
  pp_open_box ppf 1;
  field_name ppf tf.it.name; kwd ppf ":"; pp_typ ppf env tf.it.typ;
  pp_close_box ppf ()

and pp_meth ppf env meth =
  pp_open_box ppf 1;
  field_name ppf meth.it.var;
  kwd ppf ":";
  pp_typ ppf env meth.it.meth;
  pp_close_box ppf ()
  
let pp_dec ppf env dec =
  pp_open_hovbox ppf 1;
  kwd ppf "const";
  (match dec.it with
   | TypD (x, t) ->
      kwd ppf x.it;
      kwd ppf "=";
      pp_typ ppf env t
  );
  pp_close_box ppf ();
  pp_print_cut ppf ()

let pp_actor ppf env actor_opt =
  pp_open_hovbox ppf 1;
  (match actor_opt with
     None -> ()
   | Some actor ->
      kwd ppf "const";
      (match actor.it with
       | ActorD (x, {it=ServT tp; _}) ->
          id ppf x; space ppf (); kwd ppf "="; kwd ppf "new";
          str ppf "IDL.ActorInterface({";
          concat ppf pp_meth env "," tp;
          str ppf "})"
       | ActorD (x, {it=VarT var; _}) -> id ppf x; space ppf (); kwd ppf "="; id ppf var
       | ActorD (x, _) -> ()
      )
  );
  pp_close_box ppf ()

let pp_prog ppf env prog =
  pp_open_vbox ppf 0;
  List.iter (pp_dec ppf env) prog.it.decs;
  pp_actor ppf env prog.it.actor;
  pp_close_box ppf ()
   
let compile (scope : Typing_idl.scope) (prog : Syntax_idl.prog) =
  let buf = Buffer.create 100 in
  let ppf = formatter_of_buffer buf in
  pp_prog ppf scope prog;
  pp_print_flush ppf ();
  buf
