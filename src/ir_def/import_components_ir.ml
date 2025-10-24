open Mo_types
open Source
open Ir

type arg_data = {
  arg_name : string;
  arg_type : Type.typ;
}

let traverse add_import p = 
let rec exp e = match e.it with
  | VarE (m, i)         -> ()
  | LitE l              -> ()
  | PrimE (p, es)       -> prim e es p; List.iter exp es
  | AssignE (le1, e2)   -> lexp le1; exp e2
  | BlockE (ds, e1)     -> List.iter dec ds; exp e1
  | IfE (e1, e2, e3)    -> exp e1; exp e2; exp e3
  | SwitchE (e, cs)     -> exp e; List.iter case cs
  | LoopE e1            -> exp e1
  | LabelE (i, t, e)    -> exp e
  | AsyncE (_, tb, e, t) -> exp e
  | DeclareE (i, t, e1) -> exp e1
  | DefineE (i, m, e1)  -> exp e1
  | FuncE (x, s, c, tp, as_, ts, e) -> args as_; exp e
  | SelfCallE (ts, exp_f, exp_k, exp_r, exp_c) -> exp exp_f; exp exp_k; exp exp_r; exp exp_c
  | ActorE (ds, fs, u, t) -> List.iter dec ds; system u
  | NewObjE (s, fs, t)  -> ignore (Arrange_type.obj_sort s)
  | TryE (e, cs, _) -> exp e; List.iter case cs

and system { meta; preupgrade; postupgrade; heartbeat; timer; inspect; low_memory; stable_record; stable_type} = (* TODO: show meta? *)
      exp preupgrade;
      exp postupgrade;
      exp heartbeat;
      exp timer;
      exp inspect;
      exp low_memory;
      exp stable_record;
      ()

and lexp le = match le.it with
  | VarLE i             -> ()
  | IdxLE (e1, e2)      -> exp e1; exp e2
  | DotLE (e1, n)       -> exp e1

and args = function
 | [] -> ()
 | as_ -> List.iter arg as_

and arg a = ()

and prim e es = function
  | ComponentPrim (full_name, component_name, function_name, arg_types, return_type) -> 
      let string_of_arg arg =
        match arg.it with
        | VarE (_, i) -> i
        | _ -> failwith "Expected VarE for argument name" in

      assert (List.length es = List.length arg_types);
      let function_args = List.map2 (fun arg arg_type -> {arg_name=string_of_arg arg; arg_type}) es arg_types in

      (* Add the import to the component *)
      add_import component_name function_name function_args return_type
  | _ -> ()

and case c = exp c.it.exp

and dec d = match d.it with
  | LetD (p, e) -> exp e
  | VarD (i, t, e) -> exp e
  | RefD (i, t, e) -> lexp e

and comp_unit = function
  | LibU (ds, e) ->   List.iter dec ds; exp e
  | ProgU ds -> List.iter dec ds
  | ActorU (None, ds, fs, u, t) -> List.iter dec ds; system u
  | ActorU (Some as_, ds, fs, u, t) -> List.iter arg as_; List.iter dec ds; system u

and prog (cu, _flavor) = comp_unit cu

in prog p
