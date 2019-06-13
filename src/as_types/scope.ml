module T = Type

(* Scopes *)

type val_env = T.typ T.Env.t
type lib_env = T.typ T.Env.t
type typ_env = T.con T.Env.t
type con_env = T.ConSet.t

type obj_env = scope T.Env.t  (* internal object scopes *)

and scope =
  { val_env : val_env;
    lib_env : lib_env;
    typ_env : typ_env;
    con_env : con_env;
    obj_env : obj_env;
  }

let empty_scope : scope =
  { val_env = T.Env.empty;
    lib_env = T.Env.empty;
    typ_env = T.Env.empty;
    con_env = T.ConSet.empty;
    obj_env = T.Env.empty;
  }

let adjoin_scope scope1 scope2 =
  { val_env = T.Env.adjoin scope1.val_env scope2.val_env;
    lib_env = T.Env.adjoin scope1.lib_env scope2.lib_env;
    typ_env = T.Env.adjoin scope1.typ_env scope2.typ_env;
    con_env = T.ConSet.union scope1.con_env scope2.con_env;
    obj_env = T.Env.adjoin scope1.obj_env scope2.obj_env;
  }


let adjoin_val_env scope ve = {scope with val_env = T.Env.adjoin scope.val_env ve}

let library_scope f t =
  { empty_scope with lib_env = T.Env.add f t empty_scope.lib_env }
