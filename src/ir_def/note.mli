open Mo_types

type t = {
  typ : Type.typ;
  eff : Type.eff;
  const : bool;
}

val def : t

