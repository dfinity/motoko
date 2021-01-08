open Mo_types

type t = {
  typ : Type.typ;
  eff : Type.eff;
  const : bool;
}

let def : t = {
  typ = Type.Pre;
  eff = Type.Triv;
  const = false;
}

