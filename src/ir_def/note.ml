open Mo_types

type t = {
  typ : Type.typ;
  eff : Type.eff;
  check : int;
}

let def : t = {
  typ = Type.Pre;
  eff = Type.Triv;
  check = 0;
}

