open Mo_types

type t = {
  typ : Type.typ;
  eff : Type.eff;
}

let def : t = {
  typ = Type.Pre;
  eff = Type.Triv;
}

