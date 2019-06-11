
type id = string

type prim =
  | Null
  | ReadLEB of id
  | WriteLEB of id
  | ReadByte of string * id
  | WriteByte of id

and data =
  | Length of id
  | NewRecord
  | GetField of id * id
  | SetField of id * id * exp          
               
and exp =
  | Prim of prim
  | Data of data
  | Var of id
  | Let of id * exp
  | Fun of id * exp
  | App of exp * exp
  | Seq of exp list
               
and dec = VarD of id * exp

(* Program *)

type prog = dec list * exp
