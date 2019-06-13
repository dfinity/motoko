
type id = string

type prim =
  | ReadLEB         (* Read LEB128 from stream *)
  | WriteLEB of id  (* Append LEB128 to stream *)
  | ReadByte of string * id (* Read id bytes in string encoding from stream *)
  | WriteByte of id         (* Append id bytes to stream *)

and data =
  | StrLength of id
  (* record *)          
  | NewRecord
  | GetField of id * id
  | SetField of id * id * id
  (* option *)              
  | Null
  (* vector *)
  | VecLength of id
  | GetItem of id * id * exp (* vec, elem, exp *)
  | NewVec of id * id * exp (* vec, len, exp *)
  | PushItem of id * id (* vec, value *)

and binop =
  | Eq
  
and exp =
  | Prim of prim
  | Data of data
  | Var of id
  | BinOp of binop * exp * exp
  | Let of id * exp
  | Fun of id * exp
  | App of exp * exp
  | Seq of exp list
  | If of exp * exp * exp
  | Block of id * exp
               
and dec = VarD of id * exp

(* Program *)

type prog = dec list * exp
