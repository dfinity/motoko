type filepath = string

module Set = Set.Make(String)

type env = {
  base : filepath Lazy.t;
  imported : Set.t ref;
}

open Syntax
open Source

let rec decs env = List.iter (dec env)
and dec env d = match d.it with
  | TypD _ -> ()
  | ImportD (f, fp) ->
     let f = if Filename.is_relative f
             then Filename.concat (Lazy.force env.base) f
             else f in
     fp := f;
     env.imported := Set.add f !(env.imported)

let prog env p = decs env p.it.decs

let resolve : Syntax.prog -> filepath -> filepath list = fun p base ->
  let base = lazy (if Sys.is_directory base then base else Filename.dirname base) in
  let env = { base; imported = ref Set.empty } in
  prog env p;
  Set.elements !(env.imported)
