(*
See lbool.mli for an overview
*)

type callback = unit Lazy.t

let do_nothing : callback = lazy ()

let (>>) cb1 cb2 = lazy Lazy.(force cb1; force cb2)

(* The lazy bool value type *)

type t' =
  | SurelyTrue
  | SurelyFalse
  | MaybeFalse of callback (* whom to notify when turning false *)
type t = t' ref

let set_false (l : t) =
  match !l with
  | SurelyTrue ->
    raise (Invalid_argument "Lbool.set_false() on surely true variable")
  | SurelyFalse -> ()
  | MaybeFalse when_false ->
    l := SurelyFalse; (* do this first, this breaks cycles *)
    Lazy.force when_false

let when_false (l : t) (act : callback) =
  match !l with
  | SurelyTrue -> ()
  | SurelyFalse -> Lazy.force act
  | MaybeFalse when_false ->
    l := MaybeFalse (act >> when_false)

let surely_true = ref SurelyTrue (* sharing is ok *)
let surely_false = ref SurelyFalse (* sharing is ok *)
let maybe_false () = ref (MaybeFalse do_nothing) (* no sharing, so unit argument *)

let required_for (a : t) (b : t) =
  when_false a (lazy (set_false b))

let all (xs : t list) : t =
  if xs = [] then surely_true else
  let b = maybe_false () in
  List.iter (fun a -> required_for a b) xs;
  b
