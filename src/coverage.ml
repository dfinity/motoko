open Syntax
open Source

module V = Value

module ValSet = Set.Make(struct type t = V.value let compare = V.compare end)
module AtSet = Set.Make(struct type t = Source.region let compare = compare end)

type desc =
  | Any
  | Val of V.value
  | NotVal of ValSet.t
  | Tup of desc list

type ctxt =
  | InOpt of ctxt
  | InTup of ctxt * desc list * desc list * pat list * Type.typ list
  | InAlt1 of ctxt * Source.region * pat * Type.typ
  | InAlt2 of ctxt * Source.region
  | InCase of Source.region * case list * Type.typ

type sets =
  { mutable cases : AtSet.t;
    mutable alts : AtSet.t;
    mutable reached_cases : AtSet.t;
    mutable reached_alts : AtSet.t;
  }


let make_sets () =
  { cases = AtSet.empty;
		alts = AtSet.empty;
		reached_cases = AtSet.empty;
    reached_alts = AtSet.empty;
  }


let value_of_lit = function
  | NullLit -> V.Null
  | BoolLit b -> V.Bool b
  | NatLit n -> V.Nat n
  | IntLit i -> V.Int i
  | Word8Lit w -> V.Word8 w
  | Word16Lit w -> V.Word16 w
  | Word32Lit w -> V.Word32 w
  | Word64Lit w -> V.Word64 w
  | FloatLit z -> V.Float z
  | CharLit c -> V.Char c
  | TextLit t -> V.Text t
  | PreLit _ -> assert false


let skip_pat pat sets =
  sets.alts <- AtSet.add pat.at sets.alts;
  true

let rec match_pat ce ctxt desc pat t sets =
  Type.span ce t = Some 0 && skip_pat pat sets ||
  match pat.it with
  | WildP | VarP _ ->
    succeed ce ctxt desc sets
  | LitP lit ->
    match_lit ce ctxt desc (value_of_lit !lit) t sets
  | SignP (op, lit) ->
    let f = Operator.unop pat.note.note_typ op in
    match_lit ce ctxt desc (f (value_of_lit !lit)) t sets
  | TupP pats ->
    let descs, ts =
      match desc, Type.promote ce t with
      | Tup descs, Type.Tup ts -> descs, ts
      | Any, Type.Tup ts -> List.map (fun _ -> Any) pats, ts
      | _ -> assert false
    in match_tup ce ctxt [] descs pats ts sets
	| OptP pat1 ->
    match_pat ce (InOpt ctxt) desc pat1 t sets
  | AltP (pat1, pat2) ->
    sets.alts <- AtSet.add pat1.at (AtSet.add pat2.at sets.alts);
    match_pat ce (InAlt1 (ctxt, pat1.at, pat2, t)) desc pat1 t sets
  | AnnotP (pat1, _) ->
    match_pat ce ctxt desc pat1 t sets

and match_lit ce ctxt desc v t sets =
  let desc_succ = Val v in
  let desc_fail vs = NotVal (ValSet.add v vs) in
  match desc with
  | Any ->
    if Type.span ce t = Some 1 then
      succeed ce ctxt desc_succ sets
    else
      succeed ce ctxt desc_succ sets &&
      fail ce ctxt (desc_fail ValSet.empty) sets
  | Val v' ->
    if v = v'
    then succeed ce ctxt desc sets
    else fail ce ctxt desc sets
  | NotVal vs ->
    if ValSet.mem v vs then
      fail ce ctxt desc sets
    else if Type.span ce t = Some (ValSet.cardinal vs + 1) then
      succeed ce ctxt desc_succ sets
    else
      succeed ce ctxt desc_succ sets && fail ce ctxt (desc_fail vs) sets
  | _ ->
    assert false

and match_tup ce ctxt descs_r descs pats ts sets =
  match descs, pats, ts with
  | [], [], [] ->
    succeed ce ctxt (Tup (List.rev descs_r)) sets
  | desc::descs', pat::pats', t::ts' ->
    match_pat ce (InTup (ctxt, descs_r, descs', pats', ts')) desc pat t sets
  | _ ->
    assert false
    

and succeed ce ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    succeed ce ctxt' desc sets
  | InTup (ctxt', descs_r, descs, pats, ts) ->
    match_tup ce ctxt' (desc::descs_r) descs pats ts sets
  | InAlt1 (ctxt', at1, _pat2, _t) ->
    sets.reached_alts <- AtSet.add at1 sets.reached_alts;
    succeed ce ctxt' desc sets
  | InAlt2 (ctxt', at2) ->
    sets.reached_alts <- AtSet.add at2 sets.reached_alts;
    succeed ce ctxt' desc sets
  | InCase (at, cases, _t) ->
    sets.reached_cases <- AtSet.add at sets.reached_cases;
    skip cases sets

and skip cases sets : bool =
  match cases with
  | [] ->
    true
  | case::cases' ->
    sets.cases <- AtSet.add case.at sets.cases;
    skip cases' sets

and fail ce ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    fail ce ctxt' desc sets
  | InTup (ctxt', descs', descs, pats, _ts) ->
    fail ce ctxt' (Tup (List.rev descs' @ [desc] @ descs)) sets
  | InAlt1 (ctxt', at1, pat2, t) ->
    match_pat ce (InAlt2 (ctxt', pat2.at)) desc pat2 t sets
  | InAlt2 (ctxt', at2) ->
    fail ce ctxt' desc sets
  | InCase (at, [], t) ->
    Type.span ce t = Some 0
  | InCase (at, case::cases, t) ->
    Type.span ce t = Some 0 && skip (case::cases) sets ||
    match_pat ce (InCase (case.at, cases, t)) desc case.it.pat t sets


let warn at fmt =
	Printf.ksprintf (fun s ->
    if at <> Source.no_region then
      Printf.eprintf "%s: warning, %s\n%!" (Source.string_of_region at) s;
  ) fmt

let check_cases ce cases t : bool =
	let sets = make_sets () in
  let exhaustive = fail ce (InCase (Source.no_region, cases, t)) Any sets in
  let unreached_cases = AtSet.diff sets.cases sets.reached_cases in
  let unreached_alts = AtSet.diff sets.alts sets.reached_alts in
  AtSet.iter (fun at -> warn at "this case is never reached") unreached_cases;
  AtSet.iter (fun at -> warn at "this pattern is never matched")
    unreached_alts;
  exhaustive

let (@?) it at = {it; at; note = empty_typ_note}

let check_pat ce pat t : bool =
	check_cases ce
	  [{pat; exp = TupE [] @? Source.no_region} @@ Source.no_region] t
