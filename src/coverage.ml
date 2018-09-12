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
  | InTup of ctxt * desc list * desc list * pat list
  | InAlt1 of ctxt * Source.region * pat
  | InAlt2 of ctxt * Source.region
  | InCase of Source.region * case list

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


let rec match_pat ctxt desc pat sets =
	match pat.it with
	| WildP | VarP _ ->
	  succeed ctxt desc sets
	| LitP lit ->
	  match_lit ctxt desc (value_of_lit !lit) sets
	| SignP (op, lit) ->
	  let f = Operator.unop pat.note.note_typ op in
	  match_lit ctxt desc (f (value_of_lit !lit)) sets
	| TupP pats ->
    let descs =
      match desc with
      | Tup descs -> descs
      | Any -> List.map (fun _ -> Any) pats
      | _ -> assert false
    in match_tup ctxt [] descs pats sets
	| OptP pat1 ->
    match_pat (InOpt ctxt) desc pat1 sets
  | AltP (pat1, pat2) ->
    sets.alts <- AtSet.add pat1.at (AtSet.add pat2.at sets.alts);
    match_pat (InAlt1 (ctxt, pat1.at, pat2)) desc pat1 sets
  | AnnotP (pat1, _) ->
    match_pat ctxt desc pat1 sets

and match_lit ctxt desc v sets =
  let desc_succ = Val v in
  let desc_fail vs = NotVal (ValSet.add v vs) in
  match desc with
  | Any ->
    succeed ctxt desc_succ sets && fail ctxt (desc_fail ValSet.empty) sets
  | Val v' ->
    if v = v'
    then succeed ctxt desc sets
    else fail ctxt desc sets
  | NotVal vs ->
    if ValSet.mem v vs
    then fail ctxt desc sets
    else succeed ctxt desc_succ sets && fail ctxt (desc_fail vs) sets
  | _ ->
    assert false

and match_tup ctxt descs_r descs pats sets =
  match descs, pats with
  | [], [] ->
    succeed ctxt (Tup (List.rev descs_r)) sets
  | desc::descs', pat::pats' ->
    match_pat (InTup (ctxt, descs_r, descs', pats')) desc pat sets
  | _ ->
    assert false
    

and succeed ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    succeed ctxt' desc sets
  | InTup (ctxt', descs_r, descs, pats) ->
    match_tup ctxt' (desc::descs_r) descs pats sets
  | InAlt1 (ctxt', at1, pat2) ->
    sets.reached_alts <- AtSet.add at1 sets.reached_alts;
    succeed ctxt' desc sets
  | InAlt2 (ctxt', at2) ->
    sets.reached_alts <- AtSet.add at2 sets.reached_alts;
    succeed ctxt' desc sets
  | InCase (at, cases) ->
    sets.reached_cases <- AtSet.add at sets.reached_cases;
    skip cases sets

and skip cases sets : bool =
  match cases with
  | [] ->
    true
  | case::cases' ->
    sets.cases <- AtSet.add case.at sets.cases;
    skip cases' sets

and fail ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    fail ctxt' desc sets
  | InTup (ctxt', descs', descs, pats) ->
    fail ctxt' (Tup (List.rev descs' @ [desc] @ descs)) sets
  | InAlt1 (ctxt', at1, pat2) ->
    match_pat (InAlt2 (ctxt', pat2.at)) desc pat2 sets
  | InAlt2 (ctxt', at2) ->
    fail ctxt' desc sets
  | InCase (at, []) ->
    false
  | InCase (at, case::cases) ->
    match_pat (InCase (case.at, cases)) desc case.it.pat sets


let warn at fmt =
	Printf.ksprintf (fun s ->
    Printf.eprintf "%s: warning, %s\n" (Source.string_of_region at) s;
  ) fmt

let check_cases cases : bool =
	let sets = make_sets () in
  let exhaustive = fail (InCase (Source.no_region, cases)) Any sets in
  let unreached_cases = AtSet.diff sets.cases sets.reached_cases in
  let unreached_alts = AtSet.diff sets.alts sets.reached_alts in
  AtSet.iter (fun at -> warn at "this case is unreachable") unreached_cases;
  AtSet.iter (fun at -> warn at "this pattern alternative is unreachable")
    unreached_alts;
  exhaustive

let (@?) it at = {it; at; note = empty_typ_note}

let check_pat pat : bool =
	check_cases [{pat; exp = TupE [] @? pat.at} @@ pat.at]
