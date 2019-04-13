open Syntax
open Source

module V = Value

module ValSet = Set.Make(struct type t = V.value let compare = V.compare end)
module AtSet = Set.Make(struct type t = Source.region let compare = compare end)
module TagSet = Set.Make(struct type t = string let compare = compare end)

type desc =
  | Any
  | Val of V.value
  | NotVal of ValSet.t
  | Tup of desc list
  | Opt of desc
  | Tag of desc * id
  | NotTag of TagSet.t

type ctxt =
  | InOpt of ctxt
  | InTag of ctxt * id
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
  | NatLit n -> V.Int n
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

let rec match_pat ctxt desc pat t sets =
  Type.span t = Some 0 && skip_pat pat sets ||
  match pat.it with
  | WildP | VarP _ ->
    succeed ctxt desc sets
  | LitP lit ->
    match_lit ctxt desc (value_of_lit !lit) t sets
  | SignP (op, lit) ->
    let f = Operator.unop pat.note op in
    match_lit ctxt desc (f (value_of_lit !lit)) t sets
  | TupP pats ->
    let ts = Type.as_tup (Type.promote t) in
    let descs =
      match desc with
      | Tup descs -> descs
      | Any -> List.map (fun _ -> Any) pats
      | _ -> assert false
    in match_tup ctxt [] descs pats ts sets
  | OptP pat1 ->
    let t' = Type.as_opt (Type.promote t) in
    (match desc with
    | Val Value.Null ->
      fail ctxt desc sets
    | NotVal vs when ValSet.mem Value.Null vs ->
      match_pat (InOpt ctxt) Any pat1 t' sets
    | Opt desc' ->
      match_pat (InOpt ctxt) desc' pat1 t' sets
    | Any ->
      fail ctxt (Val Value.Null) sets &&
      match_pat (InOpt ctxt) Any pat1 t' sets
    | _ -> assert false
    )
  | VariantP (id, pat1) ->
     begin
       let t' = List.assoc id.it (Type.as_variant (Type.promote t)) in
       match desc with
       | NotTag ts when TagSet.mem id.it ts ->
         fail ctxt desc sets
       | NotTag ts ->
         let explored = TagSet.add id.it ts in
         (vanishing (Type.promote t) explored || fail ctxt (NotTag explored) sets) &&
           match_pat (InTag (ctxt, id)) Any pat1 t' sets
       | Tag (desc', id') ->
         if id.it <> id'.it
         then fail ctxt desc sets
         else match_pat (InTag (ctxt, id)) desc' pat1 t' sets
       | Any ->
         fail ctxt (NotTag (TagSet.singleton id.it)) sets &&
           match_pat (InTag (ctxt, id)) Any pat1 t' sets
       | _ -> assert false
     end
  | AltP (pat1, pat2) ->
    sets.alts <- AtSet.add pat1.at (AtSet.add pat2.at sets.alts);
    match_pat (InAlt1 (ctxt, pat1.at, pat2, t)) desc pat1 t sets
  | AnnotP (pat1, _)
  | ParP pat1 ->
    match_pat ctxt desc pat1 t sets

and match_lit ctxt desc v t sets =
  let desc_succ = Val v in
  let desc_fail vs = NotVal (ValSet.add v vs) in
  match desc with
  | Any ->
    if Type.span t = Some 1 then
      succeed ctxt desc_succ sets
    else
      succeed ctxt desc_succ sets &&
      fail ctxt (desc_fail ValSet.empty) sets
  | Val v' ->
    if Value.equal v v'
    then succeed ctxt desc sets
    else fail ctxt desc sets
  | NotVal vs ->
    if ValSet.mem v vs then
      fail ctxt desc sets
    else if Type.span t = Some (ValSet.cardinal vs + 1) then
      succeed ctxt desc_succ sets
    else
      succeed ctxt desc_succ sets && fail ctxt (desc_fail vs) sets
  | Opt _ ->
    fail ctxt desc sets
  | _ ->
    assert false

and match_tup ctxt descs_r descs pats ts sets =
  match descs, pats, ts with
  | [], [], [] ->
    succeed ctxt (Tup (List.rev descs_r)) sets
  | desc::descs', pat::pats', t::ts' ->
    match_pat (InTup (ctxt, descs_r, descs', pats', ts')) desc pat t sets
  | _ ->
    assert false
    

and succeed ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    succeed ctxt' (Opt desc) sets
  | InTag (ctxt', t) ->
    succeed ctxt' (Tag (desc, t)) sets
  | InTup (ctxt', descs_r, descs, pats, ts) ->
    match_tup ctxt' (desc::descs_r) descs pats ts sets
  | InAlt1 (ctxt', at1, _pat2, _t) ->
    sets.reached_alts <- AtSet.add at1 sets.reached_alts;
    succeed ctxt' desc sets
  | InAlt2 (ctxt', at2) ->
    sets.reached_alts <- AtSet.add at2 sets.reached_alts;
    succeed ctxt' desc sets
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

and fail ctxt desc sets : bool =
  match ctxt with
  | InOpt ctxt' ->
    fail ctxt' (Opt desc) sets
  | InTag (ctxt', id) ->
    fail ctxt' (Tag (desc, id)) sets
  | InTup (ctxt', descs', descs, pats, _ts) ->
    fail ctxt' (Tup (List.rev descs' @ [desc] @ descs)) sets
  | InAlt1 (ctxt', at1, pat2, t) ->
    match_pat (InAlt2 (ctxt', pat2.at)) desc pat2 t sets
  | InAlt2 (ctxt', at2) ->
    fail ctxt' desc sets
  | InCase (at, [], t) ->
    Type.span t = Some 0
  | InCase (at, case::cases, t) ->
    Type.span t = Some 0 && skip (case::cases) sets ||
    match_pat (InCase (case.at, cases, t)) desc case.it.pat t sets

and vanishing t excluded =
  match t with
  | Type.Variant cts -> List.for_all (fun (l, _) -> TagSet.mem l excluded) cts
  | _ -> assert false

let warn at fmt =
	Printf.ksprintf (fun s ->
    if at <> Source.no_region then
      Printf.eprintf "%s: warning, %s\n%!" (Source.string_of_region at) s;
  ) fmt

let check_cases cases t =
  let sets = make_sets () in
  let exhaustive = fail (InCase (Source.no_region, cases, t)) Any sets in
  let unreached_cases = AtSet.diff sets.cases sets.reached_cases in
  let unreached_alts = AtSet.diff sets.alts sets.reached_alts in
  AtSet.iter (fun at -> warn at "this case is never reached") unreached_cases;
  AtSet.iter (fun at -> warn at "this pattern is never matched")
    unreached_alts;
  exhaustive

let (@?) it at = {it; at; note = empty_typ_note}

let check_pat pat t : bool =
  check_cases [{pat; exp = TupE [] @? Source.no_region} @@ Source.no_region] t
