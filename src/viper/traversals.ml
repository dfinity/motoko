open Mo_def
open Syntax
open Source

type visitor =
  { visit_exp : exp -> exp;
    visit_typ : typ -> typ;
    visit_pat : pat -> pat;
    visit_dec : dec -> dec;
    visit_inst : inst -> inst;
  }

let rec over_exp (v : visitor) (exp : exp) : exp =
  v.visit_exp (match exp.it with
  | ImportE _ | PrimE _ | VarE _ | LitE _ | ActorUrlE _ -> exp
  | UnE (x, y, exp1) -> { exp with it = UnE (x, y, over_exp v exp1) }
  | ShowE (x, exp1) -> { exp with it = ShowE (x, over_exp v exp1) }
  | ToCandidE exps ->  { exp with it = ToCandidE (List.map (over_exp v) exps) }
  | FromCandidE (exp1) -> { exp with it = FromCandidE (over_exp v exp1) }
  | ProjE (exp1, x) -> { exp with it = ProjE (over_exp v exp1, x) }
  | OptE exp1 -> { exp with it = OptE (over_exp v exp1) }
  | DoOptE exp1 -> { exp with it = DoOptE (over_exp v exp1) }
  | BangE exp1 -> { exp with it = BangE (over_exp v exp1) }
  | TagE (x, exp1) -> { exp with it = TagE (x, over_exp v exp1) }
  | DotE (exp1, x) -> { exp with it = DotE (over_exp v exp1, x) }
  | NotE exp1 -> { exp with it = NotE (over_exp v exp1) }
  | AssertE (how, exp1) -> { exp with it = AssertE (how, over_exp v exp1) }
  | LabelE (x, y, exp1) -> { exp with it = LabelE (x, y, over_exp v exp1) }
  | BreakE (x, exp1) -> { exp with it = BreakE (x, over_exp v exp1) }
  | RetE exp1 -> { exp with it = RetE (over_exp v exp1) }
  | AnnotE (exp1, t) -> { exp with it = AnnotE (over_exp v exp1, over_typ v t) }
  | AsyncE (s, tb, exp1) -> { exp with it = AsyncE (s, tb, over_exp v exp1) }
  | AwaitE (s, exp1) -> { exp with it = AwaitE (s, over_exp v exp1) }
  | ThrowE exp1 -> { exp with it = ThrowE (over_exp v exp1) }
  | BinE (x, exp1, y, exp2) -> { exp with it = BinE (x, over_exp v exp1, y, over_exp v exp2) }
  | IdxE (exp1, exp2) -> { exp with it = IdxE (over_exp v exp1, over_exp v exp2) }
  | RelE (x, exp1, y, exp2) -> { exp with it = RelE (x, over_exp v exp1, y, over_exp v exp2) }
  | AssignE (exp1, exp2) -> { exp with it = AssignE (over_exp v exp1, over_exp v exp2) }
  | CallE (exp1, inst, exp2) -> { exp with it = CallE (over_exp v exp1, over_inst v inst, over_exp v exp2) }
  | AndE (exp1, exp2) -> { exp with it = AndE (over_exp v exp1, over_exp v exp2) }
  | OrE (exp1, exp2) -> { exp with it = OrE (over_exp v exp1, over_exp v exp2) }
  | ImpliesE (exp1, exp2) -> { exp with it = ImpliesE (over_exp v exp1, over_exp v exp2) }
  | OldE exp1 -> { exp with it = OldE (over_exp v exp1) }
  | WhileE (exp1, exp2) -> { exp with it = WhileE (over_exp v exp1, over_exp v exp2) }
  | LoopE (exp1, exp2_opt) -> { exp with it = LoopE (over_exp v exp1, Option.map (over_exp v) exp2_opt) }
  | ForE (x, exp1, exp2) -> { exp with it = ForE (x, over_exp v exp1, over_exp v exp2) }
  | DebugE exp1 -> { exp with it = DebugE (over_exp v exp1) }
  | TupE exps -> { exp with it = TupE (List.map (over_exp v) exps) }
  | ArrayE (x, exps) -> { exp with it = ArrayE (x, List.map (over_exp v) exps) }
  | BlockE ds -> { exp with it = BlockE (List.map (over_dec v) ds) }
  | ObjBlockE (x, (n, t), dfs) -> { exp with it = ObjBlockE (x, (n, Option.map (over_typ v) t), List.map (over_dec_field v) dfs) }
  | ObjE (bases, efs) -> { exp with it = ObjE (List.map (over_exp v) bases, List.map (over_exp_field v) efs) }
  | IfE (exp1, exp2, exp3) -> { exp with it = IfE(over_exp v exp1, over_exp v exp2, over_exp v exp3) }
  | TryE (exp1, cases, exp2) -> { exp with it = TryE (over_exp v exp1, List.map (over_case v) cases, Option.map (over_exp v) exp2) }
  | SwitchE (exp1, cases) -> { exp with it = SwitchE (over_exp v exp1, List.map (over_case v) cases) }
  | FuncE (name, sort_pat, typ_binds, pat, typ_opt, sugar, exp1) -> { exp with it = FuncE (name, sort_pat, typ_binds, over_pat v pat, Option.map (over_typ v) typ_opt, sugar, over_exp v exp1) }
  | IgnoreE exp1 -> { exp with it = IgnoreE (over_exp v exp1)})

and over_typ (v : visitor) (t : typ) : typ = v.visit_typ t
and over_inst (v : visitor) (i : inst) : inst = v.visit_inst i

and over_pat (v : visitor) (p : pat) : pat =
  v.visit_pat (match p.it with
  | WildP | VarP _ | LitP _ | SignP _ -> p
  | TupP ps -> { p with it = TupP (List.map (over_pat v) ps) }
  | ObjP pfs -> { p with it = ObjP (List.map (over_pat_field v) pfs) }
  | OptP p1 -> { p with it = OptP (over_pat v p1) }
  | TagP (x, p1) -> { p with it = TagP (x, over_pat v p1) }
  | AltP (p1, p2) -> { p with it = AltP (over_pat v p1, over_pat v p2) }
  | AnnotP (p1, t) -> { p with it = AnnotP (over_pat v p1, over_typ v t) }
  | ParP p1 -> { p with it = ParP (over_pat v p1) })

and over_dec (v : visitor) (d : dec) : dec =
  v.visit_dec (match d.it with
  | TypD (tid, tbs, t) -> { d with it = TypD (tid, tbs, over_typ v t) }
  | ExpD e -> { d with it = ExpD (over_exp v e)}
  | VarD (x, e) -> { d with it = VarD (x, over_exp v e)}
  | LetD (p, e, fail) -> { d with it = LetD (over_pat v p, over_exp v e, Option.map (over_exp v) fail)}
  | ClassD (sp, cid, tbs, p, t_o, s, id, dfs) -> { d with it = ClassD (sp, cid, tbs, over_pat v p, Option.map (over_typ v) t_o, s, id, List.map (over_dec_field v) dfs)})

and over_dec_field (v : visitor) (df : dec_field) : dec_field =
  { df with it = { df.it with dec = over_dec v df.it.dec } }

and over_exp_field (v : visitor) (ef : exp_field) : exp_field =
  { ef with it = { ef.it with exp = over_exp v ef.it.exp } }

and over_pat_field (v : visitor) (pf : pat_field) : pat_field =
  { pf with it = { pf.it with pat = over_pat v pf.it.pat } }

and over_case (v : visitor) (case : case) : case =
  { case with it = { pat = over_pat v case.it.pat;
                     exp = over_exp v case.it.exp } }

and over_prog (v : visitor) (prog : prog) : prog =
  { prog with it = List.map (over_dec v) prog.it }
