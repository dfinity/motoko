module Arrange = Mo_def.Arrange
module Js = Js_of_ocaml.Js
module Numerics = Mo_values.Numerics
module Operator = Mo_values.Operator
module Syntax = Mo_def.Syntax
module Type = Mo_types.Type
module Type_pretty = Mo_types.Type.MakePretty (Mo_types.Type.ElideStamps)

module type Config = Mo_def.Arrange.Config
module type TypConfig = Mo_types.Arrange_type.Config

(* AST to JSON conversion *)
module Make (Cfg : Config) = struct
  (* Creates JSON object for AST node *)
  let type_object (name : string) (args : Js.Unsafe.any array) =
    Js.Unsafe.coerce
      (object%js
         val name = Js.string name
         val args = Js.array args
      end)

  (* Converts string to JSON-string as [Js.Unsafe.any] *)
  let json_string (s : string) : Js.Unsafe.any =
    s |> Js.string |> Js.Unsafe.coerce

  module type ToJSON = sig
    type t

    val to_json : t -> Js.Unsafe.any
  end

  module Pos : ToJSON with type t = Source.pos = struct
    type t = Source.pos

    let to_json p =
      let open Source in
      let file =
        match Cfg.main_file with Some f when f <> p.file -> p.file | _ -> ""
      in
      type_object "Pos"
        (Array.map json_string
           [| file; string_of_int p.line; string_of_int p.column |])
  end

  module Prim : ToJSON with type t = Type.prim = struct
    type t = Type.prim

    let to_json p =
      let open Type in
      (match p with
      | Null -> "Null"
      | Bool -> "Bool"
      | Nat -> "Nat"
      | Nat8 -> "Nat8"
      | Nat16 -> "Nat16"
      | Nat32 -> "Nat32"
      | Nat64 -> "Nat64"
      | Int -> "Int"
      | Int8 -> "Int8"
      | Int16 -> "Int16"
      | Int32 -> "Int32"
      | Int64 -> "Int64"
      | Float -> "Float"
      | Char -> "Char"
      | Text -> "Text"
      | Blob -> "Blob"
      | Error -> "Error"
      | Principal -> "Principal"
      | Region -> "Region")
      |> json_string
  end

  module ObjSort : ToJSON with type t = Type.obj_sort = struct
    type t = Type.obj_sort

    let to_json o =
      let open Type in
      (match o with
      | Object -> "Object"
      | Actor -> "Actor"
      | Module -> "Module"
      | Memory -> "Memory")
      |> json_string
  end

  module FuncSort : ToJSON with type t = Type.shared_sort Type.shared = struct
    type t = Type.shared_sort Type.shared

    let to_json s =
      let open Type in
      (match s with
      | Local -> "Local"
      | Shared Write -> "Shared"
      | Shared Query -> "Shared Query"
      | Shared Composite -> "Shared Composite")
      |> json_string
  end

  module Control : ToJSON with type t = Type.control = struct
    type t = Type.control

    let to_json c =
      let open Type in
      (match c with
      | Returns -> "Returns"
      | Promises -> "Promises"
      | Replies -> "Replies")
      |> json_string
  end

  module Region : ToJSON with type t = Source.region = struct
    type t = Source.region

    let to_json r =
      let open Source in
      let filename = r.left.file in
      type_object "@@"
        [| json_string filename; Pos.to_json r.left; Pos.to_json r.right |]
  end

  module Mut : ToJSON with type t = Syntax.mut = struct
    type t = Syntax.mut

    let to_json m =
      let open Source in
      let open Syntax in
      json_string (match m.it with Const -> "Const" | Var -> "Var")
  end

  module MakeTyp (Self : ToJSON with type t = Type.typ) (Cfg : TypConfig) :
    ToJSON with type t = Type.typ = struct
    open Type

    type t = Type.typ

    let src ({ depr; track_region; region = r } : Type.src) : Js.Unsafe.any list
        =
      let srcs =
        match Cfg.srcs_tbl with
        | None -> []
        | Some srcs_tbl -> (
            let open Mo_types in
            match Field_sources.Srcs_map.find_opt track_region srcs_tbl with
            | None -> []
            | Some srcs ->
                List.of_seq @@ Seq.map Region.to_json
                @@ Source.Region_set.to_seq srcs)
      in
      json_string (Option.value ~default:"" depr) :: Region.to_json r :: srcs

    module Field : ToJSON with type t = Type.field = struct
      type t = Type.field

      let to_json { lab; typ = t; src = s } =
        type_object lab (Self.to_json t :: src s |> Array.of_list)
    end

    module Bind : ToJSON with type t = Type.bind = struct
      type t = Type.bind

      let to_json tb = type_object tb.var [| Self.to_json tb.bound |]
    end

    let to_json = function
      | Var (s, i) ->
          type_object "Var" (Array.map json_string [| s; string_of_int i |])
      | Con (c, ts) ->
          type_object "Con"
            (json_string (Type.string_of_con c) :: List.map Self.to_json ts
            |> Array.of_list)
      | Prim p -> type_object "Prim" [| Prim.to_json p |]
      | Obj (s, tfs) ->
          type_object "Obj"
            ([ ObjSort.to_json s ] @ List.map Field.to_json tfs |> Array.of_list)
      | Array t -> type_object "Array" [| Self.to_json t |]
      | Opt t -> type_object "Opt" [| Self.to_json t |]
      | Variant tfs ->
          type_object "Variant" (List.map Field.to_json tfs |> Array.of_list)
      | Tup ts -> type_object "Tup" (List.map Self.to_json ts |> Array.of_list)
      | Func (s, c, tbs, at, rt) ->
          type_object "Func"
            ([ FuncSort.to_json s; Control.to_json c ]
             @ List.map Bind.to_json tbs
             @ [
                 type_object "" (List.map Self.to_json at |> Array.of_list);
                 type_object "" (List.map Self.to_json rt |> Array.of_list);
               ]
            |> Array.of_list)
      | Async (Fut, t1, t2) ->
          type_object "Async" (Array.map Self.to_json [| t1; t2 |])
      | Async (Cmp, t1, t2) ->
          type_object "Async*" (Array.map Self.to_json [| t1; t2 |])
      | Mut t -> type_object "Mut" [| Self.to_json t |]
      | Any -> json_string "Any"
      | Non -> json_string "Non"
      | Pre -> json_string "Pre"
      | Typ c -> type_object "Typ" [| Type.string_of_con c |> json_string |]
      | Named (n, t) -> type_object "Name" [| json_string n; Self.to_json t |]
      | Weak t -> type_object "Weak" [| Self.to_json t |]
  end

  let add_type_annotation (t : Type.typ) (it : Js.Unsafe.any) : Js.Unsafe.any =
    if Cfg.include_types then
      match Cfg.include_type_rep with
      | Arrange.Without_type_rep ->
          type_object ":" [| it; Type_pretty.string_of_typ t |> json_string |]
      | Arrange.With_type_rep srcs_tbl ->
          let module Config = struct
            let srcs_tbl = srcs_tbl
          end in
          let module M = struct
            module rec Typ : (ToJSON with type t = Type.typ) =
              MakeTyp (Typ) (Config)
          end in
          type_object ":"
            [|
              it; Type_pretty.string_of_typ t |> json_string; M.Typ.to_json t;
            |]
    else it

  let add_source (at : Source.region) (it : Js.Unsafe.any) : Js.Unsafe.any =
    let open Source in
    if Cfg.include_sources && at <> Source.no_region then
      type_object "@" [| Pos.to_json at.left; Pos.to_json at.right; it |]
    else it

  let add_trivia (at : Source.region) (it : Js.Unsafe.any) : Js.Unsafe.any =
    match Cfg.include_docs with
    | Some table -> (
        let rec lookup_trivia (line, column) =
          Trivia.PosHashtbl.find_opt table Trivia.{ line; column }
        and find_trivia (parser_pos : Source.region) : Trivia.trivia_info =
          lookup_trivia Source.(parser_pos.left.line, parser_pos.left.column)
          |> Option.get
        in
        match Trivia.doc_comment_of_trivia_info (find_trivia at) with
        | Some s -> type_object "*" [| json_string s; it |]
        | None -> it)
    | None -> it

  let id i =
    let open Source in
    add_source i.at (type_object "ID" [| json_string i.it |])

  let rec path p =
    let open Source in
    let open Syntax in
    match p.it with
    | IdH i -> type_object "IdH" [| id i |]
    | DotH (p, i) -> type_object "DotH" [| path p; id i |]

  module Lit : ToJSON with type t = Syntax.lit = struct
    type t = Syntax.lit

    let to_json =
      let open Syntax in
      function
      | NullLit -> json_string "NullLit"
      | BoolLit true -> type_object "BoolLit" [| json_string "true" |]
      | BoolLit false -> type_object "BoolLit" [| json_string "false" |]
      | NatLit n ->
          type_object "NatLit"
            [| json_string (Numerics.Nat.to_pretty_string n) |]
      | Nat8Lit n ->
          type_object "Nat8Lit"
            [| json_string (Numerics.Nat8.to_pretty_string n) |]
      | Nat16Lit n ->
          type_object "Nat16Lit"
            [| json_string (Numerics.Nat16.to_pretty_string n) |]
      | Nat32Lit n ->
          type_object "Nat32Lit"
            [| json_string (Numerics.Nat32.to_pretty_string n) |]
      | Nat64Lit n ->
          type_object "Nat64Lit"
            [| json_string (Numerics.Nat64.to_pretty_string n) |]
      | IntLit i ->
          type_object "IntLit"
            [| json_string (Numerics.Int.to_pretty_string i) |]
      | Int8Lit i ->
          type_object "Int8Lit"
            [| json_string (Numerics.Int_8.to_pretty_string i) |]
      | Int16Lit i ->
          type_object "Int16Lit"
            [| json_string (Numerics.Int_16.to_pretty_string i) |]
      | Int32Lit i ->
          type_object "Int32Lit"
            [| json_string (Numerics.Int_32.to_pretty_string i) |]
      | Int64Lit i ->
          type_object "Int64Lit"
            [| json_string (Numerics.Int_64.to_pretty_string i) |]
      | FloatLit f ->
          type_object "FloatLit"
            [| json_string (Numerics.Float.to_pretty_string f) |]
      | CharLit c -> type_object "CharLit" [| json_string (string_of_int c) |]
      | TextLit t -> type_object "TextLit" [| json_string t |]
      | BlobLit b -> type_object "BlobLit" [| json_string b |]
      | PreLit (s, p) ->
          type_object "PreLit" [| json_string s; Prim.to_json p |]
  end

  module UnOp : ToJSON with type t = Operator.unop = struct
    type t = Operator.unop

    let to_json uo =
      let open Operator in
      json_string
        (match uo with PosOp -> "PosOp" | NegOp -> "NegOp" | NotOp -> "NotOp")
  end

  module BinOp : ToJSON with type t = Operator.binop = struct
    type t = Operator.binop

    let to_json bo =
      let open Operator in
      json_string
        (match bo with
        | AddOp -> "AddOp"
        | SubOp -> "SubOp"
        | MulOp -> "MulOp"
        | DivOp -> "DivOp"
        | ModOp -> "ModOp"
        | AndOp -> "AndOp"
        | OrOp -> "OrOp"
        | XorOp -> "XorOp"
        | ShLOp -> "ShiftLOp"
        | ShROp -> "ShiftROp"
        | RotLOp -> "RotLOp"
        | RotROp -> "RotROp"
        | CatOp -> "CatOp"
        | PowOp -> "PowOp"
        | WAddOp -> "WAddOp"
        | WSubOp -> "WSubOp"
        | WMulOp -> "WMulOp"
        | WPowOp -> "WPowOp")
  end

  module RelOp : ToJSON with type t = Operator.relop = struct
    type t = Operator.relop

    let to_json ro =
      let open Operator in
      json_string
        (match ro with
        | EqOp -> "EqOp"
        | NeqOp -> "NeqOp"
        | LtOp -> "LtOp"
        | GtOp -> "GtOp"
        | LeOp -> "LeOp"
        | GeOp -> "GeOp")
  end

  module rec Exp : (ToJSON with type t = Syntax.exp) = struct
    type t = Syntax.exp

    let to_json e =
      let open Syntax in
      let open Source in
      Exp'.to_json e |> add_type_annotation e.note.note_typ |> add_source e.at
  end

  and Exp' : (ToJSON with type t = Syntax.exp) = struct
    type t = Syntax.exp

    let exps (es : Syntax.exp list) : Js.Unsafe.any list =
      List.map Exp.to_json es

    let inst i =
      let open Source in
      match i.it with
      | None -> []
      | Some (false, ts) -> List.map Term.to_json ts
      | Some (true, ts) -> json_string "system" :: List.map Term.to_json ts

    let to_json e =
      let open Syntax in
      let open Source in
      match e.it with
      | VarE x -> type_object "VarE" [| id x |]
      | LitE l -> type_object "LitE" [| Lit.to_json !l |]
      | ActorUrlE e -> type_object "ActorUrlE" [| Exp.to_json e |]
      | UnE (ot, uo, e) ->
          type_object "UnE"
            [|
              Type.string_of_typ !ot |> json_string;
              UnOp.to_json uo;
              Exp.to_json e;
            |]
      | BinE (ot, e1, bo, e2) ->
          type_object "BinE"
            [|
              Type.string_of_typ !ot |> json_string;
              Exp.to_json e1;
              BinOp.to_json bo;
              Exp.to_json e2;
            |]
      | RelE (ot, e1, ro, e2) ->
          type_object "RelE"
            [|
              Type.string_of_typ !ot |> json_string;
              Exp.to_json e1;
              RelOp.to_json ro;
              Exp.to_json e2;
            |]
      | ShowE (ot, e) ->
          type_object "ShowE"
            [| Type.string_of_typ !ot |> json_string; Exp.to_json e |]
      | ToCandidE es -> type_object "ToCandidE" (exps es |> Array.of_list)
      | FromCandidE e -> type_object "FromCandidE" [| Exp.to_json e |]
      | TupE es -> type_object "TupE" (exps es |> Array.of_list)
      | ProjE (e, i) ->
          type_object "ProjE"
            [| Exp.to_json e; string_of_int i |> json_string |]
      | ObjBlockE (eo, s, nt, dfs) ->
          let l =
            [
              ObjSort.to_json s.it;
              (match nt with
              | None, None -> json_string "_"
              | None, Some t -> Term.to_json t
              | Some id, Some t ->
                  type_object id.it [| json_string ":"; Term.to_json t |]
              | Some id, None -> json_string id.it);
            ]
            @ List.map DecField.to_json dfs
          in
          type_object "ObjBlockE"
            ((if Cfg.include_parenthetical then
                (match eo with
                | None -> json_string "_"
                | Some e -> Exp.to_json e)
                :: l
              else l)
            |> Array.of_list)
      | ObjE ([], efs) ->
          type_object "ObjE" (List.map ExpField.to_json efs |> Array.of_list)
      | ObjE (bases, efs) ->
          type_object "ObjE"
            (exps bases @ [ json_string "with" ] @ List.map ExpField.to_json efs
            |> Array.of_list)
      | DotE (e, x) -> type_object "DotE" [| Exp.to_json e; id x |]
      | AssignE (e1, e2) ->
          type_object "AssignE" [| Exp.to_json e1; Exp.to_json e2 |]
      | ArrayE (m, es) ->
          type_object "ArrayE" ([ Mut.to_json m ] @ exps es |> Array.of_list)
      | IdxE (e1, e2) -> type_object "IdxE" [| Exp.to_json e1; Exp.to_json e2 |]
      | FuncE (x, sp, tp, p, t, sugar, e') ->
          type_object "FuncE"
            ([
               json_string (Type.string_of_typ e.note.note_typ);
               SharedPat.to_json sp;
               json_string x;
             ]
             @ List.map TypBind.to_json tp
             @ [
                 Pat.to_json p;
                 (match t with
                 | None -> json_string "_"
                 | Some t -> Term.to_json t);
                 json_string (if sugar then "" else "=");
                 Exp.to_json e';
               ]
            |> Array.of_list)
      | CallE (par_opt, e1, ts, e2) ->
          type_object "CallE"
            (let l = [ Exp.to_json e1 ] @ inst ts @ [ Exp.to_json e2 ] in
             (if Cfg.include_parenthetical then
                (match par_opt with
                | None -> json_string "_"
                | Some e -> Exp.to_json e)
                :: l
              else l)
             |> Array.of_list)
      | BlockE ds ->
          type_object "BlockE" (List.map Dec.to_json ds |> Array.of_list)
      | NotE e -> type_object "NotE" [| Exp.to_json e |]
      | AndE (e1, e2) -> type_object "AndE" [| Exp.to_json e1; Exp.to_json e2 |]
      | OrE (e1, e2) -> type_object "OrE" [| Exp.to_json e1; Exp.to_json e2 |]
      | ImpliesE (e1, e2) ->
          type_object "ImpliesE" [| Exp.to_json e1; Exp.to_json e2 |]
      | OldE e -> type_object "OldE" [| Exp.to_json e |]
      | IfE (e1, e2, e3) ->
          type_object "IfE" [| Exp.to_json e1; Exp.to_json e2; Exp.to_json e3 |]
      | SwitchE (e, cs) ->
          type_object "SwitchE"
            ([ Exp.to_json e ] @ List.map Case.to_json cs |> Array.of_list)
      | WhileE (e1, e2) ->
          type_object "WhileE" [| Exp.to_json e1; Exp.to_json e2 |]
      | LoopE (e1, None) -> type_object "LoopE" [| Exp.to_json e1 |]
      | LoopE (e1, Some e2) ->
          type_object "LoopE" [| Exp.to_json e1; Exp.to_json e2 |]
      | ForE (p, e1, e2) ->
          type_object "ForE" [| Pat.to_json p; Exp.to_json e1; Exp.to_json e2 |]
      | LabelE (i, t, e) ->
          type_object "LabelE" [| id i; Term.to_json t; Exp.to_json e |]
      | DebugE e -> type_object "DebugE" [| Exp.to_json e |]
      | BreakE (i, e) -> type_object "BreakE" [| id i; Exp.to_json e |]
      | RetE e -> type_object "RetE" [| Exp.to_json e |]
      | AsyncE (par_opt, Type.Fut, tb, e) ->
          type_object "AsyncE"
            (let l = [ TypBind.to_json tb; Exp.to_json e ] in
             (if Cfg.include_parenthetical then
                (match par_opt with
                | None -> json_string "_"
                | Some e -> Exp.to_json e)
                :: l
              else l)
             |> Array.of_list)
      | AsyncE (None, Type.Cmp, tb, e) ->
          type_object "AsyncE*" [| TypBind.to_json tb; Exp.to_json e |]
      | AsyncE (Some _, Type.Cmp, tb, e) -> assert false
      | AwaitE (Type.AwaitFut false, e) ->
          type_object "AwaitE" [| Exp.to_json e |]
      | AwaitE (Type.AwaitFut true, e) ->
          type_object "AwaitE?" [| Exp.to_json e |]
      | AwaitE (Type.AwaitCmp, e) -> type_object "AwaitE*" [| Exp.to_json e |]
      | AssertE (Runtime, e) -> type_object "AssertE" [| Exp.to_json e |]
      | AssertE (Static, e) -> type_object "Static_AssertE" [| Exp.to_json e |]
      | AssertE (Invariant, e) -> type_object "Invariant" [| Exp.to_json e |]
      | AssertE (Precondition, e) ->
          type_object "Precondition" [| Exp.to_json e |]
      | AssertE (Postcondition, e) ->
          type_object "Postcondition" [| Exp.to_json e |]
      | AssertE (Loop_entry, e) -> type_object "Loop_entry" [| Exp.to_json e |]
      | AssertE (Loop_continue, e) ->
          type_object "Loop_continue" [| Exp.to_json e |]
      | AssertE (Loop_exit, e) -> type_object "Loop_exit" [| Exp.to_json e |]
      | AssertE (Loop_invariant, e) ->
          type_object "Loop_invariant" [| Exp.to_json e |]
      | AssertE (Concurrency s, e) ->
          type_object ("Concurrency" ^ s) [| Exp.to_json e |]
      | AnnotE (e, t) ->
          type_object "AnnotE" [| Exp.to_json e; Term.to_json t |]
      | OptE e -> type_object "OptE" [| Exp.to_json e |]
      | DoOptE e -> type_object "DoOptE" [| Exp.to_json e |]
      | BangE e -> type_object "BangE" [| Exp.to_json e |]
      | TagE (i, e) -> type_object "TagE" [| id i; Exp.to_json e |]
      | PrimE p -> type_object "PrimE" [| json_string p |]
      | ImportE (f, _fp) -> type_object "ImportE" [| json_string f |]
      | ThrowE e -> type_object "ThrowE" [| Exp.to_json e |]
      | TryE (e, cs, None) ->
          type_object "TryE"
            ([ Exp.to_json e ] @ List.map Catch.to_json cs |> Array.of_list)
      | TryE (e, cs, Some f) ->
          type_object "TryE"
            ([ Exp.to_json e ]
             @ List.map Catch.to_json cs
             @ (json_string ";" :: [ Exp.to_json f ])
            |> Array.of_list)
      | IgnoreE e -> type_object "IgnoreE" [| Exp.to_json e |]
  end

  and Term : (ToJSON with type t = Syntax.typ) = struct
    type t = Syntax.typ

    let to_json tt =
      let open Source in
      Term'.to_json tt.it |> add_type_annotation tt.note |> add_source tt.at
  end

  and Term' : (ToJSON with type t = Syntax.typ') = struct
    type t = Syntax.typ'

    let typ_item ((id, ty) : Syntax.typ_item) =
      let open Source in
      match id with
      | None -> [ Term.to_json ty ]
      | Some { it; _ } -> [ json_string it; Term.to_json ty ]

    let to_json =
      let open Syntax in
      let open Source in
      function
      | PathT (p, ts) ->
          type_object "PathT"
            ([ path p ] @ List.map Term.to_json ts |> Array.of_list)
      | PrimT p -> type_object "PrimT" [| json_string p |]
      | ObjT (s, ts) ->
          type_object "ObjT"
            ([ ObjSort.to_json s.it ] @ List.map TypField.to_json ts
            |> Array.of_list)
      | ArrayT (m, t) ->
          type_object "ArrayT" [| Mut.to_json m; Term.to_json t |]
      | OptT t -> type_object "OptT" [| Term.to_json t |]
      | VariantT cts ->
          type_object "VariantT" (List.map TypTag.to_json cts |> Array.of_list)
      | TupT ts ->
          type_object "TupT" (List.concat_map typ_item ts |> Array.of_list)
      | FuncT (s, tbs, at, rt) ->
          type_object "FuncT"
            ([ FuncSort.to_json s.it ]
             @ List.map TypBind.to_json tbs
             @ [ Term.to_json at; Term.to_json rt ]
            |> Array.of_list)
      | AsyncT (Type.Fut, t1, t2) ->
          type_object "AsyncT" [| Term.to_json t1; Term.to_json t2 |]
      | AsyncT (Type.Cmp, t1, t2) ->
          type_object "AsyncT*" [| Term.to_json t1; Term.to_json t2 |]
      | AndT (t1, t2) ->
          type_object "AndT" [| Term.to_json t1; Term.to_json t2 |]
      | OrT (t1, t2) -> type_object "OrT" [| Term.to_json t1; Term.to_json t2 |]
      | ParT t -> type_object "ParT" [| Term.to_json t |]
      | NamedT (id, t) ->
          type_object "NamedT" [| json_string id.it; Term.to_json t |]
      | WeakT t -> type_object "WeakT" [| Term.to_json t |]
  end

  and TypField : (ToJSON with type t = Syntax.typ_field) = struct
    type t = Syntax.typ_field

    let to_json tf =
      let open Source in
      TypField'.to_json tf.it |> add_source tf.at
  end

  and TypField' : (ToJSON with type t = Syntax.typ_field') = struct
    type t = Syntax.typ_field'

    let to_json =
      let open Syntax in
      function
      | ValF (lab, t, m) ->
          type_object "ValF" [| id lab; Term.to_json t; Mut.to_json m |]
      | TypF (lab, tbs, t) ->
          type_object "TypF"
            ((id lab :: List.map TypBind.to_json tbs) @ [ Term.to_json t ]
            |> Array.of_list)
  end

  and TypBind : (ToJSON with type t = Syntax.typ_bind) = struct
    type t = Syntax.typ_bind

    let to_json tb =
      let open Source in
      TypBind'.to_json tb.it |> add_source tb.at
  end

  and TypBind' : (ToJSON with type t = Syntax.typ_bind') = struct
    type t = Syntax.typ_bind'

    let to_json tb =
      let open Source in
      let open Syntax in
      type_object tb.var.it [| Term.to_json tb.bound |]
  end

  and TypTag : (ToJSON with type t = Syntax.typ_tag) = struct
    type t = Syntax.typ_tag

    let to_json tt =
      let open Source in
      TypTag'.to_json tt.it |> add_source tt.at
  end

  and TypTag' : (ToJSON with type t = Syntax.typ_tag') = struct
    type t = Syntax.typ_tag'

    let to_json tt =
      let open Source in
      let open Syntax in
      type_object tt.tag.it [| Term.to_json tt.typ |]
  end

  and DecField : (ToJSON with type t = Syntax.dec_field) = struct
    type t = Syntax.dec_field

    let to_json df =
      let open Source in
      DecField'.to_json df.it |> add_source df.at |> add_trivia df.at
  end

  and DecField' : (ToJSON with type t = Syntax.dec_field') = struct
    type t = Syntax.dec_field'

    let to_json df =
      let open Syntax in
      type_object "DecField"
        [| Dec.to_json df.dec; Vis.to_json df.vis; Stab.to_json df.stab |]
  end

  and Dec : (ToJSON with type t = Syntax.dec) = struct
    type t = Syntax.dec

    let to_json d =
      let open Source in
      Dec'.to_json d.it |> add_source d.at |> add_trivia d.at
  end

  and Dec' : (ToJSON with type t = Syntax.dec') = struct
    type t = Syntax.dec'

    let to_json =
      let open Syntax in
      let open Source in
      function
      | ExpD e -> type_object "ExpD" [| Exp.to_json e |]
      | LetD (p, e, Some f) ->
          type_object "LetD" [| Pat.to_json p; Exp.to_json e; Exp.to_json f |]
      | LetD (p, e, None) ->
          type_object "LetD" [| Pat.to_json p; Exp.to_json e |]
      | VarD (x, e) -> type_object "VarD" [| id x; Exp.to_json e |]
      | TypD (x, tp, t) ->
          type_object "TypD"
            ([ id x ] @ List.map TypBind.to_json tp @ [ Term.to_json t ]
            |> Array.of_list)
      | ClassD (eo, sp, s, x, tp, p, rt, i, dfs) ->
          let l =
            (SharedPat.to_json sp :: id x :: List.map TypBind.to_json tp)
            @ [
                Pat.to_json p;
                (match rt with
                | None -> json_string "_"
                | Some t -> Term.to_json t);
                ObjSort.to_json s.it;
                id i;
              ]
            @ List.map DecField.to_json dfs
          in
          type_object "ClassD"
            ((if Cfg.include_parenthetical then
                (match eo with
                | None -> json_string "_"
                | Some e -> Exp.to_json e)
                :: l
              else l)
            |> Array.of_list)
  end

  and Pat : (ToJSON with type t = Syntax.pat) = struct
    type t = Syntax.pat

    let to_json p =
      let open Source in
      Pat'.to_json p.it |> add_type_annotation p.note |> add_source p.at
  end

  and Pat' : (ToJSON with type t = Syntax.pat') = struct
    type t = Syntax.pat'

    let to_json =
      let open Syntax in
      let open Source in
      function
      | WildP -> json_string "WildP"
      | VarP x -> type_object "VarP" [| id x |]
      | TupP ps -> type_object "TupP" (List.map Pat.to_json ps |> Array.of_list)
      | ObjP ps ->
          type_object "ObjP" (List.map PatField.to_json ps |> Array.of_list)
      | AnnotP (p, t) ->
          type_object "AnnotP" [| Pat.to_json p; Term.to_json t |]
      | LitP l -> type_object "LitP" [| Lit.to_json !l |]
      | SignP (uo, l) ->
          type_object "SignP" [| UnOp.to_json uo; Lit.to_json !l |]
      | OptP p -> type_object "OptP" [| Pat.to_json p |]
      | TagP (i, p) ->
          type_object "TagP" [| json_string ("#" ^ i.it); Pat.to_json p |]
      | AltP (p1, p2) -> type_object "AltP" [| Pat.to_json p1; Pat.to_json p2 |]
      | ParP p -> type_object "ParP" [| Pat.to_json p |]
  end

  and PatField : (ToJSON with type t = Syntax.pat_field) = struct
    type t = Syntax.pat_field

    let to_json pf =
      let open Source in
      PatField'.to_json pf.it |> add_source pf.at
  end

  and PatField' : (ToJSON with type t = Syntax.pat_field') = struct
    type t = Syntax.pat_field'

    let to_json =
      let open Source in
      let open Syntax in
      function
      | ValPF (id, p) ->
          type_object "ValPF" [| json_string id.it; Pat.to_json p |]
      | TypPF id -> type_object "TypPF" [| json_string id.it |]
  end

  and SharedPat :
    (ToJSON
      with type t =
        ( (Type.shared_sort * Syntax.pat) Type.shared,
          unit )
        Source.annotated_phrase) = struct
    type t =
      ( (Type.shared_sort * Syntax.pat) Type.shared,
        unit )
      Source.annotated_phrase

    let to_json sp =
      let open Source in
      let open Type in
      match sp.it with
      | Local -> json_string "Local"
      | Shared (Write, p) -> type_object "Shared" [| Pat.to_json p |]
      | Shared (Query, p) -> type_object "Query" [| Pat.to_json p |]
      | Shared (Composite, p) -> type_object "Composite" [| Pat.to_json p |]
  end

  and Vis : (ToJSON with type t = Syntax.vis) = struct
    type t = Syntax.vis

    let to_json v =
      let open Syntax in
      let open Source in
      match v.it with
      | Public None -> json_string "Public"
      | Public (Some m) -> type_object "Public" [| json_string m |]
      | Private -> json_string "Private"
      | System -> json_string "System"
  end

  and Stab : (ToJSON with type t = Syntax.stab option) = struct
    type t = Syntax.stab option

    let to_json =
      let open Syntax in
      let open Source in
      function
      | None -> json_string "(Flexible)"
      | Some s -> (
          match s.it with
          | Flexible -> json_string "Flexible"
          | Stable -> json_string "Stable")
  end

  and ExpField : (ToJSON with type t = Syntax.exp_field) = struct
    type t = Syntax.exp_field

    let to_json ef =
      let open Source in
      ExpField'ToJJSON.to_json ef.it |> add_source ef.at
  end

  and ExpField'ToJJSON : (ToJSON with type t = Syntax.exp_field') = struct
    type t = Syntax.exp_field'

    let to_json ef =
      let open Syntax in
      type_object "ExpField"
        [| Mut.to_json ef.mut; id ef.id; Exp.to_json ef.exp |]
  end

  and Case : (ToJSON with type t = Syntax.case) = struct
    type t = Syntax.case

    let to_json c =
      let open Source in
      let open Syntax in
      type_object "case" [| Pat.to_json c.it.pat; Exp.to_json c.it.exp |]
      |> add_source c.at
  end

  and Catch : (ToJSON with type t = Syntax.case) = struct
    type t = Syntax.case

    let to_json c =
      let open Source in
      let open Syntax in
      type_object "catch" [| Pat.to_json c.it.pat; Exp.to_json c.it.exp |]
  end

  and Prog : (ToJSON with type t = Syntax.prog) = struct
    type t = Syntax.prog

    let to_json p =
      let open Source in
      type_object "Prog" (List.map Dec.to_json p.it |> Array.of_list)
  end
end

include Make (Arrange.Default)
