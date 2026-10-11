module P = Codegen.Plugin_codegen
module C = Mo_values.Compute_plugin
module G = Codegen.InstrList
module T = Mo_types.Type
module V = Mo_values.Value

let local ins outs = T.Func (T.Local, T.Returns, [], ins, outs)

type operation = Identity | Apply | Apply0 | Apply2 | Split | Visit | Twice

let schema operation types = match operation, types with
  | Identity, [a] -> Ok ([a], a)
  | Apply, [a; b] -> Ok ([local [a] [b]; a], b)
  | Apply0, [a] -> Ok ([local [] [a]], a)
  | Apply2, [a; b; c] -> Ok ([local [a; b] [c]; a; b], c)
  | Split, [a; b; c] -> Ok ([local [a] [b; c]; a], T.Tup [b; c])
  | Visit, [a] -> Ok ([local [a] []; a], a)
  | Twice, [a] -> Ok ([local [a] [a]; a], a)
  | _ -> Error "invalid callback fixture type arguments"

module type Operation = sig val operation : operation end

module Generator (Op : Operation) = struct
  module Make (B : P.Backend) = struct
    let compile env specialization getters =
      match schema Op.operation specialization.C.type_arguments, getters with
      | Error message, _ -> B.E.trap_with env message
      | Ok _, [value] when Op.operation = Identity -> value
      | Ok (callback :: _, _), closure :: args ->
        let call = B.Call.local env callback closure args in
        (match Op.operation with
         | Visit ->
           (match args with
            | [value] -> G.concat [call; G.i Wasm_exts.Ast.Drop; value]
            | _ -> B.E.trap_with env "invalid fixtureVisit arguments")
         | Twice ->
           let set_result, get_result =
             B.new_local env "fixture_first_result" in
           G.concat [call; set_result;
             B.Call.local env callback closure [get_result]]
         | Identity | Apply | Apply0 | Apply2 | Split -> call)
      | _ -> B.E.trap_with env "invalid callback fixture schema"
  end
end

let evaluator operation evaluator types _arguments values k =
  match schema operation types, operation, values with
  | Ok _, Identity, [value] -> k (Ok value)
  | Ok (callback_type :: _, _), _, callback :: args ->
    evaluator.C.call callback_type callback args (function
      | Error message -> k (Error message)
      | Ok value ->
        (match operation, args with
         | Visit, [original] -> k (Ok original)
         | Twice, _ -> evaluator.C.call callback_type callback [value] k
         | _ -> k (Ok value)))
  | _ -> k (Error "invalid callback fixture arguments")

let binding id name operation parameters = C.{
  id;
  name;
  arguments = Fixed (fun types -> Result.map fst (schema operation types));
  type_parameters = List.init parameters (fun _ -> Opaque);
  result_type = (fun types _ -> Result.map snd (schema operation types));
  evaluate = evaluator operation;
}

let scalar types = match types with
  | [typ] ->
    (match T.promote typ with
     | T.Prim (T.Nat | T.Int | T.Nat8 | T.Int8 | T.Nat16 | T.Int16 |
         T.Nat32 | T.Int32 | T.Nat64 | T.Int64 | T.Bool | T.Char |
         T.Text | T.Blob | T.Principal as prim) -> Ok prim
     | _ -> Error "fixtureCompare requires a supported scalar type")
  | _ -> Error "invalid fixtureCompare type arguments"

module Compare = struct
  module Make (B : P.Backend) = struct
    let compile env specialization getters =
      match scalar specialization.C.type_arguments, getters with
      | Error message, _ -> B.E.trap_with env message
      | Ok prim, [left; right] ->
        G.concat [left; right;
          B.Compare.scalar env prim; B.BigNum.from_signed_word64 env]
      | _ -> B.E.trap_with env "invalid fixtureCompare arguments"
  end
end

let compare_binding = C.{
  id = "fixture_compare";
  name = "fixtureCompare";
  arguments = Fixed (fun types -> match scalar types, types with
    | Ok _, [typ] -> Ok [typ; typ]
    | Error message, _ -> Error message
    | _ -> Error "invalid fixtureCompare metadata");
  type_parameters = [Concrete];
  result_type = (fun types _ -> Result.map (fun _ -> T.int) (scalar types));
  evaluate = C.pure (fun _ _ -> function
    | [left; right] ->
      let order = compare (V.compare left right) 0 in
      Ok (V.Int (Mo_values.Numerics.Int.of_int order))
    | _ -> Error "invalid fixtureCompare values");
}

module True = struct
  module Make (B : P.Backend) = struct
    let compile _env _specialization _getters =
      (* RTS boolean bridges allow any nonzero representation of true. *)
      G.concat [B.compile_const_32 2l; B.Bool.from_rts_int32]
  end
end

let true_binding = C.{
  id = "fixture_true";
  name = "fixtureTrue";
  arguments = Fixed (fun _ -> Ok []);
  type_parameters = [];
  result_type = (fun _ _ -> Ok T.bool);
  evaluate = C.pure (fun _ _ _ -> Ok (V.Bool true));
}

let () = P.register P.{
  abi = 1;
  name = "fixture_callbacks";
  runtime = None;
  imports = [];
  bindings = [
    binding "fixture_identity" "fixtureIdentity" Identity 1,
      (module Generator (struct let operation = Identity end));
    binding "fixture_apply" "fixtureApply" Apply 2,
      (module Generator (struct let operation = Apply end));
    binding "fixture_apply0" "fixtureApply0" Apply0 1,
      (module Generator (struct let operation = Apply0 end));
    binding "fixture_apply2" "fixtureApply2" Apply2 3,
      (module Generator (struct let operation = Apply2 end));
    binding "fixture_split" "fixtureSplit" Split 3,
      (module Generator (struct let operation = Split end));
    binding "fixture_visit" "fixtureVisit" Visit 1,
      (module Generator (struct let operation = Visit end));
    binding "fixture_twice" "fixtureTwice" Twice 1,
      (module Generator (struct let operation = Twice end));
    compare_binding, (module Compare);
    true_binding, (module True);
  ];
}
