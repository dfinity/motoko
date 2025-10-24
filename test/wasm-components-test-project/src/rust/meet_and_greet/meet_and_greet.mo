module meet_and_greet {
    public func sayHello(guest_name : Text) : Text = ((prim "component:meet-and-greet:say-hello") : Text -> Text)(guest_name);
    public func sayBye(guest_name : Text, formal : Bool) : Text = ((prim "component:meet-and-greet:say-bye") : (Text, Bool) -> Text)(guest_name, formal);
    public func concat0() : Blob = ((prim "component:meet-and-greet:concat0") : () -> Blob)();
    public func concat2(a : Blob, b : Blob) : Blob = ((prim "component:meet-and-greet:concat2") : (Blob, Blob) -> Blob)(a, b);
    public func prim_bool(a : Bool) : Bool = ((prim "component:meet-and-greet:prim-bool") : Bool -> Bool)(a);
    public func prim_char(a : Char) : Char = ((prim "component:meet-and-greet:prim-char") : Char -> Char)(a);
    public func prim_u8(a : Nat8) : Nat8 = ((prim "component:meet-and-greet:prim-u8") : Nat8 -> Nat8)(a);
    public func prim_u16(a : Nat16) : Nat16 = ((prim "component:meet-and-greet:prim-u16") : Nat16 -> Nat16)(a);
    public func prim_u32(a : Nat32) : Nat32 = ((prim "component:meet-and-greet:prim-u32") : Nat32 -> Nat32)(a);
    public func prim_u64(a : Nat64) : Nat64 = ((prim "component:meet-and-greet:prim-u64") : Nat64 -> Nat64)(a);
    public func prim_i8(a : Int8) : Int8 = ((prim "component:meet-and-greet:prim-i8") : Int8 -> Int8)(a);
    public func prim_i16(a : Int16) : Int16 = ((prim "component:meet-and-greet:prim-i16") : Int16 -> Int16)(a);
    public func prim_i32(a : Int32) : Int32 = ((prim "component:meet-and-greet:prim-i32") : Int32 -> Int32)(a);
    public func prim_i64(a : Int64) : Int64 = ((prim "component:meet-and-greet:prim-i64") : Int64 -> Int64)(a);
    public func prim_f64(a : Float) : Float = ((prim "component:meet-and-greet:prim-f64") : Float -> Float)(a);
    public func prim_text_in(a : Text) : Int32 = ((prim "component:meet-and-greet:prim-string-in") : Text -> Int32)(a);
    public func prim_text_out() : Text = ((prim "component:meet-and-greet:prim-string-out") : () -> Text)();
    public func prim_text(a : Text) : Text = ((prim "component:meet-and-greet:prim-string") : Text -> Text)(a);

    public func vec_u16(a : [Nat16]) : Text = ((prim "component:meet-and-greet:vec-u16") : [Nat16] -> Text)(a);
    public func vec_text(a : [Text]) : Text = ((prim "component:meet-and-greet:vec-string") : [Text] -> Text)(a);
    public func vec_i8(a : [Int8]) : Text = ((prim "component:meet-and-greet:vec-i8") : [Int8] -> Text)(a);
    public func vec_u8_as_blob(a : Blob) : Text = ((prim "component:meet-and-greet:vec-u8-as-blob") : Blob -> Text)(a);
    public func vec_u32(a : [Nat32]) : Text = ((prim "component:meet-and-greet:vec-u32") : [Nat32] -> Text)(a);
    public func vec_i32(a : [Int32]) : Text = ((prim "component:meet-and-greet:vec-i32") : [Int32] -> Text)(a);
    public func vec_i64(a : [Int64]) : Text = ((prim "component:meet-and-greet:vec-i64") : [Int64] -> Text)(a);
    public func vec_bool(a : [Bool]) : Text = ((prim "component:meet-and-greet:vec-bool") : [Bool] -> Text)(a);
    public func vec_text_nested(a : [[Text]]) : Text = ((prim "component:meet-and-greet:vec-string-nested") : [[Text]] -> Text)(a);
    public func vec_char(a : [Char]) : Text = ((prim "component:meet-and-greet:vec-char") : [Char] -> Text)(a);
    public func vec_f64(a : [Float]) : Text = ((prim "component:meet-and-greet:vec-f64") : [Float] -> Text)(a);

    public func to_vec_bool(i : Int32, b : Bool) : [Bool] = ((prim "component:meet-and-greet:to-vec-bool") : (Int32, Bool) -> [Bool])(i, b);
    public func to_vec_char(u : Nat8, c : Char) : [Char] = ((prim "component:meet-and-greet:to-vec-char") : (Nat8, Char) -> [Char])(u, c);
    public func to_vec_i8(u : Nat16, c : Char) : [Int8] = ((prim "component:meet-and-greet:to-vec-i8") : (Nat16, Char) -> [Int8])(u, c);
    public func to_vec_u8_as_blob(u : Nat16, c : Char) : Blob = ((prim "component:meet-and-greet:to-vec-u8-as-blob") : (Nat16, Char) -> Blob)(u, c);
    public func to_vec_i16(u : Nat16, i : Int16) : [Int16] = ((prim "component:meet-and-greet:to-vec-i16") : (Nat16, Int16) -> [Int16])(u, i);
    public func to_vec_u32(u : Nat16, i : Nat32) : [Nat32] = ((prim "component:meet-and-greet:to-vec-u32") : (Nat16, Nat32) -> [Nat32])(u, i);
    public func to_vec_i64(u : Nat16, i : Int64) : [Int64] = ((prim "component:meet-and-greet:to-vec-i64") : (Nat16, Int64) -> [Int64])(u, i);
    public func to_vec_f64(u : Nat16, f : Float) : [Float] = ((prim "component:meet-and-greet:to-vec-f64") : (Nat16, Float) -> [Float])(u, f);
    public func to_vec_string(s1 : Text, s2 : Text) : [Text] = ((prim "component:meet-and-greet:to-vec-string") : (Text, Text) -> [Text])(s1, s2);
    public func to_vec_vec_simple() : [[Nat64]] = ((prim "component:meet-and-greet:to-vec-vec-simple") : () -> [[Nat64]])();
    public func to_vec_vec_u64(u : Nat64) : [[Nat64]] = ((prim "component:meet-and-greet:to-vec-vec-u64") : Nat64 -> [[Nat64]])(u);
    public func to_vec_vec(vec : [Nat64]) : [[Nat64]] = ((prim "component:meet-and-greet:to-vec-vec") : [Nat64] -> [[Nat64]])(vec);

    // Names and order of fields is important!
    public type V1 = {
        #def;
        #abc;
        #gh;
    };
    public type C = {
        #c : Text;
    };
    type Result<T, E> = {
        #ok : T;
        #err : E;
    };

    public func variant_in11(v : V1) : Text = ((prim "component:meet-and-greet:variant-in11") : V1 -> Text)(v);
    public func variant_in12(v1 : V1, v2 : V1) : Text = ((prim "component:meet-and-greet:variant-in12") : (V1, V1) -> Text)(v1, v2);
    public func variant_array_in(v : [V1]) : Text = ((prim "component:meet-and-greet:variant-array-in") : [V1] -> Text)(v);
    public func variant_result_same_in(v : Result<Nat16, Nat16>) : Text = ((prim "component:meet-and-greet:variant-result-same-in") : Result<Nat16, Nat16> -> Text)(v);
    public func variant_result_in(v : Result<Nat16, Text>) : Text = ((prim "component:meet-and-greet:variant-result-in") : Result<Nat16, Text> -> Text)(v);
    public func variant_string_in(v : C) : Text = ((prim "component:meet-and-greet:variant-string-in") : C -> Text)(v);
    public func variant_array_result_same_in(v : [Result<Nat16, Nat16>]) : Text = ((prim "component:meet-and-greet:variant-array-result-same-in") : [Result<Nat16, Nat16>] -> Text)(v);
    public func variant_array_result_in(v : [Result<Nat16, Text>]) : Text = ((prim "component:meet-and-greet:variant-array-result-in") : [Result<Nat16, Text>] -> Text)(v);

    public func variant11(v : V1) : V1 = ((prim "component:meet-and-greet:variant11") : V1 -> V1)(v);
    public func variant12(v1 : V1, v2 : V1) : V1 = ((prim "component:meet-and-greet:variant12") : (V1, V1) -> V1)(v1, v2);
    public func variant_array(v : [V1]) : [V1] = ((prim "component:meet-and-greet:variant-array") : [V1] -> [V1])(v);
    public func variant_result_same(v : Result<Nat16, Nat16>) : Result<Nat16, Nat16> = ((prim "component:meet-and-greet:variant-result-same") : Result<Nat16, Nat16> -> Result<Nat16, Nat16>)(v);
    public func variant_result(v : Result<Nat16, Text>) : Result<Nat16, Text> = ((prim "component:meet-and-greet:variant-result") : Result<Nat16, Text> -> Result<Nat16, Text>)(v);
    public func variant_string(v : C) : C = ((prim "component:meet-and-greet:variant-string") : C -> C)(v);
    public func variant_array_result_same(v : [Result<Nat16, Nat16>]) : [Result<Nat16, Nat16>] = ((prim "component:meet-and-greet:variant-array-result-same") : [Result<Nat16, Nat16>] -> [Result<Nat16, Nat16>])(v);
    public func variant_array_result(v : [Result<Nat16, Text>]) : [Result<Nat16, Text>] = ((prim "component:meet-and-greet:variant-array-result") : [Result<Nat16, Text>] -> [Result<Nat16, Text>])(v);

    public func nested_variant1(v : Result<Result<C, Text>, V1>) : Result<Result<C, Text>, V1> = ((prim "component:meet-and-greet:nested-variant1") : Result<Result<C, Text>, V1> -> Result<Result<C, Text>, V1>)(v);
    public func nested_variant2(v : Result<Result<V1, Text>, C>) : Result<Result<V1, Text>, C> = ((prim "component:meet-and-greet:nested-variant2") : Result<Result<V1, Text>, C> -> Result<Result<V1, Text>, C>)(v);

    // public func option_string(v : ?Text) : ?Text = ((prim "component:meet-and-greet:option-string") : ?Text -> ?Text)(v);
    // public func options_array(v : ?[?Text]) : ?[?Text] = ((prim "component:meet-and-greet:options-array") : ?[?Text] -> ?[?Text])(v);

    public func tuple_string_u64(v : (Text, Nat64)) : (Text, Nat64) = ((prim "component:meet-and-greet:tuple-string-u64") : (Text, Nat64) -> (Text, Nat64))(v);
    public func tuple_variant_array_result(v : (V1, [Text], Result<Nat16, Text>)) : (V1, [Text], Result<Nat16, Text>) = ((prim "component:meet-and-greet:tuple-variant-array-result") : (V1, [Text], Result<Nat16, Text>) -> (V1, [Text], Result<Nat16, Text>))(v);
    public func tuples_nested1(v1 : (Bool, (Nat8, Nat16)), v2 : ((Nat8, Nat16), Nat32)) : ((Bool, Nat32), (Nat8, Nat32)) = ((prim "component:meet-and-greet:tuples-nested1") : ((Bool, (Nat8, Nat16)), ((Nat8, Nat16), Nat32)) -> ((Bool, Nat32), (Nat8, Nat32)))(v1, v2);
    public func tuples_nested(v1 : (Bool, (Nat8, Nat16)), v2 : ((Nat8, Nat16), Nat64)) : ((Bool, Nat32), (Nat8, Nat64)) = ((prim "component:meet-and-greet:tuples-nested") : ((Bool, (Nat8, Nat16)), ((Nat8, Nat16), Nat64)) -> ((Bool, Nat32), (Nat8, Nat64)))(v1, v2);

    public func unit() : () = ((prim "component:meet-and-greet:unit") : () -> ())();
    public func unit_result(v : Result<(), ()>) : Result<(), ()> = ((prim "component:meet-and-greet:unit-result") : Result<(), ()> -> Result<(), ()>)(v);
    public func unit_result_er(v : Result<(), Text>) : Result<(), Text> = ((prim "component:meet-and-greet:unit-result-er") : Result<(), Text> -> Result<(), Text>)(v);
    public func unit_result_ok(v : Result<Text, ()>) : Result<Text, ()> = ((prim "component:meet-and-greet:unit-result-ok") : Result<Text, ()> -> Result<Text, ()>)(v);
};
