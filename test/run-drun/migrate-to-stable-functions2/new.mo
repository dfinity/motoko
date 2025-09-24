import Prim "mo:prim";

persistent actor {
    // Stable function
    persistent func test() {
        Prim.debugPrint("Test");
    };

    var testFunction = test;
    testFunction();

    // Basic primitive types
    var _stableBool : Bool = true;
    var stableNat : Nat = 42;
    var _stableInt : Int = -17;
    var _stableNat8 : Nat8 = 255;
    var _stableNat16 : Nat16 = 65535;
    var _stableNat32 : Nat32 = 4294967295;
    var _stableNat64 : Nat64 = 18446744073709551615;
    var _stableInt8 : Int8 = -128;
    var _stableInt16 : Int16 = -32768;
    var _stableInt32 : Int32 = -2147483648;
    var _stableInt64 : Int64 = -9223372036854775808;
    var _stableFloat : Float = 3.14159;
    var _stableChar : Char = 'A';
    var stableText : Text = "Hello, Motoko!";

    // Principal and Blob
    var _stablePrincipal : Principal = Prim.principalOfBlob("\04\00\00\00\00\00\00\00\01\01");
    var _stableBlob : Blob = "\48\65\6c\6c\6f";

    // Optional types
    var stableOptNat : ?Nat = ?123;
    var _stableOptNull : ?Int = null;
    var _stableNestedOpt : ??Text = ??("nested");

    // Array types
    var _stableArray : [Nat] = [1, 2, 3, 4, 5];
    var _stableNestedArray : [[Int]] = [[1, -2], [3, -4], [5, -6]];
    var _stableMutableArray : [var Bool] = [var true, false, true];

    // Tuple types (nested)
    var _stableTuple2 : (Nat, Text) = (42, "answer");
    var _stableTuple3 : (Bool, Float, Char) = (false, 2.718, 'π');
    var _stableNestedTuple : ((Int, Text), (Bool, ?Nat)) = ((-1, "negative"), (true, ?99));

    // Record types (nested)
    var _stableRecord : {name: Text; age: Nat; active: Bool} = {
        name = "Alice";
        age = 30;
        active = true;
    };

    var _stableNestedRecord : {
        user: {id: Nat; email: Text};
        settings: {theme: Text; notifications: Bool};
        metadata: ?{created: Int; modified: Int}
    } = {
        user = {id = 1001; email = "alice@example.com"};
        settings = {theme = "dark"; notifications = false};
        metadata = ?{created = 1640995200; modified = 1640995200};
    };

    // Variant types
    type Color = {#Red; #Green; #Blue; #RGB: (Nat8, Nat8, Nat8)};
    var _stableVariant : Color = #RGB(255, 128, 0);

    type Result<T, E> = {#Ok: T; #Err: E};
    var _stableResult : Result<Text, Nat> = #Ok("success");
    var _stableResultErr : Result<Bool, Text> = #Err("failed");

    // Complex nested structure combining multiple types
    type ComplexData = {
        id: Principal;
        values: [?Int];
        config: {
            enabled: Bool;
            params: [(Text, Float)];
        };
        status: {#Active: Nat; #Inactive; #Error: Text};
        metadata: ?{
            tags: [Text];
            scores: [(Text, ?Float)];
        }
    };

    var _stableComplexData : ComplexData = {
        id = Prim.principalOfBlob("\04\00\00\00\00\00\00\00\01\02");
        values = [?1, null, ?(-5), ?100];
        config = {
            enabled = true;
            params = [("threshold", 0.95), ("timeout", 30.0)];
        };
        status = #Active(42);
        metadata = ?{
            tags = ["important", "test", "data"];
            scores = [("quality", ?0.85), ("relevance", null), ("priority", ?1.0)];
        }
    };

    // Function to demonstrate the types are accessible
    public query func getTypeInfo() : async {
        primitiveCount: Nat;
        hasComplexData: Bool;
        sampleText: Text;
    } {
        {
            primitiveCount = 14; // Number of primitive types allocated
            hasComplexData = true;
            sampleText = stableText;
        }
    };

    // Function to modify some values (to test mutability)
    public func updateValues(newNat: Nat, newText: Text) : async () {
        stableNat := newNat;
        stableText := newText;
        stableOptNat := ?newNat;
    };
}
