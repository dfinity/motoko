//MOC-FLAG -fshared-code
func goNat32() {
 let x : Nat32 = 1 +% 1;
 var y : Nat32 = 1; y *%= 2;
 assert (x *% y == 4);
};
goNat32();


func goInt32() {
 let x : Int32 = 1 +% 1;
 var y : Int32 = 1; y *%= 2;
 assert (x *% y == 4);
};
goInt32();

func goNat64() {
 let x : Nat64 = 1 +% 1;
 var y : Nat64 = 1; y *%= 2;
 assert (x *% y == 4);
};
goNat64();

func goInt64() {
 let x : Int64 = 1 +% 1;
 var y : Int64 = 1; y *%= 2;
 assert (x *% y == 4);
};
goInt64();

func goFloat() {
 let x : Float = 1 + 1;
 var y : Float = 1; y *= 2;
 assert (x * y == 4);
};
goFloat();

// This just checks that the above CHECK-NOT: box_i32 is up-to-date
// (If that function gets renamed the test above could yeild false successes
func goValidNat32(x : Nat32) { assert (x *% x == 4); }; goValidNat32(2);
func goValidNat64(x : Nat64) { assert (x *% x == 4); }; goValidNat64(2);
func goValidFloat(x : Float) { assert (x * x == 4); }; goValidFloat(2);

// We have to do the filechecks in reverse order:

// FHECK: func $goValidFloat
// FHECK: f64.load
// FHECK: unreachable

// FHECK: func $goValidNat64
// FHECK: unbox_i64
// FHECK: unreachable

// FHECK: func $goValidNat32
// FHECK: unbox_i32
// FHECK: unreachable

// FHECK: func $goFloat
// FHECK-NOT: f64.load
// FHECK-STORE: f64.load
// FHECK: unreachable

// FHECK: func $goInt64
// FHECK-NOT: box_i64
// FHECK-NOT: unbox_i64
// FHECK: unreachable

// FHECK: func $goNat64
// FHECK-NOT: box_i64
// FHECK-NOT: unbox_i64
// FHECK: unreachable

// FHECK: func $goInt32
// FHECK-NOT: box_i32
// FHECK-NOT: unbox_i32
// FHECK: unreachable

// FHECK: func $goNat32
// FHECK-NOT: box_i32
// FHECK-NOT: unbox_i32
// FHECK: unreachable



// Also test that the value is included in a closure properly

func goCaptureNat32() : () -> () {
 let x : Nat32 = 1 +% 1;
 var y : Nat32 = 1 +% 1;
 return func() { assert(x == 2); assert(y == 2)};
};
goCaptureNat32()();

func goCaptureInt32() : () -> () {
 let x : Int32 = 1 +% 1;
 var y : Int32 = 1 +% 1;
 return func() { assert(x == 2); assert(y == 2)};
};
goCaptureInt32()();

func goCaptureNat64() : () -> () {
 let x : Nat64 = 1 +% 1;
 var y : Nat64 = 1 +% 1;
 return func() { assert(x == 2); assert(y == 2)};
};
goCaptureNat64()();

func goCaptureInt64() : () -> () {
 let x : Int64 = 1 +% 1;
 var y : Int64 = 1 +% 1;
 return func() { assert(x == 2); assert(y == 2)};
};
goCaptureInt64()();

func goCaptureFloat() : () -> () {
 let x : Float = 1 + 1;
 var y : Float = 1 + 1;
 return func() { assert(x == 2); assert(y == 2)};
};
goCaptureFloat()();
