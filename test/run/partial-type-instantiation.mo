// Test for partial type instantiation with wildcards

// Mock VarArray module for testing
module VarArray {
  public func map<T, R>(array : [var T], f : T -> R) : [var R] = [var];
};

// Test cases for partial type instantiation
let testArray = [var 1, 2, 3, 4, 5];

// Test 1: Full explicit instantiation (should work as before)
let result1 = VarArray.map<Nat, Nat>(testArray, func x = x * 2);

// Test 2: Partial instantiation with wildcard for first type argument
let result2 = VarArray.map<_, Nat>(testArray, func x = x * 2);

// Test 3: Partial instantiation with wildcard for second type argument  
let result3 = VarArray.map<Nat, _>(testArray, func x = x * 2);

// Test 4: Both wildcards (should infer all)
let result4 = VarArray.map<_, _>(testArray, func x = x * 2);

// Test 5: Mixed with different return types
let result5 = VarArray.map<_, Text>(testArray, func x = debug_show(x));

// Test 6: With Bool return type
let result6 = VarArray.map<_, Bool>(testArray, func x = x > 2);

// All results should be valid
result1;
result2; 
result3;
result4;
result5;
result6;
