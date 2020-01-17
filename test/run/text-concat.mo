assert ("Hello " # "World!" == "Hello World!");

assert (
  "This is a test of string " # "concatenation"
  ==
  "This is a " # "test of string concatenation"
);
assert (
  "This is a " # "test of string concatenation"
  ==
  "This is a test of string " # "concatenation"
);

assert (
  "This is a test of string " # "concatenation"
  <
  "This is a " # "test of string concatenation with " # "more text"
);

assert (
  "This is a " # "test of string concatenation with " # "more text"
  >
  "This is a test of string " # "concatenation"
);
