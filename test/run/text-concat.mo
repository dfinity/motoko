assert ("Hello " # "World!" == "Hello World!");

assert (
  "This is a test of string " # "concatentation"
  ==
  "This is a " # "test of string concatentation"
);
assert (
  "This is a " # "test of string concatentation"
  ==
  "This is a test of string " # "concatentation"
);

assert (
  "This is a test of string " # "concatentation"
  <
  "Tis is a " # "test of string concatentation with " # "more text"
);

assert (
  "This is a " # "test of string concatentation with " # "more text"
  >
  "This is a test of string " # "concatentation"
);
