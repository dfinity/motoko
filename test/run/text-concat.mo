assert ("Hello " # "World!" == "Hello World!");

assert (
  "This is a test of string " # "conctatentation"
  ==
  "This is a " # "test of string conctatentation"
);
assert (
  "This is a " # "test of string conctatentation"
  ==
  "This is a test of string " # "conctatentation"
);

assert (
  "This is a test of string " # "conctatentation"
  <
  "Tis is a " # "test of string conctatentation with " # "more text"
);

assert (
  "This is a " # "test of string conctatentation with " # "more text"
  >
  "This is a test of string " # "conctatentation"
);
