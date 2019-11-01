import A "lib/hello-string.mo";
import B "lib/dir";

assert (A.hello == "Hello!");
assert (B.hello == "Hello!");
