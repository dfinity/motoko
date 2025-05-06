import (t : Text) = "file:../index.html";
import (b : Blob) = "file:../index.html";
import c = "file:../index.html";

assert b == c
//MOC-FLAG -dl -v
