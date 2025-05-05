import (t : Text) = "file:../index.html";
import ((t0 : Text) : None) = "file:../index.html";
import (b : Blob) = "file:../index.html";
import ((b0 : Blob) : None) = "file:../index.html";
import c = "file:../index.html";
import (c0 : None) = "file:../index.html";

assert b == c
//MOC-FLAG -dl
