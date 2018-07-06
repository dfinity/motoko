.PHONY: all clean test


MENHIR := menhir 

MENHIRFLAGS     := --infer --dump --explain

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -I src -I lib -pkg wasm

MAIN            := main

all: sanity
	$(OCAMLBUILD) $(MAIN).native

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	$(MAKE) -C test all


sanity:
	ocamlfind query wasm
