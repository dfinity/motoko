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
	@echo "The following command should print 42:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).native


sanity:
	ocamlfind query wasm
