.PHONY: all clean test


MENHIR := menhir 

MENHIRFLAGS     := --infer --dump --explain

FLAGS			:= -cflags '-w +a-4-27-42-44-45' # -warn-error +a'
OCAMLBUILD      := ocamlbuild ${FLAGS} -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" -I src -I lib -pkg wasm

MAIN            := main
BUILD			:= native

all: sanity
	$(OCAMLBUILD) $(MAIN).${BUILD}

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: all
	$(MAKE) -C test ASC=../$(MAIN).${BUILD} all

sanity:
	ocamlfind query wasm
