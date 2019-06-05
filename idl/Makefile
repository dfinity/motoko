NAME = didc
BUILD = byte
MAIN = main

DIDC = `pwd`/$(NAME)

OPAM_PACKAGES = wasm stdint num vlq yojson

MENHIR = menhir
MENHIR_FLAGS = --infer --dump --explain

OCAML_FLAGS = -cflags '-w +a-4-27-30-42-44-45-58 -warn-error +a'
OCAML_FLAGS = -cflags '-w'
OCAMLBUILD = ocamlbuild $(OCAML_FLAGS) \
 -use-ocamlfind \
 -use-menhir -menhir "$(MENHIR) $(MENHIR_FLAGS)" \
 $(OPAM_PACKAGES:%=-pkg %) \
 -tags debug

$(NAME): $(MAIN).$(BUILD)
	mv $< $@

$(MAIN).$(BUILD): sanity
	$(OCAMLBUILD) $@

sanity:
	ocamlfind query $(OPAM_PACKAGES)

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
	$(MAKE) -C ../test/idl clean

test: $(NAME)
	$(MAKE) -C ../test/idl DIDC=$(DIDC) all

