NAME = asc
BUILD = byte
MAIN = main

OPAM_PACKAGES = wasm zarith

MENHIR = menhir
MENHIR_FLAGS = --infer --dump --explain

OCAML_FLAGS = -cflags '-w +a-4-27-30-42-44-45 -warn-error +a'
OCAMLBUILD = ocamlbuild $(OCAML_FLAGS) -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIR_FLAGS)" -I src -I lib $(OPAM_PACKAGES:%=-pkg %) -tags debug -plugin-tag "package(js_of_ocaml.ocamlbuild)"

.PHONY: all clean test

all: $(NAME) test

$(NAME): $(MAIN).$(BUILD)
	mv $< $@

$(MAIN).$(BUILD): sanity
	$(OCAMLBUILD) $@

sanity:
	ocamlfind query $(OPAM_PACKAGES)

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean

test: $(NAME)
	$(MAKE) -C test ASC=../$(NAME) all
	$(MAKE) -C samples ASC=../$(NAME) all

accept: $(NAME)
	$(MAKE) -C test ASC=../$(NAME) accept
