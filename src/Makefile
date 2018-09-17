NAME = asc
BUILD = byte
MAIN = main

OPAM_PACKAGES = wasm num
JS_OPAM_PACKAGES = js_of_ocaml js_of_ocaml-ppx

MENHIR = menhir
MENHIR_FLAGS = --infer --dump --explain

OCAML_FLAGS = -cflags '-w +a-4-27-30-42-44-45 -warn-error +a'
OCAMLBUILD = ocamlbuild $(OCAML_FLAGS) -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIR_FLAGS)" -I src -I lib $(OPAM_PACKAGES:%=-pkg %) -tags debug

.PHONY: all clean test

all: $(NAME) test

$(NAME): $(MAIN).$(BUILD)
	mv $< $@

$(NAME).js: js_main.byte
	js_of_ocaml +nat.js $< -o $@

$(MAIN).$(BUILD): sanity
	$(OCAMLBUILD) $@

js_main.byte: js_main.ml
	$(OCAMLBUILD) $(JS_OPAM_PACKAGES:%=-pkg %) -plugin-tag "package(js_of_ocaml.ocamlbuild)" $@


sanity:
	ocamlfind query $(OPAM_PACKAGES)

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
	(cd test; make clean)

test: $(NAME)
	$(MAKE) -C test ASC=../$(NAME) all
	$(MAKE) -C samples ASC=../$(NAME) all

accept: $(NAME)
	$(MAKE) -C test ASC=../$(NAME) accept
