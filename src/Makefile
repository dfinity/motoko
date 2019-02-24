NAME = asc
BUILD = byte
MAIN = main
JS_MAIN = js_main

ASC = `pwd`/$(NAME)

OPAM_PACKAGES = wasm num vlq yojson
JS_OPAM_PACKAGES = js_of_ocaml js_of_ocaml-ppx

MENHIR = menhir
MENHIR_FLAGS = --infer --dump --explain

OCAML_FLAGS = -cflags '-w +a-4-27-30-42-44-45 -warn-error +a'
OCAMLBUILD = ocamlbuild $(OCAML_FLAGS) \
 -use-ocamlfind \
 -plugin-tag 'package(bisect_ppx-ocamlbuild)' \
 -use-menhir -menhir "$(MENHIR) $(MENHIR_FLAGS)" \
 -I src \
 -I lib \
 $(OPAM_PACKAGES:%=-pkg %) \
 -tags debug

.PHONY: all quick clean test test-quick test-quicker

all: $(NAME) test

quick: $(NAME) test-quick
quicker: $(NAME) test-quicker

$(NAME): $(MAIN).$(BUILD)
	mv $< $@

$(NAME).js: $(JS_MAIN).byte
	js_of_ocaml +nat.js $< -o $@

$(MAIN).$(BUILD): sanity
	$(OCAMLBUILD) $@

$(JS_MAIN).byte: $(JS_MAIN).ml sanity
	$(OCAMLBUILD) $(JS_OPAM_PACKAGES:%=-pkg %) $@


sanity:
	ocamlfind query $(OPAM_PACKAGES)

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
	$(MAKE) -C ../test clean

test: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) all
	$(MAKE) -C ../samples ASC=$(ASC) all

accept: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) accept

test-quick: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) quick

test-quicker: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) quicker
