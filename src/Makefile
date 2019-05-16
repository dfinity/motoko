NAME = asc
BUILD = byte
MAIN = main
JS_MAIN = js_main

ASC = `pwd`/$(NAME)

OPAM_PACKAGES = wasm num vlq yojson bisect_ppx-ocamlbuild
JS_OPAM_PACKAGES = js_of_ocaml js_of_ocaml-ppx

MENHIR = menhir
MENHIR_FLAGS = --infer --dump --explain --strict

OCAML_FLAGS = -cflags '-w +a-4-27-30-42-44-45-58 -warn-error +a'
OCAMLBUILD = ocamlbuild $(OCAML_FLAGS) \
 -use-ocamlfind \
 -plugin-tag 'package(bisect_ppx-ocamlbuild)' \
 -use-menhir -menhir "$(MENHIR) $(MENHIR_FLAGS)" \
 -I src \
 -I lib \
 $(OPAM_PACKAGES:%=-pkg %) \
 -tags debug

.PHONY: all parallel quick clean test test-parallel test-quick

all: $(NAME) as-ld test

parallel: $(NAME) test-parallel
quick: $(NAME) test-quick

$(NAME): $(MAIN).$(BUILD)
	mv $< $@

as-ld: ld_main.$(BUILD)
	mv $< $@

$(NAME).js: $(JS_MAIN).byte
	js_of_ocaml +nat.js $< -o $@

%.$(BUILD): sanity
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
	$(MAKE) -C ../stdlib ASC=$(ASC) all
	$(MAKE) -C ../test ASC=$(ASC) all
	$(MAKE) -C ../samples ASC=$(ASC) all

accept: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) accept

test-parallel: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) parallel

test-quick: $(NAME)
	$(MAKE) -C ../test ASC=$(ASC) quick
