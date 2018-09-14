NAME = actorscript
MAIN = main
DIRS = figures
SRCFILES = $(wildcard *.cls *.tex *.bib Makefile)
DIRFILES = $(shell ls $(DIRS:%=%/*))
FILES = $(SRCFILES) $(DIRFILES)

BIB_LTX = '\\bibliography\{'
CITE_LTX = 'LaTeX Warning: Citation'
WARN_LTX = 'Rerun to get cross-references right|There were undefined references|There were undefined citations'

pdf:	$(NAME).pdf
zip:	$(NAME).zip
all:	pdf zip

$(MAIN).pdf: $(FILES)
	pdflatex $(MAIN).tex
	if egrep -s $(BIB_LTX) $(MAIN).tex; then \
	  bibtex $(MAIN) || { rm -f $(MAIN).pdf; exit 1; }; \
	  pdflatex $(MAIN).tex; \
	  if egrep -s $(CITE_LTX) $(MAIN).log; then \
	    bibtex $(MAIN) || { rm -f $(MAIN).pdf; exit 1; } \
	  fi; \
	  pdflatex $(MAIN).tex; \
	fi
	while egrep -s $(WARN_LTX) $(MAIN).log; do \
	  pdflatex $(MAIN).tex || { rm -f $(MAIN).pdf; exit 1; } \
	done

$(NAME).pdf: $(MAIN).pdf
	ln -f $< $@

$(NAME).zip: $(FILES)
	mkdir $(NAME)
	cp $(SRCFILES) $(NAME)
	for dir in $(DIRS); do \
	  mkdir $(NAME)/$$dir; \
	  cp $$dir/* $(NAME)/$$dir; \
	done
	zip -r $@ $(NAME)
	rm -rf $(NAME)

clean:
	rm -rf *.aux *.out *.bbl *.blg *.dvi *.log *.ps *.thm *.toc $(NAME) $(MAIN).pdf

distclean: clean
	rm -f *.pdf *.zip *.png *.html

