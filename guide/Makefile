all: index.html actorscript-guide.pdf

SHELL=bash

index.html: ../design/guide.md pandoc.css  inline-code.css
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --standalone \
	  --css pandoc.css \
	  --css solution.css \
	  --css inline-code.css \
	  --write=html5 \
	  -V lang=en-US \
	  $< -f gfm -o $@

TEX_OPTIONS=\
  --pdf-engine xelatex \
  -V documentclass=scrartcl \
  -V lang=en-US \
  -V classoption=DIV13 \
  -V colorlinks \
  -V links-as-notes \
  -V "mainfont=TeX Gyre Pagella" \
  -V "monofont=Inconsolata" \
  -V "mathfont=Latin Modern Math" \
  --include-in-header=<(echo '\usepackage{newunicodechar}') \
  --include-in-header=<(echo '\newfontfamily{\fallbackfont}{Unifont}[Scale=MatchLowercase]') \
  --include-in-header=<(echo '\DeclareTextFontCommand{\textfallback}{\fallbackfont}') \
  --include-in-header=<(echo '\newunicodechar{☆}{\textrm{\textfallback{☆}}}') \
  --include-in-header=<(echo '\newunicodechar{λ}{\textrm{\textfallback{λ}}}') \
  --include-in-header=<(echo '\newunicodechar{∘}{\textrm{\textfallback{∘}}}') \
  --include-in-header=<(echo '\newunicodechar{☃}{\textrm{\textfallback{☃}}}') \
  --include-in-header=<(echo '\newunicodechar{█}{\textrm{\textfallback{█}}}') \
  --include-in-header=<(echo '\newunicodechar{ツ}{\textrm{\textfallback{ツ}}}') \

actorscript-guide.pdf: ../design/guide.md
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  $(TEX_OPTIONS) \
	  $< -f gfm -o $@

as-slides.html : as-slides.md
	pandoc \
	  -t revealjs -s -o as-slides.html as-slides.md -V revealjs-url=https://revealjs.com \
	  -V title="\"ActorScript\"" \
	  -V theme=moon \
	  -V width="\"100%\"" \
	  -V height="\"100%\"" \
	  -V margin=0 \
	  # -V minScale=2 \
	  # -V maxScale=2 
