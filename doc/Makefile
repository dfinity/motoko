all: as-slides.html

SHELL=bash

as-slides.html : as-slides.md
	pandoc \
	  -t revealjs -s -o as-slides.html as-slides.md -V revealjs-url=https://revealjs.com \
	  -V theme=moon \
	  # -V width="\"100%\"" \
	  # -V height="\"100%\"" \
	  # -V margin=0 \
	  # # -V minScale=2 \
	  # # -V maxScale=2 
