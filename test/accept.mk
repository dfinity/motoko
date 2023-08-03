.PHONY: %.accept

%.accept: %.mo
	../run.sh -a $(RUNFLAGS) $<
	@ git status -s $< ok
	@ echo STAGING: $< $(wildcard ok/$(basename $<).*)
	@ if [ -n "$(wildcard ok/$(basename $<).*)" ] \
	; then git add --ignore-errors --update $< $(wildcard ok/$(basename $<).*) \
	; fi
	@ git status -s $< $(wildcard ok/$(basename $<).*)
