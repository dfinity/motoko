#!/usr/bin/env python

## Stable links
## -----------
## https://hydra.oregon.dfinity.build//job/dfinity-ci-build/actorscript.pr-234/stdlib-reference/latest/download/1/doc/
##
## (PR 234 is the current PR for the standard library and produce exchange)
##

DOCURL="https://hydra.oregon.dfinity.build//job/dfinity-ci-build/actorscript.pr-234/stdlib-reference/latest/download/1/doc/"

#############################################################################################################

import sys
import re

with open(sys.argv[1], "r") as ins:
    for line in ins:
        line = line.replace("$DOCURL", DOCURL);
        print line.rstrip()
