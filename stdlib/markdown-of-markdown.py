#!/usr/bin/env python

## Stable links
## -----------

DOCURL="https://hydra.oregon.dfinity.build//job/dfinity-ci-build/actorscript/stdlib-reference/latest/download/1/doc/"

#############################################################################################################

import sys
import re

with open(sys.argv[1], "r") as ins:
    for line in ins:
        line = line.replace("$DOCURL", DOCURL);
        print line.rstrip()
