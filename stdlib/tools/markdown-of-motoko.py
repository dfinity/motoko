#!/usr/bin/env python

## Objective
## ----------
##
## The objective of this program is to "Invert" an Motoko source
## file into a corresponding Markdown file with snippets of
## Motoko.  The markdown becomes the "outer format" with
## Motoko (as code blocks) as the "inner format".  In this sense,
## the objective is to "invert" the inner/outer relationship, and
## preserve everything else that's sensible.
## 
## Design question: If we have the freedom to order the Decls of an
## Motoko file any way we choose, then what's the best order to
## tell a **narrative** version of the file?

## The immediate benefit of using this tool:
##
## - The stdlib directory is source code that documents itself to seed
##   WIP guide for Motoko; this workflow is possible via a
##   Makefile.
##
## - By exposing the Markdown on the outside, the outline/narrative
##   structure is primary, and can be hyperlinked for sharing and
##   remote discussions, e.g., as markdown files in a github repo.
##
## - By exposing the Markdown on the outside, the outline/narrative
##   structure is primary and we can explain how implementations of
##   the Produce Exchange and the standard library, and future
##   examples, work, in a "literate programming" style.  

## Assumptions
## ------------
##
## - KISS: This tool is a placeholder for a better tool that actually 
##         understands Motoko.  
##
## - KISS: This tool does not try to be intelligent 
##         when it can force you (the programmer) to do something only 
##         somewhat annoying, e.g.:
##
##     - KISS: Dont mix mode-switch patterns and other Motoko comments  on a single line
##     - KISS: Dont mix mode switches on a single line.
##


## Stable links
## -----------
## https://hydra.oregon.dfinity.build//job/dfinity-ci-build/motoko.pr-234/stdlib-reference/latest/download/1/doc/
##
## (PR 234 is the current PR for the standard library and produce exchange)
##

DOCURL="https://hydra.oregon.dfinity.build//job/dfinity-ci-build/motoko.pr-234/stdlib-reference/latest/download/1/doc/"

#############################################################################################################

import sys
import re

showMotoko=True
#showMotoko=False

OmitMotoko="OmitMotoko"
Motoko="Motoko"
Markdown="Markdown"
Comment="Comment"
Done="Done"

modeType=[Motoko, Markdown, Comment, OmitMotoko, Done]

# eventually, detect this `outerMode` based on the input file name and
# handle the other way around; for now, we assume only this way
# around:
outerMode = Motoko
ignoreNonMarkdownComments= False

mode = outerMode
modeOpen="```Motoko"
modeLines = []
modeClose="```"

def switchModeTo(toMode, toModeOpen, toModeClose):
    global mode
    global modeOpen
    global modeClose
    global modeLines

    if toMode == mode:
        return (False, [])

    if len(modeLines) > 0:
        # Normal case:
        if mode != OmitMotoko and (showMotoko or mode != Motoko):
            print modeOpen
            if  modeLines[-1] == "":
                modeLines.pop()
                print ""
            for l in modeLines:
                l = l.replace("$DOCURL", DOCURL);
                print(l.rstrip())
            print modeClose
        # The source file explicitly omitted this
        elif mode == OmitMotoko:
            print "```"
            print ". . . (selectively omitted, explicitly) . . ."
            print "```"  
        # the flag says no
        elif mode == Motoko and not showMotoko:
            print "```"
            print ". . . (all Motoko is omitted) . . ."
            print "```"
        # impossible!
        else:
            assert False        

    mode = toMode
    modeOpen = toModeOpen
    modeClose = toModeClose
    modeLines = []

# empty line, or just whitespace; 
def whiteSpaceLine():
    #debug "whitespace-only line, noted."
    # record if its not the first, or last thing we saw
    if len(modeLines) > 0 and modeLines[-1] != "":
        modeLines.append("")

with open(sys.argv[1], "r") as ins:
    for line in ins:
        #debug "read line (", mode, "): `", line.rstrip(), "`"

        # Now discriminate between lines that switch modes, and "ordinary lines"
        # For now, assume 0 or 1 mode switches per line; later, handle breaking those on the same line up

        # Start Markdown comment
        if re.match(r'/\*\*([^\*])+\*/', line.lstrip()):
            p = re.compile(r'(/\*\*)([^\*/]*)(\*/)')
            groups = p.match(line.lstrip()).groups()
            if len(groups) == 3:
                savedMode = mode
                savedOpen = modeOpen
                savedClose = modeClose            
                switchModeTo(Markdown, "", "")
                modeLines.append(groups[1])
                switchModeTo(savedMode, savedOpen, savedClose)
            else:
                # ignore the comment; no content
                assert True
            
        # Start Markdown comment
        elif re.match(r'/\*\*', line.lstrip()):
            switchModeTo(Markdown, "", "")

        # Start ordinary comment
        elif re.match(r'/\*', line.lstrip()):
            switchModeTo(Comment, "/* ", "*/")

        # Horizontal rule, in motoko code
        elif re.match(r'//////////+', line.lstrip()):
            # Horizontal rule in Motoko
            if mode == Motoko:
                if len(modeLines) > 0:
                    modeLines.append("```")
                    modeLines.append("-----------------------------------------------------------------------")
                    modeLines.append("```")
                else:
                    assert True

        # Close markdown or comment block and omit the next otherwise-Motoko block:
        elif re.match(r'//\s*@Omit:', line.lstrip()):
            switchModeTo(OmitMotoko, "", "")

        # One-line comment
        elif re.match(r'//+ \S*\s*', line.lstrip()):
            matches = re.split(r'//+ ', line.lstrip())
            if mode == Markdown:
                modeLines.append(matches[1].rstrip())
            elif mode == Motoko:
                if ignoreNonMarkdownComments:
                    #debug "ignoring single-line comment."
                    assert True
                else:
                    #debug "append single-line comment (", mode, "): `", line.rstrip(), "`"
                    modeLines.append(line.rstrip())
            elif mode == Comment:
                modeLines.append(matches[1].rstrip())
            elif mode == OmitMotoko:
                assert True
            else:
                assert False

        # One-line comment, with no content
        elif re.match(r'//', line.lstrip()):
            whiteSpaceLine()

        # Close markdown or comment block
        elif re.match(r'\*/', line.lstrip()):
            switchModeTo(Motoko, "```Motoko", "```")

        else:
            #debug "non-empty line"
            # non-empty line
            if re.match(r'\S', line.lstrip()):
                if mode == Comment:
                    # do nothing
                    #debug "ignore line (", mode, "): `", line.rstrip(), "`"
                    assert True
                else:
                    #debug "append line (", mode, "): `", line.rstrip(), "`"
                    modeLines.append(line.rstrip())
            else:
                whiteSpaceLine()

switchModeTo(Done, "", "")
