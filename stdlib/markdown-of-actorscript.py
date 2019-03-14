#!/usr/bin/env python
import sys
import re

#showActorScript=True
showActorScript=False

ActorScript="ActorScript"
Markdown="Markdown"
Comment="Comment"
modeType=[ActorScript, Markdown, Comment]

# eventually, detect this based on the input file name and handle the other way around:
outerMode = ActorScript
ignoreNonMarkdownComments= False

mode = outerMode
modeOpen="```ActorScript"
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
        if (showActorScript or mode != ActorScript):
            #debug "begin dumping", mode, "content:"
            print modeOpen
            if  modeLines[-1] == "":
                modeLines.pop()
                print ""
            for l in modeLines:
                print(l.rstrip())
            print modeClose
            #debug "end dumping", mode, "content."
        else:
            #debug "omitting ActorScript"
            print "```"
            print "..."
            print "```"

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
        if re.match(r'/\*\*', line.lstrip()):
            switchModeTo(Markdown, "", "")

        # Start ordinary comment
        elif re.match(r'/\*', line.lstrip()):
            switchModeTo(Comment, "/* ", "*/")

        # Horizontal rule, in actorscript code
        elif re.match(r'//////////+', line.lstrip()):
            # Horizontal rule in ActorScript
            if mode == ActorScript:
                if len(modeLines) > 0:
                    modeLines.append("```")
                    modeLines.append("-----------------------------------------------------------------------")
                    modeLines.append("```")
                else:
                    assert True

        # One-line comment
        elif re.match(r'//+ \S*\s*', line.lstrip()):
            matches = re.split(r'//+ ', line.lstrip())
            if mode == Markdown:
                print (matches[1]).rstrip()
            elif mode == ActorScript:
                if ignoreNonMarkdownComments:
                    #debug "ignoring single-line comment."
                    assert True
                else:
                    #debug "append single-line comment (", mode, "): `", line.rstrip(), "`"
                    modeLines.append(line.rstrip())
            elif mode == Comment:
                modeLines.append(matches[1].rstrip())
            else:
                assert False

        # One-line comment, with no content
        elif re.match(r'//', line.lstrip()):
            whiteSpaceLine()

        # Close markdown or comment block
        elif re.match(r'\*/', line.lstrip()):
            switchModeTo(ActorScript, "```ActorScript", "```")

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
