#!/usr/bin/env bash

set -e
set -o pipefail

obelisk -i $1 |
# MacOS sed can't handle \n in a regular way
sed -e '$s/::= /::=\\\n    /g' |
# Transform
sed -f $(dirname "$BASH_SOURCE")/grammar.sed |
# Remove line breaks
sed  -e ':a' -e 'N' -e '$!ba' -e 's/\n\ \ \ \ \ \ */ /g' |
# Transform uses ~~ to indicate erasure of this line, ~~ this and the following
sed -e ':a' -e 'N' -e '$!ba' -e 's/~~~~\n[ -z]*\n//g' |
sed -e ':a' -e 'N' -e '$!ba' -e 's/~~\n//g' |
# Compress superfluous newlines
sed -e ':a' -e 'N' -e '$!ba' -e 's/\n\n\n\n*/###/g' |
sed -e $'s/###/\\\n\\\n/g'
