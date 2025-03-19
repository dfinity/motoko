#!/usr/bin/env bash

set -e
set -o pipefail

# workaround until https://github.com/Lelio-Brun/Obelisk/issues/15
TEMP_FILE="/tmp/$(basename $1)"
cp $1 ${TEMP_FILE}
sed -i 's/\[@recover\..*\]//' ${TEMP_FILE}

obelisk -i ${TEMP_FILE} |
# Insert new line after def
sed -e 's/::= /&\n    /' |
# Transform
sed -f $(dirname "${BASH_SOURCE[0]}")/grammar.sed |
# Remove line breaks
sed  -e ':a' -e 'N' -e '$!ba' -e 's/\n\ \ \ \ \ \ */ /g'
