#!/usr/bin/env bash

set -e
set -o pipefail

obelisk -i $1 |
# Insert new line after def
sed -e 's/::= /&\n    /' |
# Transform
sed -f $(dirname "${BASH_SOURCE[0]}")/grammar.sed |
# Remove line breaks
sed  -e ':a' -e 'N' -e '$!ba' -e 's/\n\ \ \ \ \ \ */ /g'
