#!/bin/bash
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} --print-deps lib/nested.mo
${MOC:-$(dirname "$BASH_SOURCE")/../../src/moc} --print-deps lib/actor-import.mo
