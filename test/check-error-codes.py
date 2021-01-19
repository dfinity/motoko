#!/usr/bin/env python3
# Checks uniqueness of error codes, and prints out the next available
# Checks that every registered code is exercised in a test
# Checks that every code exercised in a test is registered

import re
import glob
import sys

if len(sys.argv) != 2:
    print("""USAGE: python check-error-codes.py path/to/error-codes.ml
""")
    sys.exit(2)

registered_codes = set()
tested_codes = set()

# This list should only contain errors that are impossible or hard to
# exercise in our test suite
known_untested_codes = {
    "M0000", "M0004", "M0005", "M0006", "M0008", "M0009", "M0010",
    "M0011", "M0015", "M0020", "M0021", "M0022", "M0023", "M0025",
    "M0026", "M0027", "M0028", "M0029", "M0036", "M0040", "M0041",
    "M0042", "M0044", "M0053", "M0054", "M0063", "M0066", "M0067",
    "M0068", "M0070", "M0073", "M0075", "M0080", "M0082", "M0084",
    "M0088", "M0092", "M0094", "M0095", "M0099", "M0100", "M0102",
    "M0103", "M0105", "M0107", "M0108", "M0109", "M0110", "M0113",
    "M0115", "M0118", "M0124", "M0127", "M0128", "M0129", "M0130",
    "M0136", "M0144", "M0149"
    }

def populate_error_codes():
    with open(sys.argv[1]) as fp:
        for line in fp:
            line = line.strip()
            if line.startswith("(*"):
                continue
            match = re.search(r'"(M\d+)"', line)
            if match:
                code = match.group(1)
                if code in registered_codes:
                    print("Error: Duplicate error code {}".format(code))
                    sys.exit(1)
                registered_codes.add(code)


def populate_tested_codes():
    tc_ok = glob.glob("./**/*.tc.ok", recursive=True)
    comp_ref_ok = glob.glob("./**/*.comp-ref.ok", recursive=True)
    comp_ok = glob.glob("./**/*.comp.ok", recursive=True)
    paths = tc_ok + comp_ref_ok + comp_ok
    for path in paths:
        with open(path) as fp:
            for line in fp:
                match = re.search(r"(?:error|warning) \[(M\d+)\]", line)
                if match:
                    code = match.group(1)
                    tested_codes.add(code)

populate_error_codes()
populate_tested_codes()

untested_codes = registered_codes - tested_codes - known_untested_codes
unregistered_codes = tested_codes - registered_codes
ignored_unregistered_codes = known_untested_codes - registered_codes
supposedly_untested_codes = tested_codes.intersection(known_untested_codes)

print("The last registered error code is {}".format(max(registered_codes)))

if untested_codes:
    print("Error: There were untested error codes:\n{}".format(untested_codes))
    print("""This likely means you should add a test exercising the given error code.
If that isn't possible or impractical add the error code to 'known_untested_codes' in 'check-error-codes.py'""")
if unregistered_codes:
    print("Error: There were unregistered codes:\n{}".format(unregistered_codes))
    print("This likely means you need to add your new error codes to 'src/lang_utils/error_codes.ml'")
if ignored_unregistered_codes:
    print("Error: There were known untested codes that weren't registered:\n{}".format(ignored_unregistered_codes))
    print("This likely means you need to remove those codes from the 'known_untested_codes' list in 'check-error-codes.py'")
if supposedly_untested_codes:
    print("Error: There were supposedly known untested codes that were tested:\n{}".format(supposedly_untested_codes))
    print("Great job! This likely means you can remove those codes from the 'known_untested_codes' list in 'check-error-codes.py'")

if untested_codes or unregistered_codes or ignored_unregistered_codes or supposedly_untested_codes:
    sys.exit(1)
