#! /usr/bin/env python
"""
run the c test suite.
"""

import os
import subprocess as sp
from pprint import pprint

PROJECT_DIR = os.path.dirname(os.path.abspath(__file__))
CC = os.path.join(PROJECT_DIR, "zig-out/bin/chibi-vm")
TEST_DIR = os.path.join(PROJECT_DIR, "tests/cases")
LOG_FILE = os.path.join(PROJECT_DIR, "tests.log")

# these are tests I'm not interested in due to project scope
IGNORED_TESTS = set([
    # goto, break, continue, switch/case
    10,
    34,
    51,
    105,

    # recursive types
    19,

    # libc
    25,
    40,
    56,
    104,

    # global variables TODO I should probably support this
    23,
    24,
    33,
    45,
    47,
    48,
    49,
    50,
    51,
    62,
    67,
    68,
    69,
    70,
    88,
    89,
    90,
    91,
    92,
    95,
    96,
    107,

    # test only chibicc code (e.g. the preprocessor)
    75,
])

def collect_paths():
    paths = []
    for filename in os.listdir(TEST_DIR):
        if not filename.endswith(".c"):
            continue

        filepath = os.path.join(TEST_DIR, filename)
        paths.append(filepath)

    return sorted(paths)

def main():
    testpaths = collect_paths()
    total = 0
    successes = 0

    for i, testpath in enumerate(testpaths):
        # TODO next 120 tests
        if i == 100:
            break

        number = i + 1

        if number in IGNORED_TESTS:
            continue

        res = sp.run([CC, "run", testpath], capture_output=True)

        if res.returncode == 0:
            total += 1
            successes += 1

        status = "success" if res.returncode == 0 else "failure"
        print(f"[{number}] {status}")

        if res.returncode != 0:
            out = res.stdout.decode('unicode_escape')
            err = res.stderr.decode('unicode_escape')
            err = "\n".join(err.strip().split('\n')[:5]) # limit number of lines

            total += 1
            print(f"[exited] {res.returncode}")
            if len(out) > 0:
                print(f"[stdout]\n{out}\n")
            if len(err) > 0:
                print(f"[stderr]\n{err}\n")

    percent = successes / total * 100.0

    print("[test summary]")
    print(f"passed  {successes}/{total} ({percent:.2f}%)")

if __name__ == "__main__":
    main()