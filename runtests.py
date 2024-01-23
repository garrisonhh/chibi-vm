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
    successes = 0

    for testpath in testpaths:
        res = sp.run([CC, "run", testpath], capture_output=True)

        if res.returncode == 0:
            successes += 1

    print("[test summary]")
    print(f"total:     {len(testpaths)}")
    print(f"succeeded: {successes}")
    print(f"failed:    {len(testpaths) - successes}")

if __name__ == "__main__":
    main()