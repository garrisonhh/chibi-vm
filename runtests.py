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
    skipped = 0

    for i, testpath in enumerate(testpaths):
        res = sp.run([CC, "run", testpath], capture_output=True)

        if res.returncode == 0:
            total += 1
            successes += 1

        status = "success" if res.returncode == 0 else "failure"
        print(f"[{i + 1}] {status}")

        if res.returncode != 0:
            out = res.stdout.decode('unicode_escape')
            err = res.stderr.decode('unicode_escape')
            err = "\n".join(err.strip().split('\n')[:5]) # limit number of lines

            if err.startswith("unimplemented") or "stdio.h" in err:
                skipped += 1
                print("UNIMPLEMENTED")
            else:
                total += 1
                print(f"[exited] {res.returncode}")
                if len(out) > 0:
                    print(f"[stdout]\n{out}\n")
                if len(err) > 0:
                    print(f"[stderr]\n{err}\n")

    percent = successes / total * 100.0
    unskipped_percent = successes / len(testpaths) * 100.0

    print("[test summary]")
    print(f"skipped {skipped}")
    print(f"passed  {successes}/{total}/{len(testpaths)} ({percent:.2f}%/{unskipped_percent:.2f}%)")

if __name__ == "__main__":
    main()