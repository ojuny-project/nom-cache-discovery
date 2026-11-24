#!/usr/bin/env python3
"""Compare cabal2nix dependency lists with the checked-in derivation."""

import re
import sys

SECTIONS = ("libraryHaskellDepends", "executableHaskellDepends", "testHaskellDepends")


def dependency_lists(text):
    lists = {}
    for name in SECTIONS:
        match = re.search(name + r"\s*=\s*\[(.*?)\];", text, re.S)
        if match is None:
            raise ValueError(f"{name} is missing")
        lists[name] = set(match.group(1).split())
    return lists


def main():
    generated = dependency_lists(open(sys.argv[1]).read())
    checked_in = dependency_lists(open(sys.argv[2]).read())
    failed = [name for name in SECTIONS if generated[name] != checked_in[name]]
    if failed:
        for name in failed:
            print(
                f"{name}: generated={sorted(generated[name])} checked={sorted(checked_in[name])}",
                file=sys.stderr,
            )
        sys.exit(1)


if __name__ == "__main__":
    main()
