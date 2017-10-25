#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""code-fes-2017-qual-c-b"""


import sys

from functools import reduce

def solve(str):
    """Solve."""
    i = int(str)

    if i % 2 == 0:
        return 2
    else:
        return 1


def main():
    """Main function."""
    N = int(input())
    As = map(solve, input().split())
    comp = reduce(lambda a, b: a * b, As)

    print(pow(3, N) - comp)


if __name__ == '__main__':
    sys.exit(main())
