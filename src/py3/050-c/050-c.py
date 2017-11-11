#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""050-c"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

mod = 1000000007

def main():
    """Main function."""
    N = int(input())
    as_ = sorted(list(map(int, input().split(' '))), reverse=True)

    checker = N - 1
    if N % 2 != 0:
        if as_[-1] != 0:
            print(0)
            sys.exit(0)
        else:
            N = N - 1

    for i in range(0, N, 2):
        if as_[i] != checker or as_[i + 1] != checker:
            print(0)
            sys.exit(0)
        checker = checker - 2

    print(pow(2, N // 2, mod))

if __name__ == '__main__':
    sys.exit(main())
