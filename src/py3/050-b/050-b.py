#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""050-b"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    N = int(input())
    Ts = list(map(int, input().split(' ')))
    M = int(input())
    sum_ = sum(Ts)
    for _ in range(M):
        p, x = list(map(int, input().split(' ')))
        print(sum_ - Ts[p-1] + x)

if __name__ == '__main__':
    sys.exit(main())
