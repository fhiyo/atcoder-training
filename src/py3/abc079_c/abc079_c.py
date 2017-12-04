#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc079_c"""

import sys
import pprint
import re

import copy
from itertools import tee

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    d = list(map(int, list(input())))

    sentinel = object()
    def solve(v, it, s):
        n = next(it, sentinel)
        if v == 7 and n == sentinel:
            print(s[1:]+'=7')
            sys.exit(0)
        if n == sentinel:
            return
        solve(v+n, copy.copy(it), s+'+'+n.__str__())
        solve(v-n, copy.copy(it), s+'-'+n.__str__())

    solve(0, iter(d), '')

if __name__ == '__main__':
    sys.exit(main())
