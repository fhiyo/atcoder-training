#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc079_d"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)


def main():
    """Main function."""
    h,w = list(map(int, input().split(' ')))

    cs = []
    for i in range(10):
        cs.append([c for c in list(map(int, input().split(' ')))])

    as_ = []
    for i in range(h):
        as_.append([a for a in list(map(int, input().split(' ')))])

    for k in range(10):
        for i in range(10):
            for j in range(10):
                if cs[i][j] > cs[i][k] + cs[k][j]:
                    cs[i][j] = cs[i][k] + cs[k][j]

    res = 0
    for line in as_:
        for v in line:
            if v != -1:
                res += cs[v][1]

    print(res)


if __name__ == '__main__':
    sys.exit(main())
