#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""002-d"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    N, M = list(map(int, input().split(' ')))

    rel = [[False] * N for _ in range(N)]
    for _ in range(M):
        x, y = list(map(int, input().split(' ')))
        rel[x - 1][y - 1] = True
        rel[y - 1][x - 1] = True

    for i in range(N):
        rel[i][i] = True
    max_ = 1
    for bit in range(2 ** N):
        fail = False
        b = bin(bit)[2:].zfill(N)
        member = [m.start() for m in re.finditer('1', b[::-1])]
        comb = list(itertools.combinations(member, 2))
        for c in comb:
            if not rel[c[0]][c[1]]:
                fail = True
                break
        if not fail:
            max_ = max(max_, len(member))
    print(max_)

if __name__ == '__main__':
    sys.exit(main())
