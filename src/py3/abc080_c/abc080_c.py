#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc080_c"""


import sys


def numFromBitList(l):
    """Get number from bit list."""
    n = 0
    for i, v in enumerate(l):
        n += v * 2 ** i
    return n

def main():
    """Main function."""
    n = int(input())

    fs = []
    for _ in range(n):
        fs.append(numFromBitList(list(map(int, input().split(' ')))))

    ps = []
    for _ in range(n):
        ps.append(list(map(int, input().split(' '))))

    max_ = -sys.maxsize
    for i in range(1, 1024):
        profit = 0
        for idx, f in enumerate(fs):
            profit += ps[idx][bin(i & f)[2:].count('1')]
        max_ = max(max_, profit)

    print(max_)


if __name__ == '__main__':
    sys.exit(main())
