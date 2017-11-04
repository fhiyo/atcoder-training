#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""077-c"""

import sys
import pprint
import re
import itertools

import bisect

pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    N = int(input())

    As = sorted(list(map(int, input().split(' '))))
    Bs = map(int, input().split(' '))
    Cs = sorted(list(map(int, input().split(' '))))

    count = 0
    for b in Bs:
        i = bisect.bisect_left(As, b)
        k = N - bisect.bisect_right(Cs, b)
        count += i * k

    print(count)

if __name__ == '__main__':
    sys.exit(main())
