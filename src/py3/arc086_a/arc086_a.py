#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""arc086_a"""

import sys
import pprint
import re

from collections import Counter

from functools import reduce

import itertools
pp = pprint.PrettyPrinter(indent=2)


def main():
    """Main function."""
    n, k = list(map(int, input().split(' ')))
    as_ = list(map(int, input().split(' ')))

    counter = Counter(as_)

    occ_list = sorted(counter.most_common(), key=lambda t: t[1], reverse=True)
    if len(occ_list) <= k:
        print(0)
    elif len(occ_list) == k + 1:
        print(occ_list[-1][1])
    else:
        print(reduce(lambda t1, t2: (0, t1[1] + t2[1]), occ_list[k:])[1])



if __name__ == '__main__':
    sys.exit(main())
