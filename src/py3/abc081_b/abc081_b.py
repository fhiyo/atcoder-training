#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc081_b"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    n = int(input())
    as_ = list(map(int, input().split(' ')))

    count = 0
    while True:
        odds = list(filter(lambda x: x % 2 != 0, as_))
        if odds:
            break
        else:
            count += 1
            as_ = list(map(lambda x: x >> 1, as_))

    print(count)

if __name__ == '__main__':
    sys.exit(main())
