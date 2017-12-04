#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc079_b"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    n = int(input())

    l = [2,1]

    if n == 1:
        print(l[1])
    else:
        for i in range(2,n+1):
            l.append(l[-2]+l[-1])
        print(l[-1])


if __name__ == '__main__':
    sys.exit(main())
