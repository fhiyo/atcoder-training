#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""078-c"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

pow_m = [1,2,4,8,16,32,64,128,256,512,1024]

def main():
    """Main function."""
    n, m = list(map(int, input().split(' ')))

    print((1900*m + 100*(n-m)) * pow_m[m])


if __name__ == '__main__':
    sys.exit(main())
