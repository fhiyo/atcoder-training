#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""078-a"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    x, y = list(map(ord, input().split(' ')))

    if x > y:
        print('>')
    elif x < y:
        print('<')
    else:
        print('=')


if __name__ == '__main__':
    sys.exit(main())
