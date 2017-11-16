#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""078-b"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    x, y, z = list(map(int, input().split(' ')))

    print((x - z) // (y + z))


if __name__ == '__main__':
    sys.exit(main())
