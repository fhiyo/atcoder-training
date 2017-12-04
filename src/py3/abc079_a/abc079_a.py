#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc079_a"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    d = input()
    if d[0] == d[1] and d[1] == d[2] or d[1] == d[2] and d[2] == d[3]:
        print("Yes")
    else:
        print("No")


if __name__ == '__main__':
    sys.exit(main())
