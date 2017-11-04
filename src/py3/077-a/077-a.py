#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""077-a"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    c1 = input()
    c2 = input()

    if c1[0] == c2[2] and c1[1] == c2[1] and c1[2] == c2[0]:
        print("YES")
    else:
        print("NO")


if __name__ == '__main__':
    sys.exit(main())
