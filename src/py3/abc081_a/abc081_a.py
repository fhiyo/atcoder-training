#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc081_a"""

import sys
import pprint
import re

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    s = input()

    print(s.count('1'))


if __name__ == '__main__':
    sys.exit(main())
