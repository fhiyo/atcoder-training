#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""077-b"""

import sys
import pprint
import re
import math

import itertools
pp = pprint.PrettyPrinter(indent=2)

def main():
    """Main function."""
    N = int(input())
    print(int(math.sqrt(N)) * int(math.sqrt(N)))


if __name__ == '__main__':
    sys.exit(main())
