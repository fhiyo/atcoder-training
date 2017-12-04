#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc080_b"""


import sys

from functools import reduce

def main():
    """Main function."""
    n = int(input())
    sum_digits = reduce(lambda a, b: a + b, map(int, str(n)))

    if n % sum_digits == 0:
        print("Yes")
    else:
        print("No")



if __name__ == '__main__':
    sys.exit(main())
