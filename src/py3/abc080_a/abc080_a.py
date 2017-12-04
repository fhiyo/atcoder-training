#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""abc080_a"""


import sys


def main():
    """Main function."""
    n, a, b = list(map(int, input().split(' ')))

    if n * a < b:
        print(n * a)
    else:
        print(b)


if __name__ == '__main__':
    sys.exit(main())
