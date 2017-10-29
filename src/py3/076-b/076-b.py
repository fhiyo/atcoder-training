#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""076-b"""


import sys


def main():
    """Main function."""
    N = int(input())
    K = int(input())

    res = 1

    for i in range(N):
        if res * 2 < res + K:
            res = res * 2
        else:
            res = res + K

    print(res)


if __name__ == '__main__':
    sys.exit(main())
