#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""043-b"""


import sys


def main():
    """Main function."""
    s = input()

    res = ""
    for c in s:
        if c == 'B':
            res = res[:-1]
        else:
            res += c

    print(res)


if __name__ == '__main__':
    sys.exit(main())
