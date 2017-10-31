#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""002-b"""


import sys


def main():
    """Main function."""
    W = input()
    vowels = 'aiueo'

    for c in W:
        if c not in vowels:
            print(c, end='')

    print()


if __name__ == '__main__':
    sys.exit(main())
