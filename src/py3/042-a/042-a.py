#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""042-a"""


import sys


def main():
    """Main function."""
    phrases = sys.stdin.readline()[:-1].split(" ")

    print('YES') if sorted(phrases) == sorted(['5', '7', '5']) else print('NO')


if __name__ == '__main__':
    sys.exit(main())
