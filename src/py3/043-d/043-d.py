#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""043-d"""


import sys


def main():
    """Main function."""
    s = input()
    len_ = len(s)

    for i in range(len_ - 2):
        if s[i] == s[i + 1]:
            print(i + 1, i + 2)
            sys.exit(0)
        if s[i] == s[i + 2]:
            print(i + 1, i + 3)
            sys.exit(0)

    if len_ >= 2 and s[-2] == s[-1]:
        print(len_ - 1, len_)
        sys.exit(0)

    print(-1, -1)


if __name__ == '__main__':
    sys.exit(main())
