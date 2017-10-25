#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""code-fes-2017-qual-c-c"""


import sys

OP_NUM = 0

def solve(s):
    """Solve."""
    global OP_NUM

    if len(s) == 0:
        return ""
    if len(s) == 1:
        return ""

    if s[0] == 'x':
        if s[-1] == 'x':
            return s[1:-1]
        OP_NUM += 1
        return s[1:]

    if s[0] == s[-1]:
        return s[1:-1]
    if 'x' == s[-1]:
        OP_NUM += 1
        return s[:-1]

    return None


def main():
    """Main function."""
    s = input()

    while s:
        s = solve(s)
        if s is None:
            print(-1)
            sys.exit(0)

    print(OP_NUM)

if __name__ == '__main__':
    sys.exit(main())
