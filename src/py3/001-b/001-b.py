#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""001-b"""


import sys


def func(d_m):
    """Solver."""
    d_km = d_m / 1000

    if d_km < 0.1:
        vv = 0
    elif d_km >= 0.1 and d_km <= 5:
        vv = d_km * 10
    elif d_km >= 6 and d_km <= 30:
        vv = d_km + 50
    elif d_km >= 35 and d_km <= 70:
        vv = 80 + (d_km - 30) / 5
    elif d_km > 70:
        vv = 89
    else:
        vv = None

    return vv


def main():
    """Main function."""
    vv = func(int(sys.stdin.readline()))

    print('%02d' % (vv))


if __name__ == '__main__':
    sys.exit(main())
