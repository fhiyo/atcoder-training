#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""002-c"""


import sys


def main():
    """Main function."""
    x_a, y_a, x_b, y_b, x_c, y_c = list(map(int, input().split(' ')))

    (v1_x, v1_y) = (x_b - x_a, y_b - y_a)
    (v2_x, v2_y) = (x_c - x_a, y_c - y_a)

    # print(v1_x, v1_y, v2_x, v2_y)

    print('{:.3f}'.format(abs(((v1_x * v2_y) - (v1_y * v2_x)) / 2)))




if __name__ == '__main__':
    sys.exit(main())
