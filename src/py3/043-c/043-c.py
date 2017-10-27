#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""043-c"""


import sys

from functools import reduce


def main():
    """Main function."""
    N = int(input())
    a_list = list(map(int, input().split()))

    # print(N, a_list)

    x = sum(a_list) / N

    def sumOfProd(n,l):
        """Sum of product."""
        return reduce(lambda a,b: a + b,
                      map(lambda v: (v - n) * (v - n), l))

    res1 = sumOfProd(int(x), a_list)
    res2 = sumOfProd(int(x) + 1, a_list)

    print(min(res1, res2))


if __name__ == '__main__':
    sys.exit(main())
