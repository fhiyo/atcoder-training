#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""042-d"""


import sys

def factorialMod(x, mod):
    """Calculate modulo of funcotial 'x'."""
    assert isinstance(x, int)
    assert x >= 0
    assert isinstance(mod, int)
    assert mod >= 0

    if x == 0:
        return 1
    return (x % mod) * factorialMod(x - 1, mod)

def binpow(p, e):
    """Bisection exponentiation."""
    assert isinstance(e, int)
    assert e >= 0

    a = 1
    while e:
        if e % 2 == 0:
            p = p * p
            e /= 2
        else:
            a = a * p
            e = e - 1
    return a


def main():
    """Main function."""
    mod = 1000000007
    H, W, A, B = map(int, sys.stdin.readline().split())

    fact_table = [1, 1]
    [fact_table.append((fact_table[-1] * i) % mod) for i in range(2, H + W - 1)]
    inv_fact_table = [pow(fact_table[i], 1000000005, 1000000007) for i in range(H + W - 1)]

    def comb(n, r):
        """Calculate combination."""
        assert n >= r and n >= 0 and r >= 0

        return fact_table[n] * inv_fact_table[r] * inv_fact_table[n - r] % mod

    result = sum([comb(H - A + i - 1, H - A - 1) * comb(W + A - i - 2, A - 1) for i in range(B, W)]) % mod

    print(result)

if __name__ == '__main__':
    sys.exit(main())
