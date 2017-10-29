#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""076-c"""


import sys

def kmp(T, P):
    n, m = len(T), len(P)
    j = 0
    k = 0
    fail = kmp_fail(P)
    while j < n:
        if T[j] == P[k] or T[j] == '?':
            if k == m - 1:
                return j - m + 1
            j += 1
            k += 1
        elif k > 0:
            k = fail[k-1]
        else:
            j += 1
    return -1

def kmp_fail(P):
    n = len(P)
    fail = [0] * n
    j = 1
    k = 0
    while j < n:
        if P[j] == P[k]:
            fail[j] = k + 1
            j += 1
            k += 1
        elif k > 0:
            k = fail[k - 1]
        else:
            j += 1
    return fail


def main():
    """Main function."""
    S_p = input()
    T = input()

    pos = kmp(S_p, T)

    if pos < 0:
        print('UNRESTORABLE')
        sys.exit(0)

    total = pos

    pos_ = 0
    while pos_ >= 0:
        pos_ = kmp(S_p[pos+1:], T)
        if pos_ >= 0:
            pos = pos + pos_ + 1

    S_cand = S_p[:pos] + T + S_p[pos+len(T):]

    S = S_cand.replace('?', 'a')
    print(S)

if __name__ == '__main__':
    sys.exit(main())
