#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""001-d"""

import math
import numpy as np
import sys


RAINTIMES = np.full((24 * 12), False, dtype=np.bool)


def calcRainTimes(str_):
    """Calculation RAINTIMES."""
    s_h, s_m, e_h, e_m = int(str_[0:2]), int(str_[2:4]), int(str_[5:7]), int(str_[7:9])
    s = int((s_h * 60 + s_m) / 5)
    e = math.ceil((e_h * 60 + e_m) / 5)

    return s, e


def output():
    """Output."""
    raintimes_index = [i for i, b in enumerate(RAINTIMES) if b]

    rain_durations = []
    start, end = raintimes_index[0], raintimes_index[0]
    for v in raintimes_index[1:]:
        if v == end + 1:
            end = v
        else:
            rain_durations.append((start, end))
            start, end = v, v
    rain_durations.append((start, end))

    [print('{:02d}{:02d}-'.format(*divmod(s * 5, 60)) + '{:02d}{:02d}'.format(*divmod((e + 1) * 5, 60))) for s, e in rain_durations]


def main():
    """Main function."""
    sys.stdin.readline()

    for line_ in sys.stdin:
       s, e = calcRainTimes(line_)
       for r in range(s, e):
           RAINTIMES[r] = True

    output()

if __name__ == '__main__':
    sys.exit(main())
