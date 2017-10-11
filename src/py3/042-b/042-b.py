#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""042-b"""


from queue import PriorityQueue
import sys


def main():
    """Main function."""
    pq = PriorityQueue()
    N, L = map(int, sys.stdin.readline().split(' '))

    for l in range(N):
        pq.put(sys.stdin.readline()[:-1])

    while True:
        sys.stdout.write(pq.get())
        if pq.empty():
            break
    sys.stdout.write('\n')


if __name__ == '__main__':
    sys.exit(main())
