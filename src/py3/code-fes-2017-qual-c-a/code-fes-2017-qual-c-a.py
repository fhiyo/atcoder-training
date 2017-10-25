#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""code-fes-2017-qual-c-a"""


import sys
import re


def main():
    """Main function."""
    S = input()

    print("Yes") if re.search("AC", S) else print("No")


if __name__ == '__main__':
    sys.exit(main())
