#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Scraping and get input and output and store as text file."""

import os
import re
import sys

from bs4 import BeautifulSoup

import requests


def main(args):
    """Main function."""
    url = args[1]
    problem_number = args[2]

    input_loc = os.path.join(os.path.dirname(os.path.realpath(__file__)),
                             '../../test',
                             problem_number,
                             'input')
    output_loc = os.path.join(input_loc[:-1 * len('input')], 'output')

    resp = requests.get(url)
    soup = BeautifulSoup(resp.text, 'html.parser')
    parts = soup.find_all('div', class_='part')

    inputs = [part.pre.get_text()
              for part in parts
              if re.search('Sample Input (\d+)', part.h3.get_text())]
    outputs = [part.pre.get_text()
               for part in parts
               if re.search('Sample Output (\d+)', part.h3.get_text())]

    for i, text in enumerate(inputs):
        with open(os.path.join(input_loc, '{}.txt'.format(i + 1)), 'w') as f:
            f.write(text.replace('\r\n', '\n')) if os.name == 'posix' else f.write(text)
    for i, text in enumerate(outputs):
        with open(os.path.join(output_loc, '{}.txt'.format(i + 1)), 'w') as f:
            f.write(text.replace('\r\n', '\n')) if os.name == 'posix' else f.write(text)


if __name__ == '__main__':
    if len(sys.argv) != 3:
        print("Usage: python storeInputOutput.py <URL> <program-number>")
        sys.exit(1)

    sys.exit(main(sys.argv))
