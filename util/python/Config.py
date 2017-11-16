# -*- coding: utf-8 -*-

"""Configuration."""

from pathlib import Path

CONF_PATH = Path(__file__).parent.joinpath('../settings')
KEY_PATH = CONF_PATH.joinpath('key.dat')

ATCODER_PATH = CONF_PATH.joinpath('atcoder')

NAME_PATH = ATCODER_PATH.joinpath('name.txt')
PASS_PATH = ATCODER_PATH.joinpath('password.dat')

SRC_PATH = Path(__file__).parent.joinpath('../../src')
TEST_PATH = Path(__file__).parent.joinpath('../../test')
