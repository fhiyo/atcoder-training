#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""AtCoder module."""

import getpass
import json
import os
import re
import sys

from bs4 import BeautifulSoup
from cryptography.fernet import Fernet
from pathlib import Path

import requests
import urllib.parse

from Config import *

from logging import basicConfig
from logging import getLogger
from logging import DEBUG
from logging import CRITICAL

basicConfig(level=CRITICAL)
logger = getLogger(__name__)

FAILED_MSG = "You failed to sign in"
SUCCESS_MSG = "You signed in."
LOGIN_URL='https://practice.contest.atcoder.jp/login'


def generateKey(key_path=KEY_PATH, update=False):
    """Generate key for encryption."""
    logger.info("Call generateKey()")

    if key_path.exists() and not update:
        logger.info("Key file is already exist. Skip.")
        return

    with open(key_path, 'wb') as bf:
        bf.write(Fernet.generate_key())
        logger.info("Generate key file.")
        logger.info("Key path: %s" % key_path)


def generateUserInfo(key_path=KEY_PATH,
                     name_path=NAME_PATH,
                     pass_path=PASS_PATH):
    """Generate user info."""
    print('Input atcoder user info.')
    user_id = input('User ID: ')
    raw_pass = getpass.getpass('Password: ')

    if not key_path.exists():
        generateKey()

    with open(key_path, 'r') as f:
        fernet = Fernet(f.read())

    enc_pass = fernet.encrypt(raw_pass.encode())

    Path.mkdir(ATCODER_PATH, exist_ok=True)

    with open(name_path, 'w') as f:
        f.write(user_id)
    with open(pass_path, 'wb') as f:
        f.write(enc_pass)


def login(session,
          key_path=KEY_PATH,
          name_path=NAME_PATH,
          pass_path=PASS_PATH):
    """Login and make session."""
    if not name_path.exists() or not pass_path.exists():
        generateUserInfo()

    with open(key_path, 'r') as f:
        key = f.read()
    fernet = Fernet(key)

    with open(name_path, 'r') as f:
        name = f.read()
    with open(pass_path, 'rb') as f:
        decrypted_pass = fernet.decrypt(f.read())

    data = {'name': name, 'password': decrypted_pass}

    logger.debug('A session generated.')
    resp = session.request('POST', LOGIN_URL, data=data, allow_redirects=False)
    for cookie in resp.cookies:
        if cookie.name.startswith('__message_'):
            msg = json.loads(urllib.parse.unquote_plus(cookie.value))
            logger.debug('msg: %s' % str(msg))
            if FAILED_MSG in msg['h']:
                logger.warning(FAILED_MSG)
                sys.exit(1)
            if SUCCESS_MSG in msg['c']:
                logger.info(SUCCESS_MSG)


def downloadSamples(url,
                    input_path,
                    output_path,
                    session):
    """Download sample input and output."""
    problem_id = problemIdFromUrl(url)

    login(session)

    resp = session.request('GET', url)
    soup = BeautifulSoup(resp.text, 'html.parser')
    parts = soup.find_all('div', class_='part')

    inputs = [part.pre.get_text()
              for part in parts
              if re.search('Sample Input (\d+)', part.h3.get_text())]
    outputs = [part.pre.get_text()
               for part in parts
               if re.search('Sample Output (\d+)', part.h3.get_text())]

    for i, text in enumerate(inputs):
        save(input_path, i, text)
    for i, text in enumerate(outputs):
        save(output_path, i, text)


def save(path, idx, text):
    """Save example file."""
    with open(path.joinpath('{}.txt'.format(idx + 1)), 'w') as f:
        f.write(text.replace('\r\n', '\n')) if os.name == 'posix' else f.write(text)


def contestIdFromUrl(url):
    """Get contest id from url."""
    parsed = urllib.parse.urlparse(url)
    return parsed.netloc.split('.')[0]


def problemIdFromUrl(url):
    """Get proglem id from url."""
    parsed = urllib.parse.urlparse(url)
    return parsed.path.split('/')[-1]


def prepare(url, langs):
    """Prepare environment to solve problem."""
    problem_id = problemIdFromUrl(url)
    input_path = TEST_PATH.joinpath(problem_id, 'input')
    output_path = input_path.parent.joinpath('output')

    Path.mkdir(input_path, parents=True, exist_ok=True)
    Path.mkdir(output_path, parents=True, exist_ok=True)
    for lang in langs:
        Path.mkdir(SRC_PATH.joinpath(lang, problem_id), exist_ok=True)

    session = requests.Session()

    downloadSamples(url, input_path, output_path, session)


def main(argv):
    """Main function."""
    prepare(argv[1], argv[2:])


if __name__ == '__main__':
    sys.exit(main(sys.argv))
