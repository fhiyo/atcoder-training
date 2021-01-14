import subprocess
import sys
from typing import List
import webbrowser
from collections import namedtuple
import re

import requests

from Config import SRC_PATH
from download_samples import login, problemIdFromUrl


Lang = namedtuple('Lang', ('name', 'extension', 'comment_pattern'))

LANGS = {
    'cpp17': Lang('cpp17', 'cpp', re.compile(r'\s*//')),
    'py3': Lang('py3', 'py', re.compile(r'\s*#')),
}


def get_formatted_code(problem_number: str, lang: Lang) -> List[str]:
    # 自分のコードを取得し、コメントアウトを除き、EOF前のwhitespaceを捨てる
    source_file_path = SRC_PATH / lang.name / f'{problem_number}/{problem_number}.{lang.extension}'
    with open(source_file_path) as f:
        source = list(map(lambda l: l.rstrip('\r\n'), filter(lambda l: not lang.comment_pattern.match(l), f.readlines())))
        line_idx = len(source)  # 1-index
        for line in reversed(source):
            if line != '':
                break
            line_idx -= 1

    # XXX: macOSXのみ
    subprocess.run("pbcopy", universal_newlines=True, input='\n'.join(source[:line_idx]))


def main(problem_number: str, lang_name: str, open: bool):
    if not lang_name in LANGS.keys():
        raise NotImplementedError(f"Invalid lang.\n Supported language: {list(LANGS.keys())}")

    # こういう例があった: https://atcoder.jp/contests/pakencamp-2019-day3/tasks/pakencamp_2019_day3_c
    contest_name = problem_number.rpartition('_')[0].replace('_', '-')
    url = f'https://atcoder.jp/contests/{contest_name}/tasks/{problem_number}'
    get_formatted_code(problem_number, LANGS[lang_name])

    if open:
        # loginする
        problem_id = problemIdFromUrl(url)
        session = requests.Session()
        login(session, problem_id)
        # ページに遷移するだけ
        webbrowser.open(url + '#select-lang')


if __name__ == '__main__':
    # TODO: 汚いのでもうちょい整理したい
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('--problem_number', type=str, help='contest problem number (ex. abc155_a)')
    parser.add_argument('--lang', '-l', type=str, help='language')
    parser.add_argument('--open', action='store_true', help='open problem page')
    args = parser.parse_args()

    sys.exit(main(args.problem_number, args.lang, args.open))
