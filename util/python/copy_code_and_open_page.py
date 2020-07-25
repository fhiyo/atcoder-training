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
    'cpp17': Lang('cpp17', 'cpp', re.compile(r'\s*// ')),
}


def get_formatted_code(probmem_number: str, lang: Lang) -> List[str]:
    # 自分のコードを取得し、コメントアウトを除き、EOF前のwhitespaceを捨てる
    source_file_path = SRC_PATH / lang.name / f'{probmem_number}/{probmem_number}.{lang.extension}'
    with open(source_file_path) as f:
        source = list(map(lambda l: l.rstrip('\r\n'), filter(lambda l: not lang.comment_pattern.match(l), f.readlines())))
        line_idx = len(source)  # 1-index
        for line in reversed(source):
            if line != '':
                break
            line_idx -= 1

    # XXX: macOSXのみ
    subprocess.run("pbcopy", universal_newlines=True, input='\n'.join(source[:line_idx]))


def main(probmem_number: str, lang_name: str, open: bool):
    if not lang_name in LANGS.keys():
        raise NotImplementedError(f"Invalid lang.\n Supported language: {list(LANGS.keys())}")

    contest_name = probmem_number.partition('_')[0]
    url = f'https://atcoder.jp/contests/{contest_name}/tasks/{probmem_number}'
    get_formatted_code(probmem_number, LANGS[lang_name])

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
