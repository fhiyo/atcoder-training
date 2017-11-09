# AtCoderの問題を好きな言語で解く

AtCoderの好きな言語で解くための環境．  
[aoj_haskell_training](https://github.com/fhiyo/aoj_haskell_training)の流用．

## 準備

1. 各環境でPython3をインストールする
1. 各環境で`virtualenv`をインストールする
1. `cd /path/to/repo && git clone git@github.com:fhiyo/atcoder-training.git && cd atcoder-training`
1. `virtualenv util/python/.venv -p <各環境でのpython3の実行ファイルの名前>`でvirtualenv環境を構築する
1. `util/python/.venv/bin/pip install -r util/python/requirements.txt`で依存ライブラリをインストールする

## 構成

## サイクル
1. 解きたいAtCoderの問題を決める
- `./manage.sh -m <問題番号> <問題のURL>`
- `test/<問題番号>/{input,output}/` の下に同じファイル名で入力と出力のファイルを用意する (test用の入力/出力値) (複数テストケースを置ける)
- `./manage.sh <LANG> -e <問題番号>`でソースコードを書く
- `./manage.sh <LANG> --test <問題番号>`でテスト
- テストをPassしたらコードをAtCoderに提出する

## その他
- `./manage.sh <LANG> --lint <問題番号>`でlintツールを使ったコーディングチェックをする (工事中)
- `./manage.sh --clean`でビルド時に生成したファイルを削除
- `./manage.sh <LANG> --copy <問題番号>`で書いたコードをクリップボードにコピー (Macのみ．pbcopyを使用)
- 使い方は`./manage.sh -h`で参照
